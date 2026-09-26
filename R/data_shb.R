# ---- Inläsning av områden och statistik ----

# Nyckel för ett område. shb_omraden saknar egen områdeskod och samma områdesnamn
# kan förekomma i flera kommuner, därför kombineras kommunkod och områdesnamn.
omradesnyckel <- function(kommunkod, omrade) paste0(kommunkod, "_", omrade)

# Läser in shb_omraden och döper om kolumnerna till omradeskod, omradesnamn och kommunkod,
# så att resten av appen inte behöver veta vad de heter i databasen.
hamta_shb_omraden <- function(con, kommun_sf, kol) {

  omr <- tbl(con, dbplyr::in_schema("omradesindelningar", "shb_omraden"))

  saknas <- setdiff(unlist(kol), colnames(omr))
  if (length(saknas) > 0) {
    stop("Kolumnerna ", paste(saknas, collapse = ", "), " finns inte i omradesindelningar.shb_omraden. ",
         "Tillgängliga kolumner: ", paste(colnames(omr), collapse = ", "), ". Justera shb_kol i global.R.")
  }

  omr %>%
    select(omradesnamn = all_of(kol$namn), kommunkod = all_of(kol$kommunkod), all_of(kol$geom)) %>%
    collect() %>%                                                  # först här görs uttaget ur databasen
    df_till_sf(geom_col = kol$geom) %>%                            # tabellen ligger i SWEREF99 TM (3006)
    st_transform(crs = 4326) %>%
    mutate(kommunkod = str_pad(as.character(kommunkod), 4, pad = "0"),
           omradeskod = omradesnyckel(kommunkod, omradesnamn)) %>%
    filter(kommunkod %in% kommun_sf$kommunkod) %>%
    left_join(st_drop_geometry(kommun_sf), by = "kommunkod") %>%
    select(omradeskod, omradesnamn, kommunkod, kommunnamn)
}

# Statistiken ligger i långt format med en rad per geografi, år, ägarkategori och indikator:
#
#   ar              int  år
#   regionkod       chr  "00" för riket, "20" för Dalarna eller kommunkod (4 siffror, även för områden)
#   omrade          chr  områdets namn som i shb_omraden, NA för rader som gäller hela regionen
#   agarkategori    chr  Totalt, Allmännyttan, Övriga ägare eller Uppgift saknas
#   grupp           chr  indikatorgrupp, t.ex. Huvudindikator eller Bakgrund
#   indikator       chr  kod, t.ex. ekonomiskt_bistand
#   indikator_namn  chr  namn som visas i appen
#   enhet           chr  Personer eller Hushåll
#   taljare         dbl  antal
#   namnare         dbl  antal i gruppen som andelen räknas på, NA om indikatorn är ett rent antal
#
# Alla nivåer (område, kommun, län och riket) ska finnas som egna rader. Övriga kolumner ignoreras.
hamta_shb_statistik <- function(tabell, kommun_sf, omraden_sf, min_befolkning_omrade, min_grupp) {

  if (is.null(tabell)) {
    df <- skapa_exempeldata(kommun_sf, omraden_sf)
  } else {
    con <- shiny_uppkoppling_las(db_name = tabell$databas, db_user = tabell$anvandare)
    df <- tbl(con, dbplyr::in_schema(tabell$schema, tabell$tabell)) %>% collect()
    DBI::dbDisconnect(con)
  }

  resultat <- forbered_shb_statistik(df, min_befolkning_omrade, min_grupp)
  attr(resultat, "exempeldata") <- is.null(tabell)
  resultat
}

# Gör statistiken redo för appen och sekretessgranskar den. Allt som visas eller kan laddas ned
# i appen kommer härifrån, så värden som släcks här når aldrig webbläsaren.
#
# Sekretessregler:
#   - områden där den totala befolkningen är färre än min_befolkning_omrade visas inte alls
#   - andelar där nämnaren är färre än min_grupp, och antal under min_grupp, visas inte
# Släckta värden får en kommentar som visas i stället för värdet.
forbered_shb_statistik <- function(df, min_befolkning_omrade, min_grupp, befolkning_indikator = "befolkning") {
  df <- df %>%
    mutate(
      ar_omrade = !is.na(omrade),
      regionkod = as.character(regionkod),
      regionkod = ifelse(ar_omrade, omradesnyckel(regionkod, omrade), regionkod),
      ar = as.integer(ar),
      across(c(taljare, namnare), as.numeric)
    ) %>%
    # Rader utan värde, t.ex. köpkraft för år där uppgiften inte finns. Då går året inte heller att välja.
    filter(!is.na(taljare), is.na(namnare) | namnare > 0) %>%
    # Andel eller antal avgörs per indikator innan värden släcks
    group_by(indikator) %>%
    mutate(typ = if (any(!is.na(namnare))) "andel" else "antal") %>%
    ungroup()

  # Varje områdes totala befolkning samma år
  befolkning <- df %>%
    filter(indikator == befolkning_indikator, agarkategori == "Totalt") %>%
    select(regionkod, ar, total_befolkning = taljare)

  df %>%
    left_join(befolkning, by = c("regionkod", "ar")) %>%
    mutate(
      kommentar = case_when(
        ar_omrade & total_befolkning < min_befolkning_omrade ~
          paste0("Visas inte: området har färre än ", min_befolkning_omrade, " invånare"),
        typ == "andel" & namnare < min_grupp ~ paste0("Visas inte: färre än ", min_grupp, " ", tolower(enhet)),
        typ == "antal" & taljare < min_grupp ~ paste0("Visas inte: färre än ", min_grupp, " ", tolower(enhet)),
        TRUE ~ NA_character_
      ),
      skyddad = !is.na(kommentar),
      taljare = if_else(skyddad, NA_real_, taljare),
      namnare = if_else(skyddad, NA_real_, namnare),
      varde = case_when(skyddad ~ NA_real_,
                        typ == "antal" ~ taljare,
                        TRUE ~ round(taljare / namnare * 100, 1))
    ) %>%
    select(regionkod, ar, agarkategori, grupp, indikator, indikator_namn, enhet, typ,
           taljare, namnare, varde, skyddad, kommentar)
}

# Slumpade värden i samma format som den riktiga tabellen, används när shb_stat_tabell är NULL
skapa_exempeldata <- function(kommun_sf, omraden_sf) {
  set.seed(20)

  indikatorer <- tribble(
    ~grupp,           ~indikator,           ~indikator_namn,                                    ~enhet,     ~typ,
    "Huvudindikator", "ekonomiskt_bistand", "Ekonomiskt bistånd minst sex månader, 18–64 år",   "Personer", "andel",
    "Huvudindikator", "trangbodda",         "Trångbodda enligt norm 3",                          "Hushåll",  "andel",
    "Bakgrund",       "befolkning",         "Befolkning",                                        "Personer", "antal"
  )

  omraden <- st_drop_geometry(omraden_sf) %>% select(regionkod = kommunkod, omrade = omradesnamn)

  df_omr <- expand_grid(omraden, ar = 2022:2024, agarkategori = c("Allmännyttan", "Övriga ägare"), indikatorer) %>%
    group_by(regionkod, omrade, agarkategori, indikator) %>%
    mutate(
      bas = round(runif(1, 10, 2500) * (1 + cumsum(rnorm(n(), 0, 0.02)))),
      namnare = if_else(typ == "andel", bas, NA_real_),
      taljare = if_else(typ == "andel", round(bas * pmin(pmax(runif(1, 0.02, 0.4) + cumsum(rnorm(n(), 0, 0.01)), 0), 1)), bas)
    ) %>%
    ungroup() %>%
    select(-bas)

  summera <- function(df, ...) {
    df %>%
      group_by(..., ar, agarkategori, grupp, indikator, indikator_namn, enhet, typ) %>%
      summarise(taljare = sum(taljare), namnare = sum(namnare), .groups = "drop")
  }

  df_kommun <- summera(df_omr, regionkod)
  df_lan    <- summera(df_omr) %>% mutate(regionkod = "20")
  df_riket  <- df_lan %>% mutate(regionkod = "00", taljare = round(taljare * 35 * runif(n(), 0.9, 1.1)), namnare = namnare * 35)

  alla <- bind_rows(df_omr, df_kommun, df_lan, df_riket)
  totalt <- summera(alla, regionkod, omrade) %>% mutate(agarkategori = "Totalt")

  bind_rows(alla, totalt)
}

# Indikatorerna i den ordning de visas i listrutan: huvudindikatorer först
skapa_indikatorlista <- function(statistik) {
  statistik %>%
    distinct(grupp, indikator, indikator_namn, enhet, typ) %>%
    mutate(grupp = factor(grupp, levels = unique(c("Huvudindikator", sort(unique(grupp)))))) %>%
    arrange(grupp, indikator_namn)
}

# Namn och nivå för alla geografier, för att sätta namn på statistiken
skapa_geografinamn <- function(kommun_sf, omraden_sf) {
  bind_rows(
    tibble(regionkod = c("00", "20"), namn = c("Riket", "Dalarna"), niva = c("Riket", "Län")),
    st_drop_geometry(kommun_sf) %>% transmute(regionkod = kommunkod, namn = kommunnamn, niva = "Kommun"),
    st_drop_geometry(omraden_sf) %>% transmute(regionkod = omradeskod, namn = omradesnamn, niva = "Område", kommun = kommunnamn)
  )
}
