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

# Statistiken förväntas i långt format med en rad per geografi, år och indikator:
#
#   regionkod  chr  "00" för riket, "20" för Dalarna eller kommunkod (4 siffror)
#   omrade     chr  områdets namn som i shb_omraden, NA för rader som gäller hela regionen
#   ar         int  år
#   indikator  chr  indikatorns namn
#   taljare    dbl  antal
#   namnare    dbl  antal i gruppen som andelen räknas på, NA om indikatorn är ett rent antal
#
# Andelar räknas fram i appen som taljare / namnare. Kommuner som saknar egna rader
# summeras ihop från sina områden, riket och län måste finnas som egna rader.
hamta_shb_statistik <- function(tabell, kommun_sf, omraden_sf) {

  df <- if (is.null(tabell)) {
    skapa_exempeldata(kommun_sf, omraden_sf)
  } else {
    tbl(shiny_uppkoppling_las("oppna_data"), dbplyr::in_schema(tabell[1], tabell[2])) %>%
      collect()
  }

  df <- df %>%
    mutate(
      regionkod = as.character(regionkod),
      regionkod = ifelse(is.na(omrade), regionkod, omradesnyckel(regionkod, omrade)),
      ar = as.integer(ar),
      across(c(taljare, namnare), as.numeric)
    ) %>%
    select(regionkod, ar, indikator, taljare, namnare)

  # kommuner som saknas summeras från sina områden
  kommun_fran_omraden <- df %>%
    inner_join(st_drop_geometry(omraden_sf) %>% select(omradeskod, kommunkod), by = c("regionkod" = "omradeskod")) %>%
    group_by(regionkod = kommunkod, ar, indikator) %>%
    summarise(taljare = sum(taljare), namnare = sum(namnare), .groups = "drop") %>%
    anti_join(df, by = c("regionkod", "ar", "indikator"))

  resultat <- bind_rows(df, kommun_fran_omraden) %>%
    mutate(varde = ifelse(is.na(namnare), taljare, round(taljare / namnare * 100, 1)))

  attr(resultat, "exempeldata") <- is.null(tabell)
  resultat
}

# Enhet per indikator: "procent" om den har nämnare, annars "antal"
indikatorenhet <- function(df, vald_indikator) {
  if (all(is.na(df$namnare[df$indikator == vald_indikator]))) "antal" else "procent"
}

# Slumpade värden i rätt format, används tills riktig statistik finns i databasen.
# Bara områden, Dalarna och riket - kommunerna summeras ihop i hamta_shb_statistik().
skapa_exempeldata <- function(kommun_sf, omraden_sf) {
  set.seed(20)

  omraden <- st_drop_geometry(omraden_sf) %>%
    select(regionkod = kommunkod, omrade = omradesnamn)

  df_omr <- expand_grid(omraden, ar = 2018:2025, indikator = c("Exempelindikator A", "Exempelindikator B")) %>%
    group_by(regionkod, omrade, indikator) %>%
    mutate(
      namnare = round(runif(1, 200, 3000) * (1 + cumsum(rnorm(n(), 0, 0.02)))),
      taljare = round(namnare * pmin(pmax(runif(1, 0.1, 0.6) + cumsum(rnorm(n(), 0, 0.02)), 0), 1))
    ) %>%
    ungroup()

  df_lan <- df_omr %>%
    group_by(ar, indikator) %>%
    summarise(taljare = sum(taljare), namnare = sum(namnare), .groups = "drop") %>%
    mutate(regionkod = "20", omrade = NA_character_)

  df_riket <- df_lan %>%
    mutate(regionkod = "00", namnare = namnare * 35, taljare = round(taljare * 35 * runif(n(), 0.9, 1.1)))

  bind_rows(df_omr, df_lan, df_riket)
}

# Namn och nivå för alla geografier, för att sätta namn på statistiken
skapa_geografinamn <- function(kommun_sf, omraden_sf) {
  bind_rows(
    tibble(regionkod = c("00", "20"), namn = c("Riket", "Dalarna"), niva = c("Riket", "Län")),
    st_drop_geometry(kommun_sf) %>% transmute(regionkod = kommunkod, namn = kommunnamn, niva = "Kommun"),
    st_drop_geometry(omraden_sf) %>% transmute(regionkod = omradeskod, namn = omradesnamn, niva = "Område", kommun = kommunnamn)
  )
}
