# ---- Inläsning av områden och statistik ----

# Läser in shb_omraden och döper om kolumnerna till omradeskod, omradesnamn och kommunkod,
# så att resten av appen inte behöver veta vad de heter i databasen.
hamta_shb_omraden <- function(con, kommun_sf, kol) {

  # st_read hittar själv geometrikolumnen och koordinatsystemet
  omr <- st_read(con, query = "SELECT * FROM omradesindelningar.shb_omraden", quiet = TRUE)

  kol_som_kravs <- unlist(kol[!is.na(kol)])
  saknas <- setdiff(kol_som_kravs, names(omr))
  if (length(saknas) > 0) {
    stop("Kolumnerna ", paste(saknas, collapse = ", "), " finns inte i omradesindelningar.shb_omraden. ",
         "Tillgängliga kolumner: ", paste(setdiff(names(omr), attr(omr, "sf_column")), collapse = ", "),
         ". Justera shb_kol i global.R.")
  }

  omr <- omr %>%
    st_transform(crs = 4326) %>%
    rename(omradeskod = all_of(kol$kod), omradesnamn = all_of(kol$namn)) %>%
    mutate(omradeskod = as.character(omradeskod))

  if (is.na(kol$kommunkod)) {
    # ingen kommunkolumn - koppla på kommun utifrån var områdets inre punkt ligger
    punkter <- suppressWarnings(st_point_on_surface(omr))
    omr$kommunkod <- kommun_sf$kommunkod[as.integer(st_intersects(punkter, kommun_sf))]
  } else {
    omr <- omr %>% rename(kommunkod = all_of(kol$kommunkod))
  }

  omr %>%
    mutate(kommunkod = str_pad(as.character(kommunkod), 4, pad = "0")) %>%
    filter(kommunkod %in% kommun_sf$kommunkod) %>%
    left_join(st_drop_geometry(kommun_sf), by = "kommunkod") %>%
    select(omradeskod, omradesnamn, kommunkod, kommunnamn)
}

# Statistiken förväntas i långt format med en rad per geografi, år och indikator:
#
#   regionkod  chr  "20" för Dalarna, kommunkod (4 siffror) eller omradeskod
#   ar         int  år
#   indikator  chr  indikatorns namn
#   varde      dbl  värdet som visas i karta och diagram
#
# Kommuner och Dalarna ligger alltså som egna rader i tabellen istället för att
# räknas fram i appen, eftersom det beror på indikatorn hur man ska summera
# (antal kan summeras, andelar måste vägas).
hamta_shb_statistik <- function(tabell, kommun_sf, omraden_sf) {

  if (is.null(tabell)) return(skapa_exempeldata(kommun_sf, omraden_sf))

  tbl(shiny_uppkoppling_las("oppna_data"), dbplyr::in_schema(tabell[1], tabell[2])) %>%
    collect() %>%
    mutate(regionkod = as.character(regionkod), ar = as.integer(ar), varde = as.numeric(varde))
}

# Slumpade värden i rätt format, används tills riktig statistik finns i databasen
skapa_exempeldata <- function(kommun_sf, omraden_sf) {
  set.seed(20)
  koder <- c("20", kommun_sf$kommunkod, omraden_sf$omradeskod)

  df <- expand_grid(
    regionkod = koder,
    ar        = 2018:2025,
    indikator = c("Exempelindikator A", "Exempelindikator B")
  ) %>%
    group_by(regionkod, indikator) %>%
    mutate(varde = round(runif(1, 20, 80) + cumsum(rnorm(n(), 0, 3)), 1)) %>%
    ungroup()

  attr(df, "exempeldata") <- TRUE
  df
}

# Namn och nivå för alla geografier, för att sätta namn på statistiken
skapa_geografinamn <- function(kommun_sf, omraden_sf) {
  bind_rows(
    tibble(regionkod = "20", namn = "Dalarna", niva = "Län"),
    st_drop_geometry(kommun_sf) %>% transmute(regionkod = kommunkod, namn = kommunnamn, niva = "Kommun"),
    st_drop_geometry(omraden_sf) %>% transmute(regionkod = omradeskod, namn = omradesnamn, niva = "Område", kommun = kommunnamn)
  )
}
