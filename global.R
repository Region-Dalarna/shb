## Globala inställningar för Shinyappen: shb

# Ladda nödvändiga paket
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(ggiraph)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(leaflet)
library(sf)
library(writexl)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_shinyappar.R", encoding = "utf-8", echo = FALSE)

# hjälpfunktioner i R/ (Shiny laddar dem automatiskt, men vi gör det explicit så att ordningen blir tydlig)
for (fil in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(fil, encoding = "utf-8")

telemetry <- skapa_telemetry("shb")

# Allmänna options - TRUE = visa inte R-felmeddelanden i appen,
# FALSE = visa felmeddelanden från R på webben
options(shiny.sanitize.errors = FALSE)
options(dplyr.summarise.inform = FALSE)

# ---- 1. Inställningar ----

# Kolumnnamn i omradesindelningar.shb_omraden (databasen geodata).
# Justera här om kolumnerna heter något annat. Sätt kommunkod = NA om tabellen
# saknar kommunkod, då kopplas varje område till den kommun det ligger i geografiskt.
shb_kol <- list(
  kod       = "omradeskod",
  namn      = "omradesnamn",
  kommunkod = "kommunkod"
)

# Tabell med statistiken i databasen oppna_data, t.ex. c("shb", "statistik").
# Så länge den är NULL visas slumpade exempeldata så att appen kan byggas och testas.
# Förväntat format, se R/data_shb.R.
shb_stat_tabell <- NULL

KALLA_SHB <- "Källa: ..., bearbetning av Samhällsanalys, Region Dalarna"

# ---- 2. Läs in kartor ----

kommun_sf <- tbl(shiny_uppkoppling_las("geodata"), dbplyr::in_schema("karta", "kommun_scb")) %>%
  filter(str_sub(knkod, 1, 2) == "20") %>%                                               # filtrera ut Dalarnas kommuner
  collect() %>%
  df_till_sf() %>%
  select(kommunkod = knkod, kommunnamn = knnamn) %>%
  st_transform(crs = 4326)

shb_omraden_sf <- hamta_shb_omraden(shiny_uppkoppling_las("geodata"), kommun_sf, shb_kol)

# ---- 3. Läs in statistik ----

shb_statistik <- hamta_shb_statistik(shb_stat_tabell, kommun_sf, shb_omraden_sf)
shb_exempeldata <- isTRUE(attr(shb_statistik, "exempeldata"))
geografinamn <- skapa_geografinamn(kommun_sf, shb_omraden_sf)

# färgvektor
rus_tre_fokus <- c("#93cec1", "#178571", "#000000")
