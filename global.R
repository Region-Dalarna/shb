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
library(rdshinyappar)
library(leaflet)
library(sf)
library(writexl)

# hjälpfunktioner i R/ (Shiny laddar dem automatiskt, men vi gör det explicit så att ordningen blir tydlig)
for (fil in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(fil, encoding = "utf-8")

telemetry <- skapa_telemetry("shb")

# Allmänna options - TRUE = visa inte R-felmeddelanden i appen,
# FALSE = visa felmeddelanden från R på webben
options(shiny.sanitize.errors = FALSE)
options(dplyr.summarise.inform = FALSE)

# ---- 1. Inställningar ----

# Kolumnnamn i omradesindelningar.shb_omraden (databasen geodata)
shb_kol <- list(
  namn      = "omrade",
  kommunkod = "kommunkod",
  geom      = "geom"
)

# Statistiken ligger i sekretessdatabasen och läses med rollen shiny_las_sekretess.
# Sätt till NULL för att köra appen med slumpade exempeldata. Förväntat format, se R/data_shb.R.
shb_stat_tabell <- list(databas = "sekretess", anvandare = "shiny_las_sekretess", schema = "shb", tabell = "indikatorer")

# Sekretess. Värdena släcks redan när statistiken läses in, se forbered_shb_statistik().
# Områden där den totala befolkningen är färre än så här visas inte alls
shb_min_befolkning_omrade <- 50
# Andelar som bygger på färre personer (eller hushåll) än så här, och antal under det, visas inte
shb_min_grupp <- 30
# Täljare från 1 upp till under så här visas som "färre än 5" och andelen som en övre gräns
shb_min_taljare <- 5

# Områden där andelen bygger på färre än så här ingår inte i rangordningen (högst och lägst),
# eftersom små underlag lätt ger extrema andelar. De visas i kartan, diagrammen och tabellen.
shb_min_rangordning <- 50

# Ägarkategori som är förvald
shb_agarkategori_standard <- "Totalt"

# Antal områden i listorna över högst och lägst värde
shb_antal_rangordning <- 15

# Källrad längst ner i diagrammen, skrivs ut precis som den står här
KALLA_SHB <- "Källa: Dalarnas mikrodatabas (SCB), bearbetning av Samhällsanalys, Region Dalarna"

# ---- 2. Läs in kartor ----

kommun_sf <- tbl(shiny_uppkoppling_las("geodata"), dbplyr::in_schema("karta", "kommun_scb")) %>%
  filter(str_sub(knkod, 1, 2) == "20") %>%                                               # filtrera ut Dalarnas kommuner
  collect() %>%
  df_till_sf() %>%
  select(kommunkod = knkod, kommunnamn = knnamn) %>%
  st_transform(crs = 4326)

shb_omraden_sf <- hamta_shb_omraden(shiny_uppkoppling_las("geodata"), kommun_sf, shb_kol)

# ---- 3. Läs in statistik ----

shb_statistik <- hamta_shb_statistik(shb_stat_tabell, kommun_sf, shb_omraden_sf, shb_min_befolkning_omrade, shb_min_grupp, shb_min_taljare)
shb_exempeldata <- isTRUE(attr(shb_statistik, "exempeldata"))
shb_indikatorer <- skapa_indikatorlista(shb_statistik)
geografinamn <- skapa_geografinamn(kommun_sf, shb_omraden_sf)

# färgvektor
rus_tre_fokus <- c("#93cec1", "#178571", "#000000")
