KALLA_TEXT <- if (shb_exempeldata) "Exempeldata – slumpade värden, inte riktig statistik" else KALLA_SHB

# Färger som i brott-appen
kartpalett       <- "YlOrRd"
farg_stapel      <- "#3182bd"
farg_vald        <- "#e31a1c"      # valt område i diagrammen
farg_markering   <- "#1f1f1f"      # kontur runt valt område i kartan, med vit kant så att den syns mot alla färger
farg_nedtonad    <- "#c6d4e1"      # områden utanför vald kommun
farg_kommun      <- "#0f7090"
farg_lan         <- "#54a1bd"
farg_riket       <- "#8edded"
farg_ungefarlig  <- "#a9cbe6"      # staplar och punkter med ungefärligt värde (färre än 5), ritade vid gränsen
farg_ungefarlig_karta <- "#cccccc"
farg_restyta     <- "#ffffff"      # del av kommunen som inte ingår i något shb-område

# Svenska texter till tabellen (DataTables)
dt_svenska <- list(
  search = "Sök:", lengthMenu = "Visa _MENU_ rader", zeroRecords = "Inga områden matchar sökningen",
  info = "Visar _START_–_END_ av _TOTAL_ områden", infoEmpty = "Inga områden", infoFiltered = "(filtrerat från _MAX_)",
  paginate = list(first = "Första", last = "Sista", `next` = "Nästa", previous = "Föregående")
)

# Listrutans val: indikatorkod som värde och namn som text, grupperade efter indikatorgrupp.
# as.list behövs för att grupper med en enda indikator ska visas som grupp.
indikatorval <- lapply(split(setNames(shb_indikatorer$indikator, shb_indikatorer$indikator_namn),
                             shb_indikatorer$grupp, drop = TRUE), as.list)
agarkategorier <- intersect(c("Totalt", "Allmännyttan", "Övriga ägare", "Uppgift saknas"),
                            unique(shb_statistik$agarkategori))

# Kommuner som saknar shb-områden, t.ex. Gagnef
kommuner_utan_omraden <- setdiff(kommun_sf$kommunkod, shb_omraden_sf$kommunkod[!shb_omraden_sf$restyta])
restytor <- shb_omraden_sf$omradeskod[shb_omraden_sf$restyta]

shinyServer(function(input, output, session) {
  rdshinyappar::telemetri_server(telemetry, navigation_id = 'flikval', forsta_flik = 'Karta och diagram')

  kartniva     <- reactiveVal("kommun")         # "kommun" = alla kommuner, "omrade" = shb-områden i vald kommun
  vald_kommun  <- reactiveVal(NULL)             # kommunkod
  valt_omrade  <- reactiveVal(NULL)             # omradeskod, markeras i karta och diagram

  # ---- Val av indikator, ägarkategori och år ----
  # Valen finns på ett ställe (valt) och båda flikarnas listrutor (val_* och jmf_*) visar dem.
  # När appen själv ändrar en listruta sparas värdet i vantar tills listrutan rapporterat det tillbaka,
  # så att ändringen inte tolkas som användarens val. Rapporter som kommer i otakt ignoreras också.
  # Ett väntande värde gäller i högst tio sekunder, så att en listruta aldrig fastnar. Tiden behöver
  # räcka även när servern är upptagen med att rita diagram innan listrutan hinner svara.
  valt <- reactiveValues(
    indikator    = shb_indikatorer$indikator[1],
    agarkategori = shb_agarkategori_standard,
    ar           = NULL
  )
  ar_anvandare <- reactiveVal(NULL)        # år som användaren själv valt, NULL = visa alltid senaste
  vantar <- list()

  satt_listruta <- function(id, varde, choices = NULL) {
    if (!identical(isolate(input[[id]]), varde)) vantar[[id]] <<- list(varde = varde, tid = Sys.time())
    updateSelectInput(session, id, choices = choices, selected = varde)
  }

  listrutor <- list(indikator = c("val_indikator", "jmf_indikator"),
                    agarkategori = c("val_agarkategori", "jmf_agarkategori", "prof_agarkategori"),
                    ar = c("val_ar", "jmf_ar"))

  for (id in listrutor$indikator) satt_listruta(id, isolate(valt$indikator), indikatorval)
  for (id in listrutor$agarkategori) satt_listruta(id, isolate(valt$agarkategori), agarkategorier)

  for (falt in names(listrutor)) for (id in listrutor[[falt]]) local({
    falt <- falt
    id <- id
    observeEvent(input[[id]], {
      varde <- input[[id]]
      v <- vantar[[id]]
      if (!is.null(v) && difftime(Sys.time(), v$tid, units = "secs") < 10) {   # appens egen ändring eller en rapport i otakt
        if (identical(varde, v$varde)) vantar[[id]] <<- NULL
        return()
      }
      vantar[[id]] <<- NULL
      if (!isTruthy(varde) || identical(varde, valt[[falt]])) return()
      valt[[falt]] <- varde                                   # användaren har valt
      if (falt == "ar") ar_anvandare(varde)
      for (annan in setdiff(listrutor[[falt]], id)) satt_listruta(annan, varde)
    }, ignoreInit = TRUE)
  })

  # År: bara år där indikatorn har värden går att välja. Så länge användaren inte själv valt ett år visas
  # det senaste året för varje indikator. Ett år som användaren valt ligger kvar vid byte av indikator,
  # och saknas det för den nya indikatorn visas det senaste utan att valet glöms.
  observeEvent(valt$indikator, {
    ar <- shb_statistik %>% filter(indikator == valt$indikator) %>% pull(ar) %>% unique() %>% sort(decreasing = TRUE)
    ar <- as.character(ar)
    onskat <- ar_anvandare()
    visat <- if (!is.null(onskat) && onskat %in% ar) onskat else ar[1]

    if (!is.null(onskat) && !(onskat %in% ar)) {
      showNotification(paste0(indikator_info(valt$indikator)$indikator_rubrik, " finns inte för ", onskat,
                              ". Visar ", visat, "."), type = "message", duration = 6)
    }

    valt$ar <- visat
    for (id in listrutor$ar) satt_listruta(id, visat, ar)
  })

  indikator_info <- function(id) shb_indikatorer[shb_indikatorer$indikator == id, ][1, ]

  # Statistiken för vald indikator och ägarkategori, alla år och geografier
  urval <- reactive({
    shb_statistik %>% filter(indikator == valt$indikator, agarkategori == valt$agarkategori)
  })

  # Text om vad som visas, t.ex. "Andel trångbodda hushåll (allmännyttan)"
  urvalstext <- function() {
    paste0(indikator_info(valt$indikator)$indikator_rubrik,
           if (!identical(valt$agarkategori, "Totalt")) paste0(" (", tolower(valt$agarkategori), ")"))
  }

  axeltext <- function(info) if (info$typ == "andel") "Andel (%)" else paste("Antal", tolower(info$enhet))

  vald_kommun_namn <- reactive({
    req(vald_kommun())
    kommun_sf$kommunnamn[kommun_sf$kommunkod == vald_kommun()]
  })

  # "Moras" för tooltipen på restytor, som bara visas när en kommun är vald
  vald_kommun_namn_eller_tom <- function() {
    namn <- if (is.null(vald_kommun())) "kommunens" else vald_kommun_namn()
    if (grepl("s$", namn)) namn else paste0(namn, "s")
  }

  val_info <- reactive(indikator_info(valt$indikator))

  # ---- Data för kartan och geografidiagrammet ----
  # sf-objekt med kolumnerna kod, namn och värden för den geografi som visas
  data_karta <- reactive({
    req(valt$ar)

    geo <- if (kartniva() == "kommun") {
      kommun_sf %>% select(kod = kommunkod, namn = kommunnamn) %>% mutate(restyta = FALSE)
    } else {
      shb_omraden_sf %>% filter(kommunkod == vald_kommun()) %>% select(kod = omradeskod, namn = omradesnamn, restyta)
    }

    varden <- urval() %>%
      filter(ar == as.integer(valt$ar)) %>%
      select(kod = regionkod, varde, varde_max, varde_min, farre_an, farre_utan, taljare, namnare, enhet, skyddad, kommentar)

    left_join(geo, varden, by = "kod")
  })

  # ---- Klick: kommun -> områden, område -> markera ----
  klicka_geografi <- function(kod) {
    if (kartniva() == "kommun") {
      if (kod %in% kommuner_utan_omraden) {
        showNotification(paste(kommun_sf$kommunnamn[kommun_sf$kommunkod == kod], "är inte indelad i shb-områden."),
                         type = "message", duration = 5)
        return()
      }
      vald_kommun(kod)
      valt_omrade(NULL)
      kartniva("omrade")
    } else if (kod %in% restytor) {
      showNotification("Den här delen av kommunen ingår inte i något shb-område.", type = "message", duration = 5)
    } else if (identical(valt_omrade(), kod)) {
      valt_omrade(NULL)
    } else {
      valt_omrade(kod)
    }
  }

  tillbaka_till_kommuner <- function() {
    kartniva("kommun")
    vald_kommun(NULL)
    valt_omrade(NULL)
  }

  observeEvent(input$karta_shb_shape_click, {
    req(input$karta_shb_shape_click$id)
    klicka_geografi(input$karta_shb_shape_click$id)
  })

  observeEvent(input$diagram_geografi_selected, {
    kod <- input$diagram_geografi_selected
    # Rensa selection visuellt så nästa klick alltid registreras
    session$sendCustomMessage(type = "diagram_geografi_set", message = character(0))
    klicka_geografi(kod)
  })

  observeEvent(input$reset_map, {
    tillbaka_till_kommuner()
    shinyjs::runjs("Shiny.setInputValue('reset_map', null);")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$geografi_tillbaka, tillbaka_till_kommuner())

  # ---- Karta ----
  # Baskartan ritas en gång, polygonerna uppdateras sedan via leafletProxy
  output$karta_shb <- renderLeaflet({
    bbox <- st_bbox(kommun_sf)
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik", options = providerTileOptions(opacity = 0.45)) %>%
      fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]) %>%
      addEasyButton(
        easyButton(
          icon = "fa-home",
          title = "Visa alla kommuner igen",
          onClick = JS("function(btn, map){ Shiny.setInputValue('reset_map', true); }")
        )
      )
  })

  observe({
    df_map <- data_karta()
    info <- val_info()
    vald <- if (kartniva() == "omrade") valt_omrade() else NULL
    df_map$vald_geom <- !is.null(vald) & df_map$kod %in% vald

    doman <- if (any(!is.na(df_map$varde))) df_map$varde else 0
    pal <- colorNumeric(kartpalett, domain = doman, na.color = "transparent")
    df_map$ungefarlig <- df_map$farre_an %in% TRUE | df_map$farre_utan %in% TRUE
    df_map$fyllning <- case_when(df_map$restyta ~ farg_restyta,
                                 df_map$ungefarlig ~ farg_ungefarlig_karta,
                                 TRUE ~ pal(df_map$varde))

    proxy <- leafletProxy("karta_shb", data = df_map) %>%
      clearShapes() %>%
      clearControls()

    if (all(is.na(df_map$varde) & !df_map$ungefarlig)) {
      proxy %>% addControl("Inga data för valt urval", position = "topright", className = "map-filter-text")
      return()
    }

    etiketter <- lapply(ifelse(
      df_map$restyta,
      paste0("<b>Ingår inte i något shb-område</b><br>", "Invånarna här räknas med i ", vald_kommun_namn_eller_tom(), " värde"),
      paste0(
        df_map$namn, "<br>",
        "<b>", urvalstext(), "</b><br>",
        formatera_varde(df_map$varde, df_map$taljare, df_map$namnare, info$enhet, df_map$kommentar, df_map$varde_max, df_map$varde_min), "<br>",
        "<i>År ", valt$ar, "</i>"
      )
    ), HTML)

    proxy %>%
      addPolygons(
        layerId     = ~kod,
        fillColor   = ~fyllning,
        fillOpacity = ~ifelse(restyta, 0.35, 0.6),
        color       = "#555555",
        weight      = 0.7,
        label       = etiketter,
        highlightOptions = highlightOptions(weight = 2, color = "#444444", bringToFront = FALSE)
      ) %>%
      addLegend(
        "bottomleft", pal = pal, values = ~varde,
        title = axeltext(info),
        labFormat = labelFormat(big.mark = " ", suffix = if (info$typ == "andel") " %" else ""),
        className = "info legend kompakt-legend"
      ) %>%
      { if (any(df_map$restyta)) {
          addLegend(., "bottomleft", colors = farg_restyta, labels = "Ingår inte i något shb-område",
                    className = "info legend kompakt-legend")
        } else . } %>%
      { if (any(df_map$ungefarlig)) {
          addLegend(., "bottomleft", colors = farg_ungefarlig_karta, labels = paste0("Ungefärligt värde (färre än ", shb_min_taljare, ")"),
                    className = "info legend kompakt-legend")
        } else . } %>%
      addControl(
        HTML(paste0(urvalstext(), "<br>",
                    if (kartniva() == "kommun") "Dalarna" else vald_kommun_namn(), "<br>",
                    "År ", valt$ar,
                    if (shb_exempeldata) "<br><b>Exempeldata</b>" else "")),
        position = "topright",
        className = "map-filter-text"
      ) %>%
      addControl(
        HTML(paste0("<i class='fa fa-hand-pointer'></i> ",
                    if (kartniva() == "kommun") "Klicka på en kommun" else "Klicka på ett område")),
        position = "bottomright",
        className = "map-klick-hint"
      )

    # Valt område behåller sin färg och markeras med en tjock mörk kontur med vit kant runt och
    # namnet som etikett, så att markeringen inte kan förväxlas med kartans färgskala.
    # Konturerna tar inte emot klick, så ett nytt klick på området når ytan under och avmarkerar.
    vald_sf <- df_map[df_map$vald_geom, ]
    if (nrow(vald_sf) > 0) {
      proxy %>%
        addPolygons(data = vald_sf, fill = FALSE, color = "white", weight = 8, opacity = 1,
                    options = pathOptions(interactive = FALSE)) %>%
        addPolygons(data = vald_sf, fill = FALSE, color = farg_markering, weight = 4, opacity = 1,
                    options = pathOptions(interactive = FALSE),
                    label = vald_sf$namn,
                    labelOptions = labelOptions(noHide = TRUE, direction = "top", className = "vald-etikett"))
    }
  })

  # Zooma bara när geografin byts, inte när indikator eller år byts
  observeEvent(list(kartniva(), vald_kommun()), {
    geo <- if (kartniva() == "kommun") kommun_sf else shb_omraden_sf %>% filter(kommunkod == vald_kommun())
    bbox <- st_bbox(geo)
    leafletProxy("karta_shb") %>%
      fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]],
                options = list(paddingTopLeft = c(80, 5), paddingBottomRight = c(5, 5)))
  }, ignoreInit = TRUE)

  # Text om områden som inte visas av sekretesskäl, NULL om inga
  text_skyddade <- function(skyddad) {
    antal <- sum(skyddad, na.rm = TRUE)
    if (antal == 0) return(NULL)
    paste0(antal, if (antal == 1) " område visas inte" else " områden visas inte", " av sekretesskäl.")
  }

  # Ungefärliga värden: färre än 5 har (farre_an) eller saknar (farre_utan) egenskapen. De ritas vid sin gräns.
  med_ungefarliga <- function(df) {
    df %>%
      filter(!is.na(varde) | farre_an | farre_utan) %>%
      mutate(ungefarlig = farre_an | farre_utan, y = coalesce(varde, varde_max, varde_min))
  }

  text_ungefarliga <- function(ungefarlig, form = "staplar") {
    if (!any(ungefarlig %in% TRUE)) return(NULL)
    paste0("Ljusa ", form, ": ungefärliga värden där färre än ", shb_min_taljare,
           " har eller saknar egenskapen, ritade vid gränsvärdet.")
  }

  # ---- Diagram: värde per kommun eller område ----
  output$diagram_geografi <- renderGirafe({
    vald <- if (kartniva() == "omrade") valt_omrade() else NULL
    info <- val_info()
    alla <- data_karta() %>% st_drop_geometry()

    df_diag <- alla %>%
      med_ungefarliga() %>%
      mutate(
        farg = case_when(kod %in% vald ~ farg_vald, ungefarlig ~ farg_ungefarlig, TRUE ~ farg_stapel),
        etikett = paste0(namn, "<br>", formatera_varde(varde, taljare, namnare, enhet, kommentar, varde_max, varde_min))
      )

    validate(need(nrow(df_diag) > 0, "Inga data för valt urval"))

    titel <- if (kartniva() == "kommun") {
      paste0(urvalstext(), " per kommun, ", valt$ar)
    } else {
      paste0(urvalstext(), " per område i ", vald_kommun_namn(), ", ", valt$ar)
    }
    storlek <- diagram_storlek(session, "diagram_geografi")

    # Med många områden blir namnen på x-axeln oläsliga, då visas de bara vid hovring
    manga <- nrow(df_diag) > 25
    underrubrik <- paste(c(if (manga) "Håll muspekaren över en stapel för att se områdets namn.",
                           text_ungefarliga(df_diag$ungefarlig), text_skyddade(alla$skyddad)), collapse = " ")

    p <- ggplot(df_diag, aes(x = reorder(namn, y), y = y)) +
      geom_col_interactive(aes(tooltip = etikett, data_id = kod, fill = farg), color = NA) +
      scale_fill_identity() +
      scale_y_continuous(labels = formatera_tal) +
      scale_x_discrete(labels = function(x) str_trunc(x, 20)) +
      labs(x = NULL, y = axeltext(info), title = radbryt(titel, storlek$width),
           subtitle = if (nzchar(underrubrik)) radbryt(underrubrik, storlek$width, storlek_pt = diagram_caption_storlek + 1, fet = FALSE),
           caption = KALLA_TEXT) +
      tema_diagram() +
      theme(axis.text.x = if (manga) element_blank() else element_text(angle = 45, hjust = 1),
            plot.subtitle = element_text(size = diagram_caption_storlek + 1, color = "#444"),
            legend.position = "none")

    skapa_girafe(p, klickbar = TRUE, width = storlek$width, height = storlek$height)
  })

  output$geografi_klick_text <- renderText({
    if (kartniva() == "kommun") "Klicka på en kommun för att se områden" else "Klicka på ett område för att markera"
  })

  # ---- Diagram: utveckling över tid ----
  # Valt område, vald kommun, Dalarna och riket
  geografier_tid <- reactive({
    c(if (kartniva() == "omrade") valt_omrade(), vald_kommun(), "20", "00")
  })

  output$diagram_tid <- renderGirafe({
    info <- val_info()

    df_tid <- urval() %>%
      filter(regionkod %in% geografier_tid()) %>%
      med_ungefarliga() %>%
      left_join(geografinamn %>% select(regionkod, namn), by = "regionkod") %>%
      mutate(etikett = paste0(namn, " ", ar, "<br>", formatera_varde(varde, taljare, namnare, enhet, kommentar, varde_max, varde_min)))

    validate(need(nrow(df_tid) > 0, "Inga data för valt urval"))

    storlek <- diagram_storlek(session, "diagram_tid")
    koder <- intersect(geografier_tid(), df_tid$regionkod)
    farger <- c(farg_riket, farg_lan, farg_kommun, farg_vald)[match(koder, c("00", "20", vald_kommun(), valt_omrade()))]
    namn <- geografinamn$namn[match(koder, geografinamn$regionkod)]

    # Ofyllda punkter: ungefärliga värden, ritade vid gränsvärdet
    p <- ggplot(df_tid, aes(x = ar, y = y, color = regionkod, group = regionkod)) +
      { if (n_distinct(df_tid$ar) > 1) geom_line(linewidth = 1) } +          # en linje kräver minst två år
      geom_point_interactive(aes(tooltip = etikett, data_id = paste(regionkod, ar), shape = ungefarlig), size = 2,
                             fill = "white", stroke = 1) +
      scale_shape_manual(values = c(`FALSE` = 19, `TRUE` = 21), guide = "none") +
      scale_color_manual(values = setNames(farger, koder), labels = setNames(namn, koder), breaks = koder, name = NULL) +
      scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
      scale_y_continuous(labels = formatera_tal) +
      labs(x = NULL, y = axeltext(info),
           title = radbryt(paste0(urvalstext(), " över tid"), storlek$width),
           caption = KALLA_TEXT) +
      tema_diagram() +
      theme(legend.position = "top", legend.justification = "left")

    skapa_girafe(p, width = storlek$width, height = storlek$height)
  })

  # ---- Diagram: alla områden i länet, per kommun ----
  # En punkt per område, strecket visar kommunens värde. Kommunerna sorteras efter sitt värde.
  output$diagram_alla <- renderGirafe({
    req(valt$ar)
    info <- val_info()

    varden <- urval() %>% filter(ar == as.integer(valt$ar))

    omr_alla <- st_drop_geometry(shb_omraden_sf) %>%
      inner_join(varden, by = c("omradeskod" = "regionkod"))
    omr <- omr_alla %>% med_ungefarliga()

    validate(need(nrow(omr) > 0, "Inga data för valt urval"))

    kommuner <- st_drop_geometry(kommun_sf) %>%
      inner_join(varden %>% filter(!is.na(varde)), by = c("kommunkod" = "regionkod"))
    ordning <- unique(c(kommuner$kommunnamn[order(kommuner$varde)], sort(unique(omr$kommunnamn))))

    vald_k <- vald_kommun()
    vald_o <- if (kartniva() == "omrade") valt_omrade() else NULL

    # Spridning i sidled utan slumptal: set.seed() skulle nollställa slumpgeneratorn som
    # ggiraph använder för diagrammens id, så att flera diagram får samma id och ritas fel
    omr <- omr %>%
      arrange(omradeskod) %>%
      group_by(kommunnamn) %>%
      mutate(sidled = (row_number() * 0.618034) %% 1 * 0.6 - 0.3) %>%
      ungroup() %>%
      mutate(
        x = match(kommunnamn, ordning) + sidled,
        i_fokus = if (is.null(vald_k)) TRUE else kommunkod == vald_k,
        farg = case_when(omradeskod %in% vald_o ~ farg_vald, !i_fokus ~ farg_nedtonad,
                         ungefarlig ~ farg_ungefarlig, TRUE ~ farg_stapel),
        etikett = paste0(omradesnamn, ", ", kommunnamn, "<br>", formatera_varde(varde, taljare, namnare, enhet, kommentar, varde_max, varde_min))
      ) %>%
      arrange(i_fokus, omradeskod %in% vald_o)               # valda områden ritas överst

    kommuner <- kommuner %>%
      mutate(x = match(kommunnamn, ordning),
             etikett = paste0(kommunnamn, " (hela kommunen)<br>", formatera_varde(varde, taljare, namnare, enhet, kommentar, varde_max, varde_min)))

    titel <- paste0(urvalstext(), " per område och kommun, ", valt$ar)
    underrubrik <- paste(c("Punkterna är områden, strecken visar kommunens värde.", text_ungefarliga(omr$ungefarlig, "punkter"),
                           text_skyddade(omr_alla$skyddad)), collapse = " ")
    storlek <- diagram_storlek(session, "diagram_alla")

    p <- ggplot() +
      geom_point_interactive(data = omr, aes(x = x, y = y, tooltip = etikett, data_id = omradeskod, fill = farg),
                             shape = 21, color = "white", stroke = 0.3, size = 2.4) +
      geom_segment_interactive(data = kommuner, aes(x = x - 0.4, xend = x + 0.4, y = varde, yend = varde, tooltip = etikett),
                               color = farg_kommun, linewidth = 1.1) +
      scale_fill_identity() +
      scale_x_continuous(breaks = seq_along(ordning), labels = ordning, expand = expansion(add = 0.5)) +
      scale_y_continuous(labels = formatera_tal) +
      labs(x = NULL, y = axeltext(info), title = radbryt(titel, storlek$width),
           subtitle = radbryt(underrubrik, storlek$width, storlek_pt = diagram_caption_storlek + 1, fet = FALSE),
           caption = KALLA_TEXT) +
      tema_diagram() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1),
            panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
            plot.subtitle = element_text(size = diagram_caption_storlek + 1, color = "#444"),
            legend.position = "none")

    skapa_girafe(p, klickbar = TRUE, width = storlek$width, height = storlek$height)
  })

  # Klick på ett område: gå till kommunen och markera området
  observeEvent(input$diagram_alla_selected, {
    kod <- input$diagram_alla_selected
    session$sendCustomMessage(type = "diagram_alla_set", message = character(0))

    kommunkod <- shb_omraden_sf$kommunkod[shb_omraden_sf$omradeskod == kod][1]
    req(kommunkod)

    if (kartniva() == "omrade" && identical(valt_omrade(), kod)) {
      valt_omrade(NULL)
    } else {
      vald_kommun(kommunkod)
      kartniva("omrade")
      valt_omrade(kod)
    }
  })

  # ---- Fliken Jämför områden ----

  # Visa ett område i kartfliken: byt flik först så att kartan är synlig när den zoomar
  visa_i_kartan <- function(kod) {
    kommunkod <- shb_omraden_sf$kommunkod[shb_omraden_sf$omradeskod == kod][1]
    req(kommunkod)
    updateTabsetPanel(session, "flikval", selected = "Karta och diagram")
    vald_kommun(kommunkod)
    kartniva("omrade")
    valt_omrade(kod)
  }

  jmf_info <- val_info

  # Alla områden för vald indikator, ägarkategori och år, även de som inte visas av sekretesskäl
  rangordning <- reactive({
    req(valt$ar)
    st_drop_geometry(shb_omraden_sf) %>%
      inner_join(urval() %>% filter(ar == as.integer(valt$ar)), by = c("omradeskod" = "regionkod")) %>%
      mutate(utanfor_rangordning = typ == "andel" & !skyddad & namnare < shb_min_rangordning)
  })

  output$jmf_info <- renderText({
    df <- rangordning()
    antal_skyddade <- sum(df$skyddad)
    antal_utanfor <- sum(!df$skyddad & df$utanfor_rangordning)
    paste(c(
      if (antal_skyddade > 0) paste0(antal_skyddade, " av ", nrow(df), " områden visas inte av sekretesskäl."),
      if (antal_utanfor > 0) paste0(antal_utanfor, " områden ingår inte i rangordningen eftersom andelen bygger på färre än ",
                                    shb_min_rangordning, " ", tolower(jmf_info()$enhet), ".")
    ), collapse = " ")
  })

  rangordningsdiagram <- function(output_id, hogst) {
    info <- jmf_info()
    # Ungefärliga värden rangordnas efter sin gräns, på det försiktiga hållet: i listan över högst
    # räknas "över X %" som X och "under X %" som 0, i listan över lägst räknas "under X %" som X och
    # "över X %" som 100. Då kommer ett område bara med om det säkert hör hemma i listan.
    alla <- rangordning() %>%
      filter(!utanfor_rangordning) %>%
      med_ungefarliga() %>%
      mutate(rang = if (hogst) coalesce(varde, varde_min, if_else(farre_an, 0, NA_real_))
                    else coalesce(varde, varde_max, if_else(farre_utan, 100, NA_real_)))
    # Lika värden sorteras på namn så att urvalet inte blir slumpmässigt
    df <- alla %>%
      arrange(if (hogst) desc(rang) else rang, kommunnamn, omradesnamn) %>%
      slice_head(n = shb_antal_rangordning)

    validate(need(nrow(df) > 0, "Inga data för valt urval"))

    # Har fler områden samma värde som det sista i listan, t.ex. många med 0 %, sägs det i underrubriken
    sista <- df$rang[nrow(df)]
    fler_lika <- sum(alla$rang == sista & !alla$ungefarlig) - sum(df$rang == sista & !df$ungefarlig)
    if (df$ungefarlig[nrow(df)]) fler_lika <- 0
    text_lika <- if (fler_lika > 0) {
      paste0("Ytterligare ", fler_lika, " områden har också ", formatera_tal(sista),
             if (info$typ == "andel") " %" else "", " men ryms inte i listan.")
    }

    df <- df %>%
      mutate(
        axeltext = paste0(str_trunc(omradesnamn, 35), " (", kommunnamn, ")"),
        axeltext = factor(axeltext, levels = rev(unique(axeltext))),        # första området överst
        etikett = paste0(omradesnamn, ", ", kommunnamn, "<br>", formatera_varde(varde, taljare, namnare, enhet, kommentar, varde_max, varde_min))
      )

    dalarna <- urval() %>% filter(regionkod == "20", ar == as.integer(valt$ar))

    storlek <- diagram_storlek(session, output_id)
    titel <- paste0("De ", nrow(df), " områden med ", if (hogst) "högst " else "lägst ", info$typ,
                    ": ", urvalstext(), ", ", valt$ar)

    p <- ggplot(df, aes(x = y, y = axeltext)) +
      geom_col_interactive(aes(tooltip = etikett, data_id = omradeskod, fill = ifelse(ungefarlig, farg_ungefarlig, farg_stapel)),
                           width = 0.75) +
      scale_fill_identity() +
      scale_x_continuous(labels = formatera_tal, expand = expansion(mult = c(0, 0.05))) +
      labs(x = axeltext(info), y = NULL, title = radbryt(titel, storlek$width), caption = KALLA_TEXT) +
      tema_diagram() +
      theme(panel.grid.major.y = element_blank(), legend.position = "none")

    # Dalarna som referens bara för andelar, länets totala antal går inte att jämföra med ett område
    if (info$typ == "andel" && nrow(dalarna) == 1 && !is.na(dalarna$varde)) {
      p <- p +
        geom_vline_interactive(xintercept = dalarna$varde, color = farg_kommun, linetype = "dashed", linewidth = 0.8,
                               tooltip = paste0("Dalarna<br>", formatera_varde(dalarna$varde, dalarna$taljare,
                                                                               dalarna$namnare, dalarna$enhet))) +
        labs(subtitle = radbryt(paste(c(paste0("Streckad linje: Dalarna ", formatera_tal(dalarna$varde), " %."),
                                        text_ungefarliga(df$ungefarlig), text_lika), collapse = " "),
                                storlek$width, storlek_pt = diagram_caption_storlek + 1, fet = FALSE)) +
        theme(plot.subtitle = element_text(size = diagram_caption_storlek + 1, color = farg_kommun))
    } else if (!is.null(text_lika) || any(df$ungefarlig)) {
      p <- p +
        labs(subtitle = radbryt(paste(c(text_ungefarliga(df$ungefarlig), text_lika), collapse = " "),
                                storlek$width, storlek_pt = diagram_caption_storlek + 1, fet = FALSE)) +
        theme(plot.subtitle = element_text(size = diagram_caption_storlek + 1, color = "#444"))
    }

    skapa_girafe(p, klickbar = TRUE, width = storlek$width, height = storlek$height)
  }

  output$diagram_hogst <- renderGirafe(rangordningsdiagram("diagram_hogst", hogst = TRUE))
  output$diagram_lagst <- renderGirafe(rangordningsdiagram("diagram_lagst", hogst = FALSE))

  observeEvent(input$diagram_hogst_selected, {
    session$sendCustomMessage(type = "diagram_hogst_set", message = character(0))
    visa_i_kartan(input$diagram_hogst_selected)
  })

  observeEvent(input$diagram_lagst_selected, {
    session$sendCustomMessage(type = "diagram_lagst_set", message = character(0))
    visa_i_kartan(input$diagram_lagst_selected)
  })

  # Tabell med alla områden, sorterad med högst värde först
  tabell_data <- reactive({
    rangordning() %>%
      arrange(desc(varde)) %>%
      mutate(kommentar = case_when(
        skyddad ~ kommentar,
        farre_an ~ paste0("Färre än ", shb_min_taljare, ", andelen är under ", formatera_tal(varde_max), " %"),
        farre_utan ~ paste0("Alla utom färre än ", shb_min_taljare, ", andelen är över ", formatera_tal(varde_min), " %"),
        utanfor_rangordning ~ paste0("Ingår inte i rangordningen: färre än ", shb_min_rangordning, " ", tolower(enhet)),
        TRUE ~ ""
      ))
  })

  output$tabell_omraden <- renderDT({
    df <- tabell_data()
    info <- jmf_info()
    enhet <- tolower(info$enhet)

    df <- df %>% mutate(profil = sprintf('<a href="#" class="profil-lank" data-kod="%s">Profil</a>',
                                         htmltools::htmlEscape(omradeskod, attribute = TRUE)))

    visning <- if (info$typ == "andel") {
      df %>% transmute(Kommun = kommunnamn, `Område` = omradesnamn, `Andel (%)` = varde,
                       !!paste0("Täljare (", enhet, ")") := taljare,
                       !!paste0("Nämnare (", enhet, ")") := namnare,
                       Kommentar = kommentar, ` ` = profil)
    } else {
      df %>% transmute(Kommun = kommunnamn, `Område` = omradesnamn, !!paste0("Antal ", enhet) := varde, ` ` = profil)
    }

    tabell <- datatable(
      visning,
      rownames = FALSE,
      escape = setdiff(names(visning), " "),                 # länken till områdesprofilen är HTML
      selection = "single",
      class = "compact stripe hover",
      options = list(pageLength = 15, lengthMenu = c(15, 50, 100), order = list(list(2, "desc")),
                     columnDefs = list(list(orderable = FALSE, targets = ncol(visning) - 1)),
                     language = dt_svenska)
    )

    if (info$typ == "andel") {
      tabell %>%
        formatRound("Andel (%)", digits = 1, mark = " ", dec.mark = ",") %>%
        formatRound(names(visning)[4:5], digits = 0, mark = " ")
    } else {
      tabell %>% formatRound(names(visning)[3], digits = 0, mark = " ")
    }
  })

  observeEvent(input$tabell_omraden_rows_selected, {
    kod <- tabell_data()$omradeskod[input$tabell_omraden_rows_selected]
    selectRows(dataTableProxy("tabell_omraden"), NULL)
    visa_i_kartan(kod)
  })

  # ---- Fliken Områdesprofil ----
  # Ett område, eller flera som räknas ihop, med alla indikatorer jämfört med kommunen, Dalarna och riket.
  # Sammanräkningen och dess sekretessregler finns i R/profil.R.

  alla_ar <- sort(unique(shb_statistik$ar), decreasing = TRUE)
  updateSelectInput(session, "prof_ar", choices = alla_ar, selected = alla_ar[1])

  # Områden grupperade per kommun, utan restytor
  omradesval <- shb_omraden_sf %>%
    st_drop_geometry() %>%
    filter(!restyta) %>%
    arrange(kommunnamn, omradesnamn)
  omradesval <- lapply(split(setNames(omradesval$omradeskod, omradesval$omradesnamn), omradesval$kommunnamn), as.list)

  # Profil som öppnas via länk, t.ex. ?flik=omradesprofil&omraden=2081_Tjärna%20Ängar|2081_Bullermyren
  lank <- isolate(parseQueryString(session$clientData$url_search))
  lank_omraden <- if (!is.null(lank$omraden)) intersect(strsplit(lank$omraden, "|", fixed = TRUE)[[1]], shb_omraden_sf$omradeskod)
  updateSelectizeInput(session, "profil_omraden", choices = omradesval, selected = lank_omraden, server = FALSE)
  if (identical(lank$flik, "omradesprofil")) {
    updateTabsetPanel(session, "flikval", selected = "Områdesprofil")
    if (!is.null(lank$agarkategori) && lank$agarkategori %in% agarkategorier) {
      valt$agarkategori <- lank$agarkategori
      for (id in listrutor$agarkategori) satt_listruta(id, lank$agarkategori)
    }
    if (!is.null(lank$ar) && lank$ar %in% alla_ar) updateSelectInput(session, "prof_ar", selected = lank$ar)
  }

  # Adressen följer profilen, så att den går att spara som bokmärke eller skicka som länk
  observe({
    koder <- input$profil_omraden
    if (identical(input$flikval, "Områdesprofil") && length(koder) > 0) {
      fraga <- paste0("?flik=omradesprofil&omraden=", URLencode(paste(koder, collapse = "|"), reserved = TRUE),
                      if (!identical(valt$agarkategori, "Totalt")) paste0("&agarkategori=", URLencode(valt$agarkategori, reserved = TRUE)),
                      if (isTruthy(input$prof_ar) && input$prof_ar != alla_ar[1]) paste0("&ar=", input$prof_ar))
      updateQueryString(fraga, mode = "replace")
    } else {
      updateQueryString("?", mode = "replace")
    }
  })

  # Öppna profilen för ett område från kartfliken eller tabellen i Jämför områden
  ga_till_profil <- function(kod) {
    updateSelectizeInput(session, "profil_omraden", selected = kod)
    updateTabsetPanel(session, "flikval", selected = "Områdesprofil")
  }
  observe(shinyjs::toggleState("till_profil", condition = kartniva() == "omrade" && !is.null(valt_omrade())))
  observeEvent(input$till_profil, { req(valt_omrade()); ga_till_profil(valt_omrade()) })
  observeEvent(input$visa_profil, ga_till_profil(input$visa_profil))

  profil_koder <- reactive({
    validate(need(length(input$profil_omraden) > 0, "Välj ett eller flera områden ovan."))
    input$profil_omraden
  })

  profil_kommuner <- reactive({
    unique(shb_omraden_sf$kommunkod[shb_omraden_sf$omradeskod %in% profil_koder()])
  })

  # De valda områdena och jämförelserna i samma tabell: geografi, indikator, år och värden som intervall.
  # Kommunen är med bara när alla valda områden ligger i samma kommun.
  profil_data <- reactive({
    koder <- profil_koder()
    agar <- shb_statistik %>% filter(agarkategori == valt$agarkategori)

    valda <- sla_ihop_omraden(agar, koder, shb_min_taljare) %>% mutate(geografi = "Valda områden", ordning = 1)
    ref_koder <- c(if (length(profil_kommuner()) == 1) profil_kommuner(), "20", "00")
    ref <- agar %>%
      filter(regionkod %in% ref_koder) %>%
      som_intervall(shb_min_taljare) %>%
      mutate(geografi = geografinamn$namn[match(regionkod, geografinamn$regionkod)],
             ordning = match(regionkod, ref_koder) + 1)

    bind_rows(valda, ref) %>%
      left_join(shb_indikatorer %>% select(indikator, indikator_rubrik), by = "indikator") %>%
      mutate(
        text = formatera_intervall(varde_lag, varde_hog, t_lag, t_hog, namnare, typ, enhet, kommentar),
        mitt = (varde_lag + varde_hog) / 2,
        ungefarlig = !is.na(varde_lag) & varde_lag != varde_hog
      )
  })

  # Namnet på de valda områdena, t.ex. "Tjärna Ängar och Bullermyren"
  profil_namn <- reactive({
    namn <- shb_omraden_sf$omradesnamn[match(profil_koder(), shb_omraden_sf$omradeskod)]
    n <- length(namn)
    if (n == 1) namn
    else if (n <= 3) paste0(paste(namn[-n], collapse = ", "), " och ", namn[n])
    else paste0(paste(namn[1:2], collapse = ", "), " och ", n - 2, " områden till")
  })

  output$profil_rubrik <- renderUI({
    koder <- input$profil_omraden
    if (length(koder) == 0) {
      return(div(class = "profil-tom", icon("hand-pointer"),
                 " Välj ett område i listan ovan, eller flera som räknas ihop. Du kan också öppna ett område från kartan eller tabellen i Jämför områden."))
    }
    kommuner <- kommun_sf$kommunnamn[match(profil_kommuner(), kommun_sf$kommunkod)]
    div(class = "profil-rubrik",
        h3(profil_namn()),
        p(paste0(if (length(koder) > 1) paste0(length(koder), " områden räknade ihop i ") else "",
                 paste(kommuner, collapse = ", "),
                 if (!identical(valt$agarkategori, "Totalt")) paste0(" · ", valt$agarkategori) else "",
                 " · ", input$prof_ar,
                 if (length(kommuner) > 1) " · Områdena ligger i olika kommuner, därför jämförs de inte med en kommun" else "")))
  })

  # Nyckeltal: befolkning, hushåll och barnfamiljer i de valda områdena
  output$profil_nyckeltal <- renderUI({
    req(length(input$profil_omraden) > 0, input$prof_ar)
    df <- profil_data() %>%
      filter(geografi == "Valda områden", ar == as.integer(input$prof_ar), typ == "antal") %>%
      arrange(match(indikator, c("befolkning", "antal_hushall", "antal_barnfamiljer")))
    if (nrow(df) == 0) return(NULL)
    div(class = "nyckeltal",
        lapply(seq_len(nrow(df)), function(i) {
          varde <- if (is.na(df$varde_lag[i])) "Visas inte" else formatera_tal(df$t_lag[i])
          div(class = "nyckeltal-ruta", title = df$text[i],
              div(class = "nyckeltal-varde", varde),
              div(class = "nyckeltal-etikett", df$indikator_namn[i]))
        }))
  })

  # Färg och form per geografi i profildiagrammen: valda områden, kommunen (om den finns), Dalarna och riket
  profil_skalor <- function(df) {
    geo <- df %>% distinct(geografi, ordning) %>% arrange(ordning) %>% pull(geografi)
    kommun <- setdiff(geo, c("Valda områden", "Dalarna", "Riket"))
    farger <- c("Valda områden" = farg_vald, setNames(rep(farg_kommun, length(kommun)), kommun),
                "Dalarna" = farg_lan, "Riket" = farg_riket)
    former <- c("Valda områden" = 16, setNames(rep(124, length(kommun)), kommun), "Dalarna" = 18, "Riket" = 17)
    list(farger = farger[geo], former = former[geo])
  }

  # Profildiagram: en rad per indikator, de valda områdena som punkt (med intervall om ungefärligt)
  # och kommunen, Dalarna och riket som markeringar på samma rad
  profildiagram <- function(output_id, grupp_urval, titel) {
    req(input$prof_ar)
    df <- profil_data() %>%
      filter(grupp == grupp_urval, typ == "andel", ar == as.integer(input$prof_ar))
    validate(need(nrow(df) > 0, "Inga data för valt urval"))

    df <- df %>% mutate(rad = factor(str_wrap(indikator_namn, 32), levels = rev(unique(str_wrap(sort(indikator_namn), 32)))),
                        etikett = paste0(geografi, "<br><b>", indikator_rubrik, "</b><br>", text))
    valda <- df %>% filter(geografi == "Valda områden")
    ref <- df %>% filter(geografi != "Valda områden", !is.na(mitt))
    dolda <- valda %>% filter(is.na(mitt))
    skalor <- profil_skalor(df)
    storlek <- diagram_storlek(session, output_id)

    p <- ggplot(mapping = aes(y = rad)) +
      geom_point_interactive(data = ref, aes(x = mitt, color = geografi, shape = geografi, tooltip = etikett,
                                             size = geografi %in% c("Dalarna", "Riket")), stroke = 1.2) +
      scale_size_manual(values = c(`TRUE` = 3.2, `FALSE` = 7), guide = "none") +                 # kommunens streck större
      geom_linerange(data = valda %>% filter(ungefarlig), aes(xmin = varde_lag, xmax = varde_hog), color = farg_vald,
                     linewidth = 2.5, alpha = 0.35) +
      geom_point_interactive(data = valda %>% filter(!is.na(mitt)),
                             aes(x = mitt, color = geografi, shape = geografi, tooltip = etikett), size = 4) +
      { if (nrow(dolda) > 0) geom_text_interactive(data = dolda, aes(x = 0, label = "Visas inte", tooltip = etikett),
                                                   hjust = 0, size = 3.2, color = "#777") } +
      scale_color_manual(values = skalor$farger, breaks = names(skalor$farger), name = NULL) +
      scale_shape_manual(values = skalor$former, breaks = names(skalor$former), name = NULL) +
      scale_x_continuous(labels = function(x) paste(formatera_tal(x), "%"), limits = c(0, NA),
                         expand = expansion(mult = c(0.01, 0.05))) +
      labs(x = NULL, y = NULL, title = radbryt(titel, storlek$width),
           subtitle = if (any(valda$ungefarlig)) radbryt("Ljust fält: ungefärligt värde, där färre än 5 har eller saknar egenskapen.",
                                                        storlek$width, storlek_pt = diagram_caption_storlek + 1, fet = FALSE),
           caption = KALLA_TEXT) +
      tema_diagram() +
      theme(legend.position = "top", legend.justification = "left", panel.grid.major.y = element_line(color = "#e6e6e6"),
            plot.subtitle = element_text(size = diagram_caption_storlek + 1, color = "#444"))

    skapa_girafe(p, width = storlek$width, height = storlek$height)
  }

  output$profil_huvud <- renderGirafe(profildiagram("profil_huvud", "Huvudindikator", paste0("Huvudindikatorer, ", input$prof_ar)))
  output$profil_bakgrund <- renderGirafe(profildiagram("profil_bakgrund", "Bakgrund", paste0("Bakgrundsvariabler, ", input$prof_ar)))

  # Huvudsaklig inkomstkälla som staplade staplar. Varje del ritas med sitt lägsta säkra värde, och
  # det som inte är känt (dolda och ungefärliga delar) visas grått som "Osäkert".
  inkomstfarger <- c(ink_arbete = "#0f7090", ink_studier = "#54a1bd", ink_foraldraledighet_vard = "#8edded",
                     ink_pension = "#93cec1", ink_sjukdom = "#f2c14e", ink_nedsatt_arbetsformaga = "#e8894a",
                     ink_arbetsloshet = "#d95f5f", ink_ekonomiskt_bistand = "#a33b5e", ink_saknar_inkomst = "#5b3f7a",
                     osakert = "#d9d9d9")

  output$profil_inkomst <- renderGirafe({
    req(input$prof_ar)
    df <- profil_data() %>%
      filter(startsWith(as.character(grupp), "Huvudsaklig inkomstkälla"), typ == "andel", ar == as.integer(input$prof_ar))
    validate(need(nrow(df) > 0, "Inga data för valt urval"))

    delar <- df %>%
      mutate(andel = coalesce(varde_lag, 0), etikett = paste0(geografi, "<br><b>", indikator_namn, "</b><br>", text)) %>%
      select(geografi, ordning, indikator, indikator_namn, andel, etikett)
    osakert <- delar %>%
      group_by(geografi, ordning) %>%
      summarise(andel = max(100 - sum(andel), 0), .groups = "drop") %>%
      filter(andel >= 0.05) %>%
      mutate(indikator = "osakert", indikator_namn = "Osäkert (dolda eller ungefärliga värden)",
             etikett = paste0(geografi, "<br>Osäkert: ", formatera_tal(round(andel, 1)), " %"))

    namn <- c(setNames(shb_indikatorer$indikator_namn, shb_indikatorer$indikator), osakert = "Osäkert")
    nivaer <- intersect(names(inkomstfarger), unique(c(delar$indikator, osakert$indikator)))
    stapel <- bind_rows(delar, osakert) %>%
      mutate(indikator = factor(indikator, levels = rev(nivaer)),
             geografi = factor(geografi, levels = rev(unique(geografi[order(ordning)]))))
    storlek <- diagram_storlek(session, "profil_inkomst")

    p <- ggplot(stapel, aes(x = andel, y = geografi, fill = indikator)) +
      geom_col_interactive(aes(tooltip = etikett, data_id = paste(geografi, indikator)), width = 0.7, color = "white", linewidth = 0.2) +
      scale_fill_manual(values = inkomstfarger, labels = unname(namn[nivaer]), breaks = nivaer, name = NULL) +
      scale_x_continuous(labels = function(x) paste(formatera_tal(x), "%"), expand = expansion(mult = c(0, 0.02))) +
      labs(x = NULL, y = NULL, title = radbryt(paste0("Huvudsaklig inkomstkälla, 18–64 år, ", input$prof_ar), storlek$width),
           caption = KALLA_TEXT) +
      guides(fill = guide_legend(ncol = 2)) +
      tema_diagram() +
      theme(legend.position = "bottom", legend.text = element_text(size = diagram_caption_storlek),
            legend.key.size = unit(0.35, "cm"), panel.grid.major.y = element_blank())

    skapa_girafe(p, width = storlek$width, height = storlek$height)
  })

  # Utveckling över tid för huvudindikatorerna, ett litet diagram per indikator
  output$profil_tid <- renderGirafe({
    df <- profil_data() %>% filter(grupp == "Huvudindikator", typ == "andel", !is.na(mitt))
    validate(need(nrow(df) > 0, "Inga data för valt urval"))

    df <- df %>% mutate(etikett = paste0(geografi, " ", ar, "<br><b>", indikator_rubrik, "</b><br>", text),
                        panel = str_wrap(indikator_namn, 28))
    skalor <- profil_skalor(df)
    storlek <- diagram_storlek(session, "profil_tid")
    flera_ar <- n_distinct(df$ar) > 1

    p <- ggplot(df, aes(x = ar, y = mitt, color = geografi, group = geografi)) +
      { if (flera_ar) geom_line(linewidth = 0.9) } +
      geom_linerange(data = df %>% filter(ungefarlig), aes(ymin = varde_lag, ymax = varde_hog), linewidth = 2.5, alpha = 0.35) +
      geom_point_interactive(aes(tooltip = etikett, data_id = paste(geografi, indikator, ar)), size = 2) +
      facet_wrap(~panel, nrow = 1, scales = "free_y") +
      scale_color_manual(values = skalor$farger, breaks = names(skalor$farger), name = NULL) +
      scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
      scale_y_continuous(labels = function(x) paste(formatera_tal(x), "%")) +
      labs(x = NULL, y = NULL, title = radbryt("Huvudindikatorer över tid", storlek$width),
           subtitle = if (!flera_ar) "Utvecklingen över tid syns när statistiken innehåller fler år.",
           caption = KALLA_TEXT) +
      tema_diagram() +
      theme(legend.position = "top", legend.justification = "left", strip.text = element_text(size = diagram_caption_storlek + 1, face = "bold"),
            plot.subtitle = element_text(size = diagram_caption_storlek + 1, color = "#444"))

    skapa_girafe(p, width = storlek$width, height = storlek$height)
  })

  output$export_profil <- downloadHandler(
    filename = function() "shb_omradesprofil.xlsx",
    content = function(fil) {
      profil_data() %>%
        transmute(Geografi = if_else(geografi == "Valda områden", profil_namn(), geografi),
                  `År` = ar, `Ägarkategori` = agarkategori, Grupp = as.character(grupp), Indikator = indikator_rubrik,
                  `Värde` = text, `Lägsta värde` = varde_lag, `Högsta värde` = varde_hog) %>%
        arrange(Grupp, Indikator, `År`, match(Geografi, unique(Geografi))) %>%
        write_xlsx(fil)
    }
  )

  # ---- Nedladdning ----
  # Statistiken är redan sekretessgranskad när den läses in, släckta värden är tomma
  statistik_med_namn <- function(df) {
    df %>%
      left_join(geografinamn, by = "regionkod") %>%
      mutate(kommentar = case_when(
        !is.na(kommentar) ~ kommentar,
        farre_an ~ paste0("Täljaren är färre än ", shb_min_taljare, ", andelen är under ", formatera_tal(varde_max), " %"),
        farre_utan ~ paste0("Nämnaren minus täljaren är färre än ", shb_min_taljare, ", andelen är över ",
                            formatera_tal(varde_min), " %"),
        TRUE ~ ""
      )) %>%
      left_join(shb_indikatorer %>% select(indikator, indikator_rubrik), by = "indikator") %>%
      select(ar, niva, regionkod, namn, any_of("kommun"), agarkategori, grupp, indikator = indikator_rubrik, enhet,
             taljare, namnare, varde, kommentar) %>%
      arrange(grupp, indikator, agarkategori, niva, regionkod, ar)
  }

  output$export_excel <- downloadHandler(
    filename = function() "shb_statistik.xlsx",
    content = function(fil) write_xlsx(statistik_med_namn(shb_statistik), fil)
  )

  output$export_excel_urval <- downloadHandler(
    filename = function() "shb_statistik_urval.xlsx",
    content = function(fil) {
      koder <- c("00", "20", data_karta()$kod, vald_kommun())
      urval() %>%
        filter(regionkod %in% koder) %>%
        statistik_med_namn() %>%
        write_xlsx(fil)
    }
  )

})
