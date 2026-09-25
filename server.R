KALLA_TEXT <- if (shb_exempeldata) "Exempeldata – slumpade värden, inte riktig statistik" else KALLA_SHB

# Färgskala för kartan
kartfarger <- c("#eef7f5", rus_tre_fokus[1], rus_tre_fokus[2])
farg_stapel <- rus_tre_fokus[2]
farg_riket  <- "#999999"
farg_vald   <- "#000000"

shinyServer(function(input, output, session) {
  rdshinyappar::telemetri_server(telemetry, navigation_id = 'flikval', forsta_flik = 'Statistik')

  kartniva     <- reactiveVal("kommun")         # "kommun" = alla kommuner, "omrade" = shb-områden i vald kommun
  vald_kommun  <- reactiveVal(NULL)             # kommunkod
  valt_omrade  <- reactiveVal(NULL)             # omradeskod, markeras i karta och diagram

  # ---- Val av indikator och år ----
  updateSelectInput(session, "val_indikator", choices = sort(unique(shb_statistik$indikator)))

  observeEvent(input$val_indikator, {
    ar <- shb_statistik %>% filter(indikator == input$val_indikator) %>% pull(ar) %>% unique() %>% sort(decreasing = TRUE)
    valt <- if (isTruthy(input$val_ar) && input$val_ar %in% ar) input$val_ar else ar[1]
    updateSelectInput(session, "val_ar", choices = ar, selected = valt)
  })

  vald_kommun_namn <- reactive({
    req(vald_kommun())
    kommun_sf$kommunnamn[kommun_sf$kommunkod == vald_kommun()]
  })

  # "procent" eller "antal", styr axeltexter och teckenförklaring
  enhet <- reactive({
    req(input$val_indikator)
    indikatorenhet(shb_statistik, input$val_indikator)
  })
  enhet_text <- reactive(if (enhet() == "procent") "Andel (%)" else "Antal")

  geografi_text <- reactive({
    if (kartniva() == "kommun") "i Dalarnas kommuner" else paste0("i ", vald_kommun_namn())
  })

  # ---- Data för kartan och geografidiagrammet ----
  # sf-objekt med kolumnerna kod, namn och varde för den geografi som visas
  data_karta <- reactive({
    req(input$val_indikator, input$val_ar)

    geo <- if (kartniva() == "kommun") {
      kommun_sf %>% select(kod = kommunkod, namn = kommunnamn)
    } else {
      shb_omraden_sf %>% filter(kommunkod == vald_kommun()) %>% select(kod = omradeskod, namn = omradesnamn)
    }

    varden <- shb_statistik %>%
      filter(indikator == input$val_indikator, ar == as.integer(input$val_ar)) %>%
      select(kod = regionkod, varde, taljare, namnare)

    left_join(geo, varden, by = "kod")
  })

  # ---- Klick: kommun -> områden, område -> markera ----
  klicka_geografi <- function(kod) {
    if (kartniva() == "kommun") {
      vald_kommun(kod)
      valt_omrade(NULL)
      kartniva("omrade")
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
    vald <- if (kartniva() == "omrade") valt_omrade() else NULL
    df_map$vald_geom <- !is.null(vald) & df_map$kod %in% vald

    proxy <- leafletProxy("karta_shb", data = df_map) %>%
      clearShapes() %>%
      clearControls()

    if (all(is.na(df_map$varde))) {
      proxy %>% addControl("Inga data för valt urval", position = "topright", className = "map-filter-text")
      return()
    }

    pal <- colorNumeric(kartfarger, domain = df_map$varde, na.color = "#dddddd")

    etiketter <- lapply(paste0(
      df_map$namn, "<br>",
      "<b>", input$val_indikator, "</b><br>",
      formatera_varde(df_map$varde, df_map$taljare, df_map$namnare), "<br>",
      "<i>År ", input$val_ar, "</i>"
    ), HTML)

    proxy %>%
      addPolygons(
        layerId     = ~kod,
        fillColor   = ~pal(varde),
        fillOpacity = ~ifelse(vald_geom, 0.95, 0.7),
        color       = ~ifelse(vald_geom, farg_vald, "#555555"),
        weight      = ~ifelse(vald_geom, 3, 0.7),
        label       = etiketter,
        highlightOptions = highlightOptions(weight = 3, color = "#000000", bringToFront = FALSE)
      ) %>%
      addLegend(
        "bottomleft", pal = pal, values = ~varde,
        title = enhet_text(),
        labFormat = labelFormat(big.mark = " ", suffix = if (enhet() == "procent") " %" else ""),
        className = "info legend kompakt-legend"
      ) %>%
      addControl(
        HTML(paste0(input$val_indikator, "<br>",
                    if (kartniva() == "kommun") "Dalarna" else vald_kommun_namn(), "<br>",
                    "År ", input$val_ar,
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
  })

  # Zooma bara när geografin byts, inte när indikator eller år byts
  observeEvent(list(kartniva(), vald_kommun()), {
    geo <- if (kartniva() == "kommun") kommun_sf else shb_omraden_sf %>% filter(kommunkod == vald_kommun())
    bbox <- st_bbox(geo)
    leafletProxy("karta_shb") %>%
      fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]],
                options = list(paddingTopLeft = c(80, 5), paddingBottomRight = c(5, 5)))
  }, ignoreInit = TRUE)

  # ---- Diagram: värde per kommun eller område ----
  output$diagram_geografi <- renderGirafe({
    vald <- if (kartniva() == "omrade") valt_omrade() else NULL

    df_diag <- data_karta() %>%
      st_drop_geometry() %>%
      filter(!is.na(varde)) %>%
      mutate(
        farg = ifelse(kod %in% vald, farg_vald, farg_stapel),
        etikett = paste0(namn, "<br>", formatera_varde(varde, taljare, namnare))
      )

    validate(need(nrow(df_diag) > 0, "Inga data för valt urval"))

    titel <- paste0(input$val_indikator, " per ", if (kartniva() == "kommun") "kommun" else "område", " ",
                    geografi_text(), " år ", input$val_ar)

    p <- ggplot(df_diag, aes(x = reorder(namn, varde), y = varde)) +
      geom_col_interactive(aes(tooltip = etikett, data_id = kod, fill = farg), color = NA) +
      scale_fill_identity() +
      scale_y_continuous(labels = formatera_tal) +
      scale_x_discrete(labels = function(x) str_trunc(x, 20)) +
      labs(x = NULL, y = enhet_text(), title = str_wrap(titel, 90), caption = KALLA_TEXT) +
      tema_diagram() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

    skapa_girafe(p, klickbar = TRUE)
  })

  output$geografi_klick_text <- renderText({
    if (kartniva() == "kommun") "Klicka på en kommun för att se områden" else "Klicka på ett område för att markera"
  })

  # ---- Diagram: utveckling över tid ----
  # Riket, Dalarna, vald kommun och valt område
  geografier_tid <- reactive({
    c("00", "20", vald_kommun(), if (kartniva() == "omrade") valt_omrade())
  })

  output$diagram_tid <- renderGirafe({
    req(input$val_indikator)

    df_tid <- shb_statistik %>%
      filter(indikator == input$val_indikator, regionkod %in% geografier_tid()) %>%
      left_join(geografinamn %>% select(regionkod, namn), by = "regionkod") %>%
      mutate(
        namn = factor(namn, levels = geografinamn$namn[match(geografier_tid(), geografinamn$regionkod)]),
        etikett = paste0(namn, " ", ar, "<br>", formatera_varde(varde, taljare, namnare))
      )

    validate(need(nrow(df_tid) > 0, "Inga data för valt urval"))

    p <- ggplot(df_tid, aes(x = ar, y = varde, color = namn, group = namn)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(aes(tooltip = etikett, data_id = paste(regionkod, ar)), size = 2) +
      scale_color_manual(values = c(farg_riket, rus_tre_fokus), name = NULL) +
      scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
      scale_y_continuous(labels = formatera_tal) +
      labs(x = NULL, y = enhet_text(), title = paste0(input$val_indikator, " över tid"), caption = KALLA_TEXT) +
      tema_diagram() +
      theme(legend.position = "top", legend.justification = "left")

    skapa_girafe(p)
  })

  # ---- Nedladdning ----
  statistik_med_namn <- function(df) {
    df %>%
      left_join(geografinamn, by = "regionkod") %>%
      select(niva, regionkod, namn, any_of("kommun"), ar, indikator, taljare, namnare, varde) %>%
      arrange(indikator, niva, regionkod, ar)
  }

  output$export_excel <- downloadHandler(
    filename = function() "shb_statistik.xlsx",
    content = function(fil) write_xlsx(statistik_med_namn(shb_statistik), fil)
  )

  output$export_excel_urval <- downloadHandler(
    filename = function() "shb_statistik_urval.xlsx",
    content = function(fil) {
      koder <- c("00", "20", data_karta()$kod, vald_kommun())
      shb_statistik %>%
        filter(indikator == input$val_indikator, regionkod %in% koder) %>%
        statistik_med_namn() %>%
        write_xlsx(fil)
    }
  )

})
