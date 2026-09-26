KALLA_TEXT <- if (shb_exempeldata) "Exempeldata – slumpade värden, inte riktig statistik" else KALLA_SHB

# Färger som i brott-appen
kartpalett       <- "YlOrRd"
farg_stapel      <- "#3182bd"
farg_vald        <- "#e31a1c"      # valt område i diagrammen
farg_vald_fyll   <- "#f15060"      # valt område i kartan
farg_vald_kant   <- "#ae2d3a"
farg_nedtonad    <- "#c6d4e1"      # områden utanför vald kommun
farg_kommun      <- "#0f7090"
farg_lan         <- "#54a1bd"
farg_riket       <- "#8edded"

# Svenska texter till tabellen (DataTables)
dt_svenska <- list(
  search = "Sök:", lengthMenu = "Visa _MENU_ rader", zeroRecords = "Inga områden matchar sökningen",
  info = "Visar _START_–_END_ av _TOTAL_ områden", infoEmpty = "Inga områden", infoFiltered = "(filtrerat från _MAX_)",
  paginate = list(first = "Första", last = "Sista", `next` = "Nästa", previous = "Föregående")
)

shinyServer(function(input, output, session) {
  rdshinyappar::telemetri_server(telemetry, navigation_id = 'flikval', forsta_flik = 'Karta och diagram')

  kartniva     <- reactiveVal("kommun")         # "kommun" = alla kommuner, "omrade" = shb-områden i vald kommun
  vald_kommun  <- reactiveVal(NULL)             # kommunkod
  valt_omrade  <- reactiveVal(NULL)             # omradeskod, markeras i karta och diagram

  # ---- Val av indikator och år ----
  # Flikarna har egna listrutor (val_* och jmf_*) men visar alltid samma indikator och år
  indikatorer <- sort(unique(shb_statistik$indikator))
  updateSelectInput(session, "val_indikator", choices = indikatorer)
  updateSelectInput(session, "jmf_indikator", choices = indikatorer)

  observeEvent(input$val_indikator, {
    req(input$val_indikator)
    ar <- shb_statistik %>% filter(indikator == input$val_indikator) %>% pull(ar) %>% unique() %>% sort(decreasing = TRUE)
    valt <- if (isTruthy(input$val_ar) && input$val_ar %in% ar) input$val_ar else ar[1]
    updateSelectInput(session, "val_ar", choices = ar, selected = valt)
    updateSelectInput(session, "jmf_ar", choices = ar, selected = valt)
    if (!identical(input$jmf_indikator, input$val_indikator)) {
      updateSelectInput(session, "jmf_indikator", selected = input$val_indikator)
    }
  })

  observeEvent(input$jmf_indikator, {
    req(input$jmf_indikator)
    if (!identical(input$jmf_indikator, input$val_indikator)) {
      updateSelectInput(session, "val_indikator", selected = input$jmf_indikator)
    }
  })

  observeEvent(input$val_ar, {
    req(input$val_ar)
    if (!identical(input$jmf_ar, input$val_ar)) updateSelectInput(session, "jmf_ar", selected = input$val_ar)
  })

  observeEvent(input$jmf_ar, {
    req(input$jmf_ar)
    if (!identical(input$jmf_ar, input$val_ar)) updateSelectInput(session, "val_ar", selected = input$jmf_ar)
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

    pal <- colorNumeric(kartpalett, domain = df_map$varde, na.color = "transparent")

    etiketter <- lapply(paste0(
      df_map$namn, "<br>",
      "<b>", input$val_indikator, "</b><br>",
      formatera_varde(df_map$varde, df_map$taljare, df_map$namnare), "<br>",
      "<i>År ", input$val_ar, "</i>"
    ), HTML)

    proxy %>%
      addPolygons(
        layerId     = ~kod,
        fillColor   = ~ifelse(vald_geom, farg_vald_fyll, pal(varde)),
        fillOpacity = ~ifelse(vald_geom, 0.95, 0.6),
        color       = ~ifelse(vald_geom, farg_vald_kant, "#555555"),
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

    titel <- if (kartniva() == "kommun") {
      paste0(input$val_indikator, " per kommun år ", input$val_ar)
    } else {
      paste0(input$val_indikator, " per område i ", vald_kommun_namn(), " år ", input$val_ar)
    }
    storlek <- diagram_storlek(session, "diagram_geografi")

    # Med många områden blir namnen på x-axeln oläsliga, då visas de bara vid hovring
    manga <- nrow(df_diag) > 25

    p <- ggplot(df_diag, aes(x = reorder(namn, varde), y = varde)) +
      geom_col_interactive(aes(tooltip = etikett, data_id = kod, fill = farg), color = NA) +
      scale_fill_identity() +
      scale_y_continuous(labels = formatera_tal) +
      scale_x_discrete(labels = function(x) str_trunc(x, 20)) +
      labs(x = NULL, y = enhet_text(), title = radbryt(titel, storlek$width), caption = KALLA_TEXT) +
      tema_diagram() +
      labs(subtitle = if (manga) "Håll muspekaren över en stapel för att se områdets namn") +
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
    req(input$val_indikator)

    df_tid <- shb_statistik %>%
      filter(indikator == input$val_indikator, regionkod %in% geografier_tid()) %>%
      left_join(geografinamn %>% select(regionkod, namn), by = "regionkod") %>%
      mutate(etikett = paste0(namn, " ", ar, "<br>", formatera_varde(varde, taljare, namnare)))

    validate(need(nrow(df_tid) > 0, "Inga data för valt urval"))

    storlek <- diagram_storlek(session, "diagram_tid")
    koder <- geografier_tid()
    farger <- c(farg_riket, farg_lan, farg_kommun, farg_vald)[match(koder, c("00", "20", vald_kommun(), valt_omrade()))]
    namn <- geografinamn$namn[match(koder, geografinamn$regionkod)]

    p <- ggplot(df_tid, aes(x = ar, y = varde, color = regionkod, group = regionkod)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(aes(tooltip = etikett, data_id = paste(regionkod, ar)), size = 2) +
      scale_color_manual(values = setNames(farger, koder), labels = setNames(namn, koder), breaks = koder, name = NULL) +
      scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
      scale_y_continuous(labels = formatera_tal) +
      labs(x = NULL, y = enhet_text(), title = radbryt(paste0(input$val_indikator, " över tid"), storlek$width), caption = KALLA_TEXT) +
      tema_diagram() +
      theme(legend.position = "top", legend.justification = "left")

    skapa_girafe(p, width = storlek$width, height = storlek$height)
  })

  # ---- Diagram: alla områden i länet, per kommun ----
  # En punkt per område, strecket visar kommunens värde. Kommunerna sorteras efter sitt värde.
  output$diagram_alla <- renderGirafe({
    req(input$val_indikator, input$val_ar)

    varden <- shb_statistik %>%
      filter(indikator == input$val_indikator, ar == as.integer(input$val_ar), !is.na(varde))

    omr <- st_drop_geometry(shb_omraden_sf) %>%
      inner_join(varden, by = c("omradeskod" = "regionkod"))

    validate(need(nrow(omr) > 0, "Inga data för valt urval"))

    kommuner <- st_drop_geometry(kommun_sf) %>%
      inner_join(varden, by = c("kommunkod" = "regionkod"))
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
        farg = case_when(omradeskod %in% vald_o ~ farg_vald, i_fokus ~ farg_stapel, TRUE ~ farg_nedtonad),
        etikett = paste0(omradesnamn, ", ", kommunnamn, "<br>", formatera_varde(varde, taljare, namnare))
      ) %>%
      arrange(i_fokus, omradeskod %in% vald_o)               # valda områden ritas överst

    kommuner <- kommuner %>%
      mutate(x = match(kommunnamn, ordning),
             etikett = paste0(kommunnamn, " (hela kommunen)<br>", formatera_varde(varde, taljare, namnare)))

    titel <- paste0(input$val_indikator, " per område och kommun år ", input$val_ar)
    storlek <- diagram_storlek(session, "diagram_alla")

    p <- ggplot() +
      geom_point_interactive(data = omr, aes(x = x, y = varde, tooltip = etikett, data_id = omradeskod, fill = farg),
                             shape = 21, color = "white", stroke = 0.3, size = 2.4) +
      geom_segment_interactive(data = kommuner, aes(x = x - 0.4, xend = x + 0.4, y = varde, yend = varde, tooltip = etikett),
                               color = farg_kommun, linewidth = 1.1) +
      scale_fill_identity() +
      scale_x_continuous(breaks = seq_along(ordning), labels = ordning, expand = expansion(add = 0.5)) +
      scale_y_continuous(labels = formatera_tal) +
      labs(x = NULL, y = enhet_text(), title = radbryt(titel, storlek$width),
           subtitle = radbryt("Punkterna är områden, strecken visar kommunens värde", storlek$width,
                              storlek_pt = diagram_caption_storlek + 1, fet = FALSE),
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

  # Alla områden med värde för vald indikator och år
  rangordning <- reactive({
    req(input$jmf_indikator, input$jmf_ar)
    st_drop_geometry(shb_omraden_sf) %>%
      inner_join(shb_statistik %>% filter(indikator == input$jmf_indikator, ar == as.integer(input$jmf_ar)),
                 by = c("omradeskod" = "regionkod")) %>%
      filter(!is.na(varde)) %>%
      mutate(litet_underlag = !is.na(namnare) & namnare < shb_min_namnare)
  })

  jmf_enhet <- reactive({
    req(input$jmf_indikator)
    indikatorenhet(shb_statistik, input$jmf_indikator)
  })

  output$jmf_info <- renderText({
    antal_sma <- sum(rangordning()$litet_underlag)
    if (antal_sma == 0) return("")
    paste0(antal_sma, " av ", nrow(rangordning()), " områden bygger på färre än ", shb_min_namnare,
           " personer och ingår inte i rangordningen.")
  })

  rangordningsdiagram <- function(output_id, hogst) {
    df <- rangordning() %>%
      filter(!litet_underlag) %>%
      arrange(if (hogst) desc(varde) else varde) %>%
      slice_head(n = shb_antal_rangordning)

    validate(need(nrow(df) > 0, "Inga data för valt urval"))

    df <- df %>%
      mutate(
        axeltext = paste0(str_trunc(omradesnamn, 35), " (", kommunnamn, ")"),
        axeltext = factor(axeltext, levels = rev(unique(axeltext))),        # första området överst
        etikett = paste0(omradesnamn, ", ", kommunnamn, "<br>", formatera_varde(varde, taljare, namnare))
      )

    dalarna <- shb_statistik %>%
      filter(regionkod == "20", indikator == input$jmf_indikator, ar == as.integer(input$jmf_ar))

    storlek <- diagram_storlek(session, output_id)
    enhet_ord <- if (jmf_enhet() == "procent") "andel" else "antal"
    titel <- paste0("De ", nrow(df), " områden med ", if (hogst) "högst " else "lägst ", enhet_ord,
                    ": ", input$jmf_indikator, " år ", input$jmf_ar)

    p <- ggplot(df, aes(x = varde, y = axeltext)) +
      geom_col_interactive(aes(tooltip = etikett, data_id = omradeskod), fill = farg_stapel, width = 0.75) +
      scale_x_continuous(labels = formatera_tal, expand = expansion(mult = c(0, 0.05))) +
      labs(x = if (jmf_enhet() == "procent") "Andel (%)" else "Antal", y = NULL,
           title = radbryt(titel, storlek$width), caption = KALLA_TEXT) +
      tema_diagram() +
      theme(panel.grid.major.y = element_blank(), legend.position = "none")

    # Dalarna som referens bara för andelar, länets totala antal går inte att jämföra med ett område
    if (enhet_ord == "andel" && nrow(dalarna) == 1 && !is.na(dalarna$varde)) {
      p <- p +
        geom_vline_interactive(xintercept = dalarna$varde, color = farg_kommun, linetype = "dashed", linewidth = 0.8,
                               tooltip = paste0("Dalarna<br>", formatera_varde(dalarna$varde, dalarna$taljare, dalarna$namnare))) +
        labs(subtitle = paste0("Streckad linje: Dalarna ", formatera_tal(dalarna$varde), " %")) +
        theme(plot.subtitle = element_text(size = diagram_caption_storlek + 1, color = farg_kommun))
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
      mutate(kommentar = ifelse(litet_underlag,
                                paste0("Färre än ", shb_min_namnare, " personer, ingår inte i rangordningen"), ""))
  })

  output$tabell_omraden <- renderDT({
    df <- tabell_data()
    procent <- jmf_enhet() == "procent"

    visning <- if (procent) {
      df %>% transmute(Kommun = kommunnamn, `Område` = omradesnamn, `Andel (%)` = varde,
                       `Täljare` = taljare, `Nämnare` = namnare, Kommentar = kommentar)
    } else {
      df %>% transmute(Kommun = kommunnamn, `Område` = omradesnamn, Antal = varde)
    }

    tabell <- datatable(
      visning,
      rownames = FALSE,
      selection = "single",
      class = "compact stripe hover",
      options = list(pageLength = 15, lengthMenu = c(15, 50, 100), order = list(list(2, "desc")),
                     language = dt_svenska)
    )

    if (procent) {
      tabell %>%
        formatRound("Andel (%)", digits = 1, mark = " ", dec.mark = ",") %>%
        formatRound(c("Täljare", "Nämnare"), digits = 0, mark = " ")
    } else {
      tabell %>% formatRound("Antal", digits = 0, mark = " ")
    }
  })

  observeEvent(input$tabell_omraden_rows_selected, {
    kod <- tabell_data()$omradeskod[input$tabell_omraden_rows_selected]
    selectRows(dataTableProxy("tabell_omraden"), NULL)
    visa_i_kartan(kod)
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
