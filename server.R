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

# Listrutans val: indikatorkod som värde och namn som text, grupperade efter indikatorgrupp.
# as.list behövs för att grupper med en enda indikator ska visas som grupp.
indikatorval <- lapply(split(setNames(shb_indikatorer$indikator, shb_indikatorer$indikator_namn),
                             shb_indikatorer$grupp, drop = TRUE), as.list)
agarkategorier <- intersect(c("Totalt", "Allmännyttan", "Övriga ägare", "Uppgift saknas"),
                            unique(shb_statistik$agarkategori))

# Kommuner som saknar shb-områden, t.ex. Gagnef
kommuner_utan_omraden <- setdiff(kommun_sf$kommunkod, shb_omraden_sf$kommunkod)

shinyServer(function(input, output, session) {
  rdshinyappar::telemetri_server(telemetry, navigation_id = 'flikval', forsta_flik = 'Karta och diagram')

  kartniva     <- reactiveVal("kommun")         # "kommun" = alla kommuner, "omrade" = shb-områden i vald kommun
  vald_kommun  <- reactiveVal(NULL)             # kommunkod
  valt_omrade  <- reactiveVal(NULL)             # omradeskod, markeras i karta och diagram

  # ---- Val av indikator, ägarkategori och år ----
  # Flikarna har egna listrutor (val_* och jmf_*) men visar alltid samma val
  for (id in c("val_indikator", "jmf_indikator")) updateSelectInput(session, id, choices = indikatorval)
  for (id in c("val_agarkategori", "jmf_agarkategori")) {
    updateSelectInput(session, id, choices = agarkategorier, selected = shb_agarkategori_standard)
  }

  synka <- function(a, b) {
    observeEvent(input[[a]], {
      if (isTruthy(input[[a]]) && !identical(input[[a]], input[[b]])) updateSelectInput(session, b, selected = input[[a]])
    })
    observeEvent(input[[b]], {
      if (isTruthy(input[[b]]) && !identical(input[[a]], input[[b]])) updateSelectInput(session, a, selected = input[[b]])
    })
  }
  synka("val_indikator", "jmf_indikator")
  synka("val_agarkategori", "jmf_agarkategori")
  synka("val_ar", "jmf_ar")

  # Bara år där indikatorn har värden går att välja
  observeEvent(input$val_indikator, {
    req(input$val_indikator)
    ar <- shb_statistik %>% filter(indikator == input$val_indikator) %>% pull(ar) %>% unique() %>% sort(decreasing = TRUE)
    valt <- if (isTruthy(input$val_ar) && input$val_ar %in% ar) input$val_ar else ar[1]
    for (id in c("val_ar", "jmf_ar")) updateSelectInput(session, id, choices = ar, selected = valt)
  })

  # Statistiken för vald indikator och ägarkategori, alla år och geografier
  urval <- function(indikator_id, agarkategori_id) {
    reactive({
      req(input[[indikator_id]], input[[agarkategori_id]])
      shb_statistik %>% filter(indikator == input[[indikator_id]], agarkategori == input[[agarkategori_id]])
    })
  }
  urval_val <- urval("val_indikator", "val_agarkategori")
  urval_jmf <- urval("jmf_indikator", "jmf_agarkategori")

  indikator_info <- function(id) shb_indikatorer[shb_indikatorer$indikator == id, ][1, ]

  # Text om vad som visas, t.ex. "Trångbodda enligt norm 3 (allmännyttan)"
  urvalstext <- function(indikator_id, agarkategori_id) {
    agar <- input[[agarkategori_id]]
    paste0(indikator_info(input[[indikator_id]])$indikator_namn,
           if (!identical(agar, "Totalt")) paste0(" (", tolower(agar), ")"))
  }

  axeltext <- function(info) if (info$typ == "andel") "Andel (%)" else paste("Antal", tolower(info$enhet))

  vald_kommun_namn <- reactive({
    req(vald_kommun())
    kommun_sf$kommunnamn[kommun_sf$kommunkod == vald_kommun()]
  })

  val_info <- reactive({
    req(input$val_indikator)
    indikator_info(input$val_indikator)
  })

  # ---- Data för kartan och geografidiagrammet ----
  # sf-objekt med kolumnerna kod, namn och värden för den geografi som visas
  data_karta <- reactive({
    req(input$val_ar)

    geo <- if (kartniva() == "kommun") {
      kommun_sf %>% select(kod = kommunkod, namn = kommunnamn)
    } else {
      shb_omraden_sf %>% filter(kommunkod == vald_kommun()) %>% select(kod = omradeskod, namn = omradesnamn)
    }

    varden <- urval_val() %>%
      filter(ar == as.integer(input$val_ar)) %>%
      select(kod = regionkod, varde, taljare, namnare, enhet, skyddad, kommentar)

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
      "<b>", urvalstext("val_indikator", "val_agarkategori"), "</b><br>",
      formatera_varde(df_map$varde, df_map$taljare, df_map$namnare, info$enhet, df_map$kommentar), "<br>",
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
        title = axeltext(info),
        labFormat = labelFormat(big.mark = " ", suffix = if (info$typ == "andel") " %" else ""),
        className = "info legend kompakt-legend"
      ) %>%
      addControl(
        HTML(paste0(urvalstext("val_indikator", "val_agarkategori"), "<br>",
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

  # Text om områden som inte visas av sekretesskäl, NULL om inga
  text_skyddade <- function(skyddad) {
    antal <- sum(skyddad, na.rm = TRUE)
    if (antal == 0) return(NULL)
    paste0(antal, if (antal == 1) " område visas inte" else " områden visas inte", " av sekretesskäl.")
  }

  # ---- Diagram: värde per kommun eller område ----
  output$diagram_geografi <- renderGirafe({
    vald <- if (kartniva() == "omrade") valt_omrade() else NULL
    info <- val_info()
    alla <- data_karta() %>% st_drop_geometry()

    df_diag <- alla %>%
      filter(!is.na(varde)) %>%
      mutate(
        farg = ifelse(kod %in% vald, farg_vald, farg_stapel),
        etikett = paste0(namn, "<br>", formatera_varde(varde, taljare, namnare, enhet, kommentar))
      )

    validate(need(nrow(df_diag) > 0, "Inga data för valt urval"))

    titel <- if (kartniva() == "kommun") {
      paste0(urvalstext("val_indikator", "val_agarkategori"), " per kommun, ", input$val_ar)
    } else {
      paste0(urvalstext("val_indikator", "val_agarkategori"), " per område i ", vald_kommun_namn(), ", ", input$val_ar)
    }
    storlek <- diagram_storlek(session, "diagram_geografi")

    # Med många områden blir namnen på x-axeln oläsliga, då visas de bara vid hovring
    manga <- nrow(df_diag) > 25
    underrubrik <- paste(c(if (manga) "Håll muspekaren över en stapel för att se områdets namn.",
                           text_skyddade(alla$skyddad)), collapse = " ")

    p <- ggplot(df_diag, aes(x = reorder(namn, varde), y = varde)) +
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

    df_tid <- urval_val() %>%
      filter(regionkod %in% geografier_tid(), !is.na(varde)) %>%
      left_join(geografinamn %>% select(regionkod, namn), by = "regionkod") %>%
      mutate(etikett = paste0(namn, " ", ar, "<br>", formatera_varde(varde, taljare, namnare, enhet, kommentar)))

    validate(need(nrow(df_tid) > 0, "Inga data för valt urval"))

    storlek <- diagram_storlek(session, "diagram_tid")
    koder <- intersect(geografier_tid(), df_tid$regionkod)
    farger <- c(farg_riket, farg_lan, farg_kommun, farg_vald)[match(koder, c("00", "20", vald_kommun(), valt_omrade()))]
    namn <- geografinamn$namn[match(koder, geografinamn$regionkod)]

    p <- ggplot(df_tid, aes(x = ar, y = varde, color = regionkod, group = regionkod)) +
      { if (n_distinct(df_tid$ar) > 1) geom_line(linewidth = 1) } +          # en linje kräver minst två år
      geom_point_interactive(aes(tooltip = etikett, data_id = paste(regionkod, ar)), size = 2) +
      scale_color_manual(values = setNames(farger, koder), labels = setNames(namn, koder), breaks = koder, name = NULL) +
      scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
      scale_y_continuous(labels = formatera_tal) +
      labs(x = NULL, y = axeltext(info),
           title = radbryt(paste0(urvalstext("val_indikator", "val_agarkategori"), " över tid"), storlek$width),
           caption = KALLA_TEXT) +
      tema_diagram() +
      theme(legend.position = "top", legend.justification = "left")

    skapa_girafe(p, width = storlek$width, height = storlek$height)
  })

  # ---- Diagram: alla områden i länet, per kommun ----
  # En punkt per område, strecket visar kommunens värde. Kommunerna sorteras efter sitt värde.
  output$diagram_alla <- renderGirafe({
    req(input$val_ar)
    info <- val_info()

    varden <- urval_val() %>% filter(ar == as.integer(input$val_ar))

    omr_alla <- st_drop_geometry(shb_omraden_sf) %>%
      inner_join(varden, by = c("omradeskod" = "regionkod"))
    omr <- omr_alla %>% filter(!is.na(varde))

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
        farg = case_when(omradeskod %in% vald_o ~ farg_vald, i_fokus ~ farg_stapel, TRUE ~ farg_nedtonad),
        etikett = paste0(omradesnamn, ", ", kommunnamn, "<br>", formatera_varde(varde, taljare, namnare, enhet, kommentar))
      ) %>%
      arrange(i_fokus, omradeskod %in% vald_o)               # valda områden ritas överst

    kommuner <- kommuner %>%
      mutate(x = match(kommunnamn, ordning),
             etikett = paste0(kommunnamn, " (hela kommunen)<br>", formatera_varde(varde, taljare, namnare, enhet, kommentar)))

    titel <- paste0(urvalstext("val_indikator", "val_agarkategori"), " per område och kommun, ", input$val_ar)
    underrubrik <- paste(c("Punkterna är områden, strecken visar kommunens värde.", text_skyddade(omr_alla$skyddad)),
                         collapse = " ")
    storlek <- diagram_storlek(session, "diagram_alla")

    p <- ggplot() +
      geom_point_interactive(data = omr, aes(x = x, y = varde, tooltip = etikett, data_id = omradeskod, fill = farg),
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

  jmf_info <- reactive({
    req(input$jmf_indikator)
    indikator_info(input$jmf_indikator)
  })

  # Alla områden för vald indikator, ägarkategori och år, även de som inte visas av sekretesskäl
  rangordning <- reactive({
    req(input$jmf_ar)
    st_drop_geometry(shb_omraden_sf) %>%
      inner_join(urval_jmf() %>% filter(ar == as.integer(input$jmf_ar)), by = c("omradeskod" = "regionkod")) %>%
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
    alla <- rangordning() %>% filter(!is.na(varde), !utanfor_rangordning)
    # Lika värden sorteras på namn så att urvalet inte blir slumpmässigt
    df <- alla %>%
      arrange(if (hogst) desc(varde) else varde, kommunnamn, omradesnamn) %>%
      slice_head(n = shb_antal_rangordning)

    validate(need(nrow(df) > 0, "Inga data för valt urval"))

    # Har fler områden samma värde som det sista i listan, t.ex. många med 0 %, sägs det i underrubriken
    sista <- df$varde[nrow(df)]
    fler_lika <- sum(alla$varde == sista) - sum(df$varde == sista)
    text_lika <- if (fler_lika > 0) {
      paste0("Ytterligare ", fler_lika, " områden har också ", formatera_tal(sista),
             if (info$typ == "andel") " %" else "", " men ryms inte i listan.")
    }

    df <- df %>%
      mutate(
        axeltext = paste0(str_trunc(omradesnamn, 35), " (", kommunnamn, ")"),
        axeltext = factor(axeltext, levels = rev(unique(axeltext))),        # första området överst
        etikett = paste0(omradesnamn, ", ", kommunnamn, "<br>", formatera_varde(varde, taljare, namnare, enhet, kommentar))
      )

    dalarna <- urval_jmf() %>% filter(regionkod == "20", ar == as.integer(input$jmf_ar))

    storlek <- diagram_storlek(session, output_id)
    titel <- paste0("De ", nrow(df), " områden med ", if (hogst) "högst " else "lägst ", info$typ,
                    ": ", urvalstext("jmf_indikator", "jmf_agarkategori"), ", ", input$jmf_ar)

    p <- ggplot(df, aes(x = varde, y = axeltext)) +
      geom_col_interactive(aes(tooltip = etikett, data_id = omradeskod), fill = farg_stapel, width = 0.75) +
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
        labs(subtitle = paste(c(paste0("Streckad linje: Dalarna ", formatera_tal(dalarna$varde), " %."), text_lika), collapse = " ")) +
        theme(plot.subtitle = element_text(size = diagram_caption_storlek + 1, color = farg_kommun))
    } else if (!is.null(text_lika)) {
      p <- p +
        labs(subtitle = text_lika) +
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
        utanfor_rangordning ~ paste0("Ingår inte i rangordningen: färre än ", shb_min_rangordning, " ", tolower(enhet)),
        TRUE ~ ""
      ))
  })

  output$tabell_omraden <- renderDT({
    df <- tabell_data()
    info <- jmf_info()
    enhet <- tolower(info$enhet)

    visning <- if (info$typ == "andel") {
      df %>% transmute(Kommun = kommunnamn, `Område` = omradesnamn, `Andel (%)` = varde,
                       !!paste0("Täljare (", enhet, ")") := taljare,
                       !!paste0("Nämnare (", enhet, ")") := namnare,
                       Kommentar = kommentar)
    } else {
      df %>% transmute(Kommun = kommunnamn, `Område` = omradesnamn, !!paste0("Antal ", enhet) := varde)
    }

    tabell <- datatable(
      visning,
      rownames = FALSE,
      selection = "single",
      class = "compact stripe hover",
      options = list(pageLength = 15, lengthMenu = c(15, 50, 100), order = list(list(2, "desc")),
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

  # ---- Nedladdning ----
  # Statistiken är redan sekretessgranskad när den läses in, släckta värden är tomma
  statistik_med_namn <- function(df) {
    df %>%
      left_join(geografinamn, by = "regionkod") %>%
      mutate(kommentar = coalesce(kommentar, "")) %>%
      select(ar, niva, regionkod, namn, any_of("kommun"), agarkategori, grupp, indikator_namn, enhet,
             taljare, namnare, varde, kommentar) %>%
      arrange(grupp, indikator_namn, agarkategori, niva, regionkod, ar)
  }

  output$export_excel <- downloadHandler(
    filename = function() "shb_statistik.xlsx",
    content = function(fil) write_xlsx(statistik_med_namn(shb_statistik), fil)
  )

  output$export_excel_urval <- downloadHandler(
    filename = function() "shb_statistik_urval.xlsx",
    content = function(fil) {
      koder <- c("00", "20", data_karta()$kod, vald_kommun())
      urval_val() %>%
        filter(regionkod %in% koder) %>%
        statistik_med_namn() %>%
        write_xlsx(fil)
    }
  )

})
