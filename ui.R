source('global.R')

# Liten alltid-synlig badge som markerar klickbara figurer
klick_hint <- function(text = "Klickbar – borra ner") {
  div(class = "klick-hint",
      icon("hand-pointer"),
      span(text))
}

shinyUI(
  fluidPage(
    tags$head(
      tags$link(rel = 'icon', type = 'image/x-icon', href = 'favicon.ico'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'regiondalarna_ruf.css'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'app.css'),
      rdshinyappar::telemetri_ui(telemetry),

      # fada in klick-hint-badgen först när diagrammet renderats
      tags$script(HTML(
        "$(document).on('shiny:value', function(event) {",
        "  if (event.name === 'diagram_geografi') {",
        "    $(event.target).closest('.diagram-cell--klickbar')",
        "      .find('.klick-hint').addClass('klick-hint--synlig');",
        "  }",
        "});"
      ))
    ),

    useShinyjs(),

    # ---- Header (matchar .rd-header i regiondalarna_ruf.css) --------------
    tags$div(
      class = 'rd-header',
      tags$div(class = 'rd-header__title', 'shb'),
      tags$a(
        class  = 'rd-header__right',
        href   = 'https://www.regiondalarna.se',
        target = '_blank',
        tags$img(src = 'logo_liggande_fri_vit.png', alt = 'Region Dalarna')
      )
    ),

    # ---- Innehåll ---------------------------------------------------------
    tabsetPanel(
      id = 'flikval',
      tabPanel('Statistik',
        div(class = "karta-layout",
          fluidRow(
            # Vänster: kartan + nedladdningsknappar
            column(
              width = 4,
              leafletOutput("karta_shb", height = "70vh"),
              div(class = "karta-knapp",
                  downloadButton("export_excel", "Hela datasetet", icon = icon("download")),
                  downloadButton("export_excel_urval", "Aktuellt urval", icon = icon("download"))
              )
            ),
            # Höger: val + två diagram
            column(
              width = 8,
              div(class = "diagram-toolbar",
                  div(selectInput("val_indikator", "Indikator", choices = NULL)),
                  div(selectInput("val_ar", "År", choices = NULL)),
                  actionButton("geografi_tillbaka", label = NULL, icon = icon("level-up-alt"),
                               class = "btn btn-light", title = "Tillbaka till alla kommuner")
              ),
              div(class = "diagram-cell diagram-cell--klickbar",
                  div(class = "klick-hint",
                      icon("hand-pointer"),
                      textOutput("geografi_klick_text", inline = TRUE)),
                  girafeOutput("diagram_geografi", width = "100%", height = "100%")
              ),
              div(class = "diagram-cell",
                  girafeOutput("diagram_tid", width = "100%", height = "100%")
              )
            )
          )
        )
      ),
      tabPanel('Om',
        div(class = "om-text",
          p('Beskriv applikationen här.'),
          tags$ul(
            tags$li('Klicka på en kommun i kartan eller i stapeldiagrammet för att se kommunens områden.'),
            tags$li('Klicka på ett område för att markera det och se dess utveckling över tid.'),
            tags$li('Klicka på husikonen i kartan eller uppåtpilen ovanför diagrammen för att se alla kommuner igen.'),
            tags$li('För att spara ett diagram, för muspekaren över diagrammet och klicka på ikonen högst upp till höger.')
          )
        )
      )
    ),

    # ---- Footer (matchar .rd-footer i regiondalarna_ruf.css) --------------
    tags$div(
      class = 'rd-footer',
      'Samhällsanalys, Region Dalarna · ',
      tags$a(
        href = 'mailto:samhallsanalys@regiondalarna.se',
        'samhallsanalys@regiondalarna.se'
      )
    )
  )
)
