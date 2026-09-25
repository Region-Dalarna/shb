source('global.R')

APP_TITEL <- 'Socialt hållbara bostadsområden'

shinyUI(
  fluidPage(
    tags$head(
      tags$title(APP_TITEL),
      tags$link(rel = 'icon', type = 'image/x-icon', href = 'favicon.ico'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'regiondalarna_ruf.css'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'app.css'),
      rdshinyappar::telemetri_ui(telemetry),

      # fada in klick-hint-badgen först när respektive diagram renderats
      tags$script(HTML(
        "$(document).on('shiny:value', function(event) {",
        "  if (event.name === 'diagram_geografi' || event.name === 'diagram_alla') {",
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
      tags$div(class = 'rd-header__title', APP_TITEL),
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
            # Höger: val överst, två diagram bredvid varandra och alla områden under
            column(
              width = 8,
              div(class = "diagram-toolbar",
                  div(selectInput("val_indikator", "Indikator", choices = NULL)),
                  div(selectInput("val_ar", "År", choices = NULL)),
                  actionButton("geografi_tillbaka", label = NULL, icon = icon("level-up-alt"),
                               class = "btn btn-light", title = "Tillbaka till alla kommuner")
              ),
              fluidRow(
                column(
                  width = 6,
                  div(class = "diagram-cell diagram-cell--klickbar",
                      div(class = "klick-hint",
                          icon("hand-pointer"),
                          textOutput("geografi_klick_text", inline = TRUE)),
                      girafeOutput("diagram_geografi", width = "100%", height = "100%")
                  )
                ),
                column(
                  width = 6,
                  div(class = "diagram-cell",
                      girafeOutput("diagram_tid", width = "100%", height = "100%")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  div(class = "diagram-cell diagram-cell--klickbar",
                      div(class = "klick-hint",
                          icon("hand-pointer"),
                          span("Klicka på ett område för att välja det")),
                      girafeOutput("diagram_alla", width = "100%", height = "100%")
                  )
                )
              )
            )
          )
        )
      ),
      tabPanel('Om',
        div(class = "om-text",
          h4('Om rapporten'),
          p('Rapporten är en modell med fakta som identifierar socialt hållbara bostadsområden i Dalarnas län,
             och där man år för år kan följa den sociala utvecklingen i ett bostadsområde. Den är framtagen av
             Samhällsanalys, Region Dalarna, i samarbete med Länsstyrelsen Dalarna och Kopparstaden.'),

          h4('Bakgrund'),
          p('Arbetet har sin bakgrund i den regionala överenskommelsen Vägen in och det utpekade samverkansområdet
             Vägen till bostad. Det kommer återkommande rapporter om att bostadssegregationen ökar i Sverige, och
             modellen är ett sätt att se hur det ser ut i Dalarna.'),

          h4('Syfte'),
          p('Modellen ska fungera som ett verktyg för berörda aktörer genom att'),
          tags$ul(
            tags$li('ge signaler om hur ett specifikt bostadsområde, en kommun och Dalarna mår'),
            tags$li('vägleda vid beslut om vilka resurser som gör bäst nytta var'),
            tags$li('visa effekten av de åtgärder som vidtas, som uppföljning')
          ),

          h4('Indikatorer'),
          tags$ul(
            tags$li('Huvudsaklig inkomstkälla'),
            tags$li('Långtidsarbetslöshet'),
            tags$li('Andel lågutbildade (högst nioårig grundskola eller motsvarande)'),
            tags$li('Andel med ekonomiskt bistånd under minst sex månader under året'),
            tags$li('Andel med låg köpkraft'),
            tags$li('Trångboddhet enligt norm 3: fler än en person per rum utöver kök och vardagsrum, vuxna par undantagna')
          ),
          p('Områdena delas in i allmännyttiga bostadsområden och övriga. För en djupare analys finns också
             bakgrundsvariabler: befolkning, antal hushåll, antal barnfamiljer, andel kvinnor, andel barn,
             andel 65–79 år, andel 80 år och äldre, andel boende i hyresrätt, andel som invandrat för mindre än
             fem år sedan, andel utomeuropeiskt födda och andel med hög köpkraft.'),

          h4('Så använder du rapporten'),
          tags$ul(
            tags$li('Välj indikator och år ovanför diagrammen.'),
            tags$li('Figurer markerade med handikonen går att klicka i.'),
            tags$li('Klicka på en kommun i kartan eller i stapeldiagrammet för att se kommunens områden.'),
            tags$li('Det nedre diagrammet visar alla områden i länet, grupperade per kommun. Strecken visar kommunens värde.
                     Klicka på ett område för att gå till det.'),
            tags$li('Klicka på husikonen i kartan eller uppåtpilen ovanför diagrammen för att se alla kommuner igen.'),
            tags$li('För att spara ett diagram, för muspekaren över diagrammet och klicka på ikonen högst upp till höger.')
          ),

          h4('Kontakt'),
          p('Samhällsanalys, Region Dalarna, ',
            tags$a(href = 'mailto:samhallsanalys@regiondalarna.se?subject=Socialt hållbara bostadsområden',
                   'samhallsanalys@regiondalarna.se'))
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
