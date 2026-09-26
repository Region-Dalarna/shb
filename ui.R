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
        "  if (['diagram_geografi', 'diagram_alla', 'diagram_hogst', 'diagram_lagst'].includes(event.name)) {",
        "    $(event.target).closest('.diagram-cell--klickbar')",
        "      .find('.klick-hint').addClass('klick-hint--synlig');",
        "  }",
        "});"
      )),
      tags$script(HTML(
        # Diagram i en hopfälld <details> är dolda för Shiny, visa dem när rutan fälls ut
        "document.addEventListener('toggle', function(e) {",
        "  if (e.target.tagName === 'DETAILS' && e.target.open) $(e.target).trigger('shown');",
        "}, true);",
        # Länken 'Profil' i tabellen: fånga klicket innan tabellen tolkar det som val av rad
        "document.addEventListener('click', function(e) {",
        "  var a = e.target.closest && e.target.closest('a.profil-lank');",
        "  if (!a) return;",
        "  e.preventDefault(); e.stopPropagation();",
        "  Shiny.setInputValue('visa_profil', a.getAttribute('data-kod'), {priority: 'event'});",
        "}, true);",
        "function kopieraLank(knapp) {",
        "  navigator.clipboard.writeText(window.location.href).then(function() {",
        "    var text = knapp.querySelector('span'); var gammal = text.textContent;",
        "    text.textContent = 'Kopierad!'; setTimeout(function() { text.textContent = gammal; }, 2000);",
        "  });",
        "}"
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
      tabPanel('Karta och diagram',
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
                  div(class = "val-indikator", selectInput("val_indikator", "Indikator", choices = NULL)),
                  div(selectInput("val_agarkategori", "Ägarkategori", choices = NULL)),
                  div(class = "val-ar", selectInput("val_ar", "År", choices = NULL)),
                  actionButton("till_profil", label = NULL, icon = icon("id-card"),
                               class = "btn btn-light", title = "Visa områdesprofil för valt område"),
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
      tabPanel('Jämför områden',
        div(class = "jamfor-layout",
          div(class = "diagram-toolbar",
              div(class = "val-indikator", selectInput("jmf_indikator", "Indikator", choices = NULL)),
              div(selectInput("jmf_agarkategori", "Ägarkategori", choices = NULL)),
              div(class = "val-ar", selectInput("jmf_ar", "År", choices = NULL)),
              div(class = "jamfor-info", textOutput("jmf_info"))
          ),
          fluidRow(
            column(
              width = 6,
              div(class = "diagram-cell diagram-cell--klickbar",
                  div(class = "klick-hint", icon("hand-pointer"), span("Klicka på ett område för att se det i kartan")),
                  girafeOutput("diagram_hogst", width = "100%", height = "100%")
              )
            ),
            column(
              width = 6,
              div(class = "diagram-cell diagram-cell--klickbar",
                  div(class = "klick-hint", icon("hand-pointer"), span("Klicka på ett område för att se det i kartan")),
                  girafeOutput("diagram_lagst", width = "100%", height = "100%")
              )
            )
          ),
          h4(class = "jamfor-rubrik", "Alla områden"),
          p(class = "jamfor-hjalp", "Sök efter ett område eller en kommun, sortera genom att klicka på en kolumnrubrik
                                     och klicka på en rad för att se området i kartan."),
          DTOutput("tabell_omraden")
        )
      ),
      tabPanel('Områdesprofil',
        div(class = "profil-layout",
          div(class = "diagram-toolbar",
              div(class = "val-omraden",
                  selectizeInput("profil_omraden", "Områden – välj ett eller flera, de räknas ihop", choices = NULL,
                                 multiple = TRUE, width = "100%",
                                 options = list(placeholder = "Sök område eller kommun …", plugins = list("remove_button")))),
              div(selectInput("prof_agarkategori", "Ägarkategori", choices = NULL)),
              div(class = "val-ar", selectInput("prof_ar", "År", choices = NULL)),
              # Kopieringen görs direkt i webbläsaren, eftersom urklipp bara får skrivas vid ett klick
              tags$button(id = "kopiera_lank", type = "button", class = "btn btn-light", title = "Kopiera länk till profilen",
                          onclick = "kopieraLank(this)", icon("link"), span("Kopiera länk")),
              downloadButton("export_profil", "Excel", icon = icon("download"))
          ),
          uiOutput("profil_rubrik"),
          uiOutput("profil_nyckeltal"),
          fluidRow(
            column(width = 7, div(class = "diagram-cell profil-cell", girafeOutput("profil_huvud", width = "100%", height = "100%"))),
            column(width = 5, div(class = "diagram-cell profil-cell", girafeOutput("profil_inkomst", width = "100%", height = "100%")))
          ),
          div(class = "diagram-cell profil-tid", girafeOutput("profil_tid", width = "100%", height = "100%")),
          tags$details(class = "profil-detaljer",
            tags$summary("Bakgrundsvariabler"),
            div(class = "diagram-cell profil-cell", girafeOutput("profil_bakgrund", width = "100%", height = "100%"))
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
            tags$li('Fliken Karta och diagram visar en indikator i karta och diagram, från hela länet ned till enskilda områden.'),
            tags$li('Fliken Områdesprofil visar ett område, eller flera områden som räknas ihop, med alla indikatorer jämfört
                     med kommunen, Dalarna och riket. Adressen i webbläsaren uppdateras så att profilen går att spara som
                     bokmärke eller skicka som länk.'),
            tags$li(paste0('Fliken Jämför områden visar de ', shb_antal_rangordning, ' områden i länet som har högst respektive
                     lägst värde, och en tabell med alla områden där du kan söka efter ett område.')),
            tags$li('Välj indikator, ägarkategori och år ovanför diagrammen. Ägarkategorin visar om det gäller alla bostäder
                     (Totalt), allmännyttans bostäder eller bostäder med övriga ägare.'),
            tags$li('Figurer markerade med handikonen går att klicka i.'),
            tags$li('Klicka på en kommun i kartan eller i stapeldiagrammet för att se kommunens områden.'),
            tags$li('Det nedre diagrammet visar alla områden i länet, grupperade per kommun. Strecken visar kommunens värde.
                     Klicka på ett område för att gå till det.'),
            tags$li('Klicka på husikonen i kartan eller uppåtpilen ovanför diagrammen för att se alla kommuner igen.'),
            tags$li('För att spara ett diagram, för muspekaren över diagrammet och klicka på ikonen högst upp till höger.')
          ),

          h4('Områdesindelningen'),
          p('I några kommuner täcker shb-områdena inte hela kommunen, till exempel där bara centralorten är indelad.
             Delar av en kommun som inte ingår i något område visas vita i kartan. Invånarna där räknas med i kommunens,
             länets och rikets värden men visas inte som ett eget område. Gagnef är inte indelad i shb-områden.'),

          h4('Sekretess'),
          p('Statistiken bygger på uppgifter om enskilda personer och hushåll. För att ingen ska kunna pekas ut gäller följande:'),
          tags$ul(
            tags$li(paste0('Områden med färre än ', shb_min_befolkning_omrade, ' invånare visas inte.')),
            tags$li(paste0('Andelar som bygger på färre än ', shb_min_grupp, ' personer (eller hushåll), och antal under ',
                           shb_min_grupp, ', visas inte. Då visas varken andelen eller de antal den räknas fram från.')),
            tags$li(paste0('När färre än ', shb_min_taljare, ' personer (eller hushåll) har en egenskap visas inte antalet.
                           Andelen anges då som en övre gräns, till exempel under 2,4 %. I diagrammen visas de med ljusare färg
                           vid den högsta möjliga andelen, och i kartan med grått.'))
          ),
          p(paste0('I små grupper kan en eller ett par personer påverka en andel mycket. Andelar som bygger på färre än ',
                   shb_min_rangordning, ' personer ingår därför inte när områdena rangordnas, men visas i kartan, diagrammen och tabellen.')),
          p('När flera områden räknas ihop i områdesprofilen visas inte värdet om något av områdena har ett dolt värde, och
             är något av värdena ungefärligt blir det sammanräknade värdet ett intervall. Annars skulle ett dolt värde gå att
             räkna fram genom att jämföra olika urval av områden.'),

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
