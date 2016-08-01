#ui.r

### Tab 1: intro/explanation/link to published paper
### Tab 2: sample maps showing AM/IUCN/both; to select a species, can it be 
###   tied to the mouseovers on the quad plot? somehow link quad plot to maps
### Tab 3: sample coral maps showing depth overreach; can this be tied to the
###   modified quad plot?
ui <- navbarPage("Aligning marine species range data to better serve science and conservation",
  tabPanel("Introduction",
    # main tab: composite map (no ranges of values, only go/no go); check boxes to turn on/off clusters
    sidebarPanel(
      h5('sidebar text')
    ),
    mainPanel(
      h5('main panel text')
    )
  ),
  tabPanel('Quad plot',
    sidebarPanel(
      h5('controls?')
    ),
    mainPanel(
      h5('quad plot with interactivity')
    )
  ),
  tabPanel('Species map compare',
    fluidRow(column(2,
        h6('Add selector for species group, then
        adjust species selector accordingly'),
        selectInput("species","Choose a species:",
        choices = spp_choices),
        radioButtons("checkGroup",label=h3("Maps"),
        choices = list("AquaMaps"= 1,"IUCN"= 2, "Both" = 3),
        selected=1),
        sliderInput('am_cutoff','Probability of Occurrence cutoff (Aquamaps):',
        min = 0, max = 1, value = .4, step = 0.1),
        verbatimTextOutput("summary")
      ),
      column(10,
        plotOutput("comparePlot")
      )
    )
  ),
  tabPanel('Coral map compare',
    fluidRow(column(2,
        h6('coral change plot?'),
      ),
      column(10,
        h6('species map for corals')
      )
    )
  )
)