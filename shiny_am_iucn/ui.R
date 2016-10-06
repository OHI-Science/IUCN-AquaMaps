#ui.r

source('ui_setup.R')

### Tab 1: intro/explanation/link to published paper
### Tab 2: sample maps showing AM/IUCN/both; to select a species, can it be 
###   tied to the mouseovers on the quad plot? somehow link quad plot to maps
### Tab 3: sample coral maps showing depth overreach; can this be tied to the
###   modified quad plot?

ui <- navbarPage('Aligning marine species range data to better serve science and conservation',
                 theme = shinytheme('cerulean'),
                 
  tabPanel('Introduction',
    # Abstract of paper; sidebar is authors, publication info, etc
    sidebarPanel(
      h5('sidebar text')
    ),
    mainPanel(
      includeMarkdown('pages/intro_main.md')
      )
  ),
  
  tabPanel('Species paired map alignment',
    fluidRow(column(2, 
      includeMarkdown('pages/quad_plot_methods.md')
    ),
      column(10,
        plotlyOutput('quad_plot')
      )
    )
  ),
  
  tabPanel('Species paired map comparison',
    fluidRow(column(2,
        h6('Add selector for species group, then
          adjust species selector accordingly'),
        selectInput('spp_group', 'Choose a taxon:',
                    choices = unique(spp_list$spp_group_text)),
        selectInput('species', 'Choose a species:',
                    choices = unique(spp_list$sciname)),
        radioButtons('show_maps', label = h3('Maps'),
                     choices = list('AquaMaps' = 'am',
                                    'IUCN'     = 'iucn', 
                                    'Both'     = 'both'),
                     selected = 'both'),
        sliderInput('am_cutoff', 'Probability of Occurrence cutoff (Aquamaps):',
                    min = 0, max = 1, value = 0, step = 0.1),
        verbatimTextOutput('summary')
      ),
      column(10,
        plotOutput('compare_map')
      )
    )
  ),
  
  tabPanel('Coral map depth clipping',
    fluidRow(column(2, 
        h5('controls?')
      ),
      column(10,
        plotlyOutput('coral_plot')
      )
    )
  ), 
  
  tabPanel('FAO boundary map exploration',
    fluidRow(column(2, 
        h5('controls?')
      ),
      column(10,
            plotlyOutput('coral_plot')
      )
    )
  )
)