#ui.r

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)

library(shinythemes)

### Tab 1: intro/explanation/link to published paper
### Tab 2: sample maps showing AM/IUCN/both; to select a species, can it be 
###   tied to the mouseovers on the quad plot? somehow link quad plot to maps
### Tab 3: sample coral maps showing depth overreach; can this be tied to the
###   modified quad plot?

ui <- navbarPage('Aligning marine species range data to better serve science and conservation',
                 theme = shinytheme('cerulean'),
                 
  tabPanel('Introduction',
    # main tab: composite map (no ranges of values, only go/no go); check boxes to turn on/off clusters
    sidebarPanel(
      h5('sidebar text')
    ),
    mainPanel(
      h5('main panel text')
    )
  ),
  
  tabPanel('Quad plot',
    fluidRow(column(2, 
        h5('controls?')
      ),
      column(10,
        plotlyOutput('quad_plot')
      )
    )
  ),
  
  tabPanel('Species map compare',
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
                    min = 0, max = 1, value = .4, step = 0.1),
        verbatimTextOutput('summary')
      ),
      column(10,
        plotOutput('compare_map')
      )
    )
  ),
  
  tabPanel('Coral map compare',
    fluidRow(column(2, 
        h5('controls?')
      ),
      column(10,
        plotlyOutput('coral_plot')
      )
    )
  )
)