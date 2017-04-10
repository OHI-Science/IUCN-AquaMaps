#ui.r

source('ui_setup.R')

### Tab 1: intro/explanation/link to published paper
### Tab 2: sample maps showing AM/IUCN/both; to select a species, can it be 
###   tied to the mouseovers on the quad plot? somehow link quad plot to maps
### Tab 3: sample coral maps showing depth overreach; can this be tied to the
###   modified quad plot?

ui <- navbarPage('Aligning marine species range data to better serve science and conservation',
                 theme = shinytheme('cerulean'),
                 
  tabPanel('Abstract',
    sidebarPanel(
      includeMarkdown('pages/author_list.md')
    ),
    mainPanel(
      includeMarkdown('pages/abstract.md')
      )
  ),
  

  tabPanel('Figures',
    # Abstract of paper; sidebar is authors, publication info, etc
    fluidRow(
      column(2, 
        h5('sidebar text'),
        h6('This panel could be a table of contents of figures, ',
           'with each a link to the figure that would navigate the ',
           'main panel to the appropriate figure.'),
        h6('OR: Thumbnails of figures in side bar, which pull up ',
           'the full figure with caption in the main window - ',
           'each figure gets its own .html file')
      ),
      column(10,
        h4('Figures from manuscript'),
        includeMarkdown('pages/fig1.md'),
        hr(),
        includeMarkdown('pages/fig2.md'),
        hr(),
        includeMarkdown('pages/fig3.md'),
        hr(),
        includeMarkdown('pages/fig4.md'),
        hr(),
        includeMarkdown('pages/fig5.md'),
        hr(),
        h4('Supporting figures'),
        includeMarkdown('pages/s1fig.md'),
        hr(),
        includeMarkdown('pages/s2fig.md'),
        hr(),
        includeMarkdown('pages/s3fig.md'),
        hr(),
        includeMarkdown('pages/s4fig.md'),
        hr(),
        includeMarkdown('pages/s5fig.md')
      )
    )
  ),
  
  tabPanel('Map alignment',
    fluidRow(
      column(2, 
        selectInput('taxa_quad', 'Choose a taxon:',
                    choices = c('all', unique(spp_list$spp_group_text)),
                    selected = 'all'),
        radioButtons('expert_rev', label = 'AquaMaps review status',
                     choices = list('All'           = 'all',
                                    'Expert rev\'d' = 'expert'),
                     selected = 'all')
      ),
      column(10,
        plotlyOutput('quad_plot'),
        hr(),
        plotOutput('barchart')
      )
    )
  ),
  
  tabPanel('Species maps',
  ### This tab will have a side bar to select taxa, then species;
  ### at the bottom of the side bar will be a mini-quadmap to show where this
  ### species falls relative to all the others (this species is a red dot).
  ### There will be buttons to show only IUCN, only AM, or both;
  ### there will also be a slider for AquaMaps threshold?
  ### Include FAO boundaries on the main map - no need for toggle on/off.
  ### The main panel will show the species map for the given species,
  ### with the given parameters.
    fluidRow(
      column(2,
        h6('Select species group, then
          select species'),
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
        hr(),
        plotOutput('mini_quad')
      ),
      column(10,
        plotOutput('compare_map')
      )
    )
  ),
  
  tabPanel('Coral depth',
  ### This page will have a sidebar to select specific coral species or all;
  ### there will be a toggle for a 200 m bathymetry polygon.
  ### Or will this page focus on the coral-only quad map and bar chart?
  ### Should we include option for other taxa that might be limited to 200 m, e.g. 
  ### damselfish and butterflyfish? or is that going out on a limb...
  ### if we did that, use the same process as corals to determine depths
    fluidRow(
      column(2, 
        h5('controls?'),
        h6('Toggle for bathymetry poly')
      ),
      column(10,
        plotlyOutput('coral_plot'),
        hr(),
        h5('Bar chart goes here')
      )
    )
  ),
  
  tabPanel('References',
    fluidRow(
      column(2,
        includeMarkdown('pages/citation.md')
      ),
      column(10,
        includeMarkdown('pages/references.md')
      )
    )
  )
)