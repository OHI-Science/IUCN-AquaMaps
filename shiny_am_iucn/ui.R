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
  
  tabPanel('Figures from paper',
           # Abstract of paper; sidebar is authors, publication info, etc
           sidebarPanel(
             h5('sidebar text'),
             h6('This panel could be a table of contents of figures, ',
                'with each a link to the figure that would navigate the ',
                'main panel to the appropriate figure.'),
             h6('OR: Thumbnails of figures in side bar, which pull up ',
                'the full figure with caption in the main window - ',
                'each figure gets its own .html file')
           ),
           mainPanel(
             includeMarkdown('pages/intro_main.md')
           )
  ),
  
  tabPanel('Species by quadrant',
  ### This tab shows side bar with controls: expert-reviewed vs. all.
  ### The main panel shows the quadrant plot with all vs expert-reviewed
  ### map pairs, and the bar chart below it to show the membership in various quads.
  ### Could there be a selection menu to select by taxa? yes, there could.
    fluidRow(column(2, 
      includeMarkdown('pages/quad_plot_methods.md')
    ),
      column(10,
        plotlyOutput('quad_plot')
      )
    )
  ),
  
  tabPanel('Species maps',
  ### This tab will have a side bar to select taxa, then species;
  ### at the bottom of the side bar will be a mini-quadmap to show where this
  ### species falls relative to all the others (this species is a red dot).
  ### There will be buttons to show only IUCN, only AM, or both;
  ### there will also be a slider for AquaMaps threshold?
  ### And a toggle to turn on/off the FAO boundaries on the main map.
  ### The main panel will show the species map for the given species,
  ### with the given parameters.
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
  
  tabPanel('Coral species',
  ### This page will have a sidebar to select specific coral species or all;
  ### there will be a toggle for a 200 m bathymetry polygon.
  ### Or will this page focus on the coral-only quad map and bar chart?
  ### Should we include option for other taxa that might be limited to 200 m, e.g. 
  ### damselfish and butterflyfish? or is that going out on a limb...
  ### if we did that, use the same process as corals to determine depths
    fluidRow(column(2, 
        h5('controls?')
      ),
      column(10,
        plotlyOutput('coral_plot')
      )
    )
  )
)