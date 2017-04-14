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
    sidebarPanel(
      includeMarkdown('pages/author_list.md')
    ),
    mainPanel(
      h4('Figures from manuscript'),
      h5('Figure 1:'),
      includeMarkdown('pages/fig1.md'),
      hr(),
      h5('Figure 2:'),
      includeMarkdown('pages/fig2.md'),
      hr(),
      h5('Figure 3:'),
      includeMarkdown('pages/fig3.md'),
      hr(),
      h5('Figure 4:'),
      includeMarkdown('pages/fig4.md'),
      hr(),
      h5('Figure 5:'),
      includeMarkdown('pages/fig5.md')
    )
  ),
  
  tabPanel('Map alignment',
    sidebarPanel(
      includeMarkdown('pages/align_tab_side1.md'),
      selectInput('taxa_quad', 'Choose a taxon:',
                  choices = c('all', unique(spp_list$spp_group_text) %>% sort()),
                  selected = 'all'),
      radioButtons('expert_rev', label = 'AquaMaps review status',
                   choices = list('All'             = 'all',
                                  'Expert reviewed' = 'expert'),
                   selected = 'all'),
      includeMarkdown('pages/footer_sidebar.md')
    ),
    mainPanel(
      includeMarkdown('pages/align_tab_main1.md'),
      plotlyOutput('quad_plot', height = '300px'),
      hr(),
      includeMarkdown('pages/align_tab_main2.md'),
      plotOutput('barchart', height = '300px')
    )
  ),
  
  tabPanel('Species maps',
  ### Include FAO boundaries on the main map - no need for toggle on/off.
  ### The main panel will show the species map for the given species,
  ### with the given parameters.
    sidebarPanel(
      includeMarkdown('pages/map_tab_side1.md'),
      selectInput('spp_group', 'Select a taxonomic group:',
                  choices = unique(spp_list$spp_group_text) %>% sort()),
      selectInput('species', 'Then select a species:',
                  choices = unique(spp_list$sciname) %>% sort()),
      radioButtons('show_maps', label = 'Data source',
                   choices = list('AquaMaps' = 'am',
                                  'IUCN'     = 'iucn', 
                                  'Both'     = 'both'),
                   selected = 'both'),
      includeMarkdown('pages/map_tab_side2.md'),
      plotOutput('mini_quad', height = '150px'),
      includeMarkdown('pages/footer_sidebar.md')
    ),
    mainPanel(
      includeMarkdown('pages/map_tab_main1.md'),
      plotOutput('compare_map', width = '100%')
    )
  ),
  
  tabPanel('Coral depth',
  ### This page will have a sidebar to select specific coral species or all;
  ### there will be a toggle for a 200 m bathymetry polygon.
  ### Or will this page focus on the coral-only quad map and bar chart?
  ### Should we include option for other taxa that might be limited to 200 m, e.g. 
  ### damselfish and butterflyfish? or is that going out on a limb...
  ### if we did that, use the same process as corals to determine depths
    sidebarPanel(
      includeMarkdown('pages/coral_tab_side1.md'),
      selectInput('coral_spp', 'Select a coral species:',
                  choices = coral_spp_list$sciname %>% 
                                     sort()),
      includeMarkdown('pages/coral_tab_side2.md'),
      plotOutput('coral_quad', height = '150px'),
      includeMarkdown('pages/footer_sidebar.md')
    ),
    mainPanel(
      includeMarkdown('pages/coral_tab_main1.md'),
      plotOutput('coral_map'),
      includeMarkdown('pages/coral_tab_main2.md'),
      plotOutput('coral_barchart', height = '250px')
    )
  ),
  
  tabPanel('References',
    sidebarPanel(
      includeMarkdown('pages/citation.md')
    ),
    mainPanel(
      includeMarkdown('pages/references.md')
    )
  ),
  
  tabPanel('SI',
    # Abstract of paper; sidebar is authors, publication info, etc
    sidebarPanel(
      includeMarkdown('pages/author_list.md')
    ),
    mainPanel(
      h4('Supporting figures'),
      h5('Figure S1:'),
      includeMarkdown('pages/s1fig.md'),
      hr(),
      h5('Figure S2:'),
      includeMarkdown('pages/s2fig.md'),
      hr(),
      h5('Figure S3:'),
      includeMarkdown('pages/s3fig.md'),
      hr(),
      h5('Figure S4:'),
      includeMarkdown('pages/s4fig.md'),
      hr(),
      h5('Figure S5:'),
      includeMarkdown('pages/s5fig.md'),
      hr(),
      includeMarkdown('pages/si_refs.md')
    )
  )
)