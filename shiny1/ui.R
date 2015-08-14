#ui.r

shinyUI(fluidPage(
  
  #Application Title
  titlePanel("Comparing IUCN and AquaMaps species ranges"),
  
  #Sidebar with controls to select the species and the aquamaps probability
  # of occurrence cutoff point
  fluidRow(
    column(2,
      selectInput("species","Choose a species:",
                  choices = spp_choices),
      sliderInput('am_cutoff','Probability of Occurrence cutoff (Aquamaps):',
                  min = 0, max = 1, value = .4, step = 0.1),
      sliderInput('am_trans','Aquamaps opacity (0 = transparent):',
                  min = 0, max = 100, value = 50, step = 10),
      sliderInput('iucn_trans','IUCN opacity (0 = transparent):',
                  min = 0, max = 100, value = 50, step = 10),
      radioButtons('top', 'Which layer on top?',
                   c(IUCN     = 'iucn',
                     AquaMaps = 'am'),
                   'am'),
      radioButtons('am_form', 'Aquamaps as presence or probability?',
                   c(Presence    = 'pres',
                     Probability = 'prob'),
                   'prob')
      radioButtons('map_height', 'Map size?',
                   c(Small = 500,
                     Large = 800),
                   500)
    ),
    
    column(10,
      plotOutput("comparePlot")
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Comparison",
#                  plotOutput("comparePlot")),
#         
#         tabPanel("AM probability",
#                  plotOutput("aquamap_prob")),
# 
#         tabPanel("AM presence",
#                  plotOutput("aquamap_pres")),
#         
#         tabPanel("IUCN presence", 
#                  plotOutput("iucn"))
#         )
#       )
    )
  
)))