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
  
)))