#ui.r


shinyUI(fluidPage(
  
  #Application Title
  titlePanel("Comparing IUCN and AquaMaps species ranges"),
  
  #Sidebar with controls to select the species and the aquamaps probability
  # of occurrence cutoff point
  sidebarLayout(
    sidebarPanel(
      selectInput("species","Choose a species:",
                  choices=c('A','B','C')),
      
      sliderInput('cutoff','Probability of Occurrence cutoff:',
                  min=0,max=1,value=0.4,step=0.1)
      
      ),
    mainPanel("main panel")
  )
))