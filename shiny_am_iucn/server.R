### server.R

source('server_fxns.R')

# am_prob_cols <- rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme
# am_pres_cols <- colorRampPalette(brewer.pal(9, 'Oranges'))(255) # orange color scheme
# iucn_cols    <- colorRampPalette(brewer.pal(9, 'Purples'))(255) # purple color scheme

server <- shinyServer(function(input, output, session) {
  
  ### Species Map Function ###
  spp_map   <- reactiveValues()
  coral_map <- reactiveValues()
  ### This function takes a single species scientific name as input, then grabs 
  ### all occurrence cells and associated probability per cell
  
  observeEvent(input$spp_group, ### select a group, then update species list choices
    {
      spp_choices <- spp_list %>%
        filter(spp_group_text == input$spp_group) %>%
        distinct() %>%
        .$sciname
      updateSelectInput(session, inputId = "species",
                        choices = spp_choices)
    }
  )
  
  observeEvent(
    {input$species; input$am_cutoff}, ### change in species selector regenerates both maps
    {
      message('observed change in input$species or input$am_cutoff; getting spp_map dataframe')
      spp_map$df <- get_spp_map_df(input$species, input$am_cutoff)
      print(head(spp_map$df))
    }
  )
  
  create_map <- observeEvent(
    {spp_map$df; input$show_maps}, 
      ### triggered by change in spp_map$am_rast (which may also include 
      ### change in iucn_rast) or a change in the selected maps to display
    {
      message('observed change in spp_map$df or input$show_maps; creating map')
      map_rast <- get_rast(spp_map$df, type = input$show_maps)
      map_obj  <- assemble_map_base(map_rast, spp = input$species)
      
      spp_map$map <- map_obj
    }
  )
  
  output$compare_map <- renderPlot({
    spp_map$map
  }, width = 800, height = 600) 
  
  output$quad_plot <- renderPlotly({
    create_quadplot(input$taxa_quad, input$expert_rev)
  })
  
  output$barchart <- renderPlot({
    create_barchart(input$expert_rev)
  })
  
  
  output$mini_quad <- renderPlot({
    create_miniquad(input$species)
  })

  output$coral_plot <- renderPlotly({
    create_coralplot()
  })
  
})
