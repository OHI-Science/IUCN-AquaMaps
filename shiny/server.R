#server.R

source('server_fxns.R')



am_prob_cols <- rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme
am_pres_cols <- colorRampPalette(brewer.pal(9, 'Oranges'))(255) # orange color scheme
iucn_cols <- colorRampPalette(brewer.pal(9, 'Purples'))(255) # purple color scheme




shinyServer(function(input,output){
  
  ### Species Map Function ###
  # This function takes a single species scientific name as input, then grabs all occurrence cells and associated probability per cell
  spp_map <- reactive({
    spp <-input$species
    
    spp_map <- get_spp_map(spp)
  })
  
#   am_map_prob <- reactive({
#     am_map <- spp_map() %>%
#       mutate(am_pres = ifelse(am_prob >= input$am_cutoff, am_prob, NA))
#     r_am_spp <- subs(loiczid_raster, 
#                      am_map[ , c('loiczid', 'am_pres')], 
#                      by = 'loiczid', 
#                      which = 'am_pres', 
#                      subsWithNA = TRUE)
#     plot(r_am_spp, col = am_prob_cols, main = input$species, useRaster = FALSE)
#     map('world', col = 'gray95', fill = T, border = 'gray80', add = TRUE)
#   })
  
#   am_map_pres <- reactive({
#     am_map <- spp_map() %>%
#       mutate(am_pres = ifelse(am_prob >= input$am_cutoff, 1, NA))
#     r_am_spp <- subs(loiczid_raster, 
#                      am_map[ , c('loiczid', 'am_pres')], 
#                      by = 'loiczid', 
#                      which = 'am_pres', 
#                      subsWithNA = TRUE)
#     plot(r_am_spp, col = am_pres_cols, main = input$species, useRaster = FALSE)
#     map('world', col = 'gray95', fill = T, border = 'gray80', add = TRUE)
#   })
  
#   iucn_map <- reactive({
#     iucn_map <- spp_map() %>%
#       mutate(iucn_pres = ifelse(iucn_area > 0, 1, NA))
#     r_iucn_spp <- subs(loiczid_raster, 
#                        iucn_map[ , c('loiczid', 'iucn_pres')], 
#                        by = 'loiczid', 
#                        which = 'iucn_pres', 
#                        subsWithNA = TRUE)
#     plot(r_iucn_spp, col = iucn_cols, main = input$species, useRaster = FALSE)
#     map('world', col = 'gray95', fill = T, border = 'gray80', add = TRUE)
#   })
  
  map_compare <- reactive({
    if(input$am_form == 'pres') {
      am_map <- spp_map() %>%
        mutate(am_pres = am_prob >= input$am_cutoff, 1, NA)
    } else {
      am_map <- spp_map() %>%
        mutate(am_pres = am_prob >= input$am_cutoff, am_prob, NA)      
    }
    r_am_spp  <-  subs(loiczid_raster, 
                       am_map[ , c('loiczid', 'am_pres')], 
                       by = 'loiczid', 
                       which = 'am_pres', 
                       subsWithNA = TRUE)
    iucn_map <- spp_map() %>%
      mutate(iucn_pres = iucn_area > 0)
    r_iucn_spp <- subs(loiczid_raster, 
                       iucn_map[ , c('loiczid', 'iucn_pres')], 
                       by = 'loiczid', 
                       which = 'iucn_pres', 
                       subsWithNA = TRUE)

    if(input$top == 'iucn') {
      plot(r_am_spp, col = am_pres_cols, main = input$species, useRaster = FALSE, alpha = (1 - input$am_trans/100))
      plot(r_iucn_spp, col = iucn_cols, useRaster = FALSE, alpha = input$iucn_trans/100, add = TRUE)   
    } else {
      plot(r_iucn_spp, col = iucn_cols, main = input$species, useRaster = FALSE, alpha = (1 - input$iucn_trans/100))         
      plot(r_am_spp, col = am_pres_cols, useRaster = FALSE, alpha = input$am_trans/100, add = TRUE)
    }
    map('world', col = 'gray95', fill = T, border = 'gray80', add = TRUE)
  })
  
  
  output$aquamap_prob <- renderPlot({
    am_map_prob()
  })
  
  output$aquamap_pres <- renderPlot({
    am_map_pres()
  })
  
  output$iucn <- renderPlot({
    iucn_map()
  })
  
  output$comparePlot <- renderPlot({
    map_compare()
  })
  
  
})