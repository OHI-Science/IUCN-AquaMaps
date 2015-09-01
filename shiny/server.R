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
  
  
  map_compare <- reactive({
    
    am_map <- spp_map()%>%
                mutate(am_pres = ifelse(am_prob >= input$am_cutoff,1,NA))
    
    
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
    
    
    if(input$checkGroup==1){
      plot(r_am_spp,col=am_pres_cols[128],main=input$species,useRaster=FALSE,axes=FALSE,box=FALSE,legend=FALSE)
      legend("topright",legend="AquaMaps",fill=am_pres_cols[128])
      map('world', col = 'gray95', fill = T, border = 'gray80', add = TRUE)
    }
    
    if(input$checkGroup==2){
      plot(r_iucn_spp,col = iucn_cols, useRaster = FALSE,axes=FALSE,box=FALSE,legend=FALSE)
      legend("topright",legend="IUCN",fill=iucn_cols[128])
      map('world', col = 'gray95', fill = T, border = 'gray80', add = TRUE)
    }
    
    if(input$checkGroup==3){
      plot(r_iucn_spp,col=iucn_cols,useRaster=FALSE,axes=FALSE,box=FALSE,legend=FALSE)
      plot(r_am_spp,col=am_pres_cols,main=input$species,useRaster=FALSE,alpha=0.8,add=T,axes=FALSE,box=FALSE,legend=FALSE)
      legend("topright",legend=c("AquaMaps","IUCN"),fill=c(am_pres_cols[128],iucn_cols[128]))
      map('world', col = 'gray95', fill = T, border = 'gray80', add = TRUE)
    }
  
  output$comparePlot <- renderPlot({
    map_compare()
  }, width = 1000, height = 600)
  
  
  # get percent overlap of map
  
  #iucn map cells
  cells_iucn<-spp_map()%>%
    filter(iucn_pres==TRUE)%>%
    .$loiczid
  cells_am <- spp_map()%>%
    filter(!is.na(am_prob))%>%
    .$loiczid
  
  cells_total = unique(spp_map()$loiczid)
  cells_overlap = intersect(cells_iucn,cells_am)
  
  #percent overlap
  perc = (length(cells_overlap)/length(cells_total))*100
  
  am_only = setdiff(cells_am,cells_iucn)
  iucn_only = setdiff(cells_iucn,cells_am)
  
  perc_am = (length(am_only)/length(cells_total))*100
  perc_iucn = (length(iucn_only)/length(cells_total))*100

  output$summary <- renderText({
    
    paste("Percent overlap is", perc,"%")
    paste(perc_am,"% of the total area is just AquaMaps cells")
    paste(perc_iucn,"% of the total area is just IUCN")
    
    
  })
  
})