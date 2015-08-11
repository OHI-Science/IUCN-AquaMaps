#server.R

source('global.R')



cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme



shinyServer(function(input,output){
  
  ### Species Map Function ###
  # This function takes a single species scientific name as input, then grabs all occurrence cells and associated probability per cell

  
  output$aquamap <- renderPlot({
    
    sp_map_fun <- function(species){
      
      sp_id  = filter(spp,scientific==species)$SPECIESID
      sp_map = filter(spp_cells,species_id==sp_id)%>%
        merge(cells,by='csquare_code')
      
      return(sp_map)
    }
    
    
    sp <-input$species
    
    name = as.character(sp)
    
    sp_cells <- sp_map_fun(sp)
    
    r_sp = subs(r, sp_cells[,c('cid','probability')], by='cid', which='probability', subsWithNA=T)

    plot(r_sp, col=cols,main=name)
    map('world',col='gray95',fill=T,border='gray80',add=T)
    
  })
  
  
  
  
})