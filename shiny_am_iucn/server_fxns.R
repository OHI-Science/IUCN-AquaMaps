#server_fxns.R

library(raster)
library(maps)
library(tmap)
data(World)
library(RColorBrewer)


### read data for raster (used to plot species)
message('Reading in raster info...\n')
loiczid_raster_file  <- 'data/loiczid_raster.tif'
loiczid_raster       <- raster(loiczid_raster_file)
names(loiczid_raster) <- 'loiczid'

### read in spp cell files
iucn_spp_cells <- read_csv('data/iucn_cells.csv', col_types = 'dd')
am_spp_cells   <- read_csv('data/am_cells.csv')


### Species Map Function ###
# This function takes a single species scientific name as input, then grabs all 
# occurrence cells and associated Aquamaps probability and/or IUCN proportional area
# per cell
get_spp_map_df <- function(species, am_cutoff = 0){ ### species <- spp_list$sciname[1]
  message('in get_spp_map_df()')
  ### am_cutoff is currently ignored
  
  spp_id <- spp_list %>%
    filter(sciname == species) %>%
    dplyr::select(am_sid, iucn_sid, sciname) %>%
    distinct()
  
  iucn_spp_map <- iucn_spp_cells %>%
    filter(iucn_sid %in% spp_id$iucn_sid) %>%
    group_by(loiczid) %>%        
    summarize(iucn_sid = first(iucn_sid))
      ### The group_by() and summarize() are to eliminate duped cells (e.g. 
      ### one am_sid matching two iucn_sids)
    
  am_spp_map   <- am_spp_cells %>%
    filter(am_sid == spp_id$am_sid)
  
  spp_map_df <- full_join(iucn_spp_map, am_spp_map, by = 'loiczid') %>%
    mutate(am_pres = ifelse(is.na(am_sid), NA, 1),
           iucn_pres = ifelse(is.na(iucn_sid), NA, 1)) %>%
    dplyr::select(loiczid, am_pres, iucn_pres) %>%
    mutate(both_pres = ifelse(!is.na(iucn_pres), ### IUCN presence, so...
                              ifelse(is.na(am_pres), 2, 3), ### if no AM, assign 2; if AM, assign 3. 
                              am_pres))          ### no IUCN, so assign 1 for AM and NA for none
  
  return(spp_map_df)
}

get_rast <- function(spp_map_df, type) {
  message('in get_rast()')
  rast_obj  <-  subs(loiczid_raster, spp_map_df, 
                     by    = 'loiczid', 
                     which = paste0(type, '_pres'), 
                     subsWithNA = TRUE)
  
  return(rast_obj)
}

assemble_map <- function(map_rast, spp) {
  message('in assemble_map()')
  map_obj <- tm_shape(map_rast) +
    tm_raster(palette = 'Spectral',
              colorNA = NULL,
              title = spp,
              alpha = 1) +
    tm_shape(World) +
      tm_polygons() + 
    tm_layout(basemaps = "Esri.WorldTopoMap", title.position = 'TOP', legend.outside = TRUE, attr.outside = TRUE)
        
  
  # if(show_maps %in% c('am', 'both')) {
  #   message('loading AquaMaps raster')
  #   am_map <- tm_shape(am_rast) +
  #     tm_raster(palette = 'Oranges',
  #               colorNA = NULL,
  #               title = 'AquaMaps',
  #               alpha = .6)
  # }
  # if(show_maps %in% c('iucn', 'both')) {
  #   message('loading IUCN raster')
  #   iucn_map <- tm_shape(iucn_rast) +
  #     tm_raster(palette = 'Purples',
  #               colorNA = NULL,
  #               title = 'IUCN',
  #               alpha = .6)
  # }
  # 
  # map_obj <- switch(show_maps,
  #                   'am'   = am_map,
  #                   'iucn' = iucn_map,
  #                   'both' = am_map + iucn_map) +
  #   tm_shape(World) +
  #     tm_polygons()

  return(map_obj)
}




create_quadplot <- function() {
  library(ggplot2)
  
  ggtheme_basic <- theme(axis.ticks = element_blank(),
    text = element_text(family = 'Helvetica', color = 'gray30', size = 8),
    plot.title = element_text(size = rel(1.25), hjust = 0, face = 'bold'),
    panel.background = element_blank(),
    legend.position = 'right')
  
  ggtheme_plot <- ggtheme_basic + 
    theme(panel.border     = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_line(colour = 'grey90'),
          axis.line = element_line(colour = "grey30"))
  
  quad_list <- read_csv('shiny_am_iucn/data/quad_list.csv')
  ### define 25%, 50%, and 75% quartiles for both x and y axes
  
  area_align_mean <- mean(quad_list$area_ratio, na.rm = TRUE)
  dist_align_mean <- mean(quad_list$sm_perc, na.rm = TRUE)
  
  ### mongo plot time
  scatter_quadplot <- ggplot(quad_list %>% 
                               rename(dist_align = sm_perc) %>%
                               transform(popn_category = reorder(popn_category, category_score)),
                             aes(x = area_ratio, 
                                 y = dist_align,
                                 key = sciname)) + #, 
    #                               size = lg_area_pct,
    #                              color = popn_category)) + 
    ggtheme_plot + 
    ### color the quadrant backgrounds:
    annotate("rect", xmin = area_align_mean, xmax = 100, 
             ymin = dist_align_mean, ymax = 100, 
             alpha = .3, 
             fill= "#4dac26")  + 
    annotate("rect", xmax = area_align_mean, xmin =   0, 
             ymin = dist_align_mean, ymax = 100, 
             alpha = .3, 
             fill= "#b8e186") + 
    annotate("rect", xmin = area_align_mean, xmax = 100, 
             ymax = dist_align_mean, ymin =   0, 
             alpha = .3, 
             fill= "#f1b6da") + 
    annotate("rect", xmax = area_align_mean, xmin =   0, 
             ymax = dist_align_mean, ymin =   0, 
             alpha = .3, 
             fill= "#d01c8b") + 
    geom_point(color = '#4d4dac', alpha = .6)
  
  ### Manage scales for color and size 
  scatter_quadplot <- scatter_quadplot +
    #   scale_colour_manual(values = c('LC' = 'green4', 
    #                                  'NT' = 'greenyellow', 
    #                                  'VU' = 'yellow', 
    #                                  'EN' = 'orange', 
    #                                  'CR' = 'orangered2',
    #                                  'DD' = 'grey30')) + 
    #   scale_size_continuous(limits = c(0, 100),
    #                         breaks = c( 0,    25,    50,    75,    100), 
    #                         labels = c('0%', '25%', '50%', '75%', '100%')) + 
    scale_x_continuous(expand = c(0, 0), 
                       limits = c(0, 102),
                       breaks = c(seq(0, 100, 25)),
                       labels = c('0%', '25%', '50%', '75%', '100%')) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, 103),
                       breaks = c(seq(0, 100, 25)),
                       labels = c('0%', '25%', '50%', '75%', '100%'))
  
  
  ### here are mean labels:
  scatter_quadplot <- scatter_quadplot +
    annotate(geom = 'text',
             x = area_align_mean, y = 5,
             hjust = 0, vjust = 0,
             color = 'grey30', 
             size = 2,
             fontface = 'bold.italic', angle = 90,
             label = sprintf('Mean = %s%%', round(area_align_mean, 1))) +
    annotate(geom = 'text',
             x = 5, y = dist_align_mean,
             hjust = 0, vjust = 0,
             color = 'grey30', 
             size = 2,
             fontface = 'bold.italic', angle = 0,
             label = sprintf('Mean = %s%%', round(dist_align_mean, 1)))
  
  
  scatter_quadplot <- scatter_quadplot +
    labs(x = bquote('Area ratio (%)'), 
         y = bquote('Distribution alignment (%)')) #, 
  #       title = 'Distribution alignment vs Extent alignment',
  #       size = '% ocean range',
  #      color = 'Extinction\nRisk') 
  
  return(scatter_quadplot)
}




create_coralplot <- function() {
  spp_coralmaps <- read_csv('shiny_am_iucn/data/coral_spp_areas.csv')
  
  ggtheme_basic <- theme(axis.ticks = element_blank(),
                         text = element_text(family = 'Helvetica', color = 'gray30', size = 8),
                         plot.title = element_text(size = rel(1.25), hjust = 0, face = 'bold'),
                         panel.background = element_blank(),
                         legend.position = 'right')
  
  ggtheme_plot <- ggtheme_basic + 
    theme(panel.border     = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_line(colour = 'grey90'),
          axis.line = element_line(colour = "grey30"))
  
  iucn_coral_fixed <- spp_coralmaps %>%
    rename(dist_align_raw = sm_perc_raw, dist_align_clipped = sm_perc_clipped) %>%
    gather(dist, perc, c(dist_align_raw, dist_align_clipped)) %>%
    gather(area, ratio, c(area_ratio_raw, area_ratio_clipped)) %>%
    filter(!(str_detect(dist, 'raw') & !(str_detect(area, 'raw')))) %>%
    filter(!(str_detect(dist, 'clip') & !(str_detect(area, 'clip')))) %>%
    mutate(method = ifelse(str_detect(dist, 'clip'), '200m clip', 'all depth'))
  
  clipped_quads <- ggplot(iucn_coral_fixed %>%
                            filter(!iucn_sid %in% (iucn_coral_fixed %>% 
                                                     filter(perc > 100) %>% 
                                                     .$iucn_sid)),
                          aes(x = ratio, y = perc, group = iucn_sid)) +
    ggtheme_plot + 
    geom_line(color = 'grey50', size = .5, alpha = .3) +
    geom_point(aes(color = method), size = 2, alpha = .8) +
    labs(x = 'Area ratio (%)',
         y = 'Distribtion (%)',
         title = 'Coral species alignment, IUCN full range vs depth-clipped')
  
  return(clipped_quads)
}