### server_fxns.R

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
### this may be slowing down the initial display of the app... load elsewhere?
iucn_spp_cells <- read_csv('data/iucn_cells.csv', col_types = 'dd')
am_spp_cells   <- read_csv('data/am_cells.csv')

### read in quad plot files and establish means
quad_list <- read_csv('data/spp_list_quads.csv') %>%
  rename(dist_align = sm_perc)

area_align_mean <- mean(quad_list$area_ratio, na.rm = TRUE)
dist_align_mean <- mean(quad_list$dist_align, na.rm = TRUE)

spp_coralmaps <- read_csv('data/coral_spp_areas.csv') %>%
  rename(dist_align_raw = sm_perc_raw, dist_align_clipped = sm_perc_clipped) %>%
  gather(dist, perc, c(dist_align_raw, dist_align_clipped)) %>%
  gather(area, ratio, c(area_ratio_raw, area_ratio_clipped)) %>%
  filter(!(str_detect(dist, 'raw') & !(str_detect(area, 'raw')))) %>%
  filter(!(str_detect(dist, 'clip') & !(str_detect(area, 'clip')))) %>%
  mutate(method = ifelse(str_detect(dist, 'clip'), '200m clip', 'all depth'))

quad_gp_list <- read_csv('data/spp_gp_quads.csv') %>%
  mutate(expert = FALSE) %>%
  bind_rows(read_csv('data/spp_gp_quads_ex.csv') %>%
              mutate(expert = TRUE))
  
### generic theme for all plots
theme_set(theme_bw())
ggtheme_plot <- theme_update(axis.ticks = element_blank(),
  text = element_text(family = 'Helvetica', color = 'gray30', size = 9),
  plot.title = element_text(size = rel(1.25), hjust = 0, face = 'bold'),
  panel.background = element_blank(),
  legend.position = 'right',
  panel.border     = element_blank(),
  panel.grid.minor = element_blank(), 
  # panel.grid.major = element_line(colour = 'grey90', size = .25),
  panel.grid.major = element_blank(),
  legend.key = element_rect(colour = NA, fill = NA),
  axis.line = element_blank()) # element_line(colour = "grey30", size = .5))



### Species Map Function ###
# This function takes a single species scientific name as input, then grabs all 
# occurrence cells and associated Aquamaps probability and/or IUCN proportional area
# per cell
get_spp_map_df <- function(species, am_cutoff = 0) { ### species <- spp_list$sciname[1]
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
    tm_layout(basemaps = "Esri.WorldTopoMap", legend.outside = TRUE, attr.outside = TRUE)
        
  
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

create_barchart <- function(expt_rev) {
  
  if(expt_rev != 'all') {
    spp_gp_quadrants <- quad_gp_list %>%
      filter(expert)
  } else {
    spp_gp_quadrants <- quad_gp_list %>%
      filter(!expert)
  }
  
  spp_gp_quadrants <- spp_gp_quadrants %>% 
    mutate(quad = factor(quad, levels = c('q4', 'q3', 'q2', 'q1'))) %>%
    transform(spp_group_text = reorder(spp_group_text, pct_q1))
  
  barchart_spp_gp_quads <- ggplot(spp_gp_quadrants, 
                                  aes(x = spp_group_text, 
                                      y = pct_quad,
                                      fill = quad, 
                                      weight = pct_quad)) +
    theme(panel.grid.major.x = element_blank()) +
    geom_bar(stat = 'identity', alpha = 1) +
    scale_fill_manual(values = c('q1' = '#4dac26',
                                 'q2' = '#b8e186', 
                                 'q3' = '#f1b6da',
                                 'q4' = '#d01c8b'),
                      labels = quad_names$quad_name,
                      guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(expand = c(0, 0), 
                       limits = c(0, 1.1),
                       breaks = break_nums/100,
                       labels = sprintf('%s%%', break_nums)) + 
    ### add grid lines; horizontal but then get flipped
    geom_hline(yintercept = break_nums/100, size = 0.25, color = 'white', alpha = .5) +
    ### add text
    geom_text(aes(label = sprintf('n = %s', n_spp), y = 1.01), hjust = 0, 
              size = 2.5, 
              color = 'grey30') +
    coord_flip() +
    labs(x = 'Taxonomic Group', 
         y = 'Percent of species by quadrant', 
         fill = 'Alignment')
  
  return(barchart_spp_gp_quads)
  
}



create_quadplot <- function(taxa_sel, expt_rev) {
  ### mongo plot time
  
  if(taxa_sel == 'all') {
    quad_list_tmp <- quad_list
  } else {
    quad_list_tmp <- quad_list %>%
      filter(spp_group_text == taxa_sel)
  }
  
  if(expt_rev != 'all') {
    quad_list_tmp <- quad_list_tmp %>%
      filter(!is.na(reviewed))
  }
  
  scatter_quadplot <- ggplot(quad_list_tmp,
                             aes(x = area_ratio, 
                                 y = dist_align,
                                 key = sciname)) +
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
         y = bquote('Distribution alignment (%)'))
  
  return(scatter_quadplot)
}


create_miniquad <- function(spp_sel) {

  ### mongo plot time
  scatter_miniquad <- ggplot(quad_list %>% 
                               filter(sciname == spp_sel),
                             aes(x = area_ratio, 
                                 y = dist_align)) +
    ggtheme_plot + 
    theme(panel.grid.major = element_blank(),
          axis.title = element_blank(),
          axis.text  = element_blank()) +
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
    geom_point(color = 'red3', size = 2, alpha = .8)
  
  return(scatter_miniquad)
}


create_coralplot <- function() {
  clipped_quads <- ggplot(spp_coralmaps %>%
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