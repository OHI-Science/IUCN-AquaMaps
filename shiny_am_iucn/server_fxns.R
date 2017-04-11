### server_fxns.R
library(raster)
library(stringr)
# library(maps)
library(tmap)
# data(World)
library(RColorBrewer)
library(rgdal)

### read data for raster (used to plot species)
message('Reading in raster info...')
loiczid_raster <- raster('data/loiczid_raster.tif') %>%
  setNames('loiczid')

land <- readOGR(dsn = 'data', layer = "ne_110m_land") %>%
  crop(extent(-180, 180, -84, 85))

# create a blank ggplot theme
ggtheme_basic <- function(textsize = 10) {
  theme_bw() +
  theme_update(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(size = textsize),
          plot.title = element_text(size = textsize * 1.5))
}

### read in spp cell files
### this may be slowing down the initial display of the app... load elsewhere?
iucn_spp_cells <- read_csv('data/iucn_cells_app.csv', col_types = 'dd')
am_spp_cells   <- read_csv('data/am_cells_app.csv',   col_types = 'cd')

### read in quad plot files and establish means
quad_list <- read_csv('data/spp_list_quads_app.csv')

area_align_mean <- mean(quad_list$area_ratio, na.rm = TRUE)
dist_align_mean <- mean(quad_list$dist_align, na.rm = TRUE)

### read in coral species pre- and post-clipping
spp_coralmaps <- read_csv('data/coral_spp_areas_app.csv')

quad_gp_list <- read_csv('data/spp_gp_quads_app.csv') %>%
  mutate(expert = FALSE) %>%
  bind_rows(read_csv('data/spp_gp_quads_ex_app.csv') %>%
              mutate(expert = TRUE))
  
#################################.
##### Species Map Functions #####
#################################.
# These functions take a single species scientific name as input, then grab all 
# occurrence cells and associated Aquamaps probability and/or IUCN proportional area
# per cell

get_spp_map_df <- function(species) { ### species <- spp_list$sciname[1]
  message('in get_spp_map_df(), looking for species: ', species)

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
    mutate(am   = ifelse(is.na(am_sid), NA, 1),
           iucn = ifelse(is.na(iucn_sid), NA, 2)) %>%
    dplyr::select(loiczid, am, iucn) %>%
    mutate(both = ifelse(!is.na(iucn), ### IUCN presence, so...
                              ifelse(is.na(am), 2, 3), ### if no AM, assign 2; if AM, assign 3. 
                              am))          ### no IUCN, so assign 1 for AM and NA for none
  
  return(spp_map_df)
}

get_rast <- function(spp_map_df, type) {
  message('in get_rast()')
  message('... rasterizing to type = ', type)

  force_legend <- data.frame(loiczid = c(1, 2, 3))
  force_legend[type] <- c(1, 2, 3)
      ### annoying but adding back in one of each value, to force the legend

  spp_map_type <- spp_map_df %>%
    bind_rows(force_legend)
  rast_obj <- raster::subs(loiczid_raster, spp_map_type, 
                     by    = 'loiczid', 
                     which = type, 
                     subsWithNA = TRUE) %>%
    setNames('presence')
  
  return(rast_obj)
}

assemble_map_tmap <- function(map_rast, spp) {
  message('in assemble_map_tmap()')
  map_obj <- tm_shape(land, is.master = TRUE) +
      tm_polygons() + 
    tm_shape(map_rast) +
      tm_raster(palette = c("#FFAEB9", "#41B6C4", "#0C2C84"),
                style   = 'cat',
                breaks  = c(1, 2, 3),
                labels  = c("Aquamaps",   "IUCN",    "Both"),
                auto.palette.mapping = FALSE,
                colorNA = NULL,
                showNA  = TRUE,
                title = spp,
                alpha = 1) +
    tm_layout(legend.text.size = 1,
              legend.title.size = 1.2,
              # title.position = 'TOP', 
              legend.outside = FALSE, 
              legend.position = c('left', 'bottom'),
              legend.bg.color = 'white',
              legend.bg.alpha = .7,
              attr.outside = TRUE,
              outer.margins = 0, inner.margins = 0, asp = 2.1)
        
  return(map_obj)
}

#####################################################.
##### Functions for quadplots and barcharts tab #####
#####################################################.

create_barchart <- function(expt_rev) {
  
  if(expt_rev != 'all') {
    spp_gp_quadrants <- quad_gp_list %>%
      filter(expert)
  } else {
    spp_gp_quadrants <- quad_gp_list %>%
      filter(!expert)
  }
  
  quad_names <- data.frame('quad' = c('q4', 'q3', 'q2', 'q1'),
                           'quad_name' = factor(c('poorly aligned', 
                                                  'area-aligned', 
                                                  'dist-aligned',
                                                  'well-aligned'),
                                                ordered = TRUE))
  break_nums <- seq(0, 100, 20)
  
  spp_gp_quadrants <- spp_gp_quadrants %>% 
    mutate(quad = factor(quad, levels = c('q4', 'q3', 'q2', 'q1'))) %>%
    transform(spp_group_text = reorder(spp_group_text, pct_q1))
  
  barchart_spp_gp_quads <- ggplot(spp_gp_quadrants, 
                                  aes(x = spp_group_text, 
                                      y = pct_quad,
                                      fill = quad, 
                                      weight = pct_quad)) +
    ggtheme_basic(textsize = 12) +
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
              size = 3,
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
      filter(reviewed)
  }
  
  ### define windows for labels
  q1 <- c('x1' = 84, 'x2' = 98, 'y1' = 92.5, 'y2' = 97.5)
  q2 <- c('x1' =  2, 'x2' = 22, 'y1' = 92.5, 'y2' = 97.5)
  q3 <- c('x1' = 84, 'x2' = 98, 'y1' =  2.5, 'y2' = 7.5)
  q4 <- c('x1' =  2, 'x2' = 18, 'y1' =  2.5, 'y2' = 7.5)
  
  quad_list_labs <- quad_list_tmp %>%
    rename(x = area_ratio, y = dist_align) %>%
    mutate(fade = FALSE,
           fade = ifelse((x > q1[1] & x < q1[2] & y > q1[3] & y < q1[4]), TRUE, fade),
           fade = ifelse((x > q2[1] & x < q2[2] & y > q2[3] & y < q2[4]), TRUE, fade),
           fade = ifelse((x > q3[1] & x < q3[2] & y > q3[3] & y < q3[4]), TRUE, fade),
           fade = ifelse((x > q4[1] & x < q4[2] & y > q4[3] & y < q4[4]), TRUE, fade)) %>%
    rename(area_ratio = x, dist_align = y)
    
  
  
  scatter_quadplot <- ggplot(quad_list_labs,
                             aes(x = area_ratio, 
                                 y = dist_align,
                                 key = sciname,
                                 key2 = reviewed)) +
    ggtheme_basic(textsize = 12) +
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
    geom_point(data = quad_list_labs %>% filter(!fade),
               color = '#4d4dac', alpha = .6) + 
    geom_point(data = quad_list_labs %>% filter(fade),
               color = '#4d4dac', alpha = .15)
  
  ### Manage scales for color and size 
  scatter_quadplot <- scatter_quadplot +
    scale_x_continuous(expand = c(0, 0), 
                       limits = c(-1, 101),
                       breaks = c(seq(0, 100, 25)),
                       labels = c('0%', '25%', '50%', '75%', '100%')) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(-1, 101),
                       breaks = c(seq(0, 100, 25)),
                       labels = c('0%', '25%', '50%', '75%', '100%'))
  
  ### here are quadrant and mean labels:
  scatter_quadplot <- scatter_quadplot +
    annotate("text", x = 91, y = 95, hjust = 1, vjust = .5, size = 3, color = 'grey20', 
             fontface = 'bold', label = "Well-aligned") + 
    annotate("text", x =  12, y = 95, hjust = 0, vjust = .5, size = 3, color = 'grey20',
             fontface = 'bold', label = "Distribution-aligned") + 
    annotate("text", x = 91, y =  5, hjust = 1, vjust = .5, size = 3, color = 'grey20',
             fontface = 'bold', label = "Area-aligned") + 
    annotate("text", x =  10, y =  5, hjust = 0, vjust = .5, size = 3, color = 'grey20',
             fontface = 'bold', label = "Poorly aligned") +
  
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
    labs(x = 'Area ratio', 
         y = 'Distribution alignment')
  
  return(scatter_quadplot)
}


create_miniquad <- function(spp_sel) {

  scatter_miniquad <- ggplot(quad_list %>% 
                               filter(sciname == spp_sel),
                             aes(x = area_ratio, 
                                 y = dist_align)) +
    ggtheme_basic(textsize = 8) +
    theme(panel.grid.major = element_line(color = 'grey80'),
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
    geom_point(data = quad_list, 
               aes(x = area_ratio, y = dist_align),
               # color = '#4d4dac', alpha = .2) +
               color = 'grey50', alpha = .1) +
    geom_point(color = 'red3', size = 3, alpha = .8) +
    labs(x = 'Area ratio', 
         y = 'Dist. align') +
    
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE)
  
  return(scatter_miniquad)
}

########################################.
##### functions for coral maps tab #####
########################################.

create_coralquad <- function(coral_spp) {
  ### basically a mini-quad showing the before and after of the coral species
  # coral_spp <- spp_coralmaps$sciname[1]
  
  spp_coralmap <- spp_coralmaps %>%
    filter(sciname == coral_spp & method == 'all depth') %>%
    dplyr::select(sciname, area_ratio, dist_align) %>%
    left_join(spp_coralmaps %>%
                filter(sciname == coral_spp & method != 'all depth') %>%
                dplyr::select(sciname, area_ratio_clip = area_ratio, dist_align_clip = dist_align),
              by = 'sciname')
                
  coral_quad <- ggplot(spp_coralmap,
                          aes(x = area_ratio, y = dist_align)) +
    ggtheme_basic(textsize = 8) +
    theme(panel.grid.major = element_line(color = 'grey80'),
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
    geom_point(data = quad_list, 
               aes(x = area_ratio, y = dist_align),
               # color = '#4d4dac', alpha = .2) +
               color = 'grey50', alpha = .1) +
    geom_point(data = spp_coralmaps %>%
                 filter(method == 'all depth'), 
               aes(x = area_ratio, y = dist_align, group = iucn_sid),
               color = 'grey60', alpha = .1) +
    ### plot start point, then end point, then segment
    geom_segment(aes(xend = area_ratio_clip, yend = dist_align_clip),
                 color = 'grey30', size = .6,
                 arrow = arrow(angle = 15, type = 'closed', length = unit(.16, 'inches'))) +
    geom_point(color = 'grey40', size = 3, show.legend = FALSE) +
    geom_point(aes(x = area_ratio_clip, y = dist_align_clip), 
               color = 'red3', size = 3, show.legend = FALSE) +
    labs(x = 'Area ratio', 
         y = 'Dist. align') +
    
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE)
  
  return(coral_quad)
}
