### data_explore_fxns.R

library(ggplot2)

### generic theme for all plots
ggtheme_plot <- theme(axis.ticks = element_blank(),
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


### theme for species range maps - based on generic plot theme
ggtheme_map <- ggtheme_plot +
  theme_update(axis.text = element_blank(),
        axis.title = element_blank())



#################################################################=
### Species Map Function ###
# This function takes a dataframe of loiczid per species ID, and a target species ID 
### (as single or vector).  Return a df of cells ready to be rasterized.  
### If AquaMaps, returned df includes aquamaps probability; if IUCN, returned
### df includes proportional area.
get_spp_map <- function(sid, spp_cells) { 
  data_type <- ifelse('am_sid' %in% names(spp_cells), 'am', 'iucn')
  
  if(data_type == 'am') {
    ### if AquaMaps, process it thusly:
    spp_map <- spp_cells %>%
      filter(am_sid %in% sid) %>%
      rename(am_prob = prob)
    if(nrow(spp_map) == 0)
      message('no AquaMaps cells found')
  } else {
    ### if IUCN, process it thusly:
    spp_map <- spp_cells %>%
      filter(iucn_sid %in% sid)
    if(nrow(spp_map) == 0) {
      message('no IUCN cells found')
      spp_map <- spp_map %>%
        as.data.frame() %>%
        select(iucn_sid, loiczid) %>%
        mutate(area_iucn = NA)
    } else {
      spp_map <- spp_map %>%
        group_by(iucn_sid, loiczid) %>%        
        summarize(area_iucn = max(prop_area))
      ### The group_by() and summarize() are to collapse cells with 
      ### overlapped polygons, fixing the duplicate 'by' issue.
      ### Note: this drops presence, sciname, and subpop columns
    }
  }
  return(spp_map %>% unique() %>% as.data.frame())
}

#################################################################=
### Plot Species Map function
### uses ggplot to create a formatted map of species ranges, including
### Aquamaps, IUCN, and overlapped ranges; returns the plot object.
plot_rangemap <- function(spp) {
  map_iucn <- get_spp_map(spp$iucn_sid, iucn_spp_cells) %>%
    mutate(iucn_pres = ifelse(area_iucn > 0, 1, 0)) %>%
    select(-area_iucn, -iucn_sid) %>%
    unique()
  map_am   <- get_spp_map(spp$am_sid, am_spp_cells) %>%
    mutate(am_pres = ifelse(am_prob >= am_thresh, 1, 0)) %>%
    select(-am_sid, -am_prob) %>%
    unique()
  
  r_am_spp  <-  subs(loiczid_raster, 
                     map_am[ , c('loiczid', 'am_pres')], 
                     by = 'loiczid', 
                     which = 'am_pres', 
                     subsWithNA = TRUE)
  r_am_spp[!is.na(r_am_spp)] <- 1
  
  r_iucn_spp <- subs(loiczid_raster, 
                     map_iucn[ , c('loiczid', 'iucn_pres')], 
                     by = 'loiczid', 
                     which = 'iucn_pres', 
                     subsWithNA = TRUE)
  
  spp_pts <- as.data.frame(rasterToPoints(r_iucn_spp)) %>%
    full_join(as.data.frame(rasterToPoints(r_am_spp)),
              by = c('x', 'y')) %>%
    mutate(presence = ifelse(am_pres & is.na(iucn_pres), 'AquaMaps',
                             ifelse(iucn_pres & is.na(am_pres), 'IUCN',
                                    ifelse(iucn_pres & am_pres, 'Both', NA))))
  
  #library(colorspace)
  #hclvec <- heat_hcl(3); colvec <- c('AquaMaps' = hclvec[1], 'Both' = hclvec[2], 'IUCN' = hclvec[3])
  #colvec <- c('AquaMaps' = '#1b9e77', 'Both' = '#565080', 'IUCN' = '#d95f02')
  colvec <- c('AquaMaps' = '#76A94C', 'Both' = '#e18258', 'IUCN' = '#9168a9')
  
  spp_plot <- ggplot(spp_pts, aes(x = x, y = y)) +
    ggtheme_map + 
    geom_raster(aes(fill = presence), alpha = .8) +
    scale_fill_manual(values = colvec) +
    borders('world', color='gray30', fill='gray40', size = .1) +  # create a layer of borders
    scale_x_continuous(breaks = seq(-180, 180, by = 30)) +
    scale_y_continuous(breaks = seq( -90,  90, by = 30))
  
  # labs(title = sprintf('IUCN species ID: %s', spp$iucn_sid), x = NULL, y = NULL) 
}

#################################################################=
create_am_raster <- function(spp) {
  map <- get_spp_map(spp)
  
  am_map <- map %>%
    filter(!is.na(am_prob))
  
  r_am_spp  <-  subs(loiczid_raster, 
                     am_map[ , c('loiczid', 'am_prob')], 
                     by = 'loiczid', 
                     which = 'am_prob', 
                     subsWithNA = TRUE)
  
  writeRaster(r_am_spp, file.path(dir_git, 'rasters', paste(str_replace(spp, ' ', '_'), '.tif', sep = '')))
}

# spp <- 'Oculina varicosa'
# create_am_raster(spp)


#################################################################=
### Function to create a species list from scratch... use when new data is available
create_spp_list <- function(spp_list_base, data_file, am_spp_cells, 
                             iucn_spp_cells, loiczid_raster) {
  
  am_thresh <- 0 ### set the aquamaps presence threshold
  sid_vector <- unique(spp_list_base$iucn_sid)
  message(sprintf('There are a total of %s distinct species, by IUCN SID', 
                  length(sid_vector)))
  
  ### Here's the big function!
  library(parallel)
  ptm <- proc.time()
  spp_list_build <- mclapply(sid_vector, calc_areas, mc.cores = 16) %>%
    bind_rows()
  proc.time() - ptm
  
  spp_list_build <- spp_list_build %>%
    mutate(sm_range   = ifelse(area_am < area_iucn, 'AM', 'IUCN'),
           sm_perc    = ifelse(area_am < area_iucn, (area_overlap / area_am) * 100, (area_overlap / area_iucn) * 100),
           area_ratio = area_am / area_iucn,
           area_ratio = ifelse(area_ratio > 1, 1 / area_ratio, area_ratio),
           area_ratio = area_ratio * 100)
  
  spp_list_build <- spp_list_build %>%
    mutate(lg_area = ifelse(area_am > area_iucn, area_am, area_iucn),
           sm_area = ifelse(area_am < area_iucn, area_am, area_iucn),
           lg_area_pct = 100 * lg_area / max(area_total, na.rm = TRUE)) %>% 
    ### use largest data set as approx for total ocean range
    select(-lg_area, -sm_area)
  
  spp_list_build <- spp_list_build %>%
    mutate(area_total  = round(area_total, 1),
           area_am     = round(area_am, 1),
           area_iucn   = round(area_iucn, 1),
           sm_perc     = round(sm_perc, 3),
           area_ratio  = round(area_ratio, 3),
           lg_area_pct = round(lg_area_pct, 3)) %>%
    mutate(reviewed = as.integer(reviewed)) ### 'null' values force this to be character... this converts '1' to 1, and 'null' to NA
  
  print(head(spp_list_build))
  write_csv(spp_list_build, data_file)
  
}


calc_areas <- function(sid) {
  # sid <- sid_vector[1]
  
  spp_list_chunk <- spp_list_base %>%
    select(spp_group, iucn_sid, sciname, class, am_sid, reviewed) %>%
    filter(iucn_sid == sid)
  
  message(sprintf('Processing species iucn_sid: %s \n   am_sid(s):  %s \n   sciname(s): %s', 
                  sid, paste(spp_list_chunk$am_sid, collapse = ', '), paste(spp_list_chunk$sciname, collapse = ' ')))
  map_iucn <- get_spp_map(sid, iucn_spp_cells) %>%
    mutate(iucn_pres = ifelse(area_iucn > 0, 1, 0))
  map_am   <- get_spp_map(spp_list_chunk$am_sid, am_spp_cells) %>%
    mutate(am_pres = ifelse(am_prob >= am_thresh, 1, 0))
  
  ### rasterize aquamaps map
  if(any(duplicated(map_am$loiczid))) message('Duplicated cells in am spp map')
  map_am <- map_am %>%
    dplyr::select(loiczid, am_pres) %>% 
    unique()
  
  r_am_spp <- subs(x = loiczid_raster, y = map_am %>% select(loiczid, am_pres), 
                   by = 'loiczid', which = 'am_pres', 
                   subsWithNA = TRUE)
  
  ### rasterize IUCN map
  if(TRUE %in% duplicated(map_iucn$loiczid)) stop('Duplicated cells in IUCN map')
  r_iucn_spp <- subs(loiczid_raster, map_iucn %>% select(loiczid, iucn_pres), 
                     by = 'loiczid', which = 'iucn_pres', 
                     subsWithNA = TRUE)
  
  ### Start calculating areas etc.
  area_iucn <- area(r_iucn_spp, na.rm = TRUE) %>%
    cellStats(., stat = 'sum')
  area_am   <- area(r_am_spp, na.rm = TRUE) %>%
    cellStats(., stat = 'sum')
  spp_list_chunk <- spp_list_chunk %>%
    mutate(area_iucn = area_iucn,
           area_am   = area_am)
  
  ### Get total area (IUCN and AM combined)
  all_cells <- data.frame(loiczid = unique(c(map_iucn$loiczid, map_am$loiczid)),
                          spp_pres = 1)
  r_all_cells <- subs(loiczid_raster, all_cells, 
                      by = 'loiczid', which = 'spp_pres', 
                      subsWithNA = TRUE)
  area_total  <- area(r_all_cells, na.rm = TRUE) %>% cellStats(., stat = 'sum')
  spp_list_chunk <- spp_list_chunk %>%
    mutate(area_total = area_total)
  
  ### Get total overlap (IUCN and AM together). If area_iucn = 0, just assign 0
  if(area_iucn != 0) {
    overlap_cells <- unique(map_iucn$loiczid[map_iucn$loiczid %in% map_am$loiczid])
    if(length(overlap_cells) > 0) {
      overlap_cells <- data.frame(loiczid = overlap_cells,
                                  spp_pres = 1)
      r_overlap_cells <- subs(loiczid_raster, overlap_cells, 
                              by = 'loiczid', which = 'spp_pres', 
                              subsWithNA = TRUE)
      area_overlap <- area(r_overlap_cells, na.rm = TRUE) %>% cellStats(., stat = 'sum')
    } else area_overlap <- 0 ### if no overlapping cells, assign zero
  } else area_overlap <- 0 ### if no IUCN map cells, assign zero
  spp_list_chunk <- spp_list_chunk %>%
    mutate(area_overlap = area_overlap)
  
  message(sprintf('   Species iucn_sid %s: smaller range is %s. Smaller-in-larger is %.2f%%, A_sm:A_lg is %.2f%%.',
                  sid, ifelse(area_am < area_iucn, 'AquaMaps', 'IUCN'), 
                  ifelse(area_am < area_iucn, area_overlap/area_am*100, area_overlap/area_iucn*100),
                  ifelse(area_am < area_iucn, area_am/area_iucn*100, area_iucn/area_am*100)))
  
  return(spp_list_chunk)
}
