### data_explore_fxns.R

### generic theme for all plots
ggtheme_basic <- theme(axis.ticks = element_blank(),
        text = element_text(family = 'Helvetica', color = 'gray30', size = 8),
        plot.title = element_text(size = rel(1.25), hjust = 0, face = 'bold'),
        legend.position = 'right')

ggtheme_plot <- ggtheme_basic + 
  theme(panel.border = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(colour = 'grey90'),
        panel.background = element_blank(),
        axis.line = element_line(colour = "grey30"))

  

# ggtheme_map <- theme(axis.ticks = element_blank(), axis.text = element_blank(),
#                     text = element_text(family = 'Helvetica', color = 'gray30', size = 8),
#                     plot.title = element_text(size = rel(1.5), hjust = 0, face = 'bold'),
#                     legend.position = 'right')

### theme for species range maps - based on generic plot theme
ggtheme_map <- ggtheme_basic +
  theme(axis.text = element_blank())

### Function to create a species list from scratch... use when new data is available
create_spp_list <- function(spp_list_base, data_file) {
  spp_list_build <- spp_list_base %>%
    mutate(total_area = NA,
           am_area    = NA,
           iucn_area  = NA,
           perc       = NA,
           am_perc    = NA,
           iucn_perc  = NA,
           sm_perc    = NA, #filled with % of smaller range that is within the larger range
           sm_range   = NA) #what range is smaller
  
  
  #there's likely a better way to do this than a for loop (lapply or sapply?)
  for (i in 1:nrow(spp_list_build)){ # i = 1
    spp     <- spp_list_build[i, ]$sciname #species name
    spp_map <- get_spp_map(spp) #get species map
    
    ###get just the aquamaps cells
    am_spp_map <- spp_map %>%
      filter(!is.na(am_prob))%>%
      dplyr::select(loiczid, am_sid, am_prob) %>% 
      ### add in am_prob when we want to look at probability
      unique()
    
    if(any(duplicated(am_spp_map$loiczid))){
      am_spp_map <- am_spp_map %>%
        dplyr::select(loiczid, am_sid) %>% 
        unique()
    }
    
    ### rasterize aquamaps map
    r_am_spp <- subs(loiczid_raster, 
                     am_spp_map[ , c('loiczid', 'am_sid')], 
                     by = 'loiczid', 
                     which = 'am_sid', 
                     subsWithNA = TRUE)
    
    spp_list_build[i, ]$am_area <- area(r_am_spp, na.rm = TRUE) %>%
      cellStats(., stat='sum')
    
    
    ###  get the iucn cells
    iucn_map <- spp_map %>%
      mutate(iucn_pres = iucn_area > 0) %>%
      dplyr::select(loiczid, iucn_area, am_sid, am_prob, iucn_pres) %>%
      unique()
    
    #TODO: incorporate the iucn_area (area of cell that species overlaps with...I think thats what it is)
    
    ###this is incorporated because there are some duplications for loiczids with IUCN cells. I'm guessing this is because of the parent/subpopulations?
    
    if(TRUE %in% duplicated(iucn_map$loiczid)) next()
    
    ### rasterize IUCN map
    r_iucn_spp <- subs(loiczid_raster, 
                       iucn_map[ , c('loiczid', 'iucn_pres')], 
                       by = 'loiczid', 
                       which = 'iucn_pres', 
                       subsWithNA = TRUE)
    
    spp_list_build[i, ]$iucn_area <- area(r_iucn_spp, na.rm = TRUE) %>%
      cellStats(., stat = 'sum')
    
    
    ### Get total area (IUCN and AM combined)
    allCells <- spp_map %>%
      dplyr::select(loiczid) %>%
      mutate(value=1) %>%
      unique()
    
    
    fullRange <- subs(loiczid_raster, 
                      allCells[ , c('loiczid', 'value')], 
                      by = 'loiczid', 
                      which = 'value', 
                      subsWithNA = TRUE)
    
    spp_list_build[i, ]$total_area <- area(fullRange, na.rm = TRUE) %>%
      cellStats(., stat = 'sum')
    
    ### get percent overlap of map
    
    #iucn map cells
    cells_iucn <- iucn_map %>%
      filter(iucn_pres == TRUE) %>%
      .$loiczid
    cells_am <- am_spp_map %>%
      filter(!is.na(am_sid)) %>%
      .$loiczid
    
    cells_total   <- unique(spp_map$loiczid)
    cells_overlap <- intersect(cells_iucn, cells_am)
    
    #percent overlap
    perc <- (length(cells_overlap) / length(cells_total)) * 100
    spp_list_build[i, ]$perc <- perc
    
    am_only   <- setdiff(cells_am, cells_iucn)
    iucn_only <- setdiff(cells_iucn, cells_am)
    
    perc_am   <- (length(am_only) / length(cells_total)) * 100
    perc_iucn <- (length(iucn_only) / length(cells_total)) * 100
    
    spp_list_build[i, ]$am_perc   <- perc_am
    spp_list_build[i, ]$iucn_perc <- perc_iucn
    
    spp_list_build[i, ]$sm_range <- ifelse(length(cells_am)<length(cells_iucn), 'AM', 'IUCN') 
    ### what dataset has the smaller range
    spp_list_build[i, ]$sm_perc <- length(cells_overlap)/min(length(cells_am), length(cells_iucn)) * 100 
    ### this is the total percent of smaller range encompassed in the larger range
    
    cat(sprintf('%s: For species %s, %.2f%% of both datasets overlap, %.2f%% is just AquaMaps and %.2f%% is just IUCN\n', 
                i, spp, perc, perc_am, perc_iucn))
  }
  
  
  # for Conus arenatus, the IUCN map gives 4 duplicate cells, but each of the entries has a different iucn_area...which doesn't make sense
  
  #row 985 for Acanthurus leucopareius - AM is giving duplicate loiczids but with different am_prob...which is so strange. Looked
  # at the raw data and it seems to be in the raw data... (JA on 8/31/15)
  
  #remove non IUCN species and log total area (for use later on)
  spp_list_build <- spp_list_build %>%
    filter(spp_group != 'hagfishes' & spp_group != 'non-homalopsids') %>%
    mutate(log_total_area = log(total_area))
  
  spp_list_build <- spp_list_build %>%
    mutate(area_ratio = am_area / iucn_area,
           area_ratio = ifelse(area_ratio > 1, 1 / area_ratio, area_ratio),
           area_ratio = area_ratio * 100)
  
  spp_list_build <- spp_list_build %>%
    mutate(lg_area = ifelse(am_area > iucn_area, am_area, iucn_area),
           sm_area = ifelse(am_area < iucn_area, am_area, iucn_area),
           lg_area_pct = 100 * lg_area / max(total_area, na.rm = TRUE)) %>% ### use largest data set as approx for total ocean range
    select(-lg_area, -sm_area)
  
  spp_list_build <- spp_list_build %>%
    mutate(total_area  = round(total_area, 1),
           am_area     = round(am_area, 1),
           iucn_area   = round(iucn_area, 1),
           perc        = round(perc, 3),
           am_perc     = round(am_perc, 3),
           iucn_perc   = round(iucn_perc, 3),
           sm_perc     = round(sm_perc, 3),
           log_total_area = round(log_total_area, 3),
           area_ratio  = round(area_ratio, 3),
           lg_area_pct = round(lg_area_pct, 3)
    ) %>%
    mutate(reviewed = as.integer(reviewed)) ### 'null' values force this to be character... this converts '1' to 1, and 'null' to NA
  
  write.csv(spp_list_build, file = data_file, row.names = FALSE)
}

### Species Map Function ###
# This function takes a single species scientific name as input, then grabs all 
# occurrence cells and associated Aquamaps probability and/or IUCN proportional area
# per cell
get_spp_map <- function(species){ # species = 'Acanthopagrus latus' # species = 'Abudefduf concolor'
  spp_id <- spp_list %>%
    filter(sciname == species & is.na(parent_sid)) %>%
    dplyr::select(am_sid, iucn_sid, sciname) %>%
    unique()

  am_spp_map   <- am_spp_cells %>%
    filter(am_sid == spp_id$am_sid) %>%
    rename(am_prob = prob)
  
  iucn_spp_map <- iucn_spp_cells %>%
    filter(sciname == species)
  if(nrow(iucn_spp_map) == 0) {
    message(sprintf('Species %s has zero IUCN cells', species))
    spp_map <- am_spp_map %>%
      as.data.frame() %>%
      mutate(iucn_area = NA)
  } else {
    iucn_spp_map <- iucn_spp_map %>%
      group_by(loiczid) %>%        
      summarize(iucn_area = max(prop_area))
    ### The group_by() and summarize() are to collapse cells with overlapped polygons, fixing the duplicate 'by' issue
    spp_map <- full_join(iucn_spp_map, am_spp_map, by = 'loiczid') %>%
      as.data.frame()
  }
  
  return(spp_map)
}

### Plot Species Map function
### uses ggplot to create a formatted map of species ranges, including
### Aquamaps, IUCN, and overlapped ranges; returns the plot object.
plot_rangemap <- function(spp) {
  map <- get_spp_map(spp)
  
  am_map <- map %>%
    filter(!is.na(am_prob))
  
  r_am_spp  <-  subs(loiczid_raster, 
                     am_map[ , c('loiczid', 'am_prob')], 
                     by = 'loiczid', 
                     which = 'am_prob', 
                     subsWithNA = TRUE)
  r_am_spp[!is.na(r_am_spp)] <- 1
  
  iucn_map <- map %>%
    mutate(iucn_pres = iucn_area > 0)
  r_iucn_spp <- subs(loiczid_raster, 
                     iucn_map[ , c('loiczid', 'iucn_pres')], 
                     by = 'loiczid', 
                     which = 'iucn_pres', 
                     subsWithNA = TRUE)
  
  spp_pts <- as.data.frame(rasterToPoints(r_iucn_spp)) %>%
    full_join(as.data.frame(rasterToPoints(r_am_spp)),
              by = c('x', 'y')) %>%
    mutate(presence = ifelse(am_prob & is.na(iucn_pres), 'am',
                             ifelse(iucn_pres & is.na(am_prob), 'iucn',
                                    ifelse(iucn_pres & am_prob, 'both', NA))))
  
  spp_plot <- ggplot(spp_pts, aes(x = x, y = y)) +
    ggtheme_map + 
    geom_raster(aes(fill = presence), alpha = .5) +
    scale_fill_manual(values = c('iucn' = 'blue', 'am' = 'red', 'both' = 'mediumorchid')) +
    borders('world', color='gray40', fill='gray45', size = .1) +  # create a layer of borders
    scale_x_continuous(breaks = seq(-180, 180, by = 30), expand = c(0, 2)) +
    scale_y_continuous(breaks = seq( -90,  90, by = 30), expand = c(0, 2)) +
    labs(title = spp, x = NULL, y = NULL) 
}

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