#server_fxns.R

dir_N <- c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
           'Darwin'  = '/Volumes/data_edit',
           'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

dir_anx      <- file.path(dir_N, 'git-annex/globalprep/SpeciesDiversity')


cat('Reading in IUCN species info...\n')
#iucn_spp_cells <- read_csv(file.path(dir_anx, 'explore_am_v_iucn/iucn_spp_cells_ltd.csv'), col_types = 'idiii', progress = TRUE)
iucn_spp_cells <- read_csv(file.path(dir_anx, 'explore_am_v_iucn/iucn_spp_cells.csv'), col_types = 'idiii', progress = TRUE)

cat('Reading in Aquamaps species info...\n')
#am_spp_cells   <- read_csv(file.path(dir_anx, 'explore_am_v_iucn/am_spp_cells_ltd.csv'), progress = TRUE)
am_spp_cells   <- read_csv(file.path(dir_anx, 'explore_am_v_iucn/am_spp_cells.csv'), progress = TRUE)




#data for raster (used to plot species)
cat('Reading in raster info...\n')
loiczid_raster_file  <- 'data/loiczid_raster.grd'
loiczid_raster       <- raster(loiczid_raster_file)
names(loiczid_raster) <- 'loiczid'

cat('It\'s a go!\n')

### Species Map Function ###
# This function takes a single species scientific name as input, then grabs all 
# occurrence cells and associated Aquamaps probability and/or IUCN proportional area
# per cell
get_spp_map <- function(species){
  spp_id <- spp_list %>%
    filter(sciname == species & is.na(parent_sid)) %>%
    dplyr::select(am_sid, iucn_sid, sciname) %>%
    unique()
  iucn_spp_map <- iucn_spp_cells %>%
    filter(iucn_sid == spp_id$iucn_sid) %>%
    dplyr::select(-parent_sid, -subpop_sid, loiczid = iucn_loiczid) %>%
    group_by(loiczid, iucn_sid) %>%         ### this line and the summarize are to collapse cells with overlapped polygons
    summarize(iucn_area = max(iucn_area))
  am_spp_map   <- am_spp_cells %>%
    filter(am_sid == spp_id$am_sid) %>%
    rename(loiczid = am_loiczid)
  spp_map <- full_join(iucn_spp_map, am_spp_map, by = 'loiczid') %>%
    as.data.frame()
  
  return(spp_map)
}

# species = 'Balaena mysticetus'
# species = 'Acropora willisae'