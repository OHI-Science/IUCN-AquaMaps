#server_fxns.R

### if iucn/am cell file does not exist, it must be created from data on Mazu.
### - set up Mazu dir
### - pull data
### - filter
### - save locally for 
dir_M <- c('Windows' = '//mazu.nceas.ucsb.edu/ohi',
           'Darwin'  = '/Volumes/ohi',
           'Linux'   = '/home/shares/ohi')[[ Sys.info()[['sysname']] ]]

dir_anx      <- file.path(dir_M, 'git-annex/globalprep/spp_ico')


cat('Reading in IUCN species info...\n')
iucn_spp_cells <- read_csv(file.path(dir_anx, 'explore_am_v_iucn/iucn_spp_cells.csv'), col_types = 'idiii', progress = TRUE)
#iucn_spp_cells <- read_csv('data/iucn_spp_cells.csv')


cat('Reading in Aquamaps species info...\n')
#am_spp_cells   <- read_csv(file.path(dir_anx, 'explore_am_v_iucn/am_spp_cells_ltd.csv'), progress = TRUE)
am_spp_cells   <- read_csv(file.path(dir_anx, 'explore_am_v_iucn/am_spp_cells.csv'), progress = TRUE)
#am_spp_cells <- read_csv('data/am_spp_cells.csv') 



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
    group_by(loiczid, iucn_sid) %>%        
    summarize(iucn_area = max(iucn_area))
      ### The group_by() and summarize() are to collapse cells with overlapped polygons, fixing the duplicate 'by' issue
  am_spp_map   <- am_spp_cells %>%
    filter(am_sid == spp_id$am_sid) %>%
    rename(loiczid = am_loiczid)
  spp_map <- full_join(iucn_spp_map, am_spp_map, by = 'loiczid') %>%
    as.data.frame()
  
  return(spp_map)
}

get_perc <- function(spp_map){

  # get percent overlap of map
  
  #iucn map cells
  cells_iucn<-spp_map%>%
    filter(iucn_area>0)%>%
    .$loiczid
  cells_am <- spp_map%>%
    filter(!is.na(am_prob))%>%
    .$loiczid
  
  cells_total = unique(spp_map$loiczid)
  cells_overlap = intersect(cells_iucn,cells_am)
  
  #percent overlap
  perc = (length(cells_overlap)/length(cells_total))*100
  
  am_only = setdiff(cells_am,cells_iucn)
  iucn_only = setdiff(cells_iucn,cells_am)
  
  perc_am = (length(am_only)/length(cells_total))*100
  perc_iucn = (length(iucn_only)/length(cells_total))*100
  
  return(c(perc,perc_am,perc_iucn))
  
}

