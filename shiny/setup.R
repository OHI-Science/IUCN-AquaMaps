### shiny app setup: get proper files into place

### if iucn/am cell file does not exist, it must be created from data on Mazu.
### - set up Mazu dir
### - pull data
### - filter
### - save locally for 

dir_M <- c('Windows' = '//mazu.nceas.ucsb.edu/ohi',
           'Darwin'  = '/Volumes/ohi',
           'Linux'   = '/home/shares/ohi')[[ Sys.info()[['sysname']] ]]

dir_anx      <- file.path(dir_M, 'git-annex/globalprep/spp_ico')

spp_cells_file <- 'data/spp_cells.csv'
if(!file.exists(spp_cells_file)) {
  
  
  cat('Reading in IUCN species info...\n')
  iucn_spp_cells <- read_csv(file.path(dir_anx, 'v2016/int/iucn_cells_spp.csv'),
                             col_types = 'ciiddc', progress = TRUE)
    ### sciname | iucn_sid | presence | loiczid | prop_area | subpop

  cat('Reading in Aquamaps species info...\n')
  am_spp_cells   <- read_csv(file.path(dir_anx, 'v2016/int/am_cells_spp_prob0.csv'), progress = TRUE)
  
  spp_list <- read_csv(file.path(dir_anx, 'v2016/int/spp_all_cleaned.csv')) %>%
    filter(!is.na(spp_group) &!is.na(am_sid))
  
  iucn_spp_cells1 <- iucn_spp_cells %>%
    filter(iucn_sid %in% spp_list$iucn_sid)
  am_spp_cells1 <- am_spp_cells %>%
    filter(am_sid %in% spp_list$am_sid)
  
  spp_cells <- spp_list %>%
    select(am_sid, iucn_sid, sciname) %>%
    left_join(iucn_spp_cells1 %>%
                rename(loiczid = iucn_loiczid),
              by = 'iucn_sid') %>%
    left_join(am_spp_cells1 %>%
                rename(am_loiczid = loiczid))
}