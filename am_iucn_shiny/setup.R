### shiny app setup: get proper files into place
library(readr)
library(dplyr)
library(tidyr)

dir_M <- c('Windows' = '//mazu.nceas.ucsb.edu/ohi',
           'Darwin'  = '/Volumes/ohi',
           'Linux'   = '/home/shares/ohi')[[ Sys.info()[['sysname']] ]]

dir_anx      <- file.path(dir_M, 'git-annex/globalprep/spp_ico')


### Set up the species cell files.  
### These will be 200 randomly sampled species, with loiczid tables
### for both IUCN and AM.  Also the species info file.
if(FALSE) {
  cat('Reading in IUCN species info...\n')
  iucn_spp_cells_all <- read_csv(file.path(dir_anx, 'v2016/int/iucn_cells_spp.csv'),
                             col_types = 'ciiddc', progress = TRUE)
  ### sciname | iucn_sid | presence | loiczid | prop_area | subpop
  
  cat('Reading in Aquamaps species info...\n')
  am_spp_cells_all   <- read_csv(file.path(dir_anx, 'v2016/int/am_cells_spp_prob0.csv'), progress = TRUE)
  
  spp_list <- read_csv(file.path(dir_anx, 'v2016/int/spp_all_cleaned.csv')) %>%
    filter(!is.na(spp_group) &!is.na(am_sid)) %>%
    filter(is.na(iucn_subpop)) %>%
    sample_n(200) %>%
    left_join(read_csv('../data/sciname_lookups/spp_groupnames.csv'),
              by = 'spp_group')
  
  iucn_spp_cells <- iucn_spp_cells_all %>%
    dplyr::select(iucn_sid, loiczid) %>%
    filter(iucn_sid %in% spp_list$iucn_sid)
  am_spp_cells <- am_spp_cells_all %>%
    filter(am_sid %in% spp_list$am_sid)
  
  write_csv(spp_list, 'data/spp_list.csv')
  write_csv(iucn_spp_cells, 'data/iucn_cells.csv')
  write_csv(am_spp_cells, 'data/am_cells.csv')
}

### copy quad list file and quad plot
