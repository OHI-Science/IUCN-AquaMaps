#load data for species map function, function, and plotting a species

#JAfflerbach
#9.11.2014

## SETUP ##

#libraries

library(data.table)
library(readr)
library(raster)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(maps)


#increase R memory size to deal with reading in the large files

memory.limit(size=200000000)


dir_N <- c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
           'Darwin'  = '/Volumes/data_edit',
           'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

dir_anx      <- file.path(dir_N, 'git-annex/globalprep/SpeciesDiversity')
dir_aquamaps <- file.path(dir_anx, 'raw/aquamaps_2014')
dir_iucn     <- file.path(dir_anx, 'iucn_intersections')
dir_cells    <- file.path(dir_N, 'git-annex/Global/NCEAS-SpatialFishCatch_v2014/raw/ohi_spp/data')

#read in data
#fread comes from data.table package and is much more efficient at reading in the larger .csv files

spp_list <- read.csv('spp_am_v_iucn.csv', stringsAsFactors = FALSE)
### list of species with both IUCN and Aquamaps spatial data
spp_list_ltd <- spp_list %>% filter(!is.na(parent_sid) | !is.na(subpop_sid))
### truncated list

# am_spp   <- fread(input = file.path(dir_aquamaps, 'tables/ohi_speciesoccursum.csv'), header = TRUE) %>%
#               mutate(sciname = paste(Genus, Species, sep = ' '))


### get cell ID lookup table
# cells    <- read.csv(file.path(dir_cells, 'cells.csv')) %>%
# cells <- cells %>%
#   dplyr::select(cid, csq = csquarecod, loiczid, faoaream)
# 
# cells2 <- read.csv(file.path(dir_anx, 'raw/aquamaps_2014/tables/hcaf.csv'))
# cells2 <- cells2 %>% 
#   dplyr::select(csq = CsquareCode, loiczid = LOICZID, faoaream = FAOAreaM)
# write_csv(cells,  file.path(dir_anx, 'explore_am_v_iucn/cells.csv'))
# write_csv(cells2, file.path(dir_anx, 'explore_am_v_iucn/cells2.csv'))
cells <-  read_csv(file.path(dir_anx, 'explore_am_v_iucn/cells.csv'))
cells2 <- read_csv(file.path(dir_anx, 'explore_am_v_iucn/cells2.csv'))
# cells2 may be more recent, with more cells listed? not positive



### Get Aquamaps species-to-cell lookup table
# am_spp_cells <- fread(input = file.path(dir_aquamaps,'tables/ohi_hcaf_species_native.csv'), header = TRUE,
#                    colClasses = c('NULL', 'character', NA, 'numeric', 'NULL', 'NULL')) #1 min 49 sec
# am_spp_cells <- read_csv(file.path(dir_aquamaps,'tables/ohi_hcaf_species_native.csv'), col_types = '_ccd__')
# am_spp_cells <- am_spp_cells %>%
#   rename(am_sid = SpeciesID, am_csq = CsquareCode, am_prob = probability) %>%
#   filter(am_sid %in% spp_list$am_sid) %>%
#   left_join(cells2 %>% select(-faoaream), 
#             by = c('am_csq' = 'csq')) %>%
#   rename(am_loiczid = loiczid) %>%
#   select(-am_csq)
# am_spp_cells_ltd <- am_spp_cells %>% 
#   filter(am_sid %in% spp_list_ltd$am_sid)
# write_csv(am_spp_cells,     file.path(dir_anx, 'explore_am_v_iucn/am_spp_cells.csv'))
# write_csv(am_spp_cells_ltd, file.path(dir_anx, 'explore_am_v_iucn/am_spp_cells_ltd.csv'))

am_spp_cells     <-  read_csv(file.path(dir_anx, 'explore_am_v_iucn/am_spp_cells.csv'))
am_spp_cells_ltd <-  read_csv(file.path(dir_anx, 'explore_am_v_iucn/am_spp_cells_ltd.csv'))


### Get IUCN species-to-cell lookup table
get_iucn_spp_list <- function(spp_list) {
  ### Function to create a lookup of IUCN species-to-cell for species in a list
  ### IUCN intersections are in git-annex/globalprep/SpeciesDiversity/iucn_intersections/, in .csv files by species group
  ### | sciname | id_no | LOICZID | prop_area
  groups <- spp_list$spp_group %>% 
    unique()
  
  iucn_spp <- data.frame()
  for (gp in groups) { # gp = 'GROUPERS'
    intsx_fn <- file.path(dir_iucn, paste(gp, '.csv', sep = ''))
    
    if(!file.exists(intsx_fn))
      cat(sprintf('File %s doesn\'t exist...\n', intsx_fn))
    else {
      cat(sprintf('Reading %s...\n', intsx_fn))
      
      spp_gp <- read.csv(intsx_fn, stringsAsFactors = FALSE)  %>%
        select(sciname, iucn_loiczid = LOICZID, iucn_area = prop_area)
      
      spp_tmp <- spp_gp %>%
        filter(sciname %in% spp_list$sciname)
      iucn_spp <- bind_rows(iucn_spp, spp_tmp)
    }
  }
  iucn_spp <- iucn_spp %>%
    left_join(spp_list %>%
                select(iucn_sid, sciname, parent_sid, subpop_sid),
              by = 'sciname')
  
  return(iucn_spp)
}
iucn_spp_cells <- get_iucn_spp_list(spp_list)
iucn_spp_cells <- iucn_spp_cells %>%
  select(-sciname) # to keep it a smaller file
write_csv(iucn_spp_cells, file.path(dir_anx, 'explore_am_v_iucn/iucn_spp_cells.csv'))
iucn_spp <- read_csv(file.path(dir_anx, 'explore_am_v_iucn/iucn_spp_cells.csv'))


### Species Map Function ###
# This function takes a single species scientific name as input, then grabs all occurrence cells and associated probability per cell
sp_map_fun <- function(species){
  
  sp_id  <- filter(spp, scientific == species)$SPECIESID
  sp_map <- filter(spp_cells, species_id == sp_id) %>%
    merge(cells, by = 'csquare_code')
  
  return(sp_map)
}


### Plotting a single species ###

#data for raster (used to plot species)
r               <- raster(file.path(dir_cells, 'cells.tif'))
names(r)        <- 'cid'

#set species
sp <- 'Orcinus orca'

#get species occurrence cells
sp_cells <- sp_map_fun(sp)

#substitute values of raster cid with sp_cells' probability
r_sp <- subs(r, sp_cells[ , c('cid', 'probability')], by = 'cid', which = 'probability', subsWithNA = TRUE)

#plot
cols <- rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme
plot(r_sp, col=cols,main='Killer Whale')
map('world',col='gray95',fill=T,border='gray80',add=T)

