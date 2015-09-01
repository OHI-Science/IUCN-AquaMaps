# Exploring aquamaps/iucn comparisons
library(ggplot2)

server_fxns.R

dir_N <- c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
           'Darwin'  = '/Volumes/data_edit',
           'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

dir_anx      <- file.path(dir_N, 'git-annex/globalprep/SPP_ICO')


iucn_spp_cells <- read_csv(file.path(dir_anx, 'explore_am_v_iucn/iucn_spp_cells.csv'), col_types = 'idiii', progress = TRUE)

am_spp_cells   <- read_csv(file.path(dir_anx, 'explore_am_v_iucn/am_spp_cells.csv'), progress = TRUE)

#species list
spp_list <- read.csv('shiny/data/spp_am_v_iucn.csv', stringsAsFactors = FALSE)


#data for raster (used to plot species)

loiczid_raster_file  <- 'shiny/data/loiczid_raster.grd'
loiczid_raster       <- raster(loiczid_raster_file)
names(loiczid_raster) <- 'loiczid'


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


#---------------------------------------------------------------------------------------------------------------------

# Exploration

#calculate the area difference between all 

spp_list = spp_list%>%
              mutate(am_area = NA,
                     iucn_area=NA,
                     perc = NA)

for (i in 1:nrow(spp_list)){
  
  print(i)
  
  spp <- spp_list[i,]$sciname
  
  spp_map <- get_spp_map(spp)
  
  am_spp_map = spp_map%>%
                dplyr::select(loiczid,am_sid)%>% #add in am_prob when we want to look at probability
                  unique()
  
#   r_am_spp  <-  subs(loiczid_raster, 
#                      am_spp_map[ , c('loiczid', 'am_sid')], 
#                      by = 'loiczid', 
#                      which = 'am_sid', 
#                      subsWithNA = TRUE)
  
  #spp_list[i,]$am_area = area(r_am_spp,na.rm=T)%>%
              #cellStats(.,stat='sum')
  
  
  
  iucn_map <- spp_map %>%
    mutate(iucn_pres = iucn_area > 0)%>%
      dplyr::select(loiczid,iucn_area,am_sid,am_prob,iucn_pres)%>%
        unique()
  
  #TODO: incorporate the iucn_area (area of cell that species overlaps with...I think thats what it is)
  
  if(TRUE %in% duplicated(iucn_map$loiczid))next()
  
  #r_iucn_spp <- subs(loiczid_raster, 
                     #iucn_map[ , c('loiczid', 'iucn_pres')], 
                    # by = 'loiczid', 
                     #which = 'iucn_pres', 
                     #subsWithNA = TRUE)
  
  #spp_list[i,]$iucn_area = area(r_iucn_spp,na.rm=T)%>%
                #cellStats(.,stat='sum')
  
  # get percent overlap of map
  
  #iucn map cells
  cells_iucn<-iucn_map%>%
              filter(iucn_pres==TRUE)%>%
              .$loiczid
  cells_am <- am_spp_map%>%
                filter(!is.na(am_sid))%>%
                .$loiczid
  
  cells_total = unique(spp_map$loiczid)
  cells_overlap = intersect(cells_iucn,cells_am)
  
  #percent overlap
  perc = (length(cells_overlap)/length(cells_total))*100
  spp_list[i,]$perc = perc
  
  am_only = setdiff(cells_am,cells_iucn)
  iucn_only = setdiff(cells_iucn,cells_am)
  
  perc_am = (length(am_only)/length(cells_total))*100
  perc_iucn = (length(iucn_only)/length(cells_total))*100
  
cat(perc,perc_am,perc_iucn)
  
  }


# for Conus arenatus, the IUCN map gives 4 duplicate cells, but each of the entries has a different iucn_area...which doesn't make sense

#row 985 for Acanthurus leucopareius - AM is giving duplicate loiczids but with different am_prob...which is so strange. Looked
# at the raw data and it seems to be in the raw data... (JA on 8/31/15)

write.csv(spp_list,file='spp_list_w_area.csv')

#---------------------------------------------------------------------------------------------------------------------
# Calculate difference in area

df = spp_list%>%
      mutate(diff = iucn_area - am_area)

plot(df$iucn_area~df$am_area)
abline(0,1)

# log transform

df = df%>%
      mutate(am_log = log(am_area),
             iucn_log = log(iucn_area))

all_status <- ggplot(df,aes(iucn_log,am_log,color=category)) + 
  geom_point() + geom_abline() +
  labs(x='IUCN Area (log transformed)', y='AquaMaps Area (log transformed)',
        title= 'Comparing area of range maps\n (all area values are km2 log transformed)',fill='IUCN Category')


groups <- ggplot(df, aes(iucn_log, am_log,color=category)) + geom_point() +facet_wrap(~spp_group) + 
        geom_abline() + labs(x='IUCN Area (log transformed)', y='AquaMaps Area (log transformed)',
                             title='Comparing differences in area across species groups', fill='IUCN Category')

#---------------------------------------------------------------------------------------------------------------------

# look at % overlap


