# global.R

#read in species list - on global.R because it shows up in both ui.R and server.R
cat('Reading in list of species with both Aquamaps and IUCN data...\n')
spp_list <- read.csv('data/spp_am_v_iucn.csv', stringsAsFactors = FALSE)
#set species list
spp_choices <- as.character(sort(unique(spp_list$sciname)))
