# global.R


library(readr)
library(shiny)
library(dplyr)
library(raster)
library(maps)
library(RColorBrewer)


#read in species list - on global.R because it shows up in both ui.R and server.R
cat('Reading in list of species with both Aquamaps and IUCN data...\n')
spp_list <- read.csv('data/spp_am_v_iucn.csv', stringsAsFactors = FALSE)


spp_list <- spp_list %>%
  group_by(spp_group) %>%
  filter(iucn_sid < quantile(iucn_sid, 0.10, na.rm = TRUE))

#set species list
spp_choices <- as.character(sort(unique(spp_list$sciname)))
