# creating a sciname -> common name lookup table. 

## Note: there are 15 species that don't have common names. Many of which are corals, I've saved their sci names as common names
## so we can just use the common name column

library(readr)
library(dplyr)

#get the species information from aquamaps
spp_occur <- read_csv('data/speciesoccursum.csv')%>%
              mutate(sciname = paste(genus,species,sep=" "))%>%
              select(sciname, fbname)

#read in the data used for the quad map, which contains all sci names for our species of interest for the app
data <- read_csv('shiny_am_iucn/data/spp_list_quads_app.csv')%>%
        select(sciname)%>%
        left_join(spp_occur)

#get species that don't have a match
nas <- filter(data,is.na(fbname))

#create a dataframe manually that adds in these common names
missing <- data_frame(sciname = c("Neomonachus schauinslandi", "Centropyge loricula", "Centropyge interruptus", 
"Centropyge nigriocellus", 
"Palinustus mossamicus", 
"Justitia longimanus", 
"Hydrophis major", 
"Zebrasoma veliferum", 
"Parastichopus californicus", 
"Stereomastis sculpta", 
"Symphodus trutta", 
"Xyrichtys koteamea", 
"Halichoeres raisneri", 
"Xyrichtys pastellus", 
"Pseudolabrus mortonii", 
"Novaculichthys macrolepidotus", 
"Xyrichtys halsteadi", 
"Conus chiangi"),
common = c("Hawaiian monk seal","Flame angelfish", "Japanese angelfish", "Blackspot angelfish", 
           "Buffalo Blunthorn Lobster", "Gibbon Furrow Lobster", "Olive-headed sea snake", "Sailfin tang", "Giant california sea cucumber",
           "Flatback lobster", "Emerald wrasse", "Red razorfish", "Cape wrasse", "Pastel razorfish", "Rosy parrotfish",
           "Seagrass wrasse", "Halstead's razorfish", "Chiang's cone"))

#combine the two dataframes and save
out <- data%>%
      mutate(common = ifelse(is.na(fbname),missing$common[match(sciname,missing$sciname)],fbname))%>%
      mutate(common = ifelse(is.na(common),sciname,common))

write.csv(out,file='shiny_am_iucn/data/sci_to_common.csv')
