#load data for species map function, function, and plotting a species

#JAfflerbach
#9.11.2014

## SETUP ##

#libraries

library(data.table)
library(raster)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(maps)


#increase R memory size to deal with reading in the large files

memory.limit(size=200000000)


dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
          'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

dir_aquamaps = file.path(dir_N,"git-annex/globalprep/SpeciesDiversity/raw/aquamaps_2014")
dir_cells    = file.path(dir_N,"git-annex/Global/NCEAS-SpatialFishCatch_v2014/raw/ohi_spp/data")

#read in data
#fread comes from data.table package and is much more efficient at reading in the larger .csv files

spp       = fread(input=file.path(dir_aquamaps,"tables/ohi_speciesoccursum.csv"),header=T)%>%
              mutate(scientific = paste(Genus,Species,sep=" "))

cells     = read.csv(file.path(dir_cells,'cells.csv'))%>%
              dplyr::select(cid,csquarecod,faoaream)
    names(cells)<-c("cid","csquare_code","faoaream")

spp_cells = fread(input=file.path(dir_aquamaps,"tables/ohi_hcaf_species_native.csv"),header=T,
                  colClasses=c('NULL','character',NA,'numeric','NULL','NULL')) #1 min 49 sec

setnames(spp_cells,c('SpeciesID','CsquareCode','probability'),c("species_id","csquare_code","probability"))


#convert spp_cells, which is both a 'data.table' and a 'data.frame' to just a 'data.frame' to allow for plotting. A weird
# caveat of fread() is that the output is both class 'data.table' and 'data.frame'

spp_cells = as.data.frame(spp_cells)


### Species Map Function ###
# This function takes a single species scientific name as input, then grabs all occurrence cells and associated probability per cell

sp_map_fun <- function(species){
  
  sp_id  = filter(spp,scientific==species)$SPECIESID
  sp_map = filter(spp_cells,species_id==sp_id)%>%
    merge(cells,by='csquare_code')
  
  return(sp_map)
}


### Plotting a single species ###

#data for raster (used to plot species)
r               = raster(file.path(dir_cells, 'cells.tif'))
names(r)        = 'cid'

#set species

sp = "Orcinus orca"

#get species occurrence cells

sp_cells = sp_map_fun(sp)

#substitute values of raster cid with sp_cells' probability

r_sp = subs(r, sp_cells[,c('cid','probability')], by='cid', which='probability', subsWithNA=T)

#plot

cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme
plot(r_sp, col=cols,main='Killer Whale')
map('world',col='gray95',fill=T,border='gray80',add=T)

