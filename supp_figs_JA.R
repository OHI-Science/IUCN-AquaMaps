# plot for supplement

library(raster)
library(rgdal)
library(maps)
library(readr)
library(maptools)
library(tmap)

data("wrld_simpl", package="maptools")

source('~/github/ohiprep/src/R/common.R')

sp <- "Thunnus alalunga"
setwd('shiny_am_iucn')
source('server_fxns.R')
setwd('../')

spp_list <- read.csv('shiny_am_iucn/data/spp_list.csv')

# get rasters for tuna from IUCN and AM

df     <- get_spp_map_df(sp)
r_am   <- get_rast(df,type = 'am')
r_iucn <- get_rast(df,type = 'iucn')

plot(r_iucn)
#playing with grids
grid(nx = 720, ny = 360, col = 'lightgray')

#crop so we can zoom in and show grid cells

r_iucn_crop <- crop(r_iucn, extent(c(100,150,-25,25)))
r_am_crop   <- crop(r_am, extent(c(100,150,-25,25)))

basemap <- crop(wrld_simpl, extent(c(100,150,-25,25)))

#pretty up with tmap
tuna_iucn_raster_grid <- tm_shape(r_iucn_crop) +
                              tm_raster(palette = 'aquamarine3',
                                        colorNA = NULL,
                                        alpha = .8,
                                        legend.show = FALSE) +
                              tm_shape(basemap) +
                              tm_fill(col = 'gray') + 
                              tm_layout(basemaps = "Esri.WorldTopoMap", 
                                        # title.position = 'TOP', 
                                        legend.outside = TRUE, attr.outside = TRUE)+
                              tm_grid(n.x = 100, n.y = 100, projection = 'longlat', col = 'lightgray', labels.inside.frame = FALSE,labels.size=0)

save_tmap(tuna_iucn_raster_grid, filename = 'figures/si_figs_JA/tuna_iucn_grid_raster.png')

tuna_iucn_raster <- tm_shape(r_iucn_crop) +
  tm_raster(palette = 'aquamarine3',
            colorNA = NULL,
            alpha = .8,
            legend.show = FALSE) +
  tm_shape(basemap) +
  tm_fill(col = 'gray')

save_tmap(tuna_iucn_raster, filename = 'figures/si_figs_JA/tuna_iucn_raster.png')

tuna_am_raster <- tm_shape(r_am) +
  tm_raster(palette = 'indianred3',
            colorNA = NULL,
            alpha = .8,
            legend.show = FALSE) +
  tm_shape(wrld_simpl) +
  tm_fill(col = 'gray')

save_tmap(tuna_am_raster, filename = 'figures/si_figs_JA/tuna_am_world.png')

#get shapefile and crop to same extent

tuna <- readOGR(dsn = file.path(dir_M,'git-annex/globalprep/_raw_data/iucn_spp/d2015/iucn_shp'),layer = 'TUNAS_BILLFISHES')

dir_M             <- c('Windows' = '//mazu.nceas.ucsb.edu/ohi',
                       'Darwin'  = '/Volumes/ohi',    ### connect (cmd-K) to smb://mazu/ohi
                       'Linux'   = '/home/shares/ohi')[[ Sys.info()[['sysname']] ]]


ta <- tuna%>%subset(binomial == 'Thunnus alalunga')

tuna_wrld <- tm_shape(ta)+
                  tm_fill(col='aquamarine3', alpha = 0.8)+
                  tm_shape(wrld_simpl)+
                  tm_fill(col = 'gray')

save_tmap(tuna_wrld, filename ='figures/si_figs_JA/tuna_world.png')


ta_crop <- crop(ta,extent(c(100,150,-25,25)))

tuna_shp_crop <- tm_shape(ta_crop)+
                  tm_fill(col='aquamarine3', alpha = 0.8)+
                  tm_shape(basemap)+
                  tm_fill(col = 'gray')

save_tmap(tuna_shp_crop, filename ='figures/si_figs_JA/tuna_crop.png')

tuna_crop_grid <- tuna_shp_crop+
                    tm_grid(n.x = 100, n.y = 100, projection = 'longlat', col = 'lightgray', labels.inside.frame = FALSE,labels.size=0)


save_tmap(tuna_crop_grid,filename = 'figures/si_figs_JA/tuna_crop_grid.png')

