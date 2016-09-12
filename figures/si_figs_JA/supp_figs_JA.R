library(raster)
# library(maps)
library(maptools)
library(tmap)
library(rgdal)
library(readr)

source('~/github/ohiprep/src/R/common.R')


dir_git <- '~/github/IUCN-AquaMaps'

data("wrld_simpl", package = "maptools")
data(World)

ohi  <- readOGR(dsn = file.path(dir_M, 'git-annex/globalprep/spatial/d2014/data'), layer = 'regions_gcs')
land <- ohi %>%
  subset(rgn_typ == 'land')

sp <- "Thunnus alalunga"
setwd('shiny_am_iucn') ### setwd because there are some relative paths within server_fxns.R
source('server_fxns.R')
setwd('../')

spp_list <- read_csv('shiny_am_iucn/data/spp_list.csv')

# get rasters for tuna from IUCN and AM

df     <- get_spp_map_df(sp)
r_am   <- get_rast(df, type = 'am')
r_iucn <- get_rast(df, type = 'iucn')

plot(r_iucn)
#playing with grids
grid(nx = 720, ny = 360, col = 'lightgray')

#crop so we can zoom in and show grid cells
crop_ext    <- c(-80, -50,30,50)
r_iucn_crop <- crop(r_iucn, crop_ext)
r_am_crop   <- crop(r_am, crop_ext)
land_crop   <- crop(land, crop_ext) %>%
  subset(rgn_nam %in% c('Canada', 'United States'))
plot(land_crop)


basemap_simpl <- crop(wrld_simpl, crop_ext)
basemap       <- crop(World, crop_ext)

#pretty up with tmap
tuna_iucn_raster_grid <- tm_shape(r_iucn_crop) +
  tm_raster(palette = 'aquamarine3',
            colorNA = NULL,
            alpha = .8,
            legend.show = FALSE) +
  tm_layout(basemaps = "Esri.WorldTopoMap", 
            # title.position = 'TOP', 
            legend.outside = TRUE, attr.outside = TRUE)+
  tm_grid(n.x = 60, n.y = 40, projection = 'longlat', col = 'gray', 
          labels.inside.frame = FALSE,labels.size=0)+
  tm_shape(land_crop) +
  tm_polygons() +
  tm_fill(col = 'gray')

save_tmap(tuna_iucn_raster_grid, filename = 'figures/si_figs_JA/tuna_iucn_grid_raster.png',
          width = 9.5, units = "cm", dpi = 600)

tuna_iucn_raster <- tm_shape(r_iucn_crop) +
  tm_raster(palette = 'aquamarine3',
            colorNA = NULL,
            alpha = .8,
            legend.show = FALSE) +
  tm_shape(land_crop) +
  tm_polygons()+
  tm_fill(col = 'gray')

save_tmap(tuna_iucn_raster, filename = 'figures/si_figs_JA/tuna_iucn_raster.png',
          width = 9.5, units = "cm", dpi = 600)

tuna_am_raster <- tm_shape(r_am) +
  tm_raster(palette = 'indianred3',
            colorNA = NULL,
            alpha = .8,
            legend.show = FALSE) +
  tm_shape(wrld_simpl) +
  tm_fill(col = 'gray')

save_tmap(tuna_am_raster, filename = 'figures/si_figs_JA/tuna_am_world.png',
          width = 19, units = "cm", dpi = 600)

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

save_tmap(tuna_wrld, filename ='figures/si_figs_JA/tuna_world.png',
          width = 19, units = "cm", dpi = 600)


ta_crop <- crop(ta,crop_ext)
extent(ta_crop)<-crop_ext

tuna_shp_crop <- tm_shape(land_crop)+
  tm_polygons() +
  tm_fill(col = 'gray')+
  tm_shape(ta_crop)+
  tm_fill(col='aquamarine3', alpha = 0.8)



save_tmap(tuna_shp_crop, filename ='figures/si_figs_JA/tuna_crop.png',
          width = 9.5, units = "cm", dpi = 600)

tuna_crop_grid <-tm_shape(land_crop)+
  tm_fill(col = 'gray')+
  tm_shape(ta_crop)+
  tm_fill(col='aquamarine3', alpha = 0.8)+
  tm_grid(n.x = 60, n.y = 80, projection = 'longlat', col = 'lightgray', 
          labels.inside.frame = FALSE,labels.size=0)+
  tm_shape(land_crop)+
  tm_polygons()+
  tm_fill(col='gray')


save_tmap(tuna_crop_grid,filename = 'figures/si_figs_JA/tuna_crop_grid.png',
          width = 9.5, units = "cm", dpi = 600)


#create world map of tuna for IUCN combining shapefile and raster

ta_crop <- crop(ta,extent(0,180,-90,90))

r_na <- raster(vals=NA,ext=extent(0,180,-90,90), res = 0.5)
r_iucn_half <- r_iucn%>%crop(extent(-180,0,-90,90))%>%merge(r_na)
plot(r_iucn_half, box = F, axes = F, legend = F)
plot(ta_crop, add = T, col = 'darkgreen')


tuna_iucn_world <- tm_shape(r_iucn_half)+
                    tm_raster(palette = 'indianred3',
                              colorNA = NULL,
                              alpha = .8,
                              legend.show=FALSE)+
                    tm_shape(ta_crop)+
                    tm_fill(col = 'aquamarine3', alpha = 0.8) +
                    tm_shape(wrld_simpl) +
                    tm_fill(col = 'gray')

save_tmap(tuna_iucn_world, filename = 'figures/si_figs_JA/tuna_world_map_IUCN_raster_poly.png',
          width = 19, units = "cm", dpi = 600)
