library(raster)
library(rgdal)
# library(maps)
library(maptools)
library(tmap)
library(readr)

source('~/github/ohiprep/src/R/common.R')


dir_git <- '~/github/IUCN-AquaMaps'

data("wrld_simpl", package = "maptools")
data(World)


sp <- "Thunnus alalunga"
setwd('shiny_am_iucn') ### setwd because there are some relative paths within server_fxns.R
source('server_fxns.R')
setwd('../')

spp_list <- read_csv('shiny_am_iucn/data/spp_list.csv')

### get rasters for tuna from IUCN and AM

df     <- get_spp_map_df(sp)
tuna_rast_am   <- get_rast(df, type = 'am')
tuna_rast_iucn <- get_rast(df, type = 'iucn')


### crop so we can zoom in and show grid cells
crop_ext    <- c(-80, -50, 30, 50)
tuna_rast_iucn_crop <- crop(tuna_rast_iucn, crop_ext)
tuna_rast_am_crop   <- crop(tuna_rast_am, crop_ext)

# ohi  <- readOGR(dsn = file.path(dir_M, 'git-annex/globalprep/spatial/d2014/data'), layer = 'regions_gcs')
# land <- ohi %>%
#   subset(rgn_typ == 'land')
# land_crop   <- crop(land, crop_ext) %>%
#   subset(rgn_nam %in% c('Canada', 'United States'))
# writeOGR(land_crop, dsn = 'figures/si_figs_JA', layer = 'land_crop', driver = 'ESRI Shapefile')
land_crop      <- readOGR(dsn = 'figures/si_figs_JA', layer = 'land_crop')

# tuna_poly <- readOGR(dsn = file.path(dir_M, 'git-annex/globalprep/_raw_data/iucn_spp/d2015/iucn_shp'),
#                      layer = 'TUNAS_BILLFISHES')
# tuna_poly <- tuna_poly %>% 
#   subset(binomial == 'Thunnus alalunga')
# tuna_poly_crop <- crop(tuna_poly, crop_ext)
# extent(tuna_poly_crop) <- crop_ext
# writeOGR(tuna_poly, dsn = 'figures/si_figs_JA', layer = 'tuna_poly', driver = 'ESRI Shapefile')
# writeOGR(tuna_poly_crop, dsn = 'figures/si_figs_JA', layer = 'tuna_poly_crop', driver = 'ESRI Shapefile')
tuna_poly      <- readOGR(dsn = 'figures/si_figs_JA', layer = 'tuna_poly')
tuna_poly_crop <- readOGR(dsn = 'figures/si_figs_JA', layer = 'tuna_poly_crop')

########################################################=
### Create maps of cropped tuna rasters and shapes -----

### Raster finished; no grid
tuna_iucn_raster <- tm_shape(tuna_rast_iucn_crop) +
  tm_raster(palette = 'darkcyan',
            colorNA = NULL,
            alpha = .8,
            legend.show = FALSE) +
  tm_shape(land_crop) +
  tm_polygons(border.col = 'grey45', col = 'grey40', lwd = 0.25) + 
  tm_layout(legend.text.size = .5,
            legend.title.size = .6,
            # title.position = 'TOP', 
            legend.outside = FALSE, 
            legend.position = c('left', 'bottom'),
            legend.bg.color = 'white',
            legend.bg.alpha = .5,
            attr.outside = TRUE)

save_tmap(tuna_iucn_raster, filename = 'figures/si_figs_JA/tuna_iucn_raster.png',
          width = 9.4, # height = 6.5, 
          units = 'cm', dpi = 600)

### Raster finished, with grid overlay
tuna_iucn_raster_grid <- tm_shape(tuna_rast_iucn_crop) +
  tm_raster(palette = 'darkcyan',
            colorNA = NULL,
            alpha = .8,
            legend.show = FALSE) +
  tm_grid(n.x = 60, n.y = 40, projection = 'longlat', col = 'gray', 
          labels.inside.frame = FALSE, labels.size = 0, lwd = .25) +
  tm_shape(land_crop) +
  tm_polygons(border.col = 'grey45', col = 'grey40', lwd = 0.25) + 
  tm_layout(attr.outside = TRUE)

save_tmap(tuna_iucn_raster_grid, filename = 'figures/si_figs_JA/tuna_iucn_grid_raster.png',
          width = 9.4, # height = 6.5, 
          units = 'cm', dpi = 600)

### Tuna Polygon with no grid

tuna_iucn_shp <- tm_shape(tuna_rast_iucn_crop) + 
  ### use invisible raster to force extents properly... ???
    tm_raster(palette = 'darkcyan',
              colorNA = NULL,
              alpha = 0,
              legend.show = FALSE) +
  tm_shape(tuna_poly_crop) +
    tm_polygons(col = 'indianred3',
                border.col = 'indianred4',
                colorNA = NULL,
                alpha = .8,
                lwd = 0.25, 
                legend.show = FALSE) +
  tm_shape(land_crop) +
    tm_polygons(border.col = 'grey45', col = 'grey40', lwd = 0.25) + 
  tm_layout(attr.outside = TRUE)

save_tmap(tuna_iucn_shp, filename ='figures/si_figs_JA/tuna_iucn_shp.png',
          width = 9.4, # height = 6.5, 
          units = 'cm', dpi = 600)

tuna_iucn_shp_grid <-  tm_shape(tuna_rast_iucn_crop) +
  ### use invisible raster to force extents properly... ???
    tm_raster(palette = 'darkcyan',
              colorNA = NULL,
              alpha = 0,
              legend.show = FALSE) +
  tm_shape(tuna_poly_crop) +
    tm_polygons(col = 'indianred3',
                border.col = 'indianred4',
                colorNA = NULL,
                alpha = .8,
                lwd = 0.25, 
                legend.show = FALSE) +
  tm_grid(n.x = 60, n.y = 40, projection = 'longlat', col = 'gray', 
          labels.inside.frame = FALSE, labels.size = 0, lwd = .25) +
  tm_shape(land_crop) +
    tm_polygons(border.col = 'grey45', col = 'grey40', lwd = 0.25) + 
  tm_layout(attr.outside = TRUE)

save_tmap(tuna_iucn_shp_grid, filename = 'figures/si_figs_JA/tuna_iucn_shp_grid.png',
          width = 9.4, # height = 6.5, 
          units = 'cm', dpi = 600)

########################################################=
### create world map of tuna for IUCN: poly + rast -----

# tuna_poly_half <- crop(tuna_poly, extent(-180, 0, -90, 90))
# writeOGR(tuna_poly_half, 'figures/si_figs_JA', 'tuna_poly_half', driver = 'ESRI Shapefile')
tuna_poly_half <- readOGR('figures/si_figs_JA', 'tuna_poly_half')

na_rast_half   <- raster(vals = NA, ext = extent(-180, 0, -90, 90), res = 0.5)

tuna_rast_half <- tuna_rast_iucn %>% 
  crop(extent(0, 180, -90, 90)) %>% 
  merge(na_rast_half)

tuna_iucn_world <- tm_shape(tuna_rast_half) +
  tm_raster(palette = 'darkcyan',
            colorNA = NULL,
            alpha = .8,
            legend.show = FALSE) +
  tm_shape(tuna_poly_half) +
    tm_polygons(col = 'indianred3', 
                border.col = 'indianred4',
                lwd = 0.25,
                alpha = 0.8) +
  tm_shape(wrld_simpl) +
    tm_polygons(border.col = 'grey45', col = 'grey40', lwd = 0.25)
  
save_tmap(tuna_iucn_world, 
          filename = 'figures/si_figs_JA/tuna_world_IUCN_rastpoly.png',
          width = 19, 
          units = "cm", dpi = 600)

########################################################=
### Create global AquaMaps tuna raster; pres + prob -----

### pres/abs
rast_am_pres <- tuna_rast_am %>%
  crop(extent(c(0, 180, -90, 90))) %>%
  merge(raster(vals = NA, 
               ext = extent(-180, 0, -90, 90), res = 0.5))

### prob occurrence

### get spp_cells raw data for aquamaps
# dir_aquamaps <- file.path(dir_M, 'git-annex/globalprep/_raw_data/aquamaps/d2015')
# 
# am_spp_cells <- read_csv(file.path(dir_aquamaps, 'csv/hcaf_sp_native_trunc.csv'), 
#                       col_types = 'ccd', progress = TRUE) %>%
#   rename(am_sid = speciesid) %>%
#   filter(am_sid == 'Fis-22832')
# write_csv(am_spp_cells, 'figures/si_figs_JA/tuna_am_prob.csv')
am_spp_cells <- read_csv('figures/si_figs_JA/tuna_am_prob.csv')

### turn dataframe into raster

rast_am_prob <- subs(loiczid_raster, am_spp_cells %>%
                       select(loiczid, probability), 
                     by = 'loiczid', 
                     which = 'probability', 
                     subsWithNA = TRUE)
rast_am_prob_half <- rast_am_prob %>%
  crop(extent(-180, 0, -90, 90)) %>%
  merge(raster(vals = NA, ext = extent(0, 180, -90, 90), res = 0.5))


### combine with pres/absence raster
tuna_am_combine <- tm_shape(rast_am_prob_half) +
  tm_raster(palette = 'YlOrRd',
            colorNA = NULL,
            alpha = .8,
            legend.show = TRUE,
            title = "Probability of Occurrence") +
  tm_layout(legend.text.size = .5, #Casey's default from coral maps was .5 - pretty small for this map; Jamie changed to .8
            legend.title.size = .6, #default from coral maps was .6; Jamie changed to .9
            # title.position = 'TOP', 
            legend.outside = FALSE, 
            legend.position = c('left', 'bottom'),
            legend.bg.color = 'white',
            legend.bg.alpha = .8) +
  tm_shape(rast_am_pres) +
  tm_raster(palette = "darkcyan",
            colorNA = NULL,
            alpha = .8,
            legend.show = FALSE) +
  tm_shape(wrld_simpl) +
   tm_polygons(border.col = 'grey45', col = 'grey40', lwd = 0.25)

save_tmap(tuna_am_combine, 
          filename = 'figures/si_figs_JA/tuna_world_map_AM_combine.png',
          width = 19, units = "cm", dpi = 600)
