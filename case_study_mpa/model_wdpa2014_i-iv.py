# File: ohiprep/globalprep/WDPA_MPA/v2015/model.py
#
# Modified March 2015 by Casey O'Hara
#
# Filters by 
# * "STATUS" == 'Designated':  includes only officially designated areas, not
#   areas still in planning or proposal phases.
# * "MANG_PLAN" NOT LIKE 'Non-MPA%': eliminates all areas specifically designated
#   as Non-MPA Programmatic Management Plans (code only used for U.S.).  Eliminates
#   groundfish management areas, '25 mph manatee' zones, and the like.

import arcpy, os, subprocess, csv, sys, socket
from arcpy.sa import *

# configuration based on machine name
dirs =  {'git'    :'C:/Users/ohara/Documents/github/IUCN-AquaMaps', 
         'neptune':'N:',
         'tmp'    :'C:/Users/ohara/tmp',
         }
print 'Setting pathnames...'
print dirs

# paths
prod     = 'mpa_analysis/WDPA_MPA'          # name of product
dir_git  = '%s/%s' % (dirs['git'], prod)    # local github directory inside IUCN-AquaMaps
dir_tmp  = '%s/%s' % (dirs['tmp'], prod)    # local temp directory
dir_anx  = '%s/git-annex/Global/WDPA-MPA_v2014' % (dirs['neptune']) # git annex directory on neptune
gdb      = '%s/geodb.gdb' % dir_tmp         # file geodatabase
print 'dir_git = ' + dir_git
print 'dir_tmp = ' + dir_tmp
print 'dir_anx = ' + dir_anx

# inputs
poly_wdpa  = '%s/raw/WDPA_Apr2014_Public1/WDPA_Apr2014.gdb/WDPA_poly_Apr2014' % dir_anx
dir_rgn    = '%s/git-annex/globalprep/spatial/v2015/data' % dirs['neptune']

msk = '%s/rgn_marine.tif' % dir_git   # .tif of 500 m marine raster? will be huge - put on git-annex

print 'input poly_wdpa = ' + poly_wdpa
print 'input dir_rgn = ' + dir_rgn
print 'input rgn_off_3nm = ' + rgn_off_3nm
print 'input rgn_inl_1km = ' + rgn_inl_1km
print 'input msk = ' + msk


# outputs
ply_d = '%s/poly_wdpa_d' % gdb    # designated and protected
tif = '%s/wdpa_designated_mol.tif' % dir_tmp
print 'output tif = ' + tif


# initial env
arcpy.CheckOutExtension('Spatial')
arcpy.env.overwriteOutput=1

# projections: is this used anywhere else?
##sr_mol = arcpy.SpatialReference('Mollweide (world)') # projected Mollweide (54009)
##sr_gcs = arcpy.SpatialReference('WGS 1984')          # geographic coordinate system WGS84 (4326)

# shapefiles don't have nulls, so use geodatabase
if not os.path.exists(dir_tmp):
    os.makedirs(dir_tmp)
if not arcpy.Exists(gdb):
    arcpy.CreateFileGDB_management(os.path.dirname(gdb), os.path.basename(gdb))

# workspace & scratch space
arcpy.env.workspace = gdb; os.chdir(dir_tmp)
arcpy.env.scratchWorkspace = dir_tmp

# develop coastal raster to use as a mask
##print 'Developing coastal raster to use as a mask'
##r = Con(IsNull(Raster(rgn_off_3nm)), Raster(rgn_inl_1km), Raster(rgn_off_3nm))
##r.save(msk)

print 'Setting environment based on msk.'
arcpy.env.snapRaster = arcpy.env.cellSize = arcpy.env.outputCoordinateSystem = arcpy.env.extent = msk

# select only designated
print 'Selecting only designated features and creating new feature class based on STATUS = Designated.'
arcpy.Select_analysis(poly_wdpa, ply_d, '("STATUS" = \'Designated\') AND ("MANG_PLAN" NOT LIKE \'Non-MPA%\')')
# select only categories i - iv
arcpy.Select_analysis(ply_d, ply_d, '("IUCN_CAT" = \'Designated\')')

# create priority field, prioritizing by earliest STATUS_YR (take inverse of STATUS_YR)
print 'Creating and calculating priority field'
arcpy.AddField_management(ply_d, 'poly_priority', 'FLOAT')
arcpy.CalculateField_management(ply_d, 'poly_priority', '1/(!STATUS_YR! + 1)', 'PYTHON_9.3')

# polygon to raster
print 'From Protected polygons, create raster .tif'
arcpy.PolygonToRaster_conversion(ply_d, 'STATUS_YR', tif, 'MAXIMUM_COMBINED_AREA', 'poly_priority', msk)

# copy tmp to neptune
# paths = [gdb, tif, msk]
# if not os.path.exists('%s/tmp' % dir_anx):
#     os.makedirs('%s/tmp' % dir_anx)
# for fro in paths:
#     arcpy.Copy_management(fro, '%s/tmp/%s' % (dir_anx, os.path.basename(fro)))
#     #arcpy.Delete_management(tmp+'/'+d)

