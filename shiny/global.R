#global.R

library(data.table)
library(shiny)
library(dplyr)
library(raster)
library(maps)
library(RColorBrewer)


#read in data
#fread comes from data.table package and is much more efficient at reading in the larger .csv files

spp       = read.csv('data/spp.csv')

cells     = read.csv('data/cells.csv')

spp_cells = read.csv('data/spp_cells.csv')


#set species list
species <- as.character(sort(unique(spp$scientific)))


#data for raster (used to plot species)
r               = raster('data/cells.tif')
names(r)        = 'cid'

