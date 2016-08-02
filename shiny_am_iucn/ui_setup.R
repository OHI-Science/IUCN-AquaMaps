library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)

library(shinythemes)

message('working directory ', getwd())
spp_list <- read_csv('data/spp_list.csv')
