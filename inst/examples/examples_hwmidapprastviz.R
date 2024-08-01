rm(list=ls())

library(terraclivaviz)
library(magrittr)
library(terra)
library(lmomPi)
library(extRemes)

library(sf)
library(xml2)
library(ggplot2)
library(RColorBrewer)
source("/home/ecor/local/rpackages/jrc/terraclivaviz/R/hwmidapprastviz.R")

years <- 1983:2015
tmax_dataset_path <- system.file("ext_data/tmax",package="terracliva")
tmax_dataset_daily <- "%s/daily/chirts_daily_goma_tmax_%04d.grd" %>% sprintf(tmax_dataset_path,years) %>% rast()
dataset_sf <- system.file("ext_data/OSM_Goma_quartiers_210527.shp",package="terracliva") %>% st_read()


o_hw_regress <- hwmidapprast(tmax_dataset_daily,summary_regress=TRUE,start_month=7)

filenames <- "/home/ecor/local/rpackages/jrc/terraclivaviz/inst/examples/plot/hwmid/hwmid_%s.jpg"
out_hw_regress_viz <- hwmidapprastviz(x=o_hw_regress,filenames,sf=dataset_sf)
