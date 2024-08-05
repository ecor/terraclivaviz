library(terracliva)
library(ggplot2)
library(sf)
library(terraclivaviz)
library(sf)
library(xml2)
library(RColorBrewer)

source("/home/ecor/local/rpackages/jrc/terraclivaviz/R/dryspellapprastviz.R")



years <- 1982:2023
dataset_path <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/precipitation"
dataset_path <- system.file("ext_data/precipitation",package="terracliva")
dataset_daily <- "%s/daily/chirps_daily_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()
dataset_sf <- system.file("ext_data/OSM_Goma_quartiers_210527.shp",package="terracliva") %>% st_read()
filename_dry_spell <- "/home/ecor/local/rpackages/jrc/terraclivaviz/inst/ext_data/goma_dry_spell.tif"

fun_aggr=aggr_fun_suffixes()
if (!file.exists(filename_dry_spell)) {
  out <- dryspellapprast(dataset_daily,valmin=1,filename=filename_dry_spell,summary_regress=TRUE,fun_aggr=fun_aggr,overwrite=TRUE)
} else {
  out <- rast(filename_dry_spell)
}


 
filenames <- "/home/ecor/local/rpackages/jrc/terraclivaviz/inst/examples/plot/dryspell/dryspell_%s.jpg"
out2 <- dryspellapprastviz(out,sf=dataset_sf,filenames=filenames)
 

