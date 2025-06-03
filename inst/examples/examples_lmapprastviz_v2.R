

####
####
####
####



library(terracliva)
library(terraclivaviz)

library(sf)

years <- 1982:2023



dataset_path <- system.file("ext_data/precipitation",package="terracliva")
dataset_yearly <- "%s/yearly/chirps_yearly_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()
dataset_sf <- system.file("ext_data/OSM_Goma_quartiers_210527.shp",package="terracliva") %>% st_read()

out_yearly <- lmapprast(dataset_yearly)

filenames <- system.file(package="terraclivaviz") %>% file.path("examples/plot/lm/yearly/lm_%s.jpg")
###out_yearly_viz <- lmapprastviz(x=out_yearly,filenames,sf=dataset_sf)




library(lubridate)
dataset_monthly <- "%s/monthly/chirps_monthly_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()
terra::time(dataset_monthly) <-  names(dataset_monthly) %>% paste0("_01") %>% as.Date(format="X%Y_%m_%d")


out_monthly <- lmapprast(dataset_monthly,index="monthly",distrib="pe3")

filenames <- "/home/ecor/local/rpackages/jrc/terraclivaviz/inst/examples/plot/lm/monthly/lm_%s.jpg"
###out_monthly_viz <- lmapprastviz(x=out_monthly,filenames,sf=dataset_sf)


out_monthly_viz <- lmapprastviz(x=out_monthly,filenames,sf=dataset_sf,use_levelplot=TRUE)
