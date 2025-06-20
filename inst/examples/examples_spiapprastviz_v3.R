library(magrittr)
library(terra)
library(lmomPi)
library(terraclivaviz)
library(terracliva)
library(sf)
### SOURCE needed
library(xml2)
library(ggplot2)
library(RColorBrewer)
library(data.table)


years <- 1982:2023
dataset_monthly <- "/home/ecor/local/rpackages/jrc/terraclivaviz/inst/ext_data/mekrou/CHIRPS_StackMekrou.nc"
dataset_monthly <- rast(dataset_monthly)+0

dataset_sf <- "/home/ecor/local/rpackages/jrc/terraclivaviz/inst/ext_data/mekrou/Mekrou_AOI/Mekrou_AOI_v3.shp" %>% st_read()


outspi <- spiapprast(x=dataset_monthly,distrib="pe3",summary_regress=TRUE,add_cat=TRUE,spi.scale=1)

###filenames <- system.file(package="terraclivaviz") %>% file.path("examples/plot/spi/spi_%s.jpg")
settings <- "/home/ecor/local/rpackages/jrc/terraclivaviz/inst/settings/lm_plot_settings_enexus.xml"
filenames <- "/home/ecor/local/rpackages/jrc/terraclivaviz/inst/examples/plot/spi/spi_mekrow_%s.jpg"
out <- spiapprastviz(x=outspi,filenames=filenames,sf=dataset_sf,signif=0.1,write_tif=TRUE,month=6:10)

###

dataset_monthly <- "/home/ecor/local/rpackages/jrc/terraclivaviz/inst/ext_data/mekrou/CHIRPS_StackMekrou.nc "