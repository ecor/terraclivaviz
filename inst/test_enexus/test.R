source('C:/Users/Msi-pc/Desktop/JRC/E-Nexus/R scripts/general.R')
source('C:/Users/Msi-pc/Desktop/JRC/E-Nexus/R scripts/rt_funcs.R')
fileName="C:/Users/Msi-pc/Documents/Visual Studio 2015/Projects/E-Nexus/E-Nexus/bin/Debug/R/Rpackages.txt"
libInit(fileName)

library(xml2)
library(terraclivaviz)
# library(sf)
# library(RColorBrewer)
# library(rasterVis)
source('test/plot_title.R')
source("test/climvar_enexus.R")
source("test/terraclivaviz_enexus.R")

KtoC = TRUE
KtoC = FALSE
type = 'T'
type = 'P'

if(type == 'T'){
  # settings=system.file("settings/lm_plot_settings_enexus_tmp.xml",package="terraclivaviz")
  settings="inst/settings/lm_plot_settings_enexus_tmp.xml"
  t_name = 'Temperature'
  unit='°C'
}else{
  # settings=system.file("settings/lm_plot_settings_enexus.xml",package="terraclivaviz")
  settings="inst/settings/lm_plot_settings_enexus.xml"
  t_name = 'Precipitation'
  unit='mm'
}

# MaxDailyReturnPeriod(startDir = 'C:/Users/Msi-pc/Desktop/OUT/return_periods_(ann_)_PROVA',
#                      inputFile = 'C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Input/Return Periods (annual max precipitation)/CHIRPS_Stack_maxdailyMekrou_new.nc',
#                      type = 'Precip',
#                      shp_utm = shp_utm,
#                      rt_years = rt_years,
#                      start_date = '1981-01-01',
#                      end_date = '2015-01-01',
#                      KtoC = FALSE,
#                      clipByShape = FALSE)


####INPUTS
# inputFile
# inputShape
# startDir
# KtoC
# type

# inputFile = "C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Input/Heat Waves/TmaxMekrou.nc"
##ANNMAX
inputFile = "C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Input/Return Periods (annual max precipitation)/CHIRPS_Stack_maxdailyMekrou.nc"    
##MONSUM
inputFile = "C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Input/Excess-Deficit/CHIRPS_StackMekrou.nc"  
##ANNMAX TEMPERATURE
inputFile = "C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Input/Return Periods (annual max temperature)/Tmax_Stack_maxDaily.nc"
KtoC = TRUE

inputShape = "C:/Users/Msi-pc/Documents/E-Nexus/Shapefiles/Mekrou_AOI/Mekrou_AOI_v3.shp"
startDir = 'C:/Users/Msi-pc/Desktop/OUT'

start_date = '1981-01-01'
end_date = '2015-12-01'


start_date = sprintf("%04d-%02d-01",year(start_date),month(start_date))
end_date = sprintf("%04d-%02d-01",year(end_date),month(end_date))
period <- seq(from= as.Date(start_date),to=as.Date(end_date),by="months") 

dataset_yearly  <- (raster::brick(inputFile) + 0) %>% rast()

####MONSUM
mon_vec = 6:9
dataset_monthlysum  <- (raster::brick(inputFile) + 0)

ztime <- as.Date(names(dataset_monthlysum),format="X%Y.%m.%d")
dataset_monthlysum <- setZ(dataset_monthlysum,ztime)
dataset_monthlysum =dataset_monthlysum |> dateCheck(start_date,end_date,'month')

names(dataset_monthlysum) <- format(period,format="X%Y.%m.%d")
time_sel <- as.Date(names(dataset_monthlysum),format="X%Y.%m.%d")


dataset_monthlysum_sel=dataset_monthlysum[[which(month(time_sel)%in% mon_vec)]]
year_sel <- format(as.Date(names(dataset_monthlysum_sel),format="X%Y.%m.%d"),format="X%Y") ##EC 20240226
dataset_yearly <- mergeMonthly(year_sel, dataset_monthlysum_sel, time_sel,'sum') %>% rast()

####

############################  Input from PgSQL
startDir = 'C:/Users/Msi-pc/Desktop/OUT'
inputShape = 'C:/Users/Msi-pc/Documents/E-Nexus/Shapefiles/WEFE Senegal/Wefe Senegal.shp'
inputFile = "C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Input Senegal/CHIRPS_annmax_1981_2019_senegal_new.nc"
shp_utm<-shp_create('C:/Users/Msi-pc/Documents/E-Nexus/Shapefiles/WEFE Senegal/Wefe Senegal.shp')
conn = dbConnect(RPostgres::Postgres(), user = 'postgres', host = 'localhost', dbname = 'postgres', password = 'postgres')
stackName = pgsql2stack('climate','prec_annmax_senegal','precip','1981-01-01','2019-01-01','ANNUAL', conn)
stackName = pgsql2stack('climate','prec_monmax_mekrou','date','1981-01-01','2015-12-01','MONTHLY', conn)

crs(stackName)  <- 'epsg:4326'
# dataset_yearly  <- (stackName + 0) %>% rast()

names(stackName) = seq(from= as.Date(start_date),to=as.Date(end_date),by="months",format="X%Y.%m.%d")
dataset_orig = stackName + 0


###########################
source("test/lmapprastviz_luigi.R")

if(KtoC) dataset_yearly <- dataset_yearly -273.15

dataset_sf <- inputShape %>% sf::st_read()

#####
EE = ext(vect(dataset_sf))
dataset_yearly <- crop(dataset_yearly,EE)
#####
# dataset_sf2 <- sf::st_read(dsn = dirname(inputShape),layer = tools::file_path_sans_ext(basename(inputShape))) %>%
#   sf::st_zm() ##%>% sf::as_Spatial()
# shp_utm=sp::spTransform(shp_orig, sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
# 
# dataset_sf3 = dataset_sf2 |> sf::as_Spatial() ##|> sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
# shp_utm<-shp_create(inputShape)

terra::time(dataset_yearly) <- as.Date(names(dataset_yearly),format="X%Y.%m.%d")
file.path(startDir,"lm_yearly") %>% dir.create()
# filenames <- file.path(startDir,"lm_yearly/lm_%s.jpg")
filenames <- file.path(startDir,"lm_yearly/lm_%s.png")
# settings_file = "C:/Users/Msi-pc/Desktop/lm_plot_settings_enexus.xml"
# out_yearly <- lmapprast(dataset_yearly)
out_yearly <- lmapprast(dataset_yearly,pcts=c(5,10,15,20,30,40),add_t=TRUE,return_as_precipitation=FALSE) #use for excess/deficit
out_yearly <- lmapprast(dataset_yearly,add_t=TRUE)

out_yearly_viz <- lmapprastviz(x=out_yearly,filenames,sf=dataset_sf,mask = TRUE,write_tif = TRUE)
out_yearly_viz <- lmapprastviz(x=out_yearly,filenames,sf=dataset_sf,use_levelplot=TRUE, 
                               width=2800, height=1500, units='px', res=300)
out_yearly_viz <- lmapprastviz_enexus(x=out_yearly,filenames,sf=dataset_sf,settings=settings,use_levelplot=TRUE, 
                                      t_name=t_name, unit=unit, width=2800, height=1500, units='px', res=300)
out_yearly_viz <- lmapprastviz_enexus(x=out_yearly,filenames,sf=dataset_sf,settings=settings,use_levelplot=TRUE, 
                                      mon_vec=mon_vec,t_name=t_name, unit=unit, width=2800, height=1500, units='px', res=300)

##########RUN LMAPPRAST

# @param start_date start date , e.g. \code{as.Date("1981-01-01")}
# @param end_date start date , e.g. \code{as.Date("1981-01-01")}
# @param rt_years  return periods
# @param pcts variation percentages
# @param distrib probability distribution. Default is \code{c("pe3","glo","gev","gno","gpa","gam")[1]}

# AnnualRainDeficit <- function(startDir, inputFile, shp_utm,start_date,end_date,rt_years,pcts,distrib=c("pe3","glo","gev","gno","gpa","gam"),mon_vec = 1:12, clipByShape = FALSE) 

run_lmapprast_excdef <- function(inputFile,inputShape,startDir,step ="year",start_date,end_date,rt_years=c(2,5,10,20,50),pcts=c(5,10,15,20,30,40),distrib="pe3",mon_vec=1:12,clipByShape=FALSE)
{
  
  settings="inst/settings/lm_plot_settings_enexus.xml"
  t_name = 'Precipitation'
  unit='mm'
  
  # dataset_monthlysum  <- (raster::brick(inputFile) + 0)
  # time_sel <- as.Date(names(dataset_monthlysum),format="X%Y.%m.%d")
  # dataset_monthlysum = dataset_monthlysum[[which(month(time_sel)%in% mon_vec)]]
  # year_sel <- format(as.Date(names(dataset_monthlysum),format="X%Y.%m.%d"),format="X%Y") ##EC 20240226
  
  dataset_monthlysum  <- (raster::brick(inputFile) + 0)
  ztime <- as.Date(names(dataset_monthlysum),format="X%Y.%m.%d")
  dataset_monthlysum <- setZ(dataset_monthlysum,ztime)
  dataset_monthlysum = dataset_monthlysum |> dateCheck(start_date,end_date,'month')

  start_date = sprintf("%04d-%02d-01",year(start_date),month(start_date))
  end_date = sprintf("%04d-%02d-01",year(end_date),month(end_date))
  period <- seq(from= as.Date(start_date),to=as.Date(end_date),by="months")

  names(dataset_monthlysum) <- format(period,format="X%Y.%m.%d")
  time_sel <- as.Date(names(dataset_monthlysum),format="X%Y.%m.%d")
  dataset_monthlysum = dataset_monthlysum[[which(month(time_sel)%in% mon_vec)]]
  year_sel <- format(as.Date(names(dataset_monthlysum),format="X%Y.%m.%d"),format="X%Y") ##EC 20240226
  
  
  if(step == 'month')
  {
    dataset_in <- dataset_monthlysum %>% rast()
    out_dir = file.path(startDir,"lm_monthly")
    lm_ind = "monthly"
  }else{
    dataset_in <- mergeMonthly(year_sel, dataset_monthlysum, time_sel,'sum') %>% rast()
    out_dir = file.path(startDir,"lm_yearly")
    lm_ind = 1
  }
  
  rm(dataset_monthlysum)
  
  dataset_sf <- inputShape %>% sf::st_read()
  EE = ext(vect(dataset_sf))
  dataset_in <- crop(dataset_in,EE)
  
  terra::time(dataset_in) <- as.Date(names(dataset_in),format="X%Y.%m.%d")
  if(!dir.exists(out_dir)) dir.create(out_dir)
  
  filenames <- file.path(out_dir,"lm_%s.png")
  out_data <- lmapprast(dataset_in,index=lm_ind,distrib=distrib,rt=rt_years,pcts=pcts,add_t=TRUE,return_as_precipitation=FALSE)
  out_data_viz <- lmapprastviz_enexus(x=out_data,filenames,sf=dataset_sf,settings=settings,rt_only=FALSE,use_levelplot=TRUE,mask=clipByShape,
                                        mon_vec=mon_vec,t_name=t_name, unit=unit, width=2800, height=1500, units='px', res=300)
  
}

run_lmapprast_rt <- function(inputFile,inputShape,startDir,step ="year",start_date,end_date,rt_years=c(2,5,10,20,50),distrib="pe3",KtoC,type,mon_vec=1:12,clipByShape=FALSE, monthly_data = (step == "month"))
{
  if(type == 'T'){
    settings="inst/settings/lm_plot_settings_enexus_tmp.xml"
    t_name = 'Temperature'
    unit='°C'
  }else{
    settings="inst/settings/lm_plot_settings_enexus.xml"
    t_name = 'Precipitation'
    unit='mm'
  }
  
  if(monthly_data){
    start_date = sprintf("%04d-%02d-01",year(start_date),month(start_date))
    end_date = sprintf("%04d-%02d-01",year(end_date),month(end_date))
    st_chk = 'month'
  }else{
    start_date = sprintf("%s-01-01",year(start_date))
    end_date = sprintf("%s-01-01",year(end_date))
    st_chk = 'year'
  }
  
  fromPgsql=class(inputFile)!='character'
  # period <- seq(from= as.Date(start_date),to=as.Date(end_date),by=step)
  
  if(fromPgsql)
    {
    names(stackName) = seq(from= as.Date(start_date),to=as.Date(end_date),by=st_chk,format="X%Y.%m.%d")
    dataset_orig = stackName + 0
  }else dataset_orig  <- (raster::brick(inputFile) + 0)
    
 
  ztime <- as.Date(names(dataset_orig),format="X%Y.%m.%d")
  dataset_orig <- setZ(dataset_orig,ztime)
  dataset_in = dataset_orig |> dateCheck(start_date,end_date,st_chk,fromPgsql) |> rast()
  
  if(length(mon_vec)!=12) dataset_in = dataset_in[[which(month(ztime)%in% mon_vec)]]
  
  if(step == 'month')
  {
    out_dir = file.path(startDir,"lm_monthly")
    lm_ind = "monthly"
    
    # dataset_monthly  <- (raster::brick(inputFile) + 0)
    # ztime <- as.Date(names(dataset_monthly),format="X%Y.%m.%d")
    # dataset_monthly <- setZ(dataset_monthly,ztime)
    # dataset_monthly = dataset_monthly |> dateCheck(start_date,end_date,st_chk) |> rast()
    # dataset_in = dataset_monthly[[which(month(ztime)%in% mon_vec)]]

  }else if(step == 'year'){

    out_dir = file.path(startDir,"lm_yearly")
    lm_ind = 1
    
    # dataset_yearly  <- (raster::brick(inputFile) + 0) 
    # ztime <- as.Date(names(dataset_yearly),format="X%Y.%m.%d")
    # dataset_yearly <- setZ(dataset_yearly,ztime)
    # dataset_in = dataset_yearly |> dateCheck(start_date,end_date,st_chk) |> rast()
  }
  
  if(KtoC) dataset_in <- dataset_in -273.15
  
  dataset_sf <- inputShape %>% sf::st_read()
  EE = ext(vect(dataset_sf))
  dataset_in <- crop(dataset_in,EE)
  
  terra::time(dataset_in) <- as.Date(names(dataset_in),format="X%Y.%m.%d")
  if(step == 'year' && monthly_data) dataset_in = dataset_in |> tapp(index = 'years', fun = max)
  
  dir.create(out_dir)
  filenames <- file.path(out_dir,"lm_%s.png")

  out_data <- lmapprast(dataset_in,index=lm_ind,distrib=distrib,rt=rt_years,add_t=TRUE)
  out_data_viz <- lmapprastviz_enexus(x=out_data,filenames,sf=dataset_sf,settings=settings,rt_only=TRUE,use_levelplot=TRUE,mask=clipByShape,
                                      write_tif=TRUE,mon_vec=mon_vec,t_name=t_name, unit=unit, width=2800, height=1500, units='px', res=300)
  
}

#######RETURN TIMES (ANNMAX)
source("test/lmapprastviz_luigi.R")

#########PRECIPITATION
type = 'P'
KtoC = FALSE
##ANNMAX
inputFile = "C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Input/Return Periods (annual max precipitation)/CHIRPS_Stack_maxdailyMekrou.nc"  
step = 'year'
##MONMAX
inputFile = "C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Input/Return Periods (monthly max precipitation) 36 years/CHIRPS_Stack_maxMonthly.nc"  
step = 'month'

#########TEMPERATURE
type = 'T'
KtoC = TRUE
##ANNMAX 
inputFile = "C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Input/Return Periods (annual max temperature)/Tmax_Stack_maxDaily.nc"
KtoC = TRUE

inputShape = "C:/Users/Msi-pc/Documents/E-Nexus/Shapefiles/Mekrou_AOI/Mekrou_AOI_v3.shp"
startDir = 'C:/Users/Msi-pc/Desktop/OUT'
start_date = '1981-01-01'
end_date = '2010-12-31'
rt_years=c(10,20)
distrib="pe3"
mon_vec = 6:9
clipByShape = FALSE

run_lmapprast_rt(inputFile,inputShape,startDir,step,start_date,end_date,rt_years,distrib,KtoC,type,mon_vec,clipByShape)
rm(spatial_data)
########

#######EXCESS/DEFICIT
source("test/lmapprastviz_luigi.R")

# @param start_date start date , e.g. \code{as.Date("1981-01-01")}
# @param end_date start date , e.g. \code{as.Date("1981-01-01")}
# @param rt_years  return periods
# @param pcts variation percentages
# @param distrib probability distribution. Default is \code{c("pe3","glo","gev","gno","gpa","gam")[1]}

# AnnualRainDeficit <- function(startDir, inputFile, shp_utm,start_date,end_date,rt_years,pcts,distrib=c("pe3","glo","gev","gno","gpa","gam"),mon_vec = 1:12, clipByShape = FALSE)  
##MONSUM
inputFile = "C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Input/Excess-Deficit/CHIRPS_StackMekrou.nc"  
inputShape = "C:/Users/Msi-pc/Documents/E-Nexus/Shapefiles/Mekrou_AOI/Mekrou_AOI_v3.shp"
startDir = 'C:/Users/Msi-pc/Desktop/OUT'
start_date = '1981-01-01'
end_date = '2015-12-01'
rt_years=c(10,20,50)
pcts=c(20,30)
distrib="pe3"
type = 'P'
mon_vec = 6:9
KtoC = FALSE
clipByShape = FALSE
step = 'year'
step = 'month'

# function(inputFile,inputShape,startDir,step ="year",start_date,end_date,rt_years=c(2,5,10,20,50),pcts=c(5,10,15,20,30,40),distrib="pe3",mon_vec=1:12,clipByShape=FALSE)

run_lmapprast_excdef(inputFile,inputShape,startDir,step,start_date,end_date,rt_years,pcts)
run_lmapprast_excdef(inputFile,inputShape,startDir,step,start_date,end_date,rt_years,pcts,distrib,mon_vec,clipByShape)

rm(spatial_data)

########





run_lmapprast_yearly_full <- function(inputFile,inputShape,startDir,KtoC,type,clipByShape)
{
  if(type == 'T'){
    settings=system.file("settings/lm_plot_settings_enexus_tmp.xml",package="terraclivaviz")
    t_name = 'Temperature'
    unit='°C'
  }else{
    settings=system.file("settings/lm_plot_settings_enexus.xml",package="terraclivaviz")
    t_name = 'Precipitation'
    unit='mm'
  }
  dataset_yearly  <- (raster::brick(inputFile) + 0) %>% rast()
  if(KtoC) dataset_yearly <- dataset_yearly -273.15
  
  dataset_sf <- inputShape %>% sf::st_read()
  
  terra::time(dataset_yearly) <- as.Date(names(dataset_yearly),format="X%Y.%m.%d")
  file.path(startDir,"lm_yearly") %>% dir.create()
  
  filenames <- file.path(startDir,"lm_yearly/lm_%s.png")
  
  out_yearly <- lmapprast(dataset_yearly)
  
  out_yearly_viz <- lmapprastviz_enexus(x=out_yearly,filenames,sf=dataset_sf,settings = settings,use_levelplot=TRUE,mask = clipByShape,
                                 write_tif = TRUE,t_name = t_name,unit = unit,width=2800,height=1500,units='px', res=300)
}

source("test/lmapprastviz_luigi.R")

##ANNMAX
inputFile = "C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Input/Return Periods (annual max precipitation)/CHIRPS_Stack_maxdailyMekrou.nc"    
##MONSUM
inputFile = "C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Input/Excess-Deficit/CHIRPS_StackMekrou.nc"  
inputFile = "C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Input/Return Periods (annual max temperature)/Tmax_Stack_maxDaily.nc"
inputShape = "C:/Users/Msi-pc/Documents/E-Nexus/Shapefiles/Mekrou_AOI/Mekrou_AOI_v3.shp"
startDir = 'C:/Users/Msi-pc/Desktop/OUT'
KtoC = TRUE
KtoC = FALSE
type = 'T'
type = 'P'
clipByShape = TRUE

run_lmapprast_yearly_full(inputFile,inputShape,startDir,KtoC,type,clipByShape)

colorscale_test =  "blue,cyan,yellow,red"
colorscale_test = "YlGnBu"
contains(colorscale_test,',')
if (str_detect(colorscale_test,',')) colorRampPalette(strsplit(colorscale_test,',')[[1]], bias=1)

colorRampPalette(c("blue", "cyan", "yellow", "red"), bias=1)

# cols <- colorRampPalette(c("blue", "cyan", "yellow", "red"), bias=1)
##############

run_hwmidapprast_full <- function(inputFile,inputShape,startDir,start_date,end_date,start_year_ref,end_year_ref,clipByShape)
{
  tmax_dataset_daily  <- (brick(inputFile) + 0) %>% rast()
  dataset_sf <- inputShape %>% st_read()
  # shp_utm<-shp_create(inputShape)
  
  terra::time(tmax_dataset_daily) <- as.Date(names(tmax_dataset_daily),format="X%Y.%m.%d")
  if(KtoC) tmax_dataset_daily <- tmax_dataset_daily -273.15
  
  idx <- seq(as.Date(start_date),as.Date(end_date),by = "day")
  idx_ref <- seq(as.Date(sprintf('%i-01-01',start_year_ref)),as.Date(sprintf('%i-12-31',end_year_ref)),by = "day")
  timex_sim = idx
  timex_ref = idx_ref
  
  # o_hw_regress <- hwmidapprast(tmax_dataset_daily,summary_regress=TRUE)
  o_hw_regress <- hwmidapprast(tmax_dataset_daily,summary_regress=TRUE,timex_sim = idx,timex_ref = idx_ref)
  
  # filenames <- file.path(startDir,"hwmid/%s.jpg")
  filenames <- file.path(startDir,"hwmid/%s.png")
  
  out_hw_regress_viz <- hwmidapprastviz_enexus(x=o_hw_regress,filenames,sf=dataset_sf,use_levelplot=TRUE,mask = clipByShape,
                                          write_tif = TRUE, width=2800, height=1500, units='px', res=300)
}

source("test/hwmidapprastviz_luigi.R")

inputFile = "C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Input/Heat Waves/TmaxMekrou.nc"
inputShape = "C:/Users/Msi-pc/Documents/E-Nexus/Shapefiles/Mekrou_AOI/Mekrou_AOI_v3.shp"
startDir = "C:/Users/Msi-pc/Desktop/OUT"
start_date = '1981-01-01'
end_date = '2015-12-31'
start_year_ref = 1981
end_year_ref = 2010
clipByShape = FALSE
KtoC = TRUE

run_hwmidapprast_full(inputFile,inputShape,startDir,start_date,end_date,start_year_ref,end_year_ref,clipByShape)

# idx <- seq(as.Date(start_date),as.Date(end_date),by = "day")
# idx_ref <- seq(as.Date(sprintf('%i-01-01',start_year_ref)),as.Date(sprintf('%i-12-31',end_year_ref)),by = "day")
# timex_sim = idx
# timex_ref = idx_ref

#######################
tmax_dataset_daily  <- brick(inputFile) # col + 0 dà errore


idx <- seq(as.Date(start_date),as.Date(end_date),by = "day")

idx_tot <- seq(min(as.Date(sprintf('%i-01-01',start_year_ref)),as.Date(start_date)),
               max(as.Date(sprintf('%i-12-31',end_year_ref)),as.Date(end_date)),by = "day")

tmax_dataset_daily = dateCheck(tmax_dataset_daily,idx[1],idx[length(idx)], 'day')

tmax_dataset_daily  <- (tmax_dataset_daily + 0) %>% rast()
########################

run_dryspellapprast_full <- function(inputFile,inputShape,startDir,clipByShape)
{
  # years = 1
  
  dataset_daily  <- (brick(inputFile) + 0) %>% rast()
  dataset_sf <- inputShape %>% st_read()
  terra::time(dataset_daily) <- as.Date(names(dataset_daily),format="X%Y.%m.%d")
  filename_dry_spell <- file.path(startDir,"dryspell/dry_spell.tif")
  
  fun_aggr=aggr_fun_suffixes()
  
  run_function = TRUE   ##TRUE per attivare la funzione dryspellrast
  
  if (!file.exists(filename_dry_spell) || run_function) {
    out <- dryspellapprast(dataset_daily,
                           valmin=1,filename=filename_dry_spell,
                           summary_regress=TRUE,fun_aggr=fun_aggr,overwrite=TRUE,months=6:9)  ##JUN-SEP
  } else {
    out <- rast(filename_dry_spell)
  }
  
  
  shp_utm<-shp_create(inputShape)
  # filenames <- file.path(startDir,"dryspell/dryspell_%s.jpg")
  filenames <- file.path(startDir,"dryspell/dryspell_%s.png")

  out2 <- dryspellapprastviz_enexus(out,sf=dataset_sf,filenames=filenames,use_levelplot=TRUE,mask = clipByShape,
                               write_tif = TRUE,width=2800, height=1500, units='px', res=300)
}

source("test/dryspellapprastviz_luigi.R")

inputFile ="C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Input/chirps_daily_mekrou.nc"
inputShape = "C:/Users/Msi-pc/Documents/E-Nexus/Shapefiles/Mekrou_AOI/Mekrou_AOI_v3.shp"
startDir = "C:/Users/Msi-pc/Desktop/OUT"
start_date = '1981-01-01'
end_date = '2015-12-31'
mon_vec = 6:9
clipByShape = TRUE
clipByShape = FALSE
# run_dryspellapprast_full(inputFile,inputShape,startDir,clipByShape)
run_dryspellapprast_full(inputFile,inputShape,startDir,start_date,end_date,months_selected = mon_vec,clipByShape=FALSE)
# aaa = "dryspell_drySpellCount_010_021_days_2006.png"
# bbb = strsplit(aaa,"_")[[1]]
# bbb[-c(1,length(bbb))] |> paste(collapse = '_')
########
source("test/lmapprastviz_luigi.R")


dataset_monthly  <- (brick(inputFile) + 0) %>% rast()

mon_vec = 6:9
dataset_time <- as.Date(names(dataset_monthly),format="X%Y.%m.%d")
dataset_monthly = dataset_monthly[[which(month(dataset_time)%in% mon_vec)]]


if(KtoC) dataset_monthly <- dataset_monthly -273.15

dataset_sf <- inputShape %>% sf::st_read()
EE = ext(vect(dataset_sf))
dataset_monthly <- crop(dataset_monthly,EE)

terra::time(dataset_monthly) <- names(dataset_monthly) %>% as.Date(format="X%Y.%m.%d")
file.path(startDir,"lm_monthly") %>% dir.create()
# out_monthly <- lmapprast(dataset_monthly,index="monthly",distrib="pe3")
out_monthly <- lmapprast(dataset_monthly,index="monthly",distrib="pe3",pcts=c(5,10,15,20,30,40),add_t=TRUE,return_as_precipitation=FALSE)
filenames <- file.path(startDir,"lm_monthly/lm_%s.png")
out_monthly_viz <- lmapprastviz(x=out_monthly,filenames,sf=dataset_sf,mask = TRUE,write_tif = TRUE)
out_monthly_viz <- lmapprastviz_enexus(x=out_monthly,filenames,sf=dataset_sf,settings=settings,use_levelplot=TRUE,mask=clipByShape, 
                                 mon_vec=mon_vec,t_name=t_name, unit=unit, width=2800, height=1500, units='px', res=300)

####

x=out_monthly

x=out_yearly
sf=dataset_sf
distrib=eval(formals(lmomPi::pel)$distrib)
if(type == 'T'){
  settings=system.file("settings/lm_plot_settings_enexus_tmp.xml",package="terraclivaviz")
  t_name = 'Temperature'
  unit='°C'
}else{
  settings=system.file("settings/lm_plot_settings_enexus.xml",package="terraclivaviz")
  t_name = 'Precipitation'
  unit='mm'
}
mask=TRUE
mask=FALSE
write_tif=FALSE
shp_utm<-shp_create(inputShape)

###########DRYSPELLS


years <- 1982:2023
dataset_path <- system.file("ext_data/precipitation",package="terracliva")
dataset_daily <- "%s/daily/chirps_daily_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()
dataset_sf <- system.file(
  "ext_data/OSM_Goma_quartiers_210527.shp",package="terracliva") %>% 
  st_read()
filename_dry_spell <- system.file("ext_data/goma_dry_spell.tif",package="terraclivaviz")

## single input
years = 1
inputFiles ="C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Input/chirps_daily_mekrou.nc"
inputShape = "C:/Users/Msi-pc/Documents/E-Nexus/Shapefiles/Mekrou_AOI/Mekrou_AOI_v3.shp"
startDir = "C:/Users/Msi-pc/Desktop/OUT"

dataset_daily  <- (brick(inputFiles) + 0) %>% rast()
dataset_sf <- inputShape %>% st_read()
terra::time(dataset_daily) <- as.Date(names(dataset_daily),format="X%Y.%m.%d")
filename_dry_spell <- file.path(startDir,"dryspell/dry_spell.tif")

fun_aggr=aggr_fun_suffixes()

run_function = TRUE   ##TRUE per attivare la funzione dryspellrast

if (!file.exists(filename_dry_spell) || run_function) {
  out <- dryspellapprast(dataset_daily,
                         valmin=1,filename=filename_dry_spell,
                         summary_regress=TRUE,fun_aggr=fun_aggr,overwrite=TRUE,months=6:9)  ##JUN-SEP
} else {
  out <- rast(filename_dry_spell)
}


shp_utm<-shp_create(inputShape)
# filenames <- file.path(startDir,"dryspell/dryspell_%s.jpg")
filenames <- file.path(startDir,"dryspell/dryspell_%s.png")
out2 <- dryspellapprastviz(out,sf=dataset_sf,filenames=filenames,write_tif=TRUE)
out2 <- dryspellapprastviz_luigi(out,sf=dataset_sf,filenames=filenames,write_tif=TRUE)
out2 <- dryspellapprastviz(out,sf=dataset_sf,filenames=filenames,write_tif=TRUE)
out2 <- dryspellapprastviz_2(out,sf=dataset_sf,filenames=filenames,use_levelplot=TRUE, 
                             width=2800, height=1500, units='px', res=300)


######################

years <- 1983:2016
tmax_dataset_path <- system.file("ext_data/tmax",package="terracliva")
tmax_dataset_daily <- "%s/daily/chirts_daily_goma_tmax_%04d.grd" %>% 
  sprintf(tmax_dataset_path,years) %>% rast()
dataset_sf <- system.file("ext_data/OSM_Goma_quartiers_210527.shp"
                          ,package="terracliva") %>% st_read()

inputFile = "C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Input/Heat Waves/TmaxMekrou.nc"
inputShape = "C:/Users/Msi-pc/Documents/E-Nexus/Shapefiles/Mekrou_AOI/Mekrou_AOI_v3.shp"
startDir = 'C:/Users/Msi-pc/Desktop/OUT'
tmax_dataset_daily  <- (brick(inputFile) + 0) %>% rast()
dataset_sf <- inputShape %>% st_read()
shp_utm<-shp_create(inputShape)

if(KtoC) tmax_dataset_daily <- tmax_dataset_daily -273.15

terra::time(tmax_dataset_daily) <- as.Date(names(tmax_dataset_daily),format="X%Y.%m.%d")

o_hw_regress <- hwmidapprast(tmax_dataset_daily,summary_regress=TRUE)

# filenames <- file.path(startDir,"hwmid/hwmid_%s.jpg")
# filenames <- file.path(startDir,"hwmid/%s.jpg")
filenames <- file.path(startDir,"hwmid/%s.png")

out_hw_regress_viz <- hwmidapprastviz(x=o_hw_regress,filenames,sf=dataset_sf)
out_hw_regress_viz <- hwmidapprastviz_luigi(x=o_hw_regress,filenames,sf=dataset_sf)
out_hw_regress_viz <- hwmidapprastviz_2(x=o_hw_regress,filenames,sf=dataset_sf,use_levelplot=TRUE, 
                                        width=2800, height=1500, units='px', res=300)

################
source("test/spiapprastviz_luigi.R")

library(magrittr)
library(terracliva)
library(lmomPi)

library(xml2)
library(data.table)

inputFile = "C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Input/Excess-Deficit/CHIRPS_StackMekrou.nc"
inputShape = "C:/Users/Msi-pc/Documents/E-Nexus/Shapefiles/Mekrou_AOI/Mekrou_AOI_v3.shp"
startDir = 'C:/Users/Msi-pc/Desktop/OUT'
dataset_monthly  <- (brick(inputFile) + 0) %>% rast()
dataset_sf <- inputShape %>% st_read()
shp_utm<-shp_create(inputShape)
start_year_ref = 1981
end_year_ref = 2010
sel_regions = c('Péhunco','Banikoara')
region_name_field="NAME"

start_date_ref = as.Date(sprintf("%i-01-01",start_year_ref))
end_date_ref = as.Date(sprintf("%i-12-01",end_year_ref))
time_ref = seq(from=start_date_ref,to=end_date_ref,by='month')

terra::time(dataset_monthly) <- names(dataset_monthly) %>% as.Date(format="X%Y.%m.%d")

##############spi_class from file.csv
spi_class = read.table(file.path(startDir,"spi/spi_class.csv"),header=TRUE,sep=",")
red <-    rev(c(192,255,237,219,180,143,47))
green <-  rev(c(0,0,125,219,199,170,85))
blue <-   rev(c(0,0,49,219,231,220,151))
maxcol <- 255

col <- rgb(red=red,green=green,blue=blue,alpha=maxcol,names=names(red),maxColorValue=maxcol)
spi_class$color = col
###############

# outspi <- spiapprast(x=dataset_monthly,distrib="pe3",summary_regress=TRUE,add_cat=TRUE,spi.scale=1)
outspi <- spiapprast(x=dataset_monthly,distrib="pe3",summary_regress=TRUE,add_cat=TRUE,spi.scale=3,
                     spi.classes = spi_class, timex_ref = time_ref)

filenames <- file.path(startDir,"spi/spi_mekrou_%s.jpg")

out <- spiapprastviz_enexus(x=outspi,filenames=filenames,sf=dataset_sf,signif=0.1,write_tif=TRUE,month=6:10,dpi=300,limitsize=FALSE,units="mm",
                     add_spatial_statistics=TRUE,id.name=region_name_field,sel_regions=sel_regions,use_levelplot=TRUE,create.dir=TRUE)

filenames2 <- file.path(startDir,"spi_regions/spi_mekrou_%s.jpg")

if(!dir.exists(dirname(filenames2) |> file.path('png'))) dir.create(dirname(filenames2) |> file.path('png'))
if(!dir.exists(dirname(filenames2) |> file.path('tif'))) dir.create(dirname(filenames2) |> file.path('tif'))

out2 <- spiapprastviz_enexus(x=outspi,filenames=filenames2,
                              sf=dataset_sf,signif=0.1,
                              write_tif=TRUE,month=6:10,dpi=300,limitsize=FALSE,units="mm",
                              add_spatial_statistics=TRUE,id.name=region_name_field,sel_regions=sel_regions,create.dir=TRUE)

out2 <- spiapprastviz_enexus(x=outspi,filenames=filenames2,
                            sf=dataset_sf,signif=0.1,
                            write_tif=TRUE,month=6:10,dpi=300,limitsize=FALSE,units="mm",
                            add_spatial_statistics=TRUE,id.name=region_name_field,sel_regions=sel_regions,use_levelplot=TRUE,create.dir=TRUE)

run_spiapprast_full <- function(inputFile,inputShape,startDir,start_year_ref,end_year_ref,sel_regions,region_name_field,clipByShape)
{
  dataset_monthly  <- (brick(inputFile) + 0) %>% rast()
  dataset_sf <- inputShape %>% st_read()
  
  start_date_ref = as.Date(sprintf("%i-01-01",start_year_ref))
  end_date_ref = as.Date(sprintf("%i-12-01",end_year_ref))
  time_ref = seq(from=start_date_ref,to=end_date_ref,by='month')
  
  terra::time(dataset_monthly) <- names(dataset_monthly) %>% as.Date(format="X%Y.%m.%d")
  
  ##############spi_class from file.csv
  spi_class = read.table(file.path(startDir,"spi/spi_class.csv"),header=TRUE,sep=",")
  red <-    rev(c(192,255,237,219,180,143,47))
  green <-  rev(c(0,0,125,219,199,170,85))
  blue <-   rev(c(0,0,49,219,231,220,151))
  maxcol <- 255
  
  col <- rgb(red=red,green=green,blue=blue,alpha=maxcol,names=names(red),maxColorValue=maxcol)
  spi_class$color = col
  ###############
  
  outspi <- spiapprast(x=dataset_monthly,distrib="pe3",summary_regress=TRUE,add_cat=TRUE,spi.scale=3,
                       spi.classes = spi_class, timex_ref = time_ref)
  
  filenames <- file.path(startDir,"spi/spi_mekrou_%s.jpg")
  
  out <- spiapprastviz_enexus(x=outspi,filenames=filenames,sf=dataset_sf,signif=0.1,write_tif=TRUE,month=6:10,dpi=300,limitsize=FALSE,units="mm",
                             add_spatial_statistics=TRUE,id.name=region_name_field,sel_regions=sel_regions,use_levelplot=TRUE,create.dir=TRUE)
  rm(spatial_data)
}

run_spiapprast_full(inputFile,inputShape,startDir,start_year_ref,end_year_ref,sel_regions,region_name_field,clipByShape)

########################################


text_string = "spi03_on_2015_10"
y_m = str_split(text_string,'_')[[1]][3:4]
yea = y_m[1]
mon = y_m[2]

get_dates <- function(text_string)
{
  y_m = str_split(text_string,'_')[[i]][3:4]
  yea = y_m[1]
  mon = y_m[2]
  return(as.Date(sprintf('%s-%s-01',yea,mon)))
}

y_m_all = str_split(names(xvv),'_')
a <- rep(NA, length(y_m_all))
for (i in 1:length(a))
{
  y_m = y_m_all[[i]][3:4]
  yea = y_m[1]
  mon = y_m[2]
  a[i] = sprintf('%s-%s',yea,mon)
}


####

run_spiapprast_full(inputFile = 'C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Input/Excess-Deficit/CHIRPS_StackMekrou.nc',inputShape = 'C:/Users/Msi-pc/Documents/E-Nexus/Shapefiles/Mekrou_AOI/Mekrou_AOI_v3.shp',startDir = 'C:/Users/Msi-pc/Desktop/OUT/spi_15_05_2026_10_56_03',start_date = '1981-01-01',end_date = '2015-12-01',start_year_ref = 1981,end_year_ref = 2010,sel_regions = c('Banikoara'),region_name_field = 'NAME',mon_vec = c(1,2,3,4,5,6,7,8,9,10,11,12),spi.scale = 3,distrib = 'pe3',nbigcol = 1,clipByShape = FALSE)

colorscale <- settings_list[[nn2[it]]][["colorscale"]] |> str_split(",") |> unlist()

if (colorscale[1]!="") {
  gg <- gg+scale_fill_gradientn(colors=colors,na.value=NA) 
} else {
  midpoint=value_zero
  gg <- gg+scale_fill_gradient2(high=color_max,low=color_min,mid=color_zero,midpoint=midpoint,na.value=NA,limits=limits)
}
filename=str_replace_all(filenames[it]," ","_")
ggsave(filename=filename,plot=gg,width=width,height=height,limitsize=limitsize,dpi=dpi,units=units,create.dir=create.dir,...)


#########
inputFile = 'C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Tanganyika/Input/Precipitation/chirps_daily_tanganyika_1981_2025.nc'
inputShape = 'C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Tanganyika/Shapefiles/input_shp_Regions_ENexus_LakeTanganyikaBasin_v1.May2026/Regions_LTB_withLakes.shp'
startDir = 'C:/Users/Msi-pc/Desktop/JRC/E-Nexus/Tanganyika/Output/dry_spells_29_05_2026_12_09_00'
start_date = '1981-01-01'
end_date = '2025-12-31'
mon_vec=1:12
clipByShape = FALSE
frame_bf = 5

file_grd = file.path(startDir,"temp.grd")

max_size = 1.5e8 #150MB

f_size = file.info(inputFile)$size

n_proc = f_size / max_size
n_proc = (f_size / max_size) |> sqrt() |> round() |> max(1)


# n_proc = 3

n_proc_x = n_proc
n_proc_y = n_proc



dataset_in  <- raster::brick(inputFile) 

shp_utm<-shp_create(inputShape)
EE <- extent(shp_utm) * (1 + frame_bf/100)


x_dist = EE@xmax - EE@xmin
y_dist = EE@ymax - EE@ymin
x_step =  x_dist/n_proc_x
y_step =  y_dist/n_proc_y

EE_temp = EE


####debug
i=1
h=1
EE_temp@xmin = EE@xmin + (i-1)*x_step
EE_temp@xmax = EE@xmax - (n_proc_x-i)*x_step

EE_temp@ymin = EE@ymin + (h-1)*y_step
EE_temp@ymax = EE@ymax - (n_proc_y-h)*y_step

fun_aggr = "mean"
#####


# for(i in 1:n_proc_x)
# {
#   EE_temp@xmin = EE@xmin + (i-1)*x_step
#   EE_temp@xmax = EE@xmax - (n_proc_x-i)*x_step
#   for(h in 1:n_proc_y)
#   {
#     EE_temp@ymin = EE@ymin + (h-1)*y_step
#     EE_temp@ymax = EE@ymax - (n_proc_y-h)*y_step
# 
#     print(EE_temp)
#   }
# }

##BLOCCO ESTERNO

fromPgsql=class(inputFile)!='character'

if(fromPgsql)
{
  names(inputFile) = seq(from= as.Date(start_date),to=as.Date(end_date),by="day",format="X%Y.%m.%d")
  dataset_in = inputFile
}else dataset_in  <- raster::brick(inputFile) 

file_grd = file.path(startDir,"temp.grd")

max_size = 1.5e8 #150MB

f_size = file.info(inputFile)$size

n_proc = (f_size / max_size) |> sqrt() |> round() |> max(1)
n_proc_x = n_proc
n_proc_y = n_proc

shp_utm<-shp_create(inputShape)
EE <- extent(shp_utm) * (1 + frame_bf/100)


x_dist = EE@xmax - EE@xmin
y_dist = EE@ymax - EE@ymin
x_step =  x_dist/n_proc_x
y_step =  y_dist/n_proc_y

EE_temp = EE

file_grd = file.path(startDir,"temp.grd")
out_data_list = list()
n = 0

dataset_in  <- raster::brick(inputFile)
ztime <- as.Date(names(dataset_in),format="X%Y.%m.%d")

##BLOCCO INTERNO

for(i in 1:n_proc_x)
{
  EE_temp@xmin = EE@xmin + (i-1)*x_step
  EE_temp@xmax = EE@xmax - (n_proc_x-i)*x_step
  for(h in 1:n_proc_y)
  {
    EE_temp@ymin = EE@ymin + (h-1)*y_step
    EE_temp@ymax = EE@ymax - (n_proc_y-h)*y_step
    
    dataset_tmp <- crop(dataset_in, EE_temp)
    
    writeRaster(dataset_tmp,file_grd,overwrite = TRUE)
    
    dataset_tmp  <- (raster::brick(file_grd) + 0)
    
    dataset_tmp <- setZ(dataset_tmp,ztime)
    dataset_tmp = dataset_tmp |> dateCheck(start_date,end_date,"day",fromPgsql) |> rast()
    terra::time(dataset_tmp) <- as.Date(names(dataset_tmp),format="X%Y.%m.%d")
    
    filename_dry_spell <- file.path(startDir,sprintf("dry_spell_%i_%i.tif",i,h))
    
    n = n+1
    
    out_data_list[n] <- dryspellapprast(dataset_tmp,
                                        valmin=1,filename=filename_dry_spell,
                                        summary_regress=TRUE,fun_aggr=fun_aggr,overwrite=TRUE,months=mon_vec)
    
    

  }
}

####FINE BLOCCO INT.

out_data = do.call(merge, out_data_list)



filenames <- file.path(startDir,"dryspell_%s.png")

out_data_viz <- dryspellapprastviz_enexus(out_data,sf=dataset_sf,filenames=filenames,use_levelplot=TRUE,mask = clipByShape,
                                          write_tif = TRUE,width=2800, height=1500, units='px', res=300)


###eliminare i file!!!


#################