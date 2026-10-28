

run_lmapprast_excdef <- function(inputFile,inputShape,startDir,step ="year",start_date,end_date,rt_years=c(2,5,10,20,50),pcts=c(5,10,15,20,30,40),distrib="pe3",mon_vec=1:12,clipByShape=FALSE,frame_bf=5,create_tif_file=TRUE)
{
  
  settings = system.file("settings/lm_plot_settings_enexus.xml",package="terraclivaviz")
  t_name = 'Precipitation'
  unit='mm'
  
  # dataset_monthlysum  <- (raster::brick(inputFile) + 0)
  # time_sel <- as.Date(names(dataset_monthlysum),format="X%Y.%m.%d")
  # dataset_monthlysum = dataset_monthlysum[[which(month(time_sel)%in% mon_vec)]]
  # year_sel <- format(as.Date(names(dataset_monthlysum),format="X%Y.%m.%d"),format="X%Y") ##EC 20240226
  
  fromPgsql=class(inputFile)!='character'
  
  if(fromPgsql)
  {
    names(inputFile) = seq(from= as.Date(start_date),to=as.Date(end_date),by=st_chk,format="X%Y.%m.%d")
    dataset_monthlysum = inputFile + 0
  }else dataset_monthlysum  <- (raster::brick(inputFile) + 0)
  
  dataset_monthlysum  <- (raster::brick(inputFile) + 0)
  ztime <- as.Date(names(dataset_monthlysum),format="X%Y.%m.%d")
  dataset_monthlysum <- setZ(dataset_monthlysum,ztime)
  dataset_monthlysum = dataset_monthlysum |> dateCheck(start_date,end_date,'month',fromPgsql)
  
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
    filename = file.path(out_dir,"lm_monthly_excdef.tif")
  }else{
    dataset_in <- mergeMonthly(year_sel, dataset_monthlysum, time_sel,'sum') %>% rast()
    out_dir = file.path(startDir,"lm_yearly")
    lm_ind = 1
    filename = file.path(out_dir,"lm_yearly_excdef.tif")
  }
  if(!create_tif_file) filename = ""
  rm(dataset_monthlysum)
  
  dataset_sf <- inputShape %>% sf::st_read()
  EE = ext(vect(dataset_sf)) * (1 + frame_bf/100)
  dataset_in <- crop(dataset_in,EE)
  
  terra::time(dataset_in) <- as.Date(names(dataset_in),format="X%Y.%m.%d")
  if(!dir.exists(out_dir)) dir.create(out_dir)
  
  filenames <- file.path(out_dir,"lm_%s.png")
  out_data <- lmapprast(dataset_in,index=lm_ind,distrib=distrib,rt=rt_years,pcts=pcts,add_t=TRUE,return_as_precipitation=FALSE,filename=filename)
  out_data_viz <- lmapprastviz_enexus(x=out_data,filenames,sf=dataset_sf,settings=settings,rt_only=FALSE,use_levelplot=TRUE,mask=clipByShape,
                                      write_tif=TRUE,mon_vec=mon_vec,t_name=t_name, unit=unit, width=2800, height=1500, units='px', res=300)
  
}


run_lmapprast_rt <- function(inputFile,inputShape,startDir,step ="year",start_date,end_date,rt_years=c(2,5,10,20,50),distrib="pe3",KtoC,type,mon_vec=1:12,monthly_data=(step == "month"),clipByShape=FALSE,frame_bf=5,create_tif_file=TRUe)
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
  
  if (step == 'day'){
    step = 'year'
    st_chk = 'day'
  }else if(monthly_data){
    start_date = sprintf("%04d-%02d-01",year(start_date),month(start_date))
    end_date = sprintf("%04d-%02d-01",year(end_date),month(end_date))
    st_chk = 'month'
  }else{
    start_date = sprintf("%s-01-01",year(start_date))
    end_date = sprintf("%s-01-01",year(end_date))
    st_chk = 'year'
  }
  
  fromPgsql=class(inputFile)!='character'
  
  if(fromPgsql)
  {
    names(inputFile) = seq(from= as.Date(start_date),to=as.Date(end_date),by=st_chk,format="X%Y.%m.%d")
    dataset_orig = inputFile + 0
  }else dataset_orig  <- (raster::brick(inputFile) + 0)
  
  ztime <- as.Date(names(dataset_orig),format="X%Y.%m.%d")
  dataset_orig <- setZ(dataset_orig,ztime)
  dataset_in = dataset_orig |> dateCheck(start_date,end_date,st_chk,fromPgsql) |> rast()
  
  if(length(mon_vec)!=12) dataset_in = dataset_in[[which(month(ztime)%in% mon_vec)]]
  
  if(step == 'month')
  {
    out_dir = file.path(startDir,"lm_monthly")
    lm_ind = "monthly"
    filename = file.path(out_dir,"lm_monthly_rp.tif")
  }else if(step == 'year'){
    
    out_dir = file.path(startDir,"lm_yearly")
    lm_ind = 1
    filename = file.path(out_dir,"lm_yearly_rp.tif")
  }
  if(!create_tif_file) filename = ""
  if(KtoC) dataset_in <- dataset_in -273.15
  
  dataset_sf <- inputShape %>% sf::st_read()
  EE = ext(vect(dataset_sf)) * (1 + frame_bf/100)
  dataset_in <- crop(dataset_in,EE)
  
  terra::time(dataset_in) <- as.Date(names(dataset_in),format="X%Y.%m.%d")
  if(step == 'year' && monthly_data) dataset_in = dataset_in |> tapp(index = 'years', fun = max)
  
  dir.create(out_dir)
  filenames <- file.path(out_dir,"lm_%s.png")
  
  out_data <- lmapprast(dataset_in,index=lm_ind,distrib=distrib,rt=rt_years,add_t=TRUE,filename=filename)
  out_data_viz <- lmapprastviz_enexus(x=out_data,filenames,sf=dataset_sf,settings=settings,rt_only=TRUE,use_levelplot=TRUE,mask=clipByShape,
                                      write_tif=TRUE,mon_vec=mon_vec,t_name=t_name, unit=unit, width=2800, height=1500, units='px', res=300)
  
}


run_lmapprast_yearly_full <- function(inputFile,inputShape,startDir,KtoC,type,clipByShape,frame_bf=5,create_tif_file=TRUe)
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
  
  if(create_tif_file) filename = file.path(startDir,"lm_yearly/lm_yearly_rp.tif")
  else filename = ""
  
  filenames <- file.path(startDir,"lm_yearly/lm_%s.png")
  
  out_yearly <- lmapprast(dataset_yearly,filename=filename)
  
  out_yearly_viz <- lmapprastviz_enexus(x=out_yearly,filenames,sf=dataset_sf,settings = settings,use_levelplot=TRUE,mask = clipByShape,
                                        write_tif = TRUE,t_name = t_name,unit = unit,width=2800,height=1500,units='px', res=300)
}


run_hwmidapprast_full <- function(inputFile,inputShape,startDir,start_date,end_date,start_year_ref,end_year_ref,hwmi_thresh,KtoC,clipByShape,frame_bf=5,create_tif_file=TRUE)
{
  tmax_dataset_daily  <- (brick(inputFile) + 0) %>% rast()

  dataset_sf <- inputShape %>% sf::st_read()
  EE = ext(vect(dataset_sf)) * (1 + frame_bf/100)
  tmax_dataset_daily <- crop(tmax_dataset_daily,EE)
  
  terra::time(tmax_dataset_daily) <- as.Date(names(tmax_dataset_daily),format="X%Y.%m.%d")
  if(KtoC) tmax_dataset_daily <- tmax_dataset_daily -273.15
  
  idx <- seq(as.Date(start_date),as.Date(end_date),by = "day")
  idx_ref <- seq(as.Date(sprintf('%i-01-01',start_year_ref)),as.Date(sprintf('%i-12-31',end_year_ref)),by = "day")
  timex_sim = idx
  timex_ref = idx_ref
  if(create_tif_file) filename = file.path(startDir,"hwmid.tif")
  else filename = ""
  
  o_hw_regress <- hwmidapprast(tmax_dataset_daily,summary_regress=TRUE,timex_sim = idx,timex_ref = idx_ref,hwmid_thres=hwmi_thresh,filename=filename)
  
  filenames <- file.path(startDir,"%s.png")
  
  out_hw_regress_viz <- hwmidapprastviz_enexus(x=o_hw_regress,filenames,sf=dataset_sf,use_levelplot=TRUE,mask = clipByShape,
                                               write_tif = TRUE, width=2800, height=1500, units='px', res=300)
}


run_dryspellapprast_full <- function(inputFile,inputShape,startDir,start_date,end_date,mon_vec = 1:12,clipByShape=FALSE,frame_bf=5,create_tif_file=TRUe)
{
  fromPgsql=class(inputFile)!='character'
  
  if(fromPgsql)
  {
    names(inputFile) = seq(from= as.Date(start_date),to=as.Date(end_date),by="day",format="X%Y.%m.%d")
    dataset_in = inputFile
  }else dataset_in  <- raster::brick(inputFile) 
  
  ztime <- as.Date(names(dataset_in),format="X%Y.%m.%d")
  
  file_grd = file.path(startDir,"temp.grd")
  max_size = 1.5e8 #150MB
  
  f_size = file.info(inputFile)$size
  
  if(f_size / max_size > 1){
    n_proc = (f_size / max_size) |> sqrt() |> round() |> max(2)
  }else{
    n_proc = 1
  }
  
  n_proc_x = n_proc
  n_proc_y = n_proc
  
  shp_utm<-shp_create(inputShape)
  EE <- extent(shp_utm) * (1 + frame_bf/100)
  
  x_dist = EE@xmax - EE@xmin
  y_dist = EE@ymax - EE@ymin
  x_step =  x_dist/n_proc_x
  y_step =  y_dist/n_proc_y
  
  EE_temp = EE
  out_data_list = list()
  n = 0
  
  fun_aggr=aggr_fun_suffixes()
  
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

      if (n_proc > 1) filename_dry_spell <- file.path(startDir,sprintf("dry_spell_%i_%i.tif",i,h))
      else filename_dry_spell <- file.path(startDir,"dry_spell.tif")
      
      n = n+1
      
      out_data_list[n] <- dryspellapprast(dataset_tmp,
                                          valmin=1,filename=filename_dry_spell,
                                          summary_regress=TRUE,fun_aggr=fun_aggr,overwrite=TRUE,months=mon_vec)
      if(!create_tif_file) file.remove(filename_dry_spell)
    }
  }
  
  if (n_proc > 1) out_data = do.call(merge, out_data_list)
  else out_data = out_data_list[[1]]
  
  dataset_sf <- inputShape %>% sf::st_read()
  EE = ext(vect(dataset_sf)) * (1 + frame_bf/100)
  
  filenames <- file.path(startDir,"dryspell_%s.png")
  
  file.remove(file_grd)
  file.remove(file_grd |> str_replace(".grd",".gri"))
  
  out_data_viz <- dryspellapprastviz_enexus(out_data,sf=dataset_sf,filenames=filenames,use_levelplot=TRUE,mask = clipByShape,
                                            write_tif = TRUE,width=2800, height=1500, units='px', res=300)
  
}


run_spiapprast_full <- function(inputFile,inputShape,startDir,start_date,end_date,start_year_ref,end_year_ref,sel_regions,region_name_field,mon_vec = 1:12, spi.scale = 3,distrib="pe3",nbigcol=2,clipByShape=FALSE,frame_bf=5,create_tif_file=TRUE)
{
  fromPgsql=class(inputFile)!='character'
  
  if(fromPgsql)
  {
    names(inputFile) = seq(from= as.Date(start_date),to=as.Date(end_date),by="day",format="X%Y.%m.%d")
    dataset_in = inputFile + 0
  }else dataset_in  <- (brick(inputFile) + 0)
  
  ztime <- as.Date(names(dataset_in),format="X%Y.%m.%d")
  dataset_in <- setZ(dataset_in,ztime)
  dataset_in = dataset_in |> dateCheck(start_date,end_date,"day",fromPgsql) |> rast()
  
  dataset_sf <- inputShape %>% sf::st_read()
  EE = ext(vect(dataset_sf)) * (1 + frame_bf/100)
  dataset_in <- crop(dataset_in,EE)
  
  
  # dataset_monthly  <- (brick(inputFile) + 0) %>% rast()
  # 
  # dataset_sf <- inputShape %>% sf::st_read()
  # EE = ext(vect(dataset_sf)) * (1 + frame_bf/100)
  # dataset_monthly <- crop(dataset_monthly,EE)
  
  
  time_x = seq(from=as.Date(start_date),to=as.Date(end_date),by='month')
  
  start_date_ref = as.Date(sprintf("%i-01-01",start_year_ref))
  end_date_ref = as.Date(sprintf("%i-12-01",end_year_ref))
  time_ref = seq(from=start_date_ref,to=end_date_ref,by='month')
  
  terra::time(dataset_in) <- names(dataset_in) %>% as.Date(format="X%Y.%m.%d")
  
  spi_class = read.table(file.path(startDir,"spi_class.csv"),header=TRUE,sep=",",comment.char = '')
  spi_class$color = sprintf("#%sFF",substr(spi_class$color,3,8))
  ##############spi_class from file.csv
  # spi_class = read.table(file.path(startDir,"spi_class.csv"),header=TRUE,sep=",",comment.char = '')
  # red <-    rev(c(192,255,237,219,180,143,47))
  # green <-  rev(c(0,0,125,219,199,170,85))
  # blue <-   rev(c(0,0,49,219,231,220,151))
  # maxcol <- 255
  # 
  # col <- rgb(red=red,green=green,blue=blue,alpha=maxcol,names=names(red),maxColorValue=maxcol)
  # spi_class$color = col
  ###############
  if(create_tif_file) filename = file.path(startDir,sprintf("spi%02i.tif",spi.scale))
  else filename = ""
  
  outspi <- spiapprast(x=dataset_in,timex = time_x, distrib=distrib,summary_regress=TRUE,add_cat=TRUE,
                       spi.scale=spi.scale,spi.classes = spi_class, timex_ref = time_ref,filename=filename)
  
  filenames <- file.path(startDir,"spi_%s.jpg")
  
  out <- spiapprastviz_enexus(x=outspi,filenames=filenames,sf=dataset_sf,signif=0.1,write_tif=TRUE,month=mon_vec,dpi=300,limitsize=FALSE,units="mm",col=spi_class$color,
                              add_spatial_statistics=TRUE,id.name=region_name_field,sel_regions=sel_regions,use_levelplot=TRUE,mask=clipByShape,create.dir=TRUE,ncol_all=nbigcol*length(mon_vec))
  rm(spatial_data)
}
