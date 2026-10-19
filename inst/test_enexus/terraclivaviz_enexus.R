
lmapprastviz_enexus <- function(x,filenames,sf,distrib=eval(formals(lmomPi::pel)$distrib),settings=system.file("settings/lm_plot_settings_enexus.xml",package="terraclivaviz"),rt_only=TRUE,mask=FALSE,write_tif=FALSE,
                                mon_vec=1:12,t_name=NULL,unit=NULL,use_levelplot=FALSE,use_ggplot2=!use_levelplot,device="png",...){
  ## TO DO 
  #### https://en.wikipedia.org/wiki/Data_and_information_visualization
  code_fun <- "lm"
  if (mask==TRUE){ 
    x <- mask(x,mask=vect(sf)) ## added on 2024 10 04
    # EE = ext(vect(sf))
    # x <- crop(x,EE)
  }
  
  if (is.character(settings)) {
    xml_settings <- settings
    settings <- read_xml(xml_settings)
    settings <- xml_children(settings) |> lapply(FUN=xml_children)
    settings_list <- list()
    for (i in 1:length(settings)) {
      settings_list[[i]] <- xml_text(settings[[i]])
      names(settings_list[[i]]) <- xml_name(settings[[i]])
    }
    code_funs <- sapply(settings_list,FUN=function(x){x[["code_fun"]]})
    settings_list <- settings_list[which(code_funs==code_fun)]
    names(settings_list) <- sapply(settings_list,FUN=function(x){x[["particular_id"]]})
    ### settings2 <<- settings_list
  }
  ### settings 
  nn <- names(x)
  ### remove monthly prefix
  monthly_indicator <- sprintf("M%02d",1:12)
  is_monthly <- any(str_detect(nn[1],monthly_indicator))
  if (is_monthly) {
    nmm <- str_length(monthly_indicator[1])
    nn <- str_sub(nn,start=nmm+2)
    
  }
  nn2 <- nn  
  
  ###
  
  ilnx <- which(nn %in% names(settings_list))
  iexc <- which(str_detect(nn,"_exc_rt"))
  idef <- which(str_detect(nn,"_def_rt"))
  iexcp <- which(str_detect(nn,"exc_perc_rt"))
  idefp <- which(str_detect(nn,"def_perc_rt"))
  irtexc <- which(str_detect(nn,"rt_of_exc"))
  irtdef <- which(str_detect(nn,"rt_of_def"))
  
  print(distrib)
  for (ndistrib in distrib) {
    idistrib <- which(str_detect(nn,ndistrib))
    nn2[idistrib] <- "distrib"
  }
  nn2[iexc] <- "_exc_rt"
  nn2[idef] <- "_def_rt"
  nn2[iexcp] <- "_exc_p_rt"
  nn2[idefp] <- "_def_p_rt"
  nn2[irtexc] <- "_exc_pct"
  nn2[irtdef] <- "_def_pct"
  names(nn2) <- names(x)
  #####
  #####
  #####
  ###nn2 <<- nn2
  #####
  
  if (length(filenames)==1) filenames <- sprintf(filenames,names(x))
  names(filenames) <- names(x)
  spatial_data <<- sf |> as_Spatial() 
  for (it in names(x)) {
    print(it)
    print(settings_list[[nn2[it]]][["name"]])
    
    if(str_detect(it,'_prob_')) next   ##NO PROBABILITY EVALUATION
    if(rt_only && str_detect(it,'_perc_')) next
    if(rt_only && str_detect(it,'_def_')) next
    
    
    # if(unit %in% c("°C","K") && str_detect(it,'_perc_')) next
    # if(unit %in% c("°C","K") && str_detect(it,'_def_')) next
    
    # filename=str_replace_all(filenames[it]," ","_")
    filename=file.path(dirname(filenames[it]),basename(filenames[it])|> str_replace_all(" ","_"))
    
    if(is_monthly)
    {
      it_dir = strsplit(it,"_")[[1]][1]
      it_path = file.path(dirname(filenames)[1],it_dir)
      if(!dir.exists(it_path)) dir.create(it_path)
      
      filename=file.path(it_path,basename(filenames[it])|> str_replace_all(" ","_"))
    }
    
    if(settings_list[[nn2[it]]][["name"]]==""){
      out_title = plot_title(it,settings_list[[nn2[it]]][["id"]],t_name,unit,is_monthly)
    } else out_title = settings_list[[nn2[it]]][["name"]]
    
    # if(unit %in% c("°C","K")) out_title = out_title |> str_replace(" Excess "," ")
    if(rt_only) out_title = out_title |> str_replace(" Excess "," ")
    
    ####
    if((length(mon_vec) < 12 & !is_monthly)) out_title <- paste0(out_title, sprintf("[#MON: %s]",paste(mon_vec,collapse = ',')))
    ####
    colorscale <- settings_list[[nn2[it]]][["colorscale"]]
    rev <- as.numeric(settings_list[[nn2[it]]][["rev"]])
    if (str_detect(colorscale,',')) 
      colors <- colorRampPalette(strsplit(colorscale,',')[[1]], bias=1)
    else{
      if (rev<0) 
        colors <- colorRampPalette(rev(brewer.pal(9,colorscale)))
      else
        colors <- colorRampPalette(brewer.pal(9,colorscale))
    }
    
    if (use_ggplot2) {
      gg  <- ggplot()+geom_spatraster(data=x[[it]])+theme_bw()
      gg <-  gg+geom_sf(data=sf,fill=NA,color="black",linewidth=0.15)
      # gg <- gg+ggtitle(it)
      gg <- gg+ggtitle(out_title)
      # colorscale <- settings_list[[nn2[it]]][["colorscale"]]
      # ##print(colorscale)
      colors = colors(9)
      if (rev<0) colors <- rev(colors)
      print(colors)
      gg <- gg+scale_fill_gradientn(colors=colors,na.value=NA)
      # filename=str_replace_all(filenames[it]," ","_")
      ####gg <- gg++theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
      ggsave(filename=filename,plot=gg,...)
      # ggsave(filename=filename,plot=gg)
    } else if (use_levelplot) {
      # Supponiamo che 'x' sia una lista di RasterLayer e 'it' sia l'indice corrente
      raster_layer <- x[[it]]
      ##Filtrare tempi di ritorno da 0 a 50 anni
      if(settings_list[[nn2[it]]][["id"]] %in% c("lm_exc_pct","lm_def_pct"))
      {
        raster_layer[raster_layer < 0] <- NA
        raster_layer[raster_layer > 50] <- NA
      }
      
      if(length(which(!is.na(raster_layer[])))==0)
      {
        raster_layer[]=0
        colors <- colorRampPalette(c("white"), bias=1)
      }
      
      # Supponiamo che 'sf' sia un oggetto Spatial
      # spatial_data <<- sf |> as_Spatial()
      
      # Supponiamo che 'settings_list' e 'nn2' siano liste di impostazioni
      # colorscale <- settings_list[[nn2[it]]][["colorscale"]]
      # rev <- as.numeric(settings_list[[nn2[it]]][["rev"]])
      # colors <- colorRampPalette(brewer.pal(9, colorscale))
      # if (rev < 0) colors <- rev(colors)
      # Creazione del plot con rasterVis::levelplot
      # plot <- levelplot(raster_layer, col.regions = colors, margin=FALSE,main = it) +
      #   latticeExtra::layer(sp::sp.polygons(spatial_data, col = "black", lwd = 0.15))
      plot <- levelplot(raster_layer, col.regions = colors, margin=FALSE,main = out_title) +
        latticeExtra::layer(sp::sp.polygons(spatial_data))
      
      # Salvataggio del plot
      # filename=str_replace_all(filenames[it]," ","_")
      raster::extension(filename) <- ".%s" |> sprintf(device)
      trellis.device(device = device, filename = filename,...)
      print(plot)
      dev.off()
    }
    if (write_tif) {
      filename_tif <- filename
      raster::extension(filename_tif) <- ".tif"
      writeRaster(x[[it]],filename=filename_tif,overwrite=TRUE)
    }
    
  }
  out <- filenames
  return(out)
}

hwmidapprastviz_enexus <- function(x,filenames,sf,settings=system.file("settings/lm_plot_settings_enexus_tmp.xml",package="terraclivaviz"),mask=FALSE,write_tif=FALSE,
                                   use_levelplot=FALSE,use_ggplot2=!use_levelplot,device="png",...){
  
  ## TO DO 
  #### https://en.wikipedia.org/wiki/Data_and_information_visualization
  if (mask==TRUE){ 
    x <- mask(x,mask=vect(sf)) ## added on 2024 10 04
    EE = ext(vect(sf))
    x <- crop(x,EE)
  }
  code_fun <- "hwmid"
  
  if (is.character(settings)) {
    xml_settings <- settings
    settings <- read_xml(xml_settings)
    settings <- xml_children(settings) |> lapply(FUN=xml_children)
    settings_list <- list()
    for (i in 1:length(settings)) {
      settings_list[[i]] <- xml_text(settings[[i]])
      names(settings_list[[i]]) <- xml_name(settings[[i]])
      
    }
    code_funs <- sapply(settings_list,FUN=function(x){x[["code_fun"]]})
    settings_list <- settings_list[which(code_funs==code_fun)]
    names(settings_list) <- sapply(settings_list,FUN=function(x){x[["particular_id"]]})
    
    
  }
  
  
  
  ### settings 
  nn <- names(x)
  nn2 <- nn
  iregress <- which(str_detect(nn,"hwmid"))
  iyear <- -iregress
  
  nn2[iyear] <- "year"
  nn2[iregress] <- "regress"
  
  names(nn2) <- names(x)
  
  
  if (length(filenames)==1) filenames <- sprintf(filenames,names(x))
  names(filenames) <- names(x)
  spatial_data <<- sf |> as_Spatial()
  
  for (it in names(x)) {
    
    out_title = plot_title(it,settings_list[[nn2[it]]][["id"]])
    
    ####it2 <<- it
    if (use_ggplot2) {
      gg  <- ggplot()+geom_spatraster(data=x[[it]])+theme_bw()
      gg <-  gg+geom_sf(data=sf,fill=NA,color="black",linewidth=0.15)
      # gg <- gg+ggtitle(it)
      gg <- gg+ggtitle(out_title)
      
      colorscale <- settings_list[[nn2[it]]][["colorscale"]]
      
      rev <- as.numeric(settings_list[[nn2[it]]][["rev"]])
      colors <-   colorRampPalette(brewer.pal(9,colorscale))(9)
      if (rev<0) colors <- rev(colors)
      print(colors)
      gg <- gg+scale_fill_gradientn(colors=colors,na.value=NA)
      filename=str_replace_all(filenames[it]," ","_")
      # ggsave(filename=filename,plot=gg,...)
      ggsave(filename=filename,plot=gg)
    } else if (use_levelplot) {
      # Supponiamo che 'x' sia una lista di RasterLayer e 'it' sia l'indice corrente
      # raster_layer <- x[[it]]
      
      # Supponiamo che 'settings_list' e 'nn2' siano liste di impostazioni
      colorscale <- settings_list[[nn2[it]]][["colorscale"]]
      rev <- as.numeric(settings_list[[nn2[it]]][["rev"]])
      colors <- colorRampPalette(brewer.pal(9, colorscale))
      if (rev < 0) colors <- rev(colors)
      
      filename=str_replace_all(filenames[it]," ","_")
      
      if (nn2[[it]] == "year"){ 
        names(x)[which(names(x)==it)] = sprintf("X%s",it)
        it = sprintf("X%s",it) 
      }
      
      names(x)[which(names(x)==it)] %<>%  str_replace_all(" ","_")
      it %<>%  str_replace_all(" ","_") 
      
      raster_layer <- x[[it]]
      # Creazione del plot con rasterVis::levelplot
      # plot <- levelplot(raster_layer, col.regions = colors, margin=FALSE,main = it) +
      #   latticeExtra::layer(sp::sp.polygons(spatial_data, col = "black", lwd = 0.15))
      plot <- levelplot(raster_layer, col.regions = colors, margin=FALSE,main = out_title) +
        latticeExtra::layer(sp::sp.polygons(spatial_data))
      
      # Salvataggio del plot
      # filename=str_replace_all(filenames[it]," ","_")
      raster::extension(filename) <- ".%s" |> sprintf(device)
      trellis.device(device = device, filename = filename,...)
      # trellis.device(device = device, filename = filename,width=2800, height=1500, units='px', res=300)
      print(plot)
      dev.off()
    }
    
    if (write_tif) {
      
      filename_tif <- filename
      raster::extension(filename_tif) <- ".tif"
      writeRaster(x[[it]],filename=filename_tif,overwrite=TRUE)
      
    }
    
  }
  
  ##########################HWMID RESUME
  
  years = names(nn2)[iyear]
  filename <- dirname(filename) |> file.path(sprintf('HWMI summary.%s',device))
  
  raster_layer <- x[[iyear]]
  out_title = sprintf('HWMId %s - %s',years[1],tail(years,1))
  plot <- levelplot(raster_layer, col.regions = colors, margin=FALSE,main = out_title) +
    latticeExtra::layer(sp::sp.polygons(spatial_data))
  
  raster::extension(filename) <- ".%s" |> sprintf(device)
  trellis.device(device = device, filename = filename,...)
  # trellis.device(device = device, filename = filename,width=2800, height=1500, units='px', res=300)
  print(plot)
  dev.off()
  
  
  # 
  #   gg  <- ggplot()+geom_spatraster(data=x[[iyear]])+theme_bw()
  #   gg <-  gg+geom_sf(data=sf,fill=NA,color="black",linewidth=0.15)
  #   # gg <- gg+ggtitle(it)
  #   gg <- gg+ggtitle(out_title)
  # 
  #   colorscale <- settings_list$year[["colorscale"]]
  #   rev <- as.numeric(settings_list[[nn2[it]]][["rev"]])
  #   colors <-   colorRampPalette(brewer.pal(9,colorscale))(9)
  #   if (rev<0) colors <- rev(colors)
  #   print(colors)
  #   gg <- gg+scale_fill_gradientn(colors=colors,na.value=NA)
  # 
  #   ggsave(filename=filename,plot=gg,...)
  
  ##########################
  
  out <- filenames
  
  return(out)
  
}

spiapprastviz_enexus <- function(x,filenames,sf,settings=system.file("settings/lm_plot_settings_enexus.xml",package="terraclivaviz"),
                                 signif=attr(x,"signif"),mask=FALSE,write_tif=FALSE,add_spatial_statistics=!is.null(attr(x,"spi_cat")),add_spi_cat_maps=add_spatial_statistics,write_shp=TRUE,
                                 id.name="NAME",sel_regions=NA,month=1:12,add_comprehensive_view=TRUE,nrow_all=NULL,ncol_all=length(month), 
                                 width = NA,height = NA, width_panel = width,height_panel = height,limitsize=FALSE,dpi=300,units="mm",col=NA,
                                 use_levelplot=FALSE,use_ggplot2=!use_levelplot,create.dir=TRUE,...){
  
  ## TO DO 
  #### https://en.wikipedia.org/wiki/Data_and_information_visualization
  if (mask==TRUE){ 
    x <- mask(x,mask=vect(sf)) ## added on 2024 10 04
    EE = ext(vect(sf))
    x <- crop(x,EE)
  }
  code_fun <- "spi"
  spi.scale <- attr(x,"spi.scale")
  if (length(spi.scale)==0) spi.scale <- as.numeric(NA)
  
  y <- attr(x,"spi_cat")
  
  if (is.character(settings)) {
    xml_settings <- settings
    settings <- read_xml(xml_settings)
    settings <- xml_children(settings) |> lapply(FUN=xml_children)
    settings_list <- list()
    for (i in 1:length(settings)) {
      settings_list[[i]] <- xml_text(settings[[i]])
      names(settings_list[[i]]) <- xml_name(settings[[i]])
      
    }
    code_funs <- sapply(settings_list,FUN=function(x){x[["code_fun"]]})
    settings_list <- settings_list[which(code_funs==code_fun)]
    names(settings_list) <- sapply(settings_list,FUN=function(x){x[["particular_id"]]})
    
    
  }
  
  
  
  ### settings 
  nn <- names(x)
  nn2 <- nn
  ## iregress <- which(str_detect(nn,"regress")) ##CAMBIARE TERRACLIVA
  imonth <- which(str_detect(nn,"_on_"))
  ipvalue <- which(str_detect(nn,"pvalue"))
  iregress <-  -c(imonth,ipvalue)
  
  
  nn2[imonth] <- "month"
  nn2[iregress] <- "regress"
  nn2[ipvalue] <- "pvalue"
  names(nn2) <- names(x)
  
  ####
  if(is.na(col)[1]){
    red <-    c(192,255,237,219,180,143,47)
    green <-  c(0,0,125,219,199,170,85)
    blue <-   c(0,0,49,219,231,220,151)
    maxcol <- 255
    
    col <- rgb(red=red,green=green,blue=blue,alpha=maxcol,names=names(red),maxColorValue=maxcol)
  }
  cols <- colorRampPalette(col)
  ####
  
  
  if (length(filenames)==1) filenames <- sprintf(filenames,names(x))
  names(filenames) <- names(x)
  #####   SPIAPPRASTVIZ insert here a unnique_plot TO DO 
  #####
  
  
  nsp <- names(x)
  lnsp <- length(nsp)
  nsp <- nsp[spi.scale:lnsp]
  
  file.path(dirname(filenames)[1],"png") |> dir.create()
  file.path(dirname(filenames)[1],"tif") |> dir.create()
  spatial_data <<- sf |> as_Spatial() 
  for (it in nsp) {
    if (use_ggplot2){
      
      gg  <- ggplot()+geom_spatraster(data=x[[it]])+theme_bw()
      gg <-  gg+geom_sf(data=sf,fill=NA,color="black",linewidth=0.15)
      gg <- gg+ggtitle(it)
      print(settings_list)
      colorscale <- settings_list[[nn2[it]]][["colorscale"]]
      ####print("nn2:")
      ####print(nn2)
      print("it:")
      print(it)
      
      print("colorscale:")
      print(colorscale)
      ###
      color_max <- settings_list[[nn2[it]]][["color_max"]] |> str_trim()
      color_min <- settings_list[[nn2[it]]][["color_min"]]  |> str_trim()
      color_zero <- settings_list[[nn2[it]]][["color_zero"]]  |> str_trim()
      ###
      value_max <- settings_list[[nn2[it]]][["value_max"]]
      value_min <- settings_list[[nn2[it]]][["value_min"]]
      value_zero <- settings_list[[nn2[it]]][["value_zero"]]
      
      if (value_zero=="signif") value_zero <- signif
      
      
      limits <- c(as.numeric(value_min),as.numeric(value_max))
      midpoint <- as.numeric(value_zero)
      
      
      rev <- as.numeric(settings_list[[nn2[it]]][["rev"]])
      colors <-   colorRampPalette(brewer.pal(9,colorscale))(9)
      if (rev<0) colors <- rev(colors)
      ##print(colors)
      ## gg <- gg+scale_fill_gradientn(colors=colors,na.value=NA)
      midpoint=0
      #if (it=="pvalue") midpoint=signif
      gg <- gg+scale_fill_gradient2(high=color_max,low=color_min,mid=color_zero,midpoint=midpoint,na.value=NA,limits=limits)
      filename=str_replace_all(filenames[it]," ","_")
      ggsave(filename=filename,plot=gg,width=width,height=height,limitsize=limitsize,dpi=dpi,units=units,create.dir=create.dir,...)
      
    }else if (use_levelplot){
      
      # filenamepng=str_replace_all(filenames[it]," ","_") |> str_replace(".jpg",".png") 
      filenamepng = file.path(dirname(filenames[it]),'png',basename(filenames[it])) |> str_replace(".jpg",".png") 
      titlePng <- ggtitle(it)[[1]]
      at=seq(-2.5, 2.5, length.out=100)
      
      if(is.na(max(x[[it]][],na.rm = TRUE)) || is.infinite(max(x[[it]][],na.rm = TRUE)))
      {
        print(sprintf("Index '%s' aborted!",it))
        next
      }
      
      xx <- x[[it]]
      
      xx[xx>2.5] <- 2.5
      xx[xx<(-2.5)] <- -2.5
      names(xx) = sprintf('X%s',names(xx)) |>str_replace_all('-','.')
      
      png(filenamepng, width=522*3, height=677*3, units='px', res=dpi)
      print(rasterVis::levelplot(xx, margin=FALSE, main=titlePng, col.regions=cols,at=at) + latticeExtra::layer(sp.polygons(spatial_data)))
      dev.off()
    }
    ####################
    
    if (write_tif) {
      
      filename=str_replace_all(filenames[it]," ","_")
      # filename_tif <- filename
      filename_tif = file.path(dirname(filenames[it]),'tif',basename(str_replace_all(filenames[it]," ","_")))
      raster::extension(filename_tif) <- ".tif"
      writeRaster(x[[it]],filename=filename_tif,overwrite=TRUE)
      
    }
    out <- filenames
  }
  
  if (add_comprehensive_view) {
    
    
    mm <- str_detect(names(x),"_on_") 
    xvv <- x[[mm]]
    terra::time(xvv) <- names(xvv) |> str_split("_on_") |> sapply(function(x){x[[2]]}) |> paste0("_01") |> 
      as.Date(format="%Y_%m_%d")
    
    xvv <- xvv[[which(lubridate::month(terra::time(xvv)) %in% month)]]
    if (is.null(ncol_all)) ncol_all <- NA
    if (is.null(nrow_all)) nrow_all <- NA 
    ####
    if (is.na(ncol_all))  ncol_all <- length(month)  
    if (is.na(nrow_all))  {
      nrow_all <- ceiling(nlyr(xvv)/ncol_all)
    } else {
      ncol_all <- ceiling(nlyr(xvv)/nrow_all)
      
    }
    
    #### EDIT COLOR SCALE
    ####
    ###
    it <- 1 
    color_max <- settings_list[[nn2[it]]][["color_max"]] |> str_trim()
    color_min <- settings_list[[nn2[it]]][["color_min"]]  |> str_trim()
    color_zero <- settings_list[[nn2[it]]][["color_zero"]]  |> str_trim()
    ###
    value_max <- settings_list[[nn2[it]]][["value_max"]] |> as.numeric()
    value_min <- settings_list[[nn2[it]]][["value_min"]] |> as.numeric()
    value_zero <- settings_list[[nn2[it]]][["value_zero"]] |> as.numeric()
    ####
    midpoint <- value_zero 
    limits <- c(value_min,value_max)
    
    ###########################
    if(use_levelplot)
    {
      nbigcol = 2
      nsinglecol <- length(month) ####*2
      
      # ncol <- nsinglecol*nbigcol
      # nrow <- ceiling(nlyr(xvv)/ncol)
      
      width_ <- 522*3*(max(ncol_all,24)+1)/10
      height_ <- 677*max(nrow_all,10)*3/10
      cex <- 0.4 
      cex[ncol_all<3] <- 0.2
      p.strip <- list(cex=cex)
      # names.attr <- as.character(as.Date(names(xvv),format="X%Y.%m.%d"),format="%Y-%m")
      
      y_m_all = str_split(names(xvv),'_')
      names.attr <- rep(NA, length(y_m_all))
      for (i in 1:length(names.attr))
      {
        y_m = y_m_all[[i]][3:4]
        yea = y_m[1]
        mon = y_m[2]
        names.attr[i] = sprintf('%s-%s',yea,mon)
      }
      filename_comprehensive_view <- str_split(filenames[[1]],"_on_",n=2) |> sapply(FUN=function(x){x[[1]]}) |> paste0("_comprehensive.png")
      titlePng <- sprintf("Summary Maps of SPI-%d",spi.scale)
      ####uspi_raster_vis <<- spi_raster_vis
      png(filename_comprehensive_view, width=width_, height=height_, units='px', res=300)
      print(rasterVis::levelplot(xvv, cex=cex,margin=FALSE, main=titlePng,col.regions=cols,par.strip.text=p.strip,layout=c(ncol_all,nrow_all),as.table=TRUE,names.attr=names.attr) + latticeExtra::layer(sp.polygons(spatial_data)))
      (sp.polygons(spatial_data))
      dev.off()
    }
    ###########################
    
    ###
    if (use_ggplot2)
    {
      gga  <- ggplot()+geom_spatraster(data=xvv)+theme_bw()
      gga <-  gga+facet_wrap(~lyr,nrow=nrow_all,ncol=ncol_all)
      gga <-  gga+geom_sf(data=sf,fill=NA,color="black",linewidth=0.15)
      ##  gga <-  gga+ggtitle(it)
      ## gga <- gga+ggplot2::theme(text = ggplot2::element_text(size = 1),axis.text.x=ggplot2::element_text(size = 1),legend.text=ggplot2::element_text(size = 1),legend.direction="vertical",legend.position="right",legend.title = ggplot2::element_blank(),legend.title.align=0)
      gga <- gga+scale_fill_gradient2(high=color_max,low=color_min,mid=color_zero,midpoint=midpoint,na.value=NA,limits=limits)
      
      filename_comprehensive_view <- str_split(filenames[[1]],"_on_",n=2) |> sapply(FUN=function(x){x[[1]]}) |> paste0("_comprehensive.png")
      raster::extension(filename_comprehensive_view) <- raster::extension(filenames[1])
      width_panel <- width
      height_panel <- height
      aspv <- nrow(xvv)/ncol(xvv)
      if (length(width_panel)==0) width_panel <- NA 
      if (is.na(width_panel))  width_panel <- 500/dpi*25.4
      if (length(height_panel)==0) height_panel <- NA 
      if (is.na(height_panel)) {
        height_panel <- width_panel*aspv 
      } else {
        width_panel <- height_panel/aspv 
      }
      
      nrow_all <- nrow_all
      ncol_all <- ncol_all
      width_all2  <- width_panel*ncol_all
      height_all2 <- height_panel*nrow_all
      ###
      
      print(width_all2)
      print(height_all2)
      ggplot2::ggsave(filename=filename_comprehensive_view,plot=gga,width=ceiling(width_all2),height=ceiling(height_all2),limitsize=limitsize,dpi=dpi,units=units,create.dir=create.dir,...) ##... ,units="in",dpi=300) ###,...)
    }
  }
  if (add_spatial_statistics) {
    
    # y <- attr(x,"spi_cat")
    
    ncats <- nrow(terra::levels(y)[[1]])
    
    fun0 <- function(x,ncats=ncats){
      
      o <- array(as.numeric(NA),ncats)
      names(o) <- sprintf("n%03d",as.integer(1:ncats))
      if (!all(is.na(x))) {
        o[] <- 0
        index <- sprintf("n%03d",as.integer(x))
        o2 <- tapply(x,INDEX=index,FUN=length)
        o2 <- o2[names(o2) %in% names(o)]
        o[names(o2)] <- o2[names(o2)]
        o <- o/sum(o,na.rm=TRUE)*100
        
      }
      return(list(o))
    }  
    ###
    if (length(sel_regions)==0) sel_regions=NA
    
    if (!is.na(sel_regions[1])) {
      
      if (is.character(sel_regions)) sel_regions <- which(sf[[id.name]] %in% sel_regions)
      
      sf = sf[sel_regions,]
      
    } 
    
    uuu <- terra::extract(y,y=vect(sf),fun=fun0,ncats=ncats,raw=TRUE)
    # uu1 <- uuu |> as.data.table() |>  melt(id="ID")
    uu1 <- uuu |> as.data.table() |>  data.table::melt(id="ID")
    uu2 <-  lapply(uu1$value,FUN=t) |> lapply(uu1$value,FUN=as.data.table) |> rbindlist()
    uu1$variable <- str_split(as.character(uu1$variable),"[.]") |> sapply(function(x){x[1]})
    
    uu1 <- cbind(uu1 |> select(-3),uu2) 
    
    uu1$ID <- sf[,id.name][[1]][uu1$ID]
    uu1$date <- str_split(uu1$variable,"_",n=3) |> sapply(FUN=function(x){x[[3]]}) |> paste0("_01") ##|> as.Date(format="%Y_%m_%d")
    uu1 <- uu1 |> select(-variable) |> melt(id=c("ID","date"))
    names(uu1)[names(uu1)=="variable"] <- "category_id"
    names(uu1)[names(uu1)=="ID"] <- "toponym"
    names(uu1)[names(uu1)=="value"] <- "area_percentage"
    ###
    clrs <- terra::coltab(y)[[1]][,-1]
    clrs$maxColorValue <- 255
    clrs <- clrs |> apply(MARGIN=1,FUN=as.list) |> lapply(do.call,what=rgb) |> unlist()
    ###
    llrs <- terra::levels(y)[[1]]
    llrs$color <- clrs
    llrs$ID <- sprintf("n%03d",as.integer(llrs$ID))
    names(llrs)[names(llrs)=="ID"] <- "category_id"
    names(llrs)[names(llrs)=="name"] <- "category"
    
    ###
    uu1 <- full_join(uu1,llrs) |> as.data.table()
    ###
    
    file_csv <- str_split(filenames[[1]],"_on_",n=2) |> sapply(FUN=function(x){x[[1]]}) |> paste0("_spatial_stats.csv")
    write.table(uu1,file=file_csv,sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE)
    uu1$date <- as.Date(uu1$date,format="%Y_%m_%d")
    
    attr(out,"spatial_stats") <- uu1
    #####
    
    uuc <- uu1 |> dplyr::select(category,color) |> dplyr::filter(!duplicated(category)) |> dplyr::arrange(category)
    
    uu1m <- uu1 |> dplyr::filter(lubridate::month(.data$date) %in% month)
    pp <- ggplot2::ggplot(data=uu1m, mapping=ggplot2::aes(x=.data$date, y=.data$area_percentage))+ggplot2::geom_col(mapping=ggplot2::aes(colour=.data$category,fill=.data$category))
    pp <- pp+ggplot2::scale_colour_manual(values = uuc$color, breaks = uuc$category, labels = uuc$category)
    pp <- pp+ggplot2::scale_fill_manual(values = uuc$color, breaks = uuc$category, labels = uuc$category)
    pp <- pp+ggplot2::xlab("Time")+ggplot2::ylab("Percentage of Total Area [%]")
    pp <- pp+ggplot2::facet_grid(.data$toponym ~ .)
    pp <- pp+ggplot2::theme(text = ggplot2::element_text(size = 30),axis.text.x=ggplot2::element_text(size = 10),legend.text=ggplot2::element_text(size = 20),legend.direction="horizontal",legend.position="top",legend.title = ggplot2::element_blank(),legend.title.align=0)
    ##  pp <- pp+ggplot2::scale_x_date(breaks = breaks_time,labels=labels_time)
    if (all(1:12 %in% month)) {
      monsel <- "all months"
    } else {
      monsel <-  base::months(as.Date("1990-01-01")+months(0:11)) |> str_sub(1,1)
      monsel <- monsel[month] |> paste(collapse="")
    }
    pp <- pp+ggplot2::ggtitle(sprintf("Area %% affected per anomaly intensity class based on SPI- %2d  (%s)",spi.scale,monsel))
    
    
    #   
    #   
    #file_csv <- str_split(filenames[[1]],"_on_",n=2) |> sapply(FUN=function(x){x[[1]]}) |> paste0("_spatial_stats.csv")
    file_area_png <- file_csv
    raster::extension(file_area_png) <- ""
    ####
    file_area_png <- file_area_png |> paste0(sprintf("_spi%02d_cat_percentage_area_%s.png",spi.scale,monsel))
    region_names=unique(uu1$toponym)
    raster::extension(file_area_png) <- raster::extension(filenames[1]) ## EC 20250703 
    ggplot2::ggsave(filename=file_area_png,plot=pp,width=297*2,height=ceiling(210*length(region_names)/2),units="mm",limitsize=FALSE,create.dir = create.dir)  ## A4 210 * 297
    
    attr(out,"spatial_stats_plot") <- pp
    
  }
  
  #######SPI categories maps
  
  if (add_spi_cat_maps) for (it in names(y)) {
    
    if(use_ggplot2)
    {
      gg  <- ggplot()+geom_spatraster(data=y[[it]])+theme_bw()
      gg <-  gg+geom_sf(data=sf,fill=NA,color="black",linewidth=0.15)
      gg <- gg+ggtitle(it)
      
      filename_cat=str_replace_all(filenames[it]," ","_")
      raster::extension(filename_cat) <- ""
      filename_cat <- paste0(filename_cat,"_cat.jpg")
      raster::extension(filename_cat) <- raster::extension(filenames[it])
      ggsave(filename=filename_cat,plot=gg,width=width,height=height,limitsize=limitsize,dpi=dpi,units=units,create.dir=create.dir,...)
      
    }else if(use_levelplot)
    {
      
      yy = y[[it]]
      
      if(is.na(max(yy[],na.rm = TRUE)) || is.infinite(max(yy[],na.rm = TRUE))) next

      filename_cat <- file.path(dirname(filenames[it]),'png',
                basename(filenames[it])|> str_replace_all(" ","_")) |> str_replace(".jpg","_cat.png")
      
      
      titlePng <- ggtitle(it)[[1]]

      png(filename_cat, width=522*3, height=677*3, units='px', res=dpi)
      print(rasterVis::levelplot(yy, margin=FALSE, main=titlePng,col.regions=col) + latticeExtra::layer(sp.polygons(spatial_data)))
      dev.off()
    }

    if (write_tif) {
      
      filename_tif <- file.path(dirname(filenames[it]),'tif',
                basename(str_replace_all(filenames[it]," ","_"))) |> str_replace(".jpg","_cat.tif")
      
      writeRaster(y[[it]],filename=filename_tif,overwrite=TRUE)
      
    }
    attr(out,"spi_cat_gg") <- filenames
  }
  
  #######
  
  if (write_shp) {
    file.path(dirname(filenames)[1],"shp") |> dir.create()
    ## TO IMPLEMENT 
    # sfp <- sf[sel_regions,]
    names(sf)[names(sf)==id.name] <- "toponym"
    shpout <- sf |> full_join(attr(out,"spatial_stats")) 
    ####
    catids <- unique(shpout$category) 
    names(catids) <- str_replace_all(catids," ","_")
    ####
    print(file_area_png)
    file_area_shp <- file_area_png |> str_split("_cat_percentage_area_")
    file_area_shp <- file_area_shp[[1]][1] |> paste0("_%s.shp")
    file_area_shp = file.path(dirname(filenames[it]),'shp',basename(file_area_shp))
    print(file_area_shp)
    for (it in names(catids)) {
      shp_file <- file_area_shp |> sprintf(it)
      shp_df <- shpout[shpout$category==catids[it],]
      st_write(shp_df,dsn=shp_file,delete_layer=TRUE)
      
    }
    
    
    attr(out,"spatial_stats_shp") <- shpout
    }
  
  return(out)
  
}

dryspellapprastviz_enexus <- function(x,filenames,sf,settings=system.file("settings/lm_plot_settings_enexus.xml",package="terraclivaviz"),fun_aggr=terracliva::aggr_fun_suffixes(),summary_suffixes=c("pvalue","coeff","stdrerror","rsquared","senslope","pvalue_mk"),mask=FALSE,write_tif=FALSE,
                                      use_levelplot=FALSE,use_ggplot2=!use_levelplot,device="png",...){
  
  ## TO DO 
  #### https://en.wikipedia.org/wiki/Data_and_information_visualization
  ##stop("FUNCTION_TO_DO")
  code_fun <- "dryspell"
  if (mask==TRUE){ 
    x <- mask(x,mask=vect(sf)) ## added on 2024 10 04
    EE = ext(vect(sf))
    x <- crop(x,EE)
  }
  if (is.character(settings)) {
    xml_settings <- settings
    settings <- read_xml(xml_settings)
    settings <- xml_children(settings) |> lapply(FUN=xml_children)
    settings_list <- list()
    for (i in 1:length(settings)) {
      settings_list[[i]] <- xml_text(settings[[i]])
      names(settings_list[[i]]) <- xml_name(settings[[i]])
      
    }
    code_funs <- sapply(settings_list,FUN=function(x){x[["code_fun"]]})
    settings_list <- settings_list[which(code_funs==code_fun)]
    names(settings_list) <- sapply(settings_list,FUN=function(x){x[["particular_id"]]})
    
    
  }
  
  
  
  ### settings 
  nn <- names(x)
  nn2 <- nn
  
  for (itf in rev(sort(fun_aggr))) {
    
    nn2 <- str_replace(nn2,itf,"")
  }
  nn2 <- str_replace(nn2,"_","")
  
  iregress <- which(nn2 %in% summary_suffixes)
  iyear <- -iregress
  
  nn2[iyear] <- "year"
  nn2[iregress] <- "regress"
  
  
  names(nn2) <- names(x)
  spatial_data <<- sf |> as_Spatial()
  
  if (length(filenames)==1) filenames <- sprintf(filenames,names(x))
  names(filenames) <- names(x)
  for (it in names(x)) {
    # print(it)
    
    it_vec = strsplit(it,"_")[[1]]
    if(it_vec[-(1:(length(it_vec)-2))]|> paste(collapse = '_')== "pvalue_mk")
    {
      it_dir = it_vec[1:(length(it_vec)-2)] |> paste(collapse = '_')
    }else{
      it_dir = it_vec[-length(it_vec)] |> paste(collapse = '_')
    }
    it_path = file.path(dirname(filenames)[1],it_dir)
    if(!dir.exists(it_path)) dir.create(it_path)
    
    
    if(length(which(!is.na(x[[it]][]))) == 0) next
    out_title = plot_title(it,settings_list[[nn2[it]]][["id"]])
    ####it2 <<- it
    if (use_ggplot2) {
      gg  <- ggplot()+geom_spatraster(data=x[[it]])+theme_bw()
      gg <-  gg+geom_sf(data=sf,fill=NA,color="black",linewidth=0.15)
      # gg <- gg+ggtitle(it)
      gg <- gg+ggtitle(out_title)
      
      colorscale <- settings_list[[nn2[it]]][["colorscale"]]
      
      rev <- as.numeric(settings_list[[nn2[it]]][["rev"]])
      colors <-   colorRampPalette(brewer.pal(9,colorscale))(9)
      if (rev<0) colors <- rev(colors)
      print(colors)
      gg <- gg+scale_fill_gradientn(colors=colors,na.value=NA)
      # filename=str_replace_all(filenames[it]," ","_")
      filename=file.path(it_path,basename(filenames[it]) |> str_replace_all(" ","_"))
      ggsave(filename=filename,plot=gg,...)
    } else if (use_levelplot) {
      
      # Supponiamo che 'x' sia una lista di RasterLayer e 'it' sia l'indice corrente
      raster_layer <- x[[it]]
      # Supponiamo che 'sf' sia un oggetto Spatial
      # spatial_data <<- sf |> as_Spatial()
      
      # Supponiamo che 'settings_list' e 'nn2' siano liste di impostazioni
      colorscale <- settings_list[[nn2[it]]][["colorscale"]]
      rev <- as.numeric(settings_list[[nn2[it]]][["rev"]])
      colors <- colorRampPalette(brewer.pal(9, colorscale))
      if (rev < 0) colors <- rev(colors)
      # Creazione del plot con rasterVis::levelplot
      # plot <- levelplot(raster_layer, col.regions = colors, margin=FALSE,main = it) +
      #   latticeExtra::layer(sp::sp.polygons(spatial_data, col = "black", lwd = 0.15))
      plot <- levelplot(raster_layer, col.regions = colors, margin=FALSE,main = out_title) +
        latticeExtra::layer(sp::sp.polygons(spatial_data))
      
      # Salvataggio del plot
      # filename=str_replace_all(filenames[it]," ","_")
      filename=file.path(it_path,basename(filenames[it])|> str_replace_all(" ","_"))
      raster::extension(filename) <- ".%s" |> sprintf(device)
      trellis.device(device = device, filename = filename,...)
      # trellis.device(device = device, filename = filename,width=2800, height=1500, units='px', res=300)
      print(plot)
      dev.off()
    }
    if (write_tif) {
      
      filename_tif <- filename
      raster::extension(filename_tif) <- ".tif"
      writeRaster(x[[it]],filename=filename_tif,overwrite=TRUE)
      
    }
    
  }
  
  out <- filenames
  
  return(out)
  
}

plot_title <- function(file_id,type_id,t_name = NULL,unit = NULL,is_mon = FALSE)
{
  mon = NULL
  
  if(type_id == "lm_exc_rt")
  {
    params = str_split(file_id,'_exc_rt_')[[1]]  #RT years, distrib.
    
    if(is_mon)
    {
      mon = str_split(params[1],'_')[[1]][1]
      params = c(str_split(params[1],'_')[[1]][2],params[2])
    }
    
    title = sprintf("RP %i %s Excess [%s] (using %s)",
                    params[2] |> as.integer(),t_name,unit,params[1])
  }
  else if(type_id == "lm_def_rt")
  {
    params = str_split(file_id,'_def_rt_')[[1]]
    
    if(is_mon)
    {
      mon = str_split(params[1],'_')[[1]][1]
      params = c(str_split(params[1],'_')[[1]][2],params[2])
    }
    
    title = sprintf("RP %i %s Deficit [%s] (using %s)",
                    params[2] |> as.integer(),t_name,unit,params[1])
  }
  else if(type_id == "lm_exc_p_rt")
  {
    params = str_split(file_id,'_exc_perc_rt_')[[1]]
    
    if(is_mon)
    {
      mon = str_split(params[1],'_')[[1]][1]
      params = c(str_split(params[1],'_')[[1]][2],params[2])
    }
    
    title = sprintf("RP %i %s Excess [%% of mean] (using %s)",
                    params[2] |> as.integer(),t_name,params[1])
  }
  else if(type_id == "lm_def_p_rt")
  {
    params = str_split(file_id,'_def_perc_rt_')[[1]]
    
    if(is_mon)
    {
      mon = str_split(params[1],'_')[[1]][1]
      params = c(str_split(params[1],'_')[[1]][2],params[2])
    }
    
    title = sprintf("RP %i %s Deficit [%% of mean] (using %s)",
                    params[2] |> as.integer(),t_name,params[1])
  }
  else if(type_id == "lm_exc_pct")
  {
    params = str_split(file_id,'_rt_of_exc_')[[1]]
    
    title = sprintf("Return Period [years] for %s excess of %i%% (using %s)",
                    t_name,params[2] |> as.integer(),params[1])
  }
  else if(type_id == "lm_def_pct")
  {
    params = str_split(file_id,'_rt_of_def_')[[1]]
    
    if(is_mon)
    {
      mon = str_split(params[1],'_')[[1]][1]
      params = c(str_split(params[1],'_')[[1]][2],params[2])
    }
    
    title = sprintf("Return Period [years] for %s deficit of %i%% (using %s)",
                    t_name,params[2] |> as.integer(),params[1])
  }
  
  
  else if(type_id == "lm_distrib") 
  {
    params = str_split(file_id,'_')[[1]]
    
    if(is_mon)
    {
      mon = params[1]
      params = params[-1]
    }
    
    if (length(params)>2) params[2] = paste(params[2:length(params)],collapse = ' ')
    
    title = sprintf("%s (using %s)",
                    params[2] |> str_to_title(),params[1])
  }
  else if(type_id == "hwmid_year") 
  {
    title = sprintf("HWMId - year %s",file_id)
  }
  else if(type_id == "hwmid_regress") 
  {
    # title = substring(file_id,7) |> str_to_sentence()
    # title =   sprintf("HWMId - %s",substring(file_id,7)) |> str_to_sentence()
    title = substring(file_id,7) |> str_to_sentence() |> sprintf(fmt = "HWMId - %s")
  }
  else if(type_id == "dryspell_year") 
  {
    title = sprintf("%s - %s",
                    file_id |> substr(1,str_length(file_id)-5) |> str_replace_all("_"," ") |> str_to_sentence(),
                    file_id |> substr(str_length(file_id)-3,str_length(file_id)))
  }
  else if(type_id == "dryspell_regress") 
  {
    title = file_id |> str_replace_all("_"," ") |> str_to_sentence()
  }
  else title = ""
  
  return(title)
}