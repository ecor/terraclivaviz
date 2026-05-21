NULL
#' 
#' SPI / SPEI index  (see lmomPi implementation)  Climate Variability Analysis in Spatial Gridded Coverage (visualization)
#' 
#'
#' @param x a \code{SpatRast-Class} object returned by \code{\link{spiapprast}}. 
#' @param sf an \code{sf} object which will be added to the plotted maps. 
#' @param filenames vector or string for names of the output files (plots) .
#' @param settings xml files for plotting settings (see internal code).
#' @param mask logical If it is \code{TRUE} only the area within the \code{sf} shape is visualized. Default is \code{FALSE}.
#' @param signif significance , used to detect trend occurrence, see \code{\link{spiapprast}}.
#' @param write_tif logical. Default is \code{FALSE}. If \code{TRUE}, results are also written and saved as GeoTiff raster files.
#' @param add_spatial_statistics  logical. If \code{TRUE} spatial statistics are calculated on the areas/polygons of \code{sf} geospatial vector object.
#' @param id.name id name in \code{sf} for each areas/polygons.
#' @param sel_regions selected regions/areas/polygons to display (optional), otherwise all regionsare displayed.
#' @param month months to be selected for spatial stastics.
#' @param spi.scale integer value. Default is 1 , it is a property of \code{x}. See \link[terracliva]{spicliva}. 
#' @param spi.classes data frame with SPI/SPEI classes (see default csv file) 
#' @param add_comprehensive_view logical . If \code{TRUE} a comprehensive plot with panels of raster maps is presented. 
#' @param add_spi_cat_maps logical. If \code{TRUE} spi categorical maps are displayed.
#' @param write_shp logical . 
#' @param nrow_all,ncol_all number of rows and columns for frames for the comprehensive panel
#' @param width_panel,height_panel width and height of a single panel within a comprehensive (frame) plot.
#' @param width,height,limitsize,dpi,units,create.dir,... further arguments passed to \code{\link[ggplot2]{ggsave}} (see defailt values in function usage)
#'
#' 
#' @export
#'
#' @note \code{x} must have the proper time aggregation for the analysis before the execution of this function.
#' 
#' @importFrom stringr str_replace_all
#' @importFrom terra writeRaster nlyr classify coltab<-
#' @importFrom ggplot2 scale_fill_gradient2 facet_wrap
#' @importFrom rlang .data
#' @importFrom data.table as.data.table rbindlist melt
#' @importFrom dplyr select filter arrange full_join
#' @importFrom terra extract levels coltab time<- time 
#' @importFrom grDevices rgb
#' @importFrom utils write.table
#' @importFrom stringr str_trim str_split
#' @importFrom magrittr %>%
#' @importFrom raster extension extension<-  
#' @importFrom sf st_write
#' @importFrom utils read.table
#' 
#' @examples 
#' 
#' library(magrittr)
#' library(terracliva)
#' library(sf)
#' library(terra)
#' 
#' years <- 1982:2023
#
#' dataset_monthly <- system.file("ext_data/mekrou/CHIRPS_StackMekrou.nc",package="terraclivaviz")
#' dataset_monthly <- rast(dataset_monthly)+0
#' 
#' 
#' dataset_sf <- system.file("ext_data/mekrou/Mekrou_AOI/Mekrou_AOI_v3.shp",
#' package="terraclivaviz") %>% 
#'  st_read()
#' 
#' outspi <- spiapprast(x=dataset_monthly,distrib="pe3",summary_regress=TRUE,add_cat=FALSE,spi.scale=1)
#' 
#' 
#' filenames <- system.file(package="terraclivaviz") %>% file.path("examples/plot/spi/spi_mekrou_%s.jpg")
#'  
#' out <- spiapprastviz(x=outspi,filenames=filenames,
#'  sf=dataset_sf,signif=0.1,
#'  write_tif=TRUE,month=6:10,dpi=300,limitsize=FALSE,units="mm",add_spatial_statistics=TRUE)
#'
#' filenames2 <- system.file(package="terraclivaviz") %>% file.path("examples/plot/spi_regions/spi_mekrou_%s.jpg")
#'  
#' out2 <- spiapprastviz(x=outspi,filenames=filenames2,
#'  sf=dataset_sf,signif=0.1,
#'  write_tif=TRUE,month=6:10,dpi=300,limitsize=FALSE,units="mm",add_spatial_statistics=TRUE,sel_regions=c(1,2))
#'
#'
#'







spiapprastviz <- function(x,filenames,sf,settings=system.file("settings/lm_plot_settings_enexus.xml",package="terraclivaviz"),signif=attr(x,"signif"),mask=FALSE,write_tif=FALSE,add_spatial_statistics=!is.null(attr(x,"spi_cat")),add_spi_cat_maps=add_spatial_statistics,id.name="NAME",sel_regions=NA,month=1:12,add_comprehensive_view=TRUE,nrow_all=NULL,ncol_all=length(month), width = NA,
                          height = NA, width_panel = width,
                          height_panel = height,limitsize=FALSE,dpi=300,units="mm",create.dir=TRUE,write_shp=TRUE,spi.scale=1,
                          spi.classes=read.table(system.file("settings/spi_class.csv",package="terracliva"),header=TRUE,sep=",",comment.char="?"),...){
  
  ## TO DO 
  #### https://en.wikipedia.org/wiki/Data_and_information_visualization
  if (mask==TRUE){ 
    x <- mask(x,mask=vect(sf)) ## added on 2024 10 04
    EE = ext(vect(sf))
    x <- crop(x,EE)
  }
  code_fun <- "spi"
  spi.scale0 <- attr(x,"spi.scale")
  if (length(spi.scale0)==0) spi.scale0 <- as.numeric(NA)
  if (!is.na(spi.scale0)) spi.scale <- spi.scale0
  
  
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
 
  
  if (length(filenames)==1) filenames <- sprintf(filenames,names(x))
  names(filenames) <- names(x)
  #####   SPIAPPRASTVIZ insert here a unnique_plot TO DO 
  #####
  
  
 for (it in names(x)) {
    
    
    
    
    
    gg  <- ggplot()+geom_spatraster(data=x[[it]])+theme_bw()
    gg <-  gg+geom_sf(data=sf,fill=NA,color="black",linewidth=0.15)
    gg <- gg+ggtitle(it)
    print(settings_list)
    colorscale <- settings_list[[nn2[it]]][["colorscale"]] |> str_split(",") |> unlist()
    colors <- colorRampPalette(brewer.pal(9,colorscale))(9) |> try(silent=TRUE) 
    if (inherits(colors,"try-error")) colors <- colorscale
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
   
    if (rev<0) colors <- rev(colors)
   
    if (colors[1]!="") {
      print(1)
      gg <- gg+scale_fill_gradientn(colors=colors,na.value=NA) 
    } else {
    midpoint=as.numeric(value_zero)
    
    
    gg <- gg+scale_fill_gradient2(high=color_max,low=color_min,mid=color_zero,midpoint=midpoint,na.value=NA,limits=limits)
    }
    filename=str_replace_all(filenames[it]," ","_")
    ggsave(filename=filename,plot=gg,width=width,height=height,limitsize=limitsize,dpi=dpi,units=units,create.dir=create.dir,...)
    
    if (write_tif) {
      
      filename_tif <- filename
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
    midpoint <- as.numeric(value_zero) 
    limits <- c(value_min,value_max)
    print(midpoint)
    print(limits)
    ###
    gga  <- ggplot()+geom_spatraster(data=xvv)+theme_bw()
    gga <-  gga+facet_wrap(~lyr,nrow=nrow_all,ncol=ncol_all)
    gga <-  gga+geom_sf(data=sf,fill=NA,color="black",linewidth=0.15)
   ##  gga <-  gga+ggtitle(it)
   ## gga <- gga+ggplot2::theme(text = ggplot2::element_text(size = 1),axis.text.x=ggplot2::element_text(size = 1),legend.text=ggplot2::element_text(size = 1),legend.direction="vertical",legend.position="right",legend.title = ggplot2::element_blank(),legend.title.align=0)
    gga <- gga+scale_fill_gradient2(high=color_max,low=color_min,mid=color_zero,midpoint=midpoint,na.value=NA,limits=limits)
   
    filename_comprehensive_view <- str_split(filenames[[1]],"_on_",n=2) |> sapply(FUN=function(x){x[[1]]}) |> paste0("_comprehenive.png")
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
    ###
    # print(xvv)
    # print(aspv)
    # print(nrow_all)
    # print(ncol_all)
    # print(width_panel)
    # print(height_panel)
    
    nrow_all <- nrow_all
    ncol_all <- ncol_all
    width_all2  <- width_panel*ncol_all
    height_all2 <- height_panel*nrow_all
    ###
    
    print(width_all2)
    print(height_all2)
    ggplot2::ggsave(filename=filename_comprehensive_view,plot=gga,width=ceiling(width_all2),height=ceiling(height_all2),limitsize=limitsize,dpi=dpi,units=units,create.dir=create.dir,...) ##... ,units="in",dpi=300) ###,...)
    
    ##ggplot2::ggsave(filename=filename_comprehensive_view,plot=gga,width=ceiling(width_all2),height=ceiling(height_all2),limitsize=FALSE,dpi=dpi,...) units="in",dpi=300) ###,...)
    
    
    
  }
  if (add_spatial_statistics | add_spi_cat_maps) {
      
      y <- attr(x,"spi_cat")
      ###
      if (is.null(y)) {
        
        
        ## built y 
        spi.classes$ID <- 1:nrow(spi.classes)
        outb <- x[[str_detect(names(x),"_on_")]]
        rcl <- as.matrix(spi.classes[,c("min","max","ID")])
        outc <- classify(outb,rcl=rcl[,c(1,2,3)],include.lowest=TRUE)
        
        for (ii in 1:nlyr(outc)) {  
          
          coltab(outc,layer=ii) <- spi.classes[,c("ID","color")]
          levels(outc[[ii]]) <- spi.classes[,c("ID","name","color")]
        }
        names(outc) <- names(outb)
        terra::time(outc) <- names(outb) |> str_split("_on_") |> sapply(function(p){p[[2]]}) |> paste0("_01") |> as.Date(format="%Y_%m_%d") ## EC 20250606
      
        
        y <- outc
        ###attr(out,"spi_clasess")  <- spi.classes
        
      }
      
      ###
      
  }
      
      #### PRINT SPI CATEGORIES 
  if (add_spi_cat_maps) for (it in names(y)) {
        
        
        
        
        
        gg  <- ggplot()+geom_spatraster(data=y[[it]])+theme_bw()
        gg <-  gg+geom_sf(data=sf,fill=NA,color="black",linewidth=0.15)
        gg <- gg+ggtitle(it)
        # print(settings_list)
        # colorscale <- settings_list[[nn2[it]]][["colorscale"]] |> str_split(",") |> unlist()
        # colors <- colorRampPalette(brewer.pal(9,colorscale))(9) |> try(silent=TRUE) 
        # if (inherits(colors,"try-error")) colors <- colorscale
        # ####print("nn2:")
        # ####print(nn2)
        # print("it:")
        # print(it)
        # 
        # print("colorscale:")
        # print(colorscale)
        # ###
        # color_max <- settings_list[[nn2[it]]][["color_max"]] |> str_trim()
        # color_min <- settings_list[[nn2[it]]][["color_min"]]  |> str_trim()
        # color_zero <- settings_list[[nn2[it]]][["color_zero"]]  |> str_trim()
        # ###
        # value_max <- settings_list[[nn2[it]]][["value_max"]] 
        # value_min <- settings_list[[nn2[it]]][["value_min"]] 
        # value_zero <- settings_list[[nn2[it]]][["value_zero"]] 
        
        # if (value_zero=="signif") value_zero <- signif
        
        
        # limits <- c(as.numeric(value_min),as.numeric(value_max))
        # midpoint <- as.numeric(value_zero)
        # 
        # 
        # rev <- as.numeric(settings_list[[nn2[it]]][["rev"]])
        # 
        # if (rev<0) colors <- rev(colors)
        # 
        # if (colors[1]!="") {
        #   print(1)
        #   gg <- gg+scale_fill_gradientn(colors=colors,na.value=NA) 
        # } else {
        #   midpoint=as.numeric(value_zero)
        #   
        #   
        #   gg <- gg+scale_fill_gradient2(high=color_max,low=color_min,mid=color_zero,midpoint=midpoint,na.value=NA,limits=limits)
        # }
        filename_cat=str_replace_all(filenames[it]," ","_")
        raster::extension(filename_cat) <- ""
        filename_cat <- paste0(filename_cat,"_cat.jpg")
        raster::extension(filename_cat) <- raster::extension(filenames[it])
        ggsave(filename=filename_cat,plot=gg,width=width,height=height,limitsize=limitsize,dpi=dpi,units=units,create.dir=create.dir,...)
        
        if (write_tif) {
          
          filename_tif <- filename_cat
          raster::extension(filename_tif) <- ".tif"
          writeRaster(y[[it]],filename=filename_tif,overwrite=TRUE)
          
        }
        attr(out,"spi_cat_gg") <- filenames
      }
      
      
        
        
      ####
      
      
      ####
      if (add_spatial_statistics) {
      ncats <- nrow(terra::levels(y)[[1]])
      
      
  ##    for (i in terra::levels(y)[[1]]) {
        
        ###
        fun0 <- function(x,ncats=ncats){
          
          print(x)
          print(ncats)
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
          print(o)
          return(list(o))
          }  
        ###
        if (length(sel_regions)==0) sel_regions=NA
        
        if (!is.na(sel_regions[1])) {
          
          
          if (is.character(sel_regions)) sel_regions <- which(sf[,id.name] %in% sel_regions)
            
            
        } else {
            
            sel_regions <- TRUE
        }
          
          
        
        
        
       
        uuu <- terra::extract(y,y=vect(sf[sel_regions,]),fun=fun0,ncats=ncats,raw=TRUE)
        uu1 <- uuu |> as.data.table() |> melt(id="ID")
        uu2 <-  lapply(uu1$value,FUN=t) |> lapply(uu1$value,FUN=as.data.table) |> rbindlist()
        uu1$variable <- str_split(as.character(uu1$variable),"[.]") |> sapply(function(x){x[1]})
        
        uu1 <- cbind(uu1 |> select(-3),uu2) 
        print("uu1")
        print(uu1)
        print(id.name)
        uu1$ID <- sf[,id.name][[1]][uu1$ID]
        uu1$date <- str_split(uu1$variable,"_",n=3) |> sapply(FUN=function(x){x[[3]]}) |> paste0("_01") ##|> as.Date(format="%Y_%m_%d")
        uu1 <- uu1 |> select(-.data$variable) |> melt(id=c("ID","date"))
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
       ## uu1$value[is.na(uu1$value)] <- list(array(NA,ncats))
        ######
        
       
        
        
    ##    yi <- (y==i)
        
        
    ##  }
      
      
      attr(out,"spatial_stats") <- uu1
      ## SPATTIAL STATISTIC 
      
      uuc <- uu1 |> dplyr::select(.data$category,.data$color) |> dplyr::filter(!duplicated(.data$category)) |> dplyr::arrange(.data$category)
      
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
      
      if (write_shp) {
        
        ## TO IMPLEMENT 
        sfp <- sf[sel_regions,]
        names(sfp)[names(sfp)==id.name] <- "toponym"
        shpout <- sfp |> full_join(attr(out,"spatial_stats")) 
        ####
        catids <- unique(shpout$category) 
        names(catids) <- str_replace_all(catids," ","_")
        ####
        print(file_area_png)
        file_area_shp <- file_area_png |> str_split("_cat_percentage_area_")
        file_area_shp <- file_area_shp[[1]][1] |> paste0("_%s.shp")
        
        for (it in names(catids)) {
          shp_file <- file_area_shp |> sprintf(it)
          shp_df <- shpout[shpout$category==catids[it],]
          st_write(shp_df,dsn=shp_file,delete_layer=TRUE)
          
        }
        
        
        
        attr(out,"spatial_stats_shp") <- shpout
        
        
        
        ###
        
        
        
      }
    
    
    
    
  }
  
  
  
  
  
  
  return(out)
  
}
