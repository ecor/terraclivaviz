NULL
#' 
#' Heat and Cold waves analysis in Spatial Gridded Coverage (visualization)
#' 
#'
#' @param x a \code{SpatRast-Class} object returned by \code{\link{hwmidapprast}}. 
#' @param sf an \code{sf} object which will be added to the plotted maps. 
#' @param filenames vector or string for names of the output files (plots) 
#' @param settings xml files for plotting settings (see internal code)
#' @param mask logical If it is \code{TRUE} only the area within the \code{sf} shape is visualized. Default is \code{FALSE}
#' @param write_tif logical. Default is \code{FALSE}. If \code{TRUE}, results are also written and saved as GeoTiff raster files.
#' @param add_comprehensive_view logical . If \code{TRUE} a comprehensive plot with panels of raster maps is presented. 
#' @param nrow_all,ncol_all number of rows and columns for frames for the comprehensive panel
#' @param width_panel,height_panel width and height of a single panal within a comprehensive (frame) plot.
#' @param width,height,limitsize,dpi,units,create.dir,... further arguments passed to \code{\link[ggplot2]{ggsave}} (see defailt values in function usage)
#' 
#' @export
#'
#' @note \code{x} must have the proper time aggregation for the analysis before the execution of this function.
#' 
#' @importFrom stringr str_replace_all
#' @importFrom terra writeRaster
#' 
#' @examples
#' 
#' library(sf)
#' years <- 1983:2016
#' tmax_dataset_path <- system.file("ext_data/tmax",package="terracliva")
#' tmax_dataset_daily <- "%s/daily/chirts_daily_goma_tmax_%04d.grd" %>% 
#' sprintf(tmax_dataset_path,years) %>% rast()
#' dataset_sf <- system.file("ext_data/OSM_Goma_quartiers_210527.shp"
#' ,package="terracliva") %>% st_read()
#' 
#' 
#' o_hw_regress <- hwmidapprast(tmax_dataset_daily,summary_regress=TRUE)
#'
#' filenames <- system.file(package="terraclivaviz") %>% 
#'   file.path("examples/plot/hwmid/hwmid_%s.jpg")
#' ###filenames <- "/home/ecor/local/rpackages/jrc/terraclivaviz_examples/examples/plot/hwmid/hwmid_%s.jpg"
#' out_hw_regress_viz <- hwmidapprastviz(x=o_hw_regress,filenames,sf=dataset_sf)
#' 
#' filenames <- system.file(package="terraclivaviz") %>% 
#'    file.path("examples/plot/hwmid/hwmid_svg_printing_%s.svg")
#' 
#' out_hw_regress_svg_viz <- hwmidapprastviz(x=o_hw_regress,filenames,sf=dataset_sf)
#' 
#' 


hwmidapprastviz <- function(x,filenames,sf,settings=system.file("settings/lm_plot_settings_enexus.xml",package="terraclivaviz"),mask=FALSE,write_tif=FALSE,nrow_all=NULL,ncol_all=6, width = NA,height = NA, width_panel = width,
height_panel = height,limitsize=FALSE,dpi=300,units="mm",
  create.dir=TRUE,add_comprehensive_view=TRUE,...){
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
  
  
  for (it in names(x)) {
    ####it2 <<- it
    value_min <- settings_list[[nn2[it]]][["value_min"]] |> as.numeric()
    value_max <- settings_list[[nn2[it]]][["value_max"]] |> as.numeric()
    limit <- c(value_min,value_max)
    if (any(is.na(limit))) limit <- NULL
    ####
    gg  <- ggplot()+geom_spatraster(data=x[[it]])+theme_bw()
    gg <-  gg+geom_sf(data=sf,fill=NA,color="black",linewidth=0.15)
    gg <- gg+ggtitle(it)
    
    colorscale <- settings_list[[nn2[it]]][["colorscale"]]
    print(colorscale)
    rev <- as.numeric(settings_list[[nn2[it]]][["rev"]])
    colors <-   colorRampPalette(brewer.pal(9,colorscale))(9)
    if (rev<0) colors <- rev(colors)
    print(colorscale)
    gg <- gg+scale_fill_gradientn(colors=colors,na.value=NA,limit=limit)
    filename=str_replace_all(filenames[it]," ","_")
    ggsave(filename=filename,plot=gg,create.dir=create.dir,dpi=dpi,height=height,width=width,limitsize=limitsize,...)
    
    if (write_tif) {
      
      filename_tif <- filename
      raster::extension(filename_tif) <- ".tif"
      writeRaster(x[[it]],filename=filename_tif,overwrite=TRUE)
      
    }
    
  }
  ### TO DO 20250711
  if (add_comprehensive_view) {
    
    
    mm <- !str_detect(names(x),"hwmid_") 
    xvv <- x[[mm]]
    terra::time(xvv) <- names(xvv) |> paste0("_12_31") |> 
      as.Date(format="%Y_%m_%d")
    if (is.null(ncol_all)) ncol_all <- NA
    if (is.null(nrow_all)) nrow_all <- NA 
    ####
    if (is.na(ncol_all))  ncol_all <- 2 
    if (is.na(nrow_all))  {
      nrow_all <- ceiling(nlyr(xvv)/ncol_all)
    } else {
      ncol_all <- ceiling(nlyr(xvv)/nrow_all)
      
    }
    
    #### EDIT COLOR SCALE
    ####
    ###
    it <- 1 
    colorscale <- settings_list[[nn2[it]]][["colorscale"]]
    print("ba")
    print(colorscale)
    rev <- as.numeric(settings_list[[nn2[it]]][["rev"]])
    colors <-   colorRampPalette(brewer.pal(9,colorscale))(9)
    if (rev<0) colors <- rev(colors)
    print("here__")
    print(colorscale)
    print(rev)
    
    # color_max <- settings_list[[nn2[it]]][["color_max"]] |> str_trim()
    # color_min <- settings_list[[nn2[it]]][["color_min"]]  |> str_trim()
    # color_zero <- settings_list[[nn2[it]]][["color_zero"]]  |> str_trim()
    ###
    value_max <- settings_list[[nn2[it]]][["value_max"]] |> as.numeric()
    value_min <- settings_list[[nn2[it]]][["value_min"]] |> as.numeric()
    value_zero <- settings_list[[nn2[it]]][["value_zero"]] |> as.numeric()
    ####
    midpoint <- value_zero 
    limits <- c(value_min,value_max)
    print(midpoint)
    print(limits)
    ###
    gga  <- ggplot()+geom_spatraster(data=xvv)+theme_bw()
    gga <-  gga+facet_wrap(~lyr,nrow=nrow_all,ncol=ncol_all)
    gga <-  gga+geom_sf(data=sf,fill=NA,color="black",linewidth=0.15)
    ##  gga <-  gga+ggtitle(it)
    ## gga <- gga+ggplot2::theme(text = ggplot2::element_text(size = 1),axis.text.x=ggplot2::element_text(size = 1),legend.text=ggplot2::element_text(size = 1),legend.direction="vertical",legend.position="right",legend.title = ggplot2::element_blank(),legend.title.align=0)
    ####gga <- gga+scale_fill_gradient2(high=color_max,low=color_min,mid=color_zero,midpoint=midpoint,na.value=NA,limits=limits)
    gga <- gga+scale_fill_gradientn(colors=colors,na.value=NA,limit=limit)
    ####filename_comprehensive_view=str_replace_all(filenames[it]," ","_")
    print("here__")
    print(colorscale)
    print(rev)
    filename_comprehensive_view <- filenames[it] ###str_split(filename_comprehensive_view,"_",n=2) |> sapply(FUN=function(x){x[[1]]}) |> paste0("_hwmid_comprehenive.png")
    raster::extension(filename_comprehensive_view) <- "" ###raster::extension(filenames[1])
    filename_comprehensive_view <-   filename_comprehensive_view |> paste0("_hwmid_comprehenive.png")
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
    
    ###
    print(filename_comprehensive_view)
    ###
    ggplot2::ggsave(filename=filename_comprehensive_view,plot=gga,width=ceiling(width_all2),height=ceiling(height_all2),limitsize=limitsize,dpi=dpi,units=units,create.dir=create.dir,...) ##... ,units="in",dpi=300) ###,...)
    
    ##ggplot2::ggsave(filename=filename_comprehensive_view,plot=gga,width=ceiling(width_all2),height=ceiling(height_all2),limitsize=FALSE,dpi=dpi,...) units="in",dpi=300) ###,...)
    
    
    
  }
  
  ###
  out <- filenames

  return(out)
  
}
