NULL
#' 
#' SPI / SPEI index  (see lmomPi implementation)  Climate Variability Analysis in Spatial Gridded Coverage (visualization)
#' 
#'
#' @param x a \code{SpatRast-Class} object returned by \code{\link{siapprast}}. 
#' @param sf an \code{sf} object which will be added to the plotted maps. 
#' @param filenames vector or string for names of the output files (plots) 
#' @param settings xml files for plotting settings (see internal code)
#' @param mask logical If it is \code{TRUE} only the area within the \code{sf} shape is visualized. Default is \code{FALSE}
#' @param write_tif logical. Default is \code{FALSE}. If \code{TRUE}, results are also written and saved as GeoTiff raster files.
#' @param ... further arguments passed to \code{\link{ggsave}}
#'
#' 
#' @export
#'
#' @note \code{x} must have the proper time aggregation for the analysis before the execution of this function.
#' 
#' @importFrom stringr str_replace_all
#' @importFrom terra writeRaster
#' @importFrom ggplot2 scale_fill_gradient2
#' 
#' @examples 
#' 
#' library(magrittr)
#' library(terra)
#' library(lmomPi)
#' library(sf)
#' 
#' years <- 1982:2023
#' 
#' dataset_path <- system.file("ext_data/precipitation",package="terracliva")
#' dataset_monthly <- "%s/monthly/chirps_monthly_goma_%04d.grd" %>% 
#' sprintf(dataset_path,years) %>% rast()
#' terra::time(dataset_monthly) <-  names(dataset_monthly) %>% 
#' paste0("_01") %>% as.Date(format="X%Y_%m_%d")
#' dataset_sf <- system.file("ext_data/OSM_Goma_quartiers_210527.shp"
#' ,package="terracliva") %>% st_read()
#'
#' o_spi1t <- spiapprast(x=dataset_monthly,distrib="pe3",summary_regress=TRUE)
#' 
#' ###filenames <- system.file(package="terraclivaviz") %>% file.path("examples/plot/spi/spi_%s.jpg")
#' filenames <- "/home/ecor/local/rpackages/jrc/terraclivaviz/inst/examples/plot/spi/spi_%s.jpg"
#' out_spi1t_viz <- spiapprastviz(x=o_spi1t,filenames,sf=dataset_sf)
#' 
#' ###filenames <- system.file(package="terraclivaviz") %>% file.path("examples/plot/spi/spi_svg_printing_%s.svg")
#' filenames <- "/home/ecor/local/rpackages/jrc/terraclivaviz/inst/examples/plot/spi/spi_svg_printing_%s.svg"
#' out_spi1t_viz <- spiapprastviz(x=o_spi1t,filenames,sf=dataset_sf,write_tif=TRUE)
#' 
#' 
#' 
#' 
#' 








spiapprastviz <- function(x,filenames,sf,settings=system.file("settings/lm_plot_settings_enexus.xml",package="terraclivaviz"),signif=attr(x,"signif"),mask=FALSE,write_tif=FALSE,...){
  
  ## TO DO 
  #### https://en.wikipedia.org/wiki/Data_and_information_visualization
  if (mask==TRUE){ 
    x <- mask(x,mask=vect(sf)) ## added on 2024 10 04
    EE = ext(vect(sf))
    x <- crop(x,EE)
  }
  code_fun <- "spi"
  
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
   nn2g <<- nn2
  
  if (length(filenames)==1) filenames <- sprintf(filenames,names(x))
  names(filenames) <- names(x)
  for (it in names(x)) {
    ####it2 <<- it
    
    
    
    
    gg  <- ggplot()+geom_spatraster(data=x[[it]])+theme_bw()
    gg <-  gg+geom_sf(data=sf,fill=NA,color="black",linewidth=0.15)
    gg <- gg+ggtitle(it)
    print(settings_list)
    colorscale <- settings_list[[nn2[it]]][["colorscale"]]
    print("nn2:")
    print(nn2)
    print("it:")
    print(it)
    
    print("colorscale:")
    print(colorscale)
    ###
    color_max <- settings_list[[nn2[it]]][["color_max"]]
    color_min <- settings_list[[nn2[it]]][["color_min"]]
    color_zero <- settings_list[[nn2[it]]][["color_zero"]]
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
    print(colors)
   ## gg <- gg+scale_fill_gradientn(colors=colors,na.value=NA)
    midpoint=0 
    #if (it=="pvalue") midpoint=signif
    gg <- gg+scale_fill_gradient2(high=color_max,low=color_min,mid=color_zero,midpoint=midpoint,na.value=NA,limits=limits)
    filename=str_replace_all(filenames[it]," ","_")
    ggsave(filename=filename,plot=gg,...)
    
    if (write_tif) {
      
      filename_tif <- filename
      raster::extension(filename_tif) <- ".tif"
      writeRaster(x[[it]],filename=filename_tif,overwrite=TRUE)
      
    }
    
    
    
    
    
    
    
    
    
    
  }
  
  out <- filenames
  
  
  
  
  return(out)
  
}
