NULL
#' 
#' Precipitation Deficit with L-Moments  Climate Variability Analysis in Spatial Gridded Coverage (visualization)
#' 
#'
#' @param x a \code{SpatRast-Class} object returned by \code{\link{lmapprast}}. 
#' @param sf an \code{sf} object which will be added to the plotted maps. 
#' @param filenames vector or string for names of the output files (plots) 
#' @param distrib see \code{\link{pel}}
#' @param settings xml files for plotting settings (see internal code)
#' @param mask logical If it is \code{TRUE} only the area within the \code{sf} shape is visualized. Default is \code{FALSE}
#' @param write_tif logical. Default is \code{FALSE}. If \code{TRUE}, results are also written and saved as GeoTiff raster files.
#' @param ... further arguments passed to \code{\link{ggsave}}
#'
#' 
#' 
#' @importFrom terracliva lmapprast apprast lmcliva
#' @importFrom grDevices colorRampPalette
#' @importFrom stringr str_sub
#' @importFrom tidyterra geom_spatraster
#' @importFrom ggplot2 ggplot geom_sf ggtitle theme_bw ggsave scale_fill_gradientn
#' @importFrom stringr str_length str_detect 
#' @importFrom xml2 read_xml  xml_children xml_text xml_name
#' @importFrom RColorBrewer brewer.pal
#' @importFrom lmomPi pel

#' @export
#'
#' @note \code{x} must have the proper time aggregation for the analysis before the execution of this function.
#' 
#' @importFrom raster extension
#' 
#' @examples
#' 
#' 
#' library(sf)
#' 
#' years <- 1982:2023
#'
#'
#' 
#' dataset_path <- system.file("ext_data/precipitation",package="terracliva")
#' dataset_yearly <- "%s/yearly/chirps_yearly_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()
#' dataset_sf <- system.file("ext_data/OSM_Goma_quartiers_210527.shp",package="terracliva") %>% st_read()
#' 
#' out_yearly <- lmapprast(dataset_yearly)
#' 
#' filenames <- system.file(package="terraclivaviz") %>% file.path("examples/plot/lm/yearly/lm_%s.jpg")
#' out_yearly_viz <- lmapprastviz(x=out_yearly,filenames,sf=dataset_sf)
#' 
#' 
#' 
#' 
#' library(lubridate)
#' dataset_monthly <- "%s/monthly/chirps_monthly_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()
#' time(dataset_monthly) <-  names(dataset_monthly) %>% paste0("_01") %>% as.Date(format="X%Y_%m_%d")
#' 
#' 
#' out_monthly <- lmapprast(dataset_monthly,index="monthly",distrib="pe3")
#' 
#' filenames <- system.file(package="terraclivaviz") %>% file.path("examples/plot/lm/monthly/lm_%s.jpg")
#' out_monthly_viz <- lmapprastviz(x=out_monthly,filenames,sf=dataset_sf)
#' 










lmapprastviz <- function(x,filenames,sf,distrib=eval(formals(lmomPi::pel)$distrib),settings=system.file("settings/lm_plot_settings_v4.xml",package="terraclivaviz"),mask=FALSE,write_tif=FALSE,...){
  
  ## TO DO 
  #### https://en.wikipedia.org/wiki/Data_and_information_visualization
  
  code_fun <- "lm"
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
  print(distrib)
  for (ndistrib in distrib) {
    
    idistrib <- which(str_detect(nn,ndistrib))
    nn2[idistrib] <- "distrib"
    
  }

  nn2[iexc] <- "_exc_rt"
  nn2[idef] <- "_def_rt"
  
  names(nn2) <- names(x)
  #####
  #####
  #####
  ###nn2 <<- nn2
  #####
  
  
  if (length(filenames)==1) filenames <- sprintf(filenames,names(x))
  names(filenames) <- names(x)
  for (it in names(x)) {
   
    ####it2 <<- it
    gg  <- ggplot()+geom_spatraster(data=x[[it]])+theme_bw()
    gg <-  gg+geom_sf(data=sf,fill=NA,color="black",linewidth=0.15)
    gg <- gg+ggtitle(it)
    
    colorscale <- settings_list[[nn2[it]]][["colorscale"]]
    ##print(colorscale)
    rev <- as.numeric(settings_list[[nn2[it]]][["rev"]])
    colors <-   colorRampPalette(brewer.pal(9,colorscale))(9)
    if (rev<0) colors <- rev(colors)
    print(colors)
    gg <- gg+scale_fill_gradientn(colors=colors,na.value=NA)
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