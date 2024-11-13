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

#' @param ... further arguments passed to \code{\link{ggsave}}
#'
#' 
#' @export
#'
#' @note \code{x} must have the proper time aggregation for the analysis before the execution of this function.
#' 
#' @importFrom stringr str_replace_all
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
#' filenames <- system.file(package="terraclivaviz") %>% file.path("examples/plot/hwmid/hwmid_%s.jpg")
#' out_hw_regress_viz <- hwmidapprastviz(x=o_hw_regress,filenames,sf=dataset_sf)
#' 








hwmidapprastviz <- function(x,filenames,sf,settings=system.file("settings/lm_plot_settings_v4.xml",package="terraclivaviz"),mask=FALSE,...){
  
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
    if (write_tif) {
      
      filename_tif <- filename
      extansion(filename_tif) <- ".tif"
      writeRaster(x[[it]],filename=filename_it,overwrite=TRUE)
      
    }
    
    
    
    gg  <- ggplot()+geom_spatraster(data=x[[it]])+theme_bw()
    gg <-  gg+geom_sf(data=sf,fill=NA,color="black",linewidth=0.15)
    gg <- gg+ggtitle(it)
    
    colorscale <- settings_list[[nn2[it]]][["colorscale"]]

    rev <- as.numeric(settings_list[[nn2[it]]][["rev"]])
    colors <-   colorRampPalette(brewer.pal(9,colorscale))(9)
    if (rev<0) colors <- rev(colors)
    print(colors)
    gg <- gg+scale_fill_gradientn(colors=colors,na.value=NA)
    filename=str_replace_all(filenames[it]," ","_")
    ggsave(filename=filename,plot=gg,...)
    
  
    
    
    
    
    
    
    
    
    
    
  }
  
  out <- filenames
  
  
  
  
  return(out)
  
}
