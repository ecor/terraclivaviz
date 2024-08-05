NULL
#' 
#' Heat and Cold waves analysis in Spatial Gridded Coverage (visualization)
#' 
#'
#' @param x a \code{SpatRast-Class} object returned by \code{\link{dryspellapprast}}. 
#' @param sf an \code{sf} object which will be added to the plotted maps. 
#' @param filenames vector or string for names of the output files (plots) 
#' @param settings xml files for plotting settings (see internal code)
#' @param fun_aggr character aggregation function name, used as name prefixes in namaing \code{x}'s layers  (see \code{\link{dryspellcliva}} and \code{\link{dryspellapprast}})
#' @param summary_suffixes suffixes used for summay/regression functions (see \code{\link{regress}})
#' @param ... further arguments passed to \code{\link{ggsave}}
#'
#' 
#' @export
#'
#' 
#' @importFrom stringr str_replace_all str_replace
#' 
#' @examples
#' 
#' library(sf)
#'
#' years <- 1982:2023
#' dataset_path <- "/home/ecor/local/rpackages/jrc/terracliva/inst/ext_data/precipitation"
#' dataset_path <- system.file("ext_data/precipitation",package="terracliva")
#' dataset_daily <- "%s/daily/chirps_daily_goma_%04d.grd" %>% sprintf(dataset_path,years) %>% rast()
#' dataset_sf <- system.file("ext_data/OSM_Goma_quartiers_210527.shp",package="terracliva") %>% st_read()
#' filename_dry_spell <- "/home/ecor/local/rpackages/jrc/terraclivaviz/inst/ext_data/goma_dry_spell.tif"
#'
#' fun_aggr=aggr_fun_suffixes()
#' if (!file.exists(filename_dry_spell)) {
#'   out <- dryspellapprast(dataset_daily,valmin=1,filename=filename_dry_spell,summary_regress=TRUE,fun_aggr=fun_aggr,overwrite=TRUE)
#' } else {
#'   out <- rast(filename_dry_spell)
#' }
#'
#'
#'
#' filenames <- "/home/ecor/local/rpackages/jrc/terraclivaviz/inst/examples/plot/dryspell/dryspell_%s.jpg"
#' out2 <- dryspellapprastviz(out,sf=dataset_sf,filenames=filenames)
#'
#'
#'



dryspellapprastviz <- function(x,filenames,sf,settings=system.file("settings/lm_plot_settings_v4.xml",package="terraclivaviz"),fun_aggr=terracliva::aggr_fun_suffixes(),summary_suffixes=c("pvalue","coeff","stdrerror","rsquared","senslope","pvalue_mk"),...){
  
  ## TO DO 
  #### https://en.wikipedia.org/wiki/Data_and_information_visualization
  ##stop("FUNCTION_TO_DO")
  code_fun <- "dryspell"
  
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
 
  
  if (length(filenames)==1) filenames <- sprintf(filenames,names(x))
  names(filenames) <- names(x)
  for (it in names(x)) {
    ####it2 <<- it
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