#' Attribute LMF data with values from polygon data
#' @param TDat_LMF Data frame. LMF data as a data frame. Must contain the variables \code{PrimaryKey}, \code{Latitude_NAD83}, and \code{Longitude_NAD83}.
#' @param shapefile_name Character string. The name of a the polygon shapefile to read in and apply attributes from.
#' @param shapefile_path Character string. The full filepath to the location containing the shapefile named in \code{shapfile_name}.
#' @param attribute_title Character string. The name of the variable in the shapefile to attribute \code{TDat_LMF} with.
#' @returns A data frame containing the records from \code{TDat_LMF}, but only the variables \code{PrimaryKey} and whatever were inherited from the shapefile.
#' @export
AttributePlots <- function(TDat_LMF,
                           shapefile_name,
                           shapefile_path,
                           attribute_title){

  # Match coordinate reference systems for intersection
  projection <- sf::st_crs("+proj=longlat +datum=NAD83")
  
  # Join by PrimaryKey to get GPS coordinates for species indicators entries
  # TDat_LMF is output of Combine_AIM_LMF
  coordinates <- dplyr::select(TDat_LMF,
                               PrimaryKey, Latitude_NAD83, Longitude_NAD83)
  
  # Convert Terradat data into "simple feature" object class for spatial reference.
  # Not removing any NA or entries without coordinates.
  TDat_LMF_Spatial <- sf::st_as_sf(TDat_LMF,
                                   coords = c("Longitude_NAD83",
                                              "Latitude_NAD83"),
                                   na.fail = FALSE,
                                   remove = FALSE,
                                   crs = projection)
  
  ##-----------------------------------
  # Load in shapefiles and intersect with data (include AND exclude data in sf)
  ##-----------------------------------
  
  # Read in shapefiles
  shapefile  <-  sf::st_read(dsn = shapefile_path,
                             layer = shapefile_name)
  shapefile <- sf::st_transform(shapefile,
                                crs = projection)
  
  #Simplify shapefile to just the attributes we want
  shapefile <- dplyr::select(shapefile,
                             tidyselect::all_of(attribute_title))
  
  # Intersect shapefile with plots to get attributed
  TDat_LMF_Attributed <- sf::st_intersection(x = TDat_LMF_Spatial,
                                             y = sf::st_make_valid(shapefile))
  
  TDat_LMF_Attributed <- dplyr::select(TDat_LMF_Attributed,
                                       PrimaryKey, tidyselect::all_of(attribute_title))
  
  return(TDat_LMF_Attributed)
}
