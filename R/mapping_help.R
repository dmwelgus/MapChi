

spatial_df <- function(dir, file) {


  step_1 <- rgdal::readOGR(dir, file, verbose = FALSE)
  step_1@data$id <- rownames(step_1@data)

  step_2 <- ggplot2::fortify(step_1, region = "id")

  chi.df <- merge(step_2, step_1@data, by = "id")

  chi.df

}


# Take df with lat/long coordinates and convert them to the chicago coordinate system
#' Takes a set of lat/long coordinates and converts them to the Chicago coordinate system.
#' @param df A data frame with lat/long coordinates and at least one other field.
#' @param lat Latitude
#' @param long Longitude
#' @param epsg the epsg of the lat/long coordinates. Default is 4326 (WSG-84)
#' @examples
#' data(hom_14)
#' hom_14 <- dplyr::select(hom_14, -X.Coordinate, -Y.Coordinate)
#' hom_14 <- convert(hom_14, lat = "Latitude", long = "Longitude")
#'
#' @return The original data frame with two additional columns: X_coordinate and Y_coordinate.
#' @export
convert <- function(df, lat, long, epsg = 4326) {


  crs_string <- paste("+init=epsg", epsg, sep = ":")

  df <- sp::SpatialPointsDataFrame(coords = df[, c(long, lat)],
                                   data = df,
                                   proj4string = sp::CRS(crs_string))

  chi_coordinates <- sp::CRS("+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.999975 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")

  df <- sp::spTransform(df, chi_coordinates)

  df <- cbind(df@data, X_Coordinate = df@coords[, 1], Y_Coordinate = df@coords[, 2])

  df
}


leaflet_chi <- function(dir, file) {


  step_1 <- rgdal::readOGR(dir, file, verbose = FALSE)

  chi    <- sp::spTransform(step_1, CRS("+init=epsg:4326"))

  chi


}

#' Locates spatial coordinates within user-specified regions.
#' @param df A data frame with lat/long or X/Y coordinates.
#' @param regions The type of region. Options include: "CAs", "districts", "tracts", and "zips".
#' @param lat Name of the field storing latitude. Should be in quotes.
#' @param long Name of the field storing longitude. Should be in quotes.
#' @param X The 'X coordinate' you find on some tables from the data portal.
#' @param Y The 'Y coordinate' you find on some tables from the data portal. Should use EITHER lat/long or X/Y but not both.
#' @param epsg The epsg. Only relevant if using lat/long. Default is 4326 (WSG84).
#'
#' @return A data frame. More specifically, df + one or two regional variables. If region = "CAs", the CA number and name are added. For
#' all other regions a single field is added: 'TRACTCE10" for tracts, "DIST_NUM" for districts, and "ZIP" for zips.

#' @export
get_regions <- function(df, regions, lat, long, X = NULL, Y = NULL, epsg = 4326) {


  sp_df <- get(regions)

  if (!is.null(X)) {

    df <- sp::SpatialPointsDataFrame(coords = df[, c(X, Y)],
                                     data = df,
                                     proj4string = sp_df@proj4string)
  } else {

    crs_string <- paste("+init=epsg", epsg, sep = ":")

    df <- sp::SpatialPointsDataFrame(coords = df[, c(long, lat)],
                                     data = df,
                                     proj4string = sp::CRS(crs_string))

    df <- sp::spTransform(df, sp_df@proj4string)
  }

  regional_df <- sp::over(df, sp_df)

  keep_vars <- list(CAs = c("AREA_NUMBE", "COMMUNITY"), tracts = c("TRACTCE10"), districts = c("DIST_NUM"),
                    zips = c("ZIP"))

  keepers <- keep_vars[[regions]]
  regional_df[keepers] <- lapply(regional_df[keepers], as.character)

  regional_df <- regional_df[keepers]
  cbind(df@data, regional_df)

}
