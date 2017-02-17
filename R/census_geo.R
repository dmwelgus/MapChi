
#' Geocode a single batch of 1000 addresses
#'
#' @param address_dir   A directory containing lists of addresses. Each list should consist of no more than 1000 addresses. The
#' address directory should contain NOTHING BUT lists of addresses. Addresses must be formattated according to census specifications.
#' For more details see \url{https://www.census.gov/geo/maps-data/data/geocoder.html}. To ensure proper functioning, save
#' your addresses using \code{\link{save_addresses}}.
#'
#' @seealso \code{\link{save_addresses}}
#' @examples ce("file_name")
#' @return a data frame with lat/long coordinates for each address that was successfully coded and NA for all others.
#' @export
census_geo <- function(file) {
  
  
  for(j in 1:20){
    x  <- httr::POST("https://geocoding.geo.census.gov/geocoder/locations/addressbatch",
                     body = list(addressFile = httr::upload_file(file), benchmark = 9,
                                 vintage = "Census2010_Census2010"), encode = "multipart")
    
    if (x$status_code != 503) {
      break
    }
    
    print(paste("503 Error #", j))
  }
  
  
  y  <- httr::content(x, encoding = "UTF-8", type = "text/csv", col_names = FALSE)
  
  names(y) <- c("id", "o_address", "status", "quality", "m_address",
                "long", "lat", "L_R")
  
  
  if (sum(!is.na(y$id)) == 0) {
    stop("All NA's, trying again")
  }
  
  return(y)
}


#' Geocode batches of addresses using the Census geocoding api.
#'
#' @param address_dir   A directory containing lists of addresses. Each list should consist of no more than 1000 addresses. The
#' address directory should contain NOTHING BUT lists of addresses. Addresses must be formattated according to census specifications.
#' For more details see \url{https://www.census.gov/geo/maps-data/data/geocoder.html}. To ensure proper functioning, save
#' your addresses using \code{\link{save_addresses}}.
#'
#' @seealso \code{\link{save_addresses}}
#' @examples batch_geo2("/path/to/addresses")
#' @return a data frame with lat/long coordinates for each address that was successfully coded and NA for all others.
#' @export
batch_geo2 <- function(address_dir) {
  
  current_dir <- getwd()
  setwd(address_dir)
  files <- dir()
  output <- list()
  
  collect_addrs   <- c()
  
  for (i in 1:length(files)) {
    addrs <- unlist(readLines(files[i]))
    collect_addrs <- append(collect_addrs, addrs)
  }
  
  i <- length(files)
  
  
  while (i > 0) {
    
    f <- readLines(files[i])
    
    tryCatch({
      output[[i]] <- census_geo(file = files[i])
      i <- i - 1
    }, error = function(c) {
      i <<- i
    })
    
    print(i)
    
  }
  
  final <- dplyr::bind_rows(output)
  setwd(current_dir)
  
  final
}
