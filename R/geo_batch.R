## Wrap the batch geocoding loop into a function with a single input: dir.


#' Geocode batches of addresses using the Census geocoding api.
#'
#' @param address_dir   A directory containing lists of addresses. Each list should consist of no more than 1000 addresses. The
#' address directory should contain NOTHING BUT lists of addresses. Addresses must be formattated according to census specifications.
#' For more details see \url{https://www.census.gov/geo/maps-data/data/geocoder.html}. To ensure proper functioning, save
#' your addresses using \code{\link{save_addresses}}.
#'
#' @seealso \code{\link{save_addresses}}
#' @examples geo_batch("/path/to/addresses")
#' @return a data frame with lat/long coordinates for each address that was successfully coded and NA for all others.
#' @export
geo_batch <- function(address_dir) {

  current_dir <- getwd()
  setwd(address_dir)
  files <- dir()
  output <- list()

  i <- length(files)

  while (i > 0) {

    h <- census_geo(files[i])
    
    if (sum(!is.na(h$id)) == 0) {
      
      i <- i
      print("All NAs, Trying Again")
      
      } else {
        
      output[[i]] <- h
      i <- i - 1
      print(paste("Success!!!", i, "more to go"))
      
      }

  }

  final <- dplyr::bind_rows(output)
  setwd(current_dir)
  final
}


