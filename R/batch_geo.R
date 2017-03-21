## batch_geo. Same as geo_batch, but returns ids of addresses not matched.


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
batch_geo <- function(address_dir) {

  files         <- dir(address_dir)
  files         <- paste(address_dir, files, sep = "/")
  output        <- list()
  collect_addrs <- c()
  
  i <- length(files)

  while (i > 0) {

    f <- readLines(files[i])
    h <- census_geo(files[i])
    
    if (sum(!is.na(h$id)) == 0) {
      i <- i
      print("All NAs, Trying Again")

    } else {
      output[[i]]   <- h
      collect_addrs <- append(collect_addrs, f)
      
      i <- i - 1
      print(paste("Success!!!", i, "more to go"))

    }
  }

  final <- dplyr::bind_rows(output)

  # Now get indices that aren't in final and append to final. Then sort so final is in the same order as
  # the original list of addresses.
  splits  <- lapply(collect_addrs, FUN = function(x) strsplit(x, split = ",")[[1]])
  indices <- unlist(lapply(splits, function(x) x[1]))
  addrs   <- unlist(lapply(splits, function(x) x[2]))
  cities  <- unlist(lapply(splits, function(x) x[3]))
  states  <- unlist(lapply(splits, function(x) x[4]))
  zips    <- unlist(lapply(splits, function(x) x[5]))

  na_index <- which(!indices %in% final$id)

  final$o_address[is.na(final$id)] <- addrs[na_index]
  final$o_city[is.na(final$id)]    <- cities[na_index]
  final$o_state[is.na(final$id)]   <- states[na_index]
  final$o_zip[is.na(final$id)]     <- zips[na_index]
  final$id[is.na(final$id)]        <- indices[na_index]

  final
}
