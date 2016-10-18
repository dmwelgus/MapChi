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

    for(j in 1:20){
      x  <- httr::POST("https://geocoding.geo.census.gov/geocoder/locations/addressbatch",
                             body = list(addressFile = httr::upload_file(files[i]), benchmark = 9,
                             vintage = "Census2010_Census2010"), encode = "multipart")

      if (x$status_code != 503) {
        break
      }

      print(paste("503 Error #", j))
    }


    y  <- httr::content(x, encoding = "UTF-8")
    df <- strsplit(y, split = "\\\n")[[1]]
    df <- sapply(df, strsplit, split = ",")

    clean <- lapply(df, length_15)

    h   <- t(as.data.frame(clean))
    h   <- as.data.frame(h)
    h[] <- lapply(h, as.character)

    h[, c("V1", "V5", "V6", "V7", "V11", "V14", "V15")] <- lapply(h[, c("V1", "V5", "V6", "V7", "V11", "V14", "V15")], strip_both)
    h[, c("V2", "V8", "V12")]                     <- lapply(h[, c("V2", "V8", "V12")], strip_left)
    h$V13                                         <- strip_right(h$V13)

    h[]  <- lapply(h, stringr::str_trim)

    names(h) <- c("id", "o_address", "o_city", "o_state", "o_zip", "status", "quality", "m_address",
                  "m_city", "m_state", "m_zip", "long", "lat", "not_sure", "L_R")


    output[[i]] <- h

    if (sum(!is.na(h$id)) == 0) {
      i <- i

      print("All NAs, Trying Again")

      } else {
      i <- i - 1

      print(paste("Success!!!", i, "more to go"))

      }


  }

  final <- dplyr::bind_rows(output)

  setwd(current_dir)

  final
}


