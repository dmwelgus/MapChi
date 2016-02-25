# save_addresses function.

#' Saves blocks of 1000 addresses in a directory (dir).
#'
#' @param addresses A vector of addresses. Addresses should be formatted according to
#' census specifications: "unique id, address, city, state, zip". A real example:
#' "123, 33 N. LaSalle St., Chicago, IL, 60602". If any pieces are missing, substitute a
#' blank space. For example, if state is missing, use "123, 33 N. LaSalle St., Chicago, , 60602". If
#' everything is missing, use "123, , , , ". For more details, see \url{https://www.census.gov/geo/maps-data/data/geocoder.html}.
#' @param dir An empty directory.
#' @param root The root name for each address block. E.G. If root = "addr", the blocks will be labelled "addr_1.txt", "addr_2.txt", etc...
#'
#' @return Nothing returned. Properly formatted address files should appear in dir.
#'
#' @examples
#' addr_list <- c("1, 33 North LaSalle, Chicago, IL, 60602", "2, 756 West Irving Park Rd., Chicago, IL, 60613")
#' addr_dir <- "/path/to/empty_directory"
#' stem <- "addrs"
#'
#' save_addresses(addresses = addr_list, dir = addr_dir, root = stem)
#' @export
save_addresses <- function(addresses, dir, root) {

  current_dir <- getwd()
  setwd(dir)

  file_n <- ceiling(length(addresses)/1000)


  for (i in 1:file_n){

    start <- 1000*(i-1) + 1

    if (i == file_n) {
      end <- length(addresses)
    } else {
      end <- i * 1000
    }

    addrs <- addresses[start:end]

    name <- paste(root, i, sep = "_")
    name <- paste(name, "txt", sep = ".")

    write.table(addrs, name, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
  setwd(current_dir)
}
