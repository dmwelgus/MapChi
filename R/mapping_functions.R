# Maps!

# Create blank Chicago map with user-specified geographic boundaries.

#' Create a basic map of Chicago with specified regional boundaries.
#
#' @param background The background color.
#' @param lines The line color.
#' @param regions The type of region to plot. Options include: "CAs", "tracts," "districts,"
#' and "zips."
#' @param title Title.
#' @param title_size Title size.
#'
#' @examples
#' map_chi(background = "grey", lines = "blue", title = "Chicago Community Areas")
#' @return A map of Chicago. More specifically, an object of type "ggplot."

#' @export
map_chi <- function(background, lines, regions = "CAs", title = NULL, title_size = 20) {


  spatial_df <- get(regions)

  map <- ggplot2::ggplot(spatial_df) + ggplot2::geom_polygon(ggplot2::aes(long, lat, group = group), fill = background) +
    ggplot2::geom_path(ggplot2::aes(long, lat, group = group), color = lines) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = title_size),
                   panel.background = ggplot2::element_rect(fill = "white"), panel.grid.major = ggplot2::element_line(colour = "white"),
                   panel.grid.minor = ggplot2::element_line(colour = "white")) +
    ggplot2::coord_equal() +
    ggplot2::ggtitle(title)

  map
}


# Continuous color palette

#' Create a heat map of Chicago with continuous shading.
#
#' @param regions The type of region to plot. Options include: "CAs", "tracts," "districts,"
#' and "zips."
#' @param summary_df A data frame with summary statistics by region. Should have one row for each
#' region.
#' @param regions_var The variable in df.y that identifies the region. For example, if type = "CAs"
#' a common summary_var would be "Community.Areas." In general, regions_var should be numeric (e.g. use
#' community area numbers, not names.).

#' @param fill_var The variable that determines color. For example, if the map shows homicides by
#' police district, the fill_var would be the field that contains homicide totals.
#'
#' @param legend_name Self Explanatory. Should be a character vector of length one.
#' @param palette The color palette. Current options are: green, blue, red, organge, and purple.
#' @param low_color If palette is set to NULL, the low end of the color range you want to use.
#' @param high_color The high end of your color range
#' @param na_replace What value should replace NAs in the fill_var. This arises most commonly in
#' maps of homicides, where regions with zero homicides are sometimes missing from df.y.
#' @param lines Color of border lines. Default is black.
#' @param title Title.
#' @param title_size Title size.
#'
#' @return A map of Chicago. More specifically, an object of type "ggplot."

#' @examples
#' data(hom_14)
#'
#' # Create summary table
#' hom_sum <- dplyr::summarise(group_by(hom_14, District), homicides = n())
#'
#' h_map <- heat_map_continuous(type = "districts", summary_df = hom_sum, regions_var = "District",
#'                              fill_var = "homicides", legend_name = "Homicides",
#'                              na_replace = 0)

#' @export
heat_map_continuous <- function(regions, summary_df, regions_var,  fill_var, legend_name, palette = NULL,
                                low_color = "#fff5eb", high_color = "#7f2704", na_replace = NA,
                                lines = "black", title = NULL, title_size = 15) {


  if (!is.null(palette)) {

    colors <- c("green", "blue", "red", "orange", "purple")
    low    <- c("#e5f5f9", "#deebf7", "#fee0d2", "#fee6ce", "#efedf5")
    high   <- c("#00441b", "#08306b", "#67000d", "#7f2704", "#3f007d")

    min_color <- low[colors  == palette]
    max_color <- high[colors == palette]

  } else {

    min_color <- low_color
    max_color <- high_color
  }

  step_1 <- get(regions)
  step_1@data$id <- rownames(step_1@data)

  step_2 <- ggplot2::fortify(step_1)

  chi.df <- merge(step_2, step_1@data, by = "id")

  merge_vars <- c("AREA_NUMBE", "TRACTCE10", "DIST_NUM", "ZIP")
  types      <- c("CAs", "tracts", "districts", "zips")

  merge_var.x <- merge_vars[types == regions]

  df <- merge(chi.df, summary_df, by.x = merge_var.x, by.y = regions_var, all.x = TRUE)


  df$fill_it <- df[, fill_var]
  df$fill_it[is.na(df$fill_it)] <- na_replace

  df <- df[order(df$order), ]

  ggplot2::ggplot(df) + ggplot2::geom_polygon(ggplot2::aes(long, lat, group = group, fill = fill_it)) +
    ggplot2::geom_path(ggplot2::aes(long, lat, group = group), color = lines) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = title_size),
                   panel.background = ggplot2::element_rect(fill = "white"), panel.grid.major = ggplot2::element_line(colour = "white"),
                   panel.grid.minor = ggplot2::element_line(colour = "white")) +
    ggplot2::coord_equal() +
    ggplot2::scale_fill_continuous(low = min_color, high = max_color,
                          guide = ggplot2::guide_colorbar(title = legend_name, title.theme = ggplot2::element_text(size = 13, angle = 0),
                                                 label.theme = ggplot2::element_text(size = 11, angle = 0))) +
    ggplot2::ggtitle(title)



}


# Discrete color palette
#' Create a heat map of Chicago with discrete shading.
#
#' @param regions The type of region to plot. Options include: "CAs", "tracts," "districts,"
#' and "zips."
#' @param summary_df A data frame with summary statistics by region. Should have one row for each
#' region.
#' @param regions_var The variable in summary_df that identifies the region. For example, if type = "CAs"
#' a common merge_var.y would be "Community.Areas." In general, regions_var should be numeric (e.g. use
#' community area numbers, not names.).

#' @param fill_var The variable that determines color. For example, if the map shows homicides by
#' police district, the fill_var would be the field that contains homicide totals.
#'
#' @param legend_name Self Explanatory. Should be a character vector of length one.
#' @param palette The color palette. Current options are: green, blue, red, organge, and purple.
#' @param lines Color of border lines. Default is black.
#' @param title Title.
#' @param title_size Title size.
#'
#' @return A map of Chicago. More specifically, an object of type "ggplot."

#' @examples
#' data(hom_14)
#' library(dplyr)
#' hom_sum <- dplyr::summarise(group_by(hom_14, Community.Area), homicides = n())
#'
#' # No na_replace option for discrete maps. Need to manually add zero homicide CAs.
#' additions <- c(1:77)[which(!c(1:77) %in% hom_sum$Community.Area)]

#' df_additions <- data.frame(Community.Area = additions, homicides = 0)

#' hom_sum <- rbind(hom_sum, df_additions)
#' hom_sum$range <- cut(hom_sum$homicides, c(0, 5, 10, 15, 20, 30, 40),
#'                      labels = c("0-4", "5-9", "10-14", "15-19",
#'                                "20-29", "30-39"), right = FALSE)
#'
#' library(RColorBrewer)
#' colors <- brewer.pal(n = 6, name = "BuPu")
#' hm_dis <- heat_map_discrete(regions = "CAs", summary_df = hom_sum, regions_var = "Community.Area",
#'                             fill_var = "range", legend_name = "Total Homicides",
#'                             palette = colors, title = "Homicides by Community Area: 2014")


#' @export
heat_map_discrete <- function(regions, summary_df, regions_var,
                              fill_var, legend_name, palette,
                              lines = "black", title = NULL, title_size = 15) {

  step_1 <- get(regions)
  step_1@data$id <- rownames(step_1@data)

  step_2 <- ggplot2::fortify(step_1)

  chi.df <- merge(step_2, step_1@data, by = "id")

  merge_vars <- c("AREA_NUMBE", "TRACTCE10", "DIST_NUM", "ZIP")
  types      <- c("CAs", "tracts", "districts", "zips")

  merge_var.x <- merge_vars[types == regions]


  df <- merge(chi.df, summary_df, by.x = merge_var.x, by.y = regions_var, all.x = TRUE)

  df$fill_it <- df[, fill_var]
  df <- df[order(df$order), ]

  ggplot2::ggplot(df) + ggplot2::geom_polygon(ggplot2::aes(long, lat, group = group, fill = fill_it)) +
    ggplot2::geom_path(ggplot2::aes(long, lat, group = group), color = lines) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = title_size),
                   panel.background = ggplot2::element_rect(fill = "white"), panel.grid.major = ggplot2::element_line(colour = "white"),
                   panel.grid.minor = ggplot2::element_line(colour = "white")) +
    ggplot2::coord_equal() +
    ggplot2::scale_fill_manual(values = palette, guide = ggplot2::guide_legend(title = legend_name, title.theme = ggplot2::element_text(size = 13, angle = 0),
                                                             label.theme = ggplot2::element_text(size = 11, angle = 0))) +
    ggplot2::ggtitle(title)

}

