#' Plot utility function for plots in problem 1
#'
#' @param df A data frame containing the values
#' @param loc_meta A data frame containing the location information (such as city name, lat, lon, etc)
#' @param x_range The range of x values in the final plot (either 1:12 or the years)
#' @param xlab label for x axis
#' @param ylab label for y axis
#' @param type The type of plot (either "b" or "l")
#' @param legend_loc Location of the legend (either "center" or "bottom")
plot_multiple_city_data = function(df, loc_meta, x_range, xlab, ylab, type, legend_loc) {
  plot(
    x_range,
    df[, 1],
    xlab = xlab,
    ylab = ylab,
    col = loc_meta$colors[1],
    ylim = c(min(df), max(df)),
    type = type,
    pch = 1,
  )
  for (i in 2:length(loc_meta$labels)) {
    lines(x_range, df[, i], col = loc_meta$colors[i], type = type, pch = i)
  }
  grid()
  legend(
    legend_loc,
    loc_meta$labels,
    col = loc_meta$colors,
    lty = rep(1, length(loc_meta$labels)),
    pch = 1:length(loc_meta$labels)
  )
}


#' Draws a map overlayed with nation"s boundaries using the `maps` package
#'
#' Further argumets are passed to the `filled.contour` function.
#'
#' @param lon The set of values for longitude
#' @param lat The set of values for latitude
#' @param variable The data that is being plotted
#'
draw_contour_map <- function(lon, lat, variable, ...) {
  # First fix the longitude difference between map() [-180,180] and the netcdf data [0, 360]
  lonfix = lon
  lonfix[lon >= 180] = lon[lon >= 180] - 360
  lonvec = lon - 180
  idxlon = match(lonfix, lonvec)
  filled.contour(
    lonvec,
    lat,
    variable[idxlon, ],
    plot.axes = {
      map("world", add = T, col = "black", fill = F)
      map.axes()
    },
    xlab = "Longitude",
    ylab = "Latitude",
    ...
  )
}

# This color palette is used for map plots
RdBu.colors <- colorRampPalette(c(
  "#08306B",
  "#2171B5",
  "#6BAED6",
  "#C6DBEF",
  "#F7FBFF",
  "#FEE0D2",
  "#FC9272",
  "#EF3B2C",
  "#A50F15"
))
