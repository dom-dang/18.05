# Studio 3 ---

# 1. Be sure to read the instructions file!

# 2. The instruction file gives instructions for testing your code. You can see the output of the tests in studio3-test-answers.html. You should use this to verify your code is giving correct output in the format asked for.

#---------------------

# First, install the following packages:
# - ncdf4
# - maps
#
# The following piece of code extracts the necessary data and defines some variables that are used throughout the studio.
# Read them and familiarize yourself with what each line of code is doing.
# NOTE: Do not change the following lines of code.

require(ncdf4) # This will be used to read netcdf data files
require(maps) # This is used to draw country boundaries in maps

# The following file contains helper functions for plotting, namely
# - plot_multiple_city_data
# - draw_contour_map
# Take a look at the functions and see how they work
source("./studio3-helper.r")

n4obj = nc_open("t2m_data.nc") # This is an object that can be parsed by functions in ncdf4 to actually extract the data

# ncvar_get() actually extracts the data
t2m = ncvar_get(n4obj, "t2m") # Air temperature at 2 meters [Kelvin]
lats = ncvar_get(n4obj, "lat") # available latitudes
lons = ncvar_get(n4obj, "lon") # available longitudes

# For time, as.POSIXct() converts original units of time in netcdf into a date (POSIXct) object
time = as.POSIXct(ncvar_get(n4obj, "valid_time"), origin = "1970-01-01", tz = "UTC")
months = as.numeric(format(time, "%m")) # This will be useful for averaging later
years = as.numeric(format(time, "%Y")) # This will be useful for averaging later

#' The function below returns the local data of a given data frame of locations of interest.
#'
#' @param locations A data frame describing the locations of interest
#' @returns the t2m data for the specified locations
extract_data = function(locations) {
  NL = length(locations$labels)
  NT = dim(t2m)[3]
  localdata = matrix(NA, NT, NL)
  for (i in 1:NL) {
    # find the closest matching lat/lon that is available in the grid
    iloc = which.min(abs(locations$lons[i] - lons))
    jloc = which.min(abs(locations$lats[i] - lats))
    localdata[, i] = t2m[iloc, jloc, ]
  }
  return(localdata)
}

#---------------------------------
# Problem 1: Monthly and yearly Climatology. ----
# See the instructions for this studio.

#' 1a: Comparing two cities' monthly temperature averages
#'
#' @params locations A data frame containing information about two locations. See an example in the test file.
studio3_problem_1a = function(locations) {
  location_data = extract_data(locations)

  # Calculate monthly average temperature (What is the average temp in Jan, Feb...?)
  monthly_avg_temps = apply(location_data, 2, function(col) tapply(col, months, mean))

  # convert Kelvin to Celcius
  monthly_avg_temps = monthly_avg_temps - 273.15

  # plot the montly average for the cities
  plot_multiple_city_data(
    monthly_avg_temps,
    locations,
    1:12,
    "month",
    "Average Air Temperature at 2m [C]",
    "b",
    "bottom"
  )

  correlation = cor(monthly_avg_temps[, 1], monthly_avg_temps[, 2])

  cat(
    "The correlation between the monthly averages in ",
    locations$labels[1],
    " and ",
    locations$labels[2],
    " is ",
    sprintf("%.2f", correlation),
    "\n"
  )

  return(correlation)
}

# 1a: Comparing two cities' yearly temperature averages

studio3_problem_1b = function(locations) {
  # Arguments:
  #   locations = a data frame containing information about two locations. See an example in the test file.

  location_data = extract_data(locations)

  # Calculate yearly average temperature (What is the average temp in the years 1940, 1941, ..., 2024?)
  yearly_avg_temps = apply(location_data, 2, function(col) tapply(col, years, mean))

  # convert Kelvin to Celcius
  yearly_avg_temps = yearly_avg_temps - 273.15

  plot_multiple_city_data(
    yearly_avg_temps,
    locations,
    unique(years),
    "year",
    "Average Air Temperature at 2m [C]",
    "l",
    "center"
  )

  correlation = cor(yearly_avg_temps[, 1], yearly_avg_temps[, 2])

  cat(
    "The correlation between the yearly averages in ",
    locations$labels[1],
    " and ",
    locations$labels[2],
    " is ",
    sprintf("%.2f", correlation),
    "\n"
  )

  return(correlation)
}

#---------------------------------
# Problem 2: Climatology of a city vs the rest of the world. ----
# See the instructions for this studio.


# 2a: Comparing monthly temperature averages

studio3_problem_2a = function(label, lat, lon) {
  # Arguments:
  #   label = the city name
  #   lat = latitude
  #   lon = longitude

  # The following line might take a little while to execute (~1-2mins)
  monthly_avg_all_earth <- apply(
    t2m,
    c(1, 2),
    function(grid_point) tapply(grid_point, months, mean)
  )

  location_data <- extract_data(data.frame(labels = c(label), lats = c(lat), lons = c(lon)))
  loc_monthly_avg <- apply(location_data, 2, function(col) tapply(col, months, mean))

  monthly_correlation <- apply(
    monthly_avg_all_earth,
    c(2, 3),
    function(grid_point) cor(loc_monthly_avg[, 1], grid_point)
  )

  draw_contour_map(
    lons,
    lats,
    monthly_correlation,
    color.palette = RdBu.colors,
    zlim = c(-1, 1),
    main = paste(label, " vs Rest (monthly)")
  )

  return(monthly_correlation)
}

# 2b: Comparing yearly temperature averages

studio3_problem_2b = function(label, lat, lon) {
  # Arguments:
  #   label = the city name
  #   lat = latitude
  #   lon = longitude

  # The following line might take a little while to execute (~1-2mins)
  yearly_avg_all_earth <- apply(t2m, c(1, 2), function(grid_point) tapply(grid_point, years, mean))

  my_location_data <- extract_data(data.frame(labels = c(label), lats = c(lat), lons = c(lon)))
  loc_yearly_avg <- apply(my_location_data, 2, function(col) tapply(col, years, mean))

  yearly_correlation <- apply(
    yearly_avg_all_earth,
    c(2, 3),
    function(grid_point) cor(loc_yearly_avg[, 1], grid_point)
  )

  draw_contour_map(
    lons,
    lats,
    yearly_correlation,
    color.palette = RdBu.colors,
    zlim = c(-1, 1),
    main = paste(label, " vs Rest (yearly)")
  )

  return(yearly_correlation)
}


# 2c: [Optional] Comparing Boston's monthly average to the places with the same latitude

studio3_problem_2c = function() {
  boston <- data.frame(
    labels = c("Boston"),
    lats = c(42.36),
    lons = c(288.94)
  )

  boston_data <- extract_data(boston)
  boston_monthly_avg <- apply(boston_data, 2, function(col) tapply(col, months, mean))

  NT = dim(t2m)[3]
  localdata = matrix(NA, NT, length(lons))

  boston_iloc = which.min(abs(boston$lons[1] - lons))
  boston_jloc = which.min(abs(boston$lats[1] - lats))

  for (i in 1:length(lons)) {
    # find the closest matching lat/lon that is available in the grid
    localdata[, i] = t2m[i, boston_jloc, ]
  }

  monthly_avg_on_same_lat <- apply(
    localdata,
    c(2),
    function(grid_point) tapply(grid_point, months, mean)
  )

  corrs = cor(boston_monthly_avg, monthly_avg_on_same_lat)
  plot(lons, corrs, type = 'l', xlab = "Longitude", ylab = "Correlation (monthly)")
  points(x = c(lons[boston_iloc]), y = c(1.0), col = "red", pch= 16)
  text(x = c(lons[boston_iloc]), y = c(1.0), col = "red", labels="Boston", pos=4)
}

#---------------------------------
# Problem 3: Did the surface of Earth warm up uniformly between 1940 and "today"? ----
# See the instructions for this studio.

studio3_problem_3 = function() {
  # The following line might take a little while to execute (~1-2mins)
  yearly_avg_all_earth <- apply(t2m, c(1, 2), function(grid_point) tapply(grid_point, years, mean))

  idx_hist <- which(unique(years) >= 1940 & unique(years) <= 1949)
  idx_pres <- which(unique(years) >= 2015 & unique(years) <= 2024)

  temp_hist <- apply(yearly_avg_all_earth[idx_hist, , ], 2:3, mean)
  temp_pres <- apply(yearly_avg_all_earth[idx_pres, , ], 2:3, mean)
  temp_diff <- temp_pres - temp_hist
  
  max_abs_diff = max(abs(temp_diff), na.rm= TRUE)
  
  draw_contour_map(
    lons,
    lats,
    temp_diff,
    color.palette = RdBu.colors,
    zlim = c(-max_abs_diff, max_abs_diff),
    main = "Change in temps betwen 2015-2024 and 1940-1949"
  )
}

boston_and_sydney = data.frame(
  labels = c("Boston", "Sydney"),
  lats = c(42.36, -33.87),
  lons = c(288.94, 151.21),
  colors = c("blue", "red")
)

other_locations = data.frame(
  labels = c("Boston", "Nairobi", "Quito"),
  lats = c(42.36, -1.28, -0.230),
  lons = c(288.94, 36.817, 281.475)
)

# studio3_problem_1a(boston_and_sydney)
# studio3_problem_1b(boston_and_sydney)
#
for (i in 1:length(other_locations$labels)) {
  studio3_problem_2a(other_locations$labels[i], other_locations$lats[i], other_locations$lons[i])
  studio3_problem_2b(other_locations$labels[i], other_locations$lats[i], other_locations$lons[i])
}
#
studio3_problem_2c()
#
studio3_problem_3()
