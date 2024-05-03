# Load necessary packages
library(ggplot2)
library(ggmap)
library(ggrepel)


# Get a map of the US with the desired extent
# Get a map of the US with the desired extent


# Plot the map and set the theme to be completely black
plotUsCities <- function(coordinateCitiesDt, 
                         nudge_x = NULL,
                         nudge_y = NULL) {
  requiredColumnNames <- c("city", "lat", "long")
  checkRequiredColumnNames(coordinateCitiesDt, requiredColumnNames)
  
  
  nudgeSetter <- function(nudge, desiredLength) {
    if (is.null(nudge)) {
      return(rep(0, desiredLength))
    } else {
      if (length(nudge) != desiredLength)
      {
        stop("nudge needs to be same length as inputs")
      }
    }
  }
  
  nudge_x <- nudgeSetter(nudge_x, nrow(coordinateCitiesDt))
  nudge_y <- nudgeSetter(nudge_y, nrow(coordinateCitiesDt))
  
  return(
    ggplot() +
      # Layer 1: State grid
      geom_polygon(data = map_data("state"),
                   aes(x = long,
                       y = lat, 
                       group = group),
                   fill = "white",
                   color = "grey80",
                   alpha = 1) +
      # Layer 2: Tint
      geom_polygon(data = map_data("usa"),
                   aes(group = group,
                       x = long,
                       y = lat), 
                   fill = "#fd6266", 
                   colour = "grey20",
                   alpha = .6) + 
      # Layer 3: Points
      geom_point(data = coordinate_cities, 
                 aes(x = long,
                     y = lat),
                 colour = "grey10", 
                 pch = 8, # point shape
                 size = 2) + 
      # Layer 4: Text
      geom_text_repel(data = coordinate_cities, 
                      aes(label = city,
                          x = long,
                          y = lat),
                      color = "grey20",
                      size = 7,
                      nudge_x = nudge_x,
                      nudge_y = nudge_y) + 
      coord_equal() + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_blank())
  )
}

if (FALSE) {
  
  coordinate_cities <- data.frame(
    city = c("Las Vegas", 
             "Northwest Arkansas",
             "Minneapolis",
             "Chicago"),
    lat = c(36.18811,
            36.37285,
            44.9866,
            41.7396),
    long = c(-115.1764,
             -94.208817,
             -93.258,
             -87.55442))  
  plotUsCities(coordinate_cities, nudge_x = c(1,0,0,0),
               nudge_y = c(0,0,0,1))
}