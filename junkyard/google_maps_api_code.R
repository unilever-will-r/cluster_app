calculateDistance <- function(lat1, long1, lat2, long2) {
  # Create a matrix of the first set of coordinates
  coord1 <- cbind(long1, lat1)
  
  # Create a matrix of the second set of coordinates
  coord2 <- cbind(long2, lat2)
  
  # Calculate the distance using distHaversine() function
  distance <- distHaversine(coord1, coord2) / 1609.344
  
  return(distance)
}

api_key <- "AIzaSyBbF5ejKxBcQPWZiCh-kE_ag4mljkH0QH0"

collegesDt <- fread("~/Downloads/college_stores_dt.csv")
collegesDt[, storeId := gsub("T", "", LOCATION.NUMBER)]
collegeNames <- collegesDt$COLLEGE.NAME

collegeStoreLatLongs <- getStoreListDt() %>%
  merge(collegesDt[, .(storeId = as.numeric(storeId), collegeName = COLLEGE.NAME)]) %>%
  .[storeId == 3421, ':='(latitude = 32.607480, longitude = -85.482480)]




output <- copy(collegeStoreLatLongs)
output[, c_lat := 999]
output[, c_long := 999]

for (i in 1 : nrow(collegeStoreLatLongs)) {
  t_lat  <- collegeStoreLatLongs[i, latitude]
  t_long <- collegeStoreLatLongs[i, longitude]
  c_name <- collegeStoreLatLongs[i, collegeName]
  
  collegeAutocompleteResults <- google_place_autocomplete(place_input = c_name,
                                                          location    = c(t_lat, t_long),
                                                          key         = api_key)
  
  collegePlaceId <- collegeAutocompleteResults$predictions$place_id[1]
  
  collegeDetails <- google_place_details(place_id = collegePlaceId,
                                         key      = api_key)
  
  output$c_lat[i]  <- collegeDetails$result$geometry$location$lat
  output$c_long[i] <- collegeDetails$result$geometry$location$lng
}
output[, distance := calculateDistance(latitude, 
                                       longitude, 
                                       c_lat,
                                       c_long),
       by = .(latitude, 
              longitude,
              c_lat, 
              c_long)]
output[, coords := paste0(c_lat, ", ", c_long)]

