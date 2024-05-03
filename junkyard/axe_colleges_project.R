library(googleway)
library(data.table)
library(magrittr)

calculateDistance <- function(lat1, long1, lat2, long2) {
  # Create a matrix of the first set of coordinates
  coord1 <- cbind(long1, lat1)
  
  # Create a matrix of the second set of coordinates
  coord2 <- cbind(long2, lat2)
  
  # Calculate the distance using distHaversine() function
  distance <- geosphere::distHaversine(coord1, coord2) / 1609.344
  
  return(distance)
}

setwd("~/Downloads/axe_colleges_project")

storeDt <- fread("axe_store_list.csv")

collegeDt <- fread("college_list.csv")


api_key <- "AIzaSyCPZ5bKqi5vPvYOuvptLTrHcKAspcC7Xag"

collegeDt[, placeName := ""]
collegeDt[, placeId := ""]
collegeDt[, latitude := 0]
collegeDt[, longitude := 0]

for (i in 1 : nrow(collegeDt)) {
  a_college <- collegeDt$college[i]
  
  placeQuery <- google_place_autocomplete(a_college, key = api_key)
  placeId    <- placeQuery$predictions$place_id[1]
  placeName  <- placeQuery$predictions$description[1]
  
  collegeDt$placeName[i] <- placeName
  collegeDt$placeId[i]   <- placeId
  
  collegeDetails <- google_place_details(place_id = placeId,
                                         key      = api_key)
  
  collegeDt$latitude[i] <- collegeDetails$result$geometry$location$lat
  collegeDt$longitude[i] <- collegeDetails$result$geometry$location$lng
  
  print(placeName)
}

#
caStoreDt <- storeDt[state == "California"]

setkey(caStoreDt, storeId)
setkey(collegeDt, placeId)
crossDt <- caStoreDt[CJ(storeId = caStoreDt$storeId, 
                        placeId = collegeDt$placeId, unique = TRUE), allow.cartesian = TRUE] %>%
  .[collegeDt, on = "placeId"]

crossDt[, dist := calculateDistance(latitude, longitude, i.latitude, i.longitude)]

nearestDt <- crossDt[order(storeId, dist)] %>%
  .[, .(nearest_college = college[1],
        nearest_college_distance = dist[1]),
    keyby = .(storeId, store_type, city, open_date, close_date, full_address,
              unilever_dollars = ul_total_dollars_l52,
              axe_dollars)] %>%
  .[order(-axe_dollars)]



