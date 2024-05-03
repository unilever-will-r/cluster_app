library(stringr)
library(dplyr)

# Assuming the conversation is stored in a variable called conversation
# Replace this with the actual conversation string
conversation <- ""

data <- data.table(stadium_city    = conversation[grepl("City\\:", conversation)] %>% gsub("City\\: ", "", .),
                   stadium = conversation[grepl("Stadium\\:", conversation)] %>% gsub("Stadium\\: ", "", .),
                   lat     = conversation[grepl("Latitude\\:", conversation)] %>% gsub("Latitude\\: ", "", .),
                   lon     = conversation[grepl("Longitude\\:", conversation)] %>% gsub("Longitude\\: ", "", .))

data[, lat := as.numeric(lat)]
data[, lon := as.numeric(substr(lon, 0, 8)), by = lon]
data[, lon := as.numeric(lon)]

storeList <- getStoreListDt() %>% .[, .(storeId, store_type, city, state, zip, full_address, 
                                        target_lat = latitude,
                                        target_lon = longitude,
                                        open_date,
                                        close_date,
                                        ul_total_dollars_l52)]



distMatrix <- CJ(storeId = storeList$storeId, stadium = data$stadium)
distMatrix <- merge(distMatrix, data, by = "stadium") %>%
  merge(storeList, by = "storeId")

# calculate the distance to all Targets
distMatrix[, dist_in_miles := distVincentySphere(c(lon, lat), c(target_lon, target_lat)) / 1609,
           keyby = .(lon, lat, target_lon, target_lat)]

output <- distMatrix[dist_in_miles < 10][order(stadium, dist_in_miles)]


setwd("~/Downloads/_test/")
placerKey <- fread("place_data_key.csv")
placerDt <- fread("placer_data.csv")
placerDt <- merge(placerKey, placerDt, by = "id")


# 