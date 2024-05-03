dt1 <- loadData("./Target CoTenants.xlsx") %>% cleanNames %>% 
  .[, .(lat_base = latitude, 
        long_base = longitude,
        addressId = full_address)] %>% unique
dt2 <- loadData("./Target STore List - 4.20.23 Update.xlsx") %>% cleanNames %>%
  .[, .(storeId = store, lat_target = latitude, long_target = longitude, store_type)] %>%
  .[!is.na(lat_target)] %>%
  .[!is.na(long_target)] %>% unique

setkey(dt1, addressId)
setkey(dt2, storeId)

distMatrix <- dt1[CJ(dt1$addressId, dt2$storeId)]
setnames(distMatrix, old = "V2", new = "storeId")
setkey(distMatrix, storeId)
distMatrix <- distMatrix[dt2]

# This will take a moment
setkey(distMatrix, long_base, lat_base, long_target, lat_target)
distMatrix[, dist := distVincentySphere(c(long_base, lat_base), c(long_target, lat_target)),
           keyby = .(long_base, lat_base, long_target, lat_target)]

distMatrix <- distMatrix[order(addressId, dist)]

nearestTargetDt <- distMatrix[, .(nearest_target = storeId[1],
                                  dist_in_meters = dist[1],
                                  second_nearest_target = storeId[2],
                                  second_nearest_target_dist = dist[2]),
                              by = .(addressId)]

dt1Out <- merge(dt1, nearestTargetDt, by = "addressId")
