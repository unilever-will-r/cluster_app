setwd("C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/00 - Automation & Reporting/PTIQ/2023/Ice Cream Transition/Inputs_NEWDELETETHEOTHERONENOTME")

inputsDir <- file.path(getwd())

attributionDt <- getAttributionDt(config$attributionDtPath, "ICE_CREAM")


inputData <- loadPtiqData(inputsDir)


addColumnForTitleFlags <- function(baseTieDt, targetNames, outColName, prependChars = c(":")) {
  # Add attribute level flags like "warehouse" or "cluster" that are derived from the title on tie reports from domo
  # Assume the attribute level is always prepended with ":" when appearing in the title
  # Assume baseTieDt has a "title" column
  # If no match, NA is returned
  
  # Drop initial column
  requiredColumnNames <- setdiff(names(baseTieDt), outColName)
  tieDtOut <- copy(baseTieDt[, ..requiredColumnNames])
  
  # prepend char can be a vector, so we can handle stuff like "... 5DR:" and "... 5DR:..." with input c(":", " ")
  for (prependChar in prependChars) {
    tagList <- paste0(prependChar, targetNames)
    lapply(tagList, FUN = function(x) tieDtOut[grepl(x, title), new_col := gsub(pattern = prependChar, replacement = "", x)])
  }
  
  tieDtOut[is.na(new_col), new_col := ""]
  
  setnames(tieDtOut, old = "new_col", new = outColName)
  return(tieDtOut)
}

# Add warehouse tags
warehouseNames <- c("HI",
                    "TOH",
                    "TCA",
                    "TFL",
                    "TTX",
                    "TIA",
                    "CSW",
                    "CSE",
                    "CSMW",
                    "CSPNW",
                    "TMD",
                    "AK")
formatNames <- c("PF", 
                 "EXP",
                 "ST",
                 "GM")
clusterNames <- c("FRSK",
                  "MNSTRM",
                  "SBRB",
                  "HSP")
doorNames <- paste0(1 : 50, "DR") %>% c("5 DR")
inputData$tiesDtPre <- inputData$tiesDtPre %>%
  addColumnForTitleFlags(targetNames = warehouseNames, outColName = "warehouse")

inputData$tiesDtPost <- inputData$tiesDtPost %>%
  addColumnForTitleFlags(targetNames = warehouseNames, outColName = "warehouse") %>%
  addColumnForTitleFlags(targetNames = formatNames, outColName = "format") %>%
  addColumnForTitleFlags(targetNames = clusterNames, outColName = "cluster") %>%
  addColumnForTitleFlags(targetNames = doorNames, outColName = "door_count", prependChar = c(":", " "))

preCatTies <- inputData$tiesDtPre
postCatTies <- inputData$tiesDtPost
preJda <- inputData$pogDtPre
postJda <- inputData$pogDtPost


blueIds <- attributionDt[brand == "Blue Bell", unique(itemId)]


warehouseItemPre <- merge(preCatTies[, .(warehouse, pogId, storeId)], preJda[itemId %in% blueIds, .(pogId, itemId)] %>% unique,
                           by = "pogId") %>%
  .[, .(store_count_pre = uniqueN(storeId)), by = .(itemId, warehouse)]
warehouseItemPost <- merge(postCatTies[, .(warehouse, pogId, storeId)], postJda[itemId %in% blueIds, .(pogId, itemId)] %>% unique,
                          by = "pogId") %>%
  .[, .(store_count_post = uniqueN(storeId)), by = .(itemId, warehouse)]

warehouseItemPrePost <- merge(warehouseItemPre, warehouseItemPost, by = c("itemId", "warehouse"), all = TRUE) 

warehouseTotals <- warehouseItemPrePost[, .(warehouse = "AAAATOTAL",
                                            store_count_post = sum(store_count_post),
                                            store_count_pre = sum(store_count_pre)),
                                        by = .(itemId)]

if (FALSE) {
# Output 1: item x warehouse x pre pods x post pods
# shee1: byWarehouse
byWarehouse <- rbind(warehouseItemPrePost,
                     warehouseTotals)%>%
  merge(attributionDt[, .(itemId = as.numeric(itemId), item_description)]) %>%
  .[, net_store_change := store_count_post - store_count_pre] %>%
  .[, item_id := gsub("28807", "", itemId)] %>%
  .[, description := paste0("288-07-", item_id) %>% gsub(pattern = ., replacement = "", x = item_description), by = item_id] %>%
  .[order(itemId, warehouse), .(itemId, 
                                Dept = 288, 
                                Class = 7,
                                Item = item_id,
                                description, 
                                Brand = "Blue Bell",
                                Vendor = "BLUE BELL CREAMERIES, INC",
                                Warehouse = warehouse,
                                store_count_pre,
                                store_count_post,
                                net_store_change)]

# sheet2: itemsByStore
storeItemPrePost <- buildPrePostStoreItemDt(inputData, attributionDt[itemId %in% blueIds]) %>%  
  merge(attributionDt[, .(itemId, item_description)], by = "itemId") %>%
  .[, item_id := gsub("28807", "", itemId)] %>%
  .[, description := paste0("288-07-", item_id) %>% gsub(pattern = ., replacement = "", x = item_description), by = item_id] %>%
  merge(postCatTies[, .(warehouse, storeId, pogId)], by = "storeId", all.x = TRUE) %>%
  .[, .(itemId, 
        Dept = 288, 
        Class = 7,
        Item = item_id,
        dpci = itemId,
        description, 
        Segment = "01 Ice Cream",
        Brand = "Blue Bell",
        Manuf = "BLUE BELL CREAMERIES, INC",
        pod_pre, 
        pod_post,
        pogId,
        warehouse,
        storeId)] %>%
  .[, dpciPlusStoreLookup := paste0("288-07-", Item, storeId)] %>% 
  .[order(itemId, warehouse, storeId)]


# DSD Report - POG SUMMARY FACINGS

# 1: PRE COUNTS
preCounts <- storeItemPrePost[pod_pre == 1, .(dpciPlusStoreLookup, pod_pre)]

# 2: POST COUNTS
postCounts <- storeItemPrePost[pod_post == 1, .(dpciPlusStoreLookup, pod_post)]
# 3: sheet1
sheet1 <- merge(postCatTies, getStoreListDt()[, .(storeId = as.integer(storeId), city, state)]) %>%
  .[, .(storeId, pogId, title, format, state, city, cluster, door_count)]
sheet1 <- postCatTies[, .(storeId, pogId, title, format, )]
# 4: STORE_DROPDOWN
storeDropDown <- data.table(STORE = unique(sheet1$storeId))

# facings summary
storeItemPost <- merge(postCatTies, postJda[itemId %in% blueIds, .(n_facings = sum(position_hfacings)), keyby = .(itemId, pogId)], by = "pogId")
facingsPost <- storeItemPost[, .(stores_tied = uniqueN(storeId)),
                             keyby = .(itemId, pogId, title,cluster, door_count, format, facings = n_facings)]


# top half
facingsPost[, .(itemId, pogId, title, cluster, door_count, format)] %>% t

# bottom half
dcast(facingsPost, itemId~pogId, value.var = "facings")


# totals
storeItemPrePost

}
