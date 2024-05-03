library(data.table)
library(magrittr)

cleanTieReport <- function(rawTieReport, dateFilter = as.Date("2999-01-01")) {
  
  dateFormat <- "%d-%b-%Y"
  dateWindow <- 28
  
  setnames(rawTieReport, new = tolower(names(rawTieReport) %>% gsub(pattern = " ", replace = "_", .)))
  rawTieReport <- rawTieReport %>%
    .[, .(store, 
          display, 
          set_date         = as.Date(set_date, format = dateFormat),
          discontinue_date = as.Date(discontinue_date, format = dateFormat),
          new_remodel_code)] %>%
    .[set_date < dateFilter]
  
  # Anchor set date is assumed to be the one that is the furthest date in the future for the most stores
  anchorSetDate <- rawTieReport %>%
    .[, .(set_date = max(as.Date(set_date))), by = store] %>%
    .[, .N, by = set_date] %>%
    .[order(-N), set_date] %>% 
    .[1]
  rawTieReport[, anchor_set_date := anchorSetDate]
  
  # For each store, figure out the nearest set date to anchor
  rawTieReport[, days_from_anchor := set_date - anchor_set_date, by = set_date]
  storeSkeleton <- rawTieReport[, .SD[which.min(abs(days_from_anchor))], by = .(store)] %>%
    .[, .(store, set_date, dist_from_anchor = abs(days_from_anchor))] %>% unique()
  
  # Join store skeleton back to ties to join A/B POGs
  rawTieReport <- merge(storeSkeleton, rawTieReport, by = c("store", "set_date")) %>%
    .[, in_window_flag := fifelse(dist_from_anchor <= dateWindow, 1, 0), by = dist_from_anchor]
  
  return(
    rawTieReport[in_window_flag == 1]
  )
}

if (FALSE) {
  rm(list = ls(all.names = TRUE))
  setwd("~/r_code/functions")
  sapply(list.files(full.names = TRUE), source)
  
  setwd("~/mens_zone_item_compare")
  
  
  # by item
  rawMensByItem <- fread("uncleaned_ties_to_d49_by_item.csv")
  
  
  rawMensDt <- fread("uncleaned_ties_to_d49_items.csv")
  cleanDt <- cleanTieReport(rawMensDt, dateFilter = "2022-04-01")
  
  rawMensDtPrecleaned <- fread("mz_cat_ties_precleaned.csv")
  cleanPrecleanedDt <- cleanTieReport(rawMensDt)
  
  rawTextDt <- fread("category_ties_textured_hair.csv")
  cleanTextDt <- cleanTieReport(rawTextDt, dateFilter = "2022-04-01")
  
  oldDt <- fread("old_cat_ties.csv") %>% .[, 1 : 15]
  setnames(oldDt, new = tolower(gsub(" ", "_", names(oldDt))))
  
  mergeToCompare <- merge(cleanDt[, .(store, display, set_date, clean = 1)],
                          oldDt[, .(store, display, set_date, old = 1)],
                          all = TRUE,
                          by = c("store", "display"))
  
  # 1. match - pile. stores that are in both with same POGs
  exactDt <- mergeToCompare[!(is.na(clean) | is.na(old))]
  # 2. missing stores from cleanDt
  rowsNotInCleanDt <- mergeToCompare[is.na(old) & !is.na(clean)]
  # 3. missing stores from oldDt
  rowsNotInOldDt <- mergeToCompare[!is.na(old) & is.na(clean)]
  # 3. stores with differing POGs across files
  inBoth <- rowsNotInOldDt[(store %in% rowsNotInCleanDt[, store]), unique(store)]
  notInOldDt[!(store %in% inBoth)]
  notInCleanDt[!(store %in% inBoth)]
  
  
  
  
}


output <- list();
iterat <- 1
for (i in unique(rawMensByItem[, DPCI])) {
  output[[iterat]] <- cleanTieReport(rawTieReport = rawMensByItem[DPCI == i], dateFilter = "2022-05-01")[, DPCI := i]
  iterat <- iterat + 1
}
finalOutput <- output %>% rbindlist

cleanStoreList <- finalOutput[DPCI == "049-50-0354"][, unique(store)]
rawStoreList <- rawMensByItem[DPCI == "049-50-0354"]



ogList <- fread("mens_zone_store_item_list_original.csv")
newList <- finalOutput[, .(in_new_list = "YES"), by = .(STORE = store, ID = as.numeric(gsub(pattern = "-", replacement = "", x = DPCI)))]
mergedList <- merge(ogList[, in_og_list := "YES"], newList, by = c("STORE", "ID"), all = TRUE)


storeItemD49 <- finalOutput[, .(d49_n = .N), by = .(store, DPCI, set_date, discontinue_date)]
storeItemD37 <- fread("textured_hair_store_item_list.csv")

mergedList <- merge(storeItemD49[, .(is_49 = 1, store, as.numeric(gsub("-", "", DPCI)))] %>% unique,
                    storeItemD37[, .(is_37 = 1, store, as.numeric(gsub("-", "", DPCI)))] %>% unique,
                    all = TRUE)
