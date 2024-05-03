library(data.table)
library(magrittr)

createItemLookupDt <- function(pogData, storePogDt, attDt,
                               assignUncleanNames = FALSE) {
  # Check PogData
  requiredColumnNames <- c("display", "id", "position_hfacings", "pog_title")
  checkRequiredColumnNames(pogData, requiredColumnNames)
  pogData <- pogData[, ..requiredColumnNames]
  
  # Check storePogDt
  requiredColumnNames <- c("display", "store")
  checkRequiredColumnNames(storePogDt, requiredColumnNames)
  storePogDt <- storePogDt[, ..requiredColumnNames]
  
  # Check attribution
  requiredColumnNames <- c("dpci", "item_description", "brand")
  checkRequiredColumnNames(attDt, requiredColumnNames)
  attDt <- attDt[, ..requiredColumnNames]
  
  storeItemPogDt <- merge(pogData, 
                          storePogDt[, .(store_tied = .N), by = display],
                          by = "display") %>%
    merge(attDt, by.x = "id", by.y = "dpci")
  
  outputDt <- storeItemPogDt %>%
    .[, .(position_hfacings = sum(position_hfacings)),
      by = .(display, pog_title, id, item_description, brand, store_tied)] %>%
    .[, concat_lookup := paste0(display, id)]
  
  outNames <- c("concat_lookup", "id", "item_description", "brand", 
                "display", "store_tied", "position_hfacings")
  if (assignUncleanNames) {
    newNames <- c("CONCAT LOOKUP", "ID", "Item Description", "Brand",
                  "Display", "Store Count", "Position HFacings")
    setnames(outputDt, outNames, newNames)
    outputDt <- outputDt[, ..newNames]
  } else {
    outNames <- c(outNames, "pog_title")
    outputDt <- outputDt[, ..outNames]
  }
  return(
    outputDt
  )
}

if (FALSE) {
  rm(list = ls(all.names = TRUE))
  setwd("~/r_code/functions")
  sapply(list.files(full.names = TRUE), source)
  
  setwd("~/ic_revision_sandbox")
  pogData <- fread("pog_data.csv") %>% cleanNames %>%
    .[, display := planogram_desc2] %>%
    .[, pog_title := planogram_desc1]
  storePogDt <- fread("cat_ties_unclean_new.csv") %>% cleanTieReport(assignUncleanNames = FALSE)
  attDt <- fread("attribution_data.csv") %>% cleanNames
  
  itemLookupDt <- createItemLookupDt(pogData, storePogDt, attDt)
  fwrite(itemLookupDt, "item_lookup.csv")
}