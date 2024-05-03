
if (FALSE) {
  # ICE CREAM PERFORMANCE LIBRARY STEP-BY-STEP
  rm(list = ls(all.names = TRUE)); gc();
  setwd("~/r_code")
  source("00_initialize.R")
  config <- initializeProject(configPath = "~/r_code/config.yaml")
  
  setwd("~/Downloads/")
  
  # STEP 1: Get the historical ties from DOMO
  # histTies is from Historical Ties Report
  # lists all POGs with set date less than current date
  icRawHistTiesDt  <- rbind(fread("C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/06 - Ice Cream/2024/04.28 April Transvision/03 - Tie Reports/ic_hist_ties_pulled_10_31_2023.csv"),
                            fread("C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/06 - Ice Cream/2024/04.28 April Transvision/03 - Tie Reports/ic_hist_ties_2022.csv"))
  storeItemHistDt <- cleanHistTiesDt(cleanNames(icRawHistTiesDt))
  
  # STEP 2: Get the current POGs'
  # currStorePog: Category Tie Report for the POGs currently set in stores
  # currPogItem: store-item pog data, have to pull this from the planograms
  icCurrStorePogDt <- fread("C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/00 - Automation & Reporting/PTIQ/2023/Ice Cream - October Revision/Inputs/POST CAT TIES.csv")
  icCurrPogItemDt  <- fread("C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/00 - Automation & Reporting/PTIQ/2023/Ice Cream - October Revision/Inputs/POST JDA DATA.csv")
  storeItemCurrDt <- createCurrTiesDt(cleanNames(icCurrPogItemDt),
                                      cleanNames(icCurrStorePogDt))
  
  # STEP 3: glue the historic and current files together such that:
  # - any overlaps are corrected so that no weeks are double-counted
  # - any gaps between the historic end date and the current start date are nudged backwards ("giving the gap" to the current period)
  #
  storeItemDt <- glueCurrToHistDt(currDt = storeItemCurrDt,
                                  histDt = storeItemHistDt) %>%
    .[, ':='(startDate = startWeek,
             endDate = endWeek,
             startWeek = dateToWeekKey(as.Date(as.character(startWeek), format = "%Y%m%d")),
             endWeek = dateToWeekKey(as.Date(as.character(endWeek), format = "%Y%m%d"))),
      by = .(startWeek, endWeek)]
  # STEP 4: More logic applied to get store-item table (condensing consecutive traited periods into single start/end period)
  fixedStoreItemDt <- fixStoreItemPogDt(storeItemDt)
  
  # STEP 5: WRITE TO target blob storage, run `Performance Library Sales Notebook` to get traited movement data
  if (FALSE) {
    fwrite(fixedStoreItemDt, "~/Downloads/icStoreItemDt_2022_2023.csv")
  }
  
  # STEP 6: Read in traited sales and current store ties
  icSalesDt <- fread("C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/06 - Ice Cream/2024/04.28 April Transvision/03 - Tie Reports/ic_store_item_dsw.csv")
  icStoreTies <- fread("C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/06 - Ice Cream/2024/04.28 April Transvision/03 - Tie Reports/ice_cream_cat_ties.csv") %>%
    cleanNames %>%
    .[, .(storenumber = store, pogId = display)]
  
  
  # STEP 7: Generate performance library from these two tables
  # For any given planogram-item pair, stores with no sales of given item do not contribute to store count of given planogram
  # This is enforced by the merge
  pogItemSalesDt <- merge(icSalesDt, icStoreTies, by = "storenumber") %>%
    .[, .(total_dollars = sum(salesdollar),
          total_units = sum(salesunit),
          total_weeks = mean(n_traited_weeks),
          total_stores = uniqueN(storenumber)),
      by = .(pogId, itemnumber)] %>% 
    .[, usw := total_units / (total_stores * total_weeks)] %>%
    .[, dsw := total_dollars / (total_stores * total_weeks)]
  
  perfLibForOutput <- pogItemSalesDt[, .(`Planogram Alias` = pogId,
                                         `Nbr Locations` = total_stores,
                                         ID = itemnumber,
                                         `Value 39` = usw * 52,
                                         `Value 38` = dsw * 52,
                                         `Nbr Weeks` = 52,
                                         `Unit Movement` = usw,
                                         `Value 40` = dsw)]
  
  perfLibForWriting <- pogItemSalesDt[, .(planoId = pogId,
                                          itemId  = itemnumber,
                                          usw,
                                          dsw,
                                          n_stores = total_stores,
                                          n_weeks  = total_weeks)]
  if (FALSE) {
    fwrite(perfLibForOutput, "~/Desktop/performance_libraries/icPerfLibrary_latest.csv")
    fwrite(perfLibForWriting, "C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/06 - Ice Cream/2024/04.28 April Transvision/03 - Tie Reports/performance_library_latest.csv")
  }
  
  # STEP 8: Get Product library built
  # step a. read in current product library
  icProdLibPath <- "C:/Users/William.Roberts/Desktop/performance_libraries/IC Product Library.mdb"
  currentProdDt <- readProductLibrary(icProdLibPath)
  # step b. read in new item dimension file
  newItemDims <- fread("C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/06 - Ice Cream/2024/04.28 April Transvision/03 - Tie Reports/new_item_dims.csv")
  # step c. rbind new item dimensions to current product library
  newItemProductLib <- newItemDims[, .(ID = dpci,
                                       Brand = Brand,
                                       Subcategory = Subclass,
                                       Name = `Item Description`,
                                       Height = height,
                                       Width = width,
                                       Depth = depth)]
  
  # because of recycled dpcis, need to filter before joining
  newProductLib <- currentProdDt[!(ID %in% newItemProductLib$ID)] %>% 
    rbind(newItemProductLib, fill = TRUE)
  if (FALSE) {
    # new items only
    fwrite(newItemProductLib, "C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/06 - Ice Cream/2024/04.28 April Transvision/03 - Tie Reports/newProductDimensions.csv")
    fwrite(newProductLib, "C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/06 - Ice Cream/2024/04.28 April Transvision/03 - Tie Reports/icProdLibLatest.csv")
    writeProductLibrary(newProductLib, icProdLibPath)
    
  }
  
  # step d. save it down as the latest version
  
  
  # STEP 9: (DONE IN SHELF IQ) UPDATE THE POGS
  
  
}