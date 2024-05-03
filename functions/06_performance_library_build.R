buildTraitedSalesDt <- function(storeItemSalesDt,
                                storePogDt,
                                pogItemDt) {
  
  # Removes rows from the sales file that aren't "on-pog" items according to the inputs
  # store universe: storePogDt[, unique(store)]
  # item universe: pogItemDt[, unique(positionId)]
  
  requiredColumnNames <- c("storeId", "positionId", "upspw", "dpspw")
  checkRequiredColumnNames(storeItemSalesDt, requiredColumnNames = requiredColumnNames)
  storeItemSalesDt <- storeItemSalesDt[, ..requiredColumnNames]
  
  requiredColumnNames <- c("storeId", "pogId")
  checkRequiredColumnNames(storePogDt, requiredColumnNames = requiredColumnNames)
  storePogDt <- storePogDt[, ..requiredColumnNames]
  
  requiredColumnNames <- c("pogId", "positionId")
  checkRequiredColumnNames(pogItemDt, requiredColumnNames = requiredColumnNames)
  pogItemDt <- pogItemDt[, ..requiredColumnNames]
  
  setkey(storePogDt, pogId)
  setkey(pogItemDt, pogId)
  storeItemDt <- pogItemDt[storePogDt, allow.cartesian = TRUE, .(positionId, storeId)]
  
  setkey(storeItemSalesDt, storeId, positionId)
  setkey(storeItemDt, storeId, positionId)
  
  traitedSalesDt <- storeItemSalesDt[storeItemDt, nomatch = NULL]
  
  return(
    traitedSalesDt
  )
}



buildPerfLibFromSalesDt <- function(config,
                                    storeItemSalesDt,
                                    prePeriodStorePogDt,
                                    prePeriodPogItemDt,
                                    imputeItemList = c()) {
  # Performance library build
  # input:
  #     storeItemSalesDt: needs to be all stores, all items
  #     prePeriodStorePogDt: 2 columns - c("storeId", "pogId")
  #     prePeriodPogItemDt: 2 columns: c("positionId", "pogId")
  # (from config)
  #     storePogDt: c("storeId", "pogId")
  #     attributionDt: c("itemId", "subclass", "brand")
  # OUTPUT
  # table keyed by pogIdUniverse x itemUniverse with movement variables
  # - pogId universe: all pogs in storePogDt (path from config)
  # - itemUniverse: all items in the attributionDt
  # the movement is forecasted upspw for that item in the stores that POG is traited to
  
  # Read in needed data from config
  storePogDt <- fread(config$cleanTieReportPath) %>% .[, .(storeId, pogId)]
  attributionDt <- getAttributionDtFromConfig(config) %>%
    .[, impute_item_flag := fifelse(itemId %in% imputeItemList, 1, 0), by = itemId] %>%
    .[, subclass := form]
  if (attributionDt[, sum(impute_item_flag, na.rm = TRUE)] != length(imputeItemList)) {
    stop("There's at least one unattributed new item.")
  }
  setnames(attributionDt, old = "itemId", new = "positionId")
  
  # Clarify universe for filtering to traited sales
  storeUniverse <- storePogDt[, unique(storeId)]
  pogUniverse <- storePogDt[, unique(pogId)]
  itemUniverse <- attributionDt[, unique(positionId)]
  
  # Sales ETL - filter to traited items, stores getting POGs 
  requiredColumnNames <- c("storeId", "positionId", "upspw", "dpspw")
  checkRequiredColumnNames(storeItemSalesDt, requiredColumnNames)
  
  # Treat an item with an NA in the sales file as not traited for that store
  traitedSalesDt <- buildTraitedSalesDt(storeItemSalesDt, 
                                        prePeriodStorePogDt[storeId %in% storeUniverse],
                                        prePeriodPogItemDt[positionId %in% itemUniverse]) %>%
    .[!is.na(upspw)]
  
  
  # Add on the subclass and brands for merging
  setkey(traitedSalesDt, positionId)
  setkey(attributionDt, positionId)
  traitedSalesDt <- attributionDt[traitedSalesDt]
  # re-calculate itemUniverse so that we are omitting items with no valid sales history
  
  # Join historical sales to each current planogram
  # because of A/B POGs, we will be double counting sales 
  setkey(traitedSalesDt, storeId)
  setkey(storePogDt, storeId)
  storeItemSalesDtMerged <- storePogDt[traitedSalesDt, allow.cartesian = TRUE]
  
  # skeleton pogIdUniverse x itemUniverse
  # going to mergeon pog_item upspw, brand upspw, subclass upspw, 
  outputSkeleton <- data.table(expand.grid(pogId = pogUniverse,
                                           positionId = itemUniverse)) %>%
    merge(attributionDt[, .(positionId, brand, subclass)]) %>%
    merge(storePogDt[, .(store_count = uniqueN(storeId)),
                     by = pogId], by = "pogId", all.x = TRUE)
                     
  # 1. Item level movement
  pogItemSalesDt <- storeItemSalesDtMerged[, .(upspw = sum(upspw),
                                               dpspw = sum(dpspw),
                                               pods = uniqueN(storeId)),
                                           keyby = .(pogId, positionId, subclass, brand)] %>%
    .[, ':='(upspw = upspw / pods,
             dpspw = dpspw / pods)]
  setkey(outputSkeleton, pogId, positionId, subclass, brand)
  outputSkeleton <- pogItemSalesDt[outputSkeleton]
  
  # If an item doesn't have sales in the pre period, then we impute with the brand-subclass
  pogSubclassBrandSalesDt <- pogItemSalesDt[, .(brand_upspw = sum(upspw),
                                                brand_dpspw = sum(dpspw),
                                                brand_pods = sum(pods)),
                                            keyby = .(pogId, subclass, brand)] %>%
    .[, ':='(brand_avg_upspw = brand_upspw / brand_pods,
             brand_avg_dpspw = brand_dpspw / brand_pods)]
  setkey(outputSkeleton, pogId, subclass, brand)
  outputSkeleton <- pogSubclassBrandSalesDt[outputSkeleton]
  
  # if no brand-subclass sales, impute with subclass sales
  pogSubclassSalesDt <- pogItemSalesDt[, .(subclass_upspw = sum(upspw),
                                           subclass_dpspw = sum(dpspw),
                                           subclass_pods = sum(pods)),
                                       keyby = .(pogId, subclass)] %>%
    .[, ':='(subclass_avg_upspw = subclass_upspw / subclass_pods,
             subclass_avg_dpspw = subclass_dpspw / subclass_pods)]
  setkey(pogItemSalesDt, pogId, subclass)
  outputSkeleton <- pogSubclassSalesDt[outputSkeleton]
  
  outputSkeleton[, coalesced_upspw := fcoalesce(upspw,
                                                brand_avg_upspw,
                                                subclass_avg_upspw)]
  
  outputSkeleton[, coalesced_dpspw := fcoalesce(dpspw,
                                                brand_avg_dpspw,
                                                subclass_avg_dpspw)]
  
  return(
    outputSkeleton
  )
}  

if (FALSE) {
  
  rm(); gc()
  setwd("~/r_code")
  source("00_initialize.R")
  
  config <- initializeProject(configPath = "~/r_code/config.yaml")
  
  config$baseDir <- "~/Downloads/squatch_revision"
  config$attributionTableSheetName <- "PW_MALE"
  config$cleanTieReportPath <- file.path(config$baseDir, "storePogDt.csv")
  
  # 
  storeItemSalesDt <- fread(file.path(config$baseDir, "storeItemSales.csv"))
  prePeriodStorePogDt <- fread(file.path(config$baseDir, "PRE CAT TIES.csv")) %>%
    .[storeId %in% storeItemSalesDt[, unique(storeId)]]
  prePeriodPogItemDt <- fread(file.path(config$baseDir, "PRE JDA DATA.csv")) %>% cleanNames() %>%
    .[, .(positionId = id, pogId = planogram_desc2)] %>% unique %>%
    .[pogId %in% prePeriodStorePogDt[, unique(pogId)]]
  postPeriodStorePogDt <- fread(file.path(config$baseDir, "storePogDt.csv"))
  newPerformanceLibrary <- buildPerfLibFromSalesDt(config = config,
                                                   storeItemSalesDt = storeItemSalesDt,
                                                   prePeriodStorePogDt = prePeriodStorePogDt,
                                                   prePeriodPogItemDt = prePeriodPogItemDt)

  perfLibForOutput <- newPerformanceLibrary[, .(`Planogram Alias` = pogId,
                                                `Nbr Locations` = store_count,
                                                ID = positionId,
                                                `Value 39` = coalesced_upspw * 52,
                                                `Value 38` = coalesced_dpspw * 52,
                                                `Nbr Weeks` = 52,
                                                `Unit Movement` = coalesced_upspw,
                                                `Value 40` = coalesced_dpspw)]
                                                
  fwrite(perfLibForOutput, file.path(config$baseDir, "perfLibForOutput.csv"))
  # Ice cream example
  setwd(config$baseDir)
  # Pull in this data when you do a performance library for a revision
  # 1. sales --- upspw per item x store for 26 week pre period
  storeItemSalesDt <- fread(file.path(config$baseDir, "PERF_LIB_WORKFLOW/Data Pulls/2022_last26_sales_data.csv"))
  # 2. historic ties - on the day of your sales pull, pull a tie report of the POGs in stores that day
  prePeriodStorePogDt <- fread(gsub("raw", "historic", config$rawTieReportPath)) %>% cleanNames() %>% unique %>% 
    .[grepl("ICE", description)] %>%
    setnames(old = c("store_number", "description", "display"), 
             new = c("storeId", "title", "pogId")) %>% 
    .[set_date > "2022-08-01", .(storeId, pogId)] %>% unique()
  # 3. pogItemDt for all the pogIds in historic ties
  prePeriodPogItemDt <- fread(file.path(config$filesDir, "pre_period_pog_item.csv")) %>%
    .[, .(positionId, pogId)] %>% unique
  
  system.time(
    newPerformanceLibrary <- buildPerfLibFromSalesDt(config = config, storeItemSalesDt = storeItemSalesDt, prePeriodStorePogDt, prePeriodPogItemDt)
  )

  }