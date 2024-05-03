# DMCV
rm(); gc()
setwd("~/r_code")
source("00_initialize.R")

setwd(config$baseDir)

##########################################################################################################################
#     CLEAN STORES TIES                              ##                   CLEAN STORE TIES                               #
##########################################################################################################################
setDate <- "2023-05-07"

rawTieDt <- fread(config$rawTieReportPath) %>% cleanNames() %>% unique %>% 
  .[, ':='(discontinue_date = as.Date(discontinue_date, "%m/%d/%Y"),
           set_date         = as.Date(set_date, "%m/%d/%Y"))]
currentTieDt <- buildTieReport(rawTieDt)
storePogDt <- currentTieDt[, .(storeId = store,
                               pogId = display,
                               title,
                               width, depth, height)] %>%
  .[!((pogId == "J212LZZ") & (storeId == 963))]

condPogs <- storePogDt[grepl("COND", title), pogId] %>% unique


##########################################################################################################################
#    DATA INGESTION                                 ##                        DATA INGESTION                             #
##########################################################################################################################
attributionDt <- fread(config$attributionDtPath)
setnames(attributionDt,
         old = c("Brand", "Subclass", "Item Description"),
         new = c("brandId", "subcatId", "item_description"))

positionDt <- readPositionTable(config = config, tag = "may23_")
fixtureDt <- readFixtureTable(config = config, tag = "may23_")
pogDt <- readPlanogramTable(config = config, tag = "may23_")

notcoDt <- fread(file.path(config$filesDir, "notco_store_list.csv"))
hspStoreList <- fread(file.path(config$filesDir, "dmcv_hsp_stores.csv")) %>% .[, unique(storeId)]
westernStoreList <- fread(file.path(config$filesDir, "westernStoresDt.csv")) %>% .[, unique(storeId)]
storePogDt[, ':='(is_vinegar = grepl("VIN", title),
                  is_hw      = grepl("HLT", title),
                  is_hisp    = fifelse(storeId %in% hspStoreList, 1, 0),
                  is_notco   = fifelse(storeId %in% notcoDt$storeId, 1, 0),
                  is_midwest = fifelse(storeId %in% westernStoreList, 1, 0))]

perfLibDt <- fread(file.path(config$filesDir, "11_29_2022_perf_lib.csv")) %>%
  cleanNames() %>%
  .[, .(pogId = planogram_alias, positionId = id, value_38, unit_movement, value_39, value_40)] %>% 
  unique

setnames(perfLibDt, 
         old = c("value_38", "value_39", "value_40", "unit_movement"),
         new = c("total_dollars", "total_units", "dpspw", "upspw"))
perfLibDt[, eupspw := .5*upspw + .5*dpspw]

# Initialize
configs <- initializeConfigs()
positionDtOriginal <- copy(positionDt)
positionDt <- buildPositionDt(configs = configs,
                              positionDt = positionDt,
                              attributionDt = attributionDt,
                              perfLibDt = perfLibDt,
                              storePogDt = storePogDt)
if (TRUE) {
  

  
  ##########################################################################################################################
  #                         SWAPS                             ##                       SWAPS                               #
  ##########################################################################################################################
  swapsOut <- c(212080942, 212080943)
  swapsIn  <- c(212080617, 212080615)
  configs <- updateConfigsSwap(configs = configs, 
                               actionPogs = positionDt[positionId == swapsOut[1], unique(pogId)],
                               existingItemId = swapsOut[1], newItemId = swapsIn[1])
  positionDt <- buildPositionDt(configs = configs,
                                positionDt = positionDt,
                                attributionDt = attributionDt,
                                perfLibDt = perfLibDt,
                                storePogDt = storePogDt)
  configs <- updateConfigsSwap(configs = configs, 
                               actionPogs = positionDt[positionId == swapsOut[2], unique(pogId)],
                               existingItemId = swapsOut[2], newItemId = swapsIn[2])
  positionDt <- buildPositionDt(configs = configs,
                                positionDt = positionDt,
                                attributionDt = attributionDt,
                                perfLibDt = perfLibDt,
                                storePogDt = storePogDt)
  
  ##########################################################################################################################
  #                         DELETES                         ##                         DELETES                             #
  ##########################################################################################################################
  deletesDt <- fread(file.path(config$configsDir, "deleteDt.csv"))
  # global deletes - strip out swaps
  deletesDt <- deletesDt[!(positionId %in% swapsOut)]
  for (deleteItem in deletesDt$positionId) {
    configs <- updateConfigsDelete(configs = configs, 
                                   actionPogs = positionDt[positionId == deleteItem, unique(pogId)],
                                   deleteItem = deleteItem)
  }
  

  ##########################################################################################################################
  #                         DECREASES                         ##                       DECREASES                           #
  ##########################################################################################################################
  # 1. Alessi Vinegar 8.5 oz in only california stores
  alessiItemId <- 212080539
  caStores <- getStoreListDt()[state == "CA", unique(storeId)]
  storePogDt[, is_in_ca := fifelse(storeId %in% caStores, 1, 0)]
  storePogDt[, has_ca := max(is_in_ca), by = pogId]
  caPogs <- storePogDt[is_in_ca == 1, unique(pogId)]
  
  storesToKeepIn <- positionDt[positionId == alessiItemId & (pogId %in% caPogs)]
  storesToTakeFrom <- positionDt[positionId == alessiItemId & !(pogId %in% caPogs)]
  
  pogsToTakeFrom <- storesToTakeFrom[, unique(pogId)]
  
  configs <- updateConfigsDelete(configs = configs, 
                                 actionPogs = pogsToTakeFrom,
                                 deleteItem = alessiItemId)
  
  # Add into remaining california stores
  storesToAddInto <- leftOnly(caStores, storePogDt[pogId %in% storesToKeepIn$pogId, unique(storeId)])
  itemGroupIds <- attributionDt[subcatId == "10 VINEGAR", positionId]
  storeCount <- 900
  configs <- updateConfigsGenericIncrease(itemId = alessiItemId,
                                          eligibleStores = storesToAddInto,
                                          storeCount = 99999,
                                          configs = configs,
                                          positionDt = positionDt,
                                          attributionDt = attributionDt,
                                          storePogDt = storePogDt,
                                          itemGroupIds = itemGroupIds)
  
  # 2. pompeian go down to 1000 stores
  pompeianId <- 212080146
  activePositions <- positionDt[positionId == pompeianId]
  activePogs <- activePositions[, unique(pogId)]
  activeStores <- storePogDt[pogId %in% activePogs] %>% .[, .(store_count = uniqueN(storeId)), by = pogId] %>%
    merge(activePositions, by = "pogId")
  # SET THIS NUMBER
  keeperPogs <- activeStores[brand_s2s_index > 103, unique(pogId)]
  removalPogs <- leftOnly(activePogs, keeperPogs)
  configs <- updateConfigsDelete(configs = configs, actionPogs = removalPogs,
                                 deleteItem = pompeianId)
  
  # 3. two primal kitchen skus need to decrease 
  primalDec1 <- 212080695 # get from 667 to 450
  primalDec2 <- 212080701 # get from 815 to 790
  primalDecs <- c(primalDec1, primalDec2)
  
  activeCopositions <- positionDt[positionId %in% primalDecs] %>%
    .[, N := .N, by = pogId] %>% .[N == 2]
  coPogs <- activeCopositions[, unique(pogId)]
  activeCoPogs <- positionDt[positionId %in% primalDecs] %>%
    .[pogId %in% coPogs, .(pogId, positionId, store_count, item_s2s, brand_s2s, item_s2s_index, brand_s2s_index)]
  
  # Choose pogs to remove 1 to get to 450 - i.e. at current POD 667, need to delete from 217
  activeDt1 <- activeCoPogs[positionId == primalDec1]
  delete1 <- activeDt1[order(-brand_s2s_index)][cumsum(store_count) < 217, pogId]
  
  # Choose pogs from these to delete off primalDec2 as well - need 25 deletes
  activeDt2 <- activeCoPogs[positionId == primalDec2 & pogId %in% delete1]
  delete2 <- activeDt2[order(-brand_s2s_index)][cumsum(store_count) < 25, pogId]
  
  configs <- updateConfigsDelete(configs = configs, actionPogs = delete1, 
                                 deleteItem = primalDec1)
  configs <- updateConfigsDelete(configs = configs, actionPogs = delete2,
                                 deleteItem = primalDec2)
  
  # 4. Newman's own items two rules
  # 1. Keep only in super targets (212/242)
  # 2. Keep also in stores where newmans will leave a whole shelf empty
  newmansItems <- c(212080255, 212080074, 212080071, 212080449, 212080280)
  newmansBalsamic <- c(212080071, 212080449)
  superTargets <- getStoreListDt()[store_type == "SuperTarget", storeId]
  storePogDt[, is_super := fifelse(storeId %in% superTargets, 1, 0)]
  superPogs <- storePogDt[, .(N = uniqueN(is_super), is_super = max(is_super)), by = .(pogId)] %>%
    .[N == 1] %>% .[is_super == 1, unique(pogId)]
  
  newmansPositions <- positionDt %>%
    .[positionId %in% newmansItems]
  
  newmansHolePogs <- newmansPositions[, .(worst_case = min(brands_per_shelf)), by = pogId] %>%
    .[worst_case == 1, unique(pogId)]
  
  protectedPogs <- c(superPogs, newmansHolePogs) %>% unique
  
  for (newmansItem in newmansItems) {
    traitedPogs <- positionDt[positionId == newmansItem, unique(pogId)]
    removalPogs <- leftOnly(traitedPogs, protectedPogs)
    configs <- updateConfigsDelete(configs = configs,
                                   actionPogs = removalPogs,
                                   deleteItem = newmansItem)
  }
  
  
  ##########################################################################################################################
  #                         increases                         ##                         increases                         #
  ##########################################################################################################################
  # 1. putting this item everywhre except CA
  itemId <- 212080614
  eligibleStores <- storePogDt[is_in_ca == 0, unique(storeId)]
  storesCurrentlyIn <- positionDt[positionId == itemId, .(pogId)] %>% unique %>%
    merge(storePogDt, by = "pogId") %>%
    .[, unique(storeId)]
  actionStores <- leftOnly(eligibleStores, storesCurrentlyIn)
  actionStoreOnlyPogs <- leftOnly(storePogDt[storeId %in% actionStores, unique(pogId)], storePogDt[!(storeId %in% actionStores), unique(pogId)])
  itemGroupIds <- attributionDt %>%
    .[brandId == attributionDt[positionId == itemId, brandId]] %>%
    .[subcatId == attributionDt[positionId == itemId, subcatId], positionId]
  
  actionPogs <- storePogDt[pogId %in% actionStoreOnlyPogs, unique(pogId)] %>%
    intersect(positionDt[positionId %in% itemGroupIds, unique(pogId)])
  if (storePogDt[pogId %in% actionPogs, .N, by = storeId][N > 1, .N] > 0) {
    stop("itemGroupIds dually located in some store")
  }
  pogRankDt <- positionDt[(positionId %in% itemGroupIds) & (pogId %in% actionPogs),
                          .(brand_s2s_index, pogId, store_count)] %>% 
    unique %>%
    .[order(brand_s2s_index)] %>%
    .[, cumsum := cumsum(store_count)]
  actionStores <- storePogDt[pogId %in% pogRankDt[, pogId], storeId]
  configs <- updateConfigsInsertItemsByPogList(configs = configs,
                                               actionStores = actionStores,
                                               newItemIds = itemId,
                                               positionDt = positionDt,
                                               attributionDt = attributionDt,
                                               storePogDt = storePogDt)
  updatedPositionDt <- buildPositionDt(configs = configs,
                                       positionDt = positionDt,
                                       attributionDt = attributionDt,
                                       perfLibDt = perfLibDt,
                                       storePogDt = storePogDt)
  
  # 2. Planters kernels left of salad toppers
  itemId <- 212080635
  storeCount <-20000
  storesCurrentlyIn <- positionDt[positionId == itemId, .(pogId)] %>% unique %>%
    merge(storePogDt, by = "pogId") %>%
    .[, unique(storeId)]
  storeCountGap <- storeCount - length(storesCurrentlyIn)
  actionStores <- leftOnly(storePogDt$storeId %>% unique, storesCurrentlyIn)
  actionStoreOnlyPogs <- leftOnly(storePogDt[storeId %in% actionStores, unique(pogId)], storePogDt[!(storeId %in% actionStores), unique(pogId)])
  itemGroupIds <- attributionDt %>%
    .[subcatId == attributionDt[positionId == itemId, subcatId], positionId]
  
  actionPogs <- storePogDt[pogId %in% actionStoreOnlyPogs, unique(pogId)] %>%
    intersect(positionDt[positionId %in% itemGroupIds, unique(pogId)])
  if (storePogDt[pogId %in% actionPogs, .N, by = storeId][N > 1, .N] > 0) {
    stop("itemGroupIds dually located in some store") # store 963 triggering 
  }
  pogRankDt <- positionDt[(positionId %in% itemGroupIds) & (pogId %in% actionPogs),
                          .(subcat_s2s_index, pogId, store_count)] %>% 
    unique %>%
    .[order(subcat_s2s_index)] %>%
    .[, cumsum := cumsum(store_count)]
  actionStores <- storePogDt[pogId %in% pogRankDt[cumsum <= storeCountGap, pogId], storeId]
  configs <- updateConfigsInsertItemsByPogList(configs = configs,
                                               actionStores = actionStores,
                                               newItemIds = itemId,
                                               positionDt = positionDt,
                                               attributionDt = attributionDt,
                                               storePogDt = storePogDt,
                                               orientationString = "LeftofPosition")
  
  # 3 Kewpie - double faced, top shelf far-right
  itemId <- 212300450
  storeCount <- 1256
  storesCurrentlyIn <- positionDt[positionId == itemId, .(pogId)] %>% unique %>%
    merge(storePogDt, by = "pogId") %>%
    .[, unique(storeId)]
  storeCountGap <- storeCount - length(storesCurrentlyIn)
  eligibleStores <- leftOnly(storePogDt$storeId %>% unique, storesCurrentlyIn)
  
  # Put
  itemGroupIds <- getAdjacentItemSummaryTable(pogData = copy(positionDt)[, ':='(display = pogId, 
                                                                                id = positionId, 
                                                                                item_description = positionId, 
                                                                                store_tied = store_count, 
                                                                                fixture_key = fixtureId, 
                                                                                position_rankx = positionRankX)], 
                                              itemId,
                                              directionInt = 1, 
                                              attDt = cleanNames(attributionDt), 
                                              itemPogDt = copy(positionDt)[, ':='(display = pogId, 
                                                                                  id = positionId, 
                                                                                  item_description = positionId, 
                                                                                  store_tied = store_count)]) %>%
    .[!is.na(adjacent_id) & stores_tied > 5, unique(adjacent_id)]
  configs <- updateConfigsGenericIncrease(itemId = itemId,
                                          eligibleStores = eligibleStores,
                                          storeCount = storeCount,
                                          configs = configs,
                                          positionDt = positionDt,
                                          attributionDt = attributionDt,
                                          storePogDt = storePogDt,
                                          itemGroupIds = itemGroupIds,
                                          ranking_var = "subcat_upspw",
                                          rankDecreaseBool = TRUE,
                                          agg_fun = mean,
                                          faceKey = data.table(position_hfacings = 1 : 100,
                                                               new_item_face = 2,
                                                               old_item_face = c(1, 1 : 99)))
  
  
  # 4 Western
  # Only goes in midwest stores, its like hlmn/bfst with wishbone
  # UNFINISHED
  itemId <- 212080124
  storeCount <- 495
  storesCurrentlyIn <- positionDt[positionId == itemId, .(pogId)] %>% unique %>%
    merge(storePogDt, by = "pogId") %>%
    .[, unique(storeId)]
  storeCountGap <- storeCount - length(storesCurrentlyIn)
  itemGroupIds <- attributionDt[grepl("03 POURABLE", subcatId), unique(positionId)]
  eligibleStores <- leftOnly(storePogDt[is_midwest == 1, unique(storeId)], storesCurrentlyIn)
  configs <- updateConfigsGenericIncrease(itemId = itemId,
                                          eligibleStores = eligibleStores,
                                          storeCount = storeCount,
                                          configs = configs,
                                          positionDt = positionDt,
                                          attributionDt = attributionDt,
                                          storePogDt = storePogDt,
                                          itemGroupIds = itemGroupIds,
                                          data.table(position_hfacings = 1 : 100,
                                                     new_item_face = 2,
                                                     old_item_face = c(1, 1 : 99)))
  
  
  # Sir K's
  itemId <- 212080837 # 382 to 698
  sirBrandId <- attributionDt[positionId == itemId, brandId]
  brandInPogs <- positionDt[brandId == sirBrandId, unique(pogId)]
  brandInStores <- storePogDt[pogId %in% brandInPogs, unique(storeId)]
  itemInPogs <- positionDt[positionId == itemId, unique(pogId)]
  itemInStores <- storePogDt[pogId %in% itemInPogs, unique(storeId)]
  putInStores  <- leftOnly(brandInStores, itemInStores)
  putInPogs <- storePogDt[storeId %in% putInStores, unique(pogId)]
  
  insertPogs <- intersect(putInPogs, brandInPogs)
  
  insertsDt <- positionDt[pogId %in% insertPogs] %>%
    .[brandId == sirBrandId, .(insertId = positionId[which.min(positionX)]), by = pogId] %>%
    .[, seq := seq_len(.N), by = pogId] %>%
    .[seq == 1] %>%
    .[, seq := NULL] %>% .[]
  for (insertId_ in insertsDt[, unique(insertId)]) {
    configs <- updateConfigs(configs = configs, existingItemId = insertId_, newItemId = itemId,
                             orientation = "LeftofPosition", 
                             dt = data.table(display = insertsDt[insertId == insertId_, pogId]))
  }
  
  
  
  # Monari Feder
  itemId <- 212080086 # 382 to 698
  monariBrandId <- attributionDt[positionId == itemId, brandId]
  brandInPogs <- positionDt[brandId == monariBrandId, unique(pogId)]
  brandInStores <- storePogDt[pogId %in% brandInPogs, unique(storeId)]
  itemInPogs <- positionDt[positionId == itemId, unique(pogId)]
  itemInStores <- storePogDt[pogId %in% itemInPogs, unique(storeId)]
  putInStores  <- leftOnly(brandInStores, itemInStores)
  putInPogs <- storePogDt[storeId %in% putInStores, unique(pogId)]
  
  insertPogs <- intersect(putInPogs, brandInPogs)
  
  insertsDt <- positionDt[pogId %in% insertPogs] %>%
    .[brandId == monariBrandId, .(insertId = positionId[which.min(positionX)]), by = pogId] %>%
    .[, seq := seq_len(.N), by = pogId] %>%
    .[seq == 1] %>%
    .[, seq := NULL] %>% .[]
  for (insertId in insertsDt[, unique(insertId)]) {
    configs <- updateConfigs(configs = configs, existingItemId = insertId, newItemId = itemId,
                             orientation = "LeftofPosition", 
                             dt = data.table(display = insertsDt[insertId == insertId, pogId]))
  }
  
  

  
  # Two Hormel items
  itemId <- 212080381 # 382 to 698
  subcatId <- attributionDt[positionId == itemId, subcatId]
  positionDt <- buildPositionDt(configs = configs,
                                positionDt = positionDt,
                                attributionDt = attributionDt,
                                perfLibDt = perfLibDt,
                                storePogDt = storePogDt)
  eligiblePogs <- positionDt[subcatId == subcatId & subcat_facings_per_item > 2 ,unique(pogId)]
  eligibleStores <- storePogDt[pogId %in% eligiblePogs, unique(storeId)]
  storeCount <- 589
  configs <- updateConfigsGenericIncrease(itemId = itemId,
                                          eligibleStores = eligibleStores,
                                          storeCount = storeCount,
                                          configs = configs,
                                          positionDt = positionDt,
                                          attributionDt = attributionDt,
                                          storePogDt = storePogDt)
  itemId <- 212080511
  subcatId <- attributionDt[positionId == itemId, subcatId]
  positionDt <- buildPositionDt(configs = configs,
                                positionDt = positionDt,
                                attributionDt = attributionDt,
                                perfLibDt = perfLibDt,
                                storePogDt = storePogDt)
  eligiblePogs <- positionDt[subcatId == subcatId & subcat_facings_per_item > 2 ,unique(pogId)]
  eligibleStores <- storePogDt[pogId %in% eligiblePogs, unique(storeId)]
  storeCount <- 1193
  configs <- updateConfigsGenericIncrease(itemId = itemId,
                                          eligibleStores = eligibleStores,
                                          storeCount = storeCount,
                                          configs = configs,
                                          positionDt = positionDt,
                                          attributionDt = attributionDt,
                                          storePogDt = storePogDt)
  
  
  
  # Primal kitchen - more H+W stores if possible
  itemId <- 212080697
  subcatId <- attributionDt[positionId == itemId, subcatId]
  eligibleStores <- storePogDt[, unique(storeId)]
  storeCount <- 1841
  configs <- updateConfigsGenericIncrease(itemId = itemId,
                                          eligibleStores = eligibleStores,
                                          storeCount = storeCount,
                                          configs = configs,
                                          positionDt = positionDt,
                                          attributionDt = attributionDt,
                                          storePogDt = storePogDt)
  
  itemId <- 212080704
  eligibleStores <- storePogDt[, unique(storeId)]
  storeCount <- 1841
  configs <- updateConfigsGenericIncrease(itemId = itemId,
                                          eligibleStores = eligibleStores,
                                          storeCount = storeCount,
                                          configs = configs,
                                          positionDt = positionDt,
                                          attributionDt = attributionDt,
                                          storePogDt = storePogDt)
  
  itemId <- 212080698
  subcatId <- attributionDt[positionId == itemId, subcatId]
  positionDt <- buildPositionDt(configs = configs,
                                positionDt = positionDt,
                                attributionDt = attributionDt,
                                perfLibDt = perfLibDt,
                                storePogDt = storePogDt)
  eligiblePogs <- positionDt[subcatId == subcatId & subcat_facings_per_item > 1.5 ,unique(pogId)]
  eligibleStores <- storePogDt[pogId %in% eligiblePogs] %>%
    .[is_hw == 1, unique(storeId)]
  storeCount <- 1289
  configs <- updateConfigsGenericIncrease(itemId = itemId,
                                          eligibleStores = eligibleStores,
                                          storeCount = storeCount,
                                          configs = configs,
                                          positionDt = positionDt,
                                          attributionDt = attributionDt,
                                          storePogDt = storePogDt)
  
  itemId <- 212080982
  subcatId <- attributionDt[positionId == itemId, subcatId]
  positionDt <- buildPositionDt(configs = configs,
                                positionDt = positionDt,
                                attributionDt = attributionDt,
                                perfLibDt = perfLibDt,
                                storePogDt = storePogDt)
  eligiblePogs <- positionDt[subcatId == subcatId & subcat_facings_per_item > 1.5 ,unique(pogId)]
  eligibleStores <- storePogDt[pogId %in% eligiblePogs] %>%
    .[is_hw == 1, unique(storeId)]
  configs <- updateConfigsGenericIncrease(itemId = itemId,
                                          eligibleStores = eligibleStores,
                                          storeCount = storeCount,
                                          configs = configs,
                                          positionDt = positionDt,
                                          attributionDt = attributionDt,
                                          storePogDt = storePogDt)
  
  itemId <- 212080703
  subcatId <- attributionDt[positionId == itemId, subcatId]
  positionDt <- buildPositionDt(configs = configs,
                                positionDt = positionDt,
                                attributionDt = attributionDt,
                                perfLibDt = perfLibDt,
                                storePogDt = storePogDt)
  eligiblePogs <- positionDt[subcatId == subcatId & subcat_facings_per_item > 1.5 ,unique(pogId)]
  eligibleStores <- storePogDt[pogId %in% eligiblePogs, unique(storeId)]
  storeCount <- 790
  configs <- updateConfigsGenericIncrease(itemId = itemId,
                                          eligibleStores = eligibleStores,
                                          storeCount = storeCount,
                                          configs = configs,
                                          positionDt = positionDt,
                                          attributionDt = attributionDt,
                                          storePogDt = storePogDt)
  
  
  
  ##########################################################################################################################
  #                         NEW ITEMS                         ##                         NEW ITEMS                         #
  ##########################################################################################################################
  
  positionDt <- buildPositionDt(configs = configs,
                                positionDt = positionDt,
                                attributionDt = attributionDt,
                                perfLibDt = perfLibDt,
                                storePogDt = storePogDt)
  
  
  # These items are going into super target stores
  superTargetItemIds <- c(212080110, 212080116, 212080069, 212080184, 212080082)
  configs <- updateConfigsInsertItemsByPogList(configs = configs,
                                               actionStores = storePogDt[is_super == 1, unique(storeId)],
                                               newItemIds = superTargetItemIds,
                                               positionDt = positionDt,
                                               attributionDt = attributionDt,
                                               storePogDt = storePogDt)
  
  
  # 12' pogs only, far right kraft shelf
  itemId <- 212080121
  itemGroupIds <- attributionDt[brandId == attributionDt[positionId == itemId, brandId]] %>%
    .[subcatId == attributionDt[positionId == itemId, subcatId], positionId]
  storeCount <- 791
  actionPogs <- storePogDt[width >= 12, pogId] %>%
    intersect(positionDt[positionId %in% itemGroupIds, unique(pogId)])
  if (storePogDt[pogId %in% actionPogs, .N, by = storeId][N > 1, .N] > 0) {
    stop("itemGroupIds dually located in some store")
  }
  pogRankDt <- positionDt[(positionId %in% itemGroupIds) & (pogId %in% actionPogs)] %>%
    .[subcat_facings_per_item > 1.5] %>%
    .[, .(brand_s2s_index, pogId, store_count)] %>% 
    unique %>%
    .[order(brand_s2s_index)] %>%
    .[, cumsum := cumsum(store_count)]
  actionStores <- storePogDt[pogId %in% pogRankDt[cumsum <= storeCount, pogId], storeId]
  configs <- updateConfigsInsertItemsByPogList(configs = configs,
                                               actionStores = actionStores,
                                               newItemIds = itemId,
                                               positionDt = positionDt,
                                               attributionDt = attributionDt,
                                               storePogDt = storePogDt)
  
  
  # >12' pogs, Kraft in Non-H&W 12' pogs
  itemId <- 212080114
  itemGroupIds <- attributionDt[brandId == attributionDt[positionId == itemId, brandId]] %>%
    .[subcatId == attributionDt[positionId == itemId, subcatId], positionId]
  storeCount <- 450
  actionPogs <- storePogDt[width >= 12 & is_hw == 1, unique(pogId)] %>%
    intersect(positionDt[positionId %in% itemGroupIds, unique(pogId)])
  if (storePogDt[pogId %in% actionPogs, .N, by = storeId][N > 1, .N] > 0) {
    stop("itemGroupIds dually located in some store")
  }
  pogRankDt <- positionDt[(positionId %in% itemGroupIds) & (pogId %in% actionPogs),
                          .(brand_s2s_index, pogId, store_count)] %>% 
    unique %>%
    .[order(brand_s2s_index)] %>%
    .[, cumsum := cumsum(store_count)]
  actionStores <- storePogDt[pogId %in% pogRankDt[cumsum <= storeCount, pogId], storeId]
  configs <- updateConfigsInsertItemsByPogList(configs = configs,
                                               actionStores = actionStores,
                                               newItemIds = itemId,
                                               positionDt = positionDt,
                                               attributionDt = attributionDt,
                                               storePogDt = storePogDt)
  
  # Primal kitchen buffalo mayo
  # >12' + Non-H&W 12' pogs, next to Kraft Squeeze
  itemId <- 212080180
  itemGroupIds <- attributionDt %>%
    .[grepl("Squeeze", item_description)] %>%
    .[grepl("Kraft", item_description)] %>%
    .[brandId == attributionDt[positionId == itemId, brandId]] %>%
    .[subcatId == attributionDt[positionId == itemId, subcatId], positionId]
  storeCount <- 1000
  actionPogs <- storePogDt[width >= 12 & is_hw == 0, unique(pogId)] %>%
    intersect(positionDt[positionId %in% itemGroupIds, unique(pogId)])
  if (storePogDt[pogId %in% actionPogs, .N, by = storeId][N > 1, .N] > 0) {
    stop("itemGroupIds dually located in some store")
  }
  pogRankDt <- positionDt[(positionId %in% itemGroupIds) & (pogId %in% actionPogs),
                          .(brand_s2s_index, pogId, store_count)] %>% 
    unique %>%
    .[order(brand_s2s_index)] %>%
    .[, cumsum := cumsum(store_count)]
  actionStores <- storePogDt[pogId %in% pogRankDt[cumsum <= storeCount, pogId], storeId]
  configs <- updateConfigsInsertItemsByPogList(configs = configs,
                                               actionStores = actionStores,
                                               newItemIds = itemId,
                                               positionDt = positionDt,
                                               attributionDt = attributionDt,
                                               storePogDt = storePogDt,
                                               itemSubset = itemGroupIds)
  
  # Aiolis - non-h&w > 12' pogs, 
  
  itemId <- 212080195 # hellmans, next to 212080019
  itemGroupIds <- 212080019
  storeCount <- 820
  actionPogs <- storePogDt[is_hw == 0 & width >= 12, unique(pogId)] %>%
    intersect(positionDt[positionId %in% itemGroupIds, unique(pogId)])
  if (storePogDt[pogId %in% actionPogs, .N, by = storeId][N > 1, .N] > 0) {
    stop("itemGroupIds dually located in some store")
  }
  pogRankDt <- positionDt[(positionId %in% itemGroupIds) & (pogId %in% actionPogs),
                          .(brand_s2s_index, pogId, store_count)] %>% 
    unique %>%
    .[order(brand_s2s_index)] %>%
    .[, cumsum := cumsum(store_count)]
  actionStores <- storePogDt[pogId %in% pogRankDt[cumsum <= storeCount, pogId], storeId]
  configs <- updateConfigsInsertItemsByPogList(configs = configs,
                                               actionStores = actionStores,
                                               newItemIds = itemId,
                                               positionDt = positionDt,
                                               attributionDt = attributionDt,
                                               storePogDt = storePogDt)
  
  # Aioli Best Foods
  itemId <- 212080192 # BF
  itemGroupIds <- 212080059
  storeCount <- 200
  actionPogs <- storePogDt[is_hw == 0 & width >= 12, unique(pogId)] %>%
    intersect(positionDt[positionId %in% itemGroupIds, unique(pogId)])
  if (storePogDt[pogId %in% actionPogs, .N, by = storeId][N > 1, .N] > 0) {
    stop("itemGroupIds dually located in some store")
  }
  pogRankDt <- positionDt[(positionId %in% itemGroupIds) & (pogId %in% actionPogs),
                          .(brand_s2s_index, pogId, store_count)] %>% 
    unique %>%
    .[order(brand_s2s_index)] %>%
    .[, cumsum := cumsum(store_count)]
  actionStores <- storePogDt[pogId %in% pogRankDt[cumsum <= storeCount, pogId], storeId]
  configs <- updateConfigsInsertItemsByPogList(configs = configs,
                                               actionStores = actionStores,
                                               newItemIds = itemId,
                                               positionDt = positionDt,
                                               attributionDt = attributionDt,
                                               storePogDt = storePogDt)
  
  # Kraft Catalina 24 oz- Super Target only OR next to Catalina 16 oz?
  itemId <- 212080168 # 24 oz
  itemGroupIds <- 212080094 # 16 oz
  storeCount <- 200
  actionPogs <- storePogDt[is_super == 1, unique(pogId)] %>%
    intersect(positionDt[positionId %in% itemGroupIds, unique(pogId)])
  if (storePogDt[pogId %in% actionPogs, .N, by = storeId][N > 1, .N] > 0) {
    stop("itemGroupIds dually located in some store")
  }
  pogRankDt <- positionDt[(positionId %in% itemGroupIds) & (pogId %in% actionPogs),
                          .(brand_s2s_index, pogId, store_count)] %>% 
    unique %>%
    .[order(brand_s2s_index)] %>%
    .[, cumsum := cumsum(store_count)]
  actionStores <- storePogDt[pogId %in% pogRankDt[cumsum <= storeCount, pogId], storeId]
  configs <- updateConfigsInsertItemsByPogList(configs = configs,
                                               actionStores = actionStores,
                                               newItemIds = itemId,
                                               positionDt = positionDt,
                                               attributionDt = attributionDt,
                                               storePogDt = storePogDt)
  
  # Hispanic stores only
  itemId <- 212080175
  itemGroupIds <- attributionDt[brandId == attributionDt[positionId == itemId, brandId]] %>%
    .[subcatId == attributionDt[positionId == itemId, subcatId], positionId]
  storeCount <- 304
  actionPogs <- storePogDt[is_hisp == 1, unique(pogId)] %>%
    intersect(positionDt[positionId %in% itemGroupIds, unique(pogId)])
  if (storePogDt[pogId %in% actionPogs, .N, by = storeId][N > 1, .N] > 0) {
    stop("itemGroupIds dually located in some store")
  }
  pogRankDt <- positionDt[(positionId %in% itemGroupIds) & (pogId %in% actionPogs),
                          .(brand_s2s_index, pogId, store_count)] %>% 
    unique %>%
    .[order(brand_s2s_index)] %>%
    .[, cumsum := cumsum(store_count)]
  actionStores <- storePogDt[pogId %in% pogRankDt[cumsum <= storeCount, pogId], storeId]
  configs <- updateConfigsInsertItemsByPogList(configs = configs,
                                               actionStores = actionStores,
                                               newItemIds = itemId,
                                               positionDt = positionDt,
                                               attributionDt = attributionDt,
                                               storePogDt = storePogDt)
  
  # Left of primal kitchen jar
  itemId <- 212080102 # Primal kitchen squeeze mayo
  itemGroupIds <- 212080691
  storeCount <- 1289
  actionPogs <- storePogDt[, unique(pogId)] %>%
    intersect(positionDt[positionId %in% itemGroupIds, unique(pogId)])
  if (storePogDt[pogId %in% actionPogs, .N, by = storeId][N > 1, .N] > 0) {
    stop("itemGroupIds dually located in some store")
  }
  pogRankDt <- positionDt[(positionId %in% itemGroupIds) & (pogId %in% actionPogs),
                          .(brand_s2s_index, pogId, store_count)] %>% 
    unique %>%
    .[order(brand_s2s_index)] %>%
    .[, cumsum := cumsum(store_count)]
  actionStores <- storePogDt[pogId %in% pogRankDt[cumsum <= storeCount, pogId], storeId]
  configs <- updateConfigsInsertItemsByPogList(configs = configs,
                                               actionStores = actionStores,
                                               newItemIds = itemId,
                                               positionDt = positionDt,
                                               attributionDt = attributionDt,
                                               storePogDt = storePogDt)
  
  
  # Notco mayo is in store list from kelsey
  itemId <- 212080171 # Notco mayo
  itemGroupIds <- attributionDt[grepl("Vegan", item_description)][subcatId == "01 MAYO-WH SAL DRESS", positionId]
  storeCount <- 162
  # action pogs are pogs that only show up for notco stores
  # notco stores that share pogs with other regions cant get touched
  notcoPogs <- storePogDt[is_notco == 1, unique(pogId)]
  nonNotcoPogs <- storePogDt[is_notco == 0, unique(pogId)]
  notcoOnlyPogs <- leftOnly(notcoPogs, nonNotcoPogs)
  actionPogs <- notcoOnlyPogs %>%
    intersect(positionDt[positionId %in% itemGroupIds, unique(pogId)])
  if (storePogDt[pogId %in% actionPogs, .N, by = storeId][N > 1, .N] > 0) {
    stop("itemGroupIds dually located in some store")
  }
  pogRankDt <- positionDt[(positionId %in% itemGroupIds) & (pogId %in% actionPogs),
                          .(subcat_s2s_index, pogId, store_count)] %>% 
    unique %>%
    merge(storePogDt[, .(pogId, is_super, is_hw)] %>% unique, by = "pogId") %>%
    .[order(!is_super, !is_hw, subcat_s2s_index)] %>%
    .[, cumsum := cumsum(store_count)]
  actionStores <- storePogDt[pogId %in% pogRankDt[cumsum <= storeCount, pogId], storeId]
  configs <- updateConfigsInsertItemsByPogList(configs = configs,
                                               actionStores = actionStores,
                                               newItemIds = itemId,
                                               positionDt = positionDt,
                                               attributionDt = attributionDt,
                                               storePogDt = storePogDt)
}

# CLEAN UP STEP
# We can't change the facings of any deleted items -- address
badAttempts <- merge(configs$facesConfig[, .(PogId, ItemId)],
                     configs$deletesConfig[, .(PogId, ItemId)])
if (nrow(badAttempts) > 0) {
  stop("This is no good")
}



podDt <- buildPodDt(positionDt, storePogDt, attributionDt, attCols = "item_description",
                    "brandId")
pogDeletes <- rbind(configs$deletesConfig[, .(pogId = PogId, positionId = ItemId)],
                    configs$swapsConfig[, .(pogId = PogId, positionId = ExistingItemId)]) %>%
  merge(storePogDt, by = c("pogId"), allow.cartesian = TRUE) %>%
  .[, .(delta = -1 * uniqueN(storeId)), by = positionId]
pogAdds <- rbind(configs$insertsConfig[, .(pogId = PogId, positionId = CutInItemId)],
                 configs$swapsConfig[, .(pogId = PogId, positionId = NewItemId)])  %>%
  merge(storePogDt, by = c("pogId"), allow.cartesian = TRUE) %>%
  .[, .(delta = uniqueN(storeId)), by = positionId]
podDeltaDt <-  rbind(pogDeletes, pogAdds)[, positionId := as.integer(positionId)] %>% 
  .[, .(delta = sum(delta)), by = positionId]
podShiftDt <- merge(podDt, podDeltaDt, all = TRUE)
podShiftDt[is.na(delta), delta := 0]
podShiftDt[is.na(store_count), store_count := 0]
podShiftDt <- podShiftDt[, new_pod := store_count + delta] %>% 
  .[, .(positionId, store_count, delta, new_pod)] %>%
  merge(attributionDt[, .(positionId, brandId, item_description)], by = "positionId", all.x = TRUE)



##########################################################################################################################
#                         CONDIMENTS                       ##                         CONDIMENTS                         #
##########################################################################################################################
if (FALSE) {
  condConfigs <- lapply(configs, FUN = function(x) x[PogId %in% condPogs])
  fwrite(condConfigs$insertsConfig, file.path(config$configsDir, "cond_inserts_config.csv"))
  fwrite(condConfigs$deletesConfig, file.path(config$configsDir, "cond_deletes_config.csv"))
  fwrite(condConfigs$swapsConfig, file.path(config$configsDir, "cond_swaps_config.csv"))
  fwrite(condConfigs$facesConfig, file.path(config$configsDir, "cond_faces_config.csv"))
}


##########################################################################################################################
#                    ALL ELSE POGS                    ##                         ALL ELSE POGS                         #
##########################################################################################################################
allPogs <- storePogDt[, unique(pogId)]
elsePogs <- leftOnly(allPogs, condPogs) %>% unique

if (FALSE) {
  elseConfigs <- lapply(configs, FUN = function(x) x[PogId %in% elsePogs])
  fwrite(elseConfigs$insertsConfig, file.path(config$configsDir, "else_inserts_config.csv"))
  fwrite(elseConfigs$deletesConfig, file.path(config$configsDir, "else_deletes_config.csv"))
  fwrite(elseConfigs$swapsConfig, file.path(config$configsDir, "else_swaps_config.csv"))
  fwrite(elseConfigs$facesConfig, file.path(config$configsDir, "else_faces_config.csv"))
}

if (FALSE) {
  elseConfigs <- initializeConfigs()
  elseConfigs$insertsConfig <- fread(file.path(config$configsDir, "else_inserts_config.csv"))
  elseConfigs$swapsConfig <- fread(file.path(config$configsDir, "else_swaps_config.csv"))
  elseConfigs$deletesConfig <- fread(file.path(config$configsDir, "else_deletes_config.csv"))
  elseConfigs$facesConfig <- fread(file.path(config$configsDir, "else_faces_config.csv"))
  
  
  missingPogsConfigs <- lapply(elseConfigs, FUN = function(x) x[PogId %in% missingPogs$Display])

  
  }




##########################################################################################################################
#                    INDEX CONFIG                    ##                         INDEX CONFIG                             #
##########################################################################################################################

# Build index separately for condiments pogs and else pogs (just like the configs)
# Require their own cluster names too
condStorePogWithCluster <- storePogDt[grepl("COND", title), .(stores_tied = uniqueN(storeId)),
                        by = .(pogId, title, width = as.numeric(width), depth, height)] %>% unique
clusterNames <- c("CON VALU", "HLTH YNG URB", "NO CLUSTER", "MAT MAIN", "HSP")
for (clusterName in clusterNames) {
  condStorePogWithCluster[grepl(clusterName, title), cluster := clusterName]
}
condStorePogWithCluster[is.na(cluster), cluster := "BASE CLUSTER"]
condStorePogWithCluster[width <= 4, cluster := "4FTs"]
condStorePogWithCluster[pogId %in% c("J212NUM", "J212NJ9", "J212NAW"), cluster := "NOT IN CKS"]
condStorePogWithCluster[, cluster_ft := min(16, 4 * ceiling(width %>% as.numeric / 4)) %>% formatC(., width = 2, flag = "0"), by = width]
condStorePogWithCluster[cluster %in% c("NO CLUSTER", "CON VALU", "HSP"), cluster := paste0(cluster, " - ", cluster_ft, " FT")]



elseStorePogWithCluster <- storePogDt[pogId %in% elsePogs, .(stores_tied = uniqueN(storeId)),
                        by = .(pogId, title, width = as.numeric(width), depth, height)] %>% unique
clusterNames <- c("CON VALU", "HLTH YNG URB", "NO CLUSTER", "MAT MAIN", "HSP", "VINE", "HLTH SSU", "AFF TRAD")
for (clusterName in clusterNames) {
  elseStorePogWithCluster[grepl(clusterName, title), cluster := clusterName]
}
elseStorePogWithCluster[is.na(cluster), cluster := "BASE CLUSTER"]
elseStorePogWithCluster[width <= 4 | grepl("VINEGAR", title), cluster := "Vinegar and 4FTs"]
elseStorePogWithCluster[pogId %in% c("J212NUM", "J212NJ9", "J212NAW"), cluster := "NOT IN CKS"]
elseStorePogWithCluster[, cluster_ft := min(16, 4 * ceiling(width %>% as.numeric / 4)) %>% formatC(., width = 2, flag = "0"), by = width]
elseStorePogWithCluster[cluster %in% c("BASE CLUSTER", "CON VALU", "HSP", "MAT MAIN", "HLTH YNG URB"), cluster := paste0(cluster, " - ", cluster_ft, " FT")]

indexConfigCond <- buildProjectIndexDt(condStorePogWithCluster, allCols = TRUE)
indexConfigElse <- buildProjectIndexDt(elseStorePogWithCluster, 
                                       startCount = max(as.numeric(substring(indexConfigCond$ProjectId, 1, 2))),
                                       allCols = TRUE)


if (FALSE) {
  fwrite(indexConfigCond, file.path(config$configsDir, "indexConfig.csv"))
  fwrite(indexConfigElse, file.path(config$configsDir, "indexConfigElse.csv"))
}



if (FALSE) {
  # plot footages of total population against item inserts (where its subcatId is vs where its added)
  itemId <- alessiItemId # decrease item
  # global delete
  itemId <- 212080547 # delete item
  itemId <- 212080703 # increase item, primal kitchen good example
  itemId <- 212300450 # kewpie, increase
  
  # Good example of a skew towards 4' doors due to 
  itemId <- 212080704
  
  mayonesaId <- 212080175 # mayonesa, new item

  plotItemChange(configs = configs, 
                 itemId = mayonesaId, 
                 attributionDt = attributionDt, 
                 positionDt = positionDt, 
                 storePogDt = storePogDt[is_hisp == 1])
  
  plotItemChange(configs = configs, 
                 itemId = 212080511, 
                 attributionDt = attributionDt, 
                 positionDt = positionDt, 
                 storePogDt = storePogDt)
}

if (FALSE) {
  deleteItems <- rbind(configs$deletesConfig[, .(pogId = PogId, positionId = as.integer(ItemId))],
                       configs$swapsConfig[, .(pogId = PogId, positionId = as.integer(ExistingItemId))]) %>%
    .[, is_delete_flag := 1]
  positionDtToCheck <- merge(positionDtOriginal, deleteItems, by = c("pogId", "positionId"), all.x = TRUE)
  positionDtToCheck[is.na(is_delete_flag), is_delete_flag := 0]
  positionDtToCheck[, kept_newmans_flag := fifelse(is_delete_flag == 0 & positionId %in% newmansItems, 1, 0)]
  positionDtToCheck[, newmans_pog_flag := max(kept_newmans_flag), by = pogId]
  
  positionDtToCheck[, is_all_deletes_flag := min(is_delete_flag), by = .(pogId, fixtureId)] %>%
    merge(attributionDt[, .(positionId, item_description)], by = "positionId")
  problemShelves <- positionDtToCheck[is_all_deletes_flag == 1, .(pogId, fixtureId)] %>% unique
  problemPositions <- merge(positionDtOriginal, problemShelves, by = c("pogId", "fixtureId")) %>%
    .[, is_newmans := fifelse(positionId %in% newmansItems, 1, 0)] %>%
    .[, is_all_newmans := ceiling(mean(is_newmans)), by = .(pogId, fixtureId)]
  indexConfigElse[PogId %in% problemPogs]
}
