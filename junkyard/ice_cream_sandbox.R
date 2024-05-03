if (FALSE) {
  rm(); gc();
  setwd("~/r_code")
  source("00_initialize.R")
  config <- initializeProject(configPath = "~/r_code/config.yaml")
  
  attDt <- getAttributionDtFromConfig(config)
  
  uncleanMegaDt <- fread(config$rawTieReportPath) %>% cleanNames
  # TODO: Centralize 
  uncleanTieDt <- uncleanMegaDt[grepl("ICE CREAM", title)]
  
  cleanTieDt <- buildTieReport(uncleanTieDt) %>% .[set_date == "2023-10-08"]
  if (FALSE) {
    fwrite(cleanseTieReport(cleanTieDt), config$cleanTieReportPath)
  }
  
  # New performance library
  # skipping for ice cream for now
  if (FALSE) {
    prePeriodStorePogDt <- uncleanTieDt[set_date < "2023-09-01"] %>% 
      .[, sequence := set_date == max(set_date), by = store] %>%
      .[sequence == 1] %>% 
      .[, .(storeId = store, pogId = display, set_date)]
    prePeriodPogItemDt <- fread("C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/00 - Automation & Reporting/PTIQ/2023/PW - FEMALE March Transition/Inputs/POST JDA DATA.csv") %>%
      .[, .(positionId = ID, pogId = `Planogram Desc2`)] %>% unique
    storeItemSalesDt <- fread(file.path(config$filesDir, "storeItemSalesDt.csv"))
    newPerfLib <-buildPerfLibFromSalesDt(config = config,
                                         storeItemSalesDt = storeItemSalesDt,
                                         prePeriodStorePogDt = prePeriodStorePogDt,
                                         prePeriodPogItemDt = prePeriodPogItemDt)
    perfLibForOutput <- newPerfLib[, .(`Planogram Alias` = pogId,
                                       `Nbr Locations` = store_count,
                                       ID = positionId,
                                       `Value 39` = coalesced_upspw * 52,
                                       `Value 38` = coalesced_dpspw * 52,
                                       `Nbr Weeks` = 52,
                                       `Unit Movement` = coalesced_upspw,
                                       `Value 40` = coalesced_dpspw)]
  }
  
  # AUTOMATION
  # Read in preautomation data
  pogDataDir <- file.path(config$baseDir, "pog_data")
  pogDataTag <- "pre_auto"
  pogDt <- readPlanogramTable(fileDir = pogDataDir, tag = pogDataTag)
  fixtureDt <- readFixtureTable(fileDir = pogDataDir, tag = pogDataTag)
  positionDt <- readPositionTable(fileDir = pogDataDir, tag = pogDataTag)
  
  # Create configs
  configs <- initializeConfigs()
  
  swapTable <- data.table(currentItems = c(288070868,
                                       288071287,
                                       288070332,
                                       288078627,
                                       288075307,
                                       288070089,
                                       288070717,
                                       288077295,
                                       288074978,
                                       288071043,
                                       288070840),
                      newItems     = c(288070010,
                                       288071035,
                                       288070062,
                                       288070888,
                                       288070145,
                                       288070099,
                                       288070073,
                                       288070591,
                                       288070154,
                                       288070131,
                                       288070628)
  )
  
  for (i in 1 : nrow(swapTable)) {
    currItem <- swapTable$currentItems[i]
    newItem  <- swapTable$newItems[i]
    actionPogs <- positionDt[positionId == currItem, unique(pogId)]
    configs <- updateConfigsSwap(configs = configs,
                                 actionPogs = actionPogs,
                                 existingItemId = currItem,
                                 newItemId = newItem)
  }
  
  # soft serve swaps -- special care
  # vanilla
  softId1 <- 288079629
  softId2 <- 288079629
  # swap1 : where we have each, swap to remerch
  swap1Ids <- c(288071418, 288071429)
  actionPogsSwap1 <- positionDt[positionId %in% swap1Ids, .N, keyby = pogId] %>% .[N == 2, unique(pogId)]
  configs <- updateConfigsSwap(configs = configs,
                               actionPogs = actionPogsSwap1,
                               existingItemId = swap1Ids[1],
                               softId1) %>%
    updateConfigsSwap(actionPogs = actionPogsSwap1,
                      existingItemId = swap1Ids[2],
                      newItemId = softId2)
  
  swap2Ids <- c(288070211, 288070245)
  actionPogsSwap2 <- positionDt[positionId %in% swap2Ids, .N, keyby = pogId] %>% .[N == 2, unique(pogId)] %>% leftOnly(actionPogsSwap1) 
  configs <- updateConfigsSwap(configs = configs,
                               actionPogs = actionPogsSwap2,
                               existingItemId = swap2Ids[1],
                               newItemId = softId1) %>%
    updateConfigsSwap(actionPogs = actionPogsSwap2,
                      existingItemId = swap2Ids[2],
                      newItemId = softId2)
  # handful of pogs with JUST 0245, will cut in both items and remerch to find facing
  actionPogsInsert2 <- leftOnly(positionDt[positionId == swap2Ids[2], unique(pogId)], 
                                c(actionPogsSwap1, actionPogsSwap2))
  configs <- updateConfigsSwap(configs = configs,
                               actionPogs = actionPogsInsert2,
                               existingItemId = swap2Ids[2],
                               newItemId = softId1) %>%
    # Swaps are performed first, so we need to insert next to softId1
    updateConfigs(existingItemId = softId1,
                  newItemId = softId2, dt = data.table(display = actionPogsInsert2))
 
  
  # Item Count Swaps
  # Oatly
  item1 <- 288070818
  item2 <- 288075304
  pogsWithItem1 <- positionDt[positionId == item1, unique(pogId)]
  pogsWithItem2 <- positionDt[positionId == item2, unique(pogId)]
  pogsWithBoth <- intersect(pogsWithItem1, pogsWithItem2)
  pogsWithJust1 <- leftOnly(pogsWithItem1, pogsWithBoth)
  pogsWithJust2 <- leftOnly(pogsWithItem2, pogsWithBoth)
  if (length(pogsWithJust1) > 0) {
    configs <- updateConfigsSwap(configs = configs,
                                 actionPogs = pogsWithJust1,
                                 existingItemId = item1,
                                 newItemId = item2)
  }
  if (length(pogsWithJust2) > 0) {
    configs <- updateConfigsSwap(configs = configs,
                                 actionPogs = pogsWithJust2,
                                 existingItemId = item2,
                                 newItemId = item1)
  }
  
  # Talenti rasp pint
  item1 <- 288073281 # curr: 225
  item2 <- 288073280 # curr: 880
  pogsWithItem1 <- positionDt[positionId == item1, unique(pogId)]
  pogsWithItem2 <- positionDt[positionId == item2, unique(pogId)]
  pogsWithBoth <- intersect(pogsWithItem1, pogsWithItem2)
  pogsWithJust1 <- leftOnly(pogsWithItem1, pogsWithBoth)
  pogsWithJust2 <- leftOnly(pogsWithItem2, pogsWithBoth)
  if (length(pogsWithJust1) > 0) {
    configs <- updateConfigsSwap(configs = configs,
                                 actionPogs = pogsWithJust1,
                                 existingItemId = item1,
                                 newItemId = item2)
  }
  if (length(pogsWithJust2) > 0) {
    configs <- updateConfigsSwap(configs = configs,
                                 actionPogs = pogsWithJust2,
                                 existingItemId = item2,
                                 newItemId = item1)
  }
  
  # Haagen strawb cone
  item1 <- 288070288 # curr: 187 
  item2 <- 288076398 # curr: 1245
  pogsWithItem1 <- positionDt[positionId == item1, unique(pogId)]
  pogsWithItem2 <- positionDt[positionId == item2, unique(pogId)]
  pogsWithBoth <- intersect(pogsWithItem1, pogsWithItem2)
  pogsWithJust1 <- leftOnly(pogsWithItem1, pogsWithBoth)
  pogsWithJust2 <- leftOnly(pogsWithItem2, pogsWithBoth)
  if (length(pogsWithJust1) > 0) {
    configs <- updateConfigsSwap(configs = configs,
                                 actionPogs = pogsWithJust1,
                                 existingItemId = item1,
                                 newItemId = item2)
  }
  if (length(pogsWithJust2) > 0) {
    configs <- updateConfigsSwap(configs = configs,
                                 actionPogs = pogsWithJust2,
                                 existingItemId = item2,
                                 newItemId = item1)
  }
  
  # Talenti nov
  item1 <- 288074870 # curr: 1625
  item2 <- 288073289 # curr: curr: 1881
  pogsWithItem1 <- positionDt[positionId == item1, unique(pogId)]
  pogsWithItem2 <- positionDt[positionId == item2, unique(pogId)]
  pogsWithBoth <- intersect(pogsWithItem1, pogsWithItem2)
  pogsWithJust1 <- leftOnly(pogsWithItem1, pogsWithBoth)
  pogsWithJust2 <- leftOnly(pogsWithItem2, pogsWithBoth)
  if (length(pogsWithJust1) > 0) {
    configs <- updateConfigsSwap(configs = configs,
                                 actionPogs = pogsWithJust1,
                                 existingItemId = item1,
                                 newItemId = item2)
  }
  if (length(pogsWithJust2) > 0) {
    configs <- updateConfigsSwap(configs = configs,
                                 actionPogs = pogsWithJust2,
                                 existingItemId = item2,
                                 newItemId = item1)
  }
  
  
  # delete klondike and face out adjacent
  deleteItem <- 288076749
  klondikeIds <- attDt[brand == "Klondike", unique(itemId)]
  actionPogs <- positionDt[positionId == deleteItem, unique(pogId)]
  shelfMates <- getShelfMatesDt(positionDt, deleteItem)
  faceOutTable <- shelfMates[adjacent_id %in% klondikeIds] %>%
    .[order(pogId, positionId, adjacent_facings)] %>%
    .[, seq := seq_len(.N), keyby = .(pogId, positionId)] %>%
    .[seq == 1, .(pogId, positionId, adjacent_id, adjacent_facings, target_facings)]
  holdoverPogs <- leftOnly(actionPogs, faceOutTable$pogId %>% unique)
  if (length(holdoverPogs) > 0) {
    holdoverTable <- shelfMates[pogId %in% holdoverPogs]
    faceOutTable <- rbind(faceOutTable,
                          holdoverTable[order(pogId, positionId, adjacent_facings)] %>%
                            .[, seq := seq_len(.N), keyby = .(pogId, positionId)] %>%
                            .[seq == 1, .(pogId, positionId, adjacent_id, adjacent_facings, target_facings)])
  }
  adjacentIds <- faceOutTable[, unique(adjacent_id)]
  for (i in 1 : length(adjacentIds)) {
    adjacentId <- adjacentIds[i]
    faceOutTableSubset <- faceOutTable[adjacent_id == adjacentId, .(position_hfacings = target_facings,
                                                                    new_hfacings = adjacent_facings + target_facings,
                                                                    display = pogId)]
    configs <- updateConfigsDelete(configs = configs, 
                                   actionPogs = faceOutTableSubset[, display],
                                   deleteItem = deleteItem) %>%
      updateConfigsFacings(dt = faceOutTableSubset, 
                           existingItemId = adjacentId)
  }
  
  # delete itsit and face out adjacent
  deleteItem <- 288074089
  itsItIds <- attDt[brand == "It's-It", unique(itemId)]
  actionPogs <- positionDt[positionId == deleteItem, unique(pogId)]
  shelfMates <- getShelfMatesDt(positionDt, deleteItem)
  faceOutTable <- shelfMates[adjacent_id %in% itsItIds] %>%
    .[order(pogId, positionId, adjacent_facings)] %>%
    .[, seq := seq_len(.N), keyby = .(pogId, positionId)] %>%
    .[seq == 1, .(pogId, positionId, adjacent_id, adjacent_facings, target_facings)]
  holdoverPogs <- leftOnly(actionPogs, faceOutTable$pogId %>% unique)
  if (length(holdoverPogs) > 0) {
    holdoverTable <- shelfMates[pogId %in% holdoverPogs]
    faceOutTable <- rbind(faceOutTable,
                          holdoverTable[order(pogId, positionId, adjacent_facings)] %>%
                            .[, seq := seq_len(.N), keyby = .(pogId, positionId)] %>%
                            .[seq == 1, .(pogId, positionId, adjacent_id, adjacent_facings, target_facings)])
  }
  adjacentIds <- faceOutTable[, unique(adjacent_id)]
  for (i in 1 : length(adjacentIds)) {
    adjacentId <- adjacentIds[i]
    faceOutTableSubset <- faceOutTable[adjacent_id == adjacentId, .(position_hfacings = target_facings,
                                                                    new_hfacings = adjacent_facings + 1,
                                                                    display = pogId)]
    configs <- updateConfigsDelete(configs = configs, 
                                   actionPogs = faceOutTableSubset[, display],
                                   deleteItem = deleteItem) %>%
      updateConfigsFacings(dt = faceOutTableSubset, 
                           existingItemId = adjacentId)
  }
  
  # FD push pops
  deleteItem <- 288071941
  fdIds <- attDt[brand == "Favorite Day", unique(itemId)]
  actionPogs <- positionDt[positionId == deleteItem, unique(pogId)]
  shelfMates <- getShelfMatesDt(positionDt, deleteItem)
  faceOutTable <- shelfMates[adjacent_id %in% fdIds] %>%
    .[order(pogId, positionId, adjacent_facings)] %>%
    .[, seq := seq_len(.N), keyby = .(pogId, positionId)] %>%
    .[seq == 1, .(pogId, positionId, adjacent_id, adjacent_facings, target_facings)]
  holdoverPogs <- leftOnly(actionPogs, faceOutTable$pogId %>% unique)
  if (length(holdoverPogs) > 0) {
    holdoverTable <- shelfMates[pogId %in% holdoverPogs]
    faceOutTable <- rbind(faceOutTable,
                          holdoverTable[order(pogId, positionId, adjacent_facings)] %>%
                            .[, seq := seq_len(.N), keyby = .(pogId, positionId)] %>%
                            .[seq == 1, .(pogId, positionId, adjacent_id, adjacent_facings, target_facings)])
  }
  
  adjacentIds <- faceOutTable[, unique(adjacent_id)]
  for (i in 1 : length(adjacentIds)) {
    adjacentId <- adjacentIds[i]
    faceOutTableSubset <- faceOutTable[adjacent_id == adjacentId, .(position_hfacings = target_facings,
                                                                    new_hfacings = adjacent_facings + target_facings,
                                                                    display = pogId)]
    configs <- updateConfigsDelete(configs = configs, 
                                   actionPogs = faceOutTableSubset[, display],
                                   deleteItem = deleteItem) %>%
      updateConfigsFacings(dt = faceOutTableSubset, 
                           existingItemId = adjacentId)
  }
  

  
  # Oreo item expand where double faced on other oreo items
  expandId <- 288070168
  pogsCurrentlyIn <- positionDt[positionId == expandId, unique(pogId)]
  oreoIds <- attDt[grepl('oreo', tolower(item_description)), itemId] %>% setdiff(expandId)
  oreoDoubleFacingCandidates <- positionDt[positionId %in% oreoIds][positionHFacings > 1][!(pogId %in% pogsCurrentlyIn)]
  insertDt <- oreoDoubleFacingCandidates[order(pogId, -positionWidth)] %>%
    .[, seq := seq_len(.N), keyby = pogId] %>%
    .[seq == 1, .(display = pogId,
                  adjacent_id = positionId,
                  position_hfacings = positionHFacings,
                  new_hfacings = pmax(1, positionHFacings - 1))]
  adjacentIds <- insertDt[, unique(adjacent_id)]
  for (i in 1 : length(adjacentIds)) {
    insertId <- adjacentIds[i]
    insertDtSubset <- insertDt[adjacent_id == insertId]
    configs <- updateConfigs(configs = configs, 
                             existingItemId = insertId,
                             newItemId = expandId,
                             dt = data.table(display = insertDtSubset[, unique(display)])) %>%
      updateConfigsFacings(dt = insertDtSubset, existingItemId = insertId)
  }
  
  
  # jolly rancher
  jollyDt <- fread(file.path(config$filesDir, "jolly_popsicle_dt.csv"))
  jollyId <- 288073310
  insertDt <- jollyDt %>% merge(cleanTieDt, by = "store") %>% .[, .(pogId = display, positionId = itemId)] %>% unique %>%
    merge(positionDt[, .(positionHFacings = sum(positionHFacings)), by = .(pogId, positionId)], by = c("pogId", "positionId")) %>% 
    .[, .(display = pogId,
          adjacent_id = positionId,
          position_hfacings = positionHFacings,
          new_hfacings = pmax(1, positionHFacings - 1))] %>% unique
  adjacentIds <- insertDt[, unique(adjacent_id)]
  for (i in 1 : length(adjacentIds)) {
    insertId <- adjacentIds[i]
    insertDtSubset <- insertDt[adjacent_id == insertId]
    configs <- updateConfigs(configs = configs, 
                             existingItemId = insertId,
                             newItemId = jollyId,
                             dt = data.table(display = insertDtSubset[, unique(display)])) %>%
      updateConfigsFacings(dt = insertDtSubset, existingItemId = insertId)
  }
  
  # Itz bitz
  pogList <- fread(file.path(config$filesDir, "itzBitzDt.csv")) %>% .[, unique(pogId)]
  itBitId1 <- 288070051
  doughpId1 <- 288070048
  actionPogs1 <- positionDt[pogId %in% pogList][positionId == doughpId1, unique(pogId)]
  configs <- updateConfigsSwap(configs = configs, 
                               actionPogs = actionPogs1,
                               existingItemId = doughpId1,
                               newItemId = itBitId1)
  itBitId2  <- 288074727
  doughpId2 <- 288070049
  actionPogs2 <- positionDt[pogId %in% pogList][positionId == doughpId2, unique(pogId)]
  configs <- updateConfigsSwap(configs = configs, 
                               actionPogs = actionPogs2,
                               existingItemId = doughpId2,
                               newItemId = itBitId2)
  
  # FD nondairy NOVs
  novDeletes <- c(288070834,
                  288070835,
                  288070836)
  deletePogs <- positionDt[positionId %in% novDeletes, unique(pogId)]
  for (i in 1 : length(novDeletes)) {
    novDelete <- novDeletes[i]
    configs <- updateConfigsDelete(configs = configs,
                                   actionPogs = deletePogs,
                                   deleteItem = novDelete)
  }
  
  
  # Coolhaus
  # in certain markets, insert these two new items onto the FD shelf
  coolHausIds <- c(288070122,
                   288073661)
  coolhausPogUniverse <- cleanTieDt[grepl("CSPNW|TCA|CSW", title), unique(display)]
  insertDt <- positionDt[positionId %in% novDeletes] %>%
    .[pogId %in% coolhausPogUniverse] %>%
    .[order(pogId, -positionY, -positionX)] %>%
    .[, seq := seq_len(.N), keyby = pogId] %>%
    .[seq == 1, .(display = pogId,
                  adjacent_id = positionId)]
  adjIds <- insertDt[, unique(adjacent_id)]
  for (i in 1 : length(adjIds)) {
    adjId <- adjIds[i]
    subsetPogs <- insertDt[adjacent_id == adjId, unique(display)]
    configs <- updateConfigs(configs = configs,
                             existingItemId = adjId,
                             newItemId = coolHausIds[1],
                             dt = data.table(display = subsetPogs)) %>%
      updateConfigs(existingItemId = adjId,
                    newItemId = coolHausIds[2],
                    dt = data.table(display = subsetPogs))
  }
  
  # these items go in the delete POGs to +700 stores
  replacementIds <- c(288071537, # mymo
                      288070773, # kind
                      288076218) # magnum
  
  for (i in 1 : length(replacementIds)) {
    replacementId <- replacementIds[i]
    brandBlockIds <- attDt[itemId == replacementId, .(subclass)] %>%
      merge(attDt, by = c("subclass")) %>% 
      .[, setdiff(itemId, replacementId)]
    actionPogs <- leftOnly(deletePogs, positionDt[positionId == replacementId, unique(pogId)])
    
    insertDt <- positionDt %>% 
      .[pogId %in% actionPogs] %>%
      .[positionId %in% brandBlockIds] %>%
      .[order(pogId, -positionY, -positionX)] %>%
      .[, seq :=seq_len(.N), keyby = pogId] %>%
      .[seq == 1, .(display = pogId,
                    adjacent_id = positionId)]
    adjIds <- insertDt[, unique(adjacent_id)]
    for (i in 1 : length(adjIds)) {
      adjId <- adjIds[i]
      subsetPogs <- insertDt[adjacent_id == adjId, unique(display)]
      configs <- updateConfigs(configs = configs,
                               existingItemId = adjId,
                               newItemId = replacementId,
                               dt = data.table(display = subsetPogs))
    }
  }
  
  # FD super premium pints -- face out adjacent FD deletes
  fdPintDeleteIds <- c(288072087,
                       288072097,
                       288072100)
  faceOutCandidates <- attDt[brand == "Favorite Day"][subclass == "288-07-06 SUPER PREMIUM", itemId] %>%
    setdiff(fdPintDeleteIds)
  deletesDt <- positionDt[positionId %in% fdPintDeleteIds] %>%
    .[order(pogId, -positionX, -positionY)] %>%
    .[, .(pogId, 
          deleteId = positionId,
          delete_hfacings = positionHFacings)] %>%
    unique %>%
    .[, seq := seq_len(.N), keyby = pogId]
  
  insertsDt <- positionDt[positionId %in% faceOutCandidates] %>%
    .[order(pogId, -positionX, -positionY)] %>% 
    .[, .(pogId, 
          insertId = positionId,
          insert_hfacings = positionHFacings)] %>%
    unique %>%
    .[, seq := seq_len(.N), keyby = pogId]
    
  actionDt <- merge(deletesDt, insertsDt, by = c("pogId", "seq"), all.x = TRUE)
  justDeleteDt <- actionDt[is.na(insertId)]
  if (NROW(justDeleteDt) > 0) {
    stop("NOT IMPLEMENTED YET CTRL F ME")
  }
  actionDt <- actionDt[!is.na(insertId)]
  
  stepSkeleton <- actionDt[, .(deleteId, insertId)] %>% unique
  for (i in 1 : nrow(stepSkeleton)) {
    delId <- stepSkeleton$deleteId[i]
    adjId <- stepSkeleton$insertId[i]
    new_facings = stepSkeleton$new_facings[i]
    faceOutTableSubset <- actionDt[insertId == adjId & deleteId == delId, .(position_hfacings = insert_hfacings,
                                                           new_hfacings = delete_hfacings + insert_hfacings,
                                                           display = pogId)]
    configs <- updateConfigsDelete(configs = configs, 
                                   actionPogs = faceOutTableSubset[, display],
                                   deleteItem = delId) %>%
      updateConfigsFacings(dt = faceOutTableSubset, 
                           existingItemId = adjId)
  }
  faceOutTableSubset <- faceOutTable[adjacent_id == adjacentId, .(position_hfacings = target_facings,
                                                                  new_hfacings = adjacent_facings + target_facings,
                                                                  display = pogId)]

  
  # favorite day nondairy pint deletes
  # add BJ ND oatmeal pint where we can
  bjOatmealId <- 288070605
  fdPintDelete1 <- 288071576
  actionPogs <- positionDt[positionId == fdPintDelete1, unique(pogId)]
  insertPogs <- leftOnly(actionPogs, positionDt[positionId == bjOatmealId, unique(pogId)]) %>% .[1 : 50]
  configs <- updateConfigsDelete(configs = configs, actionPogs = actionPogs, deleteItem = fdPintDelete1) %>%
    updateConfigs(existingItemId = fdPintDelete1, newItemId = bjOatmealId, dt = data.table(display = insertPogs))
  
  
  fdPintDelete2 <- 288071577
  actionPogs <- positionDt[positionId == fdPintDelete2, unique(pogId)]
  configs <- updateConfigsDelete(configs = configs, actionPogs = actionPogs, deleteItem = fdPintDelete2)
  
  # expand fd nondairy pint to full chain
  fullChainId <- 288071578
  fdNdPintIds <- attDt[brand == "Favorite Day"][subclass == "288-07-05 NONDAIRY PINTS&NOV"][grepl("16oz", item_description), itemId]
  actionPogs <- leftOnly(positionDt[positionId %in% fdNdPintIds, unique(pogId)], positionDt[positionId == fullChainId, unique(pogId)])
  insertsDt <- positionDt[positionId %in% fdNdPintIds] %>% 
    .[pogId %in% actionPogs] %>%
    .[order(pogId, -positionX, -positionY)] %>%
    .[, seq := seq_len(.N), keyby = pogId] %>% 
    .[seq == 1, .(pogId, insertId = positionId)]
  insertIds <- insertsDt[, unique(insertId)]
  for (i in 1 : length(insertIds)) {
    insId <- insertIds[i]
    actionPogs <- insertsDt[insertId == insId, unique(pogId)]
    configs <- updateConfigs(configs = configs,
                             existingItemId = insId,
                             newItemId = fullChainId,
                             dt = data.table(display = actionPogs))
  }
  
  # breyers deletes
  breyersDelete1 <- 288072765
  breyersInsert1 <- 288070796 # +459 stores
  actionPogs <- positionDt[positionId == breyersDelete1, unique(pogId)]
  swapPogs <- leftOnly(actionPogs, positionDt[positionId == breyersInsert1, unique(pogId)]) %>%
    .[1 : 255]
  deletePogs <- leftOnly(actionPogs, swapPogs)
  configs <- updateConfigsSwap(configs = configs,
                               existingItemId = breyersDelete1,
                               newItemId = breyersInsert1,
                               actionPogs = swapPogs)
  configs <- updateConfigsDelete(configs = configs,
                                 actionPogs = deletePogs,
                                 deleteItem = breyersDelete1)
  breyersDelete2 <- 288074160
  configs <- updateConfigsDelete(configs = configs,
                                 actionPogs = positionDt[positionId == breyersDelete2, unique(pogId)],
                                 deleteItem = breyersDelete2)
  
  # thelmas delete
  thelmaDeleteId     <- 288070158
  thelmaDoubleFaceId <- 288070086
  actionPogs <- positionDt[positionId == thelmaDeleteId, unique(pogId)]
  configs <- updateConfigsDelete(configs = configs, actionPogs = actionPogs, deleteItem = thelmaDeleteId)
  configs <- updateConfigsFacings(configs = configs, 
                                  existingItemId = thelmaDoubleFaceId,
                                  dt = data.table(display = actionPogs, position_hfacings = 1, new_hfacings = 2))
  
  writeConfigs(configsOut = configs, projectConfig = config)
  
  # write a config per project
  
  # Build Index
  clusterNames <- c("AFM", "MNSTRM", "FRSK", "SBRB", "YNGFAM", "HIDIGI")
  for (clusterName in clusterNames) {
    cleanTieDt[grepl(clusterName, title), cluster := clusterName]
  }
  cleanTieDt[is.na(cluster), cluster := "UNCLUSTERED"]
  
  abPogs <- cleanTieDt[, pogs_per_store := uniqueN(display), by = store] %>% .[, .(min_occurs = min(pogs_per_store)), by = .(display)] %>%
    .[min_occurs > 1, unique(display)]
  cleanTieDt[display %in% abPogs, cluster := "AB POGS"]
  
  evelynPogs <- configs$insertsConfig[CutInItemId == 288070122, PogId]
  # cleanTieDt[display %in% evelynPogs, cluster := "COOLHAUS_INSERTS"]
  
  # write the automation configs on a per
  clusterDt <- cleanTieDt[, .(pogId = display, cluster)] %>% unique
  
  
  clusterList <- clusterDt[, unique(cluster)]
  for (i in 1 : length(clusterList)) {
    currCluster <- clusterList[i]
    currPogs    <- clusterDt[cluster == currCluster, unique(pogId)] 
    outConfigs <- lapply(configs, FUN = function(x) x[PogId %in% currPogs])
    writeConfigs(outConfigs, projectConfig = config, tag = currCluster)
  }
  
  
  # check1: uniqueness
  # check2: all inserts won't fail
  
  
  indexDt <- buildProjectIndexDt(copy(cleanTieDt)[, pogId := display], allCols = TRUE)
  indexDtConfig <- buildProjectIndexDt(copy(cleanTieDt)[, pogId := display])
  if (FALSE) fwrite(indexDtConfig, file.path(config$configsDir, "indexConfigDt.csv")) 
}






















































































newFacesConfig <- merge(configs$facesConfig[, .(PogId, ExistingItemId = ItemId, FaceValue)], 
                          configs$swapsConfig,
                          by = c("PogId", "ExistingItemId"),
                          all.x = TRUE)




























