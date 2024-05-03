minimumBuildIndices <- function(sorted_v, sum_to) {
  # taking a sorted (largest to smallest) vector of integers, return index of 
  # the minimum values to get to >= sum_to
  
  # Error if vector unsorted
  if (is.unsorted(sorted_v %>% rev)) {
    stop("Calling minimumBuildIndices() on an unsorted list")
  }
  
  indexOut <- c()
  gap_to_close <- sum_to
  while(gap_to_close >= 2) {
    firstElement <- which(sorted_v < gap_to_close)[1]
    indexOut <- c(indexOut, firstElement)
    gap_to_close <- gap_to_close - sum(sorted_v[indexOut], na.rm = TRUE)
    sorted_v[indexOut] <- NA
  }
  # subset the output in
  return(indexOut)
}




buildCentroidDt <- function(positionDt, 
                            attributionDt,
                            grouping_attribute = "subclass",
                            missing_group_fill = list(x = NA, y = NA)) {
  # Creates a table at the planogram-item grouping level and calculate the item groupings centroid
  # Purpose: Rank candidate items per potential position on planogram based on distnace to 
  #          some cohort of items to which they belong
  # the grouping_attribute: should relate to how the positions are blocked together on the shelf, i.e. brand
  # missing_group_fill: should be NA if we want to set it later, or 9999 if we want to effectively exclude these adds
  
  if (sum(names(missing_group_fill) %in% c("x", "y")) != 2) {
    stop("missing_group_fill needs a x and y value")
  }
  
  ### INPUT PREP ###
  # check positionDt is in correct format
  requiredColumnNames <- c("pogId", "positionId", "positionX", "positionWidth", "positionY", "positionHeight")
  positionDt <- positionDt[, ..requiredColumnNames]
  # check attributionDt as well
  requiredColumnNames <- c("positionId", grouping_attribute)
  attributionDt <- copy(attributionDt)[, ..requiredColumnNames]
  setnames(attributionDt, old = grouping_attribute, new = "grouping_attribute")
  
  ### DATA TRANSFORMATION
  # Calculate centroid coordinates
  centroidDt <- merge(positionDt, 
                      attributionDt, by = "positionId") %>%
    .[, .(subgroup_center_x = mean(positionX + (.5 * positionWidth)),
          subgroup_center_y = mean(positionY + (.5 * positionHeight)),
          item_count = .N),
      by = .(pogId, grouping_attribute)]
  
  # Scale the coordinates relative to the dimensions of each planogram 
  # for better comparison
  widthDt <- positionDt[, .(pog_width = max(positionX + positionWidth),
                            pog_height = max(positionY + positionHeight)), by = pogId]
  centroidDt <- merge(centroidDt, widthDt, by = "pogId")
  centroidDt[, ':='(subgroup_center_x = subgroup_center_x / pog_width,
                    subgroup_center_y = subgroup_center_y / pog_height)]
  
  # Cross-Join to include groupings that aren't on the POG at all 
  setkey(centroidDt, pogId, grouping_attribute)
  centroidDt <- centroidDt[CJ(pogId, grouping_attribute, unique = TRUE)]
  
  # Fill missing subgroup values per inputs
  centroidDt[is.na(subgroup_center_x), ':='(subgroup_center_x = missing_group_fill$x,
                                            subgroup_center_y = missing_group_fill$y)]
  
  setnames(centroidDt, "grouping_attribute", grouping_attribute)
  
  return(
    centroidDt
  )
}

initializeAutoSwapObject <- function(positionDt,
                                     storePogDt,
                                     attributionDt,
                                     autoSwapDt,
                                     grouping_attribute = "subclass") {
  # positionDt
  requiredColumnNames <- c("pogId", "positionId", "positionX", "positionY",
                           "positionWidth", "positionHeight")
  checkRequiredColumnNames(positionDt, requiredColumnNames)
  positionDt <- positionDt[, ..requiredColumnNames]
  
  # storePogDt
  requiredColumnNames <- c("storeId", "pogId")
  checkRequiredColumnNames(storePogDt, requiredColumnNames)
  storePogDt <- storePogDt[, ..requiredColumnNames]
  
  # attributionDt
  requiredColumnNames <- c("positionId", grouping_attribute)
  checkRequiredColumnNames(attributionDt, requiredColumnNames)
  attributionDt <- attributionDt[, ..requiredColumnNames]
  setnames(attributionDt, old = grouping_attribute, new = "grouping_attribute")
  
  # autoSwapDt
  requiredColumnNames <- c("positionId", "final_count")
  checkRequiredColumnNames(autoSwapDt, requiredColumnNames)
  autoSwapDt <- autoSwapDt[, ..requiredColumnNames]
  
  # 
  centroidDt <- buildCentroidDt(positionDt = positionDt, 
                                attributionDt = attributionDt,
                                grouping_attribute = "grouping_attribute")
  
  # podDt Build: Get initial needed vs actual PODs per item
  # columns: position
  storepositionDt <- merge(positionDt, storePogDt, by = "pogId", allow.cartesian = TRUE) %>%
    .[, .(storeId, pogId, positionId)] %>% unique()
  
  currentPodDt <- storepositionDt[, .(stores_tied = uniqueN(storeId)),
                                 by = positionId]
  podDt <- merge(autoSwapDt, currentPodDt, by = "positionId", all.x = TRUE) %>%
    .[is.na(stores_tied), stores_tied := 0] %>%
    .[, positionType := fifelse(final_count < stores_tied, "delete", "add")] %>%
    merge(attributionDt, all.x = TRUE)
  podDt %>%
    .[, needed_adjustment := final_count - stores_tied]
  
  podDt <- podDt[needed_adjustment != 0]

  unattributedItems <- podDt[is.na(grouping_attribute)]
  # podDt data checks
  if (NROW(unattributedItems) != 0) {
    print(unattributedItems)
    stop("You have unattributed items. Address and re-run.")
  }
  
  # build possibleSwapsDt
  addItems <- podDt[positionType == "add", positionId]
  deleteItems <- podDt[positionType == "delete", positionId ]
  
  addsNeeded <- podDt[positionType == "add", sum(final_count - stores_tied)]
  deletesNeeded <- podDt[positionType == "delete", sum(stores_tied - final_count)]
  swapTypeFlag <- fifelse(addsNeeded > deletesNeeded, "only_inserts_left", "only_deletes_left")
  
  # Create a DT of pogId | potentialAddID
  potentialAddsDt <- positionDt[, .(addId = leftOnly(addItems, positionId)), by = pogId]
  
  # Cross-join add/delete/pogId
  crossDt <- CJ(pogId = storePogDt[, unique(pogId)],
                addId = addItems,
                deleteId = deleteItems)
  # Filter crossDt to instances where the deleteItem is on the planogram
  crossDtMerged <- merge(crossDt, positionDt,
                         by.x = c("pogId", "deleteId"),
                         by.y = c("pogId", "positionId"))
  
  # Then filter where addItem is NOT in the planogram
  # (and add some needed columns)
  possibleSwapsDt <- merge(crossDtMerged,
                           potentialAddsDt,
                           by.x = c("pogId", "addId"),
                           by.y = c("pogId", "addId")) %>%
    merge(storePogDt[, .(stores_tied = uniqueN(storeId)), by = pogId],
          by = "pogId") %>% 
    merge(attributionDt[, .(addId = positionId, grouping_attribute)], 
          by = "addId") %>%
    merge(centroidDt,
          by.x = c("pogId", "grouping_attribute"),
          by.y = c("pogId", "grouping_attribute")) %>%
    .[, ':='(delete_item_center_x = (positionX + (.5 * positionWidth)) / pog_width,
             delete_item_center_y = (positionY + (.5 * positionHeight)) / pog_height)]
  
  return(
    list(podDt = podDt,
         possibleSwapsDt = possibleSwapsDt)
  )
}



updateAutoSwapObject <- function(autoSwapObject, swapsConfig, storePogDt) {
  # autoSwapObject: storePog (never gets updated),  podDt, possibleSwapsDt
  # recalculate PODs for items swapped on podDt
  # filter possibleSwapsDt to make swaps on swapsConfig ineligible
  if (NROW(swapsConfig) == 0) {
    return(autoSwapObject)
  } else {
    # add store ocunts to swapsConfig
    swapsConfigWithStoresTied <- merge(swapsConfig, storePogDt[, .(stores_tied = uniqueN(storeId)), by = .(PogId = pogId)], by = "PogId")
    swapShiftsDeletes <- swapsConfigWithStoresTied[, .(adjustment_made_ = sum(stores_tied)* -1), by = .(positionId = as.integer(ExistingItemId))]
    swapShiftsAdds <- swapsConfigWithStoresTied[, .(adjustment_made_ = sum(stores_tied)), by = .(positionId = as.integer(NewItemId))]
    swapShifts <- rbind(swapShiftsDeletes, swapShiftsAdds)
    # UPDATE podDT:
    # change made_adjustment, then re-calculate needed_adjustment columns of input object
    podDt_out <- merge(autoSwapObject$podDt, swapShifts, by = "positionId", all.x = TRUE) %>%
      .[!is.na(adjustment_made_), needed_adjustment := needed_adjustment - adjustment_made_]
    # filter swapsConfig from possibleSwapsDt
    swapsConfigForJoin <- swapsConfig[, .(pogId = PogId, deleteId = as.integer(ExistingItemId))]
    setkey(swapsConfigForJoin, pogId, deleteId)
    setkey(autoSwapObject$possibleSwapsDt, pogId, deleteId)
    possibleSwapsDt_out <- autoSwapObject$possibleSwapsDt[!swapsConfigForJoin]
    return(list(podDt = podDt_out,
                possibleSwapsDt = possibleSwapsDt_out))
  }
  
}

deleteOffAutoSwapObject <- function(finalAutoSwapObject, configs, storePogDt, positionDt) {
  pogItemStorestiedDt <- merge(positionDt, storePogDt[, .(stores_tied = uniqueN(storeId)), by = .(pogId)], by = "pogId") %>%
    .[, .(pogId, positionId, stores_tied)] %>% unique
  swapsConfigForMerge <- configs$swapsConfig[, .(pogId = PogId, positionId = as.integer(ExistingItemId))]
  setkey(swapsConfigForMerge, pogId, positionId)
  setkey(pogItemStorestiedDt, pogId, positionId)
  pogsStillIn <- pogItemStorestiedDt[!(swapsConfigForMerge)] %>% unique
  globalDeletes <- finalAutoSwapObject$podDt[final_count == 0 & needed_adjustment != 0, positionId]
  for (globalDelete in globalDeletes) {
    # delete these from all remaining POGs it's in
    actionPogs <- pogsStillIn[positionId == globalDelete, pogId] %>% unique
    configs <- updateConfigsDelete(configs = configs, actionPogs = actionPogs, deleteItem = globalDelete)
  } 
  decreaseIds <- finalAutoSwapObject$podDt[final_count > 0 & needed_adjustment < -10, positionId]
  for (decreaseId in decreaseIds) {
    neededRemoval <- finalAutoSwapObject$podDt[positionId == decreaseId, needed_adjustment * -1]
    removalCandidates <- pogsStillIn[positionId == decreaseId][order(-stores_tied)]
    actionPogs <- removalCandidates[minimumBuildIndices(stores_tied, neededRemoval), pogId]
    configs <- updateConfigsDelete(configs = configs, actionPogs = actionPogs, deleteItem = decreaseId)
  }
  return(configs)
}

insertOffAutoSwapObject <- function(finalAutoSwapObject, confis, storePogDt, positionDt) {
  # use inserts config to insert in leftover items
  # insert to the left 
}



outputConfigsFromAdjustments <- function(positionDt,
                                         storePogDt,
                                         attributionDt,
                                         autoSwapDt,
                                         grouping_attribute = "subclass",
                                         pogsToExclude = NULL) {

  
  # initial objects
  initialAutoSwapObject <- initializeAutoSwapObject(positionDt,
                                                    storePogDt,
                                                    attributionDt,
                                                    autoSwapDt,
                                                    grouping_attribute = grouping_attribute)
  configs <- initializeConfigs()
  isToAdjust <- which(initialAutoSwapObject$podDt$positionType == "add")
  for (i in which(initialAutoSwapObject$podDt$positionType == "add")) {
    
    autoSwapObject <- updateAutoSwapObject(initialAutoSwapObject, configs$swapsConfig, storePogDt = storePogDt)
    podDt_curr <- autoSwapObject$podDt
    swapsDt_curr <- autoSwapObject$possibleSwapsDt
    
    if (!is.null(pogsToExclude)) {
      swapsDt_curr <- swapsDt_curr[!(pogId %in% pogsToExclude)]
    }
    

    swapInRow <- autoSwapObject$podDt[i]
    
    # Filter to only swaps that have given item as a candidate add
    candidateSwaps <- swapsDt_curr %>%
      .[addId == swapInRow$positionId & stores_tied < swapInRow$needed_adjustment] %>%
      # then add column for deletes' gap to fill -- will be selecting on this column later
      merge(podDt_curr[, .(deleteId = positionId, needed_adjustment)], by = "deleteId")
    
    if (nrow(candidateSwaps) == 0) {
      print(swapInRow)
    }
    
    # calculate distance per row
    candidateSwaps[, distance := sqrt((delete_item_center_x - subgroup_center_x)^2 + 
                                        (delete_item_center_y - subgroup_center_y)^2)]
    
    # HERE!!!!! YOU COULD IMPUTE THE NA's TO BE WHATEVER YOU WANT
    # CURRENTLY GIVING PREFERENTIAL TREATMENT TO WHERE SUBGROUP NOT CARRIED
    candidateSwaps[is.na(delete_item_center_x), distance := 0.05]
    
    candidateSwaps[, distance := 1 / stores_tied]

    
    # RULES TO HARD CODE # deleteId | addId
    # FORCE A SWAP BETWEEN 37121853 AND 37120459
    candidateSwaps[deleteId == 37121853 & addId == 37120459, distance := 0.0001]
    
    
    # HERE: We may have a candidate who is #2 for a given planogram behind an item we can't add
    # because doing so would make that delete item go too far under its target
    # determine if adding a given row would throw that item's gap off
    # cumsum per item (?) this is very tricky
    
    # get the minimum distance to determine the best item to swap this item in for per planogram
    # -- in case of a tie, swap in for the item that highs the most deletes needed
    candidateSwaps[, min_distance := min(distance), by = .(pogId, addId)]
    candidatePogsOrdered <- candidateSwaps[distance == min_distance] %>%
      .[order(pogId, distance, needed_adjustment), rankByPog := seq_len(.N), by = pogId]
    candidatesPerPog <- candidatePogsOrdered[rankByPog == 1]
    
    
    # order by the minimum distance, break ties with stores_tied
    # you'll still get ties where the subgroup doesn't exist, should swap it for the delete item that needs it most
    candidatesPerPogOrdered <- candidatesPerPog[order(distance, stores_tied)]
    candidatesPerPogOrdered[, cumulative_shift := cumsum(stores_tied)]
    
    # Lock-in the number of swaps that gets us within tolerance
    finalSwapsForAddItem <- candidatesPerPogOrdered[cumulative_shift <= swapInRow$needed_adjustment + 10]
    swapsInputTable <- finalSwapsForAddItem[, .N, by = .(addId, deleteId)]
    for (j in 1 : nrow(swapsInputTable)) {
      deleteId_i= swapsInputTable$deleteId[j]
      addId_i = swapsInputTable$addId[j]
      actionPogs <- finalSwapsForAddItem[deleteId == deleteId_i & addId == addId_i, pogId]

      configs <- updateConfigsSwap(configs = configs, actionPogs = actionPogs,
                                   existingItemId = deleteId_i,
                                   newItemId = addId_i)
    }
    if (nrow(swapsInputTable[is.na(addId)]) != 0) {
      print('ok')
    }
    


  }
  finalAutoSwapObject <- updateAutoSwapObject(initialAutoSwapObject, configs$swapsConfig, storePogDt = storePogDt)
  
  # Perform leftover deletes
  configs <- deleteOffAutoSwapObject(finalAutoSwapObject = finalAutoSwapObject, 
                                     configs = configs, 
                                     storePogDt = storePogDt,
                                     positionDt = positionDt)
  # TODO: deal with leftover inserts
  # RECALCULATE Auto swap
  
  
  # Delete whats left over
  return(
    configs
  )
}



if (FALSE) {
  # TNT
  rm(list = ls()); gc();
  setwd("~/r_code")
  source("00_initialize.R")
  
  positionDt <- fread(file.path(config$configsDir, "position_table_preautomation.csv"))
  shelfDt <- fread(file.path(config$configsDir, "fixture_table_preautomation.csv")) %>%
    .[fixtureColor == "-1"]
  
  autoSwapDt <- fread(file.path(config$filesDir, "item_inc_dec_input.csv"))
  attributionDt <- fread(config$attributionDtPath) %>% cleanNames() %>% 
    .[, positionId := positionid] 
  storePogDt <- fread(config$storePogDtPath)
  

  podDt <- merge(storePogDt, positionDt, by = "pogId", allow.cartesian = TRUE) %>%
    .[, .(pods = uniqueN(storeId)), by = positionId] %>%
    merge(autoSwapDt, all.y = TRUE) %>%
    .[is.na(pods), pods := 0]
  autoSwapDt <- podDt[pods != final_count, .(positionId, final_count)]
  
  # for TNT
  indexConfig <- fread(config$cleanIndexReportPath)
  pogsToExclude <- indexConfig[a_b_pog != "", display]
  
  
  # RULES TO HARD CODE
  # FORCE A SWAP BETWEEN 37121853 AND 37120459
  # FORCE A SWAP BETWEEN 49040045 AND 49040076
  # FORCE A SWAP BETWEEN 94030472 AND 37052599
  # FORCE A SWAP BETWEEN 63030155 AND 63030017
  # FORCE A SWAP BETWEEN 63030141 AND 63030020
  # FORCE A SWAP BETWEEN 49040037 AND 49048046
  
  configsForAutomaticSwap <- outputConfigsFromAdjustments(positionDt = positionDt, 
                                                          storePogDt = storePogDt,
                                                          attributionDt = attributionDt,
                                                          autoSwapDt = autoSwapDt,
                                                          grouping_attribute = "subclass", # subclass for tnt
                                                          pogsToExclude = pogsToExclude)
  writeConfigs(configsForAutomaticSwap, config, testOut = TRUE, test_n = 5)
  
  # Check new item pods
  monkeyedPogItem <- merge(positionDt[, positionId := as.character(positionId)], 
                           configsForAutomaticSwap$swapsConfig, 
                           all.x = T, 
                           by.x = c("pogId", "positionId"), 
                           by.y = c("PogId", "ExistingItemId")) %>%
    .[!is.na(NewItemId), positionId := NewItemId]
  monkeyedPogItem <- merge(monkeyedPogItem, configsForAutomaticSwap$deletesConfig,
                           by.x = c("pogId", "positionId"),
                           by.y = c("PogId", "ItemId"), all.x = TRUE) %>%
    .[is.na(RelativeRank)]
  monkeyedPodDt <- merge(storePogDt, monkeyedPogItem,
                         by = "pogId", allow.cartesian = TRUE) %>%
    .[, .(pods = uniqueN(storeId)), by = positionId] %>%
    .[, positionId := as.integer(positionId)] %>%
    merge(autoSwapDt, all.y = TRUE) %>%
    .[is.na(pods), pods := 0]
  
  # Functionize the insertion of an item next to adjacent
  exItem    <- 49002214
  neededPod <- 1688
  setkey(positionDt, pogId, fixtureId, positionRankX)
  thisItemsLocations     <- positionDt[positionId == exItem]
  
  # neighbors DT
}


if (FALSE) {
  # filling out available space work
  rm(list = ls()); gc();
  setwd("~/r_code")
  source("00_initialize.R")
  
  pogDt <- fread(file.path(config$configsDir, "position_table_preautomation.csv"))
  positionDt <- fread(file.path(config$configsDir, "position_table_preautomation.csv"))
  shelfDt <- fread(file.path(config$configsDir, "fixture_table_preautomation.csv")) %>%
    .[fixtureColor == "-1"]
  
  autoSwapDt <- fread(file.path(config$filesDir, "item_inc_dec_input.csv"))
  attributionDt <- fread(config$attributionDtPath) %>% cleanNames() %>% 
    .[, positionId := positionid] 
  storePogDt <- fread(config$cleanTieReportPath) %>% cleanNames %>% 
    .[, .(pogId = display, storeId = store)] %>% unique
  pogStoreCountDt <- storePogDt[, .(store_count = uniqueN(storeId)), by = pogId]
  
  indexConfig <- fread(file.path(config$configsDir, "indexConfig.csv"))
  
  shelfItemDt <- merge(positionDt, shelfDt, by = c("fixtureId", "pogId"))
  shelfItemDt[, n_items_on_shelf := .N, by = .(pogId, fixtureId)]
  shelfItemDt[, sum_target_space := sum(positionTargetSpaceX), by = .(pogId, fixtureId)]
  fillUpSpace <- function(targetSpaceXs, availableLinear) {
    if (uniqueN(availableLinear) != 1) {
      stop("availableLinear not unique by key")
    }
    availableLinear <- availableLinear[1]
    if (availableLinear < 0) {
      while (availableLinear < 1) {
        targetSpaceXs[which.max(targetSpaceXs)] <- max(targetSpaceXs) - 1
        availableLinear <- availableLinear + 1
      }
    } else {
      if (availableLinear < 5) {
        while(availableLinear > 3) {
          targetSpaceXs[which.min(targetSpaceXs)] <- min(targetSpaceXs) + 1
          availableLinear <- availableLinear - 1
        }
      }
    }
    
    return(
      list(targetSpaceXs = targetSpaceXs,
           availableLinear = rep(availableLinear, length(targetSpaceXs)))
    )
  }
  
  
  podDt <- merge(storePogDt, positionDt, by = "pogId", allow.cartesian = TRUE) %>%
    .[, .(pods = uniqueN(storeId)), by = positionId]
  
  
  finalCountsToNailDt <- merge(podDt, autoSwapDt, all.y = TRUE) %>%
    .[!is.na(pods)] %>%
    .[final_count == 0 | (5 < abs(final_count - pods))]
  
  # re-work item PODs
  abPogs <- indexConfig[grepl("AB ", ProjectId), PogId]
  configs <- outputConfigsFromAdjustments(positionDt = positionDt, 
                                          storePogDt = storePogDt,
                                          attributionDt = attributionDt,
                                          autoSwapDt = finalCountsToNailDt,
                                          grouping_attribute = "subclass", # subclass for tnt
                                          pogsToExclude = abPogs)
  
  # Write down configs before adding inserts because tnt cant do inserts so doing those manual
  if (FALSE) {
    writeConfigs(configsOut = configs, projectConfig = config, testOut = FALSE)
  }
  # Check new item pods
  pullOuts <- rbind(configs$swapsConfig[, .(pogId = PogId,
                                            positionId = ExistingItemId)],
                    configs$deletesConfig[, .(pogId = PogId,
                                              positionId = ItemId)]) %>%
    .[, positionId := as.integer(positionId)]
  setkey(pullOuts, pogId, positionId)
  setkey(positionDt, pogId, positionId)
  monkeyedPogItem <- positionDt[!pullOuts]
  
  putIns <- configs$swapsConfig[, .(pogId = PogId,
                                    positionId = as.integer(NewItemId))]
  monkeyedPogItem <- rbind(monkeyedPogItem, putIns, fill = TRUE)
  
  monkeyedPodDt <- merge(storePogDt, monkeyedPogItem,
                         by = "pogId", allow.cartesian = TRUE) %>%
    .[, .(post_autoswap_pods = uniqueN(storeId)), by = positionId] %>%
    .[, positionId := as.integer(positionId)]
  
  didWeNailIt <- merge(monkeyedPodDt, autoSwapDt, all.y = TRUE) %>%
    .[!(is.na(post_autoswap_pods) & final_count == 0)] %>% 
    .[, gap := abs(post_autoswap_pods - final_count)] %>%
    .[order(-gap)]
  
  
  addsConfig <- didWeNailIt[(post_autoswap_pods - final_count) < -15,
                            .(positionId, pod_adjust = gap)]
  anchorItem <- 4014508
  allPogs <- positionDt[, unique(pogId)]
  for (i in 1:nrow(addsConfig)) {
    givenItem <- addsConfig$positionId[i]
    giveNum <- addsConfig$pod_adjust[i]
    candidatePogs <- leftOnly(allPogs, positionDt[positionId == givenItem, unique(pogId)])
    actionPogs <- pogStoreCountDt[pogId %in% candidatePogs][order(-store_count)] %>%
      .[minimumBuildIndices(store_count, giveNum), pogId]
    
    configs <- updateConfigs(configs = configs, existingItemId = anchorItem, newItemId = givenItem, 
                             dt = data.table(display = actionPogs))
  }
  
  
  deletesByBatch <- merge(indexConfig, configs$deletesConfig, by = "PogId") %>%
    .[order(ProjectId)]
  swapsByBatch <- merge(indexConfig, configs$swapsConfig, by = "PogId") %>%
    .[order(ProjectId)]
  insertsByBatch <- merge(indexConfig, configs$insertsConfig, by = "PogId") %>%
    .[order(ProjectId)]
  
  
  alteryxPogData <- fread("C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/10 - TNT/03.05.2023 Revision/03 - Tie Reports/alteryx_pog_data.csv")
  pogDataNew <- alteryxPogData[, .(pogId = `Planogram Desc2`, positionId = ID)] %>% unique
  podDtNew <- merge(storePogDt, pogDataNew, allow.cartesian = TRUE)[, .(num_stores = uniqueN(storeId)), by = positionId]
  
  stillNeedToHit <- merge(podDtNew, autoSwapDt) %>%
    .[, gap := num_stores - final_count] %>%
    merge(attributionDt[, .(positionId, brand, subclass)], ., by = "positionId") %>%
    .[order(gap)]
  
  newPogDt <- cleanNames(alteryxPogData)[, .(positionId = id, pogId = planogram_desc2)] %>%
    merge(indexConfig, by.x = "pogId", by.y = "PogId") %>%
    merge(storePogDt[, .(store_count = uniqueN(storeId)), by = pogId], all = TRUE)
  
  
  pogsWithoutItem <- function(newPogDt, candidateItem, pTag) {
    projectDt <- copy(newPogDt)[grepl(pTag, ProjectId)]
    projectPogsWithItem <- projectDt[positionId == candidateItem, unique(pogId)]
    projectDt[!(pogId %in% projectPogsWithItem), .(pogId, ProjectId, Seq, store_count)] %>% 
      unique %>%
      .[order(-store_count)]
  }
  
  pogsWithItem <- function(newPogDt, candidateItem, pTag) {
    projectDt <- copy(newPogDt)[grepl(pTag, ProjectId)]
    projectPogsWithItem <- projectDt[positionId == candidateItem, unique(pogId)]
    projectDt[(pogId %in% projectPogsWithItem), .(pogId, ProjectId, Seq, store_count)] %>% 
      unique %>%
      .[order(-store_count)]
  }
  
  newUncleanTies <- fread("C:/Users/William.Roberts/Downloads/new_cat_ties.csv")
  newCleanTies <- cleanTieReport(newUncleanTies)
  newStores <- leftOnly(newCleanTies$store %>% unique, storePogDt$storeId %>% unique)
  newUncleanTies[store %in% newStores, uniqueN(store), by = display]
  }

