plotItemChange <- function(configs, itemId, attributionDt, positionDt, storePogDt) {
  
  subcatId <- attributionDt[positionId == itemId, subcatId]
  brandId <- attributionDt[positionId == itemId, brandId]
  itemDescription <- attributionDt[positionId == itemId, `Item Description`]
  
  storePogDt <- copy(storePogDt)[, plot_footage :=min(24, (4 * ceiling(as.numeric(width) / 4))), by = width] 
  
  
  podDt <- merge(positionDt, storePogDt, by = "pogId", allow.cartesian = TRUE) %>% 
    .[, .(store_count = uniqueN(storeId)), by = .(positionId = as.character(positionId), plot_footage)]
  
  deltaDt <- rbind(
    rbind(configs$insertsConfig[, .(pogId = PogId, positionId = CutInItemId)],
          configs$swapsConfig[, .(pogId = PogId, positionId = NewItemId)]) %>%
      .[, delta := 1],
    rbind(configs$deletesConfig[, .(pogId = PogId, positionId = ItemId)],
          configs$swapsConfig[, .(pogId = PogId, positionId = ExistingItemId)]) %>%
      .[, delta := -1]) %>%
    merge(storePogDt,
          by = c("pogId"), allow.cartesian = TRUE) %>%
    .[, .(delta = sum(delta)), by = .(positionId = as.character(positionId), plot_footage)] %>%
    merge(podDt, all = TRUE) %>%
    .[is.na(delta), delta := 0] %>%
    .[is.na(store_count), store_count := 0] %>% 
    merge(attributionDt[, .(positionId = as.character(positionId), 
                            Brand, 
                            subcatId)], by = "positionId")
  
  
  # assign 1 POD to every store the item isn't in the pre period. This is our expansion universe
  expansionUniverseDt <- copy(positionDt) %>% 
    .[, ':='(has_item_flag = fifelse(itemId %in% positionId,  1, 0),
             has_subcat_flag = fifelse(subcatId %in% subcatId, 1, 0)), by = .(pogId)] %>% 
    .[has_subcat_flag == 1 & has_item_flag == 0] %>% 
    .[, .(pogId, store_count)] %>% unique() %>%
    merge(storePogDt) %>%
    .[, .(pod_count = uniqueN(storeId)), by = .(plot_footage)] %>%
    .[, label := "0_Candidate Pool"]
  if (nrow(expansionUniverseDt) == 0) {
    expansionUniverseDt <- copy(positionDt) %>% 
      .[, ':='(has_item_flag = fifelse(itemId %in% positionId,  1, 0),
               has_subcat_flag = fifelse("01 MAYO-WH SAL DRESS" %in% subcatId, 1, 0)), by = .(pogId)] %>% 
      .[has_subcat_flag == 1 & has_item_flag == 0] %>% 
      .[, .(pogId, store_count)] %>% unique() %>%
      merge(storePogDt) %>%
      .[, .(pod_count = uniqueN(storeId)), by = .(plot_footage)] %>%
      .[, label := "0_Candidate Pool"]
  }
  
  itemDeltaDt <- deltaDt[positionId %in% itemId] %>% 
    .[, .(pod_count_pre  = sum(store_count),
          pod_count_post = sum(delta + store_count),
          pod_count_delta = sum(delta)), keyby = .(plot_footage)] %>%
    .[, label := "2_Item"]
  
  subcatDeltaDt <- deltaDt[subcatId == subcatId] %>%
    .[, .(pod_count_pre  = sum(store_count),
          pod_count_post = sum(delta + store_count),
          pod_count_delta = sum(delta)), keyby = .(plot_footage)] %>% 
    .[, label := "0_Subcat"]
  
  brandDeltaDt <- deltaDt[Brand == brandId] %>%
    .[, .(pod_count_pre  = sum(store_count),
          pod_count_post = sum(delta + store_count),
          pod_count_delta = sum(delta)), keyby = .(plot_footage)] %>%
    .[, label := "1_Brand"]
  
  subtitle <- ""
  if (itemDeltaDt[, sum(pod_count_post)] == 0) {
    # Case 1: deleted item
    # We want to see the brand before, then item before, then item after (as sanity check)
    subtitle <- "NCF Item"
    ylab_title = "PODs"
    ggDt <- rbind(brandDeltaDt[, .(y = pod_count_pre), by = .(plot_footage, label = paste0(label, " .Preautomation"))],
                  itemDeltaDt[, .(y = pod_count_pre), by = .(plot_footage, label = paste0(label, " .Preautomation"))],
                  itemDeltaDt[, .(y = pod_count_post), by = .(plot_footage, label = paste0(label, " Postautomation"))])
  } else if (itemDeltaDt[, (0 < sum(pod_count_pre)) & (sum(pod_count_pre) < sum(pod_count_post))]) {
    # Case 2: item increase
    subtitle <- "Item Increase"
    # We want to see:
    #     (1) share of pods of stores that carry the subcatId that didn't carry the item before
    #     (2) share of pods that carry the subcatId that carried the item before (i.e. that carried the item before)
    #     (3) share of the PODs added of item by type
    ylab_title <- "Share of PODs"
    ggDt <- rbind(expansionUniverseDt[, .(y = pod_count), by = .(plot_footage, label)],
                  itemDeltaDt[, .(y = abs(pod_count_pre)), by = .(plot_footage, label = paste0(label, " .Preautomation"))],
                  itemDeltaDt[, .(y = pod_count_delta), by = .(plot_footage, label = paste0(label, " ADDS"))])
  } else if (itemDeltaDt[, (0 < sum(pod_count_pre)) & (sum(pod_count_pre) > sum(pod_count_post))]) {
    # Case 2: item increase
    subtitle <- "Item Decrease"
    # We want to see:
    #     (1) share of pods of stores that carry the subcatId that didn't carry the item before
    #     (2) share of pods that carry the subcatId that carried the item before (i.e. that carried the item before)
    #     (3) share of the PODs added of item by type
    ylab_title <- "PODs"
    ggDt <- rbind(itemDeltaDt[, .(y = pod_count_pre), by = .(plot_footage, label = paste0(label, " .Preautomation"))],
                  itemDeltaDt[, .(y = pod_count_post), by = .(plot_footage, label = paste0(label, " Postautomation"))])
  } else if (itemDeltaDt[, sum(pod_count_pre) == 0]) {
    # Case 4: new item
    subtitle <- "New Item"
    ylab_title <- "PODs"
    ggDt <- rbind(subcatDeltaDt[, .(y = pod_count_pre), by = .(plot_footage, label = paste0(label, ""))],
                  brandDeltaDt[, .(y = pod_count_pre), by = .(plot_footage, label = paste0(label, ""))],
                  itemDeltaDt[, .(y = pod_count_post), by = .(plot_footage, label = paste0(label))]) %>%
      .[, y := y / sum(y), by = label]
  }
  
  ggDt %>%
    ggplot(aes(x = as.factor(plot_footage), y = y, fill = factor(label))) + 
    geom_bar(stat = "identity", position = "dodge") + 
    theme_minimal() + 
    theme(legend.position = "top",
          legend.title = element_blank()) + 
    ggtitle(paste0(itemDescription, " (", subtitle, "); [", itemId, "]"),
            subtitle = paste0("Brand: ", brandId, "; subcatId: ", subcatId)) + 
    ylab(ylab_title) + 
    xlab("plot_footage")
}


buildPositionDt <- function(configs = initializeConfigs(), positionDt, attributionDt, perfLibDt, storePogDt) {
  
  requiredColumnNames <- c("pogId",
                           "fixtureId",
                           "positionId",
                           "positionHFacings",
                           "positionVFacings",
                           "positionWidth",
                           "positionHeight",
                           "positionX",
                           "positionY",
                           "positionRankX")
  checkRequiredColumnNames(positionDt, requiredColumnNames)
  newPositionDt <- copy(positionDt) %>% .[, ..requiredColumnNames]
  
  # rebuilds the calculated fields on the position dt
  pogDeletes <- rbind(configs$deletesConfig[, .(pogId = PogId, positionId = as.integer(ItemId))],
                      configs$swapsConfig[, .(pogId = PogId, positionId = as.integer(ExistingItemId))])
  setkey(pogDeletes, pogId, positionId)
  pogAdds <- rbind(configs$insertsConfig[, .(pogId = PogId, newItemId = as.integer(CutInItemId), anchorId = as.integer(ExistingItemId))],
                   configs$swapsConfig[, .(pogId = PogId, newItemId = as.integer(NewItemId), anchorId = as.integer(ExistingItemId))])
  setkey(pogAdds, pogId, anchorId)
  setkey(newPositionDt, pogId, positionId) 
  # We out the deletes
  newPositionDt <- newPositionDt[!pogDeletes] # get rid of the deletes
  
  # Add performance to positionDt
  # we do this before adding the inserts to keep them from bring in sales info -- we dont want sales info for inserts
  setkey(perfLibDt, pogId, positionId)
  setkey(newPositionDt, pogId, positionId)
  newPositionDt <- perfLibDt[newPositionDt]
  
  # add on the inserts
  newPositionDt <- rbind(newPositionDt,
                         newPositionDt[pogAdds, .(pogId, 
                                                  fixtureId,
                                                  positionId = newItemId,
                                                  positionHFacings = 1, 
                                                  positionWidth = positionWidth / positionHFacings)] %>%
                           .[, .(positionWidth = min(positionWidth)),
                             by = .(pogId, fixtureId, positionId, positionHFacings)],
                         fill = TRUE)
  
  
  # Update the facings per the config 
  facingUpdates <- configs$facesConfig[, .(pogId = PogId, positionId = as.numeric(ItemId), new_face = FaceValue)] 
  setkey(facingUpdates, pogId, positionId)
  newPositionDt <- facingUpdates[newPositionDt] %>%
    .[!is.na(new_face), ':='(positionHFacings = new_face,
                            positionWidth = new_face * (positionWidth / positionHFacings))] %>%
    .[, new_face := NULL]
   
  # Add store counts to newPositionDt
  pogCountDt <- storePogDt[, .(store_count = uniqueN(storeId)), by = pogId]
  setkey(pogCountDt, pogId)
  setkey(newPositionDt, pogId)
  newPositionDt <- pogCountDt[newPositionDt] 
  
  # Add Brand to newPositionDt
  setkey(newPositionDt, positionId)
  setkey(attributionDt, positionId)
  newPositionDt <- attributionDt[newPositionDt]
  
  # Add max facings and brands per shelf
  newPositionDt %>% 
    .[, brands_per_shelf := uniqueN(brandId), by = .(pogId, fixtureId)] %>%
    .[, max_facings_on_shelf := max(positionHFacings), by = .(pogId, fixtureId)] %>%
    .[, brand_facings_per_item := mean(positionHFacings), by = .(pogId, brandId)] %>%
    .[, subcat_facings_per_item := mean(positionHFacings), by = .(pogId, subcatId)]
  
  
  
  # Calculate space to sales index
  
  # calculate share of space at brandId level
  newPositionDt[, brand_upspw := sum(eupspw, na.rm = TRUE), by = .(pogId, brandId)]
  newPositionDt[, box_upspw := sum(eupspw, na.rm = TRUE), by = .(pogId)]
  newPositionDt[, brand_sales_share := brand_upspw / box_upspw]
  
  # Calculate share of sales at brand level
  newPositionDt[, brand_space := sum(positionWidth, na.rm = TRUE), by = .(pogId, brandId)]
  newPositionDt[, pog_space := sum(positionWidth, na.rm = TRUE), by = .(pogId)]
  newPositionDt[, brand_space_share := brand_space / pog_space]
  
  # s2s
  newPositionDt[, brand_s2s := brand_space_share / brand_sales_share]
  newPositionDt[is.infinite(brand_s2s), brand_s2s := NA]
  
  # Same process for subcatId
  # calculate share of space at subcatId level
  newPositionDt[, subcat_upspw := sum(eupspw, na.rm = TRUE), by = .(pogId, subcatId)]
  newPositionDt[, subcat_sales_share := subcat_upspw / box_upspw]
  newPositionDt[, subcat_space := sum(positionWidth, na.rm = TRUE), by = .(pogId, subcatId)]
  newPositionDt[, subcat_space_share := subcat_space / pog_space]
  newPositionDt[, subcat_s2s := subcat_space_share / subcat_sales_share]
  newPositionDt[is.infinite(subcat_s2s), subcat_s2s := NA]
  
  # Same process at item level
  newPositionDt[, item_sales_share := eupspw / box_upspw]
  newPositionDt[, item_space_share := positionWidth / pog_space]
  newPositionDt[, item_s2s := item_sales_share / item_space_share]
  newPositionDt[is.infinite(item_s2s), item_s2s := NA]
  
  # Index it
  newPositionDt[!is.na(brand_s2s), brand_s2s_index := 100 * brand_s2s / mean(brand_s2s), by = pogId]
  newPositionDt[!is.na(item_s2s), item_s2s_index := 100 * item_s2s / mean(item_s2s), by = pogId]
  newPositionDt[!is.na(subcat_s2s), subcat_s2s_index := 100 * subcat_s2s / mean(subcat_s2s), by = pogId]
  
  
  
  return(
    newPositionDt
  )
  
}

updateConfigsInsertItemsByPogList <- function(configs, 
                                              actionStores, 
                                              newItemIds,
                                              positionDt,
                                              attributionDt,
                                              storePogDt,
                                              orientationString = "RightofPosition",
                                              faceKey = NULL,
                                              itemSubset = NULL) {
  # Add a list of items onto a prescribed list of stores
  # The items are placed to the top-rightmost item in its brand-subcatId grouping
  # If there are no items in its brand-subcatId grouping for all the stores, the function will error
  if (is.null(itemSubset)) {
    itemSubset <- attributionDt[, unique(positionId)]
  }
  
  for (itemId in newItemIds) {
    
    itemInfo <- attributionDt[positionId == itemId, .(brandId, subcatId)]
    
    superPositionDt <- copy(positionDt)[pogId %in% storePogDt[storeId %in% actionStores, unique(pogId)]]
    setkey(superPositionDt, pogId)
    setkey(storePogDt, pogId)
    superStorePositionDt <- storePogDt[superPositionDt, allow.cartesian = TRUE] %>%
      merge(itemInfo, by = c("brandId", "subcatId")) %>%
      .[positionId %in% itemSubset]
    if (nrow(superStorePositionDt) == 0) {
      superStorePositionDt <- storePogDt[superPositionDt, allow.cartesian = TRUE] %>%
        merge(itemInfo, by = c("subcatId")) %>%
        .[positionId %in% itemSubset]
    }
    if (nrow(superStorePositionDt) == 0) {
      # If the subcatId is brand new, put it next to regular mayo items (kewpie mayo...)
      superStorePositionDt <- storePogDt[superPositionDt, allow.cartesian = TRUE] %>%
        .[subcatId == "01 MAYO-WH SAL DRESS"] %>%
        .[positionId %in% itemSubset]
    }
    placementsPerPog <- superStorePositionDt[, .(pog_locs = uniqueN(pogId)), by = storeId]
    # Check 1: are we accidentally affecting more or less stores than we meant to?
    if (placementsPerPog[pog_locs > 1, uniqueN(storeId)] > 0) {
      stop(paste0("subcatId: ", itemInfo$subcatId, " and brandId: ", itemInfo$brandId, " are spread across two pogs"))
    }
    superStorePositionDt[, distance_from_origin := sqrt(positionX^2 + positionY^2)]
    superStorePositionDt[, max_dist := min(distance_from_origin), by = storeId]
    dupedPogs <- superStorePositionDt[max_dist == distance_from_origin, pogId] 
    insertsDt <- superStorePositionDt[max_dist == distance_from_origin, .(pogId, positionId, positionHFacings)] %>% unique
    for (insertId in insertsDt[, unique(positionId)]) {
      configs <- updateConfigs(configs = configs, 
                               existingItemId = insertId,
                               newItemId = itemId,
                               orientation = orientationString, 
                               faceKey = faceKey,
                               dt = insertsDt[positionId == insertId, .(display = pogId, position_hfacings = positionHFacings)])
    }
  }
  return(configs)
}

updateConfigsGenericIncrease <- function(itemId, 
                                         eligibleStores,
                                         storeCount = 589,
                                         configs,
                                         positionDt,
                                         attributionDt,
                                         storePogDt,
                                         itemGroupIds = NULL,
                                         faceKey = NULL,
                                         ranking_var = "brand_s2s_index",
                                         rankDecreaseBool = FALSE,
                                         agg_fun = mean, ...) {
  storesCurrentlyIn <- positionDt[positionId == itemId, .(pogId)] %>% unique %>%
    merge(storePogDt, by = "pogId") %>%
    .[, unique(storeId)]
  
  storeCountGap <- storeCount - length(storesCurrentlyIn)
  actionStores <- leftOnly(eligibleStores, storesCurrentlyIn)
  actionStoreOnlyPogs <- leftOnly(storePogDt[storeId %in% actionStores, unique(pogId)], storePogDt[!(storeId %in% actionStores), unique(pogId)])
  
  if (is.null(itemGroupIds)) {
    itemGroupIds <- attributionDt %>%
      .[brandId == attributionDt[positionId == itemId, brandId]] %>%
      .[subcatId == attributionDt[positionId == itemId, subcatId], positionId]
  }
  
  actionPogs <- storePogDt[pogId %in% actionStoreOnlyPogs, unique(pogId)] %>%
    intersect(positionDt[positionId %in% itemGroupIds, unique(pogId)])
  if (storePogDt[pogId %in% actionPogs, .N, by = storeId][N > 1, .N] > 0) {
    stop("itemGroupIds dually located in some store")
  }
  
  
  
  positionDt <- copy(positionDt)
  setnames(positionDt, old = ranking_var, new = "ranking_var")
  pogRankDt <- positionDt[(positionId %in% itemGroupIds) & (pogId %in% actionPogs),
                          .(ranking_var = ranking_var %>% agg_fun(na.rm = TRUE)), 
                          by = .(pogId, store_count)] %>% 
    unique %>%
    .[order(ranking_var, decreasing = rankDecreaseBool)] %>%
    .[, cumsum := cumsum(store_count)]
  actionStores <- storePogDt[pogId %in% pogRankDt[cumsum <= storeCountGap, pogId], storeId]
  
  
  configs <- updateConfigsInsertItemsByPogList(configs = configs,
                                               actionStores = actionStores,
                                               newItemIds = itemId,
                                               positionDt = positionDt,
                                               attributionDt = attributionDt,
                                               storePogDt = storePogDt,
                                               faceKey = faceKey, 
                                               itemSubset = itemGroupIds)
  
  print(paste0("The item s currently in this many stores: ", storesCurrentlyIn %>% length))
  print(paste0("To hit our gap we need to put it in this: ", storeCountGap))
  print(paste0("We wound up hitting a store count of    : ", length(actionStores)))
  return(configs)
}

initializeConfigs <- function() {
  return(
    list(
      insertsConfig = data.table(
        PogId          = character(), 
        ExistingItemId = character(), 
        CutInItemId    = character(), 
        orientation    = character(),
        seq            = integer()
      ),
      facesConfig = data.table(
        PogId     = character(),
        ItemId    = character(),
        FaceValue = integer(),
        seq       = integer()
      ),
      deletesConfig = data.table(
        PogId         = character(),
        ItemId        = character(),
        RelativeRank  = character(),
        seq           = integer(),
        PreDeleteFlag = integer()
      ),
      fixtureDeletesConfig = data.table(
        PogId  = character(),
        ItemId = character(),
        seq    = integer()
      ),
      swapsConfig = data.table(
        PogId          = character(),
        ExistingItemId = character(),
        NewItemId      = character(),
        seq            = integer()
      )
    )
  )
}

writeConfigs <- function(configsOut, projectConfig, testOut = FALSE, test_n = 4, tag = "") {
  # also does some checks
  # check1: that Seq doesn't skip any numbers
  sortedOutSeqs <- configsOut %>% lapply(FUN = function(x) x[, unique(seq)]) %>% unlist %>% sort
  if (length(sortedOutSeqs) > 0) {
    seqSkeleton <- min(sortedOutSeqs) : max(sortedOutSeqs)
    if (length(sortedOutSeqs) != length(seqSkeleton)) {
      print(leftOnly(seqSkeleton, sortedOutSeqs));
      # stop("Check the logic on seq these seqs are missing")
    }
  }
   
  if (!dir.exists(projectConfig$configsDir)) {dir.create(projectConfig$configsDir)}
  basePaths <- lapply(names(configsOut), FUN = function(x) file.path(projectConfig$configsDir, paste0(x, tag, ".csv"))) %>% unlist
  mapply(configsOut, basePaths, FUN = fwrite)
  
  if (testOut) {
    # Sample at least test_n-many PogIds per seq
    set.seed(1)
    stratifiedConfigsSample <- lapply(configsOut, function(x) x[, .SD[sample(.N, min(.N, test_n), replace = FALSE)], by = seq, 
                                                                .SDcols = setdiff(names(x), "seq")])
    stratifiedSamplePogs <- getDisplayListFromConfigs(stratifiedConfigsSample)[, unique(PogId)]
    testConfigs <- lapply(configsOut, function(x) x[PogId %in% stratifiedSamplePogs])
    
    # Writing out all the test configs except the index
    if (!dir.exists(projectConfig$configsTestDir)) {dir.create(projectConfig$configsTestDir)}
    baseTestPaths <- lapply(names(configsOut), FUN = function(x) paste0(projectConfig$configsTestDir, x, ".csv")) %>% unlist
    mapply(testConfigs, baseTestPaths, FUN = fwrite)
    
    # Writing the index config - should sample from the original unless it doesn't exist, then make dummy
    indexConfigPath <- file.path(projectConfig$configsDir, "indexConfig.csv")
    if (file.exists(indexConfigPath)) {
      testIndexConfig <- fread(indexConfigPath) %>%
        .[PogId %in% stratifiedSamplePogs] %>%
        .[order(Seq), Seq := seq_len(.N), by = ProjectId] %>%
        .[order(ProjectId, Seq)]
        
    } else {
      warning("No index config exists from which to sample. Creating dummy test index config.")
      testIndexConfig <- data.table(PogId = stratifiedSamplePogs) %>%
        .[sample(.N)] %>%
        .[, ProjectId := sample(1 : min(floor(sqrt(length(stratifiedSamplePogs)))), .N, replace = TRUE) %>%
            formatC(width = 2, flag = "0") %>%
            paste0(" - DUMMY BATCH")] %>%
        .[, Seq := sample(.N, .N, replace = FALSE)] %>%
        .[order(Seq), Seq := seq_len(.N), by = ProjectId] %>%
        .[order(ProjectId, Seq)]
      }
    fwrite(testIndexConfig, file.path(projectConfig$configsTestDir, "indexConfig.csv"))
  }
}

getSeq <- function(configs_ = configs) {
  maxRow <- lapply(configs_, FUN = NROW) %>% unlist %>% max
  if (maxRow == 0) {
    return(
      1
    )
  } else {
    maxSeq <- lapply(configs_, FUN = function(x) ifelse(nrow(x) > 0, max(x$seq), 0)) %>% unlist %>% max
    return(
      maxSeq + 1
    )
  }
}

getSeqList <- function(baseSeq, numRows, chunkSize = 75) {
  return(
    baseSeq + floor((seq_len(numRows) - 1) / chunkSize)
  )
}

getAdjacentItem <- function(pogId, itemId, pogData, orientation = "RightofPosition") {
  requiredColumns <- c("id", "planogram_desc2", 
                       "position_locationid",
                       "position_rankx", "position_ranky", "position_rankz",
                       "product_brand",
                       "product_width", "product_height",
                       "position_width",
                       "filename")
  subsetData <- pogData[, ..requiredColumns] %>%
    unique() %>%
    .[planogram_desc2 == pogId]
  itemLocation <- subsetData[id == itemId]
  # If the item shows up multiple times, put in first position
  
  
}

updateConfigsInsertItemEverywhere <- function(configs,
                                              pogData,
                                              tieReportDt,
                                              newItem,
                                              priorityItems) {
  requiredColumnNames <- c("store", "display")
  checkRequiredColumnNames(tieReportDt, requiredColumnNames)
  tieReportDt <- tieReportDt[, ..requiredColumnNames] %>% unique
  requiredColumnNames <- c("id", "display")
  checkRequiredColumnNames(pogData, requiredColumnNames)
  pogData <- pogData[, ..requiredColumnNames]
  pogData <- pogData[display %in% tieReportDt$display] # not all pogs in cks so not all data
  storeItemDt <- merge(tieReportDt, pogData, by = c("display"), allow.cartesian = TRUE)
  storeItemDt[, priority_key := min(which(priorityItems %in% id)), by = store]
  # Any NA's mean store doesn't carry these items, so we aren't cutting it in
  storeItemDt <- storeItemDt[!is.na(priority_key)]
  
  storeItemDt[, store_priority_key := min(priority_key), by = store]
  storeItemDt[, priority_item := priorityItems[store_priority_key]]

  
  insertDt <- storeItemDt[priority_item == id, 
                          .(PogId = display,
                            ExistingItemId = priority_item)] %>% unique() %>%
    .[, CutInItemId := new1]
  for (item in insertDt[, unique(ExistingItemId)]) {
    cur_seq <- getSeq(configs);
    insertDtSubset <- insertDt[ExistingItemId == item]
    insertDtSubset <- insertDtSubset %>%
      .[, seq := getSeqList(cur_seq, .N)] %>%
      .[, orientation := "LeftofPosition"]
    configs$insertsConfig <- rbind(configs$insertsConfig, insertDtSubset)
  }
  return(
    configs
  )
}

updateConfigsSwapItemEverywherePegboard <- function(configs,
                                                    pogData,
                                                    tieReportDt,
                                                    newItem,
                                                    priorityItems,
                                                    completeSwap = FALSE) {
  requiredColumnNames <- c("store", "display")
  checkRequiredColumnNames(tieReportDt, requiredColumnNames)
  tieReportDt <- tieReportDt[, ..requiredColumnNames] %>% unique
  requiredColumnNames <- c("id", "display")
  checkRequiredColumnNames(pogData, requiredColumnNames)
  pogData <- pogData[, ..requiredColumnNames] %>% .[, .(facings = .N), by = .(id, display)]
  pogData <- pogData[display %in% tieReportDt$display] # not all pogs in cks so not all data
  pogData[, num_items := uniqueN(id), by = display]
  storeItemDt <- merge(tieReportDt, pogData, by = c("display"), allow.cartesian = TRUE)
  # Filter store item dt to universe of eligible items
  # eligible items (1) are priority items (2) have more than one facing
  storeItemDt <- storeItemDt[facings > 1 & id %in% priorityItems]
  # Eligible pogs are the POGs with the highest # of items in at least one store
  # ASSUMPTION CHECK: if a POG has the fewest items in one store, it won't have the most items in another store
  storeItemDt[, is_biggest_in_store := fifelse(num_items == max(num_items), 1, 0), by =  store]
  assCheck <- storeItemDt[, .(uh_oh = uniqueN(is_biggest_in_store)), by = display][uh_oh > 1, display]
  if (length(assCheck) > 0) {
    warning(paste0("These pogs are weird look into them:", paste(assCheck, collapse = ",")))
  }
  
  eligiblePogs <- storeItemDt[is_biggest_in_store == 1, uniqueN(display)]
  
  storeItemDt[, priority_key := min(which(priorityItems %in% id)), by = store]
  # Any NA's mean store doesn't carry any priority items with multiple facings, so we can't swap into those stores
  # TODO: output log of these stores getting dropped
  storeItemDt <- storeItemDt[!is.na(priority_key)]
  
  storeItemDt[, store_priority_key := min(priority_key), by = store]
  storeItemDt[, priority_item := priorityItems[store_priority_key]]
  if (completeSwap) {
    swapsDt <- storeItemDt[priority_item == id,
                           .(PogId = display,
                             ExistingItemId = priority_item)] %>% unique %>% 
      .[, NewItemId := new1]
    for (item in swapsDt[, unique(ExistingItemId)]) {
      cur_seq <- getSeq(configs)
      swapsDtSubset <- swapsDt[ExistingItemId == item]
      swapsDtSubset <- swapsDtSubset %>%
        .[, seq := getSeqList(cur_seq, .N)]
      configs$swapsConfig <- rbind(configs$swapsConfig, swapsDtSubset)
    }
  } else {
    # if not complete swap, we want to do an insert
    # TODO: configure C# code to know the difference between a pegboard swap-insert and a regular insert
    insertDt <- storeItemDt[priority_item == id, 
                            .(PogId = display,
                              ExistingItemId = priority_item)] %>% unique() %>%
      .[, CutInItemId := new1]
    for (item in insertDt[, unique(ExistingItemId)]) {
      cur_seq <- getSeq(configs);
      insertDtSubset <- insertDt[ExistingItemId == item]
      insertDtSubset <- insertDtSubset %>%
        .[, seq := getSeqList(cur_seq, .N)] %>%
        .[, orientation := "LeftofPosition"]
      configs$insertsConfig <- rbind(configs$insertsConfig, insertDtSubset)
    }
  }
  return(
    configs
  )
}

updateConfigsSwap <- function(configs,
                              actionPogs,
                              existingItemId,
                              newItemId) {
  swap_seq <- getSeq(configs)
  swapDt <- data.table(PogId = actionPogs,
                       ExistingItemId = existingItemId,
                       NewItemId = newItemId) %>%
    .[, seq := getSeqList(swap_seq, .N)]
  configs$swapsConfig <- rbind(configs$swapsConfig, swapDt)
  return(
    configs
  )
}

updateConfigsDelete <- function(configs,
                                actionPogs,
                                deleteItem,
                                faceOutOrientation = 0,
                                preDeleteFlag = 0) {
  delete_seq <- getSeq(configs)
  deleteDt <- data.table(PogId = actionPogs, 
                         ItemId = deleteItem,
                         RelativeRank = faceOutOrientation,
                         PreDeleteFlag = preDeleteFlag) %>%
    .[, seq := getSeqList(delete_seq, .N)]
  
  configs$deletesConfig <- rbind(configs$deletesConfig,
                                 deleteDt)
  return(configs)
}

updateConfigs <- function(configs,
                          existingItemId,
                          newItemId,
                          dt,
                          faceKey = NULL, 
                          orientation = c("LeftofPosition", "RightofPosition")) {
  
  orientation <- match.arg(orientation)
  
  dtIn <- copy(dt)
  
  # Update inserts
  insert_seq <- getSeq(configs)
  insertDt <- dt[, .(PogId = display, 
                     ExistingItemId = existingItemId,
                     CutInItemId = newItemId,
                     orientation = orientation)] %>% unique %>%
    .[, seq := getSeqList(insert_seq, .N)]
  configs$insertsConfig <- rbind(configs$insertsConfig, insertDt)
  
  # Update facings
  face_seq <- getSeq(configs)
  if (!is.null(faceKey)) {
    
    requiredColumnNames <- c("position_hfacings", "new_item_face", "old_item_face")
    checkRequiredColumnNames(faceKey, requiredColumnNames)
    faceKey <- faceKey[, ..requiredColumnNames] %>%
      .[, seq := getSeqList(face_seq, .N) + seq_len(.N)]
    
    dt <- merge(dt, faceKey, by = "position_hfacings")
    # Update face changes
    faceOld <- dt[, .(PogId = display,
                      ItemId = existingItemId,
                      FaceValue = old_item_face,
                      position_hfacings,
                      seq)] %>%
      .[FaceValue != position_hfacings] %>%
      .[, position_hfacings := NULL]
    configs$facesConfig <- rbind(faceOld, configs$facesConfig)
    
    face_seq <- getSeq(configs)
    faceKey <- faceKey[, ..requiredColumnNames] %>%
      .[, seq := getSeqList(face_seq, .N) + seq_len(.N)]
    dt <- merge(dtIn, faceKey, by = "position_hfacings")
    
    faceNew <- dt[, .(PogId = display,
                      ItemId = newItemId,
                      FaceValue = new_item_face,
                      seq)]
    configs$facesConfig <- rbind(faceNew, configs$facesConfig)

  }
  
  return(configs)
}

updateConfigsFacings <- function(configs,
                                 dt,
                                 existingItemId) {
  requiredColumnNames <- c("display", "position_hfacings", "new_hfacings")
  dt <- dt[, ..requiredColumnNames]
  face_seq <- getSeq(configs)
  faceChange <- dt[, .(PogId = display,
                       ItemId = existingItemId,
                       FaceValue = new_hfacings)] %>% unique()
  if (nrow(dt) != nrow(faceChange)) {
    stop("dt is not unique by row.")
  }
  faceChangeSkeleton <- faceChange[, .N, by = .(ItemId, FaceValue)]
  for (i in 1 : nrow(faceChangeSkeleton)) {
    face_seq <- getSeq(configs);
    faceChangeSubset <- faceChange[ItemId == faceChangeSkeleton$ItemId[i]
                                   ][FaceValue == faceChangeSkeleton$FaceValue[i]] %>%
      .[, seq := getSeqList(face_seq, .N)]
    configs$facesConfig <- configs$facesConfig %>%
      rbind(faceChangeSubset)
  }
  return(configs)
}

updateConfigsFixtureDelete <- function(configs,
                                       actionDt,
                                       fixtureDeleteItem) {
  delete_seq <- getSeq(configs)
  deleteDt <- actionDt[, .(PogId = display,
                           ItemId = id)] %>%
    .[, seq := getSeqList(delete_seq, .N)]
  configs$fixtureDeletesConfig <- rbind(configs$fixtureDeletesConfig,
                                        deleteDt)
  return(configs)
}


getAdjacentItemDt <- function(positionDt, targetItem, directionInt = -1) {
  requiredColumnNames <- c("positionId", "pogId", "positionRankX", "fixtureId")
  checkRequiredColumnNames(positionDt, requiredColumnNames)
  filteredPogData <- positionDt[, has_targetItem := fifelse(targetItem %in% positionId, 1, 0), by = pogId] %>%
    .[has_targetItem == 1]
  if (directionInt < 0) {
    condDt <- filteredPogData[, .(positionRankX = min(positionRankX)),
                              by = .(positionId, fixtureId, pogId)]
  } else {
    condDt <- filteredPogData[, .(positionRankX = max(positionRankX)),
                              by = .(positionId, fixtureId, pogId)]
  }
  targetItemDt <- filteredPogData[positionId == targetItem,
                                  .(fixtureId = fixtureId[1],
                                    target_facings = positionHFacings[1]), 
                                  by = .(positionId, pogId)] %>%
    merge(condDt, by = c("positionId", "pogId", "fixtureId"))
  adjacentItems <- merge(targetItemDt, 
                         filteredPogData[, .(fixtureId, 
                                             adjacent_id = positionId,
                                             pogId,
                                             positionRankX = positionRankX - directionInt,
                                             adjacent_facings = positionHFacings)],
                         by = c("pogId", "fixtureId", "positionRankX")) %>%
    .[, .(skeleton_key = .N), by = .(pogId, fixtureId, positionRankX, positionId, 
                                         adjacent_id, adjacent_facings, target_facings)] %>%
    .[, skeleton_key := NULL]
  return(
    adjacentItems
  )
}

getShelfMatesDt <- function(positionDt, targetItem) {
  requiredColumnNames <- c("positionId", "pogId", "positionRankX", "fixtureId")
  checkRequiredColumnNames(positionDt, requiredColumnNames)
  shelfSkeleton <- positionDt[positionId == targetItem, .(fixtureId, pogId)] %>% unique()
  
  filteredDt <- merge(shelfSkeleton, positionDt, by = c("fixtureId", "pogId"))
  targetDt <- filteredDt[positionId == targetItem, .(pogId, fixtureId, positionId, target_facings = positionHFacings)]
  matesDt <- filteredDt[positionId != targetItem, .(pogId, fixtureId, adjacent_id = positionId, adjacent_facings = positionHFacings)]
  shelfMatesDt <- merge(targetDt,
                        matesDt,
                        by = c("pogId", "fixtureId"))
  return(
    shelfMatesDt
  )
  
}

getAdjacentItemSummaryTable <- function(positionDt, targetItem, directionInt = -1, attDt = NULL) {
  requiredColumnNames <- c("display", "id", "item_description", "store_tied")
  checkRequiredColumnNames(itemPogDt, requiredColumnNames)
  itemPogDt <- itemPogDt[, ..requiredColumnNames]
  adjacentItems <- getAdjacentItemDt(pogData, targetItem, directionInt)
  if (nrow(adjacentItems) != adjacentItems[, uniqueN(display)]) {
    stop("logic is broken w/r/t dually located items")
  }
  outputSkeleton <- itemPogDt[id == targetItem, .(display, store_tied)]
  summaryTable <- merge(outputSkeleton, adjacentItems,
                        by = "display", all.x = TRUE) %>% 
    .[, .(stores_tied = sum(store_tied)), by = .(adjacent_id)] 
  if (!is.null(attDt)) {
    requiredColumnNames <- c("dpci", "item_description")
    checkRequiredColumnNames(attDt, requiredColumnNames)
    attDt <- attDt[, ..requiredColumnNames]
    summaryTable <- merge(summaryTable, attDt, by.x = "adjacent_id", by.y = "dpci", all.x = TRUE)
  }
  return(summaryTable[order(-stores_tied)])
}


getDisplayListFromConfigs <- function(configs_) {
  return(
    lapply(configs_, function(x) x[, .N, by = .(PogId, seq)][, N := NULL]) %>% rbindlist %>% unique
  )
}

if (FALSE) {
  setwd("~/ic_revision_sandbox")
  pogData <- fread("pog_data_fat.csv") %>% cleanNames %>% .[, display := planogram_desc2]
}
