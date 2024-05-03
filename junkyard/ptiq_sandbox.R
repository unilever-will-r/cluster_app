


loadPtiqData <- function(folderDir, attDt = attributionDt) {
  # 1. Check that all data files exist
  requiredFilesSansExtension <- c("POST JDA DATA", "PRE JDA DATA", 
                                  "POST CAT TIES", "PRE CAT TIES")
  availableFilesSansExtension <- list.files(folderDir) %>% sapply(tools::file_path_sans_ext) %>% unique 
  missingFiles <- setdiff(requiredFilesSansExtension, availableFilesSansExtension)
  if (length(missingFiles) > 0) {
    stop(paste0("Missing these files: ", paste(missingFiles, collapse = ", "), "."))
  } else {
    fullPathsSansExtension <- file.path(folderDir, requiredFilesSansExtension)
    outputDts <- list()
    outputDtNames <- c("pogDtPost", "pogDtPre",
                       "tiesDtPost", "tiesDtPre")
    
    for (i in 1 : length(fullPathsSansExtension)) {
      outputFileSansExtension <- fullPathsSansExtension[i]
      print(paste0("Loading in ", requiredFilesSansExtension[i]))
      outputDts[[outputDtNames[i]]] <- outputFileSansExtension %>%
        loadDataSansExtension %>%
        cleanNames
      print("Success.")
    }

  

    
    # Clean up PogDt -- remove all POGs we dont have data for or dont have item universe items
    outputDts$pogDtPre <- cleansePogData(outputDts$pogDtPre) %>%
      .[itemId %in% attDt[, unique(itemId)]]
    outputDts$pogDtPost <- cleansePogData(outputDts$pogDtPost) %>%
      .[itemId %in% attDt[, unique(itemId)]]
    
    
    # clean up the tie reports
    outputDts$tiesDtPre <- cleanseTieReport(outputDts$tiesDtPre)
    outputDts$tiesDtPost <- cleanseTieReport(outputDts$tiesDtPost)
    
    # Add has_data flag to ties
    hasDataPre <- outputDts$pogDtPre[, unique(pogId)]
    hasDataPost <- outputDts$pogDtPost[, unique(pogId)]
    outputDts$tiesDtPre[, has_data := fifelse(pogId %in% hasDataPre, 1, 0)]
    outputDts$tiesDtPost[, has_data := fifelse(pogId %in% hasDataPost, 1, 0)]
    
    # Add exact_flag to ties
    inBoth <- intersect(outputDts$tiesDtPost[, unique(storeId)], outputDts$tiesDtPre[, unique(storeId)])
    outputDts$tiesDtPre[, in_both := fifelse(storeId %in% inBoth, 1, 0)]
    outputDts$tiesDtPost[, in_both := fifelse(storeId %in% inBoth, 1, 0)]
    return(
      outputDts
    )
  }
}

buildStoreItemDt <- function(storePogDt, pogItemDt, attributionDt) {
  # Filters out all items not in attributionDt
  storeItemSkeleton <- expand.grid(storeId = storePogDt[, unique(storeId)], 
                                   itemId = attributionDt[, unique(itemId)])
  storeItemDt <- merge(storePogDt[, .(storeId, pogId)] %>% unique,
                       pogItemDt[, .(n_positions = .N,
                                     n_facings = sum(position_hfacings),
                                     position_width = sum(position_width)),
                                 by = .(itemId, pogId)],
                       by = c("pogId"), allow.cartesian = TRUE) %>%
    .[, .(n_positions = sum(n_positions),
          n_facings = sum(n_facings),
          position_width = sum(position_width)),
      keyby = .(itemId, storeId)] %>%
    merge(storeItemSkeleton)
  return(storeItemDt)
}

buildPrePostStoreDt <- function(inputData, attDt) {
  preCatTies <- inputData$tiesDtPre
  postCatTies <- inputData$tiesDtPost
  
  storeItemPre <- buildStoreItemDt(preCatTies, inputData$pogDtPre, attDt)
  storeItemPost <- buildStoreItemDt(postCatTies, inputData$pogDtPost, attDt)
  
  preDt <- merge(
    preCatTies[in_both==1 & has_data == 1, .(total_width_pre = sum(width),
                                             n_pogs_pre = uniqueN(pogId)),
               keyby = storeId],
    storeItemPre[, .(n_items_pre = uniqueN(itemId), 
                     n_positions_pre = sum(n_positions),
                     n_facings_pre = sum(n_facings),
                     total_space_pre = sum(position_width)),
                 keyby = .(storeId)])
  postDt <- merge(
    postCatTies[in_both==1 & has_data == 1, .(total_width_post = sum(width),
                                              n_pogs_post = uniqueN(pogId)),
                keyby = storeId],
    storeItemPost[, .(n_items_post = uniqueN(itemId), 
                      n_positions_post = sum(n_positions),
                      n_facings_post = sum(n_facings),
                      total_space_post = sum(position_width)),
                  keyby = .(storeId)])
  
  prePostDt <- merge(preDt, postDt, all = TRUE)
  return(
    prePostDt
  )
}

buildPrePostStoreItemDt <- function(inputData, attDt) {
  preCatTies <- inputData$tiesDtPre
  postCatTies <- inputData$tiesDtPost
  
  storeItemPre <- buildStoreItemDt(preCatTies, inputData$pogDtPre, attDt) %>%
    .[, .(pod_pre = 1,
          facings_pre = sum(n_facings),
          space_pre   = sum(position_width),
          positions_pre = sum(n_positions)),
      keyby = .(storeId, itemId)]
  storeItemPost <- buildStoreItemDt(postCatTies, inputData$pogDtPost, attDt)  %>%
    .[, .(pod_post = 1,
          facings_post = sum(n_facings),
          space_pst   = sum(position_width),
          positions_pst = sum(n_positions)),
      keyby = .(storeId, itemId)]
  
  prePostDt <- merge(storeItemPre, storeItemPost, all = TRUE)
  return(
    prePostDt
  )
}
  
if (FALSE) {
  # mayo 
  baseDir <- "C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/00 - Automation & Reporting/PTIQ/2023"
  categoryDir <- file.path(baseDir, "Mayo Transition")
  inputsDir <- file.path(categoryDir, "Inputs")
  
  attributionDt <- getAttributionDt(config$attributionDtPath, "ICE CREAM")
  
  
  inputData <- loadPtiqData(inputsDir)
  preCatTies <- inputData$tiesDtPre
  postCatTies <- inputData$tiesDtPost
  preJda <- inputData$pogDtPre
  postJda <- inputData$pogDtPost
}

if (FALSE) {
  # cleaners
  baseDir <- "C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/00 - Automation & Reporting/PTIQ/2023"
  categoryDir <- file.path(baseDir, "Cleaners April")
  inputsDir <- file.path(categoryDir, "Inputs")
  
  attributionDt <- getAttributionDt(config$attributionDtPath, "CLEANERS")
  
  
  inputData <- loadPtiqData(inputsDir)
  preCatTies <- inputData$tiesDtPre
  postCatTies <- inputData$tiesDtPost
  preJda <- inputData$pogDtPre
  postJda <- inputData$pogDtPost
}


if (FALSE) {
  baseDir <- "C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/00 - Automation & Reporting/PTIQ/2023"
  categoryDir <- file.path(baseDir, "Dish April")
  inputsDir <- file.path(categoryDir, "Inputs")
  
  attributionDt <- getAttributionDt(config$attributionDtPath, "DISH")
  
  
  inputData <- loadPtiqData(inputsDir, attDt = attributionDt)
  preCatTies <- inputData$tiesDtPre
  postCatTies <- inputData$tiesDtPost
  preJda <- inputData$pogDtPre
  postJda <- inputData$pogDtPost
  
  

  
  prePostDt <- buildPrePostDt(inputData, attDt = attributionDt) %>%
    merge(buildPrePostDt(inputData, attDt = attributionDt[class == "NATURALS"]) %>%
            .[, .(natty_pre = total_space_pre, natty_post = total_space_post, storeId)]) %>%
    # naturals pct change by store
    .[, ':='(naturals_pct = natty_post / natty_pre,
             conv_pct = (total_space_post - natty_post) / (total_space_pre - natty_pre))]
    # total pct change by store
  
  
  
  
  storeItemPre <- buildStoreItemDt(preCatTies, preJda, attributionDt) %>%
    .[, traited_pre := 1] %>%
    .[, pogId := NULL]
  storeItemPost <- buildStoreItemDt(postCatTies, postJda, attributionDt) %>%
    .[, traited_post := 1] %>%
    .[, pogId := NULL]
  storeItemPrePost <- merge(storeItemPre, storeItemPost, by = c("storeId", "itemId"),
                            all = TRUE) %>%
    .[is.na(traited_pre), traited_pre := 0] %>%
    .[is.na(traited_post), traited_post := 0]
  
  idRollup <- storeItemPrePost[, .(basePodCount = sum(traited_pre),
                                   podCount = sum(traited_post),
                                   pods_retained = sum(traited_pre == traited_post),
                                   addedCount = sum(traited_pre < traited_post),
                                   removedCount = sum(traited_pre > traited_post)),
                               keyby = .(itemId)] %>%
    .[, ':='(pod.change = addedCount - removedCount)]
  
  storeRollup <- storeItemPrePost[, .(basePodCount = sum(traited_pre),
                                      podCount = sum(traited_post),
                                      pods_retained = sum(traited_pre == traited_post),
                                      addedCount = sum(traited_pre < traited_post),
                                      removedCount = sum(traited_pre > traited_post)),
                                  keyby = .(storeId)] %>%
    .[, ':='(pod.change = addedCount - removedCount)]
  
  storeRollupHivery <- read_clip_tbl() %>% as.data.table()
  idRollupHivery <- fread("C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/00 - Automation & Reporting/PTIQ/2023/HBL Feb Transition/hivery_item_rollup.csv")
  
  ggStoreData <- rbind(storeRollup[, facet := "Feb 2023 Transition"],
                       storeRollupHivery[, facet := "Projected 2024 Transition"],
                       fill = TRUE)
  
  # Are all NCF'd items in Feb 2023 have base pod count == 0 in Outputs?
  ncfItems <- idRollup[podCount == 0, unique(id)]
  cfItems <- idRollup[podCount > 0, unique(id)]
  idRollupHivery[id %in% cfItems]
  
  ggplot(ggStoreData, aes(x = basePodCount, fill = facet)) + geom_density()
  
  # check data completeness
  prePogNames <- unique(preJda$pogId)
  preCatTies[, pog_flag := fifelse(pogId %in% prePogNames, 1, 0), by = pogId]
  postPogNames <- unique(postJda$pogId)
  postCatTies[, pog_flag := fifelse(pogId %in% postPogNames, 1, 0), by = pogId]
  
  badStores <- c(postCatTies[pog_flag == 0 & (pogId %in% postJda[, unique(pogId)]), unique(store)],
                 preCatTies[pog_flag == 0 & (pogId %in% preJda[, unique(pogId)]), unique(store)]) %>%
    unique
  
  # preCatTies <- preCatTies[!(store %in% badStores)]
  # postCatTies <- postCatTies[!(store %in% badStores)]
  
  
  prePostDt <- merge(preCatTies[, .(pre_space = sum(width * height)), keyby = storeId],
                     postCatTies[, .(post_space = sum(width * height)), keyby = storeId],
                     by = "storeId", all = F)
  
  preDt <- merge(preCatTies[, .(storeId, pogId)] %>% unique, preJda[, .(pre_space = sum(position_width),
                                                                        pre_faces = sum(position_hfacings)),
                                                                    keyby = .(itemId, pogId)],
                 by = "pogId", allow.cartesian = TRUE)
  postDt <- merge(postCatTies[, .(storeId, pogId)] %>% unique, postJda[, .(post_space = sum(position_width),
                                                                           post_faces = sum(position_hfacings)),
                                                                       keyby = .(itemId, pogId)],
                  by = "pogId", allow.cartesian = TRUE)
  
  prePostDt <- merge(preDt[, .(pre_space = sum(pre_space),
                               pre_faces = sum(pre_faces)), by = .(storeId)],
                     postDt[, .(post_space = sum(post_space),
                                post_faces = sum(post_faces)), by = .(storeId)],
                     by = c("storeId"))
  
  
  
  # check data completeness
  prePogNames <- unique(preJda$pogId)
  preCatTies[, pog_flag := fifelse(pogId %in% prePogNames, 1, 0), by = pogId]
  postPogNames <- unique(postJda$pogId)
  postCatTies[, pog_flag := fifelse(pogId %in% postPogNames, 1, 0), by = pogId]
  
  
  prePostNattyDt <- merge(preDt[id %in% naturalItems, .(pre_space = sum(pre_space),
                                                        pre_faces = sum(pre_faces)), by = .(store)],
                          postDt[id %in% naturalItems, .(post_space = sum(post_space),
                                                         post_faces = sum(post_faces)), by = .(store)],
                          by = c("store"))
}
