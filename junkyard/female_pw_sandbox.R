if (FALSE) {
  rm(list = ls(all.names = TRUE)); gc();
  setwd("~/r_code")
  source("00_initialize.R")
  config <- initializeProject(configPath = "~/r_code/revision_configs/config_pw.yaml")
  
  uncleanMegaDt <- fread("C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/08 - Personal Wash/Oct 2023 Revision FEMALE/03 - Tie Reports/cat_ties_7_25_2023.csv") %>%
    cleanNames
  # uncleanMegaDt <- fread(gsub(pattern = "unclean_ties", replacement = "unclean_mega_ties", config$rawTieReportPath)) %>%
  #   cleanNames
  # TODO: Centralize 
  uncleanTieDt <- uncleanMegaDt[(grepl("FEMALE & FAMILY|FEMALE AND FAMILY SOAP|FEMALE/FAMILY SOAP", title)) |
                                  ((!grepl("MALE", title)) & grepl("SOAP", title))]%>%
    .[!grepl("LIQUID HAND SOAP", title)]
  cleanMegaDt <- buildTieReport(uncleanMegaDt, inDateFormat = "%Y-%m-%d")
  
  
  cleanTieDt <- buildTieReport(uncleanTieDt, inDateFormat = "%Y-%m-%d")
  
  cleanTieDt <- cleanMegaDt[(grepl("FEMALE & FAMILY|FEMALE AND FAMILY SOAP|FEMALE/FAMILY SOAP", title)) |
                                ((!grepl("MALE", title)) & grepl("SOAP", title))] %>%
    .[!grepl("LIQUID HAND SOAP", title)]
  if (FALSE) {
    fwrite(cleanTieDt[, ':='(pogId = display, storeId = store)], config$cleanTieReportPath)
  }
  
  # New performance library
  prePeriodStorePogDt <- uncleanTieDt[set_date < "9/01/2023"] %>% 
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
  if (FALSE) {
    fwrite(perfLibForOutput, file.path(config$baseDir, "female_pw_new_perf_lib.csv"))
  }
  
  # Read in preautomation data
  pogDataDir <- file.path(config$baseDir, "pog_data")
  pogDataTag <- "pre_auto"
  pogDt <- readPlanogramTable(fileDir = pogDataDir, tag = pogDataTag)
  fixtureDt <- readFixtureTable(fileDir = pogDataDir, tag = pogDataTag)
  positionDt <- readPositionTable(fileDir = pogDataDir, tag = pogDataTag)
  
  
  
  # Second round: late autofills
  lateAutos <- fread(file.path(config$filesDir, "lateAutos.csv"))[, unique(pogId)]
  positionDt <- positionDt[pogId %in% lateAutos]
  
  # Create configs
  # 1. Delete all native items
  # 2. don't touch quiet + roar -- those will be highlighted
  # 3. Insert the native items next to the top-rightmost native item per POG
  configs <- initializeConfigs()
  # Deletes config
  deleteIds <- c(049008248, # native
                 049006364,
                 049000446,
                 049004825,
                 049006376, # quiet + roar
                 049004033)
  deleteIdsNative <- deleteIds[1 : 4]
  deleteIdsQR <- deleteIds[5 : 6]
  
  for (deleteId in deleteIdsNative) {
    actionPogs <- positionDt[positionId == deleteId, unique(pogId)]
    configs <- updateConfigsDelete(configs = configs,
                                   actionPogs = actionPogs,
                                   deleteItem = deleteId)
  }
  
  
  # Delete UHS 
  uhsIds <- c(4142053, 4142073, 4142052, 4142072, 4142057, 4142037, 4141885, 4141895, 4142063, 4142038, 4142046, 4142066, 4142069, 4142049, 4142045, 4142065)
  for (uhsId in uhsIds) {
    actionPogs <- positionDt[positionId == uhsId, unique(pogId)]
    if(length(actionPogs) > 0) {
      configs <- updateConfigsDelete(configs = configs,
                                     actionPogs = actionPogs,
                                     deleteItem = uhsId)
    }
  }
  
  # Not touching quiet + Roar
  
  # Insert new native items next to the top-rightmost native item per POG
  attDt <- getAttributionDtFromConfig(config)
  nativeIds <- getAttributionDtFromConfig(config) %>% .[brand == "Native", unique(itemId)]
  newIds <- c(049004442, # all native, priority matters (left-to-right)
              049000003,
              049006136,
              049005391,
              049000333,
              049006701) %>% rev
  nativePositionDt <- positionDt[positionId %in% nativeIds] %>%
    .[order(positionY, positionX)]
  insertPositionDt <- nativePositionDt %>% 
    .[, y_max := max(positionY), by = pogId] %>%
    .[positionY == y_max] %>%
    .[, x_max := max(positionX), by = pogId] %>%
    .[positionX == x_max]
  
  existingItemIds <- insertPositionDt[, unique(positionId)]
  for (existingId in existingItemIds) {
    actionPogs <- insertPositionDt[positionId == existingId, unique(pogId)]
    for (newId in newIds) {
      configs <- updateConfigs(configs = configs,
                               existingItemId = existingId, 
                               newItemId = newId, dt = data.table(display = actionPogs),
                               orientation = "RightofPosition")
    }
  }
  writeConfigs(configsOut = configs, config)
  
  
  
  # Build Index
  # Batches: by cluster + footage (for subset of clusters only - not GEN TST) +A/B status
  # "not_in_cks": temporary cluster
  # "unclustered": leftovers
  # There's one A/B/C pog in store 3263 -- treat it separate
  
  abcPogs <- cleanTieDt[store == 3263, unique(display)]
  
  clusterNames <- c("HSP1", "HSP2", "AFAM1", "AFAM2", "PRM", "NATS", "GEN TST")
  for (clusterName in clusterNames) {
    cleanTieDt[grepl(clusterName, title), cluster := clusterName]
  }
  cleanTieDt[display %in% abcPogs, cluster := "A_B_C"]
  cleanTieDt[is.na(cluster), cluster := "unclustered"]
  
  cleanTieDt[, footage_cluster := pmax(16, pmin(20, 4 * ceiling(width / 4)))]
  
  abPogs <- cleanTieDt[, pogs_per_store := uniqueN(display), by = store] %>% .[, .(min_occurs = min(pogs_per_store)), by = .(display)] %>%
    .[min_occurs > 1, unique(display)]
  
  # Isolate A and B pogs as separate clusters
  cleanTieDt[, ab_cluster := ""]
  cleanTieDt[grepl(":B:|B \\(|B\\(", title), ab_cluster := "B"]
  cleanTieDt[grepl(":A:|A \\(|A\\(", title), ab_cluster := "A"]
  

  cleanTieDt[ab_cluster == "", cluster := paste0(cluster, "_", footage_cluster, "FT")]
  cleanTieDt[ab_cluster == "B", cluster := paste0("B_POG")]
  cleanTieDt[ab_cluster == "A", cluster := "A_POG"]
  cleanTieDt[display %in% abcPogs, cluster := "ABC"]
  
  availablePogs <- fread(file.path(config$filesDir, "available_pogs_latest.csv")) %>% .[, unique(pogId)]
  cleanTieDt[!(display %in% availablePogs), cluster := "znot_in_cks"]
  
  indexDt <- buildProjectIndexDt(copy(cleanTieDt)[, pogId := display], allCols = TRUE)
  indexDtConfig <- buildProjectIndexDt(copy(cleanTieDt)[, pogId := display])
  if (FALSE) fwrite(indexDtConfig, file.path(config$configsDir, "indexConfigDt.csv")) 

  
  # uhs
  shelvesDT <- fixtureDt[fixtureType == 0]
  
  ##### OLD SHIT
  
  
  
  
  
  
  # Fresh assessment of FEMALE PW POGs
  uncleanTieDt <- fread(config$rawTieReportPath)
  storePogDt <- cleanTieReport(uncleanTieDt) %>%
    .[, .(storeId = store, pogId = display, title, size = pog_size)] %>%
    .[, c("width", "height", "depth") := tstrsplit(size, "x")] %>%
    .[, c("width", "height", "depth") := lapply(.SD, FUN = as.numeric),
      .SDcols = c("width", "height", "depth")]
  
  pogsWeTurnedInDt <- fread(file.path(config$configsDir, "planogram_table_postauto.csv")) 
  pogsWeTurnedIn <- pogsWeTurnedInDt %>%
    .[, unique(pogId)]
  
  
  indexDt <- storePogDt[, .(store_count = uniqueN(storeId)), keyby = .(pogId, title, size)]
  indexDt[, turned_in := fifelse(pogId %in% pogsWeTurnedIn, 1, 0)]
  
  
  indexDt <- indexDt[, .(can_turn_in = max(turned_in),
                         permutation_store_count = sum(store_count)),
                     .(title, size)] %>%
  merge(indexDt, ., by = c("title", "size"))

  templatesDt <- indexDt[turned_in == 1][ order(title, size, store_count)]
  templatesDt[, N := seq_len(.N), by = .(title, size)]
  templatesDt <- templatesDt[N == 1] %>% .[, N := NULL] %>%
    .[, .(templateId = pogId, title, size)]
  
  problemPogs <- indexDt[can_turn_in ==0]
  
  
  indexDt <- merge(indexDt, templatesDt, by = c("title", "size"), all.x = TRUE)
  
  
  templateCatTies <- merge(storePogDt, indexDt[, .(pogId, templateId)], by = "pogId", all.x = TRUE)
  
  # generate hardAutofill
  hardAutofill <- templateCatTies %>%
    .[!is.na(templateId)] %>% 
    .[!(pogId %in% pogsWeTurnedIn),
      .(templateId, pogId)] %>% unique
  
  hardAutofill[, ProjectId := paste0(as.numeric(as.factor(templateId)), " - ", templateId)]
  hardAutofill <- rbind(hardAutofill[, .(Seq = 1), by = .(PogId = templateId, ProjectId)],
                        hardAutofill[, .(Seq = seq_len(.N) + 1, PogId = pogId), by = ProjectId])
  fwrite(hardAutofill, file.path(config$configsDir, "hardAutoFill.csv"))
  
  
  write_clip(templateCatTies[!is.na(templateId), .(DISPLAY = templateId, STORE = storeId)])
    
}



if (FALSE) {
  setwd("~/r_code")
  source("00_initialize.R")
  
  storePogDt2 <- fread(config$storePogDtPath)
  
  indexDt <- fread(file.path(config$filesDir, "index_only_yellow.csv")) %>%
    .[, c("width", "height", "depth") := tstrsplit(size, "x")] %>%
    .[, c("width", "height", "depth") := lapply(.SD, FUN = as.numeric),
      .SDcols = c("width", "height", "depth")]
  indexDt[grepl("[0-9][0-9]A", title), is_ab := "A"]
  indexDt[grepl("[0-9][0-9]B", title) & !grepl("BD", title), is_ab := "B"] # account for "18BD"s
  indexDt[grepl("[0-9][0-9]C", title), is_ab := "C"]
  indexDt[grepl("[0-9][0-9]D", title), is_ab := "D"]
  indexDt[is.na(is_ab), is_ab := ""]
  indexDt[display == "J0496QY", width := 12]
  clusterNames <- c("AFAM1", "AFAM2", "NATS", "HSP1", "HSP2")
  lapply(clusterNames, FUN = function(x) indexDt[grepl(x, title), cluster := x])
  indexDt[is_ab == "" & width <= 12, cluster := "12FT_AND_UNDER"]
  indexDt[is.na(cluster), cluster := "PRM"]
  
  indexDt[, width_to_match_on := 20]
  indexDt[width < 17, width_to_match_on := 16]
  indexDt[width < 13, width_to_match_on := 12]
  
  indexDt[is_ab != "", width_to_match_on := 999]
  
  templateDt <- fread(file.path(config$configsDir, "planogram_table_templates.csv"))
  templateFixtureDt <- fread(file.path(config$configsDir, "fixture_table_templates.csv"))
  templateItemDt <- fread(file.path(config$configsDir, "position_table_templates.csv"))
  
  templateWidths <- templateFixtureDt[, .(width_to_match_on = max(fixtureX + fixtureWidth) / 12), by = pogId]
  templateDt[grepl("-A", pogTitle), is_ab := "A"]
  templateDt[grepl("-B", pogTitle), is_ab := "B"]
  templateDt[is.na(is_ab), is_ab := ""]
  # matches templates to pogs
  lapply(c("PRM", clusterNames), FUN = function(x) templateDt[grepl(x, pogTitle), cluster := x])
  
  # fix AFA1/AFAM1 and AFA2/AFAM2 switch ups 
  templateDt[grepl("AFA1", pogTitle), cluster := "AFAM1"]
  templateDt[grepl("AFA2", pogTitle), cluster := "AFAM2"]
  
  templateDt[is.na(cluster), cluster := "12FT_AND_UNDER"]
  
  templateDt <- merge(templateDt, templateWidths, by = "pogId")
  templateDt[is_ab != "", width_to_match_on := 999]
  

  
  
  indexDtWithTemplate <- merge(indexDt, templateDt, by = c("is_ab", "cluster", "width_to_match_on"), all.x = TRUE)
  
  
  
  templateKey <- indexDtWithTemplate[, .(title, size, templateId = pogId)] %>% unique
  
  indexDtWithWhite <- fread(file.path(config$filesDir, "index_white_and_yellow.csv"))
  
  indexDtWhiteWithTemplates <- merge(indexDtWithWhite, templateKey, by = c("title", "size"), all.x = TRUE)
  # some manuals
  indexDtWhiteWithTemplates[display %in% c("J0496QV", "J0496SI"), templateId := "T2"]
  
  pogItemDt <- merge(indexDtWhiteWithTemplates, templateItemDt[, .(templateId = pogId,
                                                                      positionId)] %>% unique,
                     by = c("templateId"),
                     allow.cartesian = TRUE)
  
  storePogWholeCat <- fread(file.path(config$filesDir, "d49_tie_report_10_18_2022.csv")) %>%
    .[planogram_id %in% (pogItemDt$display %>% unique)] %>%
    .[, .(display = planogram_id, store = location_id)]
  podDt <- merge(pogItemDt, storePogWholeCat, by = "display", allow.cartesian = TRUE) %>%
    .[, .(pod_count = uniqueN(store)), by = positionId]
  
  templateStoreCounts <- merge(pogItemDt, storePogWholeCat, by = "display", allow.cartesian = TRUE) %>%
    .[, .(store_count = uniqueN(store)), keyby = templateId] %>%
    merge(templateDt[, .(templateId = pogId, fileId)], by = "templateId")
  
  # diversion2: Create gapp adds
  commentsDt <- fread(file.path(config$filesDir, "pods_with_comments.csv"))
  commentsDt[grepl("Add", Comments), flag := "add"]
  commentsDt[grepl("Remove", Comments), flag := "remove"]
  
  allItemCombnsWithStoreCounts <- templateItemDt[, CJ(templateId = unique(pogId), positionId =  unique(positionId))] %>%
    merge(templateStoreCounts[, .(templateId, store_count)], by = "templateId")
  
  gapDt <- commentsDt[flag %in% c("add", "remove"),
                      .(positionId, final_pod = proposed_store_count,
                        current_pod = pod_count,
                        gap = difference)] %>% unique %>%
    # This is just a check that there's no duplicate rows
    # since commentsDt is a manually-entered table
    .[, N := .N, by = positionId] %>%
    .[N == 1] %>%
    .[, N := NULL]
  # manually update this one item becasue the difference is different in the omment than the column
  gapDt[positionId == 49003696, ':='(final_pod = 1300, gap = 425)]
  
  # Create a flag
  templateGapItemDt <- templateItemDt[, .(templateId = pogId, positionId)] %>% unique %>% 
    .[, item_on_pog_flag := 1] %>%
    merge(allItemCombnsWithStoreCounts, by = c("positionId", "templateId"), all.y = TRUE, allow.cartesian = TRUE) %>%
    merge(gapDt, by = "positionId") %>%
    merge(templateDt[, .(templateId = pogId, pogTitle, is_ab, cluster)], by = "templateId")
  
  
  removersDt <- templateGapItemDt[item_on_pog_flag == 1] %>%
    merge(commentsDt[flag == "remove", .(positionId, brand, item_description, Comments)], by = "positionId") %>%
    .[, .(positionId, brand, item_description, type = "item_decrease", pod_to_hit = final_pod, current_pod, gap,
          pod_opportunity = store_count,
          template_name = pogTitle, template_ab_status = is_ab, cluster, Comments)]
  
  addsDt <- templateGapItemDt[is.na(item_on_pog_flag)] %>%
    merge(commentsDt[flag == "add", .(positionId, brand, item_description, Comments)], by = "positionId") %>%
    .[, .(positionId, brand, item_description, type = "item_increase", pod_to_hit = final_pod, current_pod, gap,
          pods_opportunity = store_count,
          template_name = pogTitle, template_ab_status = is_ab, cluster, Comments)]
  
  
  workingToTemplateDt <- indexDtWithTemplate[!is.na(pogId), .(templateId = pogId,
                                                              workingId  = display)]
  
  # match by merge on cluster, is_ab, and width_to_match_on
  checkForStrongMatch <- function(workingPogId, 
                                  templateId,
                                  workingFixtureDt,
                                  templateFixtureDt) {
    # PogId | ShelfName | ItemId | LocationId (1 leftmost, etc.)
    # strong match flag==
    # 0: The shelves won't match at all, don't try to autofill
    # 1: The shelves aren't the same widths but there's the same number of them so fill
    # 2: The shelves match up  perfect, so fill
    # 3. The workingPog has at least as many shelves as the template and they're on the same vertical plane, so fill
    workingShelfDt <- workingFixtureDt[pogId == workingPogId] %>%
      .[fixtureType == 0 & fixtureColor == -1] %>%
      .[order(fixtureX, fixtureY)] %>%
      .[, working_shelf_flag := 1]
    workingShelfDt[, xrank := as.numeric(factor(fixtureX), levels = sort(fixtureX))]
    workingShelfDt[, yrank := as.numeric(factor(fixtureY), levels = sort(fixtureY)), by = xrank]
    workingShelfDt[, fixtureId := paste0("S_", xrank, "_", yrank)]
    
    templateShelfDt <- templateFixtureDt[pogId == templateId] %>%
      .[fixtureType == 0 & fixtureColor == -1] %>%
      .[order(fixtureX, fixtureY)]
    templateShelfDt[, xrank := as.numeric(factor(fixtureX), levels = sort(fixtureX))]
    templateShelfDt[, yrank := as.numeric(factor(fixtureY), levels = sort(fixtureY)), by = xrank]
    templateShelfDt[, fixtureId := paste0("S_", xrank, "_", yrank)]
    
    if (nrow(workingShelfDt) == nrow(templateShelfDt)) {
      mergedShelfDt <- merge(templateShelfDt,
                             workingShelfDt,
                             by = c("fixtureId",
                                    "fixtureWidth",
                                    "fixtureX",
                                    "fixtureY"))
      if (nrow(mergedShelfDt) == nrow(workingShelfDt)) {
        return(2)
      } else {
        return(1)
      }
    }
    else {
      # 
      mergedShelfDt <- merge(templateShelfDt,
                             workingShelfDt,
                             by = c("fixtureId"))
      if (NROW(mergedShelfDt) == NROW(templateShelfDt))
      {
        return(3)
      } else{
        return(0)
      }
    }
  }
  
  workingPogDt <- fread(file.path(config$configsDir, "planogram_table_preautomation.csv"))
  workingPogItemDt <- fread(file.path(config$configsDir, "position_table_preautomation.csv"))
  workingFixtureDt <- fread(file.path(config$configsDir, "fixture_table_preautomation.csv"))
  
  workingToTemplate <- indexDtWithTemplate[(!is.na(pogId) * !is.na(display)),
                                           .(templateId = pogId,
                                             workingId = display)]
  
  shelfMapDt <- createShelfMap(workingFixtureDt = workingFixtureDt,
                               templateFixtureDt = templateFixtureDt,
                               workingToTemplateDt = workingToTemplate)
  
  buildIndexOffShelfMap <- function(indexDt, shelfMapDt) {
    matchTypeDt <- shelfMapDt[, .(WorkingId, MatchFlag)] %>% unique
    
    indexConfig <- merge(indexDt[!is.na(pogId)],
                         matchTypeDt, 
                         by.x = c("display"),
                         by.y = c("WorkingId"))
    
    nonTemplatePogs <- indexConfig
    clusterBatchKey <- nonTemplatePogs[, .N, by = .(pogId, MatchFlag)]
    clusterBatchKey[, ProjectId := paste0(formatC(seq_len(.N), 
                                                  width = 2, 
                                                  flag = "0"), " - ",
                                          pogId, "_",
                                          MatchFlag, "_", N)]
    
    indexConfigMerged <- merge(nonTemplatePogs, clusterBatchKey,
                               by = c("pogId", "MatchFlag"))
    templatePogsMerged <- clusterBatchKey[, .(PogId = pogId, 
                                              ProjectId,
                                              MatchFlag,
                                              N)] %>% unique
    
    indexConfigOut <- rbind(
      templatePogsMerged[, .(PogId, ProjectId, Seq = 1)],
      indexConfigMerged[, .(PogId = display, ProjectId)] %>%
        .[, Seq := seq_len(.N), by = ProjectId] %>%
        .[, Seq := Seq + 1])
    
    return(indexConfigOut)
  }
  indexConfigOut <- buildIndexOffShelfMap(indexDtWithTemplate, shelfMapDt)
  

  
  
  


  fwrite(indexConfigOut[grepl("_2_", ProjectId)], 
         file.path(config$configsDir, "autofillConfig_type2.csv"))
  fwrite(shelfMapDt, 
         file.path(config$configsDir, "shelfMapConfig.csv"))
  
  nrbMapDt <- createNrbMap(workingFixtureDt = workingFixtureDt,
                           templateFixtureDt = templateFixtureDt,
                           templatePositionDt = templateItemDt,
                           workingToTemplateDt = workingToTemplateDt)
  nrbConfig <- buildIndexOffShelfMap(indexDtWithTemplate, nrbMapDt)
  fwrite(nrbConfig,
         file.path(config$configsDir, "autofillConfig_nrb.csv"))
  fwrite(nrbMapDt,
         file.path(config$configsDir, "shelfMapNrbConfig.csv"))
  
  
  # UHS Stuff
  
  uhsPogs <- indexDt[grepl("UHS", indexDt$title)]
  # forget these two pogs since they aren't in cks anymore
  uhsPogs <- uhsPogs[!(display %in% c("J0496VH", "J04970J"))]
  
  
  storesGettingUhs <- storePogDt[pogId %in% uhsPogs$display] %>%
    merge(fread(file.path(config$filesDir, "storePartMapDt.csv")), all.x = TRUE)
  
  uhsPogs <- merge(uhsPogs, storesGettingUhs[, .(pogId, supplier)] %>% unique, by.x = "display",
                   by.y = "pogId") %>% 
    .[, .(pogId = display, supplier, height)] %>% unique
  
  uhsFixtureDt <- fread(file.path(config$configsDir, "fixture_table_uhs.csv"))
  
  workingShelfDt <- uhsFixtureDt %>%
    .[, depth := max(fixtureDepth), by = pogId] %>% 
    .[fixtureType == 0 & fixtureColor == 33023 & fixtureWidth > 18] %>%
    .[, type := fifelse(fixtureX == 0, "starter", "add_on")]
  
  uhsNrbFixtures <- workingShelfDt %>%
    merge(uhsPogs, by = "pogId") %>% 
    .[, .(pogId, 
          fixtureId,
          supplier, 
          type,
          width = fixtureWidth,
          depth,
          height)] %>% unique
  
  # manual fix 1: need to make J0496R1 have 36' width NRBs
  uhsNrbFixtures[width == 35, width := 36]
  # manual fix 2: 72/74 mismatch
  uhsNrbFixtures[height == 72, height  := 74]
  
  
  
  uhsItemTable <- fread(file.path(config$filesDir, "uhsItemTable.csv")) %>%
    .[, supplier := fifelse(grepl("ARTITALIA", supplier), "Triad", "Envision")] %>%
    .[, type := fifelse(grepl("starter", description), "starter", "add_on")] %>%
    .[, positionId := as.numeric(gsub("-", "", positionId))]
  
  
  uhsNrbData <- merge(uhsNrbFixtures, uhsItemTable, by = c("supplier", "type", "width", "depth", "height"),
                      all.x = FALSE)
  
  addToFixtureConfig <- uhsNrbData[, .(PogId = pogId,
                                       CutInItemId = positionId,
                                       FixtureId = fixtureId)] %>% unique
  fwrite(addToFixtureConfig, file.path(config$configsDir, "nrbInsertConfig.csv"))
  anchorItem <- 4467629
  
  
  uhsDepthTable <- fread(file.path(config$filesDir, "uhsDepthTable.csv"))
  uhsFixtureDtPostAuto <- fread(file.path(config$configsDir, "fixture_table_uhs_postauto.csv"))
  currentDepthTable <- copy(uhsFixtureDtPostAuto) %>%
    .[fixtureType == 0 & fixtureColor == -1] %>%
    .[fixtureWidth > 30] %>%
    .[, basedeck_depth := max(fixtureDepth[fixtureY == 6.18], na.rm = TRUE), by = pogId] %>%
    .[, shelf_depth := fixtureDepth] %>%
    .[fixtureY > 6.18, .(pogId, fixtureId, shelf_depth, basedeck_depth, fixtureWidth)]
    
    shelfUpdateConfigUhs <- merge(currentDepthTable,
                                  uhsDepthTable, 
                                  by = c("shelf_depth", "basedeck_depth", "fixtureWidth"),
                                  all.x = TRUE)
      .[shelf_depth != new_shelf_depth,
        .(PogId = pogId,
          FixtureId = fixtureId,
          FixtureDepth = new_shelf_depth,
          FixtureDesc1 = fixtureDesc1,
          FixtureDesc19)]
    fwrite(shelfUpdateConfigUhs, file.path(config$configsDir, "shelfUpdateConfigUhs.csv"))
  

  
}

if (FALSE) {
  # PW - Female config stuff
  orangeTemplateShelves <- templateFixtureDt[fixtureType == 0 & fixtureColor == 33023]
  orangeWorkingShelves <- workingFixtureDt[fixtureType == 0 & fixtureColor == 33023]
  
  perfLibPath <- "C:/Users/William.Roberts/OneDrive - Unilever/Documents/pw_march_23_transition/3.5.23 PW Perf Lib_clean_10_28_2022.mdb"
  ch <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/William.Roberts/OneDrive - Unilever/Documents/pw_march_23_transition/3.5.23 PW Perf Lib_clean_10_28_2022.mdb")
  perfDt <- as.data.table(sqlFetch(ch, "Results0", rownames = TRUE)) %>%
    cleanNames()
  
  # Recipe:
  #  004467629 on all shelves
  # 049940307 and 049940341 where leftmost fixture is
}

if (FALSE) {
  aveenoItems <- c(49000173, 49000425, 49001246, 49007446, 49003439)
  # PW - add 049004050 to the left of 049000173
  # - Delete 049001246
  
  aveenoDt <- fread(file.path(config$configsDir, "position_table_preautomation_repull.csv")) %>%
    .[positionId %in% aveenoItems] %>%
    merge(perfDt, .,
          by.x = c("planogram_alias", "id"), by.y = c("pogId", "positionId"))
  
  
  pogsInPlay <- aveenoDt[id == 49000173, planogram_alias]
  
  aveenoDt <- aveenoDt[planogram_alias %in% pogsInPlay]
  
  candidatePlane <- aveenoDt[id == 49001246, .(planogram_alias, positionY)] %>% unique
  
  candidateFaceOuts <- copy(aveenoDt[id != 49001246]) %>%
    merge(candidatePlane, by = c("planogram_alias", "positionY")) %>%
    .[, item_rank := order(value_39 / positionHFacings), by = planogram_alias] %>%
    .[item_rank == 1]
  
  # Cut in new item
  configsForSwap <- initializeConfigs()
  configsForSwap <- updateConfigs(configs = configsForSwap,
                                  existingItemId = 49000173,
                                  newItemId = 49004050,
                                  dt = data.table(display = pogsInPlay))
  
  # Delete 49001246
  configsForSwap <- updateConfigsDelete(configs = configsForSwap,
                                        actionPogs = candidatePlane$planogram_alias,
                                        deleteItem = 49001246)
  
  # Face out best candidate for every delete
  for (faceOutId in candidateFaceOuts[, unique(id)]) {
    print(faceOutId)
    subsetDt <- candidateFaceOuts[id == faceOutId]
    configsForSwap <- updateConfigsFacings(configs = configsForSwap,
                                           existingItemId = subsetDt[1, id],
                                           dt = data.table(display = subsetDt$planogram_alias,
                                                           position_hfacings = subsetDt$positionHFacings,
                                                           new_hfacings = subsetDt$positionHFacings + 1))
  }
  
  writeConfigs(configsForSwap, config, FALSE)
  
  repullIndex <- configsForSwap$facesConfig_repull[, .(PogId = PogId,
                                                       ProjectId = paste0("faced_", ItemId))]
  repullIndex[, Seq := seq_len(.N), by = ProjectId]
  fwrite(repullIndex, file.path(config$configsDir, "repullIndex.csv"))
}

# PW - Index Config stuff
if (FALSE) {
  
  # nchar > 5 to get rid of the template pogs
  oddballPogs <- indexConfigOut[grepl("_0_", ProjectId)][nchar(PogId) > 5, PogId]
  
  # diversion1: build indexConfig for the master doc
  indexDtForMasterDoc <- copy(indexDtWithTemplate)
  indexDtForMasterDoc[, ab_status := is_ab == ""]
  indexDtForMasterDoc[, fake_width := paste0(width_to_match_on, "FT")]
  
  indexDtForMasterDoc[, batch_type := cluster]
  indexDtForMasterDoc[width == 12 & ab_status == TRUE, batch_type := "AT 12FT"]
  indexDtForMasterDoc[width < 12 & ab_status == TRUE, batch_type := "UNDER 12FT"]
  indexDtForMasterDoc[cluster == "PRM" & ab_status == TRUE, batch_type := paste0(cluster, " ", fake_width)]
  indexDtForMasterDoc[ab_status == FALSE, batch_type := paste0(cluster, " AB POGs")]
  indexDtForMasterDoc[display %in% oddballPogs, batch_type := "Unautofillable"]
  indexDtForMasterDoc[display %in% c("J0496VH", "J04970J"), batch_type := "Weird"]
  
  indexSkeletonKey <- indexDtForMasterDoc %>%
    .[, .(batch_type, ab_status)] %>%
    unique %>% 
    .[order(-ab_status, batch_type)]

  
  
  indexSkeletonKey %>%
    .[, index := seq_len(.N)] %>%
    .[, BatchId := paste0(index, " - " , batch_type)]
  
  indexDtForMasterDoc <- merge(indexDtForMasterDoc, indexSkeletonKey, by = c("batch_type", "ab_status"), all.x = TRUE)
  
  
  
  indexDtOut <- indexDtForMasterDoc[, .(display, checked_out_by = "", stores_tied, sorting_order = 0, 
                                        total_sort = "", BatchId, cluster, total_width = "", width, 
                                        height, basedeck = "", size, title, mpd = "", is_ab, index)] %>%
    .[order(index, as.numeric(width), nchar(title), title)] 
  indexDtOut[, sorting_order := seq_len(.N), by = index] %>% .[, total_sort := seq_len(.N)]
  indexDtOut[order(index, nchar(title), title, is_ab)]
  
  indexDtOutAbs <- indexDtOut[is_ab != ""]
  indexDtOutAbs[order(index, nchar(title), title), sorting_order := seq_len(.N), by = index]
  
  indexDtOut[is_ab != ""] <- indexDtOutAbs
  indexDtOut[order(index, sorting_order), total_sort := seq_len(.N)]
  indexDtOut[order(total_sort)] %>% write_clip
  
  
  batchesConfig <- indexDtOut[, .(PogId = display, ProjectId = BatchId, Seq = sorting_order)]
  fwrite(batchesConfig, file.path(config$configsDir, "finalBatchesConfig.csv"))
}

# TNT CHECKS
if (FALSE) {
  positionDt <- fread(config$configsDir %>% file.path("position_table_for_upload.csv"))
  fixtureDt <- fread(config$configsDir %>% file.path("fixture_table_for_upload.csv"))
  attributionDt <- fread(config$attributionDtPath)
  
  # check that liquid IV is always duplicate
  liquidIv <- c(94080305, 94088064)
  lvDt <- positionDt[positionId %in% liquidIv]
  # make sure lvDt flag is fine 
  lvDt[, .(flag1 = uniqueN(paste0(pogId, fixtureId, positionX))), by = positionId]
  
  # check for no overlaps
  positionDt[, positions_with_coord := uniqueN(positionId), by = .(pogId, positionX, positionY)]
  positionDt[positions_with_coord > 1, .N, by = positionId] %>%
    .[order(N)] %>% 
    merge(attributionDt, all.x = T) %>% .[order(N)]
    
  areGood <- c(94080305, 94088064, 37130092, 37137093, 94020023, 94022569)
  gotFixed <- c(49040069)
  overlapPogs <- positionDt[positions_with_coord > 1][!(positionId %in% c(areGood, gotFixed))] %>%
    .[order(pogId), .N, by = pogId] %>%
    .[N > 1, pogId]
  
  # we want only 4 skus ( 2 olly + 2 liquid iv) to have positions_with_coord > 1
  # we want them to never have positions_with_coord == 1
  splitOllyPogs <- positionDt[positionId %in% areGood] %>%
    .[, .(n_items_at_position = uniqueN(positionId)), 
      by = .(pogId, positionX, positionY)] %>%
    .[n_items_at_position != 2, unique(pogId)]
  
  
  # check for no duplicates
  positionDt[, position_occurrence := uniqueN(paste0(positionX, "_", positionY)), by = .(pogId, positionId)]
  positionDt[, .N, by = position_occurrence]
  
  # check space
  overSpacedPogs <- fixtureDt[fixtureAvailableLinear < 0, unique(pogId)]
  fixtureDt[fixtureAvailableLinear > 4]
  
  # Make the item  swap POG list 
  
  # 
  indexConfig <- fread(file.path(config$configsDir, "indexConfig.csv"))
  indexConfig2 <- copy(indexConfig)
  indexConfig2[, ProjectId := "TNT_"]
  indexConfig2[PogId %in% overlapPogs, ProjectId := paste0(ProjectId, "overlaps_")]
  indexConfig2[PogId %in% splitOllyPogs, ProjectId := paste0(ProjectId, "splitsollyoriv_")]
  indexConfig2[PogId %in% overSpacedPogs, ProjectId := paste0(ProjectId, "overspaced_")]
  indexConfig2[, Seq := seq_len(.N), by = ProjectId]
  indexConfig2[, ProjectId := paste0(ProjectId, floor(Seq / 100))]
  indexConfig2[, Seq := seq_len(.N), by = ProjectId]
  fwrite(indexConfig2, file.path(config$configsDir, "indexConfig2.csv"))
  
  
  indexConfig3 <- copy(indexConfig)
  indexConfig3[, ProjectId := "Normal"]
  indexConfig3[PogId %in% ok, ProjectId := "Needs Updating"]
  fwrite(indexConfig3[PogId %in% ok], file.path(config$configsDir, "indexConfig3.csv"))
  
}

# add/insert/deletes for template automation
if (FALSE) {
  configs <- initializeConfigs()
  
  addsWithXs <- fread(file.path(config$filesDir, "addsWithXs.csv")) %>%
    merge(templateDt[, .(pogId, template_name = pogTitle)], by = "template_name")
  
  anchorItem <- 4467629
  addsToInsert <- addsWithXs[X == "x"]
  for (givenItem in addsToInsert[, unique(positionId)]) {
    actionPogs <- addsToInsert[positionId == givenItem, pogId]
    configs <- updateConfigs(configs = configs, 
                             existingItemId = anchorItem, 
                             newItemId = givenItem,
                             dt = data.table(display = actionPogs))
  }
  
  removalsWithXs <- fread(file.path(config$filesDir, "removalsWithXs.csv")) %>%
    merge(templateDt[, .(pogId, template_name = pogTitle)], by = "template_name")
  removalsToColor <- removalsWithXs[X == "x"]
  for (givenItem in removalsToColor[positionId != 49008610, unique(positionId)]) {
    actionPogs <- removalsToColor[positionId == givenItem, pogId]
    configs <- updateConfigsDelete(configs = configs, actionPogs = actionPogs, deleteItem = givenItem)
  }
  writeConfigs(configsOut = configs, config)
}

if (FALSE) {
  
  templateItemDt[positionId == 4467629, .(min_x = min(positionX), min_y = max(positionY)), by = pogId] %>% 
    merge(templateItemDt[positionId == 4467629], ., by.x = c("pogId", "positionX", "positionY"), by.y = c("pogId", "min_x", "min_y"))
}


ok <- read_clip() %>%
  .[grepl("J049", .)] %>%
  gsub(" - This planogram already exists in database. Check out existing version in order to make changes. If you are trying to upload Template planogram, Please update Lifecycle \\(Status1 of Planogram\\) to Template", "", .) %>%
  gsub("ErrorDescription: ", "", .)
  
okFinal <- c(okFinal, ok)
  