setwd("~/r_code")
source("00_initialize.R")


# read in the tie report + clean
setwd(config$relayDir)

# Save tie report
uncleanTieReport <- fread(config$rawTieReportPath)
tieReportDt <- cleanTieReport(rawTieReport = uncleanTieReport)
tieReportOut <- cleanTieReport(rawTieReport = uncleanTieReport, assignUncleanNames = TRUE)

nonStores <- leftOnly(getStoreListDt()$store, tieReportDt$store)
if (FALSE) {
  fwrite(tieReportOut, config$cleanTieReportPath)
}

tieReportDt <- fread(config$cleanTieReportPath) %>% cleanNames
# Save index report
indexReportDt <- createIndexReport(tieReportDt, clusterNames = config$clusterNames)
if (FALSE) {
  fwrite(indexReportDt, config$cleanIndexReportPath)
}
# Pog Data
pogData <- fread(file.path(config$relayDir, config$pogDataPath)) %>% cleanNames %>%
  setnames(old = "planogram_desc2", new = "display") %>%
  .[, product_brand := NULL] %>% 
  .[, filename := NULL] %>%
  unique

# displays that aren't in pogData aren't in cks
notInCks <- leftOnly(tieReportDt[, unique(display)], pogData[, unique(display)])
# displays in pog data not in store tie report we can ignore

# filter to display universe of displays in both files
displayUniverse <- intersect(pogData[, unique(display)], tieReportDt[, unique(display)])


if (FALSE) {
  # HBL section labels
  labelCoordsDt <- pogData[fixture_type == 0, .(fixture_xend = round(max(position_x) / 12) * 12),
                           keyby = .(display, fixture_x)] %>%
    .[order(display, fixture_x)] %>%
    .[, key := seq_len(.N), by = .(display)] %>%
    .[, label := paste0("Section #", key)] %>%
    .[, .(PogId = display, FixtureX = fixture_x, 
          LabelWidth = fixture_xend - fixture_x,
          LabelText = label)]
  fwrite(labelCoordsDt, file.path(config$configsDir, "labelConfig.csv"))
  setkey(labelCoordsDt, display, key)
  labelCoordsDt[, fixture_xlag := lag(fixture_x, 1), by = display]
  
}

shelfAssigner <- function(indexDt) {
  idt <- copy(indexDt)[, .(display, title, height)]
  idt[, gnd_flag := fifelse(grepl("GND", title), 1, 0)]
  idt[, blw_flag := fifelse(grepl("BLW", title), 1, 0)]
  idt[grepl("BLW2", title), blw_flag := 2]
  idt[, exp_flag := fifelse(grepl("EXP", title), 1, 0)]
  idt[, bd_flag := fifelse(grepl("BD22", title), 1, 0)]
  
  idt[, pog_type := "no_type"]
  # GND_BLW2: 18BD and 18 shelf
  idt[gnd_flag == 1 & blw_flag == 2 & exp_flag == 0 & bd_flag == 0, pog_type := "GND_BLW2"]
  # EXP_BD18: 18"BD and 18"Shelf
  idt[exp_flag == 1 & bd_flag == 0, pog_type := "EXP_BD18"]
  # EXP_BD22: 22"BD and 22"Shelf
  idt[exp_flag == 1 & bd_flag == 1, pog_type := "EXP_BD22"]
  # NONGND_BLW2: 16BD and 14 shelf
  idt[gnd_flag == 0 & blw_flag == 2 & exp_flag == 0, pog_type := "NONGND_BLW2"]
  # BLW1: 18BD and 18 shelf
  idt[blw_flag == 1 & exp_flag == 0, pog_type := "BLW1"]
  # NONBLW: 18BD and 18 shelf
  idt[blw_flag == 0 & exp_flag == 0, pog_type := "NONBLW"]
  
  idt[pog_type %in% c("NONBLW", "BLW1", "EXP_BD18", "GND_BLW2"), pog_type := "BD18_SHELF18"]
  
  pogsWithThreeFoots <- pogData[fixture_width == 36, unique(display)]
  
  idt[, threefoot_flag := fifelse(display %in% pogsWithThreeFoots, 1, 0)]
  
  # Header flags
  idt[, header_flag := fifelse(threefoot_flag == 1 & height == 84 & blw_flag == 0, 1, 0)]
  idt[threefoot_flag == 1 & height == 84 & blw_flag == 2, header_flag := 2]
  
  shelfDt <- idt[, .(PogId = display, PogType = pog_type, ThreeFootFlag = threefoot_flag)]
  shelfDt[, skeleton_key := seq_len(.N), by = .(PogType, ThreeFootFlag)] %>%
    .[, skeleton_key := floor(skeleton_key / 75) + 1]
  shelfDtSkeleton <- shelfDt[, .(skeleton_key, PogType, ThreeFootFlag)] %>% unique %>%
    .[, Seq := seq_len(.N)]
  shelfDt <- merge(shelfDt, shelfDtSkeleton, by = c("ThreeFootFlag", "PogType", "skeleton_key"))
  shelfDt[, skeleton_key := NULL]
  shelfDt <- shelfDt[, .(PogId, PogType, ThreeFootFlag, Seq)]
  
  shelfDt <- merge(shelfDt, idt[, .(PogId = display, HeaderFlag = header_flag)], by = "PogId")
  if (FALSE) {
    fwrite(shelfDt,
           file.path(config$configsDir, "shelfConfig.csv"))
  }
}
if (FALSE) {
  # DO THIS AHEAD OF TIME:
  # 1. 4467629 is our anchor item, add it into any pogs its not in
  # Look out for J0370T9 add this item and also 4534190 facing where needed
  # pogData[id == 004467629, .(N = min(fixture_x)), by = display][N != 0]
  # 2. Need to do a certain script to get 004436445 on all non-BLW 84" pogs
  # 3. Need to do a certain script to get 004560050 on 3' sections in 84" BLW2 POGs
  
  anchorItem <- 4467629
  
  # HBL fixturing
  indexDt <- fread("index_dt.csv") %>% cleanNames
  # add types to indexDt
  indexDt[, pog_type := "esoteric"]
  indexDt[(height == 84) & !grepl(pattern = "BLW", x = title), pog_type := "84_non_blw"]
  indexDt[grepl(pattern = "BLW2", x = title), is_blw2 := 1]
  indexDt[grepl(pattern = "GND", x = title), is_gnd := 1]
  indexDt[is.na(is_gnd) & is_blw2 == 1 & height == 84, pog_type := "84_blw2_non_gnd"]
  indexDt[is_gnd == 1 & is_blw2 == 1 & height == 84, pog_type := "84_blw2_gnd"]
  
  
  configs <- initializeConfigs()
  
  # 2. Count info boxes, give 004558894 facing for each
  infoBoxIds <- c(004616318, 004616316, 004616314, 004616312,
                  004616308, 004616298, 004616280)
  infoBoxPogFaces <- pogData[, .(infobox_n = sum(id %in% infoBoxIds),
                                 infobox_item_facings = sum(position_hfacings[id == 4558894])),
                             by = display]
  indexDt <- merge(indexDt, infoBoxPogFaces, by = "display", all.x = TRUE)
  # There are 4 different types of POGs (if infobox_n == item_facings, no action needed). Other 3:
  # I. Pogs to delete from (infobox_n == 0 but item_facings doesn't)
  pogsToDeleteFrom <- indexDt[infobox_n == 0 & infobox_item_facings > 0, display]
  configs <- updateConfigsDelete(configs = configs, actionPogs = pogsToDeleteFrom, deleteItem = 4558894)
  # II. Pogs to insert on 
  pogsToInsertOn <- indexDt[infobox_n > 0 & infobox_item_facings == 0, display]
  configs <- updateConfigs(configs = configs, existingItemId = anchorItem,
                           newItemId = 4558894, dt = data.table(display = pogsToInsertOn))
  # III. Pogs to update facings on
  pogsToUpdateFacings <- indexDt[infobox_n != infobox_item_facings
                                 ][infobox_n > 0, .(display, new_hfacings = infobox_n, 
                                                    position_hfacings = pmax(1, infobox_item_facings))]
  configs <- updateConfigsFacings(configs = configs, dt = pogsToUpdateFacings, existingItemId = 4558894)
  
  # 3 count derm sections per pog; add both these items per section
  dermHeaderId <- 4585013
  item1 <- 4585014
  item2 <- 4348167
  dermFaces <- pogData[, .(derm_n = sum(id %in% dermHeaderId),
                           item1_facings = sum(position_hfacings[id == item1]),
                           item2_facings = sum(position_hfacings[id == item2])),
                       by = display]
  # we need to handle 2 types of pogs, because if facings equal (do nothing), 
  #   and also item1 and 2 are everywhere they should be...
  # I. Pogs to delete the items from
  pogsToDeleteFrom <- dermFaces[derm_n == 0][item1_facings > 0 | item2_facings > 0, display]
  configs <- updateConfigsDelete(configs = configs, actionPogs = pogsToDeleteFrom, deleteItem = item1)
  configs <- updateConfigsDelete(configs = configs, actionPogs = pogsToDeleteFrom, deleteItem = item2)
  # II. Pogs to update facings to
  pogsToUpdateFacings <- dermFaces[derm_n > 0
                                   ][derm_n != item1_facings, .(display, new_hfacings = derm_n,
                                                                position_hfacings = item1_facings)]
  configs <- updateConfigsFacings(configs = configs, dt = pogsToUpdateFacings,
                                  existingItemId = item1)
  configs <- updateConfigsFacings(configs = configs, dt = pogsToUpdateFacings,
                                  existingItemId = item2)
  
  
  # 4. derm shelf stripsone 004419205 on NRB per occurrence of 004558906
  stripFaces <- pogData[, .(shelf_strip_n = sum(position_hfacings[id == 4558906]),
                            strip_holder_n = sum(position_hfacings[id == 4419205])), by = display]
  
  # I. Delete if no strips
  pogsToDeleteFrom <- stripFaces[shelf_strip_n == 0][strip_holder_n > 0, display]
  configs <- updateConfigsDelete(configs = configs, actionPogs = pogsToDeleteFrom, deleteItem = 4419205)
  # II. Update facings if differ
  pogsToUpdateFacings <- stripFaces[shelf_strip_n > 0 & strip_holder_n > 0
                                    ][strip_holder_n != shelf_strip_n, .(display, new_hfacings = shelf_strip_n,
                                                                         position_hfacings = strip_holder_n)]
  configs <- updateConfigsFacings(configs = configs, dt = pogsToUpdateFacings, existingItemId = 4419205)
  
  # III. Insert if need faces but no holders
  pogsToInsert <- stripFaces[shelf_strip_n > 0 & strip_holder_n == 0]
  configs <- updateConfigs(configs = configs, existingItemId = anchorItem, newItemId = 4419205, 
                           dt = pogsToInsert[, .(display)])
  curSeq <- getSeq(configs)
  pogsToInsert[, Seq := as.numeric(as.factor(shelf_strip_n)) + curSeq - 1]
  configs$facesConfig <- rbind(configs$facesConfig, 
                               pogsToInsert[, .(PogId = display, 
                                                ItemId = 4419205, 
                                                FaceValue = shelf_strip_n,
                                                seq = Seq)])
  # 5. nothing to do
  
  # 6. Needs special function, add 4436445 3' header to everywhere that needs it
  pogUniverse <- indexDt[pog_type == "84_non_blw", display]
  
  # 7. 4534190 
  pogDataForCheck <- pogData[, .(sgnhlder_n = sum(position_hfacings[id == 4534190]),
                                 header_n = sum(position_hfacings[id %in% c(4585012, 4538558)])),
                             by = display]
  print(pogDataForCheck[header_n > 0][sgnhlder_n != header_n])
  print("Address this ^^^")
  pogsToDeleteFrom <- pogDataForCheck[header_n == 0, display]
  configs <- updateConfigsDelete(configs = configs, actionPogs = pogsToDeleteFrom,
                                 deleteItem = 4534190)
  
  # 8. In the universe of blw2 pogs...
  #   - we need to swap in 004534224 for 004435650
  #   - we need to swap in 004585011 for 004585013
  #  But first we need to count the old items for these pogs and update facings of 
  #  004534184 (for non-gnd) and 004534188 (for gnd)
  # And delete these from everywhere else
  nonGndSign <- 4534184
  gndSign <- 4534188
  oldItem1 <- 4435650
  oldItem2 <- 4585013
  newItem1 <- 4534224
  newItem2 <- 4585011
  # I. delete 4184 from all non-eligible stores
  #   - eligible stores are 84" non-gnd blw2.0
  eligiblePogs <- indexDt[is_blw2 == 1 & is.na(is_gnd) & height == 84, unique(display)]
  ineligiblePogs <- leftOnly(indexDt[, display], eligiblePogs)
  pogsToFaceCheck <- pogData[display %in% eligiblePogs, .(num_item1 = sum(position_hfacings[id == oldItem1]),
                                                         num_item2 = sum(position_hfacings[id == oldItem2]),
                                                         current_facings = sum(position_hfacings[id == nonGndSign])),
                                                         by = display][, needed_facings := num_item1 + num_item2]
  ineligiblePogs <- unique(c(ineligiblePogs, pogsToFaceCheck[needed_facings == 0, display]))
  # i. delete where we dont need it
  pogsToDeleteFrom <- pogData[id == nonGndSign & display %in% ineligiblePogs, unique(display)]
  configs <- updateConfigsDelete(configs = configs, actionPogs = pogsToDeleteFrom,
                                 deleteItem = nonGndSign)
  # ii. insert where we dont have it
  pogsToInsertAt <- pogsToFaceCheck[current_facings == 0, display]
  if (length(pogsToInsertAt) > 0) {
    configs <- updateConfigs(configs = configs, existingItemId = anchorItem,
                             newItemId = nonGndSign, dt = data.table(display = pogsToInsertAt))
  }
  # iii. update facings where we need to
  pogsToUpdateFacings <- pogsToFaceCheck[current_facings > 0
                                         ][current_facings != needed_facings, .(display, new_hfacings = needed_facings,
                                                                                position_hfacings = current_facings)]
  configs <- updateConfigsFacings(configs, dt = pogsToUpdateFacings, existingItemId = nonGndSign)
  # iv. swap old items for new ones in these pogs
  pogsForSwap1 <- pogData[display %in% eligiblePogs][id == oldItem1, unique(display)]
  configs <- updateConfigsSwap(configs = configs, actionPogs = pogsForSwap1, existingItemId = oldItem1,
                               newItemId = newItem1)
  pogsForSwap2 <- pogData[display %in% eligiblePogs][id == oldItem2, unique(display)]
  configs <- updateConfigsSwap(configs = configs, actionPogs = pogsForSwap2, existingItemId = oldItem2,
                               newItemId = newItem2)
  
  # Doing all this again for gnd stores and gnd item
  # I. delete 4188 from all non-eligible stores
  #   - eligible stores are 84" non-gnd blw2.0
  eligiblePogs <- indexDt[is_blw2 == 1 & is_gnd == 1 & height == 84, display]
  ineligiblePogs <- leftOnly(indexDt[, display], eligiblePogs)
  pogsToFaceCheck <- pogData[display %in% eligiblePogs, .(num_item1 = sum(position_hfacings[id == oldItem1]),
                                                          num_item2 = sum(position_hfacings[id == oldItem2]),
                                                          current_facings = sum(position_hfacings[id == gndSign])),
                             by = display][, needed_facings := num_item1 + num_item2]
  ineligiblePogs <- unique(c(ineligiblePogs, pogsToFaceCheck[needed_facings == 0, display]))
  # i. delete where we dont need it
  pogsToDeleteFrom <- pogData[id == gndSign & display %in% ineligiblePogs, unique(display)]
  configs <- updateConfigsDelete(configs = configs,
                                 actionPogs = pogsToDeleteFrom,
                                 deleteItem = gndSign)
  # ii. insert where we dont have it
  pogsToInsertAt <- pogsToFaceCheck[current_facings == 0, display]
  if (length(pogsToInsertAt) > 0) {
    configs <- updateConfigs(configs = configs, existingItemId = anchorItem,
                             newItemId = gndSign, dt = data.table(display = pogsToInsertAt))
  }
  # iii. update facings where we need to
  pogsToUpdateFacings <- pogsToFaceCheck[current_facings > 0 & needed_facings > 0] %>%
    .[current_facings != needed_facings, .(display, new_hfacings = needed_facings,
                                           position_hfacings = current_facings)]
  configs <- updateConfigsFacings(configs, dt = pogsToUpdateFacings, existingItemId = gndSign)
  # iv. swap old items for new ones in these pogs
  pogsForSwap1 <- pogData[display %in% eligiblePogs][id == oldItem1, unique(display)]
  configs <- updateConfigsSwap(configs = configs, actionPogs = pogsForSwap1, existingItemId = oldItem1,
                               newItemId = newItem1)
  pogsForSwap2 <- pogData[display %in% eligiblePogs][id == oldItem2, unique(display)]
  configs <- updateConfigsSwap(configs = configs, actionPogs = pogsForSwap2, existingItemId = oldItem2,
                               newItemId = newItem2)
  
  # We are going to go through a similar process for items with 3' headers, thing is there wont be any on the POGs currently
  # So we need to add them beforehand (so no swap step and just counting the newItem1)
  nonGndSign <- 4538305
  gndSign <- 4536641
  newItem1 <- 4560050
  # I. delete 8305 from all non-eligible stores
  #   - eligible stores are 84" non-gnd blw2.0
  eligiblePogs <- indexDt[is_blw2 == 1 & is.na(is_gnd) & height == 84, display]
  ineligiblePogs <- leftOnly(indexDt[, display], eligiblePogs)
  pogsToFaceCheck <- pogData[display %in% eligiblePogs, .(needed_facings = sum(position_hfacings[id == newItem1]),
                                                          current_facings = sum(position_hfacings[id == nonGndSign])),
                             by = display]
  ineligiblePogs <- unique(c(ineligiblePogs, pogsToFaceCheck[needed_facings == 0, display]))
  # i. delete where we dont need it
  pogsToDeleteFrom <- pogData[id == nonGndSign & display %in% ineligiblePogs, display]
  configs <- updateConfigsDelete(configs = configs, actionPogs = pogsToDeleteFrom,
                                 deleteItem = nonGndSign)
  # ii. insert where we dont have it
  pogsToInsertAt <- pogsToFaceCheck[current_facings == 0 & needed_facings > 0, display]
  if (length(pogsToInsertAt) > 0) {
    configs <- updateConfigs(configs = configs, existingItemId = anchorItem,
                             newItemId = nonGndSign, dt = data.table(display = pogsToInsertAt))
  }
  # iii. update facings where we need to
  pogsToUpdateFacings <- pogsToFaceCheck[current_facings > 0 & needed_facings > 0] %>% 
    .[current_facings != needed_facings, .(display, new_hfacings = needed_facings,
                                           position_hfacings = current_facings)]
  if (NROW(pogsToUpdateFacings) > 0) {
    configs <- updateConfigsFacings(configs, dt = pogsToUpdateFacings, existingItemId = nonGndSign)
  }
  
  # Doing all this again for gnd stores and gnd item
  # I. delete 8305 from all non-eligible stores
  #   - eligible stores are 84" non-gnd blw2.0
  eligiblePogs <- indexDt[is_blw2 == 1 & is_gnd == 1 & height == 84, display]
  ineligiblePogs <- leftOnly(indexDt[, display], eligiblePogs)
  pogsToFaceCheck <- pogData[display %in% eligiblePogs, .(needed_facings = sum(position_hfacings[id == newItem1]),
                                                          current_facings = sum(position_hfacings[id == gndSign])),
                             by = display]
  ineligiblePogs <- unique(c(ineligiblePogs, pogsToFaceCheck[needed_facings == 0, display]))
  # i. delete where we dont need it
  pogsToDeleteFrom <- pogData[id == gndSign & display %in% ineligiblePogs, display]
  configs <- updateConfigsDelete(configs = configs, actionPogs = pogsToDeleteFrom,
                                 deleteItem = gndSign)
  # ii. insert where we dont have it
  pogsToInsertAt <- pogsToFaceCheck[current_facings == 0 & needed_facings > 0, display]
  if (length(pogsToInsertAt) > 0) {
    configs <- updateConfigs(configs = configs, existingItemId = anchorItem,
                             newItemId = gndSign, dt = data.table(display = pogsToInsertAt))
  }
  # iii. update facings where we need to
  pogsToUpdateFacings <- pogsToFaceCheck[current_facings > 0 & needed_facings > 0] %>% 
    .[current_facings != needed_facings, .(display, new_hfacings = needed_facings,
                                           position_hfacings = current_facings)]
  if (NROW(pogsToUpdateFacings) > 0) {
    configs <- updateConfigsFacings(configs, dt = pogsToUpdateFacings, existingItemId = gndSign)
  }
  
  # for these items we are deleting where we dont need it and inserting where we do
  nrbItem <- 37130129
  correspondingItem <- 37130566
  pogsThatNeedNrbItem <- pogData[id == correspondingItem, unique(display)]
  pogsThatHaveNrbItem <- pogData[id == nrbItem, unique(display)]
  pogsToDeleteFrom <- leftOnly(pogsThatHaveNrbItem, pogsThatNeedNrbItem)
  pogsToInsertOnto <- rightOnly(pogsThatHaveNrbItem, pogsThatNeedNrbItem)
  configs <- updateConfigsDelete(configs = configs, actionPogs = pogsToDeleteFrom, deleteItem = nrbItem)
  configs <- updateConfigs(configs = configs, 
                           existingItemId = anchorItem, 
                           newItemId = nrbItem, 
                           dt = data.table(display = pogsToInsertOnto))
  
  nrbItem <- 37130123
  correspondingItem <- 37130384
  pogsThatNeedNrbItem <- pogData[id == correspondingItem, unique(display)]
  pogsThatHaveNrbItem <- pogData[id == nrbItem, unique(display)]
  pogsToDeleteFrom <- leftOnly(pogsThatHaveNrbItem, pogsThatNeedNrbItem)
  pogsToInsertOnto <- rightOnly(pogsThatHaveNrbItem, pogsThatNeedNrbItem)
  configs <- updateConfigsDelete(configs = configs, actionPogs = pogsToDeleteFrom, deleteItem = nrbItem)
  configs <- updateConfigs(configs = configs, 
                           existingItemId = anchorItem, 
                           newItemId = nrbItem, 
                           dt = data.table(display = pogsToInsertOnto))
  
  # They need to have one of 3 to get the display
  nrbItem <- 37130034
  correspondingItems <- c(37138649, 37137314, 37132844)
  pogsThatNeedNrbItem <- pogData[id %in% correspondingItems, unique(display)]
  pogsThatHaveNrbItem <- pogData[id == nrbItem, unique(display)]
  pogsToDeleteFrom <- leftOnly(pogsThatHaveNrbItem, pogsThatNeedNrbItem)
  pogsToInsertOnto <- rightOnly(pogsThatHaveNrbItem, pogsThatNeedNrbItem)
  configs <- updateConfigsDelete(configs = configs, actionPogs = pogsToDeleteFrom, deleteItem = nrbItem)
  if (length(pogsToInsertOnto) > 0) {
    configs <- updateConfigs(configs = configs, 
                             existingItemId = anchorItem, 
                             newItemId = nrbItem, 
                             dt = data.table(display = pogsToInsertOnto))
  }
  
  # Delete instructions from B pogs, make sure its in every other pog
  instructionId <- 4567354
  pogsThatNeedInstructions <- indexDt[abpog != "B", unique(display)]
  pogsThatDontNeedInstructions <- indexDt[abpog == "B", unique(display)]
  pogsThatHaveInstructions <- pogData[id == instructionId, unique(display)]
  pogsToAddInstructions <- leftOnly(pogsThatNeedInstructions, pogsThatHaveInstructions)
  pogsToDeleteInstructions <- rightOnly(pogsThatNeedInstructions, pogsThatHaveInstructions)
  configs <- updateConfigsDelete(configs = configs, actionPogs = pogsToDeleteInstructions, deleteItem = instructionId)
  configs <- updateConfigs(configs = configs, existingItemId = anchorItem, newItemId = instructionId, 
                           dt = data.table(display = pogsToAddInstructions))
  
  
  # Delete headers from blw 84" pogs
  headerItems <-  c(4585013, 4435650, 4436445)
  pogsThatDontNeedHeaders <- idt[height == 84 & blw_flag == 1, display]
  for (headerItem in headerItems) {
    pogsThatHaveHeader <- pogData[id == headerItem, unique(display)]
    pogsThatGetHeaderSwapped <- configs$swapsConfig[ExistingItemId == headerItem, unique(PogId)]
    if (length(pogsThatGetHeaderSwapped) > 0) {
      pogsThatHaveHeader <- leftOnly(pogsThatHaveHeader, pogsThatGetHeaderSwapped)
    }
    pogsToDeleteHeaderFrom <- intersect(pogsThatDontNeedHeaders, pogsThatHaveHeader)
    if (length(pogsToDeleteHeaderFrom) > 0) {
      configs <- updateConfigsDelete(configs = configs, actionPogs = pogsToDeleteHeaderFrom, deleteItem = headerItem)
    }
  }  
    
  writeConfigs(configsOut = configs, projectConfig = config, testOut = TRUE)
  
  # write index config - read in from clipboard for now 
  indexConfig <- clipr::read_clip_tbl() %>% data.table %>% cleanNames %>%
    .[, .(PogId = display, ProjectId = batch, Seq = sortingorder)]
    .[order(cluster), N := seq_len(.N), by = cluster] %>%
    .[, batch := paste0(cluster, " - ", ceiling(N / 20))]
  batchKey <- indexConfig[, .(batch)] %>% unique %>% .[, batch_rename := paste0(seq_len(.N), ". ", batch)]
  indexConfig <- merge(indexConfig, batchKey, by = "batch")
  indexConfig[, Seq := seq_len(.N), by = batch_rename]
  indexConfig <- indexConfig[, .(PogId = display, ProjectId = batch_rename, Seq)]
  fwrite(indexConfig, file.path(config$configsDir, "indexConfig.csv"))
  
}


if (FALSE) {
  # textured hair
  setwd("C:/Users/William.Roberts/Downloads/textured_hair")
  boxIds <- fread("talk_boxes.csv")[, 1]
  pogData <- fread("raw_pog_data.csv") %>% cleanNames
  filteredPogData <- pogData[id %in% boxIds$item_id]
  outFile <- filteredPogData[, .(Facings = .N), by = .(PogId = planogram_desc2, FixtureX = as.integer(fixture_x))]
  fwrite(outFile, "infobox_config.csv")
}
