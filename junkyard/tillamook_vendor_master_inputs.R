checkForGoldenIds <- function(goldenIds, positionDt, attDt) {
  # outputs a list of POGs and a flag for if they violate the golden IDs rule
  # golden IDs rule: all golden ids should maintain exclusivity to at least 1 sectionId
  # if the golden IDs span multiple section IDs, their maximum proportion of space should be 1
  # - exceptions are flagged (i.e. A/B POGs, really small pogs)
  
  # filter out irrelevant items
  positionDt <- positionDt[itemId %in% attDt$itemId]
  
  spacePerSection <- positionDt[, .(total_space = sum(linear)), keyby = .(planoId, sectionId)]
  sectionsWithGoldenIds <- positionDt[itemId %in% goldenIds, .(golden_space = sum(linear)),
                                      keyby = .(planoId, sectionId)] %>%
    .[spacePerSection, golden_proportion := golden_space / total_space] %>%
    .[, is_max := fifelse(seq_len(.N) == which.max(golden_space), 1, 0), by = planoId]
  
  maxSectionPerPog <- sectionsWithGoldenIds[is_max == 1] %>%
    .[order(golden_proportion)]
  
  
    
}


writeFillMasterConfig <- function(templateDt,
                                  itemUniverse,
                                  positionDt, 
                                  fixtureDt,
                                  attDt) {
  # Fills in all shelves that are occupied by itemUniverse with template from templateDt
  
  # Filter to just shelf type fixtures
  shelfDt <- fixtureDt[fixtureType == "Shelf"] %>% 
    .[order(planoId, segId, fixtureY)]
  
  shelfUniverseDt <- positionDt[itemId %in% itemUniverse, .(n_items_universe = uniqueN(itemId),
                                                            linear_universe = sum(linear),
                                                            n_facings_universe = sum(n_facings)),
                                by = .(planoId, fixtureId)] %>%
    merge(fixtureDt[, .(fixtureX = fixtureX[1], 
                        fixtureY = fixtureY[1]),
                    by = .(planoId, fixtureId)],
          by = c("planoId", "fixtureId"))
  
  sharedShelfDt <- positionDt[!(itemId %in% itemUniverse), .(n_items_not_universe = uniqueN(itemId),
                                                                linear_not_universe = sum(linear),
                                                                n_facings_not_universe = sum(n_facings)),
                              by = .(planoId, fixtureId)] %>% 
    merge(fixtureDt[, .(planoId, 
                        fixtureId,
                        free_space = fixture_available_linear)] %>% unique, 
          by = c("planoId", "fixtureId"), 
          all.x = TRUE)
  shelfUniverseDt <- merge(shelfUniverseDt, sharedShelfDt, all.x = T, by = c("planoId", "fixtureId")) %>%
    .[is.na(linear_not_universe), ':='(n_items_not_universe = 0,
                                       linear_not_universe = 0,
                                       n_facings_not_universe = 0)]
  
  # apply templateId grouping technique
  shelfUniverseDt[, templateId := sum(fifelse(n_items_not_universe > 0, .5, 1)), by = planoId]
  
  
  
  shelfUniverseDt[order(planoId, n_facings_universe, -fixtureY), shelfNumId := seq_len(.N), by = planoId]
  
  shelfToTemplatePositionDt <- merge(shelfUniverseDt, 
                                     templateDt[, templateId := as.numeric(templateId)],
                                     by = c("templateId", "shelfNumId"),
                                     allow.cartesian = T)
  
  # Write Config
  outputConfig <- initializeConfigSiq()
  
  # Delete every item in item universe
  for (item in itemUniverse) {
    outputConfig <- siqDelete(outputConfig, affItem = item)
  }
  # Then slot each shelf-position in the template Dt
  outputConfig <- rbind(outputConfig,
                        shelfToTemplatePositionDt[, .(action = "Slot",
                                                      affectedItem = tItemId,
                                                      anchorShelf = fixtureId,
                                                      adjacency = "Left",
                                                      facing = tItemFacings,
                                                      planoMatch1 = planoId)],
                        fill = TRUE)
  return(outputConfig)
}

slotItemToAnchor <- function(targetItem, 
                             sisterItems,
                             podDt,
                             positionDt) {
  # good humor in + 200
  targetPods <- podDt[itemId == targetItem, landed_vs_target]
  if (targetPods > 0) {
    stop("item doesn't need slotting." %>% paste("item: ", targetItem))
  } else {
    # find the best item to take facing from
    currentPogs <- positionDt[itemId == targetItem, unique(planoId)]
    candidatePositions <- positionDt %>%
      .[!(planoId %in% currentPogs)] %>%
      .[, .(n_facings = sum(n_facings)),
        by = .(planoId, itemId, n_stores)] %>%
      .[itemId %in% sisterItems] %>%
      .[n_facings > 1] %>%
      .[, .(itemId = itemId[1],
            n_facings = n_facings[1]),
        by = .(planoId, n_stores)] %>%
      .[order(-n_stores), tot := cumsum(n_stores)] %>%
      .[tot <= targetPods * -1]
    outputConfig <- initializeConfigSiq() %>%
      rbind(candidatePositions[, .(action = "Facing Change",
                                   affectedItem = itemId,
                                   facing = n_facings - 1,
                                   planoMatch1 = planoId)],
            candidatePositions[, .(action = "Slot",
                                   affectedItem = targetItem,
                                   anchorItem = itemId,
                                   facing = 1,
                                   planoMatch1 = planoId)], fill = T)
    return(outputConfig)
    
    
  }
}

# 12/28/2023
# 1. BJ 
if (FALSE) {
  
  # setup
  rm(list = ls(all.names = TRUE)); gc();
  setwd("~/r_code")
  source("00_initialize.R")
  source("~/r_code/tillamook_vendor_master_inputs.R")
  config <- initializeProject(configPath = "~/r_code/config.yaml")
  
  setwd("C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/06 - Ice Cream/2024/04.28 April Transvision/03 - Tie Reports")
  
  
  tiesDt <- fread("ice_cream_cat_ties.csv") %>%
    cleanNames %>%
    .[, .(storeId = store, planoId = display)]
  
  attDt <- getAttributionDt(config$attributionDtPath, "ICE_CREAM")
  positionDt <- fread("position_dt_latest.csv")
  fixtureDt <- fread("fixture_dt_latest.csv")
  perfDt <- fread("performance_library_latest.csv") %>% .[, n_stores := NULL]
  
  # join number of stores to positionDt
  positionDt <- merge(positionDt, 
                      tiesDt[, .(n_stores = uniqueN(storeId)),
                             keyby = planoId],
                      all.x = T)
  

  
  # join target and actual PODs to attDt
  targetPods <- fread("targetPods.csv") %>%
    .[, .(itemId = itemid, target_pods)]
  attDt <- merge(attDt, targetPods, all.x = T)
  podDt <- positionDt[, .(planoId, itemId, n_stores)] %>%
    unique %>%
    .[, .(landed_pods = sum(n_stores)), by = itemId] %>%
    merge(attDt, all.y = T) %>%
    .[order(landed_pods - target_pods), .(itemId, 
                                          landed_pods,
                                          target_pods,
                                          landed_vs_target = landed_pods - target_pods,
                                          brand, subclass, item_description)]
 
  # prePostDt for item lookup report sheet 2
  prePositionDt <- fread("position_dt_pre_automation.csv")
  
  prePostDt <- merge(positionDt[, .(n_facings_post = sum(n_facings)), by = .(planoId, itemId)],
                     prePositionDt[, .(n_facings_pre = sum(n_facings)), by = .(planoId, itemId)],
                     all = T)
  prePostDt[is.na(n_facings_pre), n_facings_pre := 0]
  prePostDt[is.na(n_facings_post), n_facings_post := 0]
  
  prePostDt[n_facings_post == 0 & n_facings_pre > 0, item_status := "NCF"]
  prePostDt[n_facings_pre == 0 & n_facings_post > 0, item_status := "NEW TO STORE"]
  prePostDt[is.na(item_status), item_status := "CF"]
  # 
  positionDt[, .(n_facings = sum(n_facings)), by = .(planoId, itemId)]

  # good humor
  finalOutput <- slotItemToAnchor(targetItem = 288070724,
                                  sisterItems = podDt[brand == "Good Humor", itemId],
                                  podDt = podDt,
                                  positionDt = positionDt)
    
  
  # bj SP
  bjSps <- podDt[subclass == "288-07-06 SUPER PREMIUM"][brand == "Ben & Jerry's"][landed_vs_target > -30][target_pods > 1500, itemId]
  finalOutput <- rbind(finalOutput,
                       slotItemToAnchor(targetItem = 288070231,
                                        sisterItems = bjSps[1 : 10],
                                        podDt = podDt,
                                        positionDt = positionDt),
                       slotItemToAnchor(targetItem = 288075788,
                                        sisterItems = bjSps[11 : length(bjSps)],
                                        podDt = podDt,
                                        positionDt = positionDt),
                       fill = T)
  
  # bj ND
  bjNds <- podDt[subclass == "288-07-05 NONDAIRY PINTS&NOV"][brand == "Ben & Jerry's"][landed_vs_target > -30][target_pods > 1500, itemId]
  finalOutput <- rbind(finalOutput,
                       slotItemToAnchor(targetItem = 288076798,
                                        sisterItems = c(288074818, 288072422),
                                        podDt = podDt,
                                        positionDt = positionDt),
                       slotItemToAnchor(targetItem = 288074142,
                                        sisterItems = c(288074476, 288076895),
                                        podDt = podDt,
                                        positionDt = positionDt),
                       fill = T)
  
  # swap out magnum item
  finalOutput <- rbind(finalOutput,
                       slotItemToAnchor(targetItem = 288070786,
                                        sisterItems = podDt[brand == "Magnum"][target_pods > 1300][landed_vs_target > -30, itemId],
                                        podDt = podDt,
                                        positionDt = positionDt))
  
  

  
}

if (FALSE) {
  
  # setup
  rm(list = ls(all.names = TRUE)); gc();
  setwd("~/r_code")
  source("00_initialize.R")
  source("~/r_code/tillamook_vendor_master_inputs.R")
  config <- initializeProject(configPath = "~/r_code/config.yaml")
  
  setwd("C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/06 - Ice Cream/2024/04.28 April Transvision/03 - Tie Reports")
  
  
  tiesDt <- fread("ice_cream_cat_ties.csv") %>%
    cleanNames %>%
    .[, .(storeId = store, planoId = display)]
  
  attDt <- getAttributionDt(config$attributionDtPath, "ICE_CREAM")
  positionDt <- fread("position_dt_latest.csv")
  fixtureDt <- fread("fixture_dt_latest.csv")
  
  # join number of stores to positionDt
  positionDt <- merge(positionDt, 
                      tiesDt[, .(n_stores = uniqueN(storeId)),
                             keyby = planoId],
                      all.x = T)
  
  # join target and actual PODs to attDt
  targetPods <- fread("targetPods.csv") %>%
    .[, .(itemId = itemid, target_pods)]
  attDt <- merge(attDt, targetPods, all.x = T)
  podDt <- positionDt[, .(planoId, itemId, n_stores)] %>%
    unique %>%
    .[, .(landed_pods = sum(n_stores)), by = itemId] %>%
    merge(attDt, all.y = T) %>%
    .[order(landed_pods - target_pods), .(itemId, 
                            landed_pods,
                            target_pods,
                            landed_vs_target = landed_pods - target_pods,
                            brand, subclass, item_description)]
  
  
  
  # Tillamook
  tillTemplateDt <- read_xlsx("TemplateDts.xlsx", sheet = "TILLAMOOK") %>% as.data.table
  tillUniverse <- attDt %>%
    .[brand == "Tillamook"] %>%
    .[subclass == "288-07-01 PREMIUM ICE CREAM", itemId] %>% unique
  
  tillConfig <- writeFillMasterConfig(templateDt = tillTemplateDt,
                                      itemUniverse = tillUniverse,
                                      positionDt = positionDt,
                                      fixtureDt = fixtureDt,
                                      attDt = attDt)
  
  
  # Talenti
  talentiTemplateDt <- read_xlsx("TemplateDts.xlsx", sheet = "TALENTI") %>% as.data.table
  talentiUniverse <- attDt %>%
    .[brand == "Talenti"] %>%
    .[subclass == "288-07-08 GELATO & SORBETTO", unique(itemId)] %>%
    sort %>%
    c(attDt[brand == "Noosa", unique(itemId)]) %>%
    leftOnly(c(288077677, 288074322, 288070552, 288077449)) %>%
    leftOnly(attDt[brand == "Talenti"][grepl("bar", tolower(item_description)), itemId])
  
  talentiConfig <- writeFillMasterConfig(templateDt = talentiTemplateDt,
                                         itemUniverse = talentiUniverse,
                                         positionDt = positionDt, 
                                         fixtureDt = fixtureDt,
                                         attDt = attDt)
  
  # Popsicle
  popsicleUniverse <- attDt %>%
    .[brand == "Popsicle", unique(itemId)]
  popsicleTemplateDt <- read_xlsx("TemplateDts.xlsx", sheet = "POPSICLE") %>% as.data.table
  popsicleConfig <- writeFillMasterConfig(templateDt = popsicleTemplateDt,
                                          itemUniverse = popsicleUniverse,
                                          positionDt = positionDt,
                                          fixtureDt = fixtureDt,
                                          attDt = attDt)
  # Yasso
  yassoUniverse <- attDt[brand == "Yasso", unique(itemId)]
  yassoTemplateDt <- read_xlsx("TemplateDts.xlsx", sheet = "YASSO") %>% as.data.table
  yassoConfig <-  writeFillMasterConfig(templateDt = yassoTemplateDt,
                                        itemUniverse = yassoUniverse,
                                        positionDt = positionDt,
                                        fixtureDt = fixtureDt,
                                        attDt = attDt)
  # Magnum
  magnumUniverse <- attDt[brand == "Magnum"][subclass == "288-07-03 NON-FRUIT NOVELTIE", unique(itemId)]
  magnumTemplateDt <- read_xlsx("TemplateDts.xlsx", sheet = "MAGNUM") %>% as.data.table
  magnumConfig <-  writeFillMasterConfig(templateDt = magnumTemplateDt,
                                         itemUniverse = magnumUniverse,
                                         positionDt = positionDt,
                                         fixtureDt = fixtureDt,
                                         attDt = attDt)
  # HD Novs
  hdNovsUniverse <- attDt[brand == "Haagen-Dazs"] %>%
    .[subclass == "288-07-03 NON-FRUIT NOVELTIE", unique(itemId)]
  hdNovsTemplateDt <- read_xlsx("TemplateDts.xlsx", sheet = "HD") %>% as.data.table
  hdNovsConfig <-  writeFillMasterConfig(templateDt = hdNovsTemplateDt,
                                         itemUniverse = hdNovsUniverse,
                                         positionDt = positionDt,
                                         fixtureDt = fixtureDt,
                                         attDt = attDt)
  
  hdNovsConfig[action == "Slot" & affectedItem %in% c(288070288, 288075100), adjacency := "Right"]
  
  total <- rbind(tillConfig,
                 talentiConfig,
                 popsicleConfig,
                 yassoConfig,
                 magnumConfig,
                 hdNovsConfig)
  write_clip(total)
}