initializeConfigSiq <- function() {
  return(
    data.table(action = character(),
               affectedItem = integer(),
               anchorItem = integer(),
               anchorShelf = character(),
               replacementItem = integer(),
               adjacency = character(),
               facing = integer(),
               orientation = character(),
               planoMinWidth = integer(),
               planoMaxWidth = integer(),
               planoMatch1 = character(),
               seq = integer())
  )
}

siqSlot <- function(config, affItem, anchItem, pogList = "") {
  return(
    rbind(config,
          data.table(action       = "Slot",
                     affectedItem = affItem,
                     anchorItem   = anchItem,
                     replacementItem = "",
                     facing = "",
                     planoMatch1  = pogList),
          fill = T
    )
  )
}

siqDelete <- function(config, affItem, pogList = "") {
  return(
    rbind(config,
          data.table(action       = "Delete",
                     affectedItem = affItem,
                     anchorItem   = "",
                     replacementItem = "",
                     facing = "",
                     planoMatch1 = pogList),
          fill = T
    )
  )
}

siqSwap <- function(config, oldItem, newItem, pogList = "", seq = 1) {
  return(
    rbind(config,
          data.table(action = "Swap",
                     affectedItem = oldItem,
                     anchorItem = "",
                     replacementItem = newItem,
                     facing = "",
                     planoMatch1 = pogList,
                     seq = seq),
          # data.table(action = "Slot",
          #            affectedItem = newItem,
          #            anchorItem = oldItem,
          #            replacementItem = "",
          #            facing = "",
          #            planoMatch1 = pogList),
          # data.table(action = "Delete",
          #            affectedItem = oldItem,
          #            anchorItem = "",
          #            facing = "",
          #            planoMatch1 = pogList),
          fill = T)
  )
}

siqSwapPartial <- function(siqConf, swapList = data.table(item1 = c(), item2 = c()), positionDt) {
  
  pdt <- copy(positionDt) %>% 
    .[, .(n_facings = sum(hfacings),
          n_stores = number_of_stores[1]),
      keyby = .(itemid, planoid)]
  
  j <- 1
  for (i in 1 : nrow(swapList)) {
    
    item1 <- swapList[i, item1]
    item2 <- swapList[i, item2]
    
    # CREATE TABLE
    # item1 x every pog
    everyPog <- pdt[, unique(planoid)]
    keyTable <- CJ(itemid = item1, planoid = everyPog)
    setkey(keyTable, itemid, planoid)
    # Add flag_1
    # item1 x every pog | 0 if in itemOnPogs, 1 if not in 
    itemOnPogs <- pdt[itemid == item1, unique(planoid)]
    keyTable[, flag_1 := fifelse(planoid %in% itemOnPogs, 1, 0)]
    
    # Add flag_2
    # item1 x every pog | 0 if should be in, 1 if not in
    itemToBeOnPogs <- pdt[itemid == item2, unique(planoid)]
    keyTable[, flag_2 := fifelse(planoid %in% itemToBeOnPogs, 1, 0)]
    
    # a) 0|1 (pogs01) 1. add item1|pogs01 "Swap" with item2
    #                 2. remove item2|pogs01 from pdt
    pogs01 <- keyTable[flag_1 == 0][flag_2 == 1, planoid]
    if (length(pogs01) > 0) {
      pdt[(planoid %in% pogs01 & itemid == item2), itemid := item1]
      siqConf <- siqSwap(siqConf, oldItem = item2, newItem = item1, pogList = pogs01, seq = j)
    }
    
    # b) 1|0 (pogs10) 1. add item1|pogs10 "Swap" actions to config
    #                 2. remove item1|pogs10 rows from pdt
    pogs10 <- keyTable[flag_1 == 1][flag_2 == 0, planoid]
    if (length(pogs10) > 0) {
      pdt[(planoid %in% pogs10 & itemid == item1), itemid := item2]
      siqConf <- siqSwap(siqConf, oldItem = item1, newItem = item2, pogList = pogs10, seq = j)
    }
    j <- j + 1
  }
  return(siqConf)
}

siqFace <- function(config, affItem, hface, pogList = "") {
  return(
    rbind(config,
          data.table(action = "Facing Change",
                     affectedItem = affItem,
                     anchorItem = "",
                     replacementItem = "",
                     facing = hface,
                     planoMatch1 = pogList),
          fill = T
    )
  )
}

siqDeleteAndFaceOver <- function(siqConf, affItem, pdt) {
  # Faces over the item on shelf with fewest current linear space
  shelfList <- pdt[itemid == affItem, 
                   .(planoid, 
                     fixturename,
                     facings_to_replace = hfacings)] %>%
    unique
  setkey(shelfList, planoid, fixturename)
  setkey(pdt, planoid, fixturename)
  filteredDt <- pdt[shelfList] %>%
    .[itemid != affItem] %>%
    .[order(linear), .(faceItems = itemid[1],
                       facing = hfacings[1] + facings_to_replace[1]),
      by = planoid]
  
  aloneOnShelf <- leftOnly(shelfList[, planoid], filteredDt[, planoid])
  if (length(aloneOnShelf) > 0) {
    warning(paste0("These POGs didnt have anything else on shelf with item ",
            affItem, paste(aloneOnShelf, collapse = ',')))
  }
  
  return(
    rbind(siqConf,
          filteredDt[, .(action = "Facing Change",
                         affectedItem = faceItems,
                         anchorItem = "",
                         replacementItem = "",
                         facing,
                         planoMatch1 = planoid)],
          filteredDt[, .(action = "Delete",
                         affectedItem = affItem,
                         anchorItem = "",
                         replacementItem = "",
                         facing = "",
                         planoMatch1 = planoid)],
          fill = T
    )
  )
  
}

cleanPogClip <- function(pogs = read_clip()) {
  paste0("\"", paste(pogs, collapse = "\",\""), "\"")
}

writeCc <- function(p = read_clip()) {
  write_clip(cleanPogClip(p))
}
# 12.05.2023
if (FALSE) {
  
  rm(list = ls(all.names = TRUE)); gc();
  setwd("~/r_code")
  source("00_initialize.R")
  config <- initializeProject(configPath = "~/r_code/config.yaml")
  # late series of swaps
  # straight delete
  siqConfig <- initializeConfigSiq() %>%
    siqDeleteAndFaceOver(affItem = 288070198, pdt = positionDt) %>%
    siqSwap(oldItem = 288075584, newItem = 288076798) %>%
    siqSwap(oldItem = 288070810, newItem = 288074142) %>%
    siqSwap(oldItem = 288077548, newItem = 288075788) %>%
    siqSwap(oldItem = 288076332, newItem = 288070231)
  
  siqConfig <- initializeConfigSiq() %>%
    siqDeleteAndFaceOver(affItem = 288075584, pdt = positionDt) %>%
    siqDeleteAndFaceOver(affItem = 288070810, pdt = positionDt) %>%
    siqDeleteAndFaceOver(affItem = 288077548, pdt = positionDt) %>%
    siqDeleteAndFaceOver(affItem = 288076332, pdt = positionDt)
  
  positionDt <- pathToDt()
  
  countPods <- function(a_item, pdt) {
    pdt[itemid == a_item, .(number_of_stores, planoid)] %>% 
      unique %>%
      .[, sum(number_of_stores)]
  }
  
  itemSwapDt <- data.table(item1 = c(288079346,
                                     288070285,
                                     288075882,
                                     288077155,
                                     288072057,
                                     288070141,
                                     288077413,
                                     288078771,
                                     288070134,
                                     288077082,
                                     288073314,
                                     288071555,
                                     288070710,
                                     288073503,
                                     288073310),
                           item2 = c(288077082,
                                     288075882,
                                     288077155,
                                     288072057,
                                     288077413,
                                     288070021,
                                     288078771,
                                     288070134,
                                     288072424,
                                     288073314,
                                     288071555,
                                     288070710,
                                     288073845,
                                     288071302,
                                     288073146))
  
  iDt <- siqSwapPartial(siqConfig, swapList = itemSwapDt, positionDt = positionDt)
}

# step 1: create a itemSwapMapDt which is |itemId x pogId|flag_1|flag_2
#   - flag_1: 1 if on mod, 0 if not
#   - flag_2: 1 if going to be on mod, 0 if not
# filter to flag_1 == 0 & flag_2 == 1
# insert itemId to that pogId
# 
