# Ice Cream
rm(); gc()
setwd("~/r_code")
source("00_initialize.R")

setwd(config$baseDir)

#######################################################################################################################
#                                              CLEAN STORE TIES                                                       #
#######################################################################################################################
setDate <- "2023-05-07"

rawTieDt <- fread(config$rawTieReportPath) %>% cleanNames() %>% unique %>% 
  .[, ':='(discontinue_date = as.Date(discontinue_date, "%m/%d/%Y"),
           set_date         = as.Date(set_date, "%m/%d/%Y"))] %>%
  .[grepl("ICE", title)]

currentTieDt <- buildTieReport(rawTieDt)
storePogDt <- currentTieDt[, .(storeId = store,
                               pogId = display,
                               title,
                               width, depth, height)]
if (FALSE) {
  fwrite(storePogDt, config$cleanTieReportPath)
}

# Index dt
clusterNames <- c("MNSTRM", "HSP", "FRSK", "SBRB")
for (clusterName in clusterNames) {
  storePogDt[grepl(clusterName, title), base_cluster := clusterName]
}
storePogDt[, cluster := base_cluster]
storePogDt[,fake_foot := floor(width / 2.5)]
storePogDt[, fake_foot_label := paste0(formatC(fake_foot, width = 2, flag = "0"), "DR")]
storePogDt[fake_foot < 5 & cluster == "FRSK", fake_foot_label := "04DR and under"]
storePogDt[fake_foot < 8 & cluster != "FRSK", fake_foot_label := "07DR and under"]

storePogDt[fake_foot > 13, fake_foot_label := "14DR and up"]
storePogDt[, is_ab := fifelse(grepl(" A | B ", title), 1, 0)]

storePogDt[, cluster := fifelse(is.na(cluster), "UNCLUSTERED", paste0(cluster, " - ", fake_foot_label))]
storePogDt[is_ab == 1, cluster := "IS AB"]

storePogDt[grepl("ICDTST", title), cluster := paste0("ICDTST - ", fake_foot_label)]
storePogDt[grepl("ICDTST", title) & (fake_foot < 9), cluster := "ICDTST - 08DR and under"]

storePogDt[, .(pogId, cluster)] %>% unique %>% .[, .N, keyby = cluster]


indexDt <- buildProjectIndexDt(storePogDt[, .(pogId, title, width, depth, height, cluster)] %>% unique, maxProjectSize = 30, allCols = FALSE)
indexDt <- merge(indexDt, storePogDt[, .(store_tied = uniqueN(storeId)), by = .(PogId = pogId, base_cluster, fake_foot_label,
                                                                                width, height, title)])



if (FALSE) {
  # warehouse audit
  warehouseNames <- fread(file.path(config$filesDir, "warehouse_pod_dt.csv")) %>% cleanNames() %>%
    .[,  warehouse]
  attributionDt <- getAttributionDt(config)
  pogItemDt <- getPogItemDt(config, era = "POST") %>%
    .[, .(pogId, positionId)] %>% unique %>%
    .[positionId %in% attributionDt[, unique(positionId)]]
  storePogDt <- getStorePogDt(config, era = "POST")
  
  
  
  # Check that the planogram titles are singly-assignable per the logic of the mathcing FN
  testDt <- storePogDt[, .(warehouseNameCandidates = lapply(title, FUN = function(x) {
    length(intersect(warehouseNames, strsplit(x, split = ":")[[1]])) })), by = title] %>%
    .[warehouseNameCandidates == 1]
  if (nrow(testDt[warehouseNameCandidates != 1]) > 0) {stop("warehouse logic needs investigating")}
  
  # add warehouse name to storePogDt
  storePogDt[, warehouse := lapply(title, FUN = function(x) {
    intersect(warehouseNames, strsplit(x, split = ":")[[1]])[1]}) %>% as.character]
  
  
  # add store count to pogItemDt
  setkey(storePogDt, pogId)
  setkey(pogItemDt, pogId)
  storeItemDt <- storePogDt[pogItemDt, allow.cartesian = TRUE]
  
  # item counts at warehouse level
  # omit blue bell
  # omit "HI", "AK" warehouses
  blueBellIds <- attributionDt[brand == "Blue Bell", unique(positionId)]
  warehouseItemDtRaw <- storeItemDt[, .(pod_count = uniqueN(storeId)), by = .(warehouse, positionId)]
  warehouseItemDt <- warehouseItemDtRaw %>%
    .[!(positionId %in% blueBellIds)] %>%
      .[!(warehouse %in% c("HI", "AK"))]

  configureWarehouseSwaps <- function(warehouseItemDt, 
                                      warehouseName, 
                                      pogItemDt, 
                                      storePogDt,
                                      attributionDt) {
    warehouseConfigs <- initializeConfigs()
    storePogFilteredDt <- storePogDt[warehouse == warehouseName]
    pogItemFilteredDt <- pogItemDt[pogId %in% storePogFilteredDt[, unique(pogId)]]
    warehouseItemFilteredDt <- warehouseItemDt[warehouse == warehouseName] %>%
      merge(attributionDt[, .(positionId = as.integer(positionId), brand, subclass)])
    
    increaseDt <- warehouseItemFilteredDt[pod_count >= 5] %>% .[order(brand, subclass)]
    decreaseDt <- warehouseItemFilteredDt[pod_count < 5] %>% .[order(brand, subclass)]
    
    # for every decrease, try to delete it by swapping everywhere
    decreaseItemPogDt <- merge(decreaseDt, pogItemFilteredDt, by = c("positionId"))
    increaseItemPogDt <- merge(increaseDt, pogItemFilteredDt, by = c("positionId"))
    for (currItem in decreaseDt$positionId) {
      # for every item we need to delete, we need to find candidate swaps
      # candidate swaps aren't on that pog
      currRows <- decreaseItemPogDt[positionId == currItem]
      for (pogToDeleteFrom in currRows$pogId) {
        itemsNotOnThisPog <- attributionDt[!(positionId %in% pogItemFilteredDt[pogId == pogToDeleteFrom]), positionId]
        candidateIncDts <- increaseItemPogDt %>% 
          .[subclass == currRows[1, subclass]] %>%
          .[brand == currRows[1, brand]] %>%
          .[pogId == pogToDeleteFrom] %>%
          .[positionId %in% itemsNotOnThisPog]
        setkey(candidateIncDts, position)
        .[, running_total := cumsum(pod_count)]
      }
    }
  }
  ##  generate configs per warehouse for a general swap by subclass-brand

  # create filtered storePogDt, pogItemDt
  
  # create current PODs off the filtered
  # currentPodDt 
  # desiredPodDt <- currentPodDt + warehouseItemDt
  
  # run the autoswap to get it close to there
  
}

if (FALSE) {
  
  # adhoc index
  adhocStorePogDt <- copy(storePogDt)
  adhocStorePogDt[, cluster := ""]
  clusterNames <- c("MNSTRM", "HSP", "FRSK", "SBRB", "ASN", "ICDTST")
  for (clusterName in clusterNames) {
    adhocStorePogDt[grepl(clusterName, title), cluster := clusterName]
  }
  adhocStorePogDt[cluster == "", cluster := "UNCLUSTERED"]
  adhocIndexDt <- buildProjectIndexDt(adhocStorePogDt, maxProjectSize = 60) %>%
    merge(storePogDt[, .(store_tied = uniqueN(storeId)), by = .(PogId = pogId, base_cluster, fake_foot_label,
                                                                width, height, title)])
  adhocIndexDt[order(ProjectId, Seq), .(display = PogId,
                                        store_tied,
                                        Seq,
                                        total_sort = seq_len(.N),
                                        batch = ProjectId,
                                        cluster = base_cluster,
                                        width, height, size = fake_foot_label,
                                        title, mpd = "")] %>% write_clip(
                                          
                                        )
  fwrite(adhocIndexDt, file.path(config$filesDir, "adhocIndexDt.csv"))
  fwrite(indexDt, file.path(config$filesDir, "cleanClustersConfig.csv"))
  
  
  # index dt for item lookup copypaste
  indexDtItemLookup <- indexDt[order(ProjectId, Seq), .(display = PogId,
                                                        store_tied,
                                                        Seq,
                                                        total_sort = seq_len(.N),
                                                        batch = ProjectId,
                                                        cluster = base_cluster,
                                                        width, height, size = fake_foot_label,
                                                        title, mpd = "")] %>%
    .[, ':='(combo = fifelse(grepl(title, pattern = "DESSERT"), "YES", "NO"),
             is_blbl = fifelse(grepl(title, pattern = "BLBL"), "YES", "NO"),
             is_asn = fifelse(grepl(title, pattern = "ASN"), "YES", "NO"),
             is_hsp = fifelse(grepl(title, pattern = "HSP"), "YES", "NO")), by = title]
                                                        
}

#######################################################################################################################
#                                              AUTOMATION STEPS                                                           #
#######################################################################################################################
attributionDt <- fread(config$attributionDtPath) %>%
  .[, mega_group := paste0(brand, "_", subclass)]
autoSwapDt <- fread(file.path(config$filesDir, "desiredPodDt.csv")) 
setnames(autoSwapDt, old = "desired_pods", new = "final_count")
positionPath <- file.path(config$configsDir, "jan_11_position_table.csv")
positionDt <- fread(positionPath)
fixtureDt <- fread(positionPath %>% gsub(pattern = "position", replacement = "fixture"))
planogramDt <- fread(positionPath %>% gsub(pattern = "position", replacement = "planogram"))

podDt <- buildPodDt(positionDt = positionDt,
                    storePogDt = storePogDt,
                    attributionDt = attributionDt,
                    attCols = c("item_description", "mega_group"))

# Good humor
traitedForItem <- positionDt[positionId == 288070175]

shelfMates <- traitedForItem[, .N, keyby = .(pogId, fixtureId)] %>% unique %>% merge(positionDt)
shelfMates[positionHFacings > 1, .N, keyby = positionId]

itemPositions <- positionDt[!(pogId %in% traitedForItem[, unique(pogId)])][positionId == 288070912]


# Klondike
incItem <- 288073020
stealItems <- c(288071227, 288071659, 288071235, 288070166)
traitedForItem <- positionDt[positionId == 288073020]
stealOptions <- positionDt[!(pogId %in% traitedForItem[, unique(pogId)])] %>%
  .[positionHFacings > 1] %>% 
  .[positionId %in% stealItems]

shelfMates <- traitedForItem[, .N, keyby = .(pogId, fixtureId)] %>% unique %>% merge(positionDt)
shelfMates[positionHFacings > 1, .N, keyby = positionId]
itemPositions <- positionDt[!(pogId %in% traitedForItem[, unique(pogId)])][positionId == 288070912]


finalCountsToNailDt <- merge(podDt, autoSwapDt, all.y = TRUE) %>%
  .[!is.na(store_count)] %>%
  .[final_count == 0 | (70 < abs(final_count - store_count))] %>%
  .[positionId %in% attributionDt[mega_group %in% c(# "Ben & Jerry's_288-07-05 NONDAIRY PINTS&NOV"),
                                                    "Ben & Jerry's_288-07-06 SUPER PREMIUM"),
  #                                                   "Haagen-Dazs_288-07-06 SUPER PREMIUM"),
                                  unique(positionId)]] %>%
  .[, desired_change := final_count - store_count] %>%
  .[order(desired_change)]

itemsToReduce <- finalCountsToNailDt[desired_change < 0]


# BJ non-dairy

# below lines will give pog list to delete and face out items
itemId <- itemsToReduce$positionId[1]
desiredChange <- itemsToReduce$desired_change[1]
megaGroup <- itemsToReduce$mega_group[1]
megaGroupDt <- positionDt[positionId %in% attributionDt[mega_group == megaGroup, positionId]] %>%
  merge(storePogDt[, .(store_count = uniqueN(storeId)), by = pogId])
# candidate pogs have one facing
candidatePogs <- megaGroupDt[positionId == itemId][positionHFacings == 1][order(-store_count)]
# for each candidate pog -- give facing back to item with most double facings already
relevantShelfPositions <- merge(candidatePogs[, .(pogId, fixtureId, store_count)] %>% unique,
                                positionDt[positionId %in% attributionDt[mega_group == megaGroup, positionId]])
candidateFacingsIncreases <- relevantShelfPositions[!(positionId == itemId)][positionHFacings == 1]
candidateFaceIncreaseItemsRanked <- positionDt[positionId %in% candidateFacingsIncreases[, unique(positionId)],
                                               .(avg_facing = mean(positionHFacings)), by = positionId] %>%
  .[order(-avg_facing)]
# use this table to choose, then...
cand1 <- 288074476
trigger1 <- candidateFacingsIncreases[positionId == cand1]

# Thing taylor asked for
taylorDt <- positionDt[, .(num_facings = sum(positionHFacings)), by = .(pogId, positionId)] %>%
  merge(attributionDt[, .(positionId, brand, subclass)]) %>% 
  .[, .(multifaced_item_count = sum(num_facings > 1)), 
    keyby = .(pogId, brand, subclass)]


# check for empty shelves
# empty_state: 0 (empty), 1 (nonempty)

# 

nonEmptyShelves <- positionDt[, .(empty_state = 0), by = .(fixtureId, pogId)]
allShelves <- fixtureDt[fixtureY < 70, .(fixtureId, pogId)] %>% unique
emptyShelves <- merge(nonEmptyShelves, allShelves, all.y = TRUE, by = c("fixtureId", "pogId")) %>%
  .[is.na(empty_state)]
#######################################################################################################################
#                                              DATA PREP                                                              #
#######################################################################################################################
# Kemps and MCConnels aren't called out in the POG IDs -- read in store list
kempsStores <- fread(file.path(config$filesDir, "kempsDt.csv")) %>% .[, unique(storeId)]
kempsPogs <- storePogDt[storeId %in% kempsStores ,unique(pogId)] %>%
  leftOnly(storePogDt[!(storeId %in% kempsStores), unique(pogId)]) %>% unique


mccStores <- fread(file.path(config$filesDir, "mccDt.csv")) %>% .[, unique(storeId)]
mccPogs <- storePogDt[storeId %in% mccStores, unique(pogId)] %>%
  leftOnly(storePogDt[!(storeId %in% mccStores), unique(pogId)]) %>% unique

stPogs <- storePogDt[grepl(":ST:", title), unique(pogId)]

# We look for these in the titles and group on where they are and aren't
# These all need to correspond to a file
keywordsThatNeedFiles <- c("BLBN", "HSP", "JENI", "TMOK", "VNL", "KEMPS", "MCC", "GTRS",
                           "THELMAS", "ITIT", "ASN", "AFM", "BLBL", "BLBLNOV")

tagsUniqueToTitle <- function(baseTitle, tweakedTitle, noCareTags = c()) {
  # returns the sequences of strings between ":" separator that only appear in baseTitle
  
  replaceTags <- data.table(old = "TMK", new = "TMOK")
  
  titleToTag <- function(title_, split_ = ":") {
    output <- lapply(strsplit(title_, split = split_),
                     FUN = function(x) leftOnly(x, noCareTags)) %>%
      lapply(FUN = function(x) if ("TMK" %in% x) {x[which(x == "TMK")] <- "TMOK"; x} else {x})
    return(output)
  }
  
  return(
    mapply(FUN = leftOnly, 
           titleToTag(baseTitle),
           titleToTag(tweakedTitle)) %>% 
      sapply(FUN = function(x) paste(sort(x), collapse = "_"))
  )
}

cleanseKeywordFromTag <- function(tag, keyword) {
  return(
    tag %>% 
      sapply(FUN = function(x) {
        x %>% 
          gsub(pattern = keyword,
               replacement = "") %>%
          gsub(pattern = "__",
               replacement = "_") %>%
          gsub(pattern = "^_|_$",
               replacement = "")
      }
      )
  )
}
#######################################################################################################################
#                                                AUTOFILL ROUND 1                                                     #
#######################################################################################################################

# Split out desserts POGs -- we work those first
autofillDt <- fread(file.path(config$filesDir, "anchorMap.csv")) %>%
  .[, .(pogId, autoId)] %>% unique %>%
  .[pogId != "J288AKO"]  %>%
  merge(copy(storePogDt)[, storeId := NULL] %>% unique, all.x = TRUE) %>% 
  .[, is_anchor := fifelse(pogId == autoId, 1, 0)]
if (autofillDt[is.na(title), .N] > 0) {
  print("Theres atuofilldts not in the storepogdt")
}

# Manually add regional brands to title per store lists
autofillDt[pogId %in% kempsPogs, title := paste0(title, ":KEMPS")]
autofillDt[pogId %in% mccPogs, title := paste0(title, ":MCC")]

# Split out anchor DTs into their own table
anchorsDt <- autofillDt[is_anchor == 1, .(anchor_title = title,
                            pogId)] %>% unique()
setkey(anchorsDt, pogId)
setkey(autofillDt, autoId)
autofillDt <- autofillDt[anchorsDt]
autofillDt[, N := .N, by = autoId]
noCareTags1 <- c("AFM", "ASN", 
                 "BDGT", "EXP", 
                 "FRSK", "GM", 
                 "HIDIGI", "MNSTRM", 
                 "PF", "SBFM", "SBRB", "TFL", "TMD", 
                 "W/ DESSERT 2DR",
                 "W/ DESSERT/ICE 2DR", "ICDTST")

autofillDt[, base_bespoke_tags := tagsUniqueToTitle(title, anchor_title, noCareTags = noCareTags1)]
autofillDt[, anchor_bespoke_tags := tagsUniqueToTitle(anchor_title, title, noCareTags = noCareTags1)]
# ICDTST needs to be its own batch, no matter what
autofillDt[grepl("ICDTST", title), ':='(base_bespoke_tags = "ICDTST", anchor_bespoke_tags = "")]


autofillDt[, .(num_pogs = .N), keyby = .(needs_this_done = base_bespoke_tags,
                                         needs_this_undone = anchor_bespoke_tags)] %>%
  .[order(num_pogs)]

autofillDt[, project_title:= fifelse(nchar(base_bespoke_tags) == 0, "", 
                                     paste0("Should have ", paste(strsplit(base_bespoke_tags, "_")[[1]], collapse = ", "))) %>%
             paste0(fifelse((nchar(anchor_bespoke_tags) * nchar(base_bespoke_tags)) > 0, "; ", "")) %>%
             paste0(fifelse(nchar(anchor_bespoke_tags) == 0, "", 
                            paste0("Shouldnt have ", paste(strsplit(anchor_bespoke_tags, "_")[[1]], collapse = ", "), ""))),
           by = .(base_bespoke_tags, anchor_bespoke_tags)]

# We need to build two indexDts separately
# first one - where there's a title mismatch
indexDtNoMasters <- autofillDt[project_title != "", .(PogId = pogId, project_title)]

# Build skeleton table of these project names since we need to add the templates
projectSkeleton <- indexDtNoMasters[, .N, keyby = project_title] %>%
  .[, num := formatC(seq_len(.N), width = 2, flag = "0")] %>% 
  .[, ProjectId := paste0(num, " - ", project_title)]

# build templatesForIndexDt that will 
templatesForIndexDt <- data.table(PogId = character(), ProjectId = character())
for (keyword in keywordsThatNeedFiles) {
  keywordDt <- copy(projectSkeleton) %>% 
    .[, .(ProjectId, PogId = keyword)] %>% 
    .[grepl(keyword, ProjectId)]
  templatesForIndexDt <- rbind(templatesForIndexDt, keywordDt)
}
indexDt1 <- rbind(merge(indexDtNoMasters, projectSkeleton, by = "project_title") %>% .[, .(PogId, ProjectId)],
                 templatesForIndexDt) %>%
  .[order(ProjectId, nchar(PogId))] %>% 
  .[, Seq := seq_len(.N), by = ProjectId]


# Second one - where there's no title mismatch
matchIndexDt <- copy(autofillDt)[project_title == ""]

# Build clusters for these in a super generic way then override manually what we want
clusterNames <- paste0(1 : 100, "DR") %>% c("5 DR")
for (clusterName in clusterNames)  {
  a <- matchIndexDt[grepl(clusterName, title)]
  if (nrow(a) > 0) {
    matchIndexDt[grepl(clusterName, title), cluster := clusterName]
  }
}
matchIndexDt[cluster %in% c("5DR", "5 DR"), cluster := "5DR"]
matchIndexDt[cluster %in% c("11DR", "12DR", "9DR"), cluster := "9DR and up"]
matchIndexDt[cluster %in% c("1DR", "2DR"), cluster := "1DR and 2DR"]

matchIndexDt[grepl("JENI", title), cluster := "JENI"]
matchIndexDt[grepl("VNL", title), cluster := "VNL"]
matchIndexDt[grepl("HSP", title), cluster := "HSP"]

matchIndexDt[, sum_count := 0]
for (keyword in keywordsThatNeedFiles %>% leftOnly(c("KEMPS", "MCC"))) {
  matchIndexDt[grepl(keyword, title), sum_count := sum_count + 1]
}
matchIndexDt[sum_count == 0, cluster := "All should be boring"]
matchIndexDt[sum_count > 1, cluster := "Need multiple checks"]
matchIndexDt[grepl(" A | B ", title), cluster := "Is AB"]
matchIndexDt[grepl("DR", cluster), cluster := "Miscellaneous checks"]

matchIndexDt[, cluster := paste0("Title matching; ", cluster)]
# Bringing over title to fix A/B ordering
indexDt2 <- buildProjectIndexDt(matchIndexDt, startCount = 31) %>%
  merge(storePogDt[, .(PogId = pogId, title)] %>% unique) %>% 
  .[order(nchar(gsub(" ", "", title)))] %>%
  .[grepl("Is AB", ProjectId), Seq := seq_len(.N)] %>%
  .[, title := NULL]

indexDt <- rbind(indexDt1, indexDt2)

indexDtVerbose <- merge(indexDt[, .(pogId = PogId, ProjectId, Seq)],
                        storePogDt[,.(store_count = uniqueN(storeId)), 
                                   by =  .(pogId, title, width, depth, height)] %>% unique)

if (nrow(indexDt[!(PogId %in% keywordsThatNeedFiles), .N, by = PogId][N>1])>0) {
  print(warning("INVESTIGATE INDEX"))
}

if (FALSE) {
  indexDt %>%  fwrite(file.path(config$filesDir, "dessertsIndex1.csv"))
}



#######################################################################################################################
#                                                AUTOFILL ROUND 2                                                     #
#######################################################################################################################

# Split out desserts POGs -- we work those first
autofillDt <- fread(file.path(config$filesDir, "anchorMap2.csv")) %>%
  .[, .(pogId, autoId)] %>% unique %>%
  .[pogId != "J288AKO"]  %>%
  merge(copy(storePogDt)[, storeId := NULL] %>% unique, all.x = TRUE) %>% 
  .[, is_anchor := fifelse(pogId == autoId, 1, 0)]
if (autofillDt[is.na(title), .N] > 0) {
  print("Theres atuofilldts not in the storepogdt")
}

# Manually add regional brands to title per store lists
autofillDt[pogId %in% kempsPogs, title := paste0(title, ":KEMPS")]
autofillDt[pogId %in% mccPogs, title := paste0(title, ":MCC")]

# Split out anchor DTs into their own table
anchorsDt <- autofillDt[is_anchor == 1, .(anchor_title = title,
                                          pogId)] %>% unique()
setkey(anchorsDt, pogId)
setkey(autofillDt, autoId)
autofillDt <- autofillDt[anchorsDt]
autofillDt[, N := .N, by = autoId]

noCareTags <- c("BDGT", "EXP", "FBSE", 
                "FRSK", "GM", "HIDIGI", "MNSTRM", 
                "PF", "SBFM", "SBRB", "TFL", "TMD", 
                "W/ DESSERT 2DR",
                "W/ DESSERT/ICE 2DR", "ICDTST", "TTX", "TOH")

autofillDt[, base_bespoke_tags := tagsUniqueToTitle(title, anchor_title, noCareTags = noCareTags)]
autofillDt[, anchor_bespoke_tags := tagsUniqueToTitle(anchor_title, title, noCareTags = noCareTags)]


# ICDTST needs to be its own batch, no matter what
autofillDt[grepl("ICDTST", title), ':='(base_bespoke_tags = "ICDTST", anchor_bespoke_tags = "")]

# AFM and ASN only matter for certain shelf sizes -- erase them from others
doorNames <- paste0(1 : 30, "DR") %>% c("5 DR") # have to add "5 DR" because of an incorrectly named pog in IC 2023
for (doorName in doorNames)  {
  a <- autofillDt[grepl(doorName, title)]
  if (nrow(a) > 0) {
    autofillDt[grepl(doorName, title), door_count := doorName %>% gsub("DR", "", .) %>% as.numeric]
  }
}
# AFM Rule - If AFM in title and < 9DR, don't add 
autofillDt[door_count < 9 & grepl("AFM", base_bespoke_tags),
           base_bespoke_tags := cleanseKeywordFromTag(base_bespoke_tags,
                                                      keyword = "AFM")]

# ASN RULE - like AFM but specific doors per region
# TIA - ASN 9 doors and Up 
autofillDt[!(door_count >= 9 & grepl("TIA", title)) |
             (door_count >= 10 & grepl("[TMD|TOH]", title)) |
             (door_count >= 11 & grepl("[CSW|TCA|TTX]", title)) & 
             grepl("ASN", title), base_bespoke_tags := cleanseKeywordFromTag(base_bespoke_tags,
                                                                             keyword = "ASN")]

# BLBL RULE FOR SUPER TARGETS 
# BLBLNOV ADDED TO SUPER TARGETS ALSO
autofillDt[(pogId %in% stPogs), ':='(base_bespoke_tags = gsub("BLBL", "BLBLNOV", base_bespoke_tags),
                                     anchor_bespoke_tags = gsub("BLBL", "BLBLNOV", anchor_bespoke_tags))]

# TMOK RULE - if tillamook is on the pog but not on the title, don't remove it
autofillDt[grepl("TMOK", anchor_bespoke_tags), 
           anchor_bespoke_tags := cleanseKeywordFromTag(anchor_bespoke_tags, 
                                                        keyword = "TMOK")]

allBaseTags <- autofillDt[, table(lapply(base_bespoke_tags, strsplit, split = "_") %>% unlist)] %>%
  as.data.table %>% .[, .(shouldHave = N), keyby = .(tag = V1)] %>%
  merge(autofillDt[, table(lapply(anchor_bespoke_tags, strsplit, split = "_") %>% unlist)] %>%
          as.data.table %>% .[, .(shouldNotHave = N), keyby = .(tag = V1)], all = TRUE)

autofillDt[, .(num_pogs = .N), keyby = .(needs_this_done = base_bespoke_tags,
                                         needs_this_undone = anchor_bespoke_tags)] %>%
  .[order(num_pogs)]

autofillDt[, project_title:= fifelse(nchar(base_bespoke_tags) == 0, "", 
                                     paste0("Should have ", paste(strsplit(base_bespoke_tags, "_")[[1]], collapse = ", "))) %>%
             paste0(fifelse((nchar(anchor_bespoke_tags) * nchar(base_bespoke_tags)) > 0, "; ", "")) %>%
             paste0(fifelse(nchar(anchor_bespoke_tags) == 0, "", 
                            paste0("Should NOT have ", paste(strsplit(anchor_bespoke_tags, "_")[[1]], collapse = ", "), ""))),
           by = .(base_bespoke_tags, anchor_bespoke_tags)]

# We need to build two indexDts separately
# first one - where there's a title mismatch
indexDtNoMasters <- autofillDt[project_title != "", .(PogId = pogId, project_title)]

# Build skeleton table of these project names since we need to add the templates
projectSkeleton <- indexDtNoMasters[, .N, keyby = project_title] %>%
  .[, num := formatC(seq_len(.N), width = 3, flag = "0")] %>% 
  .[, ProjectId := paste0(num, " - ", project_title)]

# build templatesForIndexDt that will 
templatesForIndexDt <- data.table(PogId = character(), ProjectId = character())
for (keyword in keywordsThatNeedFiles) {
  keywordDt <- copy(projectSkeleton) %>% 
    .[, .(ProjectId, PogId = keyword)] %>% 
    .[grepl(keyword, ProjectId)]
  templatesForIndexDt <- rbind(templatesForIndexDt, keywordDt)
}
indexDt1 <- rbind(merge(indexDtNoMasters, projectSkeleton, by = "project_title") %>% .[, .(PogId, ProjectId)],
                  templatesForIndexDt) %>%
  .[order(ProjectId, 1 * nchar(gsub("[A-Z]", "", PogId)))] %>% 
  .[, Seq := seq_len(.N), by = ProjectId]


# Second one - where there's no title mismatch
matchIndexDt <- copy(autofillDt)[project_title == ""]

# Build clusters for these in a super generic way then override manually what we want
clusterNames <- paste0(1 : 100, "DR") %>% c("5 DR")
for (clusterName in clusterNames)  {
  a <- matchIndexDt[grepl(clusterName, title)]
  if (nrow(a) > 0) {
    matchIndexDt[grepl(clusterName, title), cluster := clusterName]
  }
}
matchIndexDt[cluster %in% c("5DR", "5 DR"), cluster := "5DR"]
matchIndexDt[cluster %in% c("11DR", "12DR", "9DR"), cluster := "9DR and up"]
matchIndexDt[cluster %in% c("1DR", "2DR"), cluster := "1DR and 2DR"]

matchIndexDt[, sum_count := 0]
for (keyword in keywordsThatNeedFiles %>% leftOnly(c("KEMPS", "MCC"))) {
  matchIndexDt[grepl(keyword, title), sum_count := sum_count + 1]
}
matchIndexDt[sum_count == 0, cluster := "All should be boring"]
matchIndexDt[sum_count > 1, cluster := "Need multiple checks"]
matchIndexDt[grepl("A |B ", title), cluster := "Is AB"]
matchIndexDt[grepl("DR", cluster), cluster := "Miscellaneous checks"]

matchIndexDt[, cluster := paste0("Title matching; ", cluster)]
# Bringing over title to fix A/B ordering
indexDt2 <- buildProjectIndexDt(matchIndexDt, startCount = indexDt1[, uniqueN(ProjectId)] + 1) %>%
  merge(storePogDt[, .(PogId = pogId, title)] %>% unique) %>% 
  .[order(nchar(gsub(" ", "", title)))] %>%
  .[grepl("Is AB", ProjectId), Seq := seq_len(.N)] %>%
  .[, title := NULL]

indexDt <- rbind(indexDt1, indexDt2)

indexDtVerbose <- merge(indexDt[, .(pogId = PogId, ProjectId, Seq)],
                        storePogDt[,.(store_count = uniqueN(storeId)), 
                                   by =  .(pogId, title, width, depth, height)] %>% unique)

if (nrow(indexDt[!(PogId %in% keywordsThatNeedFiles), .N, by = PogId][N>1])>0) {
  print(warning("INVESTIGATE INDEX"))
}

if (FALSE) {
  # Only give pog hub BLBL POGs with Names "...NOT .. BLBL"
  toughBlblRows <- intersect(grep("BLBL", indexDt$ProjectId),
                             grep("BLBL", gsub("NOT.*", "", indexDt$ProjectId)))
  
  
  toughRows <- c(toughBlblRows,
                 grep("BLBN", indexDt$ProjectId),
                 grep("ICDTST", indexDt$ProjectId),
                 grep("Title matching", indexDt$ProjectId)) %>% unique
  
  indexDtForMe     <- indexDt[toughRows]
  indexDtForPogHub <- indexDt[!toughRows]
  
  indexDt %>%  fwrite(file.path(config$filesDir, "theRestIndex.csv"))
  indexDtForMe %>% fwrite(file.path(config$filesDir, "theRestIndexForMe.csv"))
  indexDtForPogHub %>%  fwrite(file.path(config$filesDir, "theRestPogHubIndex.csv"))
  
  indexDtTST <- indexDtForMe[grepl("ICDTST", ProjectId)] %>%
    .[, ProjectId := paste0(as.character(ceiling(Seq / 25)), " - ICDTST")] %>%
    .[, Seq := seq_len(.N), by = .(ProjectId)]
  indexDtTST %>% fwrite(file.path(config$filesDir, "icdtstIndex.csv"))
  
  # Prep for file
  indexForFile <- merge(indexDtForPogHub, storePogDt[, .(store_count = uniqueN(storeId)),
                                                     by = .(PogId = pogId, title,
                                                            width, depth, height)] %>% unique,
                        by = "PogId", all.x = TRUE) %>%
    .[order(ProjectId, Seq)]
  
  # Run comparisons
  indexDtForMe[, .N, by = ProjectId] %>% .[!grepl("ICDTST|Title matching", ProjectId), summary(N)] 
  
}


if (FALSE) {
  
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
  
  outputPerformanceLibrary <- newPerformanceLibrary[, .(`Planogram Alias` = pogId,
                                                        `POG Name` = "",
                                                        ID = positionId,
                                                        `Value 39` = coalesced_upspw * 52 * store_count,
                                                        `Value 38` = coalesced_dpspw * 52 * store_count,
                                                        `Nbr Location` = store_count,
                                                        `Nbr Weeks` = 52,
                                                        `Unit Movement` = coalesced_upspw,
                                                        `Value 40` = coalesced_dpspw)]
  if (FALSE) {
    fwrite(outputPerformanceLibrary, file.path(config$filesDir, "output_performance_library_12_22.csv"))
    writexl::write_xlsx(x = list(results0 = outputPerformanceLibrary),
                        file.path(config$filesDir, "output_performance_library_12_22.xlsx"))
  }
      # Check 1: upspw NA's are items that weren't on POG in pre period
  missingMovement <- newPerformanceLibrary[is.na(upspw), .(pogId, positionId)] %>% unique %>%
    merge(storePogDt[, .(storeId, pogId)] %>% unique, by = "pogId", allow.cartesian = TRUE) %>%
    .[, .(storeId, positionId)] %>% unique
  
                                            
  # Check2: item_upspw NA's are all New items with no sales history ON POG
}


#######################################################################################################################
#                                                OUTPUT CONFIGS                                                       #
#######################################################################################################################
if (FALSE) {
  condConfigs <- lapply(configs, FUN = function(x) x[PogId %in% condPogs])
  fwrite(condConfigs$insertsConfig, file.path(config$configsDir, "cond_inserts_config.csv"))
  fwrite(condConfigs$deletesConfig, file.path(config$configsDir, "cond_deletes_config.csv"))
  fwrite(condConfigs$swapsConfig, file.path(config$configsDir, "cond_swaps_config.csv"))
  fwrite(condConfigs$facesConfig, file.path(config$configsDir, "cond_faces_config.csv"))
}

##########################################################################################################################
#                    INDEX CONFIG                    ##                         INDEX CONFIG                             #
##########################################################################################################################


