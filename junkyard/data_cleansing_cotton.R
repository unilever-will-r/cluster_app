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
  setnames(old = "planogram_desc2", new = "display")

# displays that aren't in pogData aren't in cks
notInCks <- leftOnly(tieReportDt[, unique(display)], pogData[, unique(display)])
# displays in pog data not in store tie report we can ignore

# filter to display universe of displays in both files
displayUniverse <- intersect(pogData[, unique(display)], tieReportDt[, unique(display)])


if (FALSE) {
  # Cotton 2023 transition
  configs <- initializeConfigs()
  
  # 1. 
  delete1 <- 052140555 # cotton balls, deleting but could swap 052140901 but DOS makes it tricky
  pogs1 <- pogData[id == delete1, unique(display)]
  configs <- updateConfigsDelete(configs = configs, 
                                 actionPogs = pogs1, 
                                 deleteItem = delete1)
  # 2.
  delete2 <- 052140051 # qtip 170 ct
  swapIn2 <- 052140530 # qtip 750, swapping for
  pogs2 <- pogData[id == delete2, unique(display)]
  configs <- updateConfigsSwap(configs = configs, 
                               actionPogs = pogs2, 
                               existingItemId = delete2, 
                               newItemId = swapIn2)
  
  # 3. 
  
  # new item (375 ct) - need to insert next to items until we have an exhaustive list
  # try tier list - 300 ct 052140202, 500 ct052140203, 625 052140575 
  # for each one, tick off the stores it captures and increment tieReport to just the leftovers, then filter
  # pogdata to the leftover pogs
  new1 <- 052140006
  priorityItems <- c(052140202, 052140203, 052140575)
  configs <- updateConfigsSwapItemEverywherePegboard(configs = configs,
                                                     pogData = pogData,
                                                     tieReportDt = tieReportDt, 
                                                     newItem = new1,
                                                     priorityItems = priorityItems, completeSwap = FALSE)
  
  
  # 4. - just colors on product library
  # 0575 red, 
  # we aren't going to automate deletion of 052140575, color red instead
  # also color 375 yellow, removing its facings
  # we can color the new item green say to block it good
  
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
