setwd("~/r_code")
source("00_initialize.R")


# read in the tie report + clean
setwd(config$baseDir)

# Save tie report
uncleanTieReport <- fread(config$rawTieReportPath)
tieReportOut <- cleanTieReport(rawTieReport = uncleanTieReport, assignUncleanNames = TRUE, inDateFormat = "%m/%d/%Y")
if (FALSE) {
  fwrite(cleanNames(tieReportOut), config$cleanTieReportPath)
}
tieReportDt <- fread(config$cleanTieReportPath) %>% cleanNames
nonStores <- leftOnly(getStoreListDt()$store, tieReportDt$store)

# Save index report
indexReportDt <- createIndexReport(tieReportDt)
if (FALSE) {
  fwrite(indexReportDt, config$cleanIndexReportPath)
}

# Create batches and save down index config from index report
indexConfig <- copy(indexReportDt)
indexConfig[, c("width", "height", "depth") := tstrsplit(size, "x")]
indexConfig[, c("width", "height", "depth") := lapply(.SD, FUN = as.numeric),
            .SDcols = c("width", "height", "depth")]


indexConfig[, cluster := as.character(NA)]
clusterNames <- c("MCM", "URB")
for (clusterName in clusterNames) {
  indexConfig[grepl(pattern = clusterName, x = title), cluster := clusterName]
}
indexConfig[is.na(cluster), cluster := "REG"]
indexConfig[a_b_pog != "", ':='(cluster = "AB POGS", batch = "AB POGS")]


indexConfig[height < 60, batch := paste0(cluster, " HALF HEIGHTS")]

indexConfig[, size_cluster := paste0(pmax(pmin(round(width / 4) * 4, 28), 8), "FT")]
indexConfig[size_cluster == "28FT", size_cluster := "28FT+"]

indexConfig[cluster %in% c(clusterNames, "REG") & height >= 60, batch := paste0(cluster, " ", size_cluster)]

# order batches, making half height + AB pogs come at the end
orderedBatches <- indexConfig[order(cluster, width), .(batch = unique(batch))] %>%
  .[, Seq := seq_len(.N)] %>%
  .[batch == "MCM HALF HEIGHTS", Seq := 99] %>%
  .[batch == "REG HALF HEIGHTS", Seq := 999] %>%
  .[batch == "URB HALF HEIGHTS", Seq := 9999] %>%
  .[batch == "AB POGS", Seq := 99999] %>%
  .[order(Seq), Seq := seq_len(.N)] %>%
  .[, ProjectId := paste0(formatC(Seq, width = 2, flag = "0"), " - 3.5.23 TNT ", batch)] %>%
  .[order(Seq)] %>%
  setnames(old = "Seq", new = "ProjectOrder")

indexConfigWithBatches <- merge(orderedBatches, indexConfig, by = "batch") %>%
  .[order(ProjectOrder, width)]

indexConfigWithBatches[, Seq := seq_len(.N), by = ProjectId]

# prescribe the order of AB POGs
indexConfigWithBatches[grepl("AB POGS", ProjectId), a_b_seq := substring(title, 14, 16)]
indexConfigWithBatches[grepl("AB POGS", ProjectId), a_b_seq_flag := fifelse(.N > 1, 1, 0), 
                       by = .(a_b_seq, a_b_pog)]
indexConfigWithBatches[a_b_seq_flag == 1, a_b_seq := paste0(a_b_seq, nchar(title))]
indexConfigWithBatches[grepl("AB POGS", ProjectId), Seq := order(paste0(a_b_seq, a_b_pog))]
indexConfigWithBatches[order(a_b_seq, a_b_pog), abSeq := seq_len(.N)]
indexConfigWithBatches[grepl("AB POGS", ProjectId), Seq := abSeq]


# COERCE TWO BATCHES - Both arent in CKS so only add them to master document + flag
# 1. two_pogs_tied_non_ab: pogs exclusively in stores with multiple ties - might not have to work these
two_pogs_tied_non_ab <- read_clip()

exclusives <- leftOnly(two_pogs_tied_non_ab,
                       indexConfigWithBatches[display %in% two_pogs_tied_non_ab, unique(display)])
indexConfigWithBatches[display %in% exclusives, ProjectId := "24 - 3.5.23 TNT NON_AB DUPE EXCLUSIVES"]

# 2. missing autofills
missing_autofills <- read_clip()
indexConfigWithBatches[display %in% missing_autofills, ProjectId := "23 - 3.5.23 TNT MISSING AUTOFILLS"]
indexConfigWithBatches[, Seq := seq_len(.N), keyby = ProjectId]
indexConfigWithBatches <- indexConfigWithBatches[order(ProjectId, Seq)]
indexConfigWithBatches$total_sort <- 1 : NROW(indexConfigWithBatches)
# Prep for output 
indexConfigWithBatches <- indexConfigWithBatches[order(ProjectId, Seq)]
indexConfigWithBatches[, ':='(sorting_order = Seq, total_sort = seq_len(.N),
                              batch = ProjectId)]
originalNames <- names(indexReportDt)
indexConfigOutForMasterDocument <- indexConfigWithBatches[, ..originalNames] %>%
  .[order(total_sort)]
indexConfigOutForAutomation <- indexConfigWithBatches[, .(PogId = display, ProjectId, Seq)]
if (FALSE) {
  fwrite(indexConfigOutForAutomation,
         file.path(config$configsDir, "indexConfig.csv"))
}


if (FALSE) {
  # Friday, 11/4/2022
  # Adding in new item in TNT
  newItem <- 037050175
  adjItem1 <- 37120459
  adjItem2 <- 37122987
  
  storePogDt <- fread(config$storePogDtPath)
  latestPositionDt <- fread(file.path(config$configsDir, "position_table_pre_item_add.csv")) %>%
    .[, items_on_shelf := uniqueN(positionId), by = .(pogId, fixtureId)] %>%
    .[, max_taken_on_shelf := max(positionTargetSpaceX), by = .(pogId, fixtureId)] %>%
    .[, subtractable_space := sum(positionTargetSpaceX - 6), by = .(pogId, fixtureId)]
  
  
  # we want to put it in 500 stores
  # Store count of adjacent item1 or 2 > 500? great! 
  candidatePogs <- latestPositionDt[positionId %in% c(adjItem1)] %>%
    .[items_on_shelf < 6 & subtractable_space > 6, pogId] %>% unique
  
  candidatePogsDt <- storePogDt[pogId %in% candidatePogs, uniqueN(storeId) , by = pogId] %>%
    .[order(-V1)] %>%
    .[, cumsum := cumsum(V1)] %>%
    .[cumsum <= 500]
  
  indexOut <- data.table(PogId = candidatePogsDt$pogId, BatchId = "Adding_Burt_Item") %>%
    .[, Seq := seq_len(.N)]
  fwrite(indexOut, file.path(config$configsDir, "indexForItemAdd.csv"))
  
  # Filter to the POGs that item is in
  # For all the shelves chosen item appears on, filter to POGs that have fewer than 6 items on the shelf
  # still have 500 store count? Great!
}





