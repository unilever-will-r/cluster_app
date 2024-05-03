setwd("~/r_code")
source("00_initialize.R")


# read in the tie report + clean
setwd(config$baseDir)


# Creating mapping from POG to template pog
pogsNeedingTemplates <- uncleanTieReport[, .(pogId = planogram_id, 
                                             title = planogram_description,
                                             width = planogram_width / 12,
                                             height = planogram_height)] %>% unique
clusterNames <- c("AFAM1", "AFAM2", "PRM", "NATS", "HSP1", "HSP2", "URBAN")
for (clusterName in clusterNames) {
  pogsNeedingTemplates[grepl(clusterName, title), cluster := clusterName]
}

pogsNeedingTemplates[, is_ab := ""]
pogsNeedingTemplates[grepl("0[0-9][0-9]A", title), is_ab := "A"]
pogsNeedingTemplates[grepl("0[0-9][0-9]B", title), is_ab := "B"]
pogsNeedingTemplates[grepl("0[0-9][0-9]C", title), is_ab := "C"]

pogsNeedingTemplates[, width_of_template := max(4, ceiling(width / 4) * 4), by = width] %>%
  .[, .N, keyby = .(cluster, width_of_template, is_ab)] %>% 
  .[order(cluster, is_ab, width_of_template)]




# THIS SCRIPT SHOULD MAKE
# 1. clean tie report, clean index report
# 2. storePogDt, clean attribution Dt from product and performance library
uncleanTieReport <- fread(config$rawTieReportPath)
storePogDt <-  uncleanTieReport %>%
  cleanNames %>%
  cleanTieReport(inDateFormat = "%m/%d/%Y") %>%
  .[, .N, keyby = .(storeId = store, pogId = display)] %>% .[, N := NULL] %>% unique
fwrite(storePogDt, config$storePogDtPath)

# 3. Diagnostic output that shows
#     - missing stores
#     - missing pogs
#     - missing attribution
#     - 






# Save tie report

tieReportOut <- cleanTieReport(rawTieReport = uncleanTieReport, assignUncleanNames = TRUE)

nonStores <- leftOnly(getStoreListDt()$store, tieReportDt$store)
if (FALSE) {
  fwrite(tieReportOut, config$cleanTieReportPath)
}

tieReportDt <- fread(config$cleanTieReportPath) %>% cleanNames
# We exlude these stores until we have clarity about their duplicates
badStores <- c(324, 732, 926, 1406, 1462, 1815, 1984, 2032, 2584, 2586)
tieReportDt <- tieReportDt[!(store %in% badStores)]
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
  fwrite(indexReportDt, config$cleanIndexReportPath)
  fwrite(indexConfigOutForAutomation,
         file.path(config$configsDir, "indexConfig.csv"))
}
