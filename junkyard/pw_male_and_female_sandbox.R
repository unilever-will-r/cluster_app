rm(list = ls(all.names = TRUE)); gc();
setwd("~/r_code")
source("00_initialize.R")


config <- initializeProject(configPath = "~/r_code/config.yaml")

baseDir <- "C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/08 - Personal Wash/2024/"
dataDir <- file.path(baseDir, "data_files")
pogDir <- file.path(baseDir, "pogs", "collected")

uncleanTies <- fread(file.path(dataDir, "d49_ties_10022023.csv"))
catTies <- uncleanTies %>%
  cleanNames %>%
  .[grepl("SOAP|FEMALE", title)] %>%
  .[!grepl("LIQUID|SHAVE|DEODORANT", title)] %>%
  buildTieReport()

mainIndex <- catTies[, .(store_count = uniqueN(store)), keyby = .(pogId = display, 
                                                                  title = title,
                                                                  width = width,
                                                                  height = height,
                                                                  pog_size = pog_size)]
                                                                  



indexDt <- fread(file.path(dataDir, "index_w_template.csv")) %>%
  .[, .(workingId = display,
        templateId = paste0("T", substr(auto_fill_with_, 2, 7)),
        width = size_,
        size = size,
        title,
        flag = mg_)]


itemLookupIndex <- merge(mainIndex,
                         indexDt[, .(pogId = workingId, flag, templateId)],
                         all.x = TRUE,
                         by = "pogId")
itemLookupIndex[is.na(templateId), ':='(templateId = "NA - COMBO", flag = "COMBO")]

clusterNames <- c("NATS", "AFAM", "AFAM2", "PRM", "HSP")
for (clusterName in clusterNames) {
  itemLookupIndex[grepl(clusterName, title), cluster := clusterName]
}
itemLookupIndex[flag == "COMBO", cluster := "COMBO"]
itemLookupIndex[is.na(cluster), cluster := "UNCLUSTERED"]
itemLookupIndex[, gender := ""]
itemLookupIndex[grepl("MALE", title), gender := "MALE"]
itemLookupIndex[grepl("FEMALE", title), gender := "FEMALE"]



pogDt      <- fread(file.path(dataDir, "both_planogram_table.csv"))
fixtureDt  <- fread(file.path(dataDir, "both_fixture_table.csv"))
positionDt <- fread(file.path(dataDir, "both_position_table.csv"))

fixtureDt <- merge(fixtureDt, positionDt[, .(pogId, fixtureId, has_items = 1)] %>% unique,
                   by = c("fixtureId", "pogId"), all.x = TRUE)


templateNrbDt <- fixtureDt[substr(pogId, 1,1) == "T"][fixtureColor == 33023]
workingNrbDt <- fixtureDt[substr(pogId, 1, 1) != "T"][fixtureColor == 33023]


indexDt <- merge(indexDt, templateNrbDt[, .(nrb_temp_count = .N), keyby = .(templateId = pogId)],
               by = "templateId", all.x = TRUE) %>%
  merge(workingNrbDt[, .(nrb_work_count = .N), keyby = .(workingId = pogId)],
        by = "workingId", all.x = TRUE)

# classify matches by indexDt
# 2: number of NRB fixtures matches that of template
# 1: number of NRB fixtures on working > template

smDt <- createNrbMap(indexDt, workingNrbDt, templateNrbDt)
smIndex <- buildIndexOffShelfMap(smDt, copy(indexDt)[, display := workingId])

if (FALSE){
  fwrite(smDt, file.path(dataDir, "smDt.csv"))
  fwrite(undoneSmIndex, file.path(dataDir, "undoneSmIndex.csv"))
}

# Things I still need to check
# 0: package the POGs + re-pull data
indexDt0 <- smIndex[PogId %in% indexDt$workingId, .(PogId)]
indexDt0[, Seq := 1 + (seq_len(.N) %% 100)]
indexDt0[, ProjectId := paste0("project_", ceiling(seq_len(.N) / 100))]
if (FALSE) {
  fwrite(indexDt0, file.path(dataDir, "indexDt0.csv"))
}

pDt0      <- fread(file.path(dataDir, "index0_planogram_table.csv"))
fDt0  <- fread(file.path(dataDir, "index0_fixture_table.csv"))
posDt0 <- fread(file.path(dataDir, "index0_position_table.csv"))


# 1: NRB's with NO backer paper
nrbCount <- fDt0[fixtureColor == 33023, .(pogId, fixtureId)] %>% unique
nrbPositionDt <- merge(nrbCount, posDt0, all.x = TRUE)
noPaperPogs <- nrbPositionDt[is.na(positionId), unique(pogId)]

indexDt1 <- copy(indexDt0)[PogId %in% noPaperPogs, ProjectId := "need_backer_paper"]
indexDt1[ProjectId == "need_backer_paper", Seq := seq_len(.N)]
if (FALSE) {
  fwrite(indexDt1, file.path(dataDir, "indexDt1.csv"))
}
# 3: the POGs that weren't pulled from CKS initially


