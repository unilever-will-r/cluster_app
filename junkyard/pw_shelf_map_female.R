# Shelf Map Script for Male PW 2024
dataDir <- "C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/08 - Personal Wash/2024/data_files/female"


# indexDt: different from shelf map.
indexDtWithTemplate <- fread(file.path(dataDir, "index_w_template.csv")) %>%
  .[, auto_fill_with_ := gsub("J049", "T049", auto_fill_with_)]


# template tables
templateDt <- fread(file.path(dataDir, "anchors_female_planogram_table.csv"))
templateFixtureDt <- fread(file.path(dataDir, "anchors_female_fixture_table.csv"))
templateItemDt <- fread(file.path(dataDir, "anchors_female_position_table.csv"))

templateFixtureDt <- merge(templateFixtureDt, templateItemDt[, .(fixtureId, pogId, has_items = 1)] %>% unique, by = c("pogId", "fixtureId"), all.x = TRUE)

# working pog tables
workingDt <- fread(file.path(dataDir, "working_female_planogram_table.csv"))
workingFixtureDt <- fread(file.path(dataDir, "working_female_fixture_table.csv"))
workingItemDt <- fread(file.path(dataDir, "working_female_position_table.csv"))

workingFixtureDt <- merge(workingFixtureDt, workingItemDt[, .(fixtureId, pogId, has_items = 1)] %>% unique, by = c("pogId", "fixtureId"), all.x = TRUE)

# Add template pogId to indexDt
# cols: workingId, templateId
workingToTemplate <- indexDtWithTemplate[, .(workingId = display,
                                             templateId = auto_fill_with_)] %>% unique


# Check for strength of match
workingToTemplate[, match_strength := mapply(workingId, templateId, FUN = function(x, y) checkForStrongMatch(x, y, workingFixtureDt, templateFixtureDt)), by = .(workingId, templateId)]

# build the actual shelf map
shelfMapDt <- createShelfMap(workingToTemplate, workingFixtureDt, templateFixtureDt)


# Create teh custom index DT
# (autofill_name)_(match_flag)
# first POG is autofill, rest of POGs are its matches
shelfMapIndex <- buildIndexOffShelfMap(shelfMapDt, indexDt = indexDtWithTemplate)

shelfMapIndex[, N := seq_len(.N)]
projectsDt <- shelfMapIndex[, .(max_N = max(N)), by = ProjectId]

a2_projects <- projectsDt[max_N < 500][grepl("_2_", ProjectId), ProjectId]
b2_projects <- projectsDt[max_N >= 500][grepl("_2_", ProjectId), ProjectId]


if (FALSE) {
  fwrite(shelfMapDt, file.path(dataDir, "shelfMapDt.csv"))
  fwrite(shelfMapIndex[ProjectId %in% c2_projects],
         file.path(dataDir, "shelfMapIndex_c2.csv"))
f}


# ad-hoc
pogTracker <- merge(indexDtWithTemplate, shelfMapIndex[, .(display = PogId, Batch = ProjectId, Seq)], by = "display", all.x = TRUE)
pogTracker[!(display %in% workingDt$pogId), Batch := "ZPOG FILE NOT FOUND"]

pogTracker[, .(display, checked_out_by = "", stores_tied, sorting_order = Seq, total_sort = seq_len(.N), Batch, size, title)] %>% write_clip

for (colName in names(sheet1)) {
  sheet1col <- sheet1[, ..colName]
  sheet2col <- sheet2[, ..colName]
  print(colName)
  print(setdiff(sheet1col, sheet2col))
}

# inTracker
# needDrawing 
# turnedIn