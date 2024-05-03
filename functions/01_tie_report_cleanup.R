library(data.table)
library(magrittr)

addDimensionCols <- function(dt, 
                            size_col_name = "pogSize") {
  requiredColumnNames <- size_col_name
  checkRequiredColumnNames(dt, requiredColumnNames = requiredColumnNames)
  
  setnames(dt, old = size_col_name, new = "size_col")
  
  formatCheck <- function(dt) {
    # Check to ensure the input column is formatted as expected
    formatFlag <- dt[, .(check = sum(3 != length(strsplit(size_col, split = "x ")[[1]]))), by = size_col] %>%
      .[, sum(check)]
    if (formatFlag != 0) {
      stop("something is up with size_col_name column's format")
    }
    return(invisible(NULL))
  }
  
  formatCheck(dt)
  dt[, c("width", "height", "depth") := lapply(1 : 3, FUN = function(x) {
    gsub("x ", "", x = size_col) %>% strsplit(x = ., split = " ") %>% 
      `[[`(1) %>% `[`(x) %>% as.integer}),
    by = size_col]
  
  setnames(dt, old = "size_col", new = size_col_name)
  return(dt)
}

buildPodDt <- function(positionDt, 
                       storePogDt,
                       attributionDt,
                       attCols = c("item_description", "brand")) {
  # Item universe: attributionDt
  # Store/pog universe: inner join of storePogDt dt x positionDt
  requiredCols <- unique(c("positionId", attCols))
  attributionDt <- attributionDt[, ..requiredCols]
  podDt <- merge(positionDt, storePogDt, by = "pogId", allow.cartesian = TRUE) %>% 
    .[, .(store_count = uniqueN(storeId)), by = positionId] %>%
    merge(attributionDt)
  return(podDt)
}


buildTieReport <- function(rawTieReport, 
                           dateFilter = "2999-01-01",
                           inDateFormat = "%d-%b-%Y",
                           assignUncleanNames = FALSE) {
  
  dateFormat <- inDateFormat;
  dateWindow <- 28
  dateFilter <- as.Date(dateFilter)
  
  requiredColumnNames <- c("store", "display", "set_date", "discontinue_date", "title", "pog_size")
  checkRequiredColumnNames(rawTieReport, requiredColumnNames)
  rawTieReport <- rawTieReport[, ..requiredColumnNames]
  
  rawTieReport[, ':='(set_date  = as.Date(set_date, format = dateFormat),
                      discontinue_date = as.Date(discontinue_date, format = dateFormat))]
  
  rawTieReport <- rawTieReport[set_date < dateFilter]
  

  
  # Anchor set date is assumed to be the one that is the furthest date in the future for the most stores
  anchorSetDate <- rawTieReport %>%
    .[, .(set_date = max(as.Date(set_date))), by = store] %>%
    .[, .N, by = set_date] %>%
    .[order(-N), set_date] %>% 
    .[1]
  rawTieReport[, anchor_set_date := anchorSetDate]
  
  # For each store, figure out the nearest set date to anchor
  rawTieReport[, days_from_anchor := set_date - anchor_set_date, by = set_date]
  storeSkeleton <- rawTieReport[, .SD[which.min(abs(days_from_anchor))], by = .(store)] %>%
    .[, .(store, set_date, dist_from_anchor = abs(days_from_anchor))] %>% unique()
  
  # Join store skeleton back to ties to join A/B POGs
  rawTieReport <- merge(storeSkeleton, rawTieReport, by = c("store", "set_date")) %>%
    .[, in_window_flag := fifelse(dist_from_anchor <= dateWindow, 1, 0), by = dist_from_anchor] %>%
    .[in_window_flag == 1]
  
  # Extract dimensions as numeric columns
  rawTieReport <- addDimensionCols(rawTieReport, "pog_size")
  # rawTieReport[, c("width", "height", "depth") := tstrsplit(pog_size, "x")] %>%
  #   .[, c("width", "height", "depth") := lapply(.SD, FUN = as.numeric),
  #     .SDcols = c("width", "height", "depth")]
  # 
  # Remove unnecessary columns
  rawTieReport[, c("in_window_flag", "anchor_set_date", "days_from_anchor") := NULL]
  
  if (assignUncleanNames) {
    oldNames <- c( "dept", "subgroup", "display", "original_display",
                   "revision", "title", "pog_size", "store", "state", "city",
                  "set_date", "discontinue_date",
                  "new_remodel_code", "exposures")
    
    newNames <- c( "DEPT", "SUBGROUP", "DISPLAY", "ORIGINAL DISPLAY", 
                   "REVISION", "TITLE", "POG SIZE", "STORE", "STATE", "CITY",
                   "SET DATE", "DISCONTINUE DATE",
                   "NEW REMODEL CODE", "EXPOSURES")
    
    addCols <- leftOnly(oldNames, names(rawTieReport))
    rawTieReport[, (addCols) := ""]
    setnames(rawTieReport, oldNames, newNames)
    rawTieReport <- rawTieReport[, ..newNames]
  }
  
  return(
    rawTieReport
  )
}

cleanseTableNames <- function(hygieneTable, uncleanDt) {
  # Maintains consistent structure across similar tables
  # Makes sure things like store/storeId/STORE/storeNbr get consistent naming conventions
  newNames <- names(hygieneTable)
  oldNames <- newNames
  for (i in 1 : length(hygieneTable)) {
    newName <- newNames[i]
    candidateOlds <- hygieneTable[[i]]
    oldNamePool <- intersect(candidateOlds, names(uncleanDt))
    if (length(oldNamePool) == 0 ) {
      stop(paste0("Cant find valid ", newName, " column in the table."))
    } else {
      oldNames[i] <- oldNamePool[1]
    }
  }
  ok <- lapply(hygieneTable, FUN = function(x) {
    newNames <- names(x)[1]
    newNames
  })
  output <- copy(uncleanDt)
  setnames(output, old = oldNames, new = newNames)
}


cleanseTieReport <- function(uncleanDt) {
  # de-ambiguate key variables in tie reports
  renameList <- list(storeId = c("store", "storeId", "STORE", "storeNbr", "storeid"),
                     pogId = c("planogram_desc2", "pogId", "display", "pogid"),
                     pogSize = c("pog_size", "POG SIZE", "pog.size", "pogsize"),
                     title = c("title", "TITLE"),
                     set_date = c("SET DATE", "set.date", "set_date"),
                     discontinue_date = c("DISCONTINUE DATE", "discontinue.date", "discontinue_date")
                     )
  cleansedDt <- cleanseTableNames(hygieneTable = renameList, uncleanDt = uncleanDt)
  
  # add on width/height/depth columns
  cleansedDt <- addDimensionCols(cleansedDt)
  return(
    cleansedDt
  )
}

cleansePogData <- function(uncleanDt) {
  # de-ambiguate key variables in the pog data that comes from alteryx (and/or C#)
  renameList <- list(itemId = c("id", "positionId", "ID", "DPCI", "dpci"),
                     pogId = c("pogId", "planogram_desc2"))
  cleansedDt <- cleanseTableNames(hygieneTable = renameList, uncleanDt = uncleanDt)
  
  return(
    cleansedDt
  )
}

addClusterColumn <- function(tieDt, clusterNames = c("")) {
  
  # Remove cluster column if it already exists
  if ("cluster" %in% names(tieDt)) {
    tieDt[, cluster := NULL]
  }  
  for (clusterName in clusterNames) {
    cleanTieDt[grepl(clusterName, title), cluster := clusterName]
  }
  cleanTieDt[is.na(cluster), cluster := "UNCLUSTERED"]
  
  # break out A/B pogs if fewer than 30, else break out by cluster
  cleanTieDt[, is_ab := fifelse(uniqueN(display) > 1, 1, 0), by = store]
  if (cleanTieDt[is_ab == 1, .N] < 30) {
    cleanTieDt[is_ab == 1, cluster := "AB_pogs"]
  } else {
    cleanTieDt[, cluster := fifelse (is_ab == 1, paste0("AB_", cluster), cluster)]
  }
  print(cleanTieDt[, .N, by = cluster])

  return(
    cleanTieDt
  )
}


buildProjectIndexDt <- function(storePogDtWithCleanClusterColumn, maxProjectSize = 50, startCount = 1, allCols = FALSE) {
  
  requiredColumnNames <- c("pogId", "title", "width", "depth", "height", "cluster")
  checkRequiredColumnNames(storePogDtWithCleanClusterColumn)
  
  indexDt <- storePogDtWithCleanClusterColumn %>%
    .[, .(store_count = .N), by = .(pogId, title, width, depth, height, cluster)]
  
  if (indexDt[, uniqueN(pogId)] != NROW(indexDt)) {
    stop("pogId not unique by either cluster or dimension")
  }
  
  # Subdivide projects that are too big so their file sizes are favorable (< 50 is a safe size)
  indexDt <- indexDt[, rank := seq_len(.N), by = cluster] %>%
    .[, max_rank := max(rank), by = cluster] %>%
    .[max_rank > maxProjectSize, cluster := paste0(cluster, 
                                                   " pt. ",
                                                   1 + floor((rank / (maxProjectSize + 1))),
                                                   " of ",
                                                   ceiling(max_rank / maxProjectSize))]
  
  # Projects get ordered alphabetically as enforced by the keyby
  projectNames <- indexDt[, .N, keyby = cluster] %>% 
    .[, num := formatC(seq_len(.N) + (startCount - 1), 
                       width = max(2, nchar(as.character(startCount))),
                       flag = "0")] %>% 
    .[, ProjectId := paste0(num, " - ", cluster)]
  
  
  projectIndex <- merge(indexDt, projectNames, by = "cluster") %>%
    .[order(ProjectId, width, nchar(title))]
  projectIndex[, sorting_order := seq_len(.N), by = ProjectId]
  projectIndex[, total_sort := seq_len(.N)]
  
  # Re-arrange the AB fields if necessary
  abIndex <- projectIndex[grepl(" AB", ProjectId)]
  if (nrow(abIndex) > 0) {
    abIndex <- abIndex[order(nchar(title), title)]
    abIndex[, sorting_order := seq_len(.N)]
    projectIndex <- rbind(projectIndex[!grepl(" AB", ProjectId)],
                          abIndex) %>%
      .[order(ProjectId, sorting_order)]
    projectIndex[, total_sort := seq_len(.N)]
    projectIndex[, sorting_order := seq_len(.N), by = ProjectId]
  }
  
  setnames(projectIndex, old = c("pogId", "sorting_order"), new = c("PogId", "Seq"))
  
  if (!allCols) {
    projectIndex <- projectIndex[, .(PogId, ProjectId, Seq)]
  } else {
    projectIndex <- projectIndex[, .(display = PogId, 
                                     checked_out_by = "",
                                     store_count,
                                     sorting_order = Seq,
                                     total_sort,
                                     batch = ProjectId,
                                     cluster,
                                     width,
                                     height,
                                     depth,
                                     size = paste0(width, "x", height, "x", depth),
                                     title,
                                     mpd = 0,
                                     is_ab = fifelse(grepl("AB", title), 1, 0))]
  }
  
  return(
    projectIndex
  )
  
}

if (FALSE) {
  rm(list = ls(all.names = TRUE))
  setwd("~/r_code/functions")
  sapply(list.files(full.names = TRUE), source)
  
  
  exOutput <- 
  fwrite(data.table(topLine = c("words", "up", "top"),
                    bottomLine = c("words", "down", "low"),
                    share = c(.5,.4,.1)), "~/Downloads/exOutput.csv")
}
