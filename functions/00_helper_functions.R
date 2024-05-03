library(data.table)
library(magrittr)



clipToPath <- function(path = "clipboard") {
  y <- if (path == "clipboard") {
    readClipboard()
  } else {
    cat("Please enter the path:\n\n")
    readline()
  }
  x <- chartr("\\", "/", y)
  writeClipboard(x)
  return(x)
}

pathToDt <- function(v = read_clip()) {
  v %>%
    chartr("\\", "/", .) %>%
    gsub("\"", "", .) %>%
    fread() %>%
    cleanNames
}

readClipDt <- function(x = read_clip_tbl()) {
  return(
    cleanNames(as.data.table(x))
  )
}

cleanNames <- function(dt) {
  # get rid of special characters, warn if non-uniqueness introduced
  oldNames <- names(dt)
  newNames <- names(dt) %>%
    sapply(FUN = gsub, pattern = "\\+", replacement = "plus") %>%
    sapply(FUN = gsub, pattern = "\\.", replacement = "point") %>%
    sapply(FUN = gsub, pattern = "_", replacement = " ") %>% # don't want to collapse '_'s
    sapply(FUN = gsub, pattern = "[[:punct:]]", replacement = "") %>%
    sapply(FUN = gsub, pattern = " ", replacement = "_") %>%
    sapply(FUN = function(x) {if (substring(x, 0, 1) == "_") {x <- substring(x, 2, nchar(x))}; x}) %>%
    tolower()
  if (uniqueN(newNames) != length(newNames)) {
    warning("You are creating duplicate column names")
  }
  
  names(dt) <- newNames
  return(dt)
}

stripIdentityCols <- function(dt) {
  colsToKeep <- sapply(dt, )
}

checkRequiredColumnNames <- function(dt, requiredColumnNames = c()) {
  missingColumnNames <- requiredColumnNames[!(requiredColumnNames %in% names(dt))]
  if (length(missingColumnNames) > 0) {
    stop(paste0("Missing required column(s): ", paste(missingColumnNames, collapse = ", "),
                " from table: ", deparse(substitute(dt))))
  }
  return(invisible(NULL))
}

leftOnly <- function(v1, v2) {
  return(
    v1[!(v1 %in% v2)] %>% unique
  )
}

rightOnly <- function(v1, v2) {
  return(
    leftOnly(v2, v1)
  )
}

padDpci <- function(v) {
  return(
    sapply(as.integer(v), FUN = formatC, width = 9, flag = "0")
  )
}

getStoreListDt <- function(category = c("all", "frozen", "frozen_all"),
                           asOfDate = as.Date("2024-02-05")) {
  category <- match.arg(category)

  
  # Data cleansing
  storeListDt <- fread("C:/Users/William.Roberts/store_list.csv") %>%
    cleanNames %>%
    .[(tolower(locationsubtype) %in% c("super target", "small format", "general merch"))] %>%
    .[as.Date(storeopendate) < asOfDate] %>%
    .[as.Date(storeclosedate) > asOfDate]

  if (category != "all") {
    # Filter to universe of stores relevant to given category
    if (category == "frozen_all") {
      # Omit stores with no frozen section
      omit_stores <- c(1218)
    }
    if (category == "frozen") {
      # Omit stores we don't draw for (these are stores with very small frozen sections)
      omit_stores <- c(219, 253, 779, 796, 831, 905, 915,
                       953, 969, 986, 1022, 1079, 1082, 1218) 
    }
    storeListDt <- storeListDt[!(storenumber %in% omit_stores)]
  }
  return(
    storeListDt[, .(storeId = storenumber,
                    store_type = tolower(locationsubtype),
                    store_name = storedescription,
                    city = buildingcity,
                    zip = zipcode,
                    state, 
                    open_date = storeopendate,
                    close_date = storeclosedate,
                    dc_location = fooddcnumber,
                    latitude,
                    longitude,
                    full_address = buildingaddress,
                    ul_total_dollars_l52 = total_dollars_l52)]
  )
}

filterToState <- function(dt, state_name = "Minnesota", store_key = "store_id") {
  setnames(dt, old = store_key, new = "store___key")
  storeList <- getStoreListDt()[tolower(state) %in% tolower(state_name), storeId]
  outDt <- dt[store___key %in% storeList]
  setnames(outDt, old = "store___key", new = store_key)
  return(
    outDt
  )
}

addDoorCountToDt <- function(dt, title_col = "title") {
  setnames(dt, old = title_col, new = "title")
  doorCountsFound <- dt[grepl("DR", title)] %>% NROW
  if (doorCountsFound != NROW(dt)) {
    warning(paste0(doorCountsFound, "door counts found but ", NROW(dt)),
            "displays present. Assigning NA to POGs with unknown doorcounts.")
  }
  getDoorCountFromString <- function(a_string) {
    a_string %>% 
      gsub("[[:punct:]]", " ", .) %>% 
      strsplit(split = " ") %>% 
      `[[`(1) %>%
      grep(pattern = "DR", value = TRUE) %>%
      sub(pattern     = "DR.*",
          replacement = "", .) %>%
      as.integer()
  }
  dt[, doors_new := getDoorCountFromString(title), by = title]
  if (dt[, sum(is.na(doors_new))] > 0) {
    warning("Some displays NA'd out while parsing. Investigate output to figre out casue.")
  }
  setnames(dt, old = "title", new = title_col)
  return(
    dt
  )
}

renameBatchesFromCksToCustom <- function(inputDir, batchOrderDt) {
  setwd(inputDir)
  for (oldFileName in list.files()) {
    newFileName <- batchOrderDt[PogId == gsub(".PSA", "", oldFileName), paste0(BatchId, ".PSA")]
    file.rename(from = oldFileName, to = newFileName)
  }
  return(
    NULL
  )
}

consolidatePogDataFiles <- function(leanDt, bigDt) {
  keepCols <- c(names(leanDt), "Position LocationID")
  return(
    bigDt[[keepCols]] %>% cleanNames
  )
}

if (FALSE) {
  rm(list = ls(all.names = TRUE))
  setwd("~/r_code/functions")
  sapply(list.files(full.names = TRUE), source)
  
  
  setwd("~/ic_revision_sandbox")
  inputDir <- "./ic_revision_cks_pulls/"
  batchOrderDt <- fread("batch_order_dt.csv")
  renameBatchesFromCksToCustom(inputDir, batchOrderDt)

  
}
