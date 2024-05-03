loadData <- function(basePath, sheetName = NULL) {
  # generic function for reading .csvs and .xlsx files
  # force-reads from a single sheet if .xlsx based on following priorities
  # (1) if sheetName is non-null, prefers sheetName
  # (2) if sheetName is null or doesnt exist, prefers the sheet with highest number of rows
  
  basePathExtension <- tools::file_ext(basePath)
  
  if (basePathExtension == "csv") {
    data <- fread(basePath)
  } else if (basePathExtension %in% c("xlsx", "xls")) {
    
    sheetNames <- excel_sheets(basePath)
    
    if (is.null(sheetName)) {
      sheetName <- " i m p o s s i b l e s h e e t n a m e "
    }
    
    if (sheetName %in% sheetNames) {
      # Load from sheetName if it exists
      data <- read_excel(basePath, sheet = sheetName)
    } else {
      sheetData <- lapply(sheetNames, FUN = function(x) {read_xlsx(basePath, sheet = x)})
      sheetRows <- sapply(sheetData, FUN = nrow)
      # Read the first sheet
      print(paste0("Sheet name not specified or not found. Reading in sheet ",
                   sheetNames[1], " (rows: ", 
                   paste0(sheetRows[1]), ")"))
      print("Sheet list:")
      print(paste0(mapply(sheetNames, paste0(" (rows: ", sheetRows, ")"), FUN = paste0)))
      # Read the sheet with the longest number of rows
      data <- sheetData[[1]]

    }
  } else {
    stop("Unsupported file type. Please provide a .csv or .xlsx file.")
  }
  return(as.data.table(data))
}

loadDataSansExtension <- function(filePathSansExtension) {
  csvPath <- paste0(filePathSansExtension, ".csv")
  excelPath <- paste0(filePathSansExtension, ".xlsx")
  if (file.exists(csvPath)) {
    data <- loadData(csvPath)
  } else if (file.exists(excelPath)) {
    data <- loadData(excelPath)
  } else {
    NULL
  }
}

getAttributionDt <- function(attributionDtPath, attributionTableSheetName) {
  # Reads from master attribution excel file, 
  rawDt <-   readxl::read_excel(attributionDtPath, sheet =  attributionTableSheetName) %>%
    as.data.table %>%
  cleanNames
  setnames(rawDt, old = "dpci", new = "itemId")
  
  # force itemId to be an integer
  rawDt[, itemId := as.integer(itemId)]
  
  # drop all columns that are all NA
  goodCols <- names(rawDt) %>%
    .[sapply(rawDt, FUN = function(x) fifelse(length(x) == sum(is.na(x)), 1, 0)) == 0]
  return(
    rawDt[, ..goodCols]
  )
}

getAttributionDtFromConfig <- function(config) {
  getAttributionDt(attributionDtPath         = config$attributionDtPath,
                   attributionTableSheetName = config$attributionTableSheetName)
}

getPogItemDt <- function(config, era = c("PRE", "POST")) {
  era <- match.arg(era)
  rawDt <- fread(file.path(config$ptiqFilesDir,
                           paste0(era, " JDA DATA.csv"))) %>%
    cleanNames()
  setnames(rawDt, old = c("id", "planogram_desc2"), new = c("positionId", "pogId"))
  return(
    rawDt
  )
}

getStorePogDt <- function(config, era = c("PRE", "POST")) {
  era <- match.arg(era)
  rawDt <- fread(file.path(config$ptiqFilesDir,
                           paste0(era, " CAT TIES.csv")))
  return(
    rawDt
  )
}

buildDummyIndex <- function(pogData) {
  pogList <- unique(pogData$pogId) %>% sort
  dummyIndex <- data.table(PogId = pogList) %>%
    .[, ProjectId := paste0("batch_", floor(seq_len(.N) / 100))] %>%
    .[, Seq := seq_len(.N), by = ProjectId]
  return(
    dummyIndex
  )
}


readPerformanceLibrary <- function(config) {
  tableName <- "Results0"
  driverConnectString <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
                                config$performanceLibraryDbPath) %>%
    odbcDriverConnect
  prodDt <- as.data.table(sqlFetch(driverConnectString, tableName, rownames = TRUE))
  on.exit(RODBC::odbcClose(driverConnectString))
  return(prodDt)
}

writePerformanceLibrary <- function(config, perfLibDt) {
  tableName <- "Results0"
  perfLibDt$ID <- formatC(perfLibDt$ID, width = 9, flag = "0")
  driverConnectString <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
                                config$performanceLibraryDbPath) %>%
    odbcDriverConnect
  sqlDrop(channel = driverConnectString, sqtable = tableName)
  sqlSave(driverConnectString, dat = perfLibDt, tablename = tableName, rownames = FALSE)
}

readProductLibrary <- function(prodLibPath) {
  tableName <- "Sheet1"
  driverConnectString <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
                                prodLibPath) %>%
    odbcDriverConnect
  prodDt <- as.data.table(sqlFetch(driverConnectString, tableName, rownames = TRUE))
  on.exit(RODBC::odbcClose(driverConnectString))
  return(prodDt)
}

writeProductLibrary <- function(prodLibDt, prodLibPath) {
  tableName <- "Sheet1"
  driverConnectString <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
                                prodLibPath) %>%
    odbcDriverConnect
  
  # Read in existing to get the column names
  currentProdDt <- as.data.table(sqlFetch(driverConnectString, tableName, rownames = TRUE))
  requiredNewNames <- currentProdDt %>%
    names
  requiredOldNames <- currentProdDt %>% 
    names
  if (sum(names(prodLibDt) %in% requiredOldNames) != length(names(prodLibDt))) {
    stop("There are some weird column names in the product library you are trying to use")
  }
  
  # Make the names match
  setnames(prodLibDt, 
           old = requiredOldNames,
           new = requiredNewNames, 
           skip_absent = TRUE)
  
  # Overwrite the old one
  sqlDrop(channel = driverConnectString, sqtable = tableName)
  
  sqlSave(driverConnectString, dat = prodLibDt, tablename = tableName, rownames = FALSE, append = TRUE,
          verbose = TRUE, safer = FALSE)
  odbcClose(driverConnectString)
}

addNewItemsToProductLibrary <- function(config) {
  attributionDt <- fread(config$attributionDtPath) %>% cleanNames()
  newItemsDt <- copy(attributionDt)[new_flag == 1]
  newItemsDt[, positionId := as.integer(gsub("-|\\.", "", dpci))]
  productLibraryDt <- readProductLibrary(config)
  newItemsToAdd <- leftOnly(newItemsDt$positionId, productLibraryDt$id)
  newItemsDt <- newItemsDt[positionId %in% newItemsToAdd]
  productLibraryNewItemsAdded <- rbind(productLibraryDt,
                                       newItemsDt[, .(id = positionId,
                                                      manufacturer,
                                                      brand,
                                                      name = item_description,
                                                      height, width, depth)],
                                       fill = TRUE)
  writeProductLibrary(config, productLibraryNewItemsAdded)
}


readPositionTable <- function(fileDir = config$configsDir, tag = "") {
  path <- file.path(fileDir, paste0(tag, "_position_table.csv"))
  if (file.exists(path)) {
    return
    fread(path)
  } else {
    stop(paste0("This file doesn't exist: ", path))
  }
}

readFixtureTable <- function(fileDir = config$configsDir, tag = "") {
  path <- file.path(fileDir, paste0(tag, "_fixture_table.csv"))
  if (file.exists(path)) {
    return
    fread(path)
  } else {
    stop(paste0("This file doesn't exist: ", path))
  }
}

readPlanogramTable <- function(fileDir = config$configsDir, tag = "") {
  path <- file.path(fileDir, paste0(tag, "_planogram_table.csv"))
  if (file.exists(path)) {
    return
    fread(path)
  } else {
    stop(paste0("This file doesn't exist: ", path))
  }
}

hasImages <- function(dpci, imageDir = "~/Desktop/Images") {
  cleanDpci <- gsub("-", "", dpci) %>% as.numeric %>% sprintf("%09d", .)
  deptString <- substring(cleanDpci, 1, 5)
  fileString <- substring(cleanDpci, 6, 9)
  return(0 < sum(grepl(pattern = fileString,
                       list.files(file.path(imageDir, deptString)))))
}

