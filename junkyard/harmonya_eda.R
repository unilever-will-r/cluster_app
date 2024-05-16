# Harmonya sales file comparison: my pull dt1 vs dashboard pull dt2
# item count comparison
# total sales comparison week-over-week
# To be run every time (assume new v old file structure)

source("~/r_code/00_initialize.R")


readHarmonyaProductFileBatch <- function(pathToDir) {
  
  readAndRelabel <- function(a_path) {
    dt <- fread(a_path)
    names(dt)[3] <- "Department"
    dt[, upc := bit64::as.integer64(UPC)]
    return(dt)
  }
  return(
    pathToDir %>%
      list.files(full.names = TRUE) %>%
      lapply(FUN = readAndRelabel) %>%
      rbindlist %>% 
      cleanNames %>%
      cleanUpColumns
  )
}

cleanUpColumns <- function(dt) {
  # due to excel formatting, csvs can sometimes inherit dollar signs and commas, strip those where we can from all columns
  # doesnt touch any columns names upc, because could corrupt their int64 state
  cleanUpColumn <- function(v) {
    # Detect if a column can be coerced to numeric, and if it can, do so
    suppressWarnings(
      coerced_v <- as.numeric(gsub("[()$,%]", "", v), warning = F)
    )
    
    if (sum(is.na(coerced_v)) == 0) {
      return(coerced_v)
    } else {
      return(v)
    }
  }
  affected_columns <- leftOnly(names(dt), "upc")
  return(
    dt[, (affected_columns) := lapply(.SD, FUN = cleanUpColumn), .SDcols = affected_columns]
  )
}

setwd("C:/Users/William.Roberts/Downloads/harmonya_sales_share_by_item")

dt0 <- fread("by_dpci_pull.csv") %>%
  cleanNames
dt1 <- fread("part-00000-tid-3164456181807724100-5226c5d0-46c2-4132-95f3-ffd64053225b-36894-1-c000.csv") %>%
  cleanNames %>% 
  .[!(departmentnumber %in% c(3, 52))] %>% 
  .[, upc := bit64::as.integer64(upc)] %>%
  .[, .(salesdollar = sum(salesdollar),
        salesunit = sum(salesunit)),
    keyby = .(upc, dpci, department,
              itemdescription, brandname, vendorname, departmentnumber, class, subclass,
              channelnetoriginated)]
dt2 <- readHarmonyaProductFileBatch("./all_harmonya_pulls")

# check 1: which items aren't in both
new_upcs <- dt1[salesdollar > 0][, unique(upc)]
old_upcs <- dt2[total_sales > 0, unique(upc)]
in_both <- intersect(old_upcs, new_upcs)
only_in_dashboard <- leftOnly(old_upcs, new_upcs) %>% sort
only_in_new <- rightOnly(old_upcs, new_upcs) %>% sort

# flag them on dt1, summarize
dt1[, upc_flag := fifelse(upc %in% in_both, 1, 0), by = upc]
summaryDt <- dt1[, .(n_upcs = uniqueN(upc),
                     n_dpcis = uniqueN(dpci),
                     sales = sum(salesdollar)),
                 keyby = .(department, upc_flag)] %>%
  .[, sales_share := sales / sum(sales), by = department]


# check 2: sum of sales by dt0 and dt1 match
dt0[, .(item_count = uniqueN(dpci),
        dollars = sum(salesdollar) / 1000 / 1000,
        dollars_ly = sum(salesdollaryearago) / 1000 / 1000), by = department]
dt1[, .(item_count = uniqueN(dpci),
        dollars = sum(salesdollar) / 1000 / 1000,
        dollars_ly = sum(salesdollaryearago) / 1000 / 1000), by = department] 
# files show up as multiple 
dt1 <- list.files(, full.names = T) %>%
  grep(".csv", ., value = T) %>%
  lapply(FUN = fread) %>%
  rbindlist

