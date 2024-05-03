setwd("~/Downloads/_test/")
placerKey <- fread("place_data_key.csv")
placerSkeleton <- fread("placer_skeleton_all_retailers.csv")
tdLinxSkeleton <- fread("tdlinx_file.csv")
storeList <- getStoreListDt()
placerDt <- fread("placer_data.csv")

placerDt <- merge(placerKey, placerDt, by = "id")

placerDt[, lapply(.SD, FUN = uniqueN), .SD = names(placerDt)]

yearDt <- fread("store_yearly_sales_2.csv") %>%
  .[!is.na(total_yearly_dollars)] %>%
  .[last_week_in_period == 1472] %>%
  .[total_yearly_dollars > 1000] %>%
  .[storenumber %in% tiesDt$storeId] %>%
  merge(storeList, by.x = "storenumber", by.y = "storeId")

yearDt[, placer_flag := fifelse(storenumber %in% placerDt[, unique(store_id)], 1, 0),
       by = storenumber]
yearDt[, n_weeks := 1 + last_week_in_period - first_week_in_period]
yearDt[, adj_dollars := 52 * total_yearly_dollars / n_weeks]
yearDt <- yearDt[order(-adj_dollars)] 

# Plot 1: "Stores are concentrated at the bottom"
plot1Dt <- copy(yearDt) %>% .[, x := seq_len(.N)]
ggplot(plot1Dt,
       aes(x = x, 
           y = adj_dollars / 1000 / 1000, 
           fill = as.factor(placer_flag))) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("black", "orange")) +
  xlab("Target stores ordered by volume") +
  ylab("Adjusted yearly sales, in millions") +
  ggtitle("Missing stores are concentrated at lowest sale levels") +
  theme_minimal() + 
  theme(legend.position = "none")


storeSalesDt <- copy(spencerFile)[Retailer == "Walgreens"]
placerStoreList <- placerKey[name == "Walgreens", store_id]
storeSalesDt[, placer_flag := fifelse(StoreNumber %in% placerStoreList, 1, 0)]
storeSalesDt[order(-TotalSalesAmount), x := seq_len(.N)]
ggplot(storeSalesDt,
       aes(x = x,
           y = TotalSalesAmount,
           fill = as.factor(placer_flag))) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("black", "orange")) +
  xlab("Target stores ordered by volume") +
  ylab("Adjusted yearly sales, in millions") +
  ggtitle("Missing stores are concentrated at lowest sale levels") +
  theme_minimal() + 
  theme(legend.position = "none")

# Plot 2: Top 50
plot2Dt <- copy(yearDt[1 : 100]) %>%
  .[, x := seq_len(.N)]
ggplot(plot2Dt,
       aes(x = x, 
           y = adj_dollars / 1000 / 1000, 
           fill = as.factor(placer_flag))) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("black", "orange")) +
  xlab("Missing stores in black") +
  ylab("Adjusted yearly sales, in millions") +
  ggtitle("8 of top 100 Target stores not in placer data.") +
  theme_minimal() + 
  theme(legend.position = "none")

# Plot 3: Bottom 100
plot3Dt <- copy(yearDt[(NROW(yearDt) - 99):NROW(yearDt)]) %>%
                         .[, x := seq_len(.N)]
ggplot(plot3Dt,
       aes(x = x, 
           y = adj_dollars / 1000 / 1000, 
           fill = as.factor(placer_flag))) +
  geom_bar(stat = "identity") +  
  scale_fill_manual(values = c("black", "orange")) +
  xlab("Missing stores in black") +
  ylab("Adjusted yearly sales, in millions") +
  ggtitle("59 of bottom 100 Target stores not in placer data.") +
  theme_minimal() + 
  theme(legend.position = "none")


# Plot 4: where are the gaps
topTenStates <- yearDt %>% 
  .[, .N, keyby = state] %>%
  .[order(-N)] %>%
  .[1 : 10, state]

plot4Dt <- copy(yearDt) %>%
  .[state %in% topTenStates] %>%
  .[, .N, keyby = .(state, placer_flag)]
plot4Dt[, prop_n := N / sum(N), keyby = placer_flag]
ggplot(plot4Dt,
       aes(x = state,
           y = prop_n,
           fill = as.factor(placer_flag))) +
  geom_bar(stat = "identity",
           position = "dodge") +
  theme_minimal() +
  xlab("Top ten states by Target frequency (missing in black)") +
  ylab("Proportion across missingness per state") + 
  ggtitle("Densely populated areas underly missing stores") +
  scale_fill_manual(values = c("black", "orange")) +
  theme(legend.position = "none")
  
# TODO: SAME GRAPH AS ^^^ BUT FOR FORMAT

openStoreDt <- placerKey[is.na(closing_date)] %>%
  .[store_id != ""]

# Target venn diagram
# left side: placer
# right side: tdlinx
placerIds <- placerDt$store_id %>% unique
tdLinxIds <- tdLinxSkeleton[Ultimate_Parent_Name == "Target Corp"] %>%
  .[, unique(Retailer_Store_Number)]
drawnIds <- tiesDt[, unique(storeId)]
ggvenn(list(placer = placerIds,
            tdLinx = tdLinxIds,
            icTieReport = drawnIds)) + 
  ggtitle("Target")

spencerFile <- fread("spencer_file.csv")

# Walgreens: TDLinx vs placer
wgPlacerIds <- placerKey[is.na(closing_date)][name == "Walgreens", store_id %>% unique] %>%
  as.numeric
wgTds <- tdLinxSkeleton[Ultimate_Parent_Name == "Walgreens Boots Alliance", Retailer_Store_Number]
wgDrawnIds <- spencerFile[Retailer == "Walgreens", StoreNumber]
ggvenn(list(placer = wgPlacerIds,
            tdLinx = wgTds,
            spencerFile = wgDrawnIds)) + 
       ggtitle("Walgreens")

# CVS: TDLinx vs placer
cvsPlacerIds <- placerKey[is.na(closing_date)][name == "CVS/pharmacy", store_id]
cvsTds <- tdLinxSkeleton[Ultimate_Parent_Name == "CVS Health", Retailer_Store_Number]
ggvenn(list(placer = cvsPlacerIds,
            tdLinx = cvsTds)) + 
  ggtitle("CVS")

# Walmart

placerTdVenn <- function(retailerName, 
                         placerSkeleton,
                         tdLinxSkeleton,
                         thirdSet = c(),
                         outType = c("plot", "data")) {
  
  outType <- match.arg(outType, c("plot", "data"))
  # Filter placer data 
  filteredPlacerDt <- placerSkeleton[(is.na(closing_date)) & store_id != ""] %>%
    .[grepl(tolower(retailerName), tolower(name))]
  print("Placer store type breakdown:")
  print(table(filteredPlacerDt$name))
  placerIds <- filteredPlacerDt[!is.na(as.numeric(store_id)), as.numeric(store_id)]
  
  # Filter tdLinx data
  filteredTdLinxDt <- tdLinxSkeleton[grepl(tolower(retailerName), tolower(Ultimate_Parent_Name))]
  if (grepl("walmart", tolower(retailerName))) {
    filteredTdLinxDt <- filteredTdLinxDt[Name != "Sams Club"]
  }
  
  if (grepl("family dollar", tolower(retailerName))) {
    filteredTdLinxDt <- tdLinxSkeleton[grepl("family dollar", tolower(Name))]
  }
  
  
  print("TDLinx Store type breakdown")
  print(filteredTdLinxDt[, table(Name)])
  tdLinxIds <- filteredTdLinxDt[!is.na(as.numeric(Retailer_Store_Number)),
                                as.numeric(Retailer_Store_Number)]
  
  outputSets <- list(placer = placerIds,
                     tdLinx = tdLinxIds,
                     salesFileInput = thirdSet)
  # Remove thirdList if it is empty, and any other empties
  outputSets <- outputSets[which(lapply(outputSets, FUN = length) > 0)]
  if (length(outputSets) == 0) {
    stop("There's no data on this filter")
  } else {
    if (outType == "plot") {
    return(
      ggvenn(outputSets) + ggtitle(retailerName)
    )
    } else {
      # Build output data table
      outTable <- data.table(store_id = numeric(),
                             type = character(),
                             city = character(),
                             state = character(),
                             address = character())
      placerOnlyList <- leftOnly(placerIds, tdLinxIds)
      if (length(placerOnlyList) > 0) {
        outTable <- rbind(outTable,
                          filteredPlacerDt[store_id %in% placerOnlyList,
                                           .(store_id,
                                             type = "only in placer",
                                             city = city,
                                             state = state_code,
                                             address)]
        )
      }
      tdLinxOnlyList <- rightOnly(placerIds, tdLinxIds)
      if (length(tdLinxOnlyList) > 0) {
        outTable <- rbind(outTable,
                          filteredTdLinxDt[Retailer_Store_Number %in% tdLinxOnlyList,
                                           .(store_id = Retailer_Store_Number,
                                             type = "only in tdlinx",
                                             city = City_Town,
                                             state = State,
                                             address  = Street)]
        )
      }
      inBothList <- intersect(placerIds, tdLinxIds)
      if (length(inBothList) > 0) {
        outTable <- rbind(outTable,
                          filteredTdLinxDt[Retailer_Store_Number %in% inBothList,
                                           .(store_id = Retailer_Store_Number,
                                             type = "in both",
                                             city = City_Town,
                                             state = State,
                                             address = Street)]
        )
      }
      return(
        outTable
      )
    }
  }
}
  

placerTdVenn("Walmart", placerSkeleton, tdLinxSkeleton)
wmtData <- placerTdVenn("Walmart", placerSkeleton, tdLinxSkeleton, outType = "data")

placerTdVenn("Target", placerSkeleton, tdLinxSkeleton, thirdSet = tiesDt$storeId %>% unique)
tgtData <- placerTdVenn("Target", placerSkeleton, tdLinxSkeleton, thirdSet = tiesDt$storeId %>% unique, outType = "data")

placerTdVenn("CVS", placerSkeleton, tdLinxSkeleton)

placerTdVenn("dollar general", placerSkeleton, tdLinxSkeleton,
             thirdSet = spencerFile[Retailer == "Dollar General", StoreNumber])
dgData <- placerTdVenn("dollar general", placerSkeleton, tdLinxSkeleton, outType = "data")


placerTdVenn("walgreens", placerSkeleton, tdLinxSkeleton[Name != "Duane Reade"],
             thirdSet = spencerFile[State != "PR"][TotalSalesAmount > 1000][Retailer == "Walgreens", StoreNumber])
placerTdVenn("family dollar", placerSkeleton, tdLinxSkeleton,
             thirdSet = spencerFile[State != "PR"][TotalSalesAmount > 1000][Retailer == "Family Dollar", StoreNumber])
placerTdVenn("dollar general", placerSkeleton, tdLinxSkeleton,
             thirdSet = spencerFile[State != "PR"][TotalSalesAmount > 1000][Retailer == "Dollar General", StoreNumber])


placerTdVenn("dollar general", placerSkeleton, tdLinxSkeleton)

placerTdVenn("albertsons", placerSkeleton, tdLinxSkeleton)
