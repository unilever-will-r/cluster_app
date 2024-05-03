# Plotting functions for running on a planogram config


createBarPlotTopX <- function(podDt, grouping_var, top_n = 10) {
  podDt <- copy(podDt)
  setnames(podDt, old = grouping_var, new = "grouping_var")
  plotData <- podDt[, .(pods = sum(store_count)), by = grouping_var] %>%
    .[order(-pods)] %>%
    .[, pod_share := pods / sum(pods)] %>%
    .[, pod_share_format_out := (formatC(round(pod_share, 4) * 100) %>% paste0("%"))] %>%
    .[1 : top_n]
  ggplot(plotData, aes(x = factor(grouping_var, levels = grouping_var), y= pods)) + 
    geom_bar(stat = "identity") + 
    geom_text(aes(label = pod_share_format_out), vjust = 1, color = "white") + 
    xlab(grouping_var) + 
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
    ggtitle(paste0("Top ", top_n, " ", grouping_var, " by PODs"))
}

createPrePostBarPlotTopX <- function(podDt, grouping_var, top_n = 10) {
  podDt <- copy(podDt)
  setnames(podDt, old = grouping_var, new = "grouping_var")
  plotData <- podDt[, .(pre = sum(store_count),
                        post = sum(store_count_post)), by = grouping_var] %>% 
    .[order(-abs(pre - post))] %>%
    .[, pod_share_format_out := paste0(formatC(round(post/ sum(post), 4) * 100), "%")] %>%
    .[1 : top_n] %>%
    melt(id.vars = c("grouping_var", "pod_share_format_out")) %>%
    .[order(variable, -value)] %>%
    .[variable == "pre", pod_share_format_out := ""]
  ggplot(plotData, aes(x = factor(grouping_var, levels = unique(grouping_var)), fill = variable,
                       y = value)) + 
    geom_bar(stat = "identity", position = "dodge") + 
    geom_text(aes(label = pod_share_format_out), vjust = -.5, color = "black",
              position = position_dodge(width = 1)) + 
    xlab(grouping_var) + 
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
    ggtitle(paste0("Top ", top_n, " ", grouping_var, " by POD shift"))
}

createPrePostPodByItem <- function(podDt, autoSwapDt, itemList = NULL) {
  if (is.null(itemList)) {
    itemList <- podDt[, unique(positionId)]
  }
  plotData <- podDt[, .(pre = sum(store_count),
                        post = sum(store_count_post)), by = .(positionId, item_description = `Item Description`)] %>%
    .[order(pre - post)] %>%
    melt(id.vars = c("position_id")) %>%
    .[order(variable, -value)]
  ggplot(plotData, aes(x = factor(positionId, levels = unique(positionId)),
                       y = value,
                       fill = variable)) + 
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal() + 
    ggtitle("PODs from automation")
}

createRarestItemsBarPlot <- function(podDt, top_n = 20) {
  # Function to list the "rarest" items by POD
  plotData <- podDt[!is.na(`Item Description`),
                    .(pods = sum(store_count)),
                    by = .(positionId, item_desc = `Item Description`)] %>%
    .[order(pods)] %>%
    .[pods > 0] %>%
    .[ 1 : top_n] %>%
    .[, h_just := fifelse(pods / max(pods) > .6, 1, 0)] %>%
    .[, color := fifelse(pods / max(pods) > .6, "white", "black")]
  ggplot(plotData, aes(x = factor(item_desc, levels = rev(item_desc)), y = pods)) + 
    geom_bar(stat = "identity") + 
    geom_text(aes(label = item_desc, hjust = h_just, color = color)) + 
    scale_color_manual(values = c("black", "white")) + 
    coord_flip() + 
    ggtitle(paste0("Top ", NROW(plotData), " Items with fewest total PODs"),
            "Unattributed items filtered out.") +
    theme_minimal() + 
    theme(axis.text.y = element_blank(),
          axis.title = element_blank(),
          legend.position = "none")
}

createNearFullItemsBarPlot <- function(podDt, min_pods_for_full = 1900) {
  # Function to list the "rarest" items by POD
  plotData <- podDt[!is.na(`Item Description`),
                    .(pods = sum(store_count)),
                    by = .(positionId, item_desc = `Item Description`, Brand)] %>%
    .[order(pods)] %>%
    .[pods > min_pods_for_full] %>% 
    .[, n_items_per_brand := uniqueN(positionId), by = .(Brand)] %>%
    .[n_items_per_brand > 1] %>%
    .[order(Brand, pods)] %>%
    .[, plot_fill := seq_len(.N), by = Brand]
  ggplot(plotData, aes(x = factor(Brand, levels = unique(Brand)), 
                       y = n_items_per_brand * pods / max(pods))) + 
    geom_bar(aes(fill = factor(plot_fill), group = factor(plot_fill)), position = "dodge", stat = "identity") + 
    geom_text(aes(label = item_desc, group = factor(plot_fill)),
              hjust = 0,
              position = position_dodge(width = 1)) +
    coord_flip() + 
    ggtitle(paste0("Brands with more than one item in near full chain distribution")) +
    theme_minimal() + 
    theme(axis.text.x = element_blank(),
          axis.title = element_blank(),
          legend.position = "none")
}

createPodPdf <- function(config) {
  attributionDt <- fread(file.path(config$filesDir, "attribution_table.csv"))
  autoSwapDt <- fread(file.path(config$filesDir, "item_inc_dec_input.csv"))
  storePogDt <- fread(config$storePogDtPath)
  pogItemDt  <- fread(file.path(config$configsDir, "position_table_preautomation.csv")) %>%
    .[, .(facings = sum(positionHFacings)), by = .(pogId, positionId)]
  storeItemDt <- merge(storePogDt, pogItemDt, by = c("pogId"), allow.cartesian = TRUE)
  
  pogItemDtPost <- fread(file.path(config$configsDir, "position_table_postautomation.csv")) %>%
    .[, .(facings = sum(positionHFacings)), by = .(pogId, positionId)] 
  
  storeItemDtPost <- merge(storePogDt, pogItemDtPost, by = "pogId", allow.cartesian = TRUE)
  
  
  podDt <- storeItemDtPost[, .(store_count_post = uniqueN(storeId)), by = positionId] %>%
    merge( storeItemDt[, .(store_count = uniqueN(storeId)), by = positionId],
           all = TRUE, by = "positionId") %>%
    merge(attributionDt, all.x = TRUE, all.y = FALSE) %>%
    .[is.na(store_count), store_count := 0] %>%
    .[is.na(store_count_post), store_count_post := 0]
  
  podDt <- merge(podDt, autoSwapDt, by = "positionId", all = TRUE)
  
  plotList <- lapply(c("Manufacturer", "Brand"), FUN = function(x) createBarPlotTopX(podDt, grouping_var = x))
  plotList[[length(plotList) + 1]] <- createRarestItemsBarPlot(podDt = podDt, top_n = 15)
  plotList[[length(plotList) + 1]] <- createNearFullItemsBarPlot(podDt, min_pods_for_full = 1900)
  
  
  # prePostPlots
  plotList[[length(plotList) + 1]] <- createPrePostBarPlotTopX(podDt, grouping_var = "Brand", 10)
  plotList[[length(plotList) + 1]] <- createPrePostBarPlotTopX(podDt, grouping_var = "Manufacturer", 7)
  
  ggsave(filename = config$outputPdfPath,
         plot = marrangeGrob(plotList, nrow = 1, ncol = 1),
         width = 15, height = 9)
}

if (FALSE) {
  setwd("~/r_code")
  source("00_initialize.R")
  
  createPodPdf(config)
  setwd("C:/Users/William.Roberts/Unilever/Target POG HUB - Documents/00 - Automation & Reporting/PTIQ/2023/Dish March Transition/Inputs")
  preCatTies <- read_xlsx("PRE CAT TIES.xlsx") %>% cleanNames %>% as.data.table %>% buildTieReport
  postCatTies <- fread("POST CAT TIES.csv") %>% cleanNames %>% buildTieReport
  
  
  prePostDt <- merge(preCatTies[, .(pre_foot = min(32, max(8, 4 * round(sum(width) / 4)))), keyby = store],
                     postCatTies[, .(post_foot = min(32, max(8, 4 * round(sum(width) / 4)))), keyby = store],
                     all = TRUE)
  prePostDt[, ':='(pre_foot = as.character(pre_foot),
                   post_foot = as.character(post_foot))]
  prePostDt[is.na(pre_foot), pre_foot := "NO POG"]
  prePostDt[is.na(post_foot), post_foot := "NO POG"]
  prePostDt[pre_foot == "32", pre_foot := ">32"]
  prePostDt[post_foot == "32", post_foot := ">32"]
  
  plotDt <- prePostDt[, .(weights = .N), by = .(pre_foot, post_foot)] %>%
    .[, post_foot := paste0(post_foot, " ")]
  
  plot(
    gvisSankey(plotDt, from = "pre_foot", to = "post_foot", weight = weights)
  )
}
