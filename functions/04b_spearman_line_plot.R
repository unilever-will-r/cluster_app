library(data.table)
library(magrittr)
library(ggplot2)

plotRankSwaps <- function(orderedList1 = c(), orderedList2 = c(),
                          listName1 = "Ranking 1", listName2 = "Ranking 2",
                          overrideTitleText = NULL) {
  # Makes a visualization to compare the number of swaps to make one list match another
  rdt <- merge(data.table(title = rev(orderedList1))[, v1 := seq_len(.N)],
               data.table(title = rev(orderedList2))[, v2 := seq_len(.N)],
               by = "title") %>%
    .[, grp := title] %>%      # Lines are drawn per group
    .[v1 == v2, grp := NA] %>% # Don't draw lines for matching ranks
    .[, dist := (v1 - v2)^2]
  
  plotDt <- rdt %>%
    melt(id.vars = c("title", "grp", "dist")) %>%
    .[order(variable, value)]
  
  if (overrideTitleText %>% is.null) {
    plotDt[, title_text := title]
  } else {
    # overrideTitleText was implemented for the labeling of ties,
    # which are unique per variable
    overrideMerge <- rbind(
      data.table(variable = "v1",
                 title = orderedList1,
                 title_text = overrideTitleText[[1]]),
      data.table(variable = "v2",
                 title = orderedList2,
                 title_text = overrideTitleText[[2]]))
    plotDt <- merge(plotDt, overrideMerge, by = c("title", "variable"))
  }
  
  thePlot <- ggplot(plotDt, aes(x = factor(variable,
                                           levels = c("v1",
                                                      "v2")),
                                y = value,
                                group = grp,
                                color = dist)) + 
    geom_line(data = plotDt[complete.cases(plotDt),],
              linetype = "dotted", color = "grey40", alpha = .8) + 
    geom_text(aes(label  = title_text,
                  size   = 36,
                  hjust  = ifelse(variable == "v1", 1, 0))) + 
    scale_alpha_manual(guide = "none",
                       breaks = c(T, F),
                       values = c(1, .3)) + 
    scale_x_discrete(expand=c(1, 0), labels = c(listName1, listName2)) + 
    scale_y_continuous(breaks = 1 : rdt[, .N], labels = paste0(rdt[, .N] : 1, ". ")) + 
    theme(legend.position="none",
          axis.title=element_blank(),
          axis.line=element_blank(),
          axis.ticks=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          plot.subtitle=element_blank()
          # plot.title=element_blank()
    )
  return(
    thePlot
  )
}

if (FALSE) {
  dt <- fread("~/Downloads/nsv_and_to_for_mar29_meeting.csv")
  
  
  list1 <- dt[type == "Target"] %>%
    .[order(-Share), cat]
  list2 <- dt[type == "Total US"] %>%
    .[order(-Share), cat]
  ggplot(dt, aes(x = factor(cat, levels = list1), 
                 y = Share,
                 fill = Share > 0)) + geom_bar(stat = "identity") + facet_wrap(~type, ncol = 1) + 
    xlab("") + 
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, vjust = .5))
    
  
  plotRankSwaps(list1, list2,
                "Target",
                "Total US") + 
    scale_color_gradient(low = "grey40", high = "orange") + 
    ggtitle("")
  
  
  
  list3 <- dt[type == "Target"] %>%
    .[order(-PCT_of_TO), cat]
  list4 <- dt[type == "Total US"] %>%
    .[order(-PCT_of_TO), cat]
  ggplot(dt, aes(x = factor(cat, levels = list3), 
                 y = PCT_of_TO / 100)) + geom_bar(stat = "identity") + facet_wrap(~type, ncol = 1) + 
    xlab("") + ylab("% of TO") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, vjust = .5))
  
  
  plotRankSwaps(list3, list4,
                "Target",
                "Total US") + 
    scale_color_gradient(low = "grey40", high = "orange") + 
    ggtitle("")
  
  
}
