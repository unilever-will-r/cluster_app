projectProgressPlot <- function(progressDt, title = "What have we been up to?") {
  requiredColumnNames <- c("name", "start_date", "end_date")
  checkRequiredColumnNames(progressDt, requiredColumnNames)
  progressDtLong <- data.table::melt(progressDt, id.vars = "name", variable.name = "event", value.name = "date")
  ggplot(progressDtLong, aes(x = date, y = 0, color = event)) +
    geom_point(size = 5) + geom_line(aes(group = name), color = "black", size = 2) + 
    # scale_color_manual(values = c("blue", "red")) +  # Change colors if desired
    labs(x = "Date", y = "Name", color = "Event", 
         title = "What has Will been up to?") + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = as.Date(c("2023-01-15", "2023-12-01"))) + 
    facet_wrap(~name, ncol = 1) + 
    theme_minimal() + 
    theme(axis.title = element_blank(),
          legend.position = "none",
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(angle = 45),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) 
}


if (FALSE) {
  progressDt <- data.table(name = c("Onboarding Jason",
                                    "Hivery engagement",
                                    "CDA show and tell"),
                           start_date = as.Date(c("2023-05-09",
                                                  "2023-01-15",
                                                  "2023-03-01")),
                           end_date = as.Date(c("2023-07-31",
                                                "2023-08-30",
                                                "2023-12-01"))
  )
  
  projectProgressPlot(progressDt, "What has Will been up to?")

}