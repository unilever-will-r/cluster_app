# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Generate example input table
generate_data <- function() {
  data <- data.frame(
    year = rep(2019 : 2023, each = 3),
    manuf = rep(c("NESTLE", "P AND G", "LOREAL"), times = 5),
    USG = rnorm(15, 0, 1) - seq(1, 15),
    USV = rnorm(15, 1, 0.5) - seq(1, 15)
  )
  return(data)
}

# UI
ui <- fluidPage(
  selectInput("value", "What value are you interested in?", choices = c("USG", "USV")),
  actionButton("action", "plot"),
  plotOutput("plot")
)

# Server
server <- function(input, output) {
  observeEvent(input$action, {
    data <- generate_data()
    value_column <- input$value
    
    output$plot <- renderPlot({
      ggplot(data, aes_string(x = "year", y = value_column, color = "manuf")) +
        geom_line() +
        labs(x = "Year", y = value_column) +
        scale_x_continuous(breaks = 2019:2023) +
        ggtitle(paste("Plot of", value_column, "across years by manufacturer"))
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)

