library(shiny)
library(plotly)
library(DT)

ui <- fluidPage(
  titlePanel("Sales Performance Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Select Region(s):", 
                  choices = c("North", "South", "East", "West"), 
                  selected = "North", multiple = TRUE),
      checkboxGroupInput("category", "Select Product Category:", 
                         choices = c("Technology", "Office Supplies", "Furniture"), 
                         selected = c("Technology", "Furniture")),
      dateRangeInput("date", "Select Date Range:",
                     start = "2023-01-01", end = "2023-12-31"),
      downloadButton("downloadData", "Download Filtered Data")
    ),
    mainPanel(
      plotlyOutput("salesPlot"),
      plotlyOutput("pieChart"),
      dataTableOutput("topProducts"),
      verbatimTextOutput("summaryStats")
    )
  )
)
