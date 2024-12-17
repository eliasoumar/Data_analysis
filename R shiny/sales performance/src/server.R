server <- function(input, output) {
  # Sample data (replace with your dataset)
  sales_data <- data.frame(
    OrderDate = seq.Date(from = as.Date("2023-01-01"), 
                         to = as.Date("2023-12-31"), by = "day"),
    Region = sample(c("North", "South", "East", "West"), 365, replace = TRUE),
    Category = sample(c("Technology", "Office Supplies", "Furniture"), 365, replace = TRUE),
    ProductName = sample(paste("Product", 1:50), 365, replace = TRUE),
    Sales = runif(365, 100, 500),
    Profit = runif(365, 10, 50)
  )
  
  # Reactive filtered data
  filtered_data <- reactive({
    req(input$region, input$category, input$date)
    sales_data %>%
      filter(Region %in% input$region, 
             Category %in% input$category,
             OrderDate >= input$date[1] & OrderDate <= input$date[2])
  })
  
  # Line chart: Sales over time
  output$salesPlot <- renderPlotly({
    data <- filtered_data()
    plot_ly(data, x = ~OrderDate, y = ~Sales, type = 'scatter', mode = 'lines+markers',
            color = ~Region, text = ~paste("Category:", Category),
            hoverinfo = "text+x+y") %>%
      layout(title = "Sales Over Time", xaxis = list(title = "Date"), yaxis = list(title = "Sales"))
  })
  
  # Pie chart: Sales by category
  output$pieChart <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Category) %>%
      summarise(TotalSales = sum(Sales))
    plot_ly(data, labels = ~Category, values = ~TotalSales, type = 'pie') %>%
      layout(title = "Sales Distribution by Category")
  })
  
  # Top products table
  output$topProducts <- renderDataTable({
    filtered_data() %>%
      group_by(ProductName) %>%
      summarise(TotalSales = sum(Sales)) %>%
      arrange(desc(TotalSales)) %>%
      head(10)
  })
  
  # Summary statistics
  output$summaryStats <- renderText({
    data <- filtered_data()
    paste(
      "Total Sales: $", round(sum(data$Sales), 2), "\n",
      "Average Monthly Sales: $", round(mean(data$Sales), 2)
    )
  })
  
  # Download filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_sales_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}
