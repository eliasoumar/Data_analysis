library(shiny)
data(airquality)

server <- function(input, output) {
  output$displot <- renderPlot({
    x <- airquality$Ozone
    x <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#75AADB", border = "black",
         xlab = "Ozone level",
         ylab = "Frequency",
         main = "Histogram of Ozone Levels")
  })
}
