library(shiny)

ui <- fluidPage(
  titlePanel("Niveau de Ozone"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "bins", label = "Number of bins", min = 1, max = 30, value = 15)
    ),
    mainPanel(
      plotOutput(outputId = "displot")
    )
  )
)
