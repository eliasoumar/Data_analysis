library(shiny)
data(airquality)

ui <- fluidPage(
  
  titlePanel("Niveau d'oxygene zone"),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput(
        
        inputId= "bins",
        label= "Nombre de bins",
        min = 10,
        max = 50,
        value = 15
      )
    ),
    
    mainPanel(
      
      plotOutput(outputId = "displot")
      
    )
  )
)

server<- function(input, output){
  output$displot <- renderPlot({
    x <- airquality$Ozone
    x <- na.omit(x)
    bins <- seq(min(x), max(x), length.out =input$bins + 1)
    
    
    hist(x, breaks = bins, col = "red", border = "black",
         xlab = "Ozone level",
         ylab = "Frequency",
         main = "Histogram of Ozone Levels")
    
  } )
}
 

shinyApp(ui = ui, server = server)

shiny::runApp()

getwd()
