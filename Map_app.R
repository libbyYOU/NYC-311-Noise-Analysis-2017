library(shiny)

ui <- fluidPage(
  titlePanel(title = "NYC 311 Service Requests on Noises in 2017", windowTitle = "NYC 311 Service"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "type",
        label = "Choose the type of noise to display:",
        choices = c("All", "Commercial", "Helicopter", "House of Worship", "Park", "Residential", "Street/Sidewalk", "Vehicle"),
        selected = "All"
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "map")
    )
  )
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)