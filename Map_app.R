library(dplyr)
library(ggplot2)
library(ggmap)
library(shiny)

ui <- fluidPage(
  titlePanel(title = "NYC 311 Service Requests on Noises in 2017", windowTitle = "NYC 311 Service"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "type",
        label = "Choose the type of noise to display:",
        choices = c("Commercial", "Helicopter", "House of Worship", "Park", "Residential", "Street/Sidewalk", "Vehicle", "Others" = "")
      ),
      
      sliderInput(
        inputId = "time",
        label = "Choose the interval time bewtween the case was created and closed:",
        min = 0, max = 100, value = 0, step = 1
      ),
      
      selectizeInput(
        inputId = "month",
        label = "Month:",
        choices = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
      ),
      
      checkboxGroupInput(
        inputId = "weekday",
        label = "Day of the Week:",
        choices = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "map")
    )
  )
  
)

server <- function(input, output, session) {
  complaint = reactive({
    data = read.csv("data.csv")
    data %>%
      filter(Complaint.SubType == input$type, Date.Diff == input$time, Created.Month == input$month, Created.Weekday == input$weekday)
  })
  
  output$map = renderPlot({
    qmap("New York City", maptype = "roadmap", zoom = 14, color = "bw") +
      geom_point(data(),aes(x = Longitude, y = Latitude))
  })
}

shinyApp(ui, server)