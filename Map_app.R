library(dplyr)
library(ggplot2)
library(ggmap)
library(shiny)

ui <- fluidPage(
  titlePanel(title = "NYC 311 Service Requests on Noises in 2017", windowTitle = "NYC 311 Service"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Choose the following to display:"),
      
      checkboxGroupInput(
        inputId = "type",
        label = "Type of noise",
        choices = c("Commercial", "Helicopter", "House of Worship", "Park", "Residential", "Street/Sidewalk", "Vehicle", "Others" = ""),
        selected = c("Commercial", "Helicopter", "House of Worship", "Park", "Residential", "Street/Sidewalk", "Vehicle", "")
      ),
      
      sliderInput(
        inputId = "time",
        label = "Duration time in days",
        min = 1, max = 218, value = 1, step = 1
      ),
      
      selectizeInput(
        inputId = "month",
        label = "Month",
        choices = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
      ),
      
      checkboxGroupInput(
        inputId = "weekday",
        label = "Day of the Week",
        choices = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
        selected = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "map", width = "100%", height = "700px")
    )
  )
  
)

server <- function(input, output, session) {
  complaint = reactive({
    complaint = data %>%
      filter(Complaint.SubType == input$type, 
             as.numeric(Date.Diff_day, rm.na = T) <= input$time, 
             Created.Month == input$month, 
             Created.Weekday == input$weekday)
  })
  
  output$map = renderPlot({
    qmap("New York", maptype = "roadmap", color = "bw") +
      geom_point(data = complaint(),aes(x = Longitude, y = Latitude))
  })
}

shinyApp(ui, server)