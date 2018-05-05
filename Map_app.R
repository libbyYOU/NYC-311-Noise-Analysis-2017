library(dplyr)
library(ggplot2)
library(ggmap)
library(shiny)


load("data.Rdata")

data0 = data %>%
  select(Complaint.SubType, Date.Diff_day, Created.Month, Created.Weekday, Longitude, Latitude) %>%
  filter(!is.na(Date.Diff_day), !is.na(Created.Month), !is.na(Created.Weekday), !is.na(Longitude), !is.na(Latitude))

data0$Date.Diff_day = as.numeric(data0$Date.Diff_day)

ui <- fluidPage(
  titlePanel(title = "NYC 311 Service Requests on Noises in 2017", windowTitle = "NYC 311 Service"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Choose the following to display:"),
      
      checkboxGroupInput(
        inputId = "type",
        label = "Type of noise",
        choices = levels(data0$Complaint.SubType),
        selected = levels(data0$Complaint.SubType)
      ),
      
      selectizeInput(
        inputId = "month",
        label = "Month",
        choices = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
      ),
      
      checkboxGroupInput(
        inputId = "weekday",
        label = "Day of the Week",
        choices = levels(data0$Created.Weekday),
        selected = levels(data0$Created.Weekday)
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "map", width = "100%", height = "700px")
    )
  )
  
)

server <- function(input, output, session) {
  complaint = reactive({
    complaint = data0 %>%
      filter(Complaint.SubType == input$type, 
             Created.Month == input$month, 
             Created.Weekday == input$weekday)
  })
  
  output$map = renderPlot({
    qmap("New York", maptype = "roadmap", color = "bw") +
      geom_point(data = complaint(),aes(x = Longitude, y = Latitude))
  })
}

shinyApp(ui, server)