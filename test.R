library(shiny)
library(dplyr)
library(ggmap)
library(ggplot2)

levels(data.agency$Complaint.SubType)

load("Closed.RData")
load("level1.RData")
data.agency$Created.Hour=as.factor(data.agency$Created.Hour)
data.agency$Level = as.factor(data.agency$Level)
data.agency$Complaint.SubType = as.factor(data.agency$Complaint.SubType)
NewYork=qmap(location = "New York",maptype = "roadmap",color="bw")


ui = fluidPage(titlePanel(title = "11",windowTitle = "New York City 311 Noise Complaints Analysis"),
               sidebarLayout(
                 sidebarPanel(
                   helpText("Choose the following message to display"),
                   checkboxGroupInput(inputId = "Agency",
                                 label = "Choose a Responding Agency",
                                 choices = list("NYPD","EDC","DEP")),
                   checkboxGroupInput(inputId = "SubType",
                                      label = "Noise Subtype",
                                      choices = list("Commercial","Helicopter","House of Worship","Park",
                                                     "Residential","Street/Sidewalk","Vehicle","Others")),
                                      
                   selectInput(inputId = "Weekday",
                               label = "Choose a variable to display",
                               choices = list("Sun",
                                           "Mon",
                                           "Tue",
                                           "Wed",
                                           "Thur",
                                           "Fri",
                                           "Sat"))),
                 mainPanel(
                   plotOutput(outputId = "map")
                 )
              
                 )
               )
               
               
server=function(input,output) {

dataset=reactive({
    data.agency %>%
      filter(Agency%in%input$Agency,Created.Weekday==input$Weekday)
  })
  
 
  
output$map=renderPlot({
  NewYork+
    stat_density2d(data=dataset(),aes(x=Longitude, y = Latitude,fill=..level.., alpha=..level..),bins=5,
                   geom = "polygon") + 
    scale_fill_gradient(low = "black",high = "red") +
    theme(legend.position = 'none')
  
})
}



shinyApp(ui, server)
