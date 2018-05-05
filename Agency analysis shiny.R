library(shiny)
library(dplyr)
library(ggmap)
library(ggplot2)

load("Closed.RData")
load("level1.RData")
data.agency$Created.Hour=as.factor(data.agency$Created.Hour)
data.agency$Level = as.factor(data.agency$Level)
data.agency$Complaint.SubType = as.factor(data.agency$Complaint.SubType)
NewYork=qmap("New York",maptype = "roadmap",color="bw")

ui = navbarPage("New York City Noise Complaints Analysis 2017",
                   tabPanel(titlePanel(title = "Agency Response Analysis",windowTitle = "New York City 311 Noise Complaints Analysis"),
                            sidebarLayout(
                              sidebarPanel(
                                helpText("Choose the following message to display"),
                                checkboxGroupInput(inputId = "Agency",
                                                   label = "Choose a Responding Agency",
                                                   choices = list("New York Police Department" = "NYPD",
                                                                  "Economic Development Corporation"="EDC",
                                                                  "Environmental" = "DEP"),
                                                   selected = "NYPD"),
                                checkboxGroupInput(inputId = "SubType",
                                                   label = "Noise Subtype",
                                                   choices = list("Commercial","Helicopter","House of Worship","Park",
                                                                  "Residential","Street/Sidewalk","Vehicle","Others"),
                                                   selected = "Commercial"),
                                
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
                            
                            ),
                   tabPanel("Overview")
                            )
                     



server=function(input,output) {
  dataset=reactive({
    data.agency %>%
      filter(Agency%in%input$Agency,Created.Weekday==input$Weekday,Complaint.SubType==input$SubType)
  })
  
  
  
  output$map=renderPlot({
    NewYork+
      stat_density2d(data=dataset(),aes(x=Longitude, y = Latitude,fill=..level.., alpha=..level..),bins=5,
                     geom = "polygon") + 
      facet_wrap(~Level)
      scale_fill_gradient(low = "black",high = "red") +
      theme(legend.position = 'none')
    
  })
  }
  
    
        
  
  
  
  
  




shinyApp(ui, server)












