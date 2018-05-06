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
importance = levels(data.agency$Level)

load("data.Rdata")
data0 = data %>%
  select(Complaint.SubType, Date.Diff_day, Created.Month, Created.Weekday, Longitude, Latitude) %>%
  filter(!is.na(Date.Diff_day), !is.na(Created.Month), !is.na(Created.Weekday), !is.na(Longitude), !is.na(Latitude))
data0$Date.Diff_day = as.numeric(data0$Date.Diff_day)

ui = fluidPage(tabsetPanel(
  ## First Tab
  tabPanel("Overview"),
  
  ## Yicheng's tab
  tabPanel("Noise Distribution Analysis",
    titlePanel("Noise Distribution Analysis",
               windowTitle = "Noise Distribution Analysis"),
    sidebarPanel(
      helpText("Choose the following message to display"),
      
      checkboxGroupInput(inputId = "type",
                         label = "Noise Subtype",
                         choices = list("Commercial","Helicopter","House of Worship","Park",
                                        "Residential","Street/Sidewalk","Vehicle","Others")
                         ),
      
      checkboxGroupInput(inputId = "weekday",
                         label = "Choose a weekday to display",
                         choices = list("Sun", "Mon", "Tue", "Wed", "Thur",  "Fri", "Sat")),
      
      selectizeInput(inputId = "month",
                     label = "Month",
                     choices = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
    ),
    mainPanel(
      plotOutput(outputId = "map1", width = "100%", height = "700px"))),
  
  
  ## He YOU's tab 1 
  tabPanel("Agency Response Analysis",
    titlePanel("Complaint Importance Analysis",
               windowTitle = "New York 311 Noise Complaint"),
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
                  label = "Choose a weekday to display",
                  choices = list("Sun",
                                 "Mon",
                                 "Tue",
                                 "Wed",
                                 "Thur",
                                 "Fri",
                                 "Sat")),
      checkboxGroupInput(inputId = "Hour",
                         label = "Choose created hour to display",
                         choices = list("0","1","2","3","4","5","6","7","8","9","10",
                                        "11","12","13","14","15","16","17","18","19",
                                        "20","21","22","23"),
                         selected = "0"),
      checkboxGroupInput(inputId = "Level",
                         label = "Choose importance level to display",
                         choices = list("Level 1:Response or investigation demanded"="Level_1",
                                        "Level 2:Solved or closed via referring or contacting"="Level_2",
                                        "Level 3:Response unnecessary"="Level_3"),
                         selected = "Level_1")
      ),
    mainPanel(
      plotOutput(outputId = "map")))
  ))
                     



server=function(input,output) {
  dataset=reactive({
    data.agency %>%
      filter(Agency%in%input$Agency,Created.Weekday==input$Weekday,Complaint.SubType==input$SubType,
             Created.Hour%in%input$Hour,Level%in%input$Level)
  })
  
  complaint = reactive({
    complaint = data0 %>%
      filter(Complaint.SubType %in% input$type, 
             Created.Month == input$month, 
             Created.Weekday %in% input$weekday)
  })  
  
  output$map=renderPlot({
    #fill_vector=input$Level
    NewYork+
      stat_density2d(data=dataset(),aes(x=Longitude, y = Latitude,fill=Level, alpha=..level..),bins=5,
                     geom = "polygon") +
      theme(legend.position = 'none')
    })  
    
  output$map1=renderPlot({
    NewYork+
      stat_density2d(data=complaint(),aes(x=Longitude, y = Latitude,fill=..level.., alpha=..level..),bins=5,
                      geom = "polygon") +
      theme(legend.position = 'none')
    })        

  }
  
    
        
  
  
  
  
  




shinyApp(ui, server)












