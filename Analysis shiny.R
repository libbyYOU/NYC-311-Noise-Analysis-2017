library(shiny)
library(dplyr)
library(ggmap)
library(ggplot2)

load("Closed.RData")
load("level1.RData")
load("data.RData")
data.agency$Created.Hour=as.factor(data.agency$Created.Hour)
data$Created.Hour = as.factor(data$Created.Hour)
data.agency$Level = as.factor(data.agency$Level)
data.agency$Complaint.SubType = as.factor(data.agency$Complaint.SubType)
data.level1$Complaint.SubType = as.factor(data.level1$Complaint.SubType)
NewYork=qmap("New York",maptype = "roadmap",color="bw")


ui = fluidPage(titlePanel("New York 311 Noise Complaints Analysis 2017"),
  tabsetPanel(
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
             
             sliderInput(inputId = "hour",
                         label = "Created Hour",
                         min = 0, max = 23, value = c(8,12)),
            
             selectizeInput(inputId = "month",
                            label = "Month",
                            choices = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
           ),
           mainPanel(
             plotOutput(outputId = "map1", width = "100%", height = "700px"))),
  
  ## He YOU's tab 1 
  tabPanel("Agency Response Analysis",
    titlePanel("Complaint Importance Analysis",
               windowTitle = "New York 311 Noise Complaint Analysis"),
    sidebarPanel(
      helpText("Choose the following message to display"),
      checkboxGroupInput(inputId = "yh1Agency",
                         label = "Responding Agency",
                         choices = list("New York Police Department" = "NYPD",
                                        "Economic Development Corporation"="EDC",
                                        "Environmental" = "DEP"),
                         selected = "NYPD"),
      
      checkboxGroupInput(inputId = "yh1SubType",
                         label = "Noise Subtype",
                         choices = list("Commercial","Helicopter","House of Worship","Park",
                                        "Residential","Street/Sidewalk","Vehicle","Others"),
                         selected = "Commercial"),
      
      checkboxGroupInput(inputId = "yh1Weekday",
                         label = "Weekday",
                         choices = list("Sun",
                                        "Mon",
                                        "Tue",
                                        "Wed",
                                        "Thur",
                                        "Fri",
                                        "Sat"),
                         selected = "Sun"),
      
      sliderInput(inputId = "yh1Hour",
                  label = "Created Hour",
                  min = 0, max = 23,
                  value = c(8,12)),
      
      checkboxGroupInput(inputId = "yh1Level",
                         label = "Importance Level",
                         choices = list("Level 1:Response or investigation demanded"="Level_1",
                                        "Level 2:Solved or closed via referring or contacting"="Level_2",
                                        "Level 3:Response unnecessary"="Level_3"),
                         selected = "Level_1")
      ),
    mainPanel(
      plotOutput(outputId = "mapyh1",width = "100%", height = "700px"))),
  
  ### HeYou's tab 2
  tabPanel("Agency Relotion Analysis",
           titlePanel("Agency Relotion Analysis - Level 1",
                      windowTitle = "New York 311 Noise Complaint Analysis"),
           sidebarPanel(
             helpText("Choose the following message to display"),
             checkboxGroupInput(inputId = "yh2SubType",
                                label = "Complaint Subtype",
                                choices = list("Commercial","Helicopter","House of Worship","Park",
                                               "Residential","Street/Sidewalk","Vehicle","Others"),
                                selected = c("Commercial","Helicopter","House of Worship","Park",
                                             "Residential","Street/Sidewalk","Vehicle","Others"))),
           mainPanel(
             verticalLayout(
               h5("Problem Resolution Status of Level 1", align = "center"),
               plotOutput(outputId = "yhcolumn1"),
               h5("Resolution Details for cases with specific solutions", align = "center"),
               plotOutput(outputId = "yhcolumn2"),
               h5("Resolution Details for cases with no specific solution", align = "center"),
               plotOutput(outputId = "yhcolumn3")
           ))
           )
           
           
           )
             
             
             )
           
  

  
                     


server=function(input,output) {
  ### Yicheng's output
  complaint = reactive({
    complaint = data %>%
      filter(Complaint.SubType %in% input$type, 
             Created.Month == input$month, 
             Created.Weekday %in% input$weekday,
             Created.Hour %in% input$hour)
  })  
  
  output$map1=renderPlot({
    NewYork+
      stat_density2d(data=complaint(),aes(x=Longitude, y = Latitude,fill=..level.., alpha=..level..),bins=5,
                     geom = "polygon") +
      theme(legend.position = 'none')
  })        
  ### He YOU's tab 1 results
  datasetyh1=reactive({
    data.agency %>%
      filter(Agency%in%input$yh1Agency,
             Created.Weekday %in% input$yh1Weekday,
             Complaint.SubType %in% input$yh1SubType,
             Created.Hour %in% input$yh1Hour,
             Level %in% input$yh1Level)
  })
  
  output$mapyh1=renderPlot({
    
    NewYork+
      stat_density2d(data=datasetyh1(),aes(x=Longitude, y = Latitude,fill=Level, alpha=..level..),bins=5,
                     geom = "polygon") 
    
  })
  
  ### He YOU's tab 2 results
  datasetyh2=reactive({
    data.level1 %>%
      filter(Complaint.SubType%in%input$yh2SubType)
  })
  
  output$yhcolumn1=renderPlot({
    ggplot(datasetyh2(),aes(x=Complaint.SubType,fill=Problem.Solution)) +
      geom_bar() +
      xlab("Complaint Subtype") +
      ylab("Total Count")
  })
  datasetyh3=reactive({
    data.level1 %>%
      filter(Complaint.SubType%in%input$yh2SubType,Problem.Solution=="Yes")
  })
  output$yhcolumn2=renderPlot({
    ggplot(datasetyh3(),aes(x=Complaint.SubType,fill=Result.Detail)) +
      geom_bar() +
      xlab("Complaint Subtype") +
      ylab("Total Count")
  })
  
  datasetyh4=reactive({
    data.level1 %>%
      filter(Complaint.SubType%in%input$yh2SubType,Problem.Solution=="No")
  })
  
  output$yhcolumn3=renderPlot({
    ggplot(datasetyh4(),aes(x=Complaint.SubType,fill=Result.Detail)) +
      geom_bar() +
      xlab("Complaint Subtype") +
      ylab("Total Count")
  })
  
 

  
  
  
  }
  
    
        
  
  
  
  

shinyApp(ui, server)












