library(shiny)
library(ggmap)
library(stringr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)

load("Closed.RData")
load("level1.RData")
load("data.RData")
data.agency$Created.Hour=as.factor(data.agency$Created.Hour)
data$Created.Hour = as.factor(data$Created.Hour)
data.agency$Level = as.factor(data.agency$Level)
data.agency$Complaint.SubType = as.factor(data.agency$Complaint.SubType)
data.level1$Complaint.SubType = as.factor(data.level1$Complaint.SubType)
NewYork=qmap("New York",maptype = "roadmap",color="bw")
NewYork2=qmap("New York",maptype = "roadmap",color="bw", zoom = 11)


ui = fluidPage(titlePanel("New York 311 Noise Complaints Analysis 2017"),
  tabsetPanel(
    ## ChangZhou's Tab
    tabPanel("Overview",
            titlePanel("311 Noise Complaints in 2017"), 
            sidebarPanel(
              helpText("This app is to visualize 311 Noise Complaints data"), 
              selectInput(inputId = "pie", label = "Share Pie:", choices = c("Complaint Type","Location Type","Agency","Status")),
              selectInput(inputId = "bar", label = "Rank Bar:",choices=c("City","Problem")),
              radioButtons(inputId = "map",label = "Distribution Map:",choices =list("Time distribution","Location distribution","Location distribution by weekday" ))
            ), 
            mainPanel(
               plotOutput(outputId = "plot1"),
               plotOutput(outputId = "plot2"),
               plotOutput(outputId = "plot3")
               
             )
    ),
  
  ## Yicheng's tab
  tabPanel("Noise Distribution Analysis",
           tabsetPanel(
             
             # Overall
             tabPanel("Noise Distribution Analysis",
                      titlePanel("Noise Distribution Analysis",
                                 windowTitle = "Noise Distribution Analysis"),
              sidebarPanel(
              helpText("Choose the following to display"),
             
              checkboxGroupInput(inputId = "type",
                                label = "Noise Type",
                                choices = list("Commercial","Helicopter","House of Worship","Park",
                                               "Residential","Street/Sidewalk","Vehicle","Others"),
                                selected = c("Commercial","Helicopter","House of Worship","Park",
                                            "Residential","Street/Sidewalk","Vehicle","Others")),
              
              checkboxInput(inputId = "check",
                             label = "Differentiate Types by Color"),
              
              checkboxGroupInput(inputId = "month",
                                 label = "Month",
                                 choices = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                                 selected = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
              
              checkboxGroupInput(inputId = "weekday",
                                label = "Day of Week",
                                choices = list("Sun", "Mon", "Tue", "Wed", "Thu",  "Fri", "Sat"),
                                selected = c("Sun", "Mon", "Tue", "Wed", "Thu",  "Fri", "Sat")),
             
              sliderInput(inputId = "hour",
                         label = "Created Hour",
                         min = 0, max = 24, value = c(0,24))

            ),
            mainPanel(
              plotOutput(outputId = "map1", width = "100%", height = "700px"))),
            
            # Week of Day
            tabPanel("Compare by Day of Week",
                     titlePanel("Noise Distribution Analysis by Week of Day",
                                windowTitle = "Noise Distribution Analysis by Week of Day"),
                     sidebarPanel(
                       helpText("Choose the following to display"),
                       
                       selectizeInput(inputId = "type2",
                                          label = "Noise Type",
                                          choices = list("Commercial","Helicopter","House of Worship","Park",
                                                         "Residential","Street/Sidewalk","Vehicle","Others"),
                                          selected = "Commercial"),
                       sliderInput(inputId = "hour2",
                                   label = "Created Hour",
                                   min = 0, max = 24, value = c(0,24))),
                     
                    mainPanel(
                      plotOutput(outputId = "map2", width = "100%", height = "700px")
                    )),
            
            
            # Month
            tabPanel("Compare by Month",
                     titlePanel("Noise Distribution Analysis by Month",
                                windowTitle = "Noise Distribution Analysis by Month"),
                     sidebarPanel(
                       helpText("Choose the following to display"),
                       
                       selectizeInput(inputId = "type3",
                                      label = "Noise Type",
                                      choices = list("Commercial","Helicopter","House of Worship","Park",
                                                     "Residential","Street/Sidewalk","Vehicle","Others"),
                                      selected = "Commercial"),
                       sliderInput(inputId = "hour3",
                                 label = "Created Hour",
                                 min = 0, max = 24, value = c(0,24))),
                     
                     mainPanel(
                       plotOutput(outputId = "map3", width = "100%", height = "700px")
                     ))
  )),
  
  ## He YOU's tab 1 
  tabPanel("Agency Response Analysis",
    titlePanel("Complaint Importance Analysis",
               windowTitle = "New York 311 Noise Complaint Analysis"),
    sidebarPanel(
      helpText("Choose the following to display"),
      checkboxGroupInput(inputId = "yh1Agency",
                         label = "Responding Agency",
                         choices = list("New York Police Department" = "NYPD",
                                        "Economic Development Corporation"="EDC",
                                        "Environmental" = "DEP"),
                         selected = "NYPD"),
      
      checkboxGroupInput(inputId = "yh1SubType",
                         label = "Noise Type",
                         choices = list("Commercial","Helicopter","House of Worship","Park",
                                        "Residential","Street/Sidewalk","Vehicle","Others"),
                         selected = c("Commercial","Helicopter","House of Worship","Park",
                                      "Residential","Street/Sidewalk","Vehicle","Others")),
      
      checkboxGroupInput(inputId = "yh1Weekday",
                         label = "Day of Week",
                         choices = list("Sun",
                                        "Mon",
                                        "Tue",
                                        "Wed",
                                        "Thu",
                                        "Fri",
                                        "Sat"),
                         selected = "Sun"),
      
      sliderInput(inputId = "yh1Hour",
                  label = "Created Hour",
                  min = 0, max = 24,
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
  tabPanel("Agency Resolution Analysis",
           titlePanel("Agency Relotion Analysis - Level 1",
                      windowTitle = "New York 311 Noise Complaint Analysis"),
           sidebarPanel(
             helpText("Choose the following message to display"),
             checkboxGroupInput(inputId = "yh2SubType",
                                label = "Noise Type",
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
  
  complaint2 = reactive({
    complaint2 = data %>%
      filter(Complaint.SubType %in% input$type2,
             Created.Hour %in% input$hour2)
  })  
  
  complaint3 = reactive({
    complaint = data %>%
      filter(Complaint.SubType %in% input$type3,
             Created.Hour %in% input$hour3)
  })
  
  output$map1=renderPlot({
    if(input$check){
      NewYork2+
        stat_density2d(data=complaint(),aes(x = Longitude, y = Latitude, fill = Complaint.SubType, alpha = ..level..),bins=5,
                       geom = "polygon") +
        guides(fill=guide_legend(title = "Noise Type"))
        #theme(legend.title = "Noise Type")
    }else{
      NewYork2+
        stat_density2d(data=complaint(),aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),bins=5,
                       geom = "polygon") +
        scale_fill_gradient(low= "white", high = "#bd0026") +
        theme(legend.position = 'none')
    }
  })  
  
  output$map2 = renderPlot({
    NewYork2+
      stat_density2d(data = complaint2(),
                   aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
                   geom = "polygon") +
      scale_fill_gradient(low= "white", high = "#bd0026") +
      facet_wrap(~Created.Weekday, nrow = 3) +
      theme(legend.position = 'none')
  })
  
  output$map3 = renderPlot({
    NewYork2+
      stat_density2d(data = complaint3(),
                     aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
                     geom = "polygon") +
      scale_fill_gradient(low= "white", high = "#bd0026") +
      facet_wrap(~Created.Month, nrow = 3) +
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
  
  ## Changzhou's tab
  output$plot1 <- renderPlot(
    if(input$pie=="Complaint Type"){
      data2=data.frame(table(data$Complaint.Type))
      data2$Percentage=paste((round(data2$Freq/sum(data2$Freq),3))*100,"%",sep = "")
      ggplot(data2,aes(x="",y=Freq,fill=Var1))+geom_col(width = 1)+coord_polar(theta = "y")+labs(x=NULL,y = NULL, fill = "Complaint_Type")+theme(axis.ticks = element_blank())+scale_fill_discrete(labels=paste(data2$Var1,data2$Percentage))+theme(axis.text.x = element_blank())+ theme(panel.background = element_rect(fill = NA))
    } 
    else{if(input$pie=="Location Type"){
      data3=data.frame(table(data$Location.Type))
      data3$Percentage=paste((round(data3$Freq/sum(data3$Freq),3))*100,"%",sep = "")
      ggplot(data3,aes(x="",y=Freq,fill=Var1))+geom_col(width = 1)+coord_polar(theta = "y")+labs(x=NULL,y = NULL, fill = "Location_type")+theme(axis.ticks = element_blank())+scale_fill_discrete(labels=paste(data3$Var1,data3$Percentage))+theme(axis.text.x = element_blank())+ theme(panel.background = element_rect(fill = NA))
    }else{if(input$pie=="Agency"){
      data1=data.frame(table(data$Agency))
      data1$Percentage=paste((round(data1$Freq/sum(data1$Freq),3))*100,"%",sep = "")
      ggplot(data1,aes(x="",y=Freq,fill=Var1))+geom_col(width = 1)+coord_polar(theta = "y")+labs(x=NULL,y = NULL, fill = "Agency")+theme(axis.ticks = element_blank())+scale_fill_discrete(labels=paste(data1$Var1,data1$Percentage))+theme(axis.text.x = element_blank())+ theme(panel.background = element_rect(fill = NA))
    }else{
      data4=data.frame(table(data$Status)) 
      data4$Percentage=paste((round(data4$Freq/sum(data4$Freq),3))*100,"%",sep = "")
      ggplot(data4,aes(x="",y=Freq,fill=Var1))+geom_col(width = 1)+coord_polar(theta = "y")+labs(x=NULL,y = NULL, fill = "Status")+theme(axis.ticks = element_blank())+scale_fill_discrete(labels=paste(data4$Var1,data4$Percentage))+theme(axis.text.x = element_blank()) + theme(panel.background = element_rect(fill = NA))
    }
    }
    } 
  )
  
  output$plot2<- renderPlot(
    if(input$bar=="City"){
      data5=data.frame(table(data$City))
      top5=(data5%>%arrange(-Freq))[1:5,]
      ggplot(top5,aes(reorder(Var1,-Freq),Freq))+geom_col(fill="blue")+ggtitle("Top5 cities which received 311")+xlab("")+ylab("")+geom_text(aes(label=Freq), vjust=-0.2)+scale_y_continuous(breaks = seq(5000,150000,by=25000), labels = seq(5000,150000,by=25000))+theme_bw()
    }
    else{
      data6=data.frame(table(data$Descriptor))
      Top5=(data6%>%arrange(-Freq))[1:5,]
      ggplot(Top5,aes(reorder(Var1,-Freq),Freq))+geom_col(fill="yellow")+ggtitle("Top5 311 Problems")+xlab("")+ylab("")+geom_text(aes(label=Freq), vjust=-0.2)+scale_y_continuous(breaks = seq(5000,250000,by=50000), labels = seq(5000,250000,by=50000))+theme_bw()
    }
  )
  
  
  output$plot3<- renderPlot(
    if(input$map=="Time distribution"){
      dataheat=data%>%group_by(Created.Weekday,Created.Hour)%>%summarise(count=n())
      levels(data$Created.Weekday)=c("Sun","Mon","Tue","Wed","Thurs","Fri","Sat")
      myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
      ggplot(dataheat,aes(x=Created.Weekday,y=Created.Hour,fill=count))+geom_tile()+scale_fill_gradientn(colours = myPalette(256))+ggtitle("Heatmap of New York 311 service Day V.S. Hour")+xlab("Weekday")+ylab("Hour of the day")+theme_bw()+theme(panel.border=element_blank()) + theme(axis.ticks = element_line(linetype = "blank"))
    }
    else{
      if(input$map=="Location distribution"){
        Newyork1=qmap(location = "New York", zoom = 10, maptype ="roadmap", color="bw")
        Newyork1+geom_point(data=data,aes(x=Longitude, y=Latitude),color="darkblue", alpha=0.01,size= 0.5)
      }
      else{
        Newyork2 <- get_map(location = "New York", zoom = 13, color = "bw")
        NewyorkMap <- ggmap(Newyork2)
        NewyorkMap+
          geom_point(data=data,aes(x=Longitude, y=Latitude),color="darkblue", alpha=0.01)+facet_wrap(~Created.Weekday, nrow = 2)+theme(legend.position = 'none')+ ggtitle("311 calls distribution by Day of the Week")
      }
    }
  )
  
 

  
  
  
  }
  
    
        
  
  
  
  

shinyApp(ui, server)












