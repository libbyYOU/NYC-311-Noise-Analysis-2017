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

###ui 
ui = fluidPage(tabsetPanel(
  ## ChangZhou's Tab
  tabPanel("Overview",
           titlePanel("311 Service Request in 2017"), 
    sidebarPanel(
      helpText("This app is to visualize 311 Service Request data"), 
      selectInput(inputId = "pie", label = "Share Pie:", choices = c("Complaint Type","Location Type","Agecny","Status")),
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
                     


###server 
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
      ggplot(Top5,aes(reorder(Var1,-Freq),Freq))+geom_col(fill="yellow")+ggtitle("Top5 311 Descriptors")+xlab("")+ylab("")+geom_text(aes(label=Freq), vjust=-0.2)+scale_y_continuous(breaks = seq(5000,250000,by=50000), labels = seq(5000,250000,by=50000))+theme_bw()
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












