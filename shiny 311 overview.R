library(shiny)
library(ggmap)
library(maps)
library(stringr)
library(lubridate)
library(dplyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)

data = read.csv("311_Service_Requests_2017.csv",na.strings = "")


str(data)

## Add a new column named "Complaint.SubType" with subtypes of complaint subtracted from Complaint.Type

data$Complaint.Type = as.character(data$Complaint.Type)
data$Complaint.SubType = str_sub(data$Complaint.Type,start = 9)
data$Complaint.SubType = as.factor(data$Complaint.SubType)
data$Complaint.Type = as.factor(data$Complaint.Type)



data$Created.Date = mdy_hms(data$Created.Date)
data$Closed.Date = mdy_hms(data$Closed.Date)
data$Due.Date = mdy_hms(data$Due.Date)
data$Resolution.Action.Updated.Date = mdy_hms(data$Resolution.Action.Updated.Date)


## Add new columns named Created.Month, Created.Weekday, Created.Time and Created.Hour to store the month, day of the week, time and hour of the day for created date of the complaint.


data$Created.Month = month(data$Created.Date,label = T, abbr = T)
data$Created.Weekday = wday(data$Created.Date,label = T, abbr = T)
data$Created.Time = str_sub(data$Created.Date,start = 12)
data$Created.Time = hms(data$Created.Time)
data$Created.Hour = hour(data$Created.Date)

## Add a new column named Date.Diff to calculate the difference between created date and closed data in seconds.
data$Date.Diff_day = difftime(data$Closed.Date,data$Created.Date,units = "days")


## Select columns that will be used

data = data %>%
  select(-c(Taxi.Company.Borough:Bridge.Highway.Segment,Street.Name:Intersection.Street.2,BBL:Vehicle.Type))

##Save data
save(data,file = "~/Desktop/Data.RData")

#user interface
ui <- fluidPage(
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
)

###server
server <- function(input, output) {
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
      Newyork=qmap(location = "New York", zoom = 10, maptype ="roadmap", color="bw")
      Newyork+geom_point(data=data,aes(x=Longitude, y=Latitude),color="darkblue", alpha=0.01,size= 0.5)
    }
      else{
      Newyork <- get_map(location = "New York", zoom = 13, color = "bw")
      NewyorkMap <- ggmap(Newyork)
      NewyorkMap+
        geom_point(data=data,aes(x=Longitude, y=Latitude),color="darkblue", alpha=0.01)+facet_wrap(~Created.Weekday, nrow = 2)+theme(legend.position = 'none')+ ggtitle("311 calls distribution by Day of the Week")
      }
    }
  )
}
  

shinyApp(ui, server)