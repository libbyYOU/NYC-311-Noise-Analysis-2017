library(MASS)
data=birthwt

str(data)
data$race=as.factor(data$race)
levels(data$race)=c('White',"Black","Others")

library(ggplot2)
ggplot(data,aes(x=race,y=bwt)) +
  geom_boxplot(width=0.5,
               outlier.colour = "red",
               outlier.shape = 15,
               outlier.size = 3) +
  stat_summary(fun.y = "mean",geom = "point",color="blue",shape=23) +
  annotate("text",x="White",y=2000,label="Hi")

ggplot(data,aes(x=1,y=bwt)) +
  geom_boxplot()+
  scale_x_continuous(breaks=NULL) +
  xlab("")

ggplot(data,aes(x=1,y=bwt)) +
  geom_boxplot()+
  scale_x_continuous(breaks=NULL) +
  xlab("") +
  scale_y_continuous(name = "Birthweight (000's)",
    breaks = seq(1000,5000,1000),
    labels = 1:5)


## line graph
BOD
ggplot(BOD,aes(x=Time,y=demand)) +
  geom_line() +
  scale_x_continuous(breaks = 1:7,
                     labels = 1:7) +
  geom_point(aes(x=BOD[5,1],y=BOD[5,2]),color="red") +
  geom_vline(xintercept = 3,
             color="blue",
             linetype="dashed") +
  expand_limits(x=10,y=30) +  ## expand limit on axis to include a number
  theme_dark()

?geom_line
### toothgrowth

library(dplyr)
?ToothGrowth
View(ToothGrowth)
## for each combination of supp and dose
## find the average length of teeth

tg = ToothGrowth %>%
  group_by(supp,dose) %>%
  summarise(avg_len=mean(len))

ggplot(tg,aes(x=dose,y=avg_len,col=supp)) +
  geom_line()
  

ggplot(tg,aes(x=dose,y=avg_len,group=supp)) +
  geom_line()


## stacked area graph
library(gcookbook)
View(uspopage)

ggplot(uspopage,aes(x=Year,y=Thousands,
                    fill=AgeGroup)) +
  geom_area()


uspopage_perc=uspopage %>%
  group_by(Year) %>%
  mutate(Percent=Thousands/sum(Thousands)*100)

ggplot(uspopage_perc,aes(x=Year,y=Percent,fill=AgeGroup)) +
  geom_area()

## surprise
t = seq(0,100,len=1000)
x = 16*sin((t))^3
y = 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
data=data.frame(x,y)

ggplot(data,aes(x,y)) +
  geom_polygon(fill="red") +
  theme_void()







