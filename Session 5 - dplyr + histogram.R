library(dplyr)
library(ggplot2)
library(hflights)

?hflights
# 1.
hflights %>%
  summarize(min_dist=min(Distance),
            max_dist=max(Distance))

# 2.
hflights %>%
  filter(Diverted==1) %>%
  summarize(max_div=max(Diverted))

# 3.
class(hflights$UniqueCarrier)
hflights %>%
  summarize(n_obs=n(),
            n_carrier=n_distinct(UniqueCarrier),
            n_dest=n_distinct(Dest),
            dest100=nth(Dest,100))

# 4.
hflights %>%
  mutate(diff=TaxiOut-TaxiIn) %>%
  filter(!is.na(diff)) %>%
  summarize(avg=mean(diff))

# mean(diff, na.rm = TRUE)
# filter(diff !="NA")


### Extra
# create a new dataset new_data containing Dest, UniqueCarrier, Distance and Actual

new_data = hflights %>%
  select(Dest,UniqueCarrier,Distance,ActualElapsedTime, DepTime) %>%
  mutate(RealTime=ActualElapsedTime+100) %>%
  filter(DepTime>800)

## Find out the number of flights flown by each carrier

# 5.
hflights %>%
  group_by(UniqueCarrier) %>%
  summarize(n_flights = n(),
            n_canc=sum(Cancelled),
            avg_delay=mean(ArrDelay,na.rm =T)) %>%
  arrange(avg_delay,n_canc)


# 6. 7.
diamonds %>%
  group_by(cut) %>%
  summarize(avg=mean(price)) %>%
  ggplot(aes(x=cut,y=avg)) +
  geom_col()


### Histogram
# 8.
View(faithful)

ggplot(faithful,aes(x=waiting)) +
  geom_histogram(fill="lightblue")

# 9.
?birthwt
library(MASS)
View(birthwt)
data=birthwt
str(data)
data$smoke=factor(birthwt$smoke,levels = c("0","1"),labels = c("Nonsmokers","Smokers"))
data$race=factor(birthwt$race,levels = c("1","2","3"),labels = c("white","black","other"))


ggplot(birthwt,aes(x=bwt)) +
  geom_histogram() +
  facet_wrap(~factor(birthwt$smoke,levels = c("0","1"),labels = c("Nonsmokers","Smokers")),nrow = 2) +
  ggtitle("Birth Weight of Infants for Smoker and Nonsmoker Moms")


ggplot(birthwt,aes(x=bwt)) +
  geom_histogram() +
  facet_wrap(~factor(birthwt$race,levels = c("1","2","3"),labels = c("white","black","other")),
             nrow = 3)

ggplot(data,aes(x=bwt, fill=smoke)) +
  geom_histogram(position="identity",alpha=0.5)


?geom_histogram
# 12.

ggplot(faithful,aes(x=waiting)) +
  geom_line(stat = "density")

ggplot(faithful,aes(x=waiting)) +
  geom_line(stat = "density",adjust = 0.25)

## to better explain the changes brought by different parameters
ggplot(faithful,aes(x=waiting)) +
  geom_line(stat = "density") +
  geom_line(stat = "density", adjust=0.25,color="red") +
  geom_line(stat = "density", adjust=2.5,color="green")

?geom_line

