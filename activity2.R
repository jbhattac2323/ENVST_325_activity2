#IN Class -----
#install.packages(c("dplyr","lubridate")) 
#install only those packages you need- only need to run it once
library(dplyr)
library(lubridate)

streamH <-read.csv('/cloud/project/activtiy02/stream_gauge.csv')
#read.csv makes code reproducible
siteInfo <- read.csv('/cloud/project/activtiy02/site_info.csv')
#ymd_hm

streamH$dateF <- ymd_hm(streamH$datetime)
streamH$year <- year(streamH$dateF)


peaceH <- streamH %>%
  filter(siteID == 2295637) #is equal to #rows in a column meeting the filter criteria

plot(peaceH$dateF, peaceH$gheight.ft, type ='b',pch = 19, xlab="Date",ylab="Stream Height (ft)")

#Prompt 1
floods <- full_join(streamH,siteInfo, by="siteID") #by is common identifier

height.ave<- floods%>%
  group_by(names) %>%
  summarise(mean.height = mean(gheight.ft))

#Prompt 2
floods$doy <- yday(floods$dateF)

height.day <- floods %>%
  group_by(names, doy) %>%
  summarise(mean.height =mean(gheight.ft))

max.cat<- floods %>%
  group_by(names)%>%
  filter(gheight.ft >= major.ft)%>%
  summarise(n.major=n())
#Prompt 3
earliest.flood<- floods%>%
  filter(gheight.ft>=flood.ft)%>%
  group_by(names)%>%
  summarise(min_date=min(dateF))

#Homewwork -----
#Q1: Plot each river
#Peace River
plot(peaceH$dateF, peaceH$gheight.ft, type ='b',pch = 19, main='PEACE RIVER', xlab="Date",ylab="Stream Height (ft)")
#Withriver
with.river <- floods%>%
  filter(siteID==2312000)
plot(with.river$dateF, with.river$gheight.ft, type ='b',pch = 19,main = "WITHLACOOCHEE RIVER", xlab="Date",ylab="Stream Height (ft)")

#FishEating Creek
fish<-floods%>%
  filter(siteID==2256500)
plot(fish$dateF, fish$gheight.ft, type ='b', main = "FISHEATING CREEK RIVER", xlab="Date",ylab="Stream Height (ft)")

#Santa River
santa <- floods%>%
  filter(siteID==2322500)
plot(santa$dateF, santa$gheight.ft, type ='b', main = "Santa Fe River", xlab="Date",ylab="Stream Height (ft)")


#Q2 Earliest Flood for each catregory
#A. Action
early.action<- floods%>%
  filter(gheight.ft>= action.ft & gheight.ft < flood.ft)%>%
  group_by(names)%>%
  summarise(min_date=min(dateF))

#B. Flood
early.flood<- floods%>%
  filter(gheight.ft>=flood.ft & gheight.ft < moderate.ft)%>%
  group_by(names)%>%
  summarise(min_date=min(dateF))

#C. Moderate 
early.moderate<-floods%>%
  filter(gheight.ft>=moderate.ft & gheight.ft < major.ft)%>%
  group_by(names)%>%
  summarise(min_date=min(dateF))

#D. Major
early.major<-floods%>%
  filter(gheight.ft>=major.ft)%>%
  group_by(names)%>%
  summarise(min_date=min(dateF))


#Q3 Highest Stream Stage
#Overall highest recorded (extra step)
highest.major<-floods%>%
  filter(gheight.ft>=major.ft)%>%
  group_by(names)%>%
  summarise(max_height=max(gheight.ft))
#Peace River at 23.85ft but this is overall- lets check net increase from major stage baseline
#Net increase- what the question asks for
highest.increase<-floods%>%
  filter(gheight.ft>=major.ft)%>%
  group_by(names)%>%
  summarise(max_increase=max(gheight.ft-major.ft)) #net increase
#Answer:Peace River: 7.85ft more than major stage baseline

#Q4 
help(select, dplyr)
help("ifelse")
help("hist")
