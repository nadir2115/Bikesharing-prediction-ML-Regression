# Machine learning Regression project-predicting bike usage- Nadir Nibras

# clear workspace variables
rm(list = ls());
# it means ctrl+L. clear window
cat("\014") 
# close all plots
graphics.off() 


library(MASS)
library(readr)
library(datasets)
library(ggplot2)
library(ggExtra)


# set directory to R script folder
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# load datasets
rawData<- read_csv("hour.csv")
processedData <- read_csv("bikehour.csv")

# plot  atemp
p<-ggplot(processedData,aes(x=atemp,y=count))+
  geom_point(alpha=0.07, color='green')+
  labs(x='Adjusted Temperature', y= 'Hourly Usage Count')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))

# plot temp
p<-ggplot(rawData,aes(x=temp,y=count))+
  geom_point(alpha=0.07,color='orange')+
  labs(x='Temperature', y= 'Hourly Usage Count')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))

# plot humidity
p<-ggplot(processedData,aes(x=hum,y=count))+
  geom_point(alpha=0.07, color='violet')+
  labs(x='Humidity', y= 'Hourly Usage Count')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))

# plot weather situation
p<-ggplot(rawData,aes(x=weathersit,y=cnt))+
  geom_count(color='yellow')+
  geom_point(alpha=0.07, color='green')+
  labs(x='Weather situation', y= 'Hourly Usage Count')+
  geom_smooth(method='loess')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))


# plot wind-speed
p<-ggplot(rawData,aes(x=windspeed,y=count))+
  geom_point(alpha=0.07, color='green')+
  labs(x='Wind speed', y= 'Hourly Usage Count')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))


library(corrplot)
cont_data<-rawData[c(11:17)]
o=corrplot(cor(cont_data),method='number') # this method can be changed try using method='circle'

# plot 24 hours
ggplot(rawData,aes(x=hr,y=count))+
  geom_count(color= 'yellow1')+
  geom_point(alpha=0.05, color= 'coral')+
  labs(x='Military time', y= 'Usage count')+
  geom_smooth()

# plot hours from 4am
ggplot(processedData,aes(x=hours_from_4am,y=count))+
  geom_count(color='yellow')+
  geom_point(alpha=0.07, color= 'pink')+
  labs(x='Hours from 4am', y= 'Hourly Usage Count')+
  geom_smooth(method='lm')+
  geom_smooth(method='loess', color ='blue')

# Plot Months
ggplot(rawData,aes(x= mnth,y=count))+
  geom_count(color= 'yellow1')+
  geom_point(alpha=0.05, color= 'green')+
  labs(x='Month number', y= 'Hourly Usage Count')+
  geom_smooth()

# plot months from January
ggplot(processedData,aes(x=months_from_Jan,y=count))+
  geom_count(color='yellow')+
  geom_point(alpha=0.07, color= 'pink')+
  labs(x='Months from January', y= 'Usage count')+
  geom_smooth(method='lm', color ='black')+
  geom_smooth(method='loess', color ='blue')

# Boxplots
rawData$yr<-factor(rawData$yr)

# plot year vs count
ggplot(data=rawData,aes(x=time,y=cnt,  fill=yr ))+  
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Year', y= 'Hourly Usage Count')

scatter.smooth(rawData$casual, rawData$registered)

