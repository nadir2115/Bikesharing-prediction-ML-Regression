#Machine learning Regression project-predicting bike usage- Nadir Nibras

rm(list = ls()); # clear workspace variables
cat("\014") # it means ctrl+L. clear window
graphics.off() # close all plots


# install.packages("MASS")
# install.packages("ggExtra")
library(MASS)
library(readr)
library(datasets)
library(ggplot2)
library(ggExtra)
rawd<- read_csv("C:/Users/nadir/Documents/Machine Learning/Bike-Sharing-Dataset/hour.csv")
biked <- read_csv("C:/Users/nadir/Documents/Machine Learning/Bike-Sharing-Dataset/bikehour.csv")

#plotting atemp
p<-ggplot(biked,aes(x=atemp,y=count))+
  geom_point(alpha=0.07, color='green')+
  labs(x='Adjusted Temperature', y= 'Hourly Usage Count')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))

#plotting temp
p<-ggplot(rawd,aes(x=temp,y=count))+
  geom_point(alpha=0.07,color='orange')+
  labs(x='Temperature', y= 'Hourly Usage Count')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))

#plotting humidity
p<-ggplot(biked,aes(x=hum,y=count))+
  geom_point(alpha=0.07, color='violet')+
  labs(x='Humidity', y= 'Hourly Usage Count')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))

#plotting weather situation
p<-ggplot(rawd,aes(x=weathersit,y=cnt))+
  geom_count(color='yellow')+
  geom_point(alpha=0.07, color='green')+
  labs(x='Weather situation', y= 'Hourly Usage Count')+
  geom_smooth(method='loess')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))


#plotting wind-speed
p<-ggplot(rawd,aes(x=windspeed,y=count))+
  geom_point(alpha=0.07, color='green')+
  labs(x='Wind speed', y= 'Hourly Usage Count')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))


library(corrplot)
cont_data<-rawd[c(11:17)]
o=corrplot(cor(cont_data),method='number') # this method can be changed try using method='circle'


#plotting 24 hours
ggplot(rawd,aes(x=hr,y=count))+
  geom_count(color= 'yellow1')+
  geom_point(alpha=0.05, color= 'coral')+
  labs(x='Military time', y= 'Usage count')+
  geom_smooth()

#plotting hours from 4am
ggplot(biked,aes(x=hours_from_4am,y=count))+
  geom_count(color='yellow')+
  geom_point(alpha=0.07, color= 'pink')+
  labs(x='Hours from 4am', y= 'Hourly Usage Count')+
  geom_smooth(method='lm')+
  geom_smooth(method='loess', color ='blue')

# Plotting Months
ggplot(rawd,aes(x= mnth,y=count))+
  geom_count(color= 'yellow1')+
  geom_point(alpha=0.05, color= 'green')+
  labs(x='Month number', y= 'Hourly Usage Count')+
  geom_smooth()

#plotting months from January
ggplot(biked,aes(x=months_from_Jan,y=count))+
  geom_count(color='yellow')+
  geom_point(alpha=0.07, color= 'pink')+
  labs(x='Months from January', y= 'Usage count')+
  geom_smooth(method='lm', color ='black')+
  geom_smooth(method='loess', color ='blue')

# Boxplots
rawd$yr<-factor(rawd$yr)
#plotting year vs count
ggplot(data=rawd,aes(x=time,y=cnt,  fill=yr ))+  
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Year', y= 'Hourly Usage Count')

scatter.smooth(rawd$casual, rawd$registered)

# boxplot(log(1+rawd$cnt))
# boxplot(rawd$cnt)
