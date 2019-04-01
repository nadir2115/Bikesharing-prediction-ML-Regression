# Machine learning Regression project-predicting bike usage- Nadir Nibras

# clear workspace variables
rm(list = ls());
# it means ctrl+L. clear window
cat("\014") 
# close all plots
graphics.off() 


library(readr)
library(datasets)
library(ggplot2)
library(ggExtra)
library(rstudioapi)
library(corrplot)
library(Metrics)
library(dplyr)
library(tidyr)
library(caret)
library(tictoc)
library(lubridate)

# set directory to R script folder
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# load datasets
rawData<- read_csv("hour.csv")


# Pre-processing data -----------------------------------------------------


processedData<-rawData
processedData$weeknum<-week(processedData$dteday)
processedData$dteday<- NULL
processedData<-processedData[,c(1,17,2:16)]

for(i in unique(processedData$season)) {
  processedData[[paste0("season_",i)]] <- ifelse(processedData$season==i,1,0)
}
for(i in unique(processedData$weekday)) {
  processedData[[paste0("weekday_",i)]] <- ifelse(processedData$weekday==i,1,0)
}
for(i in unique(processedData$weathersit)) {
  processedData[[paste0("weathersit_",i)]] <- ifelse(processedData$weathersit==i,1,0)
}
for(i in unique(processedData$mnth)) {
  processedData[[paste0("mnth_",i)]] <- ifelse(processedData$mnth==i,1,0)
}
for(i in unique(processedData$hr)) {
  processedData[[paste0("hour_",i)]] <- ifelse(processedData$hr==i,1,0)
}

for(i in unique(processedData$weeknum)) {
  processedData[[paste0("weeknum_",i)]] <- ifelse(processedData$weeknum==i,1,0)
}

# Checkign ACF
acf(x=processedData$cnt, lag.max=10 , plot=TRUE)

#creating lag values
processedData$lag1<-0
processedData$lag2<-0
processedData$lag3<-0

processedData$lag1[2:17379]=processedData$cnt[1:17378]
processedData$lag2[3:17379]=processedData$cnt[1:17377]
processedData$lag3[4:17379]=processedData$cnt[1:17376]


# Fix cyclic variables
processedData$weeknum<-pmin(abs(3-processedData$weeknum),56-processedData$weeknum);  
processedData$mnth<-pmin(abs(1-processedData$mnth),13-processedData$mnth);  
processedData$hr<-pmin(abs(4-processedData$hr),28-processedData$hr);  


# move count to end
processedData<- processedData[c(1:16, 18:124,17)]
# remove excessive columns
processedData<- processedData[-c(1,3,8,10,15,16)]

processedData<- processedData[3:17379,]


# Exploratory data analysis -----------------------------------------------


# plot  atemp
p<-ggplot(processedData,aes(x=atemp,y=cnt))+
  geom_point(alpha=0.07, color='green')+
  labs(x='Adjusted Temperature', y= 'Hourly Usage Count')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))


# plot temp
p<-ggplot(rawData,aes(x=temp,y=cnt))+
  geom_point(alpha=0.07,color='orange')+
  labs(x='Temperature', y= 'Hourly Usage cnt')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))

# plot humidity
p<-ggplot(processedData,aes(x=hum,y=cnt))+
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
p<-ggplot(rawData,aes(x=windspeed,y=cnt))+
  geom_point(alpha=0.07, color='green')+
  labs(x='Wind speed', y= 'Hourly Usage Count')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))

# correlation plot
cont_data<-rawData[c(11:17)]
o=corrplot(cor(cont_data),method='number') # this method can be changed try using method='circle'

# plot 24 hours
ggplot(rawData,aes(x=hr,y=cnt))+
  geom_count(color= 'yellow1')+
  geom_point(alpha=0.05, color= 'coral')+
  labs(x='Military time', y= 'Usage count')+
  geom_smooth()

# plot hours from 4am using processed Data
ggplot(processedData,aes(x=hr,y=cnt))+
  geom_count(color='yellow')+
  geom_point(alpha=0.07, color= 'pink')+
  labs(x='Hours from 4am', y= 'Hourly Usage Count')+
  geom_smooth(method='lm', color='red')+
  geom_smooth(method='loess', color ='blue')

# Plot Months
ggplot(rawData,aes(x= mnth,y=cnt))+
  geom_count(color= 'yellow1')+
  geom_point(alpha=0.05, color= 'green')+
  labs(x='Month number', y= 'Hourly Usage Count')+
  geom_smooth()

# plot months from January
ggplot(processedData,aes(x=mnth,y=cnt))+
  geom_count(color='yellow')+
  geom_point(alpha=0.07, color= 'pink')+
  labs(x='Months from January', y= 'Usage count')+
  geom_smooth(method='lm', color ='black')+
  geom_smooth(method='loess', color ='blue')

# Boxplots -plot year vs count
rawData$yr<-factor(rawData$yr)
ggplot(data=rawData,aes(x=yr,y=cnt,  fill=yr ))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Year', y= 'Hourly Usage Count')

# --------------------------------------------------------------------------



# Poisson regression set up
processedData$cnt <- log10(processedData$cnt+1)

# Partitioning dataset into training/validation and test
set.seed(111)
trainIndex <- c(1:15643)
bikeTrainingValidation <- processedData[ trainIndex,]
bikeTest  <- processedData[-trainIndex,]
xTest <- data.matrix(bikeTest[-c(118)])
yTest <- data.matrix(bikeTest[c(118)])


# Maximum Likelihood Estimate ---------------------------------------------

p_error<- 100000;

tic("MLE time")
xTrain<- data.matrix(bikeTrainingValidation[-c(118)])
yTrain<- data.matrix(bikeTrainingValidation[c(118)])
XtXtrain<- (t(xTrain))%*%xTrain;

wMLE <- (ginv(XtXtrain))%*%(t(xTrain))%*%yTrain;
yMLE_est= xTest%*%wMLE;

toc()

# Maximum A Posteriori -------------------------------------------------

tic("MAP time")

# Identity matrix
Ide <- matrix(0, ncol(xTrain), ncol(xTrain))
diag(Ide) <- 1

p_error<- 100000;

for (i in 1:100){
  for (k in 1:10){
    mw= 0.01*k; 
    tausq= 0.01*i; 
    lambda = mw/tausq;            
    
    
    set.seed(k*(i+100))
    trainIndex <- createDataPartition(bikeTrainingValidation$cnt, p = .8, list = FALSE, times = 1)
    bikeTrain <- bikeTrainingValidation[ trainIndex,]
    bikeVal  <- bikeTrainingValidation[-trainIndex,]
    xTrain <- data.matrix(bikeTrain[-c(118)])
    xVal <- data.matrix(bikeVal[-c(118)])
    yTrain <- data.matrix(bikeTrain[c(118)])
    yVal <- data.matrix(bikeVal[c(118)])
    
    XtXtrain<- (t(xTrain))%*%xTrain;
    
    wMAP_k= (ginv(XtXtrain+(lambda*Ide)))%*%((lambda*mw)+((t(xTrain))%*%yTrain));
    yMAP_est<- xVal%*%wMAP_k; 
    MSE_MAP_k <- mse(yMAP_est,yVal);                    
    
    
    if (MSE_MAP_k<p_error) {
      wMAP=wMAP_k;
      p_error= MSE_MAP_k
      ideal_lambda=lambda
    }
  }
}

yMAP_est= xTest%*%wMAP;

toc()

# evaluating model performance ------------------------------------------------------

yTest<- 10^(yTest)-1
yMLE_est<- 10^(yMLE_est)-1
yMAP_est<- 10^(yMAP_est)-1
y_naive= bikeTest$lag1

MAP_MSE= mse(yMAP_est,yTest)
MLE_MSE= mse(yMLE_est,yTest)
naive_MSE= mse(y_naive,yTest)

MAP_correlation= cor(yTest,yMAP_est)
MLE_correlation= cor(yTest,yMLE_est)
naive_correlation= cor(yTest,y_naive)


R2MAP <- 1 - (sum((yTest-yMAP_est )^2)/sum((yTest-mean(yTest))^2))
R2MLE <- 1 - (sum((yTest-yMLE_est )^2)/sum((yTest-mean(yTest))^2))
R2naive <- 1 - (sum((yTest-y_naive )^2)/sum((yTest-mean(yTest))^2))



testresults <- data.frame(yTest, yMLE_est, yMAP_est,y_naive)
names(testresults) <- c("yTest", "yMLE_est", "yMAP_est", "y_naive")

testresults$MLEerror <- abs(testresults$yMLE_est-testresults$yTest)
testresults$MAPerror <- abs(testresults$yMAP_est-testresults$yTest)
testresults$naiveerror <- abs(testresults$y_naive-testresults$yTest)


meanMLEerror<- mean(testresults$MLEerror)
medianMLEerror<- median(testresults$MLEerror)

meanMAPerror<- mean(testresults$MAPerror)
medianMAPerror<- median(testresults$MAPerror)

meannaiveerror<- mean(testresults$naiveerror)
mediannaiveerror<- median(testresults$naiveerror)

ggplot(testresults,aes(yMLE_est,yTest))+
  geom_point(alpha=0.37, color='violet')+
  geom_smooth(method='lm')

ggplot(testresults,aes(yMAP_est,yTest))+
  geom_point(alpha=0.37, color='orange')+
  geom_smooth(method='lm')

ggplot(testresults,aes(y_naive,yTest))+
  geom_point(alpha=0.37, color='red')+
  geom_smooth(method='lm')

# Visualizing subset
i=200:300
ytest_s=yTest[i]
yMLE_s=yMLE_est[i]
yMAP_s=yMAP_est[i]
y_naive_s= y_naive[i]

df <- data.frame(ytest_s, yMLE_s, yMAP_s,y_naive_s, i)

df.plot <- df %>%
  gather(predictions, value, -c("i"))

ggplot(df.plot, aes(x=i, y=value, color=predictions)) +
  geom_line( size=0.9)


