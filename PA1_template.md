---
title: "Reproducible Research Peer Review Project 1"
author: "Ram Kishore Pasupathi"
date: "8 July 2018"
output: html_document
---

This is an R markdown document containing the code and descriptions of the code used to create the necessary graphs for Peer Review Project 1. The Questions given in the assignment are as below,

1) Code for reading in the dataset and/or processing the data
```{r}
data=read.csv("activity.csv")
data1=data
data1$date=as.Date(as.character(data1$date),"%Y-%m-%d")
```


2) Histogram of the total number of steps taken each day
```{r fig.width=6, fig.height=4}
Total_Steps_Taken_Each_Day=tapply(data1$steps,data1$date,sum,na.rm=T)
hist(Total_Steps_Taken_Each_Day)
```


3) Mean and median number of steps taken each day
```{r fig.width=6, fig.height=4}
data_frame_data=as.data.frame(Total_Steps_Taken_Each_Day)
data_frame_data[,1]=as.numeric(data_frame_data[,1])
mean(data_frame_data[,1],na.rm=T)
median(data_frame_data[,1],na.rm=T)
```


4) Time series plot of the average number of steps taken
```{r fig.width=6, fig.height=4}
Avg_Steps_Taken_Each_Day=tapply(data1$steps,data1$interval,mean,na.rm=T)
plot(names(Avg_Steps_Taken_Each_Day),Avg_Steps_Taken_Each_Day,type="l",xlab="Date",ylab="Avg Steps", main="Avg Steps across Date")
```


5) The 5-minute interval that, on average, contains the maximum number of steps
```{r fig.width=6, fig.height=4}
int_num=which.max(Avg_Steps_Taken_Each_Day)
names(int_num)
```


6) Code to describe and show a strategy for imputing missing data
```{r fig.width=6, fig.height=4}
data12=data1
for(i in 1:nrow(data1)){
  if(is.na(data1[i,1])==TRUE)
  {
    data12[i,1]=mean(data1$steps,na.rm=T)}
  
  else
  {
    data12[i,1]=data1[i,1]
  }}
```


7) Histogram of the total number of steps taken each day after missing values are imputed
```{r fig.width=6, fig.height=4}
Total_Steps_Taken_Each_Day_Missing_Treated=tapply(data12$steps,data12$date,sum,na.rm=T)
hist(Total_Steps_Taken_Each_Day_Missing_Treated)
```


8) Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
```{r fig.width=6, fig.height=4}
library(lattice)
data2=data1
days=weekdays(data2$date)
type=ifelse(days=="Saturday"|days=="Sunday","weekend","weekday")
data2$type=type
meansteps <- aggregate(data2$steps, by = list(data2$interval, data2$type),mean,na.rm=T)
names(meansteps)=c("interval","day_type","steps")
xyplot(steps ~ interval | day_type, meansteps, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
```


~~~THE END~~~
