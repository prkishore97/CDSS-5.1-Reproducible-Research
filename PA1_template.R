setwd("C:/Users/ram.p/Documents/R/CDSS5.1")

#---------Q1---------#
data=read.csv("activity.csv")
data1=data
data1$date=as.Date(as.character(data1$date),"%Y-%m-%d")

#---------Q2---------#
Total_Steps_Taken_Each_Day=tapply(data1$steps,data1$date,sum,na.rm=T)
hist(Total_Steps_Taken_Each_Day)

#---------Q3---------#
data_frame_data=as.data.frame(Total_Steps_Taken_Each_Day)
data_frame_data[,1]=as.numeric(data_frame_data[,1])
mean(data_frame_data[,1],na.rm=T)
median(data_frame_data[,1],na.rm=T)

#---------Q4---------#
Avg_Steps_Taken_Each_Day=tapply(data1$steps,data1$interval,mean,na.rm=T)
plot(names(Avg_Steps_Taken_Each_Day),Avg_Steps_Taken_Each_Day,type="l",xlab="Date",ylab="Avg Steps", main="Avg Steps across Date")

#---------Q5---------#
int_num=which.max(Avg_Steps_Taken_Each_Day)
names(int_num)

#---------Q6---------#
data12=data1
for(i in 1:nrow(data1)){
  if(is.na(data1[i,1])==TRUE)
  {
    data12[i,1]=mean(data1$steps,na.rm=T)}
  
  else
  {
    data12[i,1]=data1[i,1]
}}

#---------Q7---------#
Total_Steps_Taken_Each_Day_Missing_Treated=tapply(data12$steps,data12$date,sum,na.rm=T)
hist(Total_Steps_Taken_Each_Day_Missing_Treated)

#---------Q8---------#
library(lattice)
data2=data1
days=weekdays(data2$date)
type=ifelse(days=="Saturday"|days=="Sunday","weekend","weekday")
data2$type=type
meansteps <- aggregate(data2$steps, by = list(data2$interval, data2$type),mean,na.rm=T)
names(meansteps)=c("interval","day_type","steps")
xyplot(steps ~ interval | day_type, meansteps, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")


View(meansteps)
