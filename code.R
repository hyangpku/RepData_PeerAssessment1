library("lattice")
## loda the data and process the date
setwd("H:\\Users\\hy\\Desktop\\Reproducible Research\\RepData_PeerAssessment1")
activity <- read.csv("activity.csv", colClasses = c("numeric", "character","numeric"))
activity$date <- as.Date(activity$date, "%Y-%m-%d")

## calculate the mean step per day using aggregating function
meanstep <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
hist(meanstep$step,xlab="days",main="mean total number of steps taken per day1",col="grey")
dev.copy(png,filename="plot1.png")
dev.off ()

## using tapply calculate the mean step per interval and plot the histogram
data <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
plot(names(data), data, type = "l", xlab = "interval",ylab = "mean", main = "average daily activity pattern",col = "grey")
dev.copy(png,filename="plot2.png")
dev.off ()

## count the missing value in the data set
count<- sum(is.na(activity))
count

## fill the missing value using the average of the step per interval
avg <- aggregate(steps ~ interval, data = activity, mean)
newstep<-numeric(0)

## start to fill and use newstep records the new steps
for (i in 1:nrow(activity)) {
  smalldata <- activity[i, ]
  if (is.na(smalldata$steps)) {
    smalldata$steps <- subset(avg, interval == smalldata$interval)$steps
  }
  newstep <- c(newstep, smalldata$steps)
}

## create new tidy data which does not contain NA value
new_activity <- activity
new_activity$steps <- newstep

## plot the new histogram
meanstep2 <- aggregate(steps ~ date, data = new_activity, sum)
hist(meanstep2$step, main = "mean total number of steps taken per day 2", xlab = "day", col = "grey")
dev.copy(png,filename="plot3.png")
dev.off ()

mean(meanstep2$steps)
median(meanstep2$steps)

## add the day label, please pay attention that my computer's language is Chinese so the conditions are 
## expressed in Chinses, otherwise it will not work, so if your computer language is in English, the 
## following code may not run properly. Sorry for the incovenience
day <- weekdays(activity$date)
level <- vector(mode="character")
for (i in 1:nrow(activity)) {
  if (day[i] == "星期六") {
    level[i] <- "Weekend"
  } else if(day[i]=="星期天"){
    level[i]<-"weekend"
  }
  else {
    level[i] <- "Weekday"
  }
}

activity$level <- level
activity$level <- factor(activity$level)

meanstep3 <- aggregate(steps ~ interval + level, data = activity, mean)
names(meanstep3) <- c("interval", "level", "steps")

xyplot(steps ~ interval | level, data=meanstep3, type = "l", layout = c(1, 2), 
       xlab = "interval", ylab = "steps")
dev.copy(png,filename="plot4.png")
dev.off ()
