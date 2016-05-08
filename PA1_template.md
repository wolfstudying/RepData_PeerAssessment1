## 1.Loading the data and preprocessing the data

library(xlsx)
library(ggplot2)
library(scales)

library(Hmisc)
setwd("F:/R_Coursera/05_ReproducibleResearch")
activity <- read.csv("activity.csv")
str(activity)
head(activity)
summary(activity)

## 2.What is mean total number of steps taken per day?

stepsByDay <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
qplot(stepsByDay, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)
stepsByDayMean <- mean(stepsByDay)
stepsByDayMedian <- median(stepsByDay)
stepsByDayMean
stepsByDayMedian

## 3.What is the average daily activity pattern?

averageStepsPerTimeBlock <- aggregate(x=list(meanSteps=activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)
ggplot(data=averageStepsPerTimeBlock, aes(x=interval, y=meanSteps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") 
mostSteps <- which.max(averageStepsPerTimeBlock$meanSteps)
timeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", averageStepsPerTimeBlock[mostSteps,'interval'])

## 4.Imputing missing values

numMissingValues <- length(which(is.na(activity$steps)))
numMissingValues
activityImputed <- activity
activityImputed$steps <- impute(activity$steps, fun=mean)
stepsByDayImputed <- tapply(activityImputed$steps, activityImputed$date, sum)
qplot(stepsByDayImputed, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)
stepsByDayMeanImputed <- mean(stepsByDayImputed)
stepsByDayMeanImputed
stepsByDayMedianImputed <- median(stepsByDayImputed)
stepsByDayMedianImputed

## 5.Are there differences in activity patterns between weekdays and weekends?

activityImputed$dateType <-  ifelse(as.POSIXlt(activityImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
averagedActivityImputed <- aggregate(steps ~ interval + dateType, data=activityImputed, mean)
ggplot(averagedActivityImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
