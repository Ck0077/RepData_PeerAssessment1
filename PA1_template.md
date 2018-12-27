---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
data<-read.csv("activity.csv")
```



## What is mean total number of steps taken per day?

```r
total_steps_per_Date<- data %>% group_by(date) %>% summarise(total=sum(steps, na.rm = TRUE))

hist(total_steps_per_Date$total, main ="Total Steps taken in a Day", xlab = "Total Steps", ylab = "Count")
```

![](PA1_template_files/figure-html/meantotalstepschunk-1.png)<!-- -->

```r
meanSteps<-mean(total_steps_per_Date$total)

medianSteps<-median(total_steps_per_Date$total)
```

The mean steps taken per day is 'r meanSteps' and median is 'r medianSteps`

## What is the average daily activity pattern?

```r
meanPerInterval<- data %>% group_by(interval) %>% summarise(mean=mean(steps, na.rm = TRUE))

plot(x= meanPerInterval$interval, y=meanPerInterval$mean, type = "l", main="Mean Per 5 min Time Interval", xlab = "Time Interval", ylab = "Mean")
```

![](PA1_template_files/figure-html/meanPerIntervalChunk-1.png)<!-- -->

```r
maxStepsInterval<-meanPerInterval$interval[which.max(meanPerInterval$mean)]
```
The Maximum number of steps are taken in 'r maxStepsInterval` interval.


## Imputing missing values

```r
missingDataRows<-sum(is.na(data$steps))

for(i in 1:nrow(data)){
        if(is.na(data$steps[i])){
                meanInterval<-meanPerInterval$mean[which(meanPerInterval$interval==data$interval[i])]
                data$steps[i]<-meanInterval
        }
}

newData<-data

newDataMeanPerDate<- newData %>% group_by(date) %>% summarise(total=sum(steps))

hist(newDataMeanPerDate$total, main ="Total Steps taken in a Day", xlab = "Total Steps", ylab = "Count")
```

![](PA1_template_files/figure-html/missingvalueschunk-1.png)<!-- -->

```r
newmeanSteps<-mean(newDataMeanPerDate$total)

newmedianSteps<-median(newDataMeanPerDate$total)
```
The mean total number of steps per day is 1.0766189\times 10^{4} and the median is 1.0766189\times 10^{4} The mean is unchanged, but the median moved slightly and is now equal to the mean. Inputting missing data based on an average leaves the average daily total number of steps the same while changing the median.

## Are there differences in activity patterns between weekdays and weekends?

```r
newData$day<-weekdays(as.Date(data$date))
newData$dayType<-"Weekday"
newData$dayType[newData$day %in% c("Saturday", "Sunday")]<-"Weekend"

averageSteps<- newData %>% group_by(dayType, interval) %>% summarise(mean=mean(steps))
library(ggplot2)

qplot(interval, mean, data=averageSteps,
      type="l",
      geom="line",
      xlab="Interval",
      ylab="Number of Steps (Average)",
      main="Average steps taken Weekends vs. Weekdays",
      facets =dayType ~ .)
```

```
## Warning: Ignoring unknown parameters: type
```

![](PA1_template_files/figure-html/activityPatternChunk-1.png)<!-- -->
