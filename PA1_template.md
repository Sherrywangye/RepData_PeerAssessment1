Reproducible Research: Peer Assessment 1
=======================================
by Sherry(Oct,2015)


#Introduction

This assignment makes use of data from a personalPA1_template activity monitoring device. This device collects data at 5 minute intervals through out the day.Specifically this assignment use R markdown to write a report that answers the questions detailed in the sections below. In the process, the single R markdown document will be processed by knitr and be transformed into an HTML file.

#Prepare the R environment


```r
library(ggplot2)
library(knitr)
opts_chunk$set(echo=TRUE,results='hold')
library(Hmisc)
```

```
## Loading required package: grid
## Loading required package: lattice
## Loading required package: survival
## Loading required package: Formula
## 
## Attaching package: 'Hmisc'
## 
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

#Loading and preprocessing the data

###1.load the data


```r
if(!file.exists('activity.csv')){
    unzip('repdata-data-activity.zip')
}
activityData <- read.csv('activity.csv')
```


#What is mean total number of steps taken per day?

###1. Make a histogram of the total number of steps taken each day


```r
library(ggplot2)
stepsByDay <- tapply(activityData$steps, activityData$date, sum, na.rm=TRUE)
qplot(stepsByDay, xlab='Total number of steps taken each  day', ylab='count', binwidth=1000)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

###2. Calculate and report the mean and median total number of steps taken per day


```r
mean(stepsByDay,na.rm = TRUE)
median(stepsByDay,na.rm = TRUE)
#report the mean and median total number of steps taken per day
```

```
## [1] 9354.23
## [1] 10395
```

#What is the average daily activity pattern?

###1.Make a time series plot



```r
averages<-aggregate(x=list(meanSteps=activityData$steps),by=list(interval=activityData$interval), FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=meanSteps)) + geom_line() + xlab("5-minute interval") + ylab("average number of steps taken") 
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 


###2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
averages[which.max(averages$meanSteps),]
```

```
##     interval meanSteps
## 104      835  206.1698
```
**Observations:**

Base on steps taken pattern,the person's daily activity peaks around 8:35am.

#Imputing missing values

###1.Calculate and report the total number of missing values in the dataset


```r
length(which(is.na(activityData$steps)))
```

```
## [1] 2304
```

###2.Devise a strategy for filling in all of the missing values in the dataset.


```r
missing<-is.na(activityData$steps) #How many missing
table(missing)
```

```
## missing
## FALSE  TRUE 
## 15264  2304
```

###3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activityDataImputed <- activityData
activityDataImputed$steps <- impute(activityData$steps, fun=mean)
```

###4.Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsByDayImputed <- tapply(activityDataImputed$steps, activityDataImputed$date, sum)
qplot(stepsByDayImputed, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

#Are there differences in activity patterns between weekdays and weekends?

###1.Create a new factor variable in the dataset with two levels ¨C ¡°weekday¡± and ¡°weekend¡± indicating whether a given date is a weekday or weekend day.


```r
activityDataImputed$dateType <-  ifelse(as.POSIXlt(activityDataImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
```



###2. Make a panel plot containing a time series plot


```r
averagedActivityDataImputed <- aggregate(steps ~ interval + dateType, data=activityDataImputed, mean)
ggplot(averagedActivityDataImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 









