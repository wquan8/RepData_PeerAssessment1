# repdata-006

# Reproducible Research:  
# Peer Assessment 1
    
## Load R libs


```r
library(ggplot2)
library(grid)
library(gridExtra)
library(plyr)
options(warn=-1)
setwd("~/classes/repdata-006/RepData_PeerAssessment1/")
```

## Loading and preprocessing the data

```r
#load data and clean up
srcFileName <- "activity.csv"
srcData <- read.csv(srcFileName,stringsAsFactors=FALSE)
srcData$date <- as.Date(srcData$date)
srcData$dt <- as.POSIXlt(srcData$date) + srcData$interval %/% 100 * 3600 + srcData$interval %% 100 * 60
```


## What is mean total number of steps taken per day?


```r
aggdata <- ddply(srcData, c("date"), summarise,
      total = sum(steps),
      avgSteps = mean(steps),
      mdSteps  = median(steps))

pt <- ggplot(aggdata, aes(x=total)) + geom_histogram(colour="black", fill="white", binwidth = 700) +
  geom_vline(aes(xintercept=mean(total, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(total, na.rm=T)),   # Ignore NA values for mean
             color="blue", linetype="dotted", size=1)    

meanTotalWNa <- mean(aggdata$total,na.rm=TRUE)
medianTotalWNa <- median(aggdata$total,na.rm=TRUE)

grid.arrange(pt, main = "total steps per day (with mean and median)")
```

![plot of chunk unnamed-chunk-3](./PA1_template_files/figure-html/unnamed-chunk-3.png) 

The mean is 1.0766 &times; 10<sup>4</sup>. Median is 10765.


## What is the average daily activity pattern?

####Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
intervalMean <- tapply(srcData$steps, srcData$interval, mean,na.rm=TRUE)
plot(dimnames(intervalMean)[[1]], intervalMean,xlab="interval", ylab="mean across all days", type="l")
title(main="average number of steps taken, averaged across all days")
```

![plot of chunk unnamed-chunk-4](./PA1_template_files/figure-html/unnamed-chunk-4.png) 


#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
names(intervalMean[which(intervalMean == max(intervalMean))])
```

```
## [1] "835"
```

## Imputing missing values

NA values are replaced with total average over the period
  
Total NA values is

```r
sum(is.na(srcData$steps))
```

```
## [1] 2304
```

####strategy for filling in all of the missing values in the dataset
Transform NA values with 5 minutes mean and create new dataset


```r
meanAll <- mean(srcData$steps, na.rm=TRUE)
replacewithmean <- function(x) replace(x, is.na(x), ifelse(is.na(mean(x, na.rm = TRUE)), meanAll, mean(x, na.rm = TRUE)))              
srcDataNew <- ddply(srcData, ~ date, transform, steps = replacewithmean(steps))

aggdataNew <- ddply(srcDataNew, c("date"), summarise,
      total = sum(steps),
      avgSteps = mean(steps),
      mdSteps  = median(steps))


#draw histgram
hg <- ggplot(aggdata, aes(x=total)) + geom_histogram(colour="black", fill="white", binwidth = 700)+
  geom_vline(aes(xintercept=mean(total, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(total, na.rm=T)),   # Ignore NA values for mean
             color="blue", linetype="dotted", size=1)    
hgNew <- ggplot(aggdataNew, aes(x=total)) + geom_histogram(colour="black", fill="white", binwidth = 700)+
  geom_vline(aes(xintercept=mean(total, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(total, na.rm=T)),   # Ignore NA values for mean
             color="blue", linetype="dotted", size=1)    
grid.arrange(hg, hgNew, nrow = 2, main = "total steps per day (with NA) vs. (NA replaced)")
```

![plot of chunk unnamed-chunk-7](./PA1_template_files/figure-html/unnamed-chunk-7.png) 

Red dashed line is mean and blue dotted line is median. Replacement of NA values with total mean does not make a big difference on histgram.

## Are there differences in activity patterns between weekdays and weekends?

Two charts are drawn seperately for weekdays and weekends. Charts shows there are more total daily steps for weekends.


```r
aggdataNew$weekday <- ifelse(as.POSIXlt(aggdataNew$date)$wday == 0 | as.POSIXlt(aggdataNew$date)$wday == 6, 'weekend', 'weekday')
aggdataNew$weekday <- as.factor(aggdataNew$weekday)

srcDataNew$weekday <- ifelse(as.POSIXlt(srcDataNew$date)$wday == 0 | as.POSIXlt(srcDataNew$date)$wday == 6, 'weekend', 'weekday')
srcDataNew$weekday <- as.factor(srcDataNew$weekday)

weekdayData <- srcDataNew[srcDataNew$weekday == 'weekday',]
weekendData <- srcDataNew[srcDataNew$weekday == 'weekend',]

weekdaymean <- mean(weekdayData$steps)
weekendmean <- mean(weekendData$steps)

intervalWeekdayMean <- tapply(weekdayData$steps, weekdayData$interval, mean,na.rm=TRUE)
intervalWeekendMean <- tapply(weekendData$steps, weekendData$interval, mean,na.rm=TRUE)


plot(dimnames(intervalWeekdayMean)[[1]], intervalWeekdayMean,xlab="interval", ylab="mean across all week days", type="l")
```

![plot of chunk unnamed-chunk-8](./PA1_template_files/figure-html/unnamed-chunk-81.png) 

```r
plot(dimnames(intervalWeekendMean)[[1]], intervalWeekendMean,xlab="interval", ylab="mean across all weekend days", type="l")
```

![plot of chunk unnamed-chunk-8](./PA1_template_files/figure-html/unnamed-chunk-82.png) 

#### Looks like steps over weekend are more evenly distributed.
