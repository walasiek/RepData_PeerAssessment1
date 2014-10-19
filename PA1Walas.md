# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

The data was downloaded from 
[Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)


```r
library(plyr)

data <- read.csv(unz('activity.zip', 'activity.csv'))
sumStepsData <- ddply(data, .(date), summarise, steps=sum(steps))
sumStepsDataNoNA <- sumStepsData[!is.na(sumStepsData[,2]),]
avgSteps <- ddply(
  data, .(interval), summarise, steps=mean(steps, na.rm=TRUE))
```


## What is mean total number of steps taken per day?

The following figure shows the historgram of the total number of steps taken each day.

```r
hist(sumStepsDataNoNA$steps)
```

![](./PA1Walas_files/figure-html/unnamed-chunk-2-1.png) 

The mean and median of the total number of steps is calculated as:

```r
summary(sumStepsDataNoNA$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```


## What is the average daily activity pattern?

The following figure describes the 5-minute interval (x-axis) 
and the average number of steps taken, averaged across all days (y-axis):


```r
plot(avgSteps$interval, 
     avgSteps$steps, type='l', 
     xlab='Interval', ylab='Average number. of steps')
```

![](./PA1Walas_files/figure-html/unnamed-chunk-4-1.png) 

The maximum value of the averaged number of steps is for the interval:

```r
avgSteps$interval[which.max(avgSteps$steps)]
```

```
## [1] 835
```

## Imputing missing values

The data contains the following number of missing values 
for date/interval/steps respectively:


```r
sum(is.na(data$date))
```

```
## [1] 0
```

```r
sum(is.na(data$interval))
```

```
## [1] 0
```

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

The mising data for intervals was estimated by using mean of the appropriate interval.


```r
dataFill <- data

for (k in 1:nrow(avgSteps)) {
  i <- k
  avgValue <- avgSteps$steps[k]
  while(i <= nrow(dataFill)) {
    row <- dataFill[i,]
    
    if (is.na(row$steps)) {
      dataFill$steps[i] <- avgValue
    }
    
    i <- i + nrow(avgSteps)  
  }

}

sumStepsDataFill <- ddply(dataFill, .(date), summarise, steps=sum(steps))
sumStepsDataFillNA <- sumStepsData[!is.na(sumStepsData[,2]),]
```

The following figure shows the historgram of the total number 
of steps taken each day (with missing values filled in as mean).

```r
hist(sumStepsDataFillNA$steps)
```

![](./PA1Walas_files/figure-html/unnamed-chunk-8-1.png) 

The mean and median of the total number of steps 
is calculated as (with missing values filled in as mean):

```r
summary(sumStepsDataFillNA$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```

There is no difference from the first part of the assignment.

## Are there differences in activity patterns between weekdays and weekends?

The new facet variable is added:


```r
# Remark: Polish names are due to my locale set to PL
# For compatibility, I have added English names too
dataFill$weekend <- weekdays(
  as.Date(dataFill$date)) %in% c("sobota", "niedziela", "Saturday", "Sunday")

avgStepsPerWeekday <- aggregate(
  dataFill$steps, 
  list(interval=dataFill$interval, weekend=dataFill$weekend), 
  mean)
```


```r
avgStepsPerWeekdayOnlyWeekday <-
  avgStepsPerWeekday[!avgStepsPerWeekday$weekend,]
avgStepsPerWeekdayOnlyWeekend <-
  avgStepsPerWeekday[avgStepsPerWeekday$weekend,]
```

The plot is given below:


```r
par(mfrow = c(2,1))
plot(avgStepsPerWeekdayOnlyWeekday$interval, 
     avgStepsPerWeekdayOnlyWeekday$x, type='l', 
     xlab='Interval', 
     ylab='Average number. of steps',
     main='weekdays')

plot(avgStepsPerWeekdayOnlyWeekend$interval, 
     avgStepsPerWeekdayOnlyWeekend$x, 
     type='l', 
     xlab='Interval', 
     ylab='Average number. of steps', 
     main='weekends')
```

![](./PA1Walas_files/figure-html/unnamed-chunk-12-1.png) 
