Reproducible Research Peer Assessment 1
========================================
https://github.com/shanhangel/RepData_PeerAssessment1


## Loading and preprocessing the data
### 1.Load the data (i.e. read.csv())

```r
setwd("C:/Users/huangshan/Documents/GitHub/RepData_PeerAssessment1")
unzip("activity.zip")
activity <- read.csv("activity.csv")
```

### 2.Process/transform the data (if necessary) into a format suitable for your analysis
convert to Date format

```r
activity$date <- as.Date(activity$date,format="%Y-%m-%d")
```


## What is mean total number of steps taken per day?

```r
step_sum_day <- aggregate(steps ~ date, data=activity, sum)
```

### 1.Make a histogram of the total number of steps taken each day

```r
barplot(step_sum_day$steps, names.arg=step_sum_day$date, xlab="Date", 
        ylab="Total Steps", main="Total Steps by Date")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

### 2.Calculate and report the mean and median total number of steps taken per day

```r
mean(step_sum_day$steps, na.rm=TRUE)
```

```
## [1] 10766
```

```r
median(step_sum_day$steps, na.rm=TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
### 1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
step_interval <- aggregate(steps ~ interval, data=activity, mean, na.rm=TRUE)
plot(step_interval,type="l", main="Time Series Average Step")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

### 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
index <- which.max(step_interval$steps)
step_interval[index,]
```

```
##     interval steps
## 104      835 206.2
```

## Imputing missing values
### 1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity))
```

```
## [1] 2304
```

### 2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
new_activity_1 <- merge(activity, step_interval, by="interval", sort=FALSE)

new_activity_1$steps.x[is.na(new_activity_1$steps.x)] <- 
    new_activity_1$steps.y[is.na(new_activity_1$steps.x)]
```

### 3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
new_activity_2 <- data.frame(steps=new_activity_1$steps.x, 
                           date=new_activity_1$date, 
                           interval=new_activity_1$interval)
new_activity <- new_activity_2[order(new_activity_2$date,
                                     new_activity_2$interval),]
```

### 4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
step_sum_day_new <- aggregate(steps ~ date, data=new_activity, sum)
barplot(step_sum_day_new$steps, names.arg=step_sum_day_new$date, xlab="Date", 
        ylab="Total Steps", main="Total Steps by Date(NA value replaced)")                     
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
It seems that NA replacement has little impact on the orginal dataset.


## Are there differences in activity patterns between weekdays and weekends?
### 1.Create a new factor variable in the dataset with two levels ¨C ¡°weekday¡± and ¡°weekend¡± indicating whether a given date is a weekday or weekend day.

```r
weekday <- vector()
for (i in 1:17568){
    if (weekdays(new_activity$date[i])%in%c("ÐÇÆÚÈÕ","ÐÇÆÚÁù")){
        weekday[i]="weekend"
    } else{
        weekday[i]="weekday"
    }
}
new_activity[,"weekdays"] <- weekday
new_activity_weekday <- subset(new_activity, weekdays=="weekday")
new_activity_weekend <- subset(new_activity, weekdays=="weekend")
step_interval_weekend <- aggregate(steps ~ interval, data=new_activity_weekend, 
                                   mean)
step_interval_weekday <- aggregate(steps ~ interval, data=new_activity_weekday, 
                                   mean)
step_interval_weekend[,"weekdays"] <- "weekend"
step_interval_weekday[,"weekdays"] <- "weekday"
step_interval_weekdays <- rbind(step_interval_weekday,step_interval_weekend)
```

### 2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
library(ggplot2)
qplot(interval, steps, data=step_interval_weekdays, facets=weekdays~.,
geom="line", binwidth=2)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 
