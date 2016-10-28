# Reproducible Research: Peer Assessment 1
##Load Required Lib

```r
library(knitr)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.1
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.3.1
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

## Loading and preprocessing the data
###Steps
1. Set the working directory
2. Unzip the zip file
3. read data into variable acts


```r
setwd("E:/Coursera/RepData_PeerAssessment1-master")
if(file.exists("activity.zip")) { unzip("activity.zip") }
acts <- read.csv("activity.csv")
sub_acts <- acts[!is.na(acts$steps),]
```


## What is mean total number of steps taken per day?
###Steps
1. aggregate sum(steps) per day
2. plot the result
3. Caculate mean & median


```r
agg_acts <- aggregate(steps ~ date, data = sub_acts, FUN = sum)

hist(agg_acts$steps, 
    main="Total Steps per Day", 
    xlab="Number of Steps per Day", 
    ylab = "Interval",
    col="yellow",
    breaks=50)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
mean_of_steps <- mean(agg_acts$steps)
mean_of_steps
```

```
## [1] 10766.19
```

```r
median_of_steps <- median(agg_acts$steps)
median_of_steps
```

```
## [1] 10765
```


## What is the average daily activity pattern?
###Steps
1. aggreagte mean(steps) per interval
2. plot the result
3. Caculate max steps


```r
fivemin <- aggregate(steps ~ interval, data = sub_acts, FUN = mean)

plot(x = fivemin$interval, 
    y = fivemin$steps, 
    type = "l", 
    col = "orange",
    xlab = "5-minute Intervals",
    ylab = "Average Steps Taken - Days",
    main = "Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
maxsteps <- fivemin$interval[which.max(fivemin$steps)]
maxsteps
```

```
## [1] 835
```


## Imputing missing values
###Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
impute <- sum(is.na(acts$steps))
impute
```

```
## [1] 2304
```

###Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
###Replace NA values with the mean results for five minute intervals
###Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
acts2 <- acts
nas <- is.na(acts2$steps)
avg_interval <- tapply(acts2$steps, acts2$interval, mean, na.rm=TRUE, simplify = TRUE)
acts2$steps[nas] <- avg_interval[as.character(acts2$interval[nas])]
names(acts2)
```

```
## [1] "steps"    "date"     "interval"
```

```r
sum(is.na(acts2))
```

```
## [1] 0
```

###Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
totalstepsperday2 <- aggregate(steps ~ date, data = acts2, FUN = sum, na.rm = TRUE)
totalstepsperday2
```

```
##          date    steps
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## 11 2012-10-11 10304.00
## 12 2012-10-12 17382.00
## 13 2012-10-13 12426.00
## 14 2012-10-14 15098.00
## 15 2012-10-15 10139.00
## 16 2012-10-16 15084.00
## 17 2012-10-17 13452.00
## 18 2012-10-18 10056.00
## 19 2012-10-19 11829.00
## 20 2012-10-20 10395.00
## 21 2012-10-21  8821.00
## 22 2012-10-22 13460.00
## 23 2012-10-23  8918.00
## 24 2012-10-24  8355.00
## 25 2012-10-25  2492.00
## 26 2012-10-26  6778.00
## 27 2012-10-27 10119.00
## 28 2012-10-28 11458.00
## 29 2012-10-29  5018.00
## 30 2012-10-30  9819.00
## 31 2012-10-31 15414.00
## 32 2012-11-01 10766.19
## 33 2012-11-02 10600.00
## 34 2012-11-03 10571.00
## 35 2012-11-04 10766.19
## 36 2012-11-05 10439.00
## 37 2012-11-06  8334.00
## 38 2012-11-07 12883.00
## 39 2012-11-08  3219.00
## 40 2012-11-09 10766.19
## 41 2012-11-10 10766.19
## 42 2012-11-11 12608.00
## 43 2012-11-12 10765.00
## 44 2012-11-13  7336.00
## 45 2012-11-14 10766.19
## 46 2012-11-15    41.00
## 47 2012-11-16  5441.00
## 48 2012-11-17 14339.00
## 49 2012-11-18 15110.00
## 50 2012-11-19  8841.00
## 51 2012-11-20  4472.00
## 52 2012-11-21 12787.00
## 53 2012-11-22 20427.00
## 54 2012-11-23 21194.00
## 55 2012-11-24 14478.00
## 56 2012-11-25 11834.00
## 57 2012-11-26 11162.00
## 58 2012-11-27 13646.00
## 59 2012-11-28 10183.00
## 60 2012-11-29  7047.00
## 61 2012-11-30 10766.19
```

```r
hist(totalstepsperday2$steps, 
    main = "Total Steps per Day (no-NA)", 
    xlab = "Number of Steps per Day", 
    ylab = "Interval",
    col="green",
    breaks=50)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
summary(agg_acts)
```

```
##          date        steps      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```

```r
summary(totalstepsperday2)
```

```
##          date        steps      
##  2012-10-01: 1   Min.   :   41  
##  2012-10-02: 1   1st Qu.: 9819  
##  2012-10-03: 1   Median :10766  
##  2012-10-04: 1   Mean   :10766  
##  2012-10-05: 1   3rd Qu.:12811  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55
```


## Are there differences in activity patterns between weekdays and weekends?
###Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
w_acts <- acts[!is.na(acts$steps),]
w_acts$date <- as.Date(w_acts$date, "%Y-%m-%d")

w_acts<- w_acts %>% mutate(typeofday= ifelse(weekdays(w_acts$date)=="星期六" | weekdays(w_acts$date)=="星期日", "Weekend", "Weekday"))
```

###Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
fivemin2<- aggregate(steps ~ interval, data = w_acts, FUN = mean)

head(fivemin2)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
ggplot(w_acts, aes(x =interval , y=steps, color=typeofday)) +
       geom_line() +
       labs(title = "Ave Daily Steps (type of day)", x = "Interval", y = "Total Number of Steps") +
       facet_wrap(~ typeofday, ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
