---
output: 
  html_document: 
    keep_md: yes
---
# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data


```r
library("dplyr")
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
library("ggplot2")
```


```r
unzip("activity.zip")
activitydata <- read.csv("activity.csv", header=TRUE)
```




## What is mean total number of steps taken per day?


```r
activitycomplete <- na.omit(activitydata)
stepdata <- activitycomplete %>% group_by(date) %>% summarise(steps=sum(steps), .groups='drop')
```



## Histogram for total number of steps taken per day

```r
qplot(steps,data=stepdata, binwidth=1000, xlab="Total steps per day", ylab="Frequency", main="Total No. of Steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->




## Mean & Median of total number of steps taken per day

```r
stepmean <- mean(stepdata$steps)
stepmedian <- median(stepdata$steps)
```
* Mean : 1.0766189\times 10^{4}
* Median : 10765




## What is the average daily activity pattern?


```r
activityinterval <- activitydata %>% group_by(interval) %>% summarise(steps=mean(steps,na.rm=TRUE), .groups='drop')
```

#### Time series plot

```r
ggplot(activityinterval, aes(interval,steps)) + geom_line() + ggtitle("Average Daily Activity Pattern") 
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


### 5- min interval contains maximum No. of steps

```r
fivemin <-activityinterval[which.max(activityinterval$steps),]
```
* 5 Min Interval : 835, 206.1698113



## Imputing missing values
#### Total number of missing values in the dataset

```r
missingvalues <- length(which(is.na(activitydata$steps)))
```
* Number of missing values : 2304


#### Fill data & create new dataset with no missing values

```r
names(activityinterval)[2] <- "meansteps"
activityimpute <- merge(activitydata, activityinterval)
activityimpute$steps[is.na(activityimpute$steps)] <- activityimpute$meansteps[is.na(activityimpute$steps)]

activitydayimpute <- activityimpute %>% group_by(date) %>% summarise(steps=sum(steps), .groups='drop')
```


#### Histogram for total number of steps taken per day with new dataset

```r
qplot(steps, data=activitydayimpute, binwidth=1000, xlab="Total steps per day ", ylab="Frequency", main="Total No. of Steps Taken Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


#### Mean & Median of total number of steps taken per day with new dataset

```r
newmean <- mean(activitydayimpute$steps)
newmedian <- median(activitydayimpute$steps)
```
* Mean : 1.0766189\times 10^{4}
* Median : 1.0766189\times 10^{4}



## Are there differences in activity patterns between weekdays and weekends?
#### Create factor variable & making levels for weedays & weekend


```r
activityimpute$day <- weekdays(as.Date(activityimpute$date))
activityimpute$weekend <- as.factor(activityimpute$day=="Saturday" | activityimpute$day=="Sunday")
levels(activityimpute$weekend) <- c("Weekday", "Weekend")
activityday <- activityimpute %>% group_by(interval,weekend) %>% summarise(steps=mean(steps), .groups='drop')
```


#### Time series plot for pattern between weekdays & weekends

```r
qplot(interval,steps, data=activityday, geom="line", facets=weekend~., xlab="5 Min Intervals", ylab="Steps", main="Step pattern between Weekdays & Weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


