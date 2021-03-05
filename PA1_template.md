---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

In this document we examine the patterns of walking activity from observations 
in the activity data set.

## I. Loading and preprocessing the data
The first step is to load the data set into R, and load the first few rows to 
have an idea how the data looks like.
We see that there is a date variable in the class character. It might be useful 
for later analyses to format it into a factor variable.

```r
df <- read.csv("activity.csv") 
head(df)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
df$date <- as.factor(as.Date(df$date)) 
```
The variable *interval* isn't very informative. The following chunk of code will
convert the interval into the time of the day.

```r
library(datetime)
df$hour <- df$interval %/% 100
df$minute <- df$interval %% 100
df$time <- as.time(paste(df$hour, df$minute, sep=":"), format="%H:%M")
```

## II. What is mean total number of steps taken per day?
Let's look at the total number of steps taken each day using a histogram.

*(the missing values are ignored in this section)*

```r
daySum <- tapply(df$steps, df$date, sum)
hist(daySum, breaks=10, xlab="Nimber of Steps", ylab="Number of Days", 
     main="Total Steps Per Day") 
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

The vector *daySum* can further be used to calculate the mean and the median of 
the steps taken per day. 

```r
dayMean <- round(mean(daySum, na.rm=T), digits=0)
dayMedian <- median(daySum, na.rm=T) 
```
The daily average is thus 1.0766\times 10^{4} and the daily median is 10765.


## III. What is the average daily activity pattern?
We can further zoom in on the data to a finer resolution at the level of 
interval, by first creating a vector of average steps taken by interval, then
visualise with a simple line plot.

```r
perInterval <- tapply(df$steps, df$time, mean, na.rm=T)
plot(perInterval, type="l", main="Daily Activity Pattern", ylim=c(0,230),
     xlab="Time of the Day (h)", ylab="Steps Per 5min-Interval", xaxt="n")
axis(side=1, c(12*seq(24)), c(seq(24)))
```

![](PA1_template_files/figure-html/perInterval-1.png)<!-- -->

The graph illustrates the activity pattern during a day, where the number of 
steps peaks between 8am and 9am: 

```r
which.max(perInterval)
```

```
## 08:35 
##   104
```


## IV. Imputing missing values
This section deals with the missing values in this data set. 

The numbers and the percentage of the missing values are the following:

```r
sum(is.na(df$steps))
```

```
## [1] 2304
```

```r
mean(is.na(df$steps))
```

```
## [1] 0.1311475
```

Using the median of steps per interval seems a suitable strategy to impute the
missing data. This way, the daily activity patterns can be retained to some 
extent.

The next chunk of code calculates the median of steps per interval, copies the 
column *steps* as *steps2*, then fills in the missing values of *steps2* with 
the median values matched by *interval*.

```r
medInterval <- as.data.frame(tapply(df$steps, df$interval, median, na.rm=T))
names(medInterval) <- "steps"
medInterval$interval <- row.names(medInterval)
df$steps2 <- df$steps
df$steps2[is.na(df$steps2)] <- medInterval$steps[match(df$interval,medInterval$interval)][is.na(df$steps2)]
```

Let's compare the two columns *steps* and *steps2* using the *summary* function.

```r
summary(df$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```

```r
summary(df$steps2)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0       0       0      33       8     806
```

The 5-number summaries show that the column with imputed missing data has a 
similar distribution with the original data.

After filling the missing data, the new total steps per day is shown in the 
plot below:

```r
newSum <- tapply(df$steps2, df$date, sum)
hist(newSum, breaks=10, main="Total Steps Per Day (NA filled)", xlab="Steps", 
     ylab="Number of Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

The new mean and the median of steps taken each day are:

```r
mean(newSum)
```

```
## [1] 9503.869
```

```r
median(newSum)
```

```
## [1] 10395
```

Compared with the original mean and median, filling in missing values seems to 
have only slightly decreased the number of steps per day, without any major impacts.


## V. Are there differences in activity patterns between weekdays and weekends?
In the last section of this study, the differences in the activity patterns 
between weekdays and weekends are compared. We used the data with imputed 
missing values.
To do so, a new factor variable is created to indicate "weekday" or "weekend",
using the *lubridate* and *plyr* package.

```r
library(lubridate)
library(plyr)
df$weekday <- wday(df$date, label=F, week_start=1)
df$weekend <- df$weekday %in% c(6:7)
df$weekend <- as.factor(df$weekend)
df$weekend <- revalue(df$weekend, c("TRUE"="weekend", "FALSE"="weekday"))
```

Next, the data is split into two data tables based on this factor in order to
compute the average steps taken per interval separately for weekdays and for 
weekends. The data sets are then bound together again by row.

```r
library(dplyr)
library(data.table)
dt.wd <- as.data.table(filter(df, weekend=="weekday"))
dt.wd[,mean:=mean(steps2), by=interval]
dt.we <- as.data.table(filter(df, weekend=="weekend"))
dt.we[,mean:=mean(steps2), by=interval]
dt.mean <- rbind(dt.wd, dt.we)
```

Lastly, we plot the activity pattern of weekdays under that of weekends using 
the lattice plotting system.

```r
library(lattice)
xyplot(mean ~ interval | weekend, data=dt.mean, type="l", layout=c(1,2), 
       xlab="Interval", ylab="Number of steps", 
       main="Average Steps per Interval Weekend vs. Weekday")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

The graphs show that the active period on a weekend starts and ends later than 
on a weekday, and that the activity level stays relatively high throughout the
day as compared to a weekday.
