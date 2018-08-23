---
title: 'Reproducible Research: Peer Assessment 1'
author: "Ivan Vemado Marques"
date: "21 de agosto de 2018"
output: 
  html_document: 
    keep_md: yes
---



# Introduction

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps:** Number of steps taking in a 5-minute interval (missing values are coded as \color{red}{\verb|NA|}NA)
* **date:** The date on which the measurement was taken in YYYY-MM-DD format
* **interval:** Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


## Loading and preprocessing the data

1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis

* Download and unzip datafile from [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)


```r
library(plyr)
library(lattice)

## Set repository
setwd("C:/Users/Ivan Vermado/Desktop/coursera")

## Create file
file <- "Activity monitoring data.zip"

## Downloading and unziping the dataset
if (!file.exists(file)){
  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(url, file, mode = "wb")
}  

if (file.exists("Activity monitoring data.zip")) { 
  unzip(file)
}
```

* Reading and preparing datafile


```r
## Reading file
arq <- read.csv("activity.csv")

##Structure of file
str(arq)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
## File Preview
head(arq)
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


## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day
2. Calculate and report the mean and median total number of steps taken per day

* Analysing variable **steps** by day


```r
## Histogram of steps by day
sum_steps <- tapply(arq$steps, arq$date, sum, na.rm = TRUE)
hist(sum_steps, main="Number of Steps", xlab="Total number of steps taken each day", ylab = "Number of Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

* Average variable **steps**


```r
## Mean and Median of steps
mean(sum_steps)
```

```
## [1] 9354.23
```

```r
median(sum_steps)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

* Analysing time series of **steps** by **5-minute interval**


```r
## Time series of avarage steps
avg_steps <- tapply(arq$steps, arq$interval, mean, na.rm = TRUE)

plot(names(avg_steps), avg_steps, type = "l", main = "Average Daily Activity Pattern", xlab = "5-minute interval", ylab = "Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

* Maximum of **steps**


```r
## 5-minute interval on average contains the maximum number of steps
names(avg_steps[avg_steps==max(avg_steps)])
```

```
## [1] "835"
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

* Missing values


```r
## Calculate the missing values in the dataset
sum(is.na(arq))
```

```
## [1] 2304
```

* Complete missing cases with mean of **5-minute interval**


```r
## Treating missing values
new_arq <- arq

for(i in 1:nrow(new_arq)){
  new_arq[i,is.na(new_arq[i,])] <- mean(new_arq[new_arq[,3]==new_arq[1,3],1], na.rm = TRUE)
}
```

* Analysing variable **steps** with the missing data filled in


```r
## Histogram of steps by day
sum_steps2 <- tapply(new_arq$steps, new_arq$date, sum, na.rm = TRUE)
hist(sum_steps2, main="Number of Steps", xlab="Total number of steps taken each day", ylab = "Number of Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

The new dataset contains more than **steps** and increase de range of y-axis.


* Average variable **steps** with the missing data filled in


```r
## Mean and Median of steps
mean(sum_steps2)
```

```
## [1] 9419.081
```

```r
median(sum_steps2)
```

```
## [1] 10395
```

If dataset is fill in with the mean of **steps** on the correspondent **5-minute interval**, the general mean decreases.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

* Compute if variable **date** is a weekday or weekend day in dataset


```r
weekday <- ifelse(weekdays(as.Date(new_arq$date)) %in% c("sábado", "domingo"), "weekend", "weekday")
new_arq <- cbind(new_arq,weekday)
```

* Analysing time series of **steps** by **5-minute interval** across all **weekday days** or **weekend days**


```r
## Time series of avarage steps
avg_steps <- ddply(new_arq, .(interval, weekday), summarise, steps=mean(steps))

xyplot(steps ~ interval | weekday, data = avg_steps, layout = c(1, 2), type="l", xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
