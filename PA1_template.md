# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

```r
activity_data = read.csv("activity.csv")
activity_data$date = as.Date(activity_data$date, "%Y-%m-%d")
```


## What is mean total number of steps taken per day?

```r
total_steps = aggregate(steps~date, activity_data, FUN = sum, na.rm = T)
barplot(total_steps$steps, total_steps$steps, xaxt="n",
        ylab = "Steps", main = "Steps taken each day\n(2012-10-02 to 2012-11-29)")
mtext(side=1, text="Day 1 to 61\n(excluding day: 1,8,32,35,40,41,45,61)", line=1)
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
options("scipen" = 10)
mean = mean(total_steps$steps, na.rm = T)
median = median(total_steps$steps, na.rm = T)
```

mean =  10766.1886792

median =  10765

## What is the average daily activity pattern?

```r
complete = subset(activity_data, complete.cases(activity_data)==T)
plot(type = 'l', tapply(complete$steps, complete$interval, mean), main = "Average steps taken of each time interval", 
     xlab = "Time interval", ylab = "Averge steps", xlim = c(0, 290))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
minute = names(which.max(tapply(complete$steps, complete$interval, mean)))
index = which.max(unname(tapply(complete$steps, complete$interval, mean)))
```
The 104th (835) interval contains the maximum number of steps, averaged across all the days.


## Imputing missing values

```r
NA_row = unname(table(complete.cases(activity_data))[1])
```
There are 2304 rows with NAs.


```r
#First find days which measurment never took place.
missing_day = tapply(activity_data$steps, activity_data$interval, function(x)which(is.na(x)))
missing_days = unique(activity_data$date)[missing_day[[1]]]
missing_days
```

```
## [1] "2012-10-01" "2012-10-08" "2012-11-01" "2012-11-04" "2012-11-09"
## [6] "2012-11-10" "2012-11-14" "2012-11-30"
```

```r
#Find averge for each interval (over entire study period)
average_4_intervals = unname(tapply(activity_data$steps, activity_data$interval, mean, na.rm = T))

#impute missing stpes with average step of that particular interval 
for (day in missing_days){
        activity_data$steps[which(activity_data$date == day)] = average_4_intervals
}

any(is.na(activity_data$steps))
```

```
## [1] FALSE
```

```r
mean = mean(tapply(activity_data$steps, activity_data$date, sum), na.rm = T)
median = median(tapply(activity_data$steps, activity_data$date, sum), na.rm = T)

no_na = aggregate(steps~date, activity_data, FUN = sum, na.rm = T)
barplot(no_na$steps, no_na$steps, xaxt="n",
        ylab = "Steps", main = "Histogram of steps taken per day with imputed data")
mtext(side=1, text="Day 1 to 61", line=1)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

mean = 10766.1886792

median = 10766.1886792

The means stays the same, the meandin is slightly higher.


## Are there differences in activity patterns between weekdays and weekends?

```r
activity_data$`day of the week` <- ifelse(weekdays(activity_data$date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
activity_data$`day of the week` = as.factor(activity_data$`day of the week`)

library(lattice)
weekday = data.frame(Steps = with(activity_data[activity_data$`day of the week`=='Weekday',], unname(tapply(steps, interval, mean))), Interval = unique(activity_data$interval), day = "Weekday")
weekend = data.frame(Steps = with(activity_data[activity_data$`day of the week`=='Weekend',], unname(tapply(steps, interval, mean))), Interval = unique(activity_data$interval), day = "Weekend")
combined = rbind(weekday, weekend)
xyplot(Steps~Interval | day, data = combined, type = "l", layout = c(1, 2)
)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
