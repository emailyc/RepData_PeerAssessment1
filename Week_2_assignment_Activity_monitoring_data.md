# Activity monitoring data
YIu Chung Wong  
23/08/2017  





##Download data

```r
URL = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

download_zip = function(URL,  zip_name, output_file_name){
        if(!file.exists(output_file_name)) { #Check wether the folder containing the data sets exists in the working directory.
                if(!file.exists(zip_name)){ # If not then see whether the zip file exist. #If not then download it.
                        switch(Sys.info()[['sysname']],
                               Windows = {download.file(URL, 
                                                        zip_name, method = "wininet", mode = "wb")},
                               Linux  = {download.file(URL, 
                                                       zip_name, method="curl")},
                               Darwin = {download.file(URL, 
                                                       zip_name, method="curl")})
                        unzip(normalizePath(zip_name), exdir=getwd()) #And unzip it
                } else if (file.exists(zip_name)) { # If the zip exists then unzip it.
                        unzip(normalizePath(zip_name), exdir=getwd())
                }
        }    
}

download_zip(URL, "activity.zip", "activity.csv")
```

###Read in data

```r
activity_data = read.csv("activity.csv")
```

###Reformating dates to date format

```r
activity_data$date = as.Date(activity_data$date, "%Y-%m-%d")
```




##What is mean total number of steps taken per day?

###Draw plot

```r
barplot(tapply(activity_data[complete.cases(activity_data),1], activity_data[complete.cases(activity_data),2], sum), xaxt="n",
        ylab = "Steps", main = "Steps taken each day\n(2012-10-02 to 2012-11-29)")
mtext(side=1, text="Day 1 to 61\n(excluding day: 1,8,32,35,40,41,45,61)", line=1)
```

![](Week_2_assignment_Activity_monitoring_data_files/figure-html/Plot histogram of number of steps each day-1.png)<!-- -->

###Calculate and report the mean and median total number of steps taken per day

```r
mean_median = data.frame(Date = unique(activity_data[complete.cases(activity_data),2]), Mean = tapply(activity_data[complete.cases(activity_data),1], activity_data[complete.cases(activity_data),2], mean), Median = tapply(activity_data[complete.cases(activity_data),1], activity_data[complete.cases(activity_data),2], median), row.names = 1:53)
print(mean_median, include.rownames = FALSE)
```

```
##          Date       Mean Median
## 1  2012-10-02  0.4375000      0
## 2  2012-10-03 39.4166667      0
## 3  2012-10-04 42.0694444      0
## 4  2012-10-05 46.1597222      0
## 5  2012-10-06 53.5416667      0
## 6  2012-10-07 38.2465278      0
## 7  2012-10-09 44.4826389      0
## 8  2012-10-10 34.3750000      0
## 9  2012-10-11 35.7777778      0
## 10 2012-10-12 60.3541667      0
## 11 2012-10-13 43.1458333      0
## 12 2012-10-14 52.4236111      0
## 13 2012-10-15 35.2048611      0
## 14 2012-10-16 52.3750000      0
## 15 2012-10-17 46.7083333      0
## 16 2012-10-18 34.9166667      0
## 17 2012-10-19 41.0729167      0
## 18 2012-10-20 36.0937500      0
## 19 2012-10-21 30.6284722      0
## 20 2012-10-22 46.7361111      0
## 21 2012-10-23 30.9652778      0
## 22 2012-10-24 29.0104167      0
## 23 2012-10-25  8.6527778      0
## 24 2012-10-26 23.5347222      0
## 25 2012-10-27 35.1354167      0
## 26 2012-10-28 39.7847222      0
## 27 2012-10-29 17.4236111      0
## 28 2012-10-30 34.0937500      0
## 29 2012-10-31 53.5208333      0
## 30 2012-11-02 36.8055556      0
## 31 2012-11-03 36.7048611      0
## 32 2012-11-05 36.2465278      0
## 33 2012-11-06 28.9375000      0
## 34 2012-11-07 44.7326389      0
## 35 2012-11-08 11.1770833      0
## 36 2012-11-11 43.7777778      0
## 37 2012-11-12 37.3784722      0
## 38 2012-11-13 25.4722222      0
## 39 2012-11-15  0.1423611      0
## 40 2012-11-16 18.8923611      0
## 41 2012-11-17 49.7881944      0
## 42 2012-11-18 52.4652778      0
## 43 2012-11-19 30.6979167      0
## 44 2012-11-20 15.5277778      0
## 45 2012-11-21 44.3993056      0
## 46 2012-11-22 70.9270833      0
## 47 2012-11-23 73.5902778      0
## 48 2012-11-24 50.2708333      0
## 49 2012-11-25 41.0902778      0
## 50 2012-11-26 38.7569444      0
## 51 2012-11-27 47.3819444      0
## 52 2012-11-28 35.3576389      0
## 53 2012-11-29 24.4687500      0
```




##What is the average daily activity pattern?

###Time series plot 

```r
plot(type = 'l', tapply(activity_data[complete.cases(activity_data),1], activity_data[complete.cases(activity_data),3], mean), main = "Average steps taken of each time interval", 
     xlab = "Time interval", ylab = "Averge steps", xlim = c(0, 290))
```

![](Week_2_assignment_Activity_monitoring_data_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

###Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_step = which.max(unname(tapply(activity_data[complete.cases(activity_data),1], activity_data[complete.cases(activity_data),3], mean)))
```
The 104th interval contains the maximum number of steps, averaged across all the days.



##Imputing missing values

###Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
NA_row = unname(table(complete.cases(activity_data))[1])
```
There are 2304 rows with NAs.


###Devise a strategy for filling in all of the missing values in the dataset.

```r
#First find days which measurment never took place.
missing_day = tapply(activity_data$steps, activity_data$interval, function(x)which(is.na(x)))
missing_days = unique(activity_data$date)[missing_day[[1]]]

#Find averge for each interval (over entire study period)
average_4_intervals = unname(tapply(activity_data[complete.cases(activity_data),1], activity_data[complete.cases(activity_data),3], mean))

#impute missing stpes with average step of that particular interval 
for (day in missing_days){
        activity_data$steps[which(activity_data$date == day)] = average_4_intervals
}

print(head(activity_data))
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```


##Histogram of the total number of steps taken each day

```r
barplot(tapply(activity_data[,1], activity_data[,2], sum), xaxt="n",
        ylab = "Steps", main = "Steps taken each day\n(2012-10-02 to 2012-11-29)")
mtext(side=1, text="Day 1 to 61", line=1)
```

![](Week_2_assignment_Activity_monitoring_data_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
mean_median = data.frame(Date = unique(activity_data[,2]), Mean = tapply(activity_data[,1], activity_data[,2], mean), Median = tapply(activity_data[,1], activity_data[,2], median), row.names = 1:61)
print(mean_median, include.rownames = FALSE)
```

```
##          Date       Mean   Median
## 1  2012-10-01 37.3825996 34.11321
## 2  2012-10-02  0.4375000  0.00000
## 3  2012-10-03 39.4166667  0.00000
## 4  2012-10-04 42.0694444  0.00000
## 5  2012-10-05 46.1597222  0.00000
## 6  2012-10-06 53.5416667  0.00000
## 7  2012-10-07 38.2465278  0.00000
## 8  2012-10-08 37.3825996 34.11321
## 9  2012-10-09 44.4826389  0.00000
## 10 2012-10-10 34.3750000  0.00000
## 11 2012-10-11 35.7777778  0.00000
## 12 2012-10-12 60.3541667  0.00000
## 13 2012-10-13 43.1458333  0.00000
## 14 2012-10-14 52.4236111  0.00000
## 15 2012-10-15 35.2048611  0.00000
## 16 2012-10-16 52.3750000  0.00000
## 17 2012-10-17 46.7083333  0.00000
## 18 2012-10-18 34.9166667  0.00000
## 19 2012-10-19 41.0729167  0.00000
## 20 2012-10-20 36.0937500  0.00000
## 21 2012-10-21 30.6284722  0.00000
## 22 2012-10-22 46.7361111  0.00000
## 23 2012-10-23 30.9652778  0.00000
## 24 2012-10-24 29.0104167  0.00000
## 25 2012-10-25  8.6527778  0.00000
## 26 2012-10-26 23.5347222  0.00000
## 27 2012-10-27 35.1354167  0.00000
## 28 2012-10-28 39.7847222  0.00000
## 29 2012-10-29 17.4236111  0.00000
## 30 2012-10-30 34.0937500  0.00000
## 31 2012-10-31 53.5208333  0.00000
## 32 2012-11-01 37.3825996 34.11321
## 33 2012-11-02 36.8055556  0.00000
## 34 2012-11-03 36.7048611  0.00000
## 35 2012-11-04 37.3825996 34.11321
## 36 2012-11-05 36.2465278  0.00000
## 37 2012-11-06 28.9375000  0.00000
## 38 2012-11-07 44.7326389  0.00000
## 39 2012-11-08 11.1770833  0.00000
## 40 2012-11-09 37.3825996 34.11321
## 41 2012-11-10 37.3825996 34.11321
## 42 2012-11-11 43.7777778  0.00000
## 43 2012-11-12 37.3784722  0.00000
## 44 2012-11-13 25.4722222  0.00000
## 45 2012-11-14 37.3825996 34.11321
## 46 2012-11-15  0.1423611  0.00000
## 47 2012-11-16 18.8923611  0.00000
## 48 2012-11-17 49.7881944  0.00000
## 49 2012-11-18 52.4652778  0.00000
## 50 2012-11-19 30.6979167  0.00000
## 51 2012-11-20 15.5277778  0.00000
## 52 2012-11-21 44.3993056  0.00000
## 53 2012-11-22 70.9270833  0.00000
## 54 2012-11-23 73.5902778  0.00000
## 55 2012-11-24 50.2708333  0.00000
## 56 2012-11-25 41.0902778  0.00000
## 57 2012-11-26 38.7569444  0.00000
## 58 2012-11-27 47.3819444  0.00000
## 59 2012-11-28 35.3576389  0.00000
## 60 2012-11-29 24.4687500  0.00000
## 61 2012-11-30 37.3825996 34.11321
```
For non missing days, the value stays the same. For days without measurments, their median is now the median of all the averaged steps (over the study period) within each interval.
They also happen to have the same mean because they have exactly the same data. 




##Are there differences in activity patterns between weekdays and weekends?

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

![](Week_2_assignment_Activity_monitoring_data_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
