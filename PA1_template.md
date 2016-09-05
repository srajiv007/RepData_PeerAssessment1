# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Loading activity data frame and clean data frame to ignore NA observations


```r
act_data = read.csv("activity.csv")
act_data$date = as.Date(act_data$date)
act_clean = act_data[!is.na(act_data$steps),]
act_clean$steps = as.numeric(act_clean$steps)
```

## What is mean total number of steps taken per day?

```r
agg_steps_sum = aggregate(act_clean$steps, by=list(date=act_clean$date), FUN=sum, na.rm=TRUE)
names(agg_steps_sum) = c("date", "sum")
agg_steps_count = transform(table(act_clean$date))
names(agg_steps_count)=c("date", "count")
agg_steps_all = merge(agg_steps_sum, agg_steps_count, by="date")
#Histogram - total steps per day
hist(agg_steps_sum$sum)
```

![](PA1_template_files/figure-html/aggregatesteps-1.png)<!-- -->

```r
# Mean - total steps per day
mean(agg_steps_all$sum)
```

```
## [1] NaN
```

```r
# Median - total steps per day
median(agg_steps_all$sum)
```

```
## [1] NA
```


## What is the average daily activity pattern?

```r
agg_steps_interval=aggregate(act_clean$steps, by=list(act_clean$interval), FUN=mean)
names(agg_steps_interval)=c("interval", "avg.steps")
t=ts(agg_steps_interval)
plot(t[,2], type="l", xlab="5-min. interval", ylab="avg. steps", main="avg. steps over 5-min. interval")
```

![](PA1_template_files/figure-html/intervaldata-1.png)<!-- -->

```r
#Max Avg steps over all intervals
max(agg_steps_interval$avg.steps)
```

```
## [1] 206.1698
```


## Imputing missing values

```r
#Total missing rows
sum(is.na(act_data$steps))
```

```
## [1] 2304
```

```r
#Cleaning NA values 
replaced.df = merge(act_data, agg_steps_interval, by = "interval")
replaced.df$steps[is.na(replaced.df$steps)]=replaced.df$avg.steps[is.na(replaced.df$steps)]
replaced.df=replaced.df[order(replaced.df$date, replaced.df$interval),]
replaced.df.sum = aggregate(replaced.df$steps, by=list(date=replaced.df$date), FUN=sum, na.rm=FALSE)
names(replaced.df.sum)=c("date", "steps")
hist(replaced.df.sum$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
mean(replaced.df.sum$steps)
```

```
## [1] 10766.19
```

```r
median(replaced.df.sum$steps)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

```r
library(lattice)
# Creating weekday factor column in the imputed data frame
act_clean_data=replaced.df
act_clean_data$day=ifelse(grepl("S(at|un)", weekdays(act_clean_data$date)), "weekend", "weekday")
act_clean_data$day=as.factor(act_clean_data$day)
agg=aggregate(act_clean_data$steps, by=list(interval=act_clean_data$interval, day=act_clean_data$day), FUN=mean,na.rm=TRUE)
agg$day=as.factor(agg$day)
names(agg)=c("interval", "day", "avg.steps")
xyplot(avg.steps~interval|day, data=agg, main="Avg. steps in 5-min interval", xlab="interval", ylab="avg. steps", type="l", layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
