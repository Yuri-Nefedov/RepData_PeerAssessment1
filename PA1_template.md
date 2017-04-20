# Course Project - Activity monitoring
Yuri Nefedov  
17 04 2017  



## Loading the data

Reading cs-file and checking it.


```r
act <- read.csv("./activity.csv")
dim(act)
```

```
## [1] 17568     3
```

```r
head(act)
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

## Mean total number of steps taken per day

Calculating steps by date with tapply().  
Missing values ignored.


```r
date_steps <- tapply(act$steps, act$date, sum)
hist(date_steps, xlab="Steps",col="blue",border="red",main="Total number of steps taken each day (NA's not filled)")
```

![](PA1_template-2_files/figure-html/mean with NA-1.png)<!-- -->

```r
mean(date_steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(date_steps, na.rm = TRUE)
```

```
## [1] 10765
```

## Average daily activity pattern

Calculating steps by date with tapply(). 
Missing values still ignored. Finding maximum.


```r
int_steps <- tapply(act$steps, act$interval, mean, na.rm = TRUE)
plot(int_steps, type = "l", col = "red", xlab='Intervals',lwd=3, ylab='Average number of steps', main ='Average number of steps taken in 5-minute interval, averaged across all days')
```

![](PA1_template-2_files/figure-html/average activity pattern-1.png)<!-- -->

```r
print(paste("Max is",max(int_steps),"at interval",names(int_steps)[which.max(int_steps)]))
```

```
## [1] "Max is 206.169811320755 at interval 835"
```

## Imputing missing values
Filling strategy - use mean for the interval, calculated for it through all filled dates.
They've been already calculated into variable int_steps.
Calculated mean and median don't look shifted comparing with those above.


```r
print(paste("Steps are NA in",sum(is.na(act$steps)),"intervals"))
```

```
## [1] "Steps are NA in 2304 intervals"
```

```r
act_imputed <- act
for (i in 1:nrow(act_imputed)) { 
  if (is.na(act_imputed$steps[i]) == TRUE) {
  st <- int_steps[as.character(act_imputed$interval[i])] 
  act_imputed$steps[i] <- st         
  }
}
date_steps_imp <- tapply(act_imputed$steps, act_imputed$date, sum)
hist(date_steps_imp, xlab="Steps",col="grey",border="green",main="Total number of steps taken each day (NA's filled)")
```

![](PA1_template-2_files/figure-html/imputting NA-1.png)<!-- -->

```r
mean(date_steps_imp)
```

```
## [1] 10766.19
```

```r
median(date_steps_imp)
```

```
## [1] 10766.19
```

## Differences in activity patterns between weekdays and weekends

Creating factor variable. Calculating step means for each interval differently for weekdays|ends. 


```r
week_day_end <- function(act_date) {
  wd <- weekdays(as.Date(act_date, "%Y-%m-%d"))
  if ((wd == "Saturday" || wd =="Sunday")||(wd == "суббота" || wd =="воскресенье")) ## need for RUS                                                                                     ## location
    x <- "Weekend" else x <- "Weekday"
  x
}
act_imputed$day_type <- as.factor(sapply(act_imputed$date, week_day_end))  ## adding factor variable
table(act_imputed$day_type)
```

```
## 
## Weekday Weekend 
##   12960    4608
```

```r
steps_per_day_impute <- aggregate(steps ~ interval+day_type, act_imputed, mean)
library(ggplot2)
plt <- ggplot(steps_per_day_impute, aes(interval, steps)) + geom_line(stat = "identity", aes(colour = day_type)) + theme_gray() + facet_grid(day_type ~ ., scales="fixed", space="fixed") + labs(x="Interval", y=expression("Quantity of Steps")) + ggtitle("Steps quantity per interval (by day type)")
print(plt)
```

![](PA1_template-2_files/figure-html/weekdays|weekends diffs-1.png)<!-- -->
