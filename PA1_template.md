# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

```r
d <- read.csv('activity.csv')
```


## What is mean total number of steps taken per day?

the number of stpes is devided by the number of days. very simple..

I used median function for median value.

```r
library(plyr)
out <- ddply(d, c("date"),summarize, steps=sum(steps))
n <- nrow(out)
mean_val <- sum(out$steps, na.rm = T) / n
median_val <- median(out$steps, na.rm=T)

mean_val
```

```
## [1] 9354.23
```

```r
median_val 
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1 day has 24 hours. 1 hour has 60 minutes. therefore 24*60 means minutes of a day.
24*60 is divided by 5 because they made data  every 5 minutes.



```r
days <- nrow(out)
interval_cnt <- 24*60/5   

out2 <- rep(0, interval_cnt)

for(i in 1:interval_cnt)
{
    some <- seq(i, nrow(d), by= interval_cnt)
    out2[i] <- mean( d[some,]$steps, na.rm=T)
}

plot(out2 , type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
max_interal <- max(out2)
```

## Imputing missing values

```r
d2 <- d
for(i in 1:nrow(d2) )
{
    if( is.na(d2[i,1])  == TRUE)
    {
       d2[i,1] <- out2[ i %% 288 + 1]

    }
}
```



## Are there differences in activity patterns between weekdays and weekends?
As we expected, peopel tend to move more on the weekend.
I'll calculate the average value of steps on the weekdays/weekends.


FYI, I set the language to english becuase I found out weekdays() returns  the value from this setting. some web browser might not support mine.

I also made the new column weekend.if the date is weekend, it gets 1. 

```r
out3 <- ddply(d2, c("date"),summarize, steps=sum(steps))

weekdays_n <- 0
weekend_n <- 0

weekdays_sum <-0
weekend_sum <- 0
for(i in 1:nrow(out3))
{
    if( weekdays( as.Date(out3[i,1])) == "읏?" || 
        weekdays( as.Date(out3[i,1])) == "탓?")
    {
       weekend_n <- weekend_n +1 
       weekend_sum <- weekend_sum + out3[i,2]
    
    }
    else
    {
       weekdays_n <- weekdays_n + 1
       weekdays_sum <- weekdays_sum + out3[i,2]
    
    }
}

# print average steps in weekend & weekdays
print( weekend_sum / weekend_n )
```

```
## [1] NaN
```

```r
print( weekdays_sum / weekdays_n )
```

```
## [1] 10766.19
```







p.S.
FYI.. most of the work was done it in the previous session. I modified that little bit..
