# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
d <- read.csv('activity.csv')
```


## What is mean total number of steps taken per day?

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







