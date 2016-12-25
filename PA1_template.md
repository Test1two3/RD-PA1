# PA1_template

#Code for reading in the dataset and/or processing the data

```r
temp <- tempfile()
download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp)
A <- read.csv(unz(temp, "activity.csv"), header = TRUE, sep = ",", na.string = "NA")
unlink(temp)
```

#Histogram of the total number of steps taken each day

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.2
```

```r
A$date <- as.Date(A$date)
TS <- tapply(A$steps, A$date, FUN=sum, na.rm=TRUE)
qplot(TS, fill = I("red"), geom = "histogram", xlab="total number of steps taken", binwidth = 1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

#Mean and median number of steps taken each day

```r
TS <- tapply(A$steps, A$date, FUN=sum, na.rm=TRUE)
mean(TS, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(TS, na.rm=TRUE)
```

```
## [1] 10395
```


#Time series plot of the average number of steps taken

```r
ave <- aggregate(x=list(steps=A$steps), by=list(interval=A$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=ave, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("interval(5 minutes)") +
    ylab("average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


#The 5-minute interval that, on average, contains the maximum number of steps

```r
ave[which.max(ave$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```


#Code to describe and show a strategy for imputing missing data

```r
missing <- is.na(A$steps)
# Show how many missing values the dataset contains
table(missing)
```

```
## missing
## FALSE  TRUE 
## 15264  2304
```

```r
# Replace each missing value with the average of its interval category
Overwrite <- function(steps, interval) {
    val <- NA
    if (!is.na(steps))
        val <- c(steps)
    else
        val <- (ave[ave$interval==interval, "steps"])
    return(val)
}
A2 <- A
A2$steps <- mapply(Overwrite, A2$steps, A2$interval)
```

#Histogram of the total number of steps taken each day after missing values are imputed

```r
TS2 <- tapply(A2$steps, A2$date, FUN=sum)
qplot(TS, fill = I("green"), geom = "histogram", xlab="total number of steps taken", binwidth = 1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
#Identify weekdays and weekends
wow <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
A2$date <- as.Date(A2$date)
A2$day <- sapply(A2$date, FUN=wow)


averages <- aggregate(steps ~ interval + day, data=A2, mean)
ggplot(averages, aes(interval, steps), colour = day) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps") 
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->




