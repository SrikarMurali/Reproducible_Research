Load Data
---------

``` r
data <- read.csv('C:/Users/srika/Documents/R/John Hopkins/Reproducible Research/activity.csv', header = TRUE)
data2 <- na.omit(data)

data2$date <- as.Date(data2$date)
```

Mean total number of steps
--------------------------

You can also embed plots, for example:

``` r
dailySteps <- data2 %>%
  group_by(date) %>%
  summarise(TotalSteps = sum(steps))
dailySteps
```

    ## # A tibble: 53 x 2
    ##          date TotalSteps
    ##        <date>      <int>
    ##  1 2012-10-02        126
    ##  2 2012-10-03      11352
    ##  3 2012-10-04      12116
    ##  4 2012-10-05      13294
    ##  5 2012-10-06      15420
    ##  6 2012-10-07      11015
    ##  7 2012-10-09      12811
    ##  8 2012-10-10       9900
    ##  9 2012-10-11      10304
    ## 10 2012-10-12      17382
    ## # ... with 43 more rows

``` r
stepHist <- ggplot(dailySteps, aes(TotalSteps)) +
  geom_histogram(fill = 'blue') +
  xlab('Total Number of steps per day') +
  ylab('Count') +
  ggtitle('Histogram of steps by day')
stepHist
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](PA1_template_files/figure-markdown_github/mean1-1.png)

``` r
averageSteps <- mean(dailySteps$TotalSteps)
#Average number of steps
averageSteps
```

    ## [1] 10766.19

``` r
medianSteps <- median(dailySteps$TotalSteps)
#Median number of steps
medianSteps
```

    ## [1] 10765

``` r
stepsTSeries <- xts(dailySteps$TotalSteps, order.by = dailySteps$date)
```

The average number of steps is 10766.19. The median number of steps is 10765.

``` r
intervalTSeries <- data2 %>%
  group_by(interval) %>%
  summarise(AvgSteps = mean(steps))
intervalTSeries
```

    ## # A tibble: 288 x 2
    ##    interval  AvgSteps
    ##       <int>     <dbl>
    ##  1        0 1.7169811
    ##  2        5 0.3396226
    ##  3       10 0.1320755
    ##  4       15 0.1509434
    ##  5       20 0.0754717
    ##  6       25 2.0943396
    ##  7       30 0.5283019
    ##  8       35 0.8679245
    ##  9       40 0.0000000
    ## 10       45 1.4716981
    ## # ... with 278 more rows

``` r
plot(intervalTSeries$interval, intervalTSeries$AvgSteps,
     type = 'l',
     xlab = 'Interval',
     ylab = 'Average Steps',
     main = 'Average Steps by Interval')
```

![](PA1_template_files/figure-markdown_github/average%20daily%20pattern-1.png)

``` r
maxSteps <- intervalTSeries$interval[which.max(intervalTSeries$AvgSteps)]
maxSteps
```

    ## [1] 835

The 835th interval had the max steps.

Remove Na's
-----------

``` r
nas <- sum(is.na(data$steps))
nas
```

    ## [1] 2304

``` r
filledData <- data

meansteps <- mean(data2$steps)
meansteps
```

    ## [1] 37.3826

``` r
filledData$steps[is.na(filledData$steps)] <- meansteps

filledDataNas <- sum(is.na(filledData$steps))
filledDataNas
```

    ## [1] 0

``` r
dailySteps2 <- filledData %>%
  group_by(date) %>%
  summarise(TotalSteps = sum(steps))
dailySteps2
```

    ## # A tibble: 61 x 2
    ##          date TotalSteps
    ##        <fctr>      <dbl>
    ##  1 2012-10-01   10766.19
    ##  2 2012-10-02     126.00
    ##  3 2012-10-03   11352.00
    ##  4 2012-10-04   12116.00
    ##  5 2012-10-05   13294.00
    ##  6 2012-10-06   15420.00
    ##  7 2012-10-07   11015.00
    ##  8 2012-10-08   10766.19
    ##  9 2012-10-09   12811.00
    ## 10 2012-10-10    9900.00
    ## # ... with 51 more rows

``` r
stepHist2 <- ggplot(dailySteps2, aes(TotalSteps)) +
  geom_histogram(fill = 'blue') +
  xlab('Total Number of steps per day') +
  ylab('Count') +
  ggtitle('Histogram of steps by day')
stepHist2
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](PA1_template_files/figure-markdown_github/NA-1.png)

``` r
medianSteps2 <- median(dailySteps2$TotalSteps)
medianSteps2
```

    ## [1] 10766.19

``` r
meanSteps2 <- mean(dailySteps2$TotalSteps)
meanSteps2
```

    ## [1] 10766.19

There are 2304 NA's. The NA's were replaced by the mean. The mean stayed the same while the median became the mean, since all of the NA's were replaced by the mean. The histogram became slightly different, with it being more bunched in the middle since all of the NA's are now the mean.

Weekends and Weekdays
---------------------

``` r
filledData$date <- as.Date(filledData$date)
filledData$Day <- weekdays(filledData$date)


filledData$dtype <- 'weekday'
filledData$dtype[filledData$Day %in% c('Saturday', 'Sunday')] <- 'weekend'

avgDay <- filledData %>%
  group_by(dtype, interval) %>%
  summarise(AvgSteps = mean(steps))
avgDay
```

    ## # A tibble: 576 x 3
    ## # Groups:   dtype [?]
    ##      dtype interval AvgSteps
    ##      <chr>    <int>    <dbl>
    ##  1 weekday        0 7.006569
    ##  2 weekday        5 5.384347
    ##  3 weekday       10 5.139902
    ##  4 weekday       15 5.162124
    ##  5 weekday       20 5.073235
    ##  6 weekday       25 6.295458
    ##  7 weekday       30 5.606569
    ##  8 weekday       35 6.006569
    ##  9 weekday       40 4.984347
    ## 10 weekday       45 6.584347
    ## # ... with 566 more rows

``` r
t <- ggplot(avgDay, aes(interval, AvgSteps)) +
  geom_line() + 
  xlab('Interval') +
  ylab('Average Step Count') +
  ggtitle('Average steps by Weekend vs Weekday') +
  facet_wrap(~dtype)
t
```

![](PA1_template_files/figure-markdown_github/weekdays-1.png)

Activity is earlier on the weekdays as people have to get up early to go to work or school or some other place.
