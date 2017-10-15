

library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(xts)

data <- read.csv('C:/Users/srika/Documents/R/John Hopkins/Reproducible Research/activity.csv', header = TRUE)
data2 <- na.omit(data)

data2$date <- as.Date(data2$date)



totalSteps <- sum(data2$steps)
totalSteps

dailySteps <- data2 %>%
  group_by(date) %>%
  summarise(TotalSteps = sum(steps))
dailySteps


stepHist <- ggplot(dailySteps, aes(TotalSteps)) +
  geom_histogram(fill = 'blue') +
  xlab('Total Number of steps per day') +
  ylab('Count') +
  ggtitle('Histogram of steps by day')
stepHist

averageSteps <- mean(dailySteps$TotalSteps)
averageSteps

medianSteps <- median(dailySteps$TotalSteps)
medianSteps

stepsTSeries <- xts(dailySteps$TotalSteps, order.by = dailySteps$date)
stepsTSeries

intervalTSeries <- data2 %>%
  group_by(interval) %>%
  summarise(AvgSteps = mean(steps))
intervalTSeries

plot(intervalTSeries$interval, intervalTSeries$AvgSteps,
     type = 'l',
     xlab = 'Interval',
     ylab = 'Average Steps',
     main = 'Average Steps by Interval')
maxSteps <- intervalTSeries$interval[which.max(intervalTSeries$AvgSteps)]
maxSteps

nas <- sum(is.na(data$steps))
nas

filledData <- data

meansteps <- mean(data2$steps)
meansteps
filledData$steps[is.na(filledData$steps)] <- meansteps

filledDataNas <- sum(is.na(filledData$steps))
filledDataNas

dailySteps2 <- filledData %>%
  group_by(date) %>%
  summarise(TotalSteps = sum(steps))
dailySteps2

stepHist2 <- ggplot(dailySteps2, aes(TotalSteps)) +
  geom_histogram(fill = 'blue') +
  xlab('Total Number of steps per day') +
  ylab('Count') +
  ggtitle('Histogram of steps by day')
stepHist2

medianSteps2 <- median(dailySteps2$TotalSteps)
medianSteps2

meanSteps2 <- mean(dailySteps2$TotalSteps)
meanSteps2

filledData$Day <- weekdays(filledData$date)
filledData$Day

filledData$dtype <- 'weekday'
filledData$dtype[filledData$Day %in% c('Saturday', 'Sunday')] <- 'weekend'

avgDay <- filledData %>%
  group_by(dtype, interval) %>%
  summarise(AvgSteps = mean(steps))
avgDay

t <- ggplot(avgDay, aes(interval, AvgSteps)) +
  geom_line() +
  facet_wrap(~dtype)
t

