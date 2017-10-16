library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(R.utils)

if(!file.exists("repdata-data-StormData.csv.bz2"))
  download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                destfile="repdata-data-StormData.csv.bz2")
if(!file.exists("repdata-data-StormData.csv"))
  bunzip2("repdata-data-StormData.csv.bz2", overwrite=TRUE, remove=FALSE)

data <- read.csv('C:/Users/srika/Documents/R/John Hopkins/Reproducible Research/repdata%2Fdata%2FStormData.csv')
str(data)

requiredData <- select(data, BGN_DATE,EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
requiredData$byYear <- as.numeric(format(as.Date(requiredData$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y"))
stormHist <- ggplot(requiredData, aes(byYear)) +
  geom_histogram(bins = 25, fill = 'blue') +
  ggtitle('Weather Fatilities by Year')
stormHist

fatalities <- requiredData %>%
  group_by(EVTYPE) %>%
  summarise(TotalFatilities = sum(FATALITIES)) %>%
  arrange(desc(TotalFatilities)) %>%
  top_n(10)
eventFatalities <- ggplot(fatalities, aes(x = EVTYPE, y = TotalFatilities)) +
  geom_bar(stat = 'identity',fill = 'purple') +
  xlab('Event Type') +
  ggtitle('Fatilities by Event')
eventFatalities

injuries <- requiredData %>%
  group_by(EVTYPE) %>%
  summarise(TotalInjured = sum(INJURIES)) %>%
  arrange(desc(TotalInjured)) %>%
  top_n(10)
eventInjured <- ggplot(injuries, aes(x = EVTYPE, y = TotalInjured)) +
  geom_bar(stat = 'identity', fill = 'green') +
  xlab('Event Type') +
  ggtitle('People Injured by Event')
eventInjured


unique(requiredData$PROPDMGEXP)
unique(requiredData$CROPDMGEXP)


requiredData$PROPDMGEXP <- as.character(requiredData$PROPDMGEXP)
requiredData$PROPDMGEXP <- gsub("\\-|\\+|\\?","0",requiredData$PROPDMGEXP)
requiredData$PROPDMGEXP <- gsub('B|b', '9', requiredData$PROPDMGEXP)
requiredData$PROPDMGEXP <- gsub('M|m', '6', requiredData$PROPDMGEXP)
requiredData$PROPDMGEXP <- gsub('K|k', '3', requiredData$PROPDMGEXP)
requiredData$PROPDMGEXP <- gsub('H|h', '2', requiredData$PROPDMGEXP)
requiredData$PROPDMGEXP <- as.numeric(requiredData$PROPDMGEXP)
requiredData$PROPDMGEXP[is.na(requiredData$PROPDMGEXP)] <- 0
requiredData$TrueDamage <- requiredData$PROPDMG * 10^requiredData$PROPDMGEXP

propertyDamage <- aggregate(TrueDamage ~ EVTYPE, data = requiredData, sum)
propertyDamage <- propertyDamage[order(-propertyDamage$TrueDamage),]
propertyDamage <- propertyDamage[1:10,]


requiredData$CROPDMGEXP <- as.character(requiredData$CROPDMGEXP)
requiredData$CROPDMGEXP <- gsub("\\-|\\+|\\?","0",requiredData$CROPDMGEXP)
requiredData$CROPDMGEXP <- gsub('B|b', '9', requiredData$CROPDMGEXP)
requiredData$CROPDMGEXP <- gsub('M|m', '6', requiredData$CROPDMGEXP)
requiredData$CROPDMGEXP <- gsub('K|k', '3', requiredData$CROPDMGEXP)
requiredData$CROPDMGEXP <- gsub('H|h', '2', requiredData$CROPDMGEXP)
requiredData$CROPDMGEXP <- as.numeric(requiredData$CROPDMGEXP)
requiredData$CROPDMGEXP[is.na(requiredData$CROPDMGEXP)] <- 0
requiredData$TrueCropDamage <- requiredData$CROPDMG * 10^requiredData$CROPDMGEXP

cropDamage <- aggregate(TrueCropDamage ~ EVTYPE, data = requiredData, sum)
cropDamage <- cropDamage[order(-cropDamage$TrueCropDamage),]
cropDamage <- cropDamage[1:10,]

damageCombined <- aggregate(TrueDamage + TrueCropDamage ~ EVTYPE, data = requiredData, sum)
names(damageCombined)[2] <- 'Total_Damage'
damageCombined <- damageCombined %>%
  arrange(desc(Total_Damage)) %>%
  top_n(10)
propertyDamage
cropDamage
damageCombined  


par(mfrow = c(1,3))
DamagetoCrops <- ggplot(cropDamage, aes(x = EVTYPE, y = TrueCropDamage)) +
  geom_bar(stat = 'identity', fill = 'red') +
  xlab('Event Type') +
  ggtitle('Crop Damage by Event Type')
damagetoProperty <- ggplot(propertyDamage, aes(x = EVTYPE, y = TrueDamage)) +
  geom_bar(stat = 'identity', fill = 'red') +
  xlab('Event Type') +
  ggtitle('Property Damage by Event Type')

fullDamage <- ggplot(damageCombined, aes(x = EVTYPE, y = Total_Damage)) +
  geom_bar(stat = 'identity', fill = 'red') +
  xlab('Event Type') +
  ggtitle('Total Damage by Event Type')

DamagetoCrops
damagetoProperty
fullDamage
