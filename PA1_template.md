---
title: "Reproducible Research - Peer-graded Assignment: Course Project 1"
output:
html_document:
keep_md: true
self_contained: true
author: "Evgenia Alberg"
date: "8 februari 2018"
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


###Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

###Loading and preprocessing the data
This part of the code is necessary for the inital loading of the dataset
```{r}
 
  
  amd <- read.csv("activity.csv")
  
```

###What is mean total number of steps taken per day?

```{r}
  #Total number of steps/Median per day
  amd.NAless <- na.omit(amd)
  library(ggplot2)
  totalsteps <- tapply(amd.NAless$steps, amd.NAless$date, FUN=sum)
  
  head(totalsteps)
  g <- ggplot(amd.NAless, aes(date,steps))
  g+geom_bar(stat="identity")+labs(title="Total Number of Steps per Day", x= "Date", y="Number of Steps")
  
```  
  
Total number of steps/day
```{r, echo=FALSE}
summary(totalsteps)

```
 The mean is 10770, and median is 10760
  
###What is the average daily activity pattern?
The code initializes the dataset and creates a plot to find the average steps vs time interval
```{r}
  AvgDailyActPat <- aggregate(steps~interval, data=amd.NAless, FUN=mean)
  plot1 <- plot(AvgDailyActPat, type="l")
  plot1
  AvgDailyActPat$interval[which.max(AvgDailyActPat$steps)]
##Imputing missing values

```{r}
  ImputedAMD <- merge(amd, AvgDailyActPat, by= "interval", suffixes = c("", ".y"))
  NAvals <- is.na(ImputedAMD$steps)
  ImputedAMD$steps[NAvals] <- ImputedAMD$steps.y[NAvals]
  ImputedAMD <- ImputedAMD[, c(1:3)]
  g2 <- ggplot(ImputedAMD, aes(date,steps))
  plot2 <- g2+geom_bar(stat="identity", color = "black")+labs(title="Imputed Total Number of Steps Taken per Day", x="Date", y="Daily Steps")
  plot2
```
  

```{r}
   
```

###Are there differences in activity patterns between weekdays and weekends?

The following creates subsets containing the weekday/weekend values of the dataset  
```{r}
  totalimputedsteps <- tapply(ImputedAMD$steps, ImputedAMD$date, FUN = sum)
  mean(totalimputedsteps)
  median(totalimputedsteps)
  ImputedAMDdays <- ImputedAMD
  Weekenddays <- weekdays(as.Date(ImputedAMDdays$date)) %in% c("Saturday", "Sunday")
  ImputedAMDdays$daytype <- "Weekday"
  ImputedAMDdays$daytype[Weekenddays == TRUE] <- "Weekend"
  ImputedAMDdays$daytype <- as.factor(ImputedAMDdays$daytype)
  head(ImputedAMDdays)
  # daily average steps by daytype
  AvgStepsByDayType <- aggregate(steps ~ interval + daytype, ImputedAMDdays, mean)
  names(AvgStepsByDayType)[3] <- "Avg_Steps"
  library(lattice)
  plot3 <- xyplot(Avg_Steps ~ interval | daytype, AvgStepsByDayType, type="l", layout=c(1,2), main = "Weekend vs Weekday Average Number of Steps", xlab="5 min Interval", ylab="Avg Steps taken")
  plot3
```
