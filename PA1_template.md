# Wearable Pedometer/Activity Tracker Mini-Study

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as the Fitbit, Nike Fuelband, Galaxy Gear Fit or Jawbone Up. These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or to affiliate themselves with the tech community. But, typically, this data remains under-utilized both because the raw data are hard to obtain and because there is a lack of statistical methods and software for processing and interpreting the data that are easily understood by the layperson.

The following short study makes use of data obtained from a personal activity monitoring device. This device collected pedometer (step tracking) data from an anonymous subject at 5 minute intervals through out the day for two months (October ~ November, 2012).

## Loading and preprocessing the data

To begin this study, the reader will need to load a few libraries. If you have not installed them you will need to install ggplot, gridExtra and dplyr using:

1. install.packages("ggplots")
2. install.packages("gridExtra")
3. install.packages("dplyr"). 

```{r packageInstalls, results='hide'}
# load the libraries we will need
library(ggplot2)
library(dplyr)
library(gridExtra)

```

The activity data is provided in the github repo and can be downloaded from the following URL:

https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

The following R code "Chunk"" provides a method for obtaining the data that accounts for both scenarios where the file needs to be downloaded, unzipped, both or is otherwise present in the root directory where the .Rmd file is located. 

```{r fileload}
   unzip("activity.zip")

##read file using read.csv
activityData<-read.csv("./activity.csv", header=TRUE, sep=",", )
##convert date column from factor to Date class
activityData$date<-as.Date(activityData$date)
##provide a summary of the activityData table
summary(activityData)
## variables for documenting the observations and variables for the activity dataset
obs<-nrow(activityData)
vars<-ncol(activityData)
```

The file, "activity.csv", consists of `r obs` observation of `r vars` variables.

Just looking at the summary, this data set has a number of missing observations, or NA data points. I first want to create a dataset that has those NAs removed (We will impute this missing data later). In this dataset I will also make the date a factor which will help me when I subset the data using dplyr.

```{r na.omits}
activityDataComplete <- na.omit(activityData)
activityDataComplete$date <- as.factor(activityDataComplete$date)
##provide a summary of the activityDataComplete table
summary(activityDataComplete)
## variables for documenting the observations and variables for the activity dataset
obs2<-nrow(activityDataComplete)
vars2<-ncol(activityDataComplete)
```

The activityDataComplete table consists of only `r obs2` observation of `r vars2` variables.

## What is mean total number of steps taken per day? The median?

To subset the data, calculate the sum, median and mean of the daily step count, I have created a third data set based on the activityDataCleaned dataset. I am using Hadley Wickham's dplyr package. The package is fast and elegant and uses a new syntax that is becoming very popular for its readability. 

```{r groupbydate}
calcActivity <- activityDataComplete %>% 
        group_by(date) %>% 
        summarise (step.sum = sum(steps), step.mean = round(mean(steps)), step.median=median(steps)) 
calcActivity
```


Now that I have all the data subset and calculated it's possible to show the distribution of steps across the two month recording window in a histogram with the daily stepcount mean and median provided in the legend.

```{r overallmeanhistogram,fig.width=10}
dMean <- round(mean(calcActivity$step.sum))
dMedian <- median(calcActivity$step.sum)

p <- ggplot(calcActivity, aes(x=step.sum)) + geom_histogram(binwidth=750, colour="blue", fill="white")
p <- p+geom_vline(aes(xintercept=dMean), color=554, size = 5, linetype = "solid", alpha=0.45)
p <- p+geom_vline(aes(xintercept=dMedian), color=48, linetype = "dashed", size=1, alpha=1)
p <-  p+ggtitle("A Histogram of the Total Step Counts per Day for the month of October ~ November")+ theme(plot.title = element_text(lineheight=.8, face="bold"))
p<-p+xlab("StepCount Totals per Day") + ylab("StepCount Frequencies")
p <- p+geom_text(aes(dMean,0,label = "mean =", hjust=-1, vjust = -23))
p <- p+geom_text(aes(dMean,0,label = dMean, hjust=-2.5, vjust = -23))
p <- p+geom_text(aes(dMedian,0,label = dMedian, hjust=-2.5, vjust = -21))
p <- p+geom_text(aes(dMedian,0,label = "median = ", hjust=-0.5, vjust = -21))
p
```


the mean stepcount (October~November): `r dMean`

the mean stepcount (October~November): `r dMedian`

## What is the average daily activity pattern?

To observe the pedometer wearer's daily activity we can look at the number of steps in each interval across a given day. This would give us a general trend line for the users activity in that day. If we take the mean of each intervals across every day in the two month recording window, we might be able to observe the users overall activity patterns during the sample period.

```{r dailygroup.summarize}
##make a new complete and clean data set
activityDataComplete2 <- na.omit(activityData)
## convert the interval col to a factor
activityDataComplete2$interval <- as.factor(activityDataComplete2$interval)
##group by interval factor and average
calcActivity2 <- activityDataComplete2 %>% 
        group_by(interval) %>% 
        summarise (step.mean = mean(steps))
calcActivity2
```

```{r dailylineplot, fig.width=10}
l <- ggplot(data=calcActivity2, aes(x=as.numeric(levels(interval))[interval], y=step.mean, group=1)) + geom_line(colour="red", linetype="solid", size=2) + geom_point(colour="green", size=1.5, shape=21, fill="black")
l <- l + scale_x_continuous(breaks=c(0,500,1000, 1500, 2000, 2500))
l <- l + ggtitle("Average Number of Steps per Interval from the Month of October ~ November")+ theme(plot.title = element_text(lineheight=.8, face="bold"))
l <- l + xlab("StepCount Recording Interval (800 = 8:00AM, etc)") + ylab("Average StepCount")
l
```

In order to find the most active time of the day, on average, across the 2 month recording window, we would only need to call the following piece of code in R:

```{r maxactivity}
calcActivity2[which.max(calcActivity2$step.mean),]
```

The most active interval for this particular user is approximatly 200+ steps at the 8:35AM interval.

## Imputing missing values

Missing values are a problem with any activity tracking dataset. The user occasionaly forgets to wear their pedometer or the battery dies and data will be missing. There are packages like impute and others designed to fill in this missing data with logical equivalents. In this examples we will use a more simple method and use our list of average steps per interval to fill in the missing data.

First we can make an array with the missing values
```{r missingdata}
missingValues<-activityData[which(is.na(activityData)),]
```

and we can determine how many missing values there are using either summary orthe following code chunk:

```{r sizeofmissingdata}
nrow(missingValues)
```

And now we can use R to loop through our original dataActivity dataset and select the intervals where the step count is equal to NA and replace those "steps" with the step.mean of the same index from our calcactivity2 dataset (which was a the daily averages for each interval minus the NAs).

I will copy the orginal dataset into a new temp dataset (ad) in order to preserve the original data with NAs

```{r basicmissingdataloop}
ad<- activityData
for(i in 1:nrow(ad)){
        if(is.na(ad[i,]$steps)){
                temp<-ad[i,]$interval  
                ad[i,]$steps <- calcActivity2[which(calcActivity2$interval==temp),]$step.mean
        }        
}
summary(ad)
```

Now we can perform our original calculations to compute a new histogram. This will be the same calculations as before except the NAs that we removed before have now been imputed with daily averages and we can see the overall effect of the imputation on the mean, median and daily averages.

```{r imputedgroup.summarise}
calcActivity3 <- ad %>% 
        group_by(date) %>% 
        summarise (step.sum = sum(steps), step.mean = round(mean(steps)), step.median=median(steps)) 
calcActivity3
```


```{r missingvalueshistogram,fig.width=10}
dMean3 <- round(mean(calcActivity3$step.sum))
dMedian3 <- round(median(calcActivity3$step.sum))

p <- ggplot(calcActivity3, aes(x=step.sum)) + geom_histogram(binwidth=750, colour="blue", fill="white")
p <- p+geom_vline(aes(xintercept=dMean), color=554, size = 5, linetype = "dashed", alpha=0.45)
p <- p+geom_vline(aes(xintercept=dMedian), color=48, linetype = "dotted", size=1, alpha=1)
p <-  p+ggtitle("A Histogram of the Total Step Counts per Day for the Months October ~ November")+ theme(plot.title = element_text(lineheight=.8, face="bold"))
p<-p+xlab("StepCount Totals per Day") + ylab("StepCount Frequencies")
p <- p+geom_text(aes(dMean3,0,label = "mean =", hjust=-1, vjust = -23))
p <- p+geom_text(aes(dMean3,0,label = dMean3, hjust=-2.5, vjust = -23))
p <- p+geom_text(aes(dMedian3,0,label = dMedian3, hjust=-2.5, vjust = -21))
p <- p+geom_text(aes(dMedian3,0,label = "median = ", hjust=-0.5, vjust = -21))

p

```

The new calculations are nearly identical to the first histogram with the NAs removed. The imputation had only a small effect on the overall median. 

the mean stepcount (October~November): `r dMean3`

the mean stepcount (October~November): `r dMedian3`

## Are there differences in activity patterns between weekdays and weekends?

The last plot will explore the differences in activity based on the day of the week. I will make a plot similar to the line plot above, where the steps per interval are averaged across every day during the two month sample period. The only difference this time is that I will factor out the weekend days from the weekdays and see if there is any discernable changes between them.

First we need to create a new column in our data frame to be able to select for weekends or weekdays. First, we will need to convert our date to a Date class.

```{r createcol}
ad$date <- as.Date(ad$date)
class(ad$date)

temp2<-ad
```

Then we can create the new column using dplyr and the weekday() function. I will also convert this character to a factor to prepare for grouping and summary operations.

```{r mutateddataset}
##mutate a new col using weekdays, an inline ifelse and dplyr
temp3 <- mutate(temp2, day = ifelse(weekdays(ad$date)=="Sunday" | weekdays(ad$date)=="Saturday", "weekend", "weekday"))

## convert to a factor
temp3$day<-as.factor(temp3$day)
##provide a summary to confirm
summary(temp3)
```

Using the dplyr package, we group and summarise the data by the weekend and weekday factor

```{r weekdayandweekendsets}
##group by interval factor and average
temp3$interval <- as.factor(temp3$interval)

calcActivity5 <- temp3 %>% 
        filter(day=="weekend") %>%
        group_by(interval) %>% 
        summarise (step.mean = mean(steps))
calcActivity5

calcActivity4 <- temp3 %>% 
        filter(day=="weekday") %>%
        group_by(interval) %>% 
        summarise (step.mean = mean(steps))
calcActivity4
```

And then create one line plot for each of the average weekend and weekday activities, by sample interval, across the two month sample period.

```{r lineplot3, fig.width=10}
l <- ggplot(data=calcActivity5, aes(x=as.numeric(levels(interval))[interval], y=step.mean, group=1)) + geom_line(colour=554, linetype="solid", size=2) + geom_point(colour=78, size=1.5, shape=21, fill="white")
l <- l + scale_x_continuous(breaks=c(0,500,1000, 1500, 2000, 2500))
l <- l + ggtitle("Average Number of Steps per Interval on Weekends")+ theme(plot.title = element_text(lineheight=.8, face="bold"))
l <- l + xlab("StepCount Recording Interval (800 = 8:00AM, etc)") + ylab("Average StepCount")
```
```{r lineplot4, fig.width=10}
r <- ggplot(data=calcActivity4, aes(x=as.numeric(levels(interval))[interval], y=step.mean, group=1)) + geom_line(colour=554, linetype="solid", size=2) + geom_point(colour=78, size=1.5, shape=21, fill="white")
r <- r + scale_x_continuous(breaks=c(0,500,1000, 1500, 2000, 2500))
r <- r + ggtitle("Average Number of Steps per Interval on WeekDays")+ theme(plot.title = element_text(lineheight=.8, face="bold"))
r <- r + xlab("StepCount Recording Interval (800 = 8:00AM, etc)") + ylab("Average StepCount")
```

we can use the gridExtra package and the function grid.arrange to arrange these two plots to be condensed onto the same panel.

```{r panel plot, fig.width=10}
grid.arrange(l, r, ncol = 1, nrow = 2)
```

We can see that this particular user had a higher activity level on average throughout the day on the weekend but a lower peak activity level between 5AM ~ 10AM. This could be for a number of reasons including time spent at work (likley sitting or not moving their feet) and a morning commute on foot during the weekdays vs more distributed (across the day) and sustained activity on the weekends.
