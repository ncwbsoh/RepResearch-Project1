### 1. Loading Data
Data = read.csv("activity.csv")


### 2. Histogram of Total Steps
sumDataDate = aggregate(Data$steps, by = list(Data$date), FUN = sum, na.rm = TRUE)
hist(sumDataDate$x, xlab = "steps per day", main = "Histogram of total steps per day", breaks = 20)

### 3. Median and Mean Total Steps
median(sumDataDate$x, na.rm = TRUE)
mean(sumDataDate$x, na.rm = TRUE)

### 4. Time-Series Plot of Average Steps by Interval
meanDataInter = aggregate(Data$steps, by = list(Data$interval), FUN = mean, na.rm = TRUE)
plot(meanDataInter$Group.1, meanDataInter$x, type = "l", xlab = "5-minute Intervals", ylab = "Average number of steps")

### 5. Interval With Maximum Number of Steps on Average
meanDataInter[which(meanDataInter$x == max(meanDataInter$x)),]$Group.1


### 6. Description and Showing Strategy for Imputing Missing Values
sum(is.na(Data))

imputeData = Data
#Get the vector of intervals with missing steps
naIntervals = imputeData$interval[is.na(imputeData$steps)]
#Impute the missing values with mean interval value by checking where the intervals are the same
imputeData$steps[is.na(imputeData$steps)] = meanDataInter$x[sapply(naIntervals, function(x) which(meanDataInter$Group.1 == x))]
head(imputeData)

### 7. Histogram of Total Steps After Imputing
sumImputeDate = aggregate(imputeData$steps, by = list(imputeData$date), FUN = sum, na.rm = TRUE)
hist(sumImputeDate$x, xlab = "steps per day", main = "Histogram of total steps per day", breaks = 20)

### 8. Panel Plot Comparing Average Steps by Interval on Weekdays vs. Weekends
weekEndData = Data[(weekdays(as.Date(Data$date)) %in% c("Saturday", "Sunday")),]
weekDayData = Data[!(weekdays(as.Date(Data$date)) %in% c("Saturday", "Sunday")),]

meanWeekEndInter = aggregate(weekEndData$steps, by = list(weekEndData$interval), FUN = mean, na.rm = TRUE)
meanWeekDayInter = aggregate(weekDayData$steps, by = list(weekDayData$interval), FUN = mean, na.rm = TRUE)

par (mfrow = c(2,1))
plot(meanWeekEndInter$Group.1, meanWeekEndInter$x, type = "l", xlab = "5-minute Intervals", ylab = "Average number of steps", main = "Week Ends")
plot(meanWeekDayInter$Group.1, meanWeekDayInter$x, type = "l", xlab = "5-minute Intervals", ylab = "Average number of steps", main = "Week Days")

