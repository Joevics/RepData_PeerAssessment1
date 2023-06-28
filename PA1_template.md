#To load and process data
if (!file.exists('activity.csv')) {
  unzip('activity.zip',overwrite=TRUE)
}
activity <- read.csv("activity.csv", header = T, sep = ",")

#Calclaue the mean total number of steps taken per day?

# to calculate the total number of steps taken per day
shn <- tapply(activity$steps, activity$date, sum, na.rm=T)

#to create a Histogram of the total number of steps taken each day
hist(shn, xlab = "Steps sum", main = "Steps per day")

#To calculate and report the mean and median of the total number of steps taken per day
mean_su <- round(mean(shn))
median_su <- round(median(shn))
print(c("Mean",mean_su))
print(c("Median",median_su))

#What is the average daily activity pattern?
# to create a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
mn_int <- tapply(activity$steps, activity$interval, mean, na.rm=T)
plot(mn_int ~ unique(activity$interval), type="l", xlab = "5-min interval")
#5-minute interval, on average across all the days in the dataset, which contains the maximum number of steps
mn_int[which.max(mn_int)]

#To input missing values
#Total number of missing values in the dataset
table(is.na(activity) == TRUE)

#Strategy for filling in all of the missing values in the dataset: replace any NA with the mean of the corresponding interval
#New dataset that is equal to the original dataset but with the missing data filled in
activity2 <- activity
for (i in 1:nrow(activity)){
  if(is.na(activity$steps[i])){
    activity2$steps[i]<- mn_int[[as.character(activity[i, "interval"])]]
  }
}

#Histogram of the total number of steps taken each day
shu2 <- tapply(activity2$steps, activity2$date, sum, na.rm=T)
hist(shu2, xlab = "Steps sum", main = "Steps per day")

#New mean and median total number of steps taken per day
mean_su2 <- round(mean(shu2))
median_su2 <- round(median(shu2))

print(c("New Mean",mean_su2))
print(c("New Median",median_su2))

#Impact of imputing missing data on the estimates of the total daily number of steps: (a). We set about 14% of new values, all of them with the same value of the all mean, so the result is that we drive both the mean and median to values closer to the old mean. (b) Surprisingly, this drove both the median and the mean to the same value.


#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day
Sys.setenv(LANGUAGE = "en")
Sys.setlocale("LC_TIME", "English")
activity2$weekday <- c("weekday")
activity2[weekdays(as.Date(activity2[, 2])) %in% c("Saturday", "Sunday", "saturday", "sunday"), ][4] <- c("weekend")
activity2$weekday <- factor(activity2$weekday)

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
activity2_weekend <- subset(activity2, activity2$weekday == "weekend")
activity2_weekday <- subset(activity2, activity2$weekday == "weekday")
mean_activity2_weekday <- tapply(activity2_weekday$steps, activity2_weekday$interval, mean)
mean_activity2_weekend <- tapply(activity2_weekend$steps, activity2_weekend$interval, mean)

library(lattice)
df_weekday <- data.frame(interval = unique(activity2_weekday$interval), avg = as.numeric(mean_activity2_weekday), day = rep("weekday", length(mean_activity2_weekday)))
df_weekend <- data.frame(interval = unique(activity2_weekend$interval), avg = as.numeric(mean_activity2_weekend), day = rep("weekend", length(mean_activity2_weekend)))
df_final <- rbind(df_weekday, df_weekend)

xyplot(avg ~ interval | day, data = df_final, layout = c(1, 2), type = "l", ylab = "Average Number of Steps")
