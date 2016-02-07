#read in data
tracker <- read.csv("./activity.csv")
#processing data
head(tracker)
class(tracker$date)
class(tracker$interval)
levels(tracker$date)
tracker$factorinterval <- as.factor(tracker$interval)

#total steps a day
stepsday <- with(tracker, tapply(steps, date, sum, na.rm = TRUE))

#histogram
png("histogram1.png")
hist(stepsday, xlab = "Steps per Day", main = "Histogram of Steps per Day", 
     col = "red")
dev.off()

#median/mean of steps per day
mean(stepsday)
median(stepsday)

#time series plot
meaninterval <- with(tracker, tapply(steps, factorinterval, mean, 
                                     na.rm = TRUE, simplify = FALSE))

png("TimeSeries1.png")
plot(unique(tracker$interval), meaninterval, type = "l", xlim = c(0, 2400), 
     xlab = "5 min Intervals", ylab = "Average Number of Steps", 
     main = "Average Number of Steps Based on 5 Minute Intervals")
dev.off()

#Max average steps for intervals
stepsday1 <- aggregate(tracker$steps, by = list(tracker$factorinterval), 
                       FUN = mean, na.rm = TRUE)
stepsday1 <- stepsday1[order(stepsday1$x),]
tail(stepsday1, n = 1)

#number of NAs in the dataset
sum(is.na(tracker$steps))

#replace NAs with mean
#get means for each day
daymeans <- aggregate(tracker$steps, by = list(tracker$date), FUN = mean, 
                      na.rm = TRUE)
#get total mean
rounded <- mean(tracker$steps, na.rm = TRUE)
#replace NAs from daymeans with total mean
daymeans[is.na(daymeans)] <- rounded

tracker$steps[tracker$date == '2012-10-01'] <- daymeans$x[daymeans$Group.1 == '2012-10-01']
tracker$steps[tracker$date == '2012-10-08'] <- daymeans$x[daymeans$Group.1 == '2012-10-08']
tracker$steps[tracker$date == '2012-11-01'] <- daymeans$x[daymeans$Group.1 == '2012-11-01']
tracker$steps[tracker$date == '2012-11-04'] <- daymeans$x[daymeans$Group.1 == '2012-11-04']
tracker$steps[tracker$date == '2012-11-09'] <- daymeans$x[daymeans$Group.1 == '2012-11-09']
tracker$steps[tracker$date == '2012-11-10'] <- daymeans$x[daymeans$Group.1 == '2012-11-10']
tracker$steps[tracker$date == '2012-11-14'] <- daymeans$x[daymeans$Group.1 == '2012-11-14']
tracker$steps[tracker$date == '2012-11-30'] <- daymeans$x[daymeans$Group.1 == '2012-11-30']

stepsday1 <- with(tracker, tapply(steps, date, sum, na.rm = TRUE))
png("histogram2.png")
hist(stepsday1, xlab = "Steps per Day", main = "Histogram of Steps per Day", 
     col = "red")
dev.off()
mean(stepsday1)
median(stepsday1)

# Create a Factor for weekday and weekend
tracker$weekday <- as.Date(tracker$date, format = "%Y-%m-%d")
tracker$weekday <- weekdays(tracker$weekday)
tracker$weekday[tracker$weekday == "Monday"] <- "Weekday"
tracker$weekday[tracker$weekday == "Tuesday"] <- "Weekday"
tracker$weekday[tracker$weekday == "Wednesday"] <- "Weekday"
tracker$weekday[tracker$weekday == "Thursday"] <- "Weekday"
tracker$weekday[tracker$weekday == "Friday"] <- "Weekday"
tracker$weekday[tracker$weekday == "Saturday"] <- "Weekend"
tracker$weekday[tracker$weekday == "Sunday"] <- "Weekend"
tracker$weekday <- as.factor(tracker$weekday)

#time series plot weekend vs weekday
weekday <- subset(tracker, tracker$weekday == "Weekday")
weekend <- subset(tracker, tracker$weekday == "Weekend")

meanweekday <- with(weekday, tapply(steps, factorinterval, mean, 
                                     na.rm = TRUE, simplify = TRUE))
meanweekend <- with(weekend, tapply(steps, factorinterval, mean, 
                                    na.rm = TRUE, simplify = TRUE))

intval <- c(unique(tracker$interval), unique(tracker$interval))
days <- rep("Weekday", times = 288)
ends <- rep("Weekend", times = 288)
dfstep <- c(meanweekday, meanweekend)
week <- c(days, ends)
means <- data.frame(intval, dfstep, week)

png("timeseries2.png")
ggplot(data = means, aes(intval, dfstep, fill = week)) + geom_line() +
      facet_grid(week ~ .) + xlab("Interval") +
      ylab("Average Steps") + ggtitle("Average Steps Weekend Vs. Weekday")
dev.off()

