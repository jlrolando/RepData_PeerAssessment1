##question1

activity<-read.csv(unz("activity.zip","activity.csv"),header=TRUE)
activity$date<-as.Date(activity$date)
activity.day<-tapply(activity$steps,activity$date, sum, na.rm=TRUE)
hist(activity.day, main="Histogram of steps per day", xlab = "Number of steps",
     ylab="Frequency of days", breaks = 10)
mean(activity.day)
median(activity.day)

##question2

act.int<-tapply(activity$steps,activity$interval,mean, na.rm=TRUE)
plot(unique(activity$interval),act.int, type="l")
maximum<-act.int==max(act.int)
names(maximum[maximum==TRUE])

##question3

sum(is.na(activity$steps))

activity.new<-activity

for(i in 1:length(activity.new$steps)) {
      if (is.na(activity.new$steps[i])==TRUE) {
            activity.new$steps[i] <- mean(activity.new$steps[activity.new$interval==activity.new$interval[i]], na.rm=T)
      }
      else next()
}

act.new.day<-tapply(activity.new$steps,activity.new$date, sum, na.rm=TRUE)
hist(act.new.day, main="Histogram of steps per day", xlab = "Number of steps",
     ylab="Frequency of days")
mean(act.new.day)
median(act.new.day)

##question4

activity.new$day<-weekdays(activity.new$date)
activity.new$weekday<-character(length = 17568)

for (i in 1:length(activity.new$day)) {
      if (activity.new$day[i] == "Saturday" | activity.new$day[i] == "Sunday") {
            activity.new$weekday[i] <- "Weekend"
      }
      else {
            activity.new$weekday[i] <- "Weekday"
      }
}

weekend<-subset(activity.new, weekday=="Weekend")
weekend.int<-tapply(weekend$steps,weekend$interval, mean)
weekend.int<-as.numeric(weekend.int)

weekday<-subset(activity.new, weekday=="Weekday")
weekday.int<-tapply(weekday$steps,weekday$interval, mean)
weekday.int<-as.numeric(weekday.int)

activity.weekday<-data.frame(row.names = 1:576)
activity.weekday$steps<-c(weekend.int,weekday.int)
activity.weekday$day<-c(rep("Weekend",288),rep("Weekday",288))
activity.weekday$interval<-rep(unique(activity.new$interval),2)

library(ggplot2)

g<-ggplot(activity.weekday,aes(interval,steps))
g+geom_line()+theme_bw()+facet_grid(day~.)
