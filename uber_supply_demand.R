#loading packages
install.packages("tidyverse")
install.packages("ggpubr")
library(tidyverse)
library(ggpubr)

##PART 0.1 - DATA CLEANING

#loading the file

data <- read.csv("Uber Request Data.csv")
View(data)

#cleaning the data

data$Request.timestamp <- gsub("/","-",data$Request.timestamp)
data$Drop.timestamp <- gsub("/","-",data$Drop.timestamp)

#separating columns to clean data
data <- separate(data, Request.timestamp, c("Request.Date", "Request.Time"), sep=" ")
data <- separate(data, Drop.timestamp, c("Drop.Date", "Drop.Time"), sep=" ")

#function to fix time discrepancies
add_seconds <- function(time) {
  new_time <- ifelse(nchar(time)<=5, paste(time,":00", sep=""),paste(time,"", sep=""))
  return(new_time)
}

data$Request.Time <- sapply(data$Request.Time, add_seconds)
data$Drop.Time <- sapply(data$Drop.Time, add_seconds)

data <- unite(data, Request.timestamp, Request.Date, Request.Time, sep=" ")
data <- unite(data, Drop.timestamp, Drop.Date, Drop.Time, sep=" ")

data$Request.timestamp <- as.POSIXct(data$Request.timestamp, format="%d-%m-%Y %H:%M:%S")
data$Drop.timestamp <- as.POSIXct(data$Drop.timestamp, format="%d-%m-%Y %H:%M:%S")

data <- data[,-9:-10] #Removing the dummy Time only columns which are of no use to us now

str(data)

## PART 0.2 - DERIVED VARIABLES

#Finding trip durations

data$Trip.duration <- data$Drop.timestamp - data$Request.timestamp

#Let us create a new column, where 1 stands for "Trip Completed" and 0 for anything else.

data$Status.bool <- ifelse(data$Status=="Trip Completed", 1, 0)
data$Status.bool <- as.factor(data$Status.bool)

#Let us create a column called Day.of.week.

data$Day.of.week <- weekdays(data$Request.timestamp)

#Let us create a new column called Driver.gap which tells the difference between a driver's drop time and the next request time.

data$Driver.gap <- data$Request.timestamp - lag(data$Drop.timestamp, 1)
data$Driver.gap[data$Driver.gap<0] <- 0 #making each Driver's first value as 0
data$Driver.gap <- round(data$Driver.gap)

## PART 1 - PLOTS

hist_status_1 <- ggplot(data, aes(x=Status, fill=Pickup.point))
hist_status_1 <- hist_status_1 + geom_histogram(stat="count") + ylab("Rides") + ggtitle("Status of Trips by Rides Categorised by Pickup Points")
hist_status_1

#We can clearly see that there is a significant number of trips with "No Cars Available" in trips from the Airport.

hist_status_2 <- ggplot(data, aes(x=Status.bool, fill=Pickup.point))
hist_status_2 <- hist_status_2 + geom_histogram(stat="count") + xlab("Status in Boolean") + ylab("Rides") + ggtitle("Status as Boolean where 1 represents Completed Rides")
hist_status_2

ggarrange(hist_status_1, hist_status_2, ncol=1, nrow=2)

#The second histogram tells us that more trips are being Cancelled or have Unavailability than those which are being Completed.

#This is clearly a supply-demand gap.

weekend_plot <- ggplot(data, aes(x=Day.of.week, fill=Pickup.point))
weekend_plot <- weekend_plot + geom_bar(stat="count", position="fill") + xlab("Day") + ylab("Rides") + ggtitle("A Comparison of Cab Request by the Day of the Week and Pickup Point")
weekend_plot

#We can see that the number of rides from the city to the airport increase on weekends which is natural but the change isn't as much.
#Also, oddly our data seems to have no records for Saturdays and Sundays, so focusing on weekends won't help as much.

weekend_status_plot <- ggplot(data, aes(x=Day.of.week, fill=Status))
weekend_status_plot <- weekend_status_plot + geom_bar(stat="count", position="fill") + xlab("Day") + ylab("Rides") + ggtitle("A Comparison of Cab Request Status by Day of the Week")
weekend_status_plot

ggarrange(weekend_plot, weekend_status_plot, nrow=2, ncol=1)

#There's also, no significant difference in the Status on different days.

#A thing to note is that the demand increases slightly on Thursdays and Fridays, and it is only on these days that
#the number of completed Trips is lesser.

#This could add to the supply-demand gap, naturally. Although not by a large extent.

#Finding Peak Hours

time_plot_1 <- ggplot(data, aes(x=format(Request.timestamp, format="%H")))
time_plot_1 + geom_histogram(stat="count") + xlab("Hours") + ylab("Rides") + ggtitle("Plot Showing Cab Requests by Hours")

#We can clearly see that there is a huge demand for cabs between 5-10AM and 5-10PM. We can consider these as Peak Hours.

peak <- c(05,06,07,08,09,10, 17,18,19,20,21,22)
data$is.Peak <- ifelse(as.integer(format(data$Request.timestamp, format="%H")) %in% peak, 1, 0)

peak_plot_1 <- ggplot(data, aes(x=factor(is.Peak), fill=Status))
peak_plot_1 <- peak_plot_1 + geom_histogram(stat="count", position = "dodge") + xlab("Is it a Peak Hour?") + ylab("Rides") + ggtitle("Plot showing Status of Rides to Rush Hour")
peak_plot_1

peak_plot_2 <- ggplot(data, aes(x=factor(is.Peak), fill=factor(Status.bool)))
peak_plot_2  <- peak_plot_2 + geom_histogram(stat="count", position = "dodge") + xlab("Is it a Peak Hour?") + ylab("Rides") + ggtitle("Plot showing Status of Rides to Rush Hour") + labs(fill = "Ride Status")
peak_plot_2 

#It is clear that the number of rides which aren't completed in peak hours exceeds the number of rides completed by a large margin.

ggarrange(peak_plot_1, peak_plot_2, ncol=2, nrow=1)

## PART 2 - QUANTITATIVE ANALYSIS

#Mean and Median Driver Gaps
average_driver_gap <- mean(data$Driver.gap, na.rm=T)
quantile(data$Driver.gap, na.rm=T)
average_driver_gap

#So, the average time that a driver has to wait for another trip is over 9 hours
#The median is roughly at 7.5 hours.
#We can assume that a driver changes his shifts after making a few trips.
#There's a huge gap between supply and demand as drivers only do about a couple or so rides

#Mean Trip Durations
average_trip_duration <- mean(data$Trip.duration, na.rm=T)
quantile(data$Trip.duration, na.rm = T)
average_trip_duration

#So, the average time that a driver spends on a trip is roughly 52 minutes.

##Assumptions

#The reason we're using na.rm=T is because trips which have NA as duration are irrelevant because they didn't happen.
#We're only focusing on trips that are Completed i.e. they have a Request and a Drop time.

#For Uber's context, a completed trip is supply while a requested trip is demand.
#This is the assumption we're going to use for our analysis.

supply <- sum(data$Status.bool==1)
demand <- as.integer(count(data))
lag <- sum(data$Status.bool==0)

lag_ratio <- lag/demand
supply_ratio <- supply/demand

lag_ratio
supply_ratio

#Currently, nearly 42% of the trips are being completed/supplied while 58% of trips are incomplete.

supply_peak <- sum(data$Status.bool==1 & data$is.Peak==1)
demand_peak <- as.integer(sum(data$is.Peak==1))
lag_peak <- sum(data$Status.bool==0 & data$is.Peak==1)

lag_peak_ratio <- lag_peak/demand_peak
supply_peak_ratio <- supply_peak/demand_peak

lag_peak_ratio
supply_peak_ratio

#Currently, during peak hours, only 38% rides are being completed/supplied while 62% rides are incomplete.

supply_not_peak <- sum(data$Status.bool==1 & data$is.Peak==0)
demand_not_peak <- as.integer(sum(data$is.Peak==0))
lag_not_peak <- sum(data$Status.bool==0 & data$is.Peak==0)

lag_not_peak_ratio <- lag_not_peak/demand_not_peak
supply_not_peak_ratio <- supply_not_peak/demand_not_peak

lag_not_peak_ratio
supply_not_peak_ratio

#Currently, during non-peak hours, 48% trips are completed/supplied while 52% trips are incomplete.

peak_supply_gap <- supply_peak_ratio - supply_ratio
peak_supply_gap

not_peak_supply_gap <- supply_not_peak_ratio - supply_ratio
not_peak_supply_gap

#During peak hours, the number of rides being completed/supplied decreases by 4%.

#During normal hours, the number of rides being completed/supplied increases by 10%

#This concludes the analysis for Supply-Demand Gap in Uber Services between the City and Airport

## PART 3 - EXPORTING FINAL DATA

write.csv(data, file = "uber_supply_demand.csv")

#The final data is cleaned, and has derived variables which could help in other applications such as Tableau or Excel
