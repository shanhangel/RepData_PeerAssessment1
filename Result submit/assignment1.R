##Loading and preprocessing the data
##Show any code that is needed to
##Load the data (i.e. read.csv())
##Process/transform the data (if necessary) into a format suitable for your analysis

setwd()
activity <- read.csv("./Result submit/activity.csv")

##What is mean total number of steps taken per day?
##For this part of the assignment, you can ignore the missing values in the dataset.
##Make a histogram of the total number of steps taken each day
##Calculate and report the mean and median total number of steps taken per day

mean_by_day <- tapply(activity$steps, activity$date, mean, na.rm=TRUE)
