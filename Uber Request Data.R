
##========================= ASSUMPTIONS =====================================

# We assume that working directory is set throught Session-> Set Working Directory, To the folder were all the following files are kept for the assignment.
# Or  
# Set working directory
setwd("folder_path")
getwd()

#============= CHECK  AND INSTALL PACAKGES ================

#Check if pacakges are installed or not and if not get them installed

check_and_install <- function(pkg) {
  if (!is.element(pkg, installed.packages()[, 1]))
    install.packages(pkg, dependencies  = TRUE)
}

check_and_install("tidyr")
check_and_install("dplyr")
check_and_install("stringr")
check_and_install("lubridate")
check_and_install("ggplot2")

# loading libraries
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

# ================Data-set: Uber Request Data===========
# Load data set in a variable called "uber"

uber <- read.csv("Uber Request Data.csv",stringsAsFactors = F)

# Check the structure of data
str(uber)



#====================Required Functions=================

#Function for replacing blank values with NA
remove_blank <- function(blank_val){
  new_blank <- blank_val[which(blank_val == "")] <- NA
  return(new_blank)
}
#For Question and Answer
print_QnA <- function(q , a) {
  print(paste(q,a , sep = " :- "),quote = FALSE)
}

#year
year <- function(date_year){
  date_year <- as.Date(date_year,format = "%d-%m-%Y")
  year <- format(date_year,"%Y")
  return(year)
}

#day
day <- function(date_day){
  date_day <- as.Date(date_day,format = "%d-%m-%Y")
  day <- format(date_day,"%d")
  return(day)
}

#month
month <- function(date_month){
  date_month <- as.Date(date_month,format = "%d-%m-%Y")
  month <- format(date_month,"%m")
  return(month)
}


#=================== Data prepration and driving new variables for analysis=============

#Date and Time in same format
# Using lubridate package
# Lubridate provides tools that make it easier to parse and manipulate dates.
# Lubridate's parsing functions read strings into R as POSIXct date-time objects.

uber$req_dt <-  parse_date_time(uber$Request.timestamp, c("%d/%m/%Y %H:%M", "%d-%m-%Y %H:%M:%S"))
uber$drop_dt <-  parse_date_time(uber$Drop.timestamp, c("%d/%m/%Y %H:%M", "%d-%m-%Y %H:%M:%S"))
str(uber)

#Adding a column for weekdays
uber$day <- weekdays(uber$req_dt)

# Converting date and time to POSTXlt to extract parameters individually
uber$req_dt <- as.POSIXlt(uber$req_dt, format = "%d-%m-%Y %H:%M:%S")
uber$drop_dt <- as.POSIXlt(uber$drop_dt, format = "%d-%m-%Y %H:%M:%S")
str(uber)

#Extracting day, month and year
unique(year(uber$req_dt))
unique(day(uber$req_dt))
unique(month(uber$req_dt))

#We have saw that the data is for july 2016 only
#So, we take Date and hours(only for tome slots prepration) only into the new columns
uber$req_date = format(uber$req_dt, "%d")
uber$req_time = format(uber$req_dt,"%H")
uber$drop_date = format(uber$drop_dt, "%d")
uber$drop_time = format(uber$drop_dt,"%H")

str(uber)

# Convert request date and time columns to numeric class from charecter to perform operation.
uber$req_date <- as.numeric(uber$req_date)
uber$req_time <- as.numeric(uber$req_time)
uber$drop_date <- as.numeric(uber$drop_date)
uber$drop_time <- as.numeric(uber$drop_time)
str(uber)

# ==============Time Slotes========

hist(uber$req_time,col = "black",border = "red",main="No.of requests",xlab = "Request time",ylab = "Ferquency")

#Make Time slots
uber$req_time_slot[uber$req_time >=0 & uber$req_time <=2] <- "Midnight"
uber$req_time_slot[uber$req_time >=3 & uber$req_time <=6] <- "Early morning"
uber$req_time_slot[uber$req_time >=6 & uber$req_time <=11] <- "Morning"
uber$req_time_slot[uber$req_time >=12 & uber$req_time <=16] <- "Noon"
uber$req_time_slot[uber$req_time >=17 & uber$req_time <=19] <- "Evening"
uber$req_time_slot[uber$req_time >=20 & uber$req_time <=23] <- "Night"


#============== Data Cleaning ===========

#Type of data
class(uber)
summary(uber)

#Removing unwanted columns
uber$Request.timestamp <- NULL
uber$Drop.timestamp <- NULL

#Checking NA values in uber dataset
summary(is.na(uber))

#How many unique Driver_id in data set
unique_driver_id <-  n_distinct(uber$Driver.id , na.rm = TRUE)
print_QnA("How many unique driver's id are present in dataet? " , unique_driver_id)

#Pickup points
unique(uber$Pickup.point)  
unique(is.na(uber$Pickup.point))

# Trip Status
unique(uber$Status)  
unique(is.na(uber$Status))

#Replacing Blank values with NA
unique(is.na(uber$Driver.id))
remove_blank(uber$Driver.id)
unique(is.na(uber$Request.id))



#========Visually identify the most pressing problems for Uber=========

#Analysis on most of the request made from which pickup point
#Analysis on request made by customer for cab in different time slots from both the pick up points.

request_count <- ggplot(uber,aes(x=factor(req_time_slot),fill=factor(Pickup.point)))
request_count+geom_bar(stat='count',position = "dodge")+
  ggtitle("Time slots of requests")+
  labs(x="time slot", y="Number of Requests",fill="Pickup Point")


# Which Time slot have maximum request
count_req <- c(length(which(uber$req_time_slot=="Early morning")),
               length(which(uber$req_time_slot=="Morning")),
               length(which(uber$req_time_slot=="Noon")),
               length(which(uber$req_time_slot=="Evening")),
               length(which(uber$req_time_slot=="Night")),
               length(which(uber$req_time_slot=="Midnight")))
slots <- c("Early morning","Morning","Noon","Evening","Night","Midnight")

max_req <- data.frame(slots,count_req)
max_req
##Morning time got maximum requests.

#========Analysis on trips from city============
city <- subset(uber,uber$Pickup.point=="City")

#Plot on trips from city which are completed
#SUBSET
city_Complete <- subset(city,city$Status == "Trip Completed")

#PLOT
ggplot(city_Complete,aes(x=factor(req_time_slot),fill=factor(day)))+
  geom_bar(position = "dodge",col = "blue")+
  ggtitle("Trips completed during different time slots and days where pick up point is City")+
  labs(x="Time Slots",y="Trips Completed")

##Morning time Uber got the maximum requests.
##maximum trips completed on the thrusday.
## The reason behind this may be it is the time when outgoing flights are maximum fron the city.

#plot on trips from city which are get cancelled
#SUBSET
city_cancelled <- subset(city,city$Status=="Cancelled")

#PLOT
ggplot(city_cancelled,aes(x=factor(req_time_slot),fill = factor(day)))+
  geom_bar(position = "dodge",col = "green")+
  ggtitle("Trips cancelled during different time slots and days where pick up point is City")+
  labs(x="Time Slots",y="Trips cancelled")

##maximum cancelled trips are from wednesday morning. 

#Plot on trips from city which shows 'no car available'
#SUBSET
city_NCA <- subset(uber,uber$Pickup.point=="City" & uber$Status=="No Cars Available")

#PLOT
ggplot(city_NCA,aes(x=factor(req_time_slot),fill = factor(day)))+
  geom_bar(position = "dodge", col = "black")+
  ggtitle("No cars available during different time slots and days where pick up point is City")+
  labs(x= "Time Slots",y="No car available")
##Maximum request which shows "No cars available" are from thrusday morning.

#===================Analysis on trips from Airport===========
#SUBSET
airport <- subset(uber,uber$Pickup.point=="Airport")

#Plot on trips from Airport which are completed
#SUBSET
airport_Complete <- subset(airport,airport$Status == "Trip Completed")

#PLOT
ggplot(airport_Complete,aes(x=factor(req_time_slot),fill=factor(day)))+
  geom_bar(position = "dodge",col = "blue")+
  ggtitle("Trips completed during different time slots and days where pick up point is airport")+
  labs(x="Time Slots",y="Trips Completed")

##Maximum trips completed when the pickup point is Airport in the tuesday morning.

#Plot on trips from Airport which are cancelled
#SUBSET
airport_cancelled <- subset(airport,airport$Status == "Cancelled")

#PLOT
ggplot(airport_cancelled,aes(x=factor(req_time_slot),fill=factor(day)))+
  geom_bar(position = "dodge",col = "blue")+
  ggtitle("Trips cancelled during different time slots and days where pick up point is airport")+
  labs(x="Time Slots",y="Trips Completed")

## maximum trips cancelled in the wedneday night 

#Plot on trips from Airport which shows No car available
#SUBSET
airport_NCA <- subset(airport,airport$Status == "No Cars Available")

#PLOT
ggplot(airport_NCA,aes(x=factor(req_time_slot),fill=factor(day)))+
  geom_bar(position = "dodge",col = "blue")+
  ggtitle("No cars available during different time slots and days where pick up point is airport")+
  labs(x="Time Slots",y="Trips Completed")

##Thrusday morning shows maximum "No cars available".

#==========Analysis on Demand and supply=============

#Demand :- Number of request made by customers
#Supply :- Number of trips completed
#Demand and supply gap :- Total number of trips that is cancelled and shows no cars available
#Demand and supply gap = number of request - trip completed

# Demand and supply of the trips from city
length(city$Request.id)
length(city_Complete$Request.id)
city_DS_gap <-length(city$Request.id)-length(city_Complete$Request.id)
city_DS_gap

# Demand and supply of the trips from Airport
length(airport$Request.id)
length(airport_Complete$Request.id)
airport_DS_gap <-length(airport$Request.id)-length(airport_Complete$Request.id)
airport_DS_gap

# Maximum demand and supply gap is on the trips whose pickup point is "CITY"
# Demand and supply gap is in the early morning, morning and midnight time slots is higher than 
# the gap in noon and evening.

#=========End of the script===================






