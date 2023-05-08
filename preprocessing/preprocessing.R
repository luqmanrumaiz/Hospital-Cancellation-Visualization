# Load the necessary packages
library(dplyr)

# Read the data frame
dataset <- read.csv("../data/raw/noshow_hosptial_appointment.csv")
print(dataset)

# Renaming columns to fix inconsistencies and misspellings
colnames(dataset)[which(colnames(dataset) == "Hipertension")] <- "Hypertension"
colnames(dataset)[which(colnames(dataset) == "Handcap")] <- "Handicapped"
colnames(dataset)[which(colnames(dataset) == "SMS_received")] <- "SMSReceived"
colnames(dataset)[which(colnames(dataset) == "No.show")] <- "Cancelled"

dataset$ScheduledDayTemp <- dataset$ScheduledDay

# Convert the date columns to datetime
dataset$AppointmentDay <- as.Date(dataset$AppointmentDay)
dataset$ScheduledDayTemp <- as.Date(dataset$ScheduledDayTemp)

print(unique(format(dataset$ScheduledDayTemp, "%Y")))

# This field will help us see the waiting time till the appointment for better insights 
dataset$DaysWaited <- as.numeric(as.Date(dataset$AppointmentDay) -
                                   as.Date(dataset$ScheduledDayTemp))

dataset$ScheduledDayTemp <- NULL # Lets delete this now that we do not require it

# Since this column has time in addition to the date we shall convert the data into 
# the POSIXct datatype

dataset$ScheduledDay <- as.POSIXct(dataset$ScheduledDay, format = "%Y-%m-%dT%H:%M:%SZ")

# Creating Column for the day of week that the appointment was scheduled to 

dataset$DayOfWeek <- weekdays(as.Date(dataset$ScheduledDay))

# Removing ages less than 0 as they are data entry errors
dataset$Age <- ifelse(dataset$Age < 0, 0, dataset$Age)

# Define the age bin edges
age_bins <- c(0, 2, 16, 30, 45, max(dataset$Age))
# Define the age group labels
age_labels <- c('Baby', 'Children', 'YoungAdult', 'MiddleAgedAdult', 'OldAdult')

dataset$AgeGroup <- cut(dataset$Age, age_bins, labels = age_labels)

# This is another important insight that was gathered by the literature that indicates the
# number of cancellations that patient has made up-to the appointment in the row
dataset <- dataset %>% 
  group_by(PatientId) %>%  
  mutate(NoOfAppointmentsCancelled = sum(Cancelled == "Yes")) %>% 
  ungroup()

dataset$Cancelled<- ifelse(dataset$Cancelled == "Yes", TRUE, FALSE)
dataset$Scholarship<- ifelse(dataset$Scholarship == 1, TRUE, FALSE)
dataset$Hypertension<- ifelse(dataset$Hypertension == 1, TRUE, FALSE)
dataset$Handicapped <- ifelse(dataset$Handicapped > 0, TRUE, FALSE)
dataset$Diabetes<- ifelse(dataset$Diabetes == 1, TRUE, FALSE)
dataset$Alcoholism<- ifelse(dataset$Alcoholism == 1, TRUE, FALSE)
dataset$SMSReceived<- ifelse(dataset$SMSReceived == 1, TRUE, FALSE)

write.csv(dataset, "../data/processed/final.csv", row.names=FALSE)