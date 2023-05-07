# Load the necessary packages
library(dplyr)
#library(ggmap)

# We have to register our service for Google maps with an API key
#register_google(
#  'AIzaSyCx6V0sc626jiStBX-jXfK_l5QcV8tVim0',
#)

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

# This field will help us see the waiting time till the appointment for better insights 
dataset$DaysWaited <- as.numeric(as.Date(dataset$AppointmentDay) -
                                   as.Date(dataset$ScheduledDayTemp))

dataset$ScheduledDayTemp <- NULL # Lets delete this now that we do not require it

# Since this column has time in addition to the date we shall convert the data into 
# the POSIXct datatype

dataset$ScheduledDay <- as.POSIXct(dataset$ScheduledDay, format = "%Y-%m-%dT%H:%M:%SZ")

# Removing ages less than 0 as they are data entry errors
dataset$Age <- ifelse(dataset$Age < 0, 0, dataset$Age)

# This is another important insight that was gathered by the literature that indicates the
# number of cancellations that patient has made up-to the appointment in the row
dataset <- dataset %>% 
  group_by(PatientId) %>%  
  mutate(NoOfAppointmentsCancelled = sum(Cancelled == "Yes")) %>% 
  ungroup()

dataset$Cancelled<- ifelse(dataset$Cancelled == "Yes", TRUE, FALSE)
dataset$Scholarship<- ifelse(dataset$Scholarship == 1, TRUE, FALSE)
dataset$Hypertension<- ifelse(dataset$Hypertension == 1, TRUE, FALSE)
dataset$Diabetes<- ifelse(dataset$Diabetes == 1, TRUE, FALSE)
dataset$Alcoholism<- ifelse(dataset$Alcoholism == 1, TRUE, FALSE)
dataset$SMSReceived<- ifelse(dataset$SMSReceived == 1, TRUE, FALSE)

write.csv(dataset, "../data/processed/final.csv", row.names=FALSE)

# 
# neighborhoods <- unique(dataset$Neighbourhood)
# 
# # Create a dictionary (list) to store latitude and longitude
# coords_dict <- list()
# 
# # Loop through unique values and obtain latitude and longitude
# for (neighborhood in neighborhoods) {
#   address <- unique(dataset$Neighbourhood[dataset$Neighbourhood == neighborhood])
#   coords <- geocode(address)
#   coords_dict[[neighborhood]] <- list(lat = coords$lat, lon = coords$lon)
# }
# 
# coords_dict[[neighborhood]] <- list(lat = coords$lat, lon = coords$lon)
# 
# # Iterate through coords_dict using a for loop
# for (address in names(coords_dict)) {
#   lat <- coords_dict[[address]]$lat
#   lon <- coords_dict[[address]]$lon
# 
#   if (is.na(lat)) {
#     cat("Address:", address, "Lat is null\n")
#   }
# }
# 
# addresses <- c("REPÚBLICA", "ANDORINHAS", "DA PENHA", "SÃO BENEDITO", "MARIA ORTIZ",
#                "RESISTÊNCIA", "SANTO ANTÔNIO", "SANTA HELENA", "CENTRO", "DO MOSCOSO",
#                "SANTOS DUMONT", "JOANA D´ARC", "ESTRELINHA", "SANTOS REIS", "JESUS DE NAZARETH",
#                "CRUZAMENTO", "SANTA CECÍLIA", "DO QUADRO", "DO CABRAL", "HORTO", 
#                "AEROPORTO", "PARQUE INDUSTRIAL")
# 
# null_cities_lat <- c(-23.54409, -10.34500001, -46.54378, -4.04491, -19.550,
#                      -27.46056)
# null_cities_lon <- c(-46.64266, 39.8327777878, -23.52368, -40.86514, -40.467,
#                      -58.98389)
# 
#  
# by(dataset, seq_len(nrow(dataset)), function(row) print(row$Neighbourhood)
# 
# dataset <- dataset %>%
#   mutate(Latitude = coords_dict[[dataset$Neighbourhood]]$lat,
#          Longtitude = coords_dict[[dataset$Neighbourhood]]$lon)
