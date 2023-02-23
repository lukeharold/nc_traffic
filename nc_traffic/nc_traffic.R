## traffic collisions in North County coastal (Del Mar, Solana Beach and Encinitas) from 2018 to 2022

library("tidyverse")
library("lubridate")

## master list of all collisions
nc_traffic <- read_csv("~/documents/U-T/nc_traffic/nc_traffic - Sheet1.csv")
nc_traffic %>% view()

##formatting the date
nc_traffic$date <- mdy(nc_traffic$date)
nc_traffic %>% view()
yq(nc_traffic$date)

##adding a column to get just the year
nc_traffic$year <- year(nc_traffic$date)
nc_traffic %>%
  view()

## eliminating duplicate incident numbers to get only the location for mapping purposes
nc_traffic22 <- distinct(nc_traffic, incident_no, .keep_all = TRUE) %>%
  view()

##how many collisions were there in each year?
table(nc_traffic22$year)
##total number of collisions:
421+395+286+389+353
nrow(nc_bike_collisions) / nrow(nc_traffic22)

# total traffic deaths and injuries
sum(nc_traffic22$injured)
sum(nc_traffic22$killed)

write.csv(nc_traffic22, "~/Documents/U-T/nc_traffic/nc_traffic_locationonly.csv", row.names=FALSE)

## different types 
table(nc_traffic$vehicle_type)
table(nc_traffic$role)


## BICYCLES

## separating out all bicycle collisions

##filtering based on bicycle as vehicle type
nc_bicycles1 <- nc_traffic[nc_traffic$vehicle_type == "BICYCLE",]
nrow(nc_bicycles1)

## filtering based on bicycle as role
nc_bicycles2 <- nc_traffic[nc_traffic$role == "BICYCLIST",]
nrow(nc_bicycles2)

## combining the two
nc_bicycles_master <- rbind(nc_bicycles1,nc_bicycles2) %>%
  view()

## eliminating duplicate incident numbers to get total number of bicycle collisions
nc_bike_collisions <- distinct(nc_bicycles_master, incident_no, .keep_all = TRUE)
nc_bike_collisions %>% view()

write.csv(nc_bike_collisions, "~/Documents/U-T/nc_traffic/nc_bike_collisions(13).csv", row.names=FALSE)

## bicycle collisions by year
table(nc_bike_collisions$year)
(59-40)/(40)

## percent increase when comparing 2022 to 2018
(59-40)/(40)

## total killed and injured in bicycle collisions
sum(nc_bike_collisions$killed, na.rm = TRUE)
sum(nc_bike_collisions$injured, na.rm = TRUE)




nc_bicycles_master_final <- distinct(nc_bicycles_master, incident_no, .keep_all = TRUE)
nc_bicycles_master %>% view()

table(nc_bicycles_master_final$year)

write.csv(nc_bicycles_master_final, "~/Documents/U-T/nc_traffic/nc_bicycles_master(1).csv", row.names=FALSE)


## bicycle collisions in Encinitas

enc1 <- nc_bike_collisions[nc_bike_collisions$city == "ENCINITAS",]
enc2 <- nc_bike_collisions[nc_bike_collisions$city == "CARDIFF BY THE SEA",]

enc_bike_collisions <- rbind(enc1,enc2)
enc_bike_collisions %>% view()

table(enc_bike_collisions$year)
table(enc2$year)

enc2021 <- enc_bike_accidents[enc_bike_accidents$year == 2021,]

sum(enc2021$injured, na.rm=TRUE)


table(nc_bike_collisions$injured)
198+15+4
198+15+1
