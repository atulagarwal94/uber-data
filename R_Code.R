
library(dplyr)
library(tidyr)
library(ggplot2)
par(mfrow=c(1, 2))
datasets <- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE,header = TRUE)


date_clean <- function(param) {

  param$Drop.timestamp <-  strptime(gsub("/","-",param$Drop.timestamp), format ="%d-%m-%Y %H:%M")
  param$Request.timestamp <-  strptime(gsub("/","-",param$Request.timestamp), format ="%d-%m-%Y %H:%M")
  
  param <- param %>% separate(Request.timestamp,c("Request.d","Request.t"), " ")
  param <- param %>% separate(Drop.timestamp,c("Drop.d","Drop.t"), " ")
  
  param$Drop.d <-  as.Date(param$Drop.d, format ="%Y-%m-%d")
  param$Request.d <-  as.Date(param$Request.d, format ="%Y-%m-%d")
  
  param$Request.hour = as.factor(format(as.POSIXct(param$Request.t,format="%H:%M:%S"),"%H"))
  param$Drop.hour = format(as.POSIXct(param$Drop.t,format="%H:%M:%S"),"%H")
  
  return(param)
    
}

datasets_clean <- date_clean(datasets)
datasets_sub <-datasets_clean %>% filter(datasets_clean$Status != "Trip Completed")

#Problematic Types of Requests Plot
ggplot(datasets_sub , aes(x=Status , fill = Pickup.point)) + geom_bar(position = "dodge")


## SEGMENT ANALYSIS WHEN STATUS IS CANCELLED FOR TIME SLOT
#cancelled-----------------------------

is_cancelled <- datasets_clean %>% filter(datasets_clean$Status == 'Cancelled')

ggplot(is_cancelled , aes(x=Request.hour  ,fill = Pickup.point )) + geom_bar(position = "dodge",width = 0.5)

## SEGMENT ANALYSIS WHEN STATUS IS NO CARS AVAILABLE  FOR TIME SLOT
#no cars available-------------------

no_cars_available = datasets_clean %>% filter(datasets_clean$Status == 'No Cars Available')

ggplot(no_cars_available , aes(x=Request.hour  ,fill = Pickup.point )) + geom_bar(position = "dodge",width = 0.5)


###2
##Supply Demand

#Demand Supply Graph

#FILTERED ACC TO PICKUP POINT CITY
demand_supply_subset_city <- datasets_clean %>% filter(datasets_clean$Pickup.point == "City")
ggplot(demand_supply_subset_city , aes(x=Request.hour , fill = Status , width = 1)) + geom_bar(position = "dodge",width = 0.5) + labs(title = "Demand Graph" , x = "Request Hour" , y = "Frequency/Count")+
  theme(plot.title = element_text(hjust = 0.5))

#FILTERED ACC TO PICKUP POINT AIRPORT
demand_supply_subset_airport <- datasets_clean %>% filter(datasets_clean$Pickup.point == "Airport")
ggplot(demand_supply_subset_airport , aes(x=Request.hour , fill = Status , width = 1)) + geom_bar(position = "dodge",width = 0.5) + labs(title = "Demand Graph" , x = "Request Hour" , y = "Frequency/Count")+
  theme(plot.title = element_text(hjust = 0.5))

