setwd("c:/Projects/Carsharing/datasets/beezero-muc")

#Load data
all_vehicles_df <- read.csv("all_vehicles_df.csv",header=TRUE,row.names=NULL)
all_vehicles_df <- cbind(all_vehicles_df, datetime = NA)
all_vehicles_df$datetime <- strptime(substr(all_vehicles_df$filename, 1, 18), "%Y-%m-%dT%H_%M_%S")

all_vehicles_df <- unique(all_vehicles_df)
all_vehicles_df <- na.omit(all_vehicles_df)

vehicles_group_df <- unique(data.frame(car_id = all_vehicles_df$id, car_name = all_vehicles_df$name))

all_car_interval <- data.frame()
all_rent_cases <- data.frame()

carGroupCount <- nrow(vehicles_group_df)

for (ii in 1:carGroupCount) {
  
  print(paste("Car #", ii, " of ", carGroupCount))
  
  car_data <- data.frame()
  car_data <- all_vehicles_df[all_vehicles_df$id == vehicles_group_df$car_id[ii],]
  car_data <- car_data[order(car_data$datetime),]
  
  car_interval <- data.frame(
    carID = car_data$id,
    carName = car_data$name,
    start_datetime = car_data$datetime,
    end_datetime = c(car_data$datetime[-1], NA),
    start_fuel = car_data$fuelLevel,
    end_fuel = c(car_data$fuelLevel[-1], NA)
  )

  car_interval$rent_time_min <- as.numeric( difftime(car_interval$end_datetime, car_interval$start_datetime,units="mins") )  
  car_interval$rent_fuel_loss <- round(car_interval$start_fuel - car_interval$end_fuel, digits = 3)
  
  all_car_interval <- rbind(all_car_interval, car_interval)  

  rent_cases <- car_interval[(car_interval$rent_time_min > 5 & car_interval$rent_fuel_loss > 0), ]
  all_rent_cases <- rbind(all_rent_cases, rent_cases) 
}

write.csv2(all_car_interval, file = "all_car_interval.csv", na = "NA", row.names = FALSE)

all_rent_cases <- na.omit(all_rent_cases)
all_rent_cases <- unique(all_rent_cases)

#Analize rent case

#calc possible rent distanse
cost1perFuel <- 4 # 4 euro per 1 percent fuel
all_rent_cases$calc_dist <- all_rent_cases$rent_fuel_loss*100*cost1perFuel


#calc paid & free time rent
all_rent_cases <- cbind(all_rent_cases, paidTime_min = 0)
all_rent_cases <- cbind(all_rent_cases, freeTime_min = 0)

#calc night tariff
require(lubridate)

for (i in 1: nrow(all_rent_cases)) {
  print(i)
  MyDatesTable <- table(cut(c(all_rent_cases$start_datetime[i],all_rent_cases$end_datetime[i]), breaks="min"))
  MyDatesTable <- data.frame(MyDatesTable)
  MyDatesTable$isPaid <- 0

  free1h <- ifelse(nrow(MyDatesTable) > 60, 60, nrow(MyDatesTable))
  MyDatesTable[!(hour( MyDatesTable$Var1 ) >= 00 & hour( MyDatesTable$Var1 ) < 06), c("isPaid")] <- 1
  MyDatesTable[(hour( MyDatesTable$Var1 ) >= 00 & hour( MyDatesTable$Var1 ) < 06), c("isPaid")] <- 0
  MyDatesTable[1:free1h, c("isPaid")] <- 0   
  
  paidTime <- nrow(MyDatesTable[MyDatesTable$isPaid == 1 , ] )
  freeTime <- nrow(MyDatesTable[MyDatesTable$isPaid == 0 , ] )
  
  all_rent_cases$paidTime_min[i] <- paidTime
  all_rent_cases$freeTime_min[i] <- freeTime
}

#calc summary rent cost
cost1hour <- 5.5 # 5.5 euro per 1 hour rent
cost1km <- 0.29 # 0.29 euro per 1 km trip
cost1min <- cost1hour / 60

#calc summ rent case = trip time + distance
all_rent_cases$rentCost <- all_rent_cases$paidTime_min*cost1min + all_rent_cases$calc_dist*cost1km
incomeAllCars <- sum(all_rent_cases$rentCost)
incomeAllCars

summary(all_rent_cases)

write.csv2(all_rent_cases, file = "all_rent_cases.csv", na = "NA", row.names = FALSE)
