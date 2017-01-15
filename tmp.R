all_vehicles_df <- read.csv("all_vehicles_df.csv",header=TRUE,row.names=NULL)
all_vehicles_df <- cbind(all_vehicles_df, datetime = NA)
all_vehicles_df$datetime <- strptime(substr(all_vehicles_df$filename, 1, 18), "%Y-%m-%dT%H_%M_%S")

all_vehicles_df <- unique(all_vehicles_df)
all_vehicles_df <- na.omit(all_vehicles_df)

vehicles_group_df <- unique(data.frame(car_id = all_vehicles_df$id, car_name = all_vehicles_df$name))

all_car_interval <- data.frame()
all_rent_interval <- data.frame()

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

  car_interval$trip_time <- difftime(car_interval$end_datetime, car_interval$start_datetime,units="mins")  
  car_interval$trip_fuel <- round(car_interval$start_fuel - car_interval$end_fuel, digits = 2)
  
  all_car_interval <- rbind(all_car_interval, car_interval)  
  #write.csv2(car_interval, file = "all_car_interval.csv", na = "NA", row.names = FALSE, append = TRUE)

  rent_interval <- car_interval[(car_interval$trip_time > 5 & car_interval$trip_fuel > 0), ]
  all_rent_interval <- rbind(all_rent_interval, rent_interval) 
}

all_rent_interval$trip_hours <- ceiling(all_rent_interval$trip_time/60)



all_rent_interval <- na.omit(all_rent_interval)
all_rent_interval <- unique(all_rent_interval)


all_rent_interval$coef <- (all_rent_interval$trip_fuel*100) / as.numeric(all_rent_interval$trip_time)

truth_rent <- all_rent_interval[all_rent_interval$coef <0.3, ]
  
write.table(all_car_interval, "all_car_interval.csv", col.names=TRUE, row.names = FALSE, sep=",")
write.table(all_rent_interval, "all_rent_interval.csv", col.names=TRUE, row.names = FALSE, sep=",")


cost1perFuel <- 4

all_rent_interval$calc_dist <- all_rent_interval$trip_fuel*100*cost1perFuel

all_rent_interval <- cbind(all_rent_interval, paidTime = 0)
all_rent_interval <- cbind(all_rent_interval, nightTime = 0)

#calc night tariff
require(lubridate)

for (i in 1: nrow(all_rent_interval)) {
  print(i)
  MyDatesTable <- table(cut(c(all_rent_interval$start_datetime[i],all_rent_interval$end_datetime[i]), breaks="min"))
  MyDatesTable <- data.frame(MyDatesTable)
  MyDatesTable$isPaid <- 0

  free1h <- ifelse(nrow(MyDatesTable) > 60, 60, nrow(MyDatesTable))
  MyDatesTable[!(hour( MyDatesTable$Var1 ) >= 00 & hour( MyDatesTable$Var1 ) < 06), c("isPaid")] <- 1
  MyDatesTable[(hour( MyDatesTable$Var1 ) >= 00 & hour( MyDatesTable$Var1 ) < 06), c("isPaid")] <- 0
  MyDatesTable[1:free1h, c("isPaid")] <- 0   
  
  paidTime <- nrow(MyDatesTable[MyDatesTable$isPaid == 1 , ] )
  freeTime <- nrow(MyDatesTable[MyDatesTable$isPaid == 0 , ] )
  
  all_rent_interval$paidTime[i] <- paidTime
  all_rent_interval$nightTime[i] <- freeTime
}

cost1hour <- 5.5
cost1km <- 0.29
cost1min <- cost1hour / 60

costPaidTime <- round(paidTime * cost1min,2)

all_rent_interval$rentCost <- all_rent_interval$paidTime*cost1min + all_rent_interval$calc_dist*cost1km

sum(all_rent_interval$rentCost)

rent_period <- as.interval(starttime, endtime)
nighttime<- as.interval(as.POSIXct("2017-01-05 00:00:00"), as.POSIXct("2017-01-05 06:00:00"))

int_overlaps(rent_period, nighttime)


require( lubridate )




library(xts)

data.xts <- as.xts(MyDatesTable$Freq, as.POSIXct(MyDatesTable$Var1))
data.xts.filterd <- data.xts["T00:00/T06:00"]

data.xts.filtered <- data.xts[data.xts %in% !data.xts["T00:00/T06:00"]]

MyDatesTable$nightTariff <- int_overlaps(MyDatesTable$Var1, nighttime)

nighttime <- as.interval("24:00:00", "06:00:00")

in.bed



library(ggplot2) 


base <- ggplot(all_rent_interval, 
               aes(x=trip_time,
                   y=trip_fuel))+
  geom_point()

base + 
  scale_x_continuous(limits = c(0, 10000))




period_car <- unique(all_car_interval[all_car_interval$carName %in% c("Benedikt") & (all_car_interval$start_datetime >= '2016-12-01' & all_car_interval$start_datetime <= '2016-12-05'),])

base <- ggplot(subset(all_car_interval, carName %in% c("Benedikt")),
               aes(x=start_datetime,
                   y=start_fuel))+
  geom_point()

base + 
  scale_x_datetime(date_minor_breaks = "1 day", date_labels = "%b %d")


period_car <- unique(all_car_interval[all_car_interval$carName %in% c("Benedikt") & (all_car_interval$start_datetime >= '2016-12-15' & all_car_interval$start_datetime <= '2016-12-17'),])

base <- ggplot(subset(all_car_interval, (carName %in% c("Benedikt") & (start_datetime >= '2016-12-15' & start_datetime <= '2016-12-17'))),
               aes(x=start_datetime,
                   y=start_fuel))+
  geom_point()

base + 
  scale_x_datetime(date_minor_breaks = "1 hour")




library(ggplot2)
library(ggmap)

# getting the map
mapgilbert <- get_map(location = c(lon = mean(period_car$start_longitude), lat = mean(period_car$start_latitude)), zoom = 16,
                      maptype = "roadmap", scale = "auto")

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = period_car, aes(x = period_car$start_longitude, y = period_car$start_latitude, fill = "red", alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

hist(all_rent_interval$coef)

all_rent_interval <- read.csv("all_rent_interval.csv", header=TRUE,row.names=NULL)

all_rent_interval$start_datetime <- as.POSIXct(all_rent_interval$start_datetime)
all_rent_interval$end_datetime <- as.POSIXct(all_rent_interval$end_datetime)


