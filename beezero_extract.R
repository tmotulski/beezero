require(jsonlite)

setwd("c:/Projects/Carsharing/datasets/beezero-muc")

files <- list.files(pattern = ".json")

all_vehicles_df <- data.frame(
  filename = character(0),
  id = numeric(0),
  name = character(0),
  lat = numeric(0),
  lng = numeric(0),
  fuelLevel = numeric(0),
  lastAddr = character(0)
)

write.table(all_vehicles_df, "all_vehicles_df.csv", col.names=TRUE, row.names = FALSE, sep=",")

for (fileCount in seq_along(files)) {
#for (fileCount in 2371:28962) {
 
  filename <- files[fileCount]
  
  #print progress
  print(paste(fileCount," -- ",filename))
  
  json_data <- fromJSON(filename)
  
  available_vehicles_df <- json_data$response$availableVehicles
  
  if (length(available_vehicles_df) > 0) {
  vehicles_df <- data.frame(
    #datetime = strptime(substr(filename, 1, 18), "%Y-%m-%dT%H_%M_%S", tz="Europe/Berlin"),
    filename = filename,
    id = available_vehicles_df$id,
    name = available_vehicles_df$name,
    lat = available_vehicles_df$coordinate$latitude,
    lng = available_vehicles_df$coordinate$longitude,
    fuelLevel = available_vehicles_df$fuelLevel,
    lastAddr = iconv(available_vehicles_df$lastAddress, from = "UTF-8", to = "latin1")
  )

  write.table(vehicles_df, "all_vehicles_df.csv", col.names=FALSE, row.names = FALSE, sep=",",append = TRUE)
  }
}

all_vehicles_df <- read.csv("all_vehicles_df.csv",header=TRUE,row.names=NULL)
all_vehicles_df <- cbind(all_vehicles_df, datetime = NA)
all_vehicles_df$datetime <- strptime(substr(all_vehicles_df$filename, 1, 18), "%Y-%m-%dT%H_%M_%S")

all_vehicles_df <- unique(all_vehicles_df)
all_vehicles_df <- na.omit(all_vehicles_df)

vehicles_group_df <- unique(data.frame(car_id = all_vehicles_df$id, car_name = all_vehicles_df$name))

carGroupCount <- nrow(vehicles_group_df)

full_interval <- data.frame()
full_route <- data.frame()
full_route2 <- data.frame()

for (ii in 1:carGroupCount) {
  
  print(paste("Car #", ii, " of ", carGroupCount))
  
  car_data <- data.frame()
  car_data <- all_vehicles_df[all_vehicles_df$id == vehicles_group_df$car_id[ii],]
  car_data <- car_data[order(car_data$datetime),]
  
  car_interval <- data.frame()
  
  carRCount <- nrow(car_data)
  
  #loop cars
  for (i in 1:carRCount) {
    
    if (i == 1) {
      #first row
      car_interval <- data.frame(
        start_datetime = car_data$datetime[i],
        end_datetime = as.POSIXct(NA),
        car_name = car_data$name[i],
        car_id = car_data$id[i],
        fuel_level = car_data$fuelLevel[i],
        route = car_data$lastAddr[i],
        latitude = car_data$lat[i],
        longitude = car_data$lng[i],
        activity = "Parking" )
      
    } else if (i == carRCount) {
      #last row to close interval
      car_interval[nrow(car_interval), 2] <- as.POSIXct(car_data$datetime[i])
      
    } else  {
      
      #most cases
      
      #different places
      if (car_data$lat[i] != car_data$lat[i+1] & 
          car_data$lng[i] != car_data$lng[i+1]) {
        
        #close current period of parking        
        car_interval[nrow(car_interval), 2] <- as.POSIXct(car_data$datetime[i])
        
        #add record about car`s relocation
        car_interval <- rbind(car_interval, data.frame(
          start_datetime = car_data$datetime[i],
          end_datetime = car_data$datetime[i+1],
          car_name = car_data$name[i],
          car_id = car_data$id[i],
          fuel_level = car_data$fuelLevel[i],
          route = paste(car_data$lastAddr[i]," -> ",car_data$lastAddr[i+1]),
          latitude = car_data$lat[i],
          longitude = car_data$lng[i],
          activity = "Moving" ))
        
        #add record about new parking location
        car_interval <- rbind(car_interval, data.frame(
          start_datetime = car_data$datetime[i+1],
          end_datetime = as.POSIXct(NA),
          car_name = car_data$name[i+1],
          car_id = car_data$id[i+1],
          fuel_level = car_data$fuelLevel[i+1],
          route = car_data$lastAddr[i+1],
          latitude = car_data$lat[i+1],
          longitude = car_data$lng[i+1],
          activity = "Parking" ))
        
      } else {
        
        #same place
        #close current`s car period
        car_interval[nrow(car_interval), 2] <- as.POSIXct(car_data$datetime[i])
      }  
    }
  }
  
  full_interval <- rbind (full_interval, car_interval)
  
}

#full_route2 <- cbind(full_route2, period_min = difftime(full_route2$end_datetime, full_route2$start_datetime,units="mins"))
full_interval <- cbind(full_interval, period_min = difftime(full_interval$end_datetime, full_interval$start_datetime,units="mins"))
full_route <- full_interval[full_interval$activity == 'Moving',]

write.csv(full_interval, file = "full_interval.csv")


test_car_number = 92274
test_df <- all_vehicles_df[all_vehicles_df$car_number == test_car_number,]
test_interval <- full_interval[full_interval$car_number == test_car_number,]
test_route <- full_route[full_route$car_number == test_car_number,]

##############################################3
# Calculate possible income
##############################################

total_df <- data.frame()
total_df <- full_route[full_route$period_min <= 24*60,]

total_df <- cbind(total_df, trip_cost = 0)
total_df$trip_cost <- ifelse(total_df$period_min<=30, 1, 9) # 30 min = 1 euro, 24h = 9 euro

total_income_sum <- sum(total_df$trip_cost, na.rm = TRUE)
total_car_count <- length(unique(full_route$car_number))
total_start_date <- range(full_route$start_datetime,na.rm=TRUE)
total_end_date <- range(full_route$end_datetime,na.rm=TRUE)
mean(full_route$period_min)
total_income_by_car <- total_income_sum / total_car_count

summary(total_df)


