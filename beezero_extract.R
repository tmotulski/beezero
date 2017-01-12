require(jsonlite)

setwd("c:/Projects/Carsharing/beezero/samples")

files <- list.files(pattern = ".json")

vehicles_df <- data.frame()

for (fileCount in seq_along(files)) {
 
  filename <- files[fileCount]
  
  json_data <- fromJSON(filename)
  names(json_data)
  
  available_vehicles_df <- json_data$response$availableVehicles
  
  vehicles_df <- rbind(vehicles_df, data.frame(
    filename = strptime(substr(filename, 1, 18), "%Y-%m-%dT%H_%M_%S", tz="Europe/Berlin"),
    id = available_vehicles_df$id,
    name = available_vehicles_df$name,
    lat = available_vehicles_df$coordinate.latitude,
    lng = available_vehicles_df$coordinate.longitude,
    fuelLevel = available_vehicles_df$fuelLevel,
    lastAddr = available_vehicles_df$lastAddress
  ))
    
}

write.table(vehicles_df, "vehicles_df.csv", col.names=TRUE, row.names = FALSE, sep=",")

