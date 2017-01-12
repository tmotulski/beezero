require(jsonlite)

setwd("c:/Projects/Carsharing/datasets/beezero-muc")

files <- list.files(pattern = ".json")

all_vehicles_df <- data.frame(
  datetime = character(0),
  id = numeric(0),
  name = character(0),
  lat = numeric(0),
  lng = numeric(0),
  fuelLevel = numeric(0),
  lastAddr = character(0)
)

write.table(all_vehicles_df, "all_vehicles_df.csv", col.names=TRUE, row.names = FALSE, sep=",")

for (fileCount in seq_along(files)) {
 
  filename <- files[fileCount]
  
  #print progress
  print(paste(fileCount," -- ",filename))
  
  json_data <- fromJSON(filename)
  names(json_data)
  
  available_vehicles_df <- json_data$response$availableVehicles
  
  vehicles_df <- data.frame(
    datetime = strptime(substr(filename, 1, 18), "%Y-%m-%dT%H_%M_%S", tz="Europe/Berlin"),
    id = available_vehicles_df$id,
    name = available_vehicles_df$name,
    lat = available_vehicles_df$coordinate$latitude,
    lng = available_vehicles_df$coordinate$longitude,
    fuelLevel = available_vehicles_df$fuelLevel,
    lastAddr = iconv(available_vehicles_df$lastAddress, from = "UTF-8", to = "latin1")
  )

  all_vehicles_df <- rbind(all_vehicles_df, vehicles_df)
  write.table(all_vehicles_df, "all_vehicles_df.csv", col.names=FALSE, row.names = FALSE, sep=",",append = TRUE)
  
}


