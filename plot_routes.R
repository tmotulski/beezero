library(gmapsdistance)

all_vehicles_df <- read.csv("all_vehicles_df.csv",header=TRUE,row.names=NULL)
all_vehicles_df <- cbind(all_vehicles_df, datetime = NA)
all_vehicles_df$datetime <- strptime(substr(all_vehicles_df$filename, 1, 18), "%Y-%m-%dT%H_%M_%S")

vehicles_group_df <- unique(data.frame(car_id = all_vehicles_df$id, car_name = all_vehicles_df$name))

carGroupCount <- nrow(vehicles_group_df)

full_car <- data.frame()

for (ii in 1:carGroupCount) {
  
  print(paste("Car #", ii, " of ", carGroupCount))
  
  car_data <- data.frame()
  car_data <- all_vehicles_df[all_vehicles_df$id == vehicles_group_df$car_id[ii],]
  car_data <- car_data[order(car_data$datetime),]
  
  car_interval <- data.frame()
  
  carRCount <- nrow(car_data)
  
  #loop cars
  for (i in 1:(carRCount-1)) {
    
      if (car_data$lat[i] != car_data$lat[i+1] & 
          car_data$lng[i] != car_data$lng[i+1]) {
        
        #add record about car`s relocation
        car_interval <- rbind(car_interval, data.frame(
          start_datetime = car_data$datetime[i],
          end_datetime = car_data$datetime[i+1],
          trip_time = difftime(car_data$datetime[i+1], car_data$datetime[i],units="mins"),
          start_latitude = car_data$lat[i],
          start_longitude = car_data$lng[i],
          end_latitude = car_data$lat[i+1],
          end_longitude = car_data$lng[i+1],
          trip_distance = NA,
          fuel_level = car_data$fuelLevel[i],
          car_name = car_data$name[i],
          car_id = car_data$id[i],
          activity = "Moving" ))
        }  
    }
  
  full_car <- rbind (full_car, car_interval)
  
}



# loading the required packages
library(ggplot2)
library(ggmap)


car_route <- full_interval[full_interval$car_name == 'Benedikt',]
# getting the map
mapgilbert <- get_map(location = c(lon = mean(car_data$lng), lat = mean(car_data$lat)), zoom = 16,
                      maptype = "roadmap", scale = "auto")

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = car_data, aes(x = car_data$lng, y = car_data$lat, fill = "red", alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)



#Get the map of Amsterdam from Google
MunMap <- qmap('munich', zoom = 14)

#Plot the map and the routes on top of that
MunMap +
  geom_path(aes(x = car_data$lng, y = car_data$lat, group = factor(car_data$datetime)), 
            colour="#1E2B6A", data = car_data, alpha=0.3)




route_df <- data.frame()
from <- 'St Georges Hall, Liverpool, UK'
to <- 'L69 3GP'
route_df <- route(from, to, structure = 'route', mode = 'driving')
qmap('Warren St, Liverpool', zoom = 16) +
  geom_path(
    aes(x = lon, y = lat),  colour = 'red', size = 1.5,
    data = route_df, lineend = 'round')

gkey <- "AIzaSyDVNbED6eCoGO75NkBHo9A_OSupyLeWO7c"

results = gmapsdistance(origin = "Washington+DC",
                        destination = "New+York+City+NY",
                        mode = "driving")

results = gmapsdistance(origin = "38.1621328+24.0029257",
                        destination = "37.9908372+23.7383394",
                        mode = "walking")

results = gmapsdistance(origin = paste0(car_interval$start_latitude[1],"+",car_interval$start_longitude[1]),
                        destination = paste0(car_interval$end_latitude[1],"+",car_interval$end_longitude[1]),
                        mode = "driving")

results = gmapsdistance(origin = "48.15670013,11.57559967",
                        destination = "48.1534996,11.57849979",
                        key = gkey,
                        mode = "driving")


results = gmapsdistance(origin = "Berlin",
                        destination = "Munich",
                        mode = "driving")

library(googleway)

## your valid API key
key <- read.dcf("~/Documents/.googleAPI", fields = c("GOOGLE_API_KEY"))

directions <- google_directions(origin = c(48.15670013,11.57559967),
                                destination = c(48.1534996,11.57849979),
                                key = gkey, 
                                simplify = T)

directions$routes

df_route <- decode_pl(directions$routes$overview_polyline$points)

google_map(data = df_route, key = key, height = 800, search_box = T) %>%
  add_markers()

diff_time <- data.frame(carname = car_data$name,  datetime = car_data$datetime)
diff_time$next_date <- c(diff_time$datetime[-1], NA)
diff_time$diff <- difftime(diff_time$next_date, diff_time$datetime,units="mins")
diff_time$diff <- as.numeric(diff_time$diff)

res <- diff_time[diff_time$diff >30,]
res <- na.omit(res)

ls <- diff_time[diff_time$diff >30, c('diff')]
sum(ls,na.rm = TRUE)

day_df <- car_interval[car_interval$start_datetime >= '2016-09-27' & car_interval$start_datetime <= '2016-09-28',]

car_coordinates <- data.frame(day_df$longitude, day_df$latitude)

# getting the map
mapgilbert <- get_map(location = c(lon = mean(day_df$longitude), lat = mean(day_df$latitude)), zoom = 16,
                      maptype = "roadmap", scale = "auto")

map <- get_googlemap(center = c(lon = mean(day_df$longitude), lat = mean(day_df$latitude)), markers = car_coordinates,
                                zoom = 18, path = df, scale = 2)
ggmap(map, extent = 'device')

