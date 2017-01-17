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



#plot fuel
library(ggplot2) 

base <- ggplot(subset(all_vehicles_df, name %in% c("Elli")),
       aes(x=datetime,
           y=fuelLevel,
           color=name))+
  geom_line()

base + 
  scale_x_datetime(date_minor_breaks = "1 day", date_labels = "%b %d") +
  scale_y_discrete()


p5 <- ggplot(all_vehicles_df, aes(x = datetime, y = fuelLevel))
p5 + geom_line(aes(color = name))
(p5 <- p5 + geom_line() +
    facet_wrap(~name, ncol = 10))


#calc ratio
all_rent_cases$ratio <- (all_rent_cases$trip_fuel_loss*100) / as.numeric(all_rent_cases$trip_time_min)
truth_rent <- all_rent_cases[all_rent_cases$coef <0.3, ]
  



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


base <- ggplot(all_rent_cases, 
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

hist(all_rent_cases$coef)

all_rent_cases <- read.csv("all_rent_cases.csv", header=TRUE,row.names=NULL)

all_rent_cases$start_datetime <- as.POSIXct(all_rent_cases$start_datetime)
all_rent_cases$end_datetime <- as.POSIXct(all_rent_cases$end_datetime)


library(ggplot2)
library(plotly)

#Load data

ggplot(all_rent_cases,
       aes(x=rent_time_min,
           y=rent_fuel_loss))+
  geom_boxplot()

ggplotly()
