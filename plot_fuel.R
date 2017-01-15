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
