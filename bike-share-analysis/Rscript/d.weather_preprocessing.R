### PREPROCESSING OF WEATHER DATA ####

# remove all objects loaded and clear memory
rm(list = ls(all.names = TRUE))
gc()


## load packages and install them when necessary ##
list.of.packages <- c("installr", "Hmisc", "ggmap", "tidyverse", "tidyr",
                      "viridis", "leaflet", "lubridate", "checkpoint", "zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for (p in list.of.packages){
  library(p, character.only = TRUE)
}

checkpoint(snapshotDate = "2099-12-29")



###### LOAD DATA ######
d.weather.raw <- read_csv("./data/weather_nyc_2016.csv")
colnames(d.weather.raw)
describe(d.weather.raw)
dim(d.weather.raw)
###### CONVERT DATA ######
d.weather <- d.weather.raw %>% select_all(snakecase::to_snake_case)

colnames(d.weather)
describe(d.weather)

farenheit_to_celsius <- function(variable_in_farenheit){
  return(as.numeric(5/9*(variable_in_farenheit-32)))
}

colnames(d.weather)
for (col in d.weather){
  print(class(col))
}

colnames(d.weather)
d.weather <- d.weather %>%
  mutate(
    weekday = weekdays(as.Date(date)),
    minimum_temperature_celsius = farenheit_to_celsius(minimum_temperature),
    maximum_temperature_celsius = farenheit_to_celsius(maximum_temperature),
    average_temperature_celsius = farenheit_to_celsius(average_temperature),
    precipitation = as.numeric(precipitation),
    snow_fall = as.numeric(snow_fall),
    snow_depth = as.numeric(snow_depth)
  )

colnames(d.weather)
for (col in d.weather){
  print(class(col))
}

###### CREATE DATE ######
describe(d.weather$date)

###### MERGE WEATHER AND BIKE DATA ######
d.bike_weather <- base::merge(y=d.bike, x=d.weather, 
                        by.y=c("startdate"), 
                        by.x=c("date")
                        )

colnames(d.bike_weather)
d.bike_weather$date = as.Date(d.bike_weather$date)

saveRDS(d.bike_weather, file = "./data/d.bike_weather.rds")
