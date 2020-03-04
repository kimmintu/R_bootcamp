# Install and load packages

# remove all objects loaded and clear memory
rm(list = ls(all.names = TRUE))
gc()

# library(checkpoint)
# checkpoint(snapshotDate = "2020-01-01")

###### LOAD PACKAGES ######
# create function eventually install and load all packages <- 
if(!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")
library("lubridate")
library("Hmisc")

###### LOAD DATA ######
d.bike <- readRDS("./data/d.bike.prepared.rds")
colnames(d.bike)

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

d.weather <- d.weather %>%
  mutate(
    date = dmy(date),
    weekday = weekdays(date),
    minimum_temperature_celsius = farenheit_to_celsius(minimum_temperature),
    maximum_temperature_celsius = farenheit_to_celsius(maximum_temperature),
    average_temperature_celsis = farenheit_to_celsius(average_temperature),
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
d.bike_weather <- base::merge(x=d.bike, y=d.weather, 
                        by.x=c("startdate"), 
                        by.y=c("date")
                        )

x <- c("28-11-2016", "29-11-2016", "30-11-2016")
x <- dmy(x)
x

saveRDS(d.bike_weather, file = "./data/d.bike_weather.rds")
