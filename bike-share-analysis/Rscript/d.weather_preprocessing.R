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
d.bike <- readRDS("../data/d.bike.prepared.rds")
colnames(d.bike)

# setwd("C:/Users/Andy Gubser/OneDrive - Hochschule Luzern/01 Studium/03 MSc Data Science/Master HS19/Wahlpflichtmodule/W.MSCIDS_RB01.H1901/Assignment/R_bootcamp/bike-share-analysis/Rscript")
d.weather.raw <- read_csv("./data/weather_nyc_2016.csv")
colnames(d.weather.raw)
describe(d.weather.raw)

###### CONVERT DATA ######
d.weather <- d.weather.raw %>% select_all(snakecase::to_snake_case)
colnames(d.weather)
describe(d.weather)

###### CREATE DATE ######
d.weather$date <- dmy(d.weather$date)
describe(d.weather$date)

###### MERGE WEATHER AND BIKE DATA ######
d.bike_weather <- base::merge(x=d.bike, y=d.weather, 
                        by.x=c("startdate"), 
                        by.y=c("date")
                        )

colnames(d.bike_weather)

saveRDS(d.bike_weather, file = "./data/d.bike_weather.rds")

