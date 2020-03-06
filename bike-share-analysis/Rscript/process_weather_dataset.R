#### PREPROCESSING OF WEATHER DATA ####

# remove all objects loaded and clear memory
rm(list = ls(all.names = TRUE))
gc()


## load packages and install them when necessary ##
list.of.packages <- c("installr", "Hmisc", "ggmap", "tidyverse", "tidyr", "corrplot",
                      "viridis", "leaflet", "lubridate", "checkpoint", "zoo", "caTools",
                      "randomForest", "boot", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for (p in list.of.packages){
  library(p, character.only = TRUE)
}

checkpoint(snapshotDate = "2099-12-29")

######################################################################
## user defined functions and constants ##

## mapping of 33 different weather conditions to 6 different weather types
sunny  <- c("Fair", "Fair / Windy")
cloudy <- c("Cloudy", "Mostly Cloudy", "Partly Cloudy", "Mostly Cloudy / Windy", "Haze")
rainy  <- c("Light Rain", "Light Drizzle", "Rain", "Heavy Rain", "Light Rain / Windy", 
            "Fog", "Light Freezing Drizzle", "Rain / Windy", "Heavy Rain / Windy", 
            "Haze / Windy", "Light Drizzle / Windy", "T-Storm", "Drizzle and Fog", 
            "Thunder", "Heavy T-Storm", "Light Rain with Thunder", "Patches of Fog")
windy  <- c("Partly Cloudy / Windy", "Cloudy / Windy", "Haze / Windy")
snowy  <- c("Light Snow", "Wintry Mix", "Snow", "Heavy Snow", "Light Snow / Windy", 
            "Snow / Windy", "Heavy Snow / Windy")

## holidays in New York City in 2016
public_holidays <- c("2016-01-01", "2016-01-18", "2016-02-12", "2016-02-15", 
                     "2016-05-30", "2016-07-04", "2016-09-05", "2016-10-10", 
                     "2016-11-11", "2016-11-24", "2016-12-26")

## convert temperature degree from Fahrenheit to Celsius
## @param fahrenheit: degree in fahrenheit
## @return degree in celcius
farenheit_to_celsius <- function(fahrenheit){
  return(as.numeric(5/9*(fahrenheit-32)))
}

## convert miles per hour to kilometer per hour
## @param mph: miles per hour
## @return kilometer per hour
mph_to_kmh <- function(mph) {
  return (as.numeric(1.61 * mph))
}

## convert 32 different weather conditions to 5 typical weather types
## @param conditions: a vector of weather condition values
## @return a vector of weather types corresponding to the weather conditions
get_weather_type_vector <- function (conditions) {
  types <- 
    ifelse(conditions %in% sunny,"sunny",
           ifelse(conditions %in% cloudy,"cloudy",
                  ifelse(conditions %in% rainy,"rainy",
                         ifelse(conditions %in% windy,"windy",
                                ifelse(conditions %in% snowy,"snowy", NA)))))
  return (types)
}

######################################################################

## process weather data ##
d.weather.raw = read_csv("./data/New_York_Weather_Hourly_2016.csv")
d.weather <- d.weather.raw
## fahrenheit to celsius
d.weather$dewpoint <- round(farenheit_to_celsius(d.weather$dewpoint))
d.weather$temperature <- round(farenheit_to_celsius(d.weather$temperature))
## miles per hour to km per hour
d.weather$windspeed <- round(mph_to_kmh(d.weather$windspeed))
## create new column hour from time
d.weather$hour <- hour(d.weather$time)
## remove column time
d.weather <- subset(d.weather, select=-c(time))
## remove duplicated (date and time) rows -> only one weather reading for each hour
d.weather <- d.weather %>% distinct(date, hour, .keep_all=TRUE)
## introduce new column weather type
d.weather$type <- get_weather_type_vector(d.weather$condition)
d.weather$type <- as.factor(d.weather$type)
## remove unused column weather condition
d.weather <- subset(d.weather, select=-c(condition))

## there are 5 NA in the windspeed
colSums(is.na(d.weather))

## expecting weather data has 8784 hourly records (366 days in 2016 x 24h equals 8784)
## but there are only 8775 records, there needs to insert and fill missing records with NA
nrow(d.weather)

## create a full data frame with 24 x 366 records of date and hour
full <- expand.grid(date = unique(d.weather$date),
                    hour = unique(d.weather$hour))
## order the full data frame with date and hour
full <- full[with(full, order(date, hour)), ]
## re-index the data frame after ordering
rownames(full) <- NULL

## merge weather data frame (less records) with full data frame (full records)
d.weather <- merge(full, d.weather, all = TRUE)

## check NA values after merge, the new records are added with NA
colSums(is.na(d.weather))
## check which records with NA
d.weather %>% filter(is.na(windspeed)) %>% select(date, hour)

## as the NA are spreading with the date and hour, we can use the previous available measured weather
## using 'zoo' library to fill the missing NA data with the previous available data
d.weather <- na.locf(d.weather)

## check NA again, we see that all NA are now filled
colSums(is.na(d.weather))
head(d.weather)

saveRDS(d.weather, file = "./data/d.weather.rds")

