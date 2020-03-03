######################################################################
### prepare libraries ###
list.of.packages <- c("installr", "Hmisc", "ggmap", "tidyverse", "tidyr",
                      "viridis", "leaflet", "lubridate", "checkpoint", "zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for (p in list.of.packages){
  library(p, character.only = TRUE)
}


######################################################################
### functions and constants ###
farenheit_to_celsius <- function(variable_in_farenheit){
  return(as.numeric(5/9*(variable_in_farenheit-32)))
}

mph_to_kmh <- function(mph) {
  return (as.numeric(1.61 * mph))
}

### holidays in New York City in 2016
public_holidays <- c("2016-01-01", "2016-01-18", "2016-02-12", "2016-02-15", 
              "2016-05-30", "2016-07-04", "2016-09-05", "2016-10-10", 
              "2016-11-11", "2016-11-24", "2016-12-26")


######################################################################
### process bike sharing data ###
d.bike.raw = read_csv("./data/NYC-CitiBike-2016.csv")

# replace white space by underscore in column names 
d.bike.raw <- d.bike.raw %>% select_all(snakecase::to_snake_case)

# convert data
d.bike <- d.bike.raw %>%
  select(starttime, tripduration) %>%
  mutate(starttime = mdy_hms(starttime),
         date = date(starttime), # YYYY-MM-DD
         month = factor(month(starttime)),
         weekday = factor(weekdays(starttime)),
         hour = factor(hour(starttime)),
         tripduration = round(tripduration/60) #second to minute
  )

# handle outliers
# consider tripduration outliers
d.bike %>% filter(tripduration < 121) %>% 
  ggplot(aes(x=(tripduration))) + geom_histogram() + xlim(c(0, 50))

d.bike <- filter(d.bike, tripduration < 1441) # only use data with rental < 24 hours

# aggreate bike sharing data on hour basis
d.bike <- d.bike %>% 
  group_by(date, month, weekday, hour) %>%
  summarise(tripduration=sum(tripduration), rental_count=n())
head(d.bike)
str(d.bike)


######################################################################
### process weather data ###
d.weather.raw = read_csv("./data/New_York_Weather_Hourly_2016.csv")
d.weather <- d.weather.raw
# fahrenheit to celsius
d.weather$dewpoint <- round(farenheit_to_celsius(d.weather$dewpoint))
d.weather$temperature <- round(farenheit_to_celsius(d.weather$temperature))
# miles per hour to km per hour
d.weather$windspeed <- round(mph_to_kmh(d.weather$windspeed))
# create new column hour from time
d.weather$hour <- hour(d.weather$time)
# remove column time
d.weather <- subset(d.weather, select=-c(time))
# remove duplicated (date and time) rows
d.weather <- d.weather %>% distinct(date, hour, .keep_all=TRUE)
# set factor for condition
d.weather$condition <- as.factor(d.weather$condition)


######################################################################
### process to merge bike and weather ###
# daily weather data in 2016 has 366 * 24 = 8784 records (year 2016 has 366 days)
# but our weather data has less, so we need to fill missing values of weather data 
# create a full data frame with 24 x 366 records of date and hour
full <- expand.grid(date = unique(d.weather$date),
                    hour = unique(d.weather$hour))
full <- full[with(full, order(date, hour)), ]
rownames(full) <- NULL
# merge weather data (less records) with full data (full records)
d.weather <- merge(full, d.weather, all = TRUE)
# using 'zoo' library to fill the missing NA data with the previous available data
d.weather <- na.locf(d.weather)
# check if all NA are filled
colSums(is.na(d.weather))

## merge both bike and weather data for analysis
d.total <- merge(d.bike, d.weather, all.x=TRUE)
# order by data and hour
d.total <- d.total[with(d.total, order(date, hour)),]
# reset row index to normal
rownames(d.total) <- NULL


######################################################################
### public holiday analysis ###
# rental statistics in weekdays
rentweekday <- d.total %>%
  group_by(weekday) %>%
  summarise(rental=round(sum(rental_count)/52),
            tripduration=round(sum(tripduration)/52)) %>% # 52 weeks of a year
  arrange(weekday)
rentweekday
# rentals statistics in public holidays
rentholiday <- d.total %>%
  filter(as.character(date) %in% public_holidays) %>%
  group_by(date) %>%
  summarise(rental=sum(rental_count),
            tripduration=sum(tripduration)) %>% 
  arrange(date)
rentholiday
# remove public holiday dataset
d.total <- filter(d.total, !as.character(date) %in% public_holidays)

## save processed data to file
saveRDS(d.total, file = "./data/d.total.rds")


######################################################################
### EDA ###
str(d.total)
unique(d.total$condition)

hist(log(d.total$rental_count))
hist(log(d.total$tripduration))

# d.data expects properties: category, hour, rental
show_24h_category_statistics_plot <- function (d.data, title, category_name) {
  ggplot(d.data, aes(x=hour, y=rental, color=category)) + 
    geom_point(data=d.data, aes(group=category)) + 
    geom_line(data=d.data, aes(group=category)) +
    ggtitle(title) + 
    scale_color_hue(category_name, breaks=levels(d.data$category))
}

d.month <- d.total %>% group_by(month, hour) %>% 
  summarise(rental=round(mean(rental_count))) %>%
  rename(month = category)
show_24h_category_statistics_plot(d.month, "24h Rental by Month", "Month")






## simple model - LM


## sophisticated model - tree


## new trial model - NN or ...





































