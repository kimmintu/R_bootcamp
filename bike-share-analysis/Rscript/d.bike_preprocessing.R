#### PREPROCESSING OF BIKESHAREING DATA ####

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
d.bike.raw = read_csv("./data/NYC-CitiBike-2016.csv")
colnames(d.bike.raw)
dim(d.bike.raw)
# describe(d.bike.raw)

###### CONVERT DATA ######

# replace white space by underscore in column names 
d.bike <- d.bike.raw %>% select_all(snakecase::to_snake_case)

# mutate data
d.bike <- d.bike %>%
  mutate(
    # extract month, date, weekday, hour from starttime
    starttime = mdy_hms(starttime),
    stoptime = mdy_hms(stoptime),
    date = as.Date(date(starttime)),
    month = factor(month(starttime), 
                   levels = seq(1,12,1), 
                   labels = c("Jan", "Feb", "Mar", 
                              "Apr", "Mai", "Jun", 
                              "Jul", "Aug", "Sep", 
                              "Oct", "Nov", "Dec")),
    weekday = factor(weekdays(starttime)),
    hour = factor(hour(starttime)),

    tripduration_min = tripduration/60,
    gender = factor(
      gender, 
      levels = c(0,1,2), 
      labels = c("X", "M", "F")
      ) ,
    start_station_id = factor(start_station_id),
    start_station_name = factor(start_station_name),
    end_station_id = factor(end_station_id),
    end_station_name = factor(end_station_name),
    bikeid = factor(bikeid),
    usertype = factor(usertype),
    age = 2016-as.numeric(birth_year),
    age_capped = pmin(age, 70), 
    age_group = cut(age_capped, breaks = 10)
    )

## Mutate Age above 70 ##
summary(d.bike$age)
summary(as.numeric(d.bike$age_cappedegory))
hist(d.bike$age)
hist(as.numeric(d.bike$age_cappedegory))


###### DROP OUTLIERS######

## COORDINATES ##
p.start_station.raw <- ggplot() +
  geom_point(
    aes(x=start_station_latitude, y=start_station_longitude), data=d.bike)
p.start_station.raw

p.end_station.raw <- ggplot() +
  geom_point(
    aes(x=end_station_latitude, y=end_station_longitude), data=d.bike)
p.end_station.raw

d.bike <- filter(d.bike, 
                 start_station_latitude > 40.6,
                 end_station_latitude > 40.6
)

p.start_station.filtered <- ggplot() +
  geom_point(
    aes(x=start_station_latitude, y=start_station_longitude), data=d.bike)
p.start_station.filtered

p.end_station.filtered <- ggplot() +
  geom_point(
    aes(x=end_station_latitude, y=end_station_longitude), data=d.bike)
p.end_station.filtered



## TRIPDURATION ##
hist(d.bike$tripduration_min)

# drop tripduration above 2 hours
d.bike <- filter(d.bike, tripduration_min <= 120)
hist(d.bike$tripduration_min)


###### FILTER SUBSCRIBERS ######
summary(d.bike$usertype)
d.bike <- filter(d.bike, usertype == "Subscriber")

###### EXCLUDE UNKNOWN GENDER ######
summary(d.bike$gender)
d.bike <- filter(d.bike, 
                 gender == "F" | gender == "M") %>%
  droplevels()

summary(d.bike$gender)


## EXPORT DATA ##
saveRDS(d.bike, file = "./data/d.bike.prepared.rds")
