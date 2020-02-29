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
# getwd()
# setwd("C:/Users/Andy Gubser/OneDrive - Hochschule Luzern/01 Studium/03 MSc Data Science/Master HS19/Wahlpflichtmodule/W.MSCIDS_RB01.H1901/Assignment/R_bootcamp/bike-share-analysis/Rscript")
d.bike.raw = read_csv("./data/NYC-CitiBike-2016.csv")
colnames(d.bike.raw)
dim(d.bike.raw)
# describe(d.bike.raw)


###### CONVERT DATA ######
d.bike <- d.bike.raw %>% select_all(snakecase::to_snake_case)

d.bike <- d.bike %>%
  mutate(starttime = mdy_hms(starttime),
         stoptime = mdy_hms(stoptime),
         startdate = date(starttime), # YYYY-MM-DD
         startdate = as.Date(startdate),
         tripduration_min = tripduration/60,
         gender = factor(gender, 
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
         age_group = cut(age, breaks = 20),
         
         month = factor(month(starttime), 
                        levels = seq(1,12,1), 
                        labels = c("Jan", "Feb", "Mar", 
                                   "Apr", "Mai", "Jun", 
                                   "Jul", "Aug", "Sep", 
                                   "Oct", "Nov", "Dec"))
         )

summary(d.bike$age)
summary(as.numeric(d.bike$age_cappedegory))
hist(d.bike$age)
hist(as.numeric(d.bike$age_cappedegory))
# describe(d.bike$startdate)


###### DROP OUTLIERS######


d.bike <- filter(d.bike, 
                 tripduration_min < 200
                   )
hist(d.bike$tripduration_min)

table(d.bike$tripduration)

# coordinates
# start_plot <- ggplot() +
#   geom_point(
#     aes(x=start_station_latitude, y=start_station_longitude), data=d.bike)
# start_plot
# 
# end_plot <- ggplot() +
#   geom_point(
#     aes(x=end_station_latitude, y=end_station_longitude), data=d.bike)
# end_plot

d.bike <- filter(d.bike, 
                  start_station_latitude > 40.6,
                  end_station_latitude > 40.6
                  )
# 
# start_plot2 <- ggplot() +
#   geom_point(
#     aes(x=start_station_latitude, y=start_station_longitude), data=d.bike)
# # start_plot2
# 
# end_plot2 <- ggplot() +
#   geom_point(
#     aes(x=end_station_latitude, y=end_station_longitude), data=d.bike)
# # end_plot2

summary(d.bike$usertype)

###### FILTER SUBSCRIBERS ######
d.bike <- filter(d.bike, usertype == "Subscriber")

###### EXCLUDE UNKNOWN GENDER ######
summary(d.bike$gender)
d.bike <- filter(d.bike, 
                 gender == "F" | gender == "M") %>%
  droplevels()

summary(d.bike$gender)



saveRDS(d.bike, file = "./data/d.bike.prepared.rds")