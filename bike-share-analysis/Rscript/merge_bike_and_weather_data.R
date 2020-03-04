#### MERGE BIKE AND WEATHER DATA ####

###### LOAD DATA ######
d.bike.raw = read_csv("./data/NYC-CitiBike-2016.csv")
colnames(d.bike.raw)
dim(d.bike.raw)
# describe(d.bike.raw)

d.bike <- readRDS("./data/d.bike.prepared.rds")
colnames(d.bike)



###### MERGE WEATHER AND BIKE DATA ######
d.bike_weather <- base::merge(y=d.bike, x=d.weather, 
                              by.y=c("startdate"), 
                              by.x=c("date")
)

colnames(d.bike_weather)
d.bike_weather$date = as.Date(d.bike_weather$date)

saveRDS(d.bike_weather, file = "./data/d.bike_weather.rds")


