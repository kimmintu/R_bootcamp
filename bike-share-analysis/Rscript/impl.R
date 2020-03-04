######################################################################
## prepare libraries ##
list.of.packages <- c("installr", "Hmisc", "ggmap", "tidyverse", "tidyr", "corrplot",
                      "viridis", "leaflet", "lubridate", "checkpoint", "zoo", "caTools",
                      "randomForest", "boot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for (p in list.of.packages){
  library(p, character.only = TRUE)
}


######################################################################
## user defined functions and constants ##

## mapping of 33 different weather conditions to 6 different weather types
sunny <- c("Fair", "Fair / Windy")
cloudy <- c("Cloudy", "Mostly Cloudy", "Partly Cloudy", "Mostly Cloudy / Windy", "Haze")
rainy <- c("Light Rain", "Light Drizzle", "Rain", "Heavy Rain", "Light Rain / Windy", "Fog", "Light Freezing Drizzle", "Rain / Windy", "Heavy Rain / Windy", "Haze / Windy", "Light Drizzle / Windy", "T-Storm", "Drizzle and Fog", "Thunder", "Heavy T-Storm", "Light Rain with Thunder", "Patches of Fog")
windy <- c("Partly Cloudy / Windy", "Cloudy / Windy", "Haze / Windy")
snowy <- c("Light Snow", "Wintry Mix", "Snow", "Heavy Snow", "Light Snow / Windy", "Snow / Windy", "Heavy Snow / Windy")


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

## convert 32 different weather conditions to 5 typical weather types, i.e. sunny, cloudy, rainy, windy, snowy.
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

## plot bike rental count over 24 hours for different category groups
## @param d.data: expects properties: category, hour, rental
## @param title: the title of the plot
## @param category_name: the category group name used as the legend's name in the plot
show_24h_category_statistics_plot <- function (d.data, title, category_name) {
  ggplot(d.data, aes(x=hour, y=rental, color=category)) + 
    geom_point(data=d.data, aes(group=category)) + 
    geom_line(data=d.data, aes(group=category)) +
    ggtitle(title) + 
    scale_color_hue(category_name, breaks=levels(d.data$category))
}


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

"was machst du da, Tu? "
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
d.weather$type <- get_weather_type_vector(d.weather$condition)
d.weather$type <- as.factor(d.weather$type)
d.weather <- subset(d.weather, select=-c(condition))

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

# ggplot 24h by month
d.month <- d.total %>% 
  group_by(month, hour) %>% 
  summarise(rental=round(mean(rental_count))) %>%
  rename(category = month)
show_24h_category_statistics_plot(d.month, "24h Rental by Month", "Month")

# ggplot 24h by weekday
d.weekday <- d.total %>% 
  group_by(weekday, hour) %>% 
  summarise(rental=round(mean(rental_count))) %>%
  rename(category = weekday)
show_24h_category_statistics_plot(d.weekday, "24h Rental by Weekday", "Weekday")

# ggplot 24h by weather types
d.wtype <- d.total %>% 
  group_by(type, hour) %>%
  summarise(rental = round(mean(rental_count))) %>%
  rename(category = type)
show_24h_category_statistics_plot(d.wtype, "24h Rental by Weather Type", "W.Type")

ggplot(d.total, aes(x=type, y=rental_count, color=type)) +
  geom_boxplot(data=d.total, aes(group=type))

ggplot(d.total, aes(x=weekday, y=rental_count)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_grid(. ~ type)

# correlation plot
d.weather_rental <- d.total[,5:10]
d.weather_rental <- cor(d.weather_rental)
corrplot(d.weather_rental, method = 'color', addCoef.col="black")


######################################################################
### a chapter of choice ###
set.seed(1)
rf.fit <- randomForest(rental_count ~ . - rental_count - tripduration - date, data = d.train,
                      mtry=3, ntree=50, importance=TRUE)
rf.pred <- predict(rf.fit, d.test)
mean((rf.pred - d.test$rental_count)^2)
importance(rf.fit)
varImpPlot(rf.fit)


######################################################################
### more complicated model ###

lm.fit <- glm(rental_count ~ type + hour + weekday * (
    poly(temperature, degree=2) + poly(humidity, degree=3)
  ), family="poisson", data=d.train)

lm.fit <- lm(log(rental_count) ~ type + hour + weekday * (
  poly(temperature, degree=2) + poly(humidity, degree=3)
), data=d.train)

# predict model on test data
lm.pred <- predict(lm.fit, newdata = d.test)
head(round(lm.pred))
head(lm.pred)
# compute R^2
cor(lm.pred, d.test$rental_count)^2
drop1(lm.fit, test="F")
# compute MSE
mean((d.test$rental_count - lm.pred)^2)
# residual
resid_values <- resid(lm.fit)
length(resid_values)
head(resid_values)

# plot
par(mfrow=c(2,2))
plot(lm.fit)

#### k-Cross validation ###
set.seed(1)

cv.error.10 <- rep(0, 10)

for (i in 1:10) {
  glm.fit <- glm(rental_count ~ type + hour * poly(temperature, i), data=d.train)
  cv.error.10[i] <- cv.glm(d.train, glm.fit, K=10)$delta[1]
}

cv.error.10
# 5 poly best
lm.fit <- glm(rental_count ~ type + hour * poly(temperature, 5), data=d.train)


######################################################################
### very simple model ###
set.seed(1)
d.split <- sample.split(d.total, SplitRatio = 0.7)

d.train <- subset(d.total, subset = d.split)
d.test <- subset(d.total, subset = !d.split)

# fit model on train data
# linear regression
lm.fit <- lm(log(rental_count) ~ type + hour, data=d.train)
# poisson
lm.fit <- glm(rental_count ~ type + hour, family="poisson", data=d.train)
# predict model on test data
lm.pred <- predict(lm.fit, newdata = d.test)
head(round(lm.pred))
head(lm.pred)
# compute R^2
cor(lm.pred, d.test$rental_count)^2
drop1(lm.fit, test="F")
# residual
resid_values <- resid(lm.fit)
length(resid_values)
head(resid_values)

# plot
par(mfrow=c(2,2))
#par(mar = rep(2, 4))
plot(lm.fit)


######################################################################
### simple model - LM ###
set.seed(1)
d.split <- sample.split(d.total, SplitRatio = 0.7)

d.train <- subset(d.total, subset = d.split)
d.test <- subset(d.total, subset = !d.split)

lm.fit <- lm(rental_count ~ . - rental_count - date - tripduration, data=d.train)
lm.fit <- lm(log(rental_count) ~ . - rental_count - date - tripduration, data=d.train)
summary(lm.fit)
lm.fit = step(lm.fit)

par(mfrow=c(1,1))
par(mar = rep(2, 4))
plot(lm.fit)

summary(lm.fit)$coefficients
confint(lm.fit)
formula(lm.fit)
summary(lm.fit)$r.squared
summary(lm.fit)$adj.r.squared

fitted_values <- fitted(lm.fit)
str(fitted_values)
head(fitted_values)

resid_values <- resid(lm.fit)
length(resid_values)
head(resid_values)

drop1(lm.fit, test="F")

######################################################################
## sophisticated model - regression tree (randomforest)

set.seed(1)
#rf.fit <- randomForest(rental_count ~ )





######################################################################
## new trial model - NN or ...





































