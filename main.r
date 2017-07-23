library(data.table)
library(dplyr)
library(lubridate)
library(rgdal)
library(xgboost)

ny.map <- readOGR("./input/zillow/ZillowNeighborhoods-NY.shp", layer = "ZillowNeighborhoods-NY")
neighborhoods <- ny.map[ny.map$City == "New York", ]

find_neighborhoods <- function(df, long_feature, lat_feature, neighborhood_feature) {
  dat <- data.frame(long = df[long_feature][[1]], lat = df[lat_feature][[1]])
  coordinates(dat) <- ~ long + lat
  proj4string(dat) <- proj4string(neighborhoods)
  df[neighborhood_feature] <- over(dat, neighborhoods)$Name
}

clean <- function(df) {
  df <- df %>%
    mutate(pickup_datetime = ymd_hms(pickup_datetime),
           vendor_id = factor(vendor_id),
           passenger_count = factor(passenger_count),
           store_and_fwd_flag = factor(store_and_fwd_flag))
  
  if (is.element("dropoff_datetime", names(df))) {
    df <- df %>%
      mutate(dropoff_datetime = ymd_hms(dropoff_datetime))
  }
  
  df["month"] <- month(df["pickup_datetime"][[1]])
  df["day"] <- day(df["pickup_datetime"][[1]])
  df["weekday"] <- wday(df["pickup_datetime"][[1]])
  df["hour"] <- hour(df["pickup_datetime"][[1]])
  df["minute"] <- minute(df["pickup_datetime"][[1]])
  
  df["dist_long"] <- df["pickup_longitude"] - df["dropoff_longitude"]
  df["dist_lat"] <- df["pickup_latitude"] - df["dropoff_latitude"]
  df["distance"] <- sqrt(df["dist_long"] ^ 2 + df["dist_lat"] ^ 2)
  
  find_neighborhoods(df, "pickup_longitude", "pickup_latitude", "pickup_neighborhood")
  find_neighborhoods(df, "dropoff_longitude", "dropoff_latitude", "dropoff_neighborhood")
  
  return(df)
}

train <- clean(fread("./input/train.csv"))
test <- clean(fread("./input/test.csv"))

Y_train <- train["trip_duration"][[1]]
id_test <- test["id"][[1]]

train[c("id", "pickup_datetime", "dropoff_datetime", "trip_duration")] <- list(NULL)
test[c("id", "pickup_datetime")] <- list(NULL)

model <- xgboost(data.matrix(train), label = Y_train, nrounds = 1)
predictions <- pmax(0, round(predict(model, data.matrix(test))))

results = data.frame(id_test, predictions)
names(results) <- c("id", "trip_duration")

write.csv(results, "./output/submission.csv", quote = FALSE, row.names = FALSE)