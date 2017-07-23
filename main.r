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
  return(df[complete.cases(df), ])
}

clean <- function(df) {
  df <- df %>%
    mutate(pickup_datetime = ymd_hms(pickup_datetime),
           vendor_id = factor(vendor_id),
           passenger_count = factor(passenger_count),
           store_and_fwd_flag = factor(store_and_fwd_flag))
  
  if (is.element("trip_duration", names(df))) {
    # filter out rides that are shorter than 10 seconds or longer than 4 hours
    df <- df[df$trip_duration > 10 & df$trip_duration < 14400, ]
    
    df <- df %>%
      mutate(dropoff_datetime = ymd_hms(dropoff_datetime))
  }
  
  df["month"] <- month(df["pickup_datetime"][[1]])
  df["day"] <- day(df["pickup_datetime"][[1]])
  df["weekday"] <- wday(df["pickup_datetime"][[1]])
  df["hour"] <- hour(df["pickup_datetime"][[1]])
  df["minute"] <- minute(df["pickup_datetime"][[1]])
  
  df["distance"] <- sqrt((df["pickup_longitude"] - df["dropoff_longitude"]) ^ 2 + (df["pickup_latitude"] - df["dropoff_latitude"]) ^ 2)
  
  df = find_neighborhoods(df, "pickup_longitude", "pickup_latitude", "pickup_neighborhood")
  df = find_neighborhoods(df, "dropoff_longitude", "dropoff_latitude", "dropoff_neighborhood")
  
  return(df)
}

train <- clean(fread("./input/train.csv"))
test <- clean(fread("./input/test.csv"))

Y_train <- train["trip_duration"][[1]]
id_test <- test["id"][[1]]

train[c("id", "dropoff_datetime", "trip_duration")] <- list(NULL)
test[c("id")] <- list(NULL)

model <- xgboost(data.matrix(train), label = Y_train, nrounds = 100)
importance_matrix <- xgb.importance(colnames(data.matrix(train)), model = model)
xgb.plot.importance(importance_matrix, rel_to_first = TRUE, xlab = "Relative importance")

predictions <- pmax(0, round(predict(model, data.matrix(test))))
results = data.frame(id_test, predictions)
names(results) <- c("id", "trip_duration")

write.csv(results, "./output/submission.csv", quote = FALSE, row.names = FALSE)