setwd("/Users/jackcook/Desktop/taxi-r")

library(data.table)
library(dplyr)
library(lubridate)
library(xgboost)

train <- fread("./input/train.csv")
test <- fread("./input/test.csv")

train <- train %>%
  mutate(pickup_datetime = ymd_hms(pickup_datetime),
         dropoff_datetime = ymd_hms(dropoff_datetime),
         vendor_id = factor(vendor_id),
         passenger_count = factor(passenger_count),
         store_and_fwd_flag = factor(store_and_fwd_flag))
test <- test %>%
  mutate(pickup_datetime = ymd_hms(pickup_datetime),
         vendor_id = factor(vendor_id),
         passenger_count = factor(passenger_count),
         store_and_fwd_flag = factor(store_and_fwd_flag))

train["month"] <- month(train["pickup_datetime"][[1]])
train["day"] <- day(train["pickup_datetime"][[1]])
train["weekday"] <- wday(train["pickup_datetime"][[1]])
train["hour"] <- hour(train["pickup_datetime"][[1]])
train["minute"] <- minute(train["pickup_datetime"][[1]])

test["month"] <- month(test["pickup_datetime"][[1]])
test["day"] <- day(test["pickup_datetime"][[1]])
test["weekday"] <- wday(test["pickup_datetime"][[1]])
test["hour"] <- hour(test["pickup_datetime"][[1]])
test["minute"] <- minute(test["pickup_datetime"][[1]])

train["dist_long"] <- train["pickup_longitude"] - train["dropoff_longitude"]
test["dist_long"] <- test["pickup_longitude"] - test["dropoff_longitude"]

train["dist_lat"] <- train["pickup_latitude"] - train["dropoff_latitude"]
test["dist_lat"] <- test["pickup_latitude"] - test["dropoff_latitude"]

train["distance"] <- sqrt(train["dist_long"] ^ 2 + train["dist_lat"] ^ 2)
test["distance"] <- sqrt(test["dist_long"] ^ 2 + test["dist_lat"] ^ 2)

ytrain <- train["trip_duration"]
id_train <- train["id"]
id_test <- test["id"]

train[c("id", "pickup_datetime", "dropoff_datetime", "trip_duration")] <- list(NULL)
test[c("id", "pickup_datetime")] <- list(NULL)

bst <- xgboost(data = data.matrix(train), label = ytrain[[1]], nrounds = 100, objective = "reg:linear")
pred <- predict(bst, data.matrix(test))

results = data.frame(id_test[[1]], pred)