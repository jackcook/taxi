using DataFrames

train_df = readtable("./input/train.csv")
test_df = readtable("./input/test.csv")

ids = test_df[:id]
mean_time = convert(Int64, round(mean(train_df[:trip_duration])))
test_df[:time] = mean_time

submission = DataFrame(id = ids, trip_duration = test_df[:time])
writetable("./output/submission.csv", submission)
