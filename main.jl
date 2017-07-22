using DataFrames

train_df = readtable("./input/train.csv")
test_df = readtable("./input/test.csv")

function calculate_distances!(df::DataFrame)
    R = 6371000
    pickup_latitude_radians = deg2rad.(df[:pickup_latitude])
    dropoff_latitude_radians = deg2rad.(df[:dropoff_latitude])
    latitude_difference = deg2rad.(df[:dropoff_latitude] - df[:pickup_latitude])
    longitude_difference = deg2rad.(df[:dropoff_longitude] - df[:pickup_longitude])
    a = sin(latitude_difference / 2) .* sin(latitude_difference / 2) .+ cos(pickup_latitude_radians) .* cos(dropoff_latitude_radians) .* sin(longitude_difference / 2) .* sin(longitude_difference / 2)
    c = atan2.(sqrt(a), sqrt(1 - a))
    df[:distance] = R * c
end

calculate_distances!(train_df)
calculate_distances!(test_df)

n = size(train_df, 1)
x = train_df[:distance]
y = train_df[:trip_duration]

r = (n * sum(x .* y) - sum(x) * sum(y)) / (sqrt(n * sum(x .* x) - sum(x) * sum(x)) * sqrt(n * sum(y .* y) - sum(y) * sum(y)))
b = r * std(y) / std(x)
a = mean(y) - b * mean(x)

X = test_df[:distance]
Y = max.(1, convert.(Int64, round.(a + b * X)))

submission = DataFrame(id = test_df[:id], trip_duration = Y)
writetable("./output/submission.csv", submission)
