### Cyclistic_Exercise_Full_Year_Analysis ###

# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
library(lubridate)
install.packages("skimr")
library(skimr)
install.packages("janitor")
library(janitor)

#Step 1 : COLLECT DATA
q2_2019 <-
  read.csv(
    "C:/Users/USER/Downloads/Cyclics-trip-data/divvy-tripdata/2019/Divvy_Trips_2019_Q2.csv"
  )
q3_2019 <-
  read.csv(
    "C:/Users/USER/Downloads/Cyclics-trip-data/divvy-tripdata/2019/Divvy_Trips_2019_Q3.csv"
  )
q4_2019 <-
  read.csv(
    "C:/Users/USER/Downloads/Cyclics-trip-data/divvy-tripdata/2019/Divvy_Trips_2019_Q4.csv"
  )
q1_2020 <-
  read.csv(
    "C:/Users/USER/Downloads/Cyclics-trip-data/divvy-tripdata/2020/Divvy_Trips_2020_Q1.csv"
  )


View(q1_2020)
head(q4_2019)
skim_without_charts(q4_2019)

#STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)

#RENAME COLUMNS NAME TO MAKE IT CONSISTENT
(
  q4_2019 <- rename(
    q4_2019,
    ride_id = trip_id,
    started_at = start_time,
    ended_at = end_time,
    rideable_type = bikeid,
    start_station_id = from_station_id,
    start_station_name = from_station_name,
    end_station_id = to_station_id,
    end_station_name = to_station_name,
    member_casual = usertype
  )
)

(
  q3_2019 <- rename(
    q3_2019,
    ride_id = trip_id,
    started_at = start_time,
    ended_at = end_time,
    rideable_type = bikeid,
    start_station_id = from_station_id,
    start_station_name = from_station_name,
    end_station_id = to_station_id,
    end_station_name = to_station_name,
    member_casual = usertype
  )
)

(
  q2_2019 <- rename(
    q2_2019,
    ride_id = X01...Rental.Details.Rental.ID,
    started_at = X01...Rental.Details.Local.Start.Time,
    ended_at = X01...Rental.Details.Local.End.Time,
    rideable_type = X01...Rental.Details.Bike.ID,
    start_station_id = X03...Rental.Start.Station.ID,
    start_station_name = X03...Rental.Start.Station.Name,
    end_station_id = X02...Rental.End.Station.ID,
    end_station_name = X02...Rental.End.Station.Name,
    member_casual = User.Type
  )
)

str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

# Convert ride_id and rideable_type to character so that they can stack correctly
q4_2019 <- mutate(q4_2019,
                  ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))
q3_2019 <- mutate(q3_2019,
                  ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))
q2_2019 <- mutate(q2_2019,
                  ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)
View(all_trips)

# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%
  select(
    -c(
      start_lat,
      start_lng,
      end_lat,
      end_lng,
      birthyear,
      gender,
      X01...Rental.Details.Duration.In.Seconds.Uncapped,
      X05...Member.Details.Member.Birthday.Year,
      Member.Gender,
      tripduration
    )
  )




# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS

# Inspect the new table that has been created
colnames(all_trips) #List of column names
nrow(all_trips) #How many rows are in data frame?
dim(all_trips) #Dimensions of the data frame?
head(all_trips) #See the first 6 rows of data frame. Also tail(qs_raw)
str(all_trips) #See list of columns and data types (numeric, character, etc)
summary(all_trips) #Statistical summary of data.


# Reassign to the desired values (we will go with the current  2020 labels)
all_trips <- all_trips %>%
  mutate(member_casual = recode(
    member_casual,
    "Subscriber" = "member",
    "Customer" = "casual"
  ))

# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)
str(all_trips)

# Check if 'ride_length' is a factor
is.factor(all_trips$ride_length)

# Convert 'ride_length' to numeric and check if it is numeric
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

View(all_trips)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length < 0), ]

View(all_trips_v2)

# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
# Descriptive analysis on ride_length
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(
  all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
  FUN = mean
)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(
  all_trips_v2$day_of_week,
  levels = c(
    "Sunday",
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday"
  )
)
# Now, let's run the average ride time by each day for members vs casual users
aggregate(
  all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
  FUN = mean
)

# analyze ridership data by type and weekday
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)


# Let's visualize the number of rides by rider type
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

