## Cyclistic-Case-Study: A case study done as part of my Google career certificate program, Practicing coding in R.
 
For this case study I was asked to take on the role of a junior data anayist working at a ficticious bike sharing service, Cyclistic. I was given two data sets covering a total of 6 months of data and asked to identify how annual members and casual riders use Cyclistic bikes differently

The data for this case study was made public by Lyft and the City of Chicago under this license https://divvybikes.com/data-license-agreement. 

I began by downloading the two files into Google Sheets and inspecting the data to see what information would and would not be helpful in my analysis. I removed columns that contained unessecary data as well as made sure that the dates and times were formated correctly and that the length of the rides was calculated correctly. 

After my initial inspection of the data and some basic cleaning, I formed my hypothesis; Users who are anual membership holders are more likley to take short trips and make a up a majority of the riders compared to nonmembers or casual riders. 

Following my initial inspection and cleaning I loaded my data into R studio so I could have a more in depth look at what the data showed.

# Step 1 
 Loading the nessecary packages 

```
library(tidyverse)

library(conflicted)

conflict_prefer("filter", "dplyr")

conflict_prefer("lag", "dplyr")
```
# Step 2
 After loading in my data sets, I created variables to make futher code easier and inspected the column names to find any differences.

```
q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")

q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

colnames(q1_2019)

colnames(q1_2020)
```
# Step 3
 I converted the column names in the q1_2019 data frame to match the column names from q1_2020

```
(q1_2019 <- rename(q1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype
                   ))
```
# Step 4
 I prepared the two data frames to be merged together removing columns from q1_2019 data frame that were not present in the q1_2020 data frame.

```
q1_2019 <- mutate(q1_2019, ride_id = as.character(ride_id))
+                   (rideable_type = as.character(rideable_type))
```
```
all_trips <- bind_rows(q1_2019, q1_2020)
```
```
all_trips <- all_trips %>%  
+     select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender,  "tripduration"))
```
# Step 5
 Then I standarized the terminology for refering to the two customer types I was looking at. 

```
q1_2019 <- q1_2019 %>%
  mutate(member_casual = recode(member_casual,
                                "Subscriber" = "member", 
                                "Customer" = "casual"))
```
I formated columns to list out dates and times seperatly so that I can aggregate the data at a finer level. 

```  
all_trips$date <- as.Date(all_trips$started_at) 
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
```
# Step 6
 Then I calculated the mean, median, max, and minimum ride lengths for all rides from both data frames. 

```
mean(all_trips_v2$ride_length) 
#[1] 1189.459]
median(all_trips_v2$ride_length) 
#[1] 539
max(all_trips_v2$ride_length) 
#[1] 10632022
min(all_trips_v2$ride_length)
#[1] 1
mean(all_trips_v2$ride_length)
#[1] 1189.459
median(all_trips_v2$ride_length)
#[1] 539
max(all_trips_v2$ride_length)
#[1] 10632022
min(all_trips_v2$ride_length) 
#[1] 1
summary(all_trips_v2$ride_length)
   # Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    #   1      331      539     1189      912 10632022
```
# Step 7
 Then I repeat the same process but instead of all rides I break it down by casual riders and member riders. 

```
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
    #all_trips_v2$member_casual all_trips_v2$ride_length
#1                     casual                5372.7839
#2                     member                 795.2523
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
#all_trips_v2$member_casual all_trips_v2$ride_length
#1                     casual                     1393
#2                     member                      508
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
#all_trips_v2$member_casual all_trips_v2$ride_length
#1                     casual                 10632022
#2                     member                  6096428
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
#all_trips_v2$member_casual all_trips_v2$ride_length
#1                     casual                        2
#2                     member                        1
```
This code ordered the dats of the week in the appropriate order. 

```
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
```
```
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
```
# Step 8 
 Finally I created plots using the ggplot2 package in R studio. 

 The plot below is the average ride length 

```
> >  all_trips_v2 %>%
+   mutate(weekday = wday(started_at, label = TRUE)) %>%
+   group_by(member_casual, weekday) %>%
+   summarise(number_of_rides = n(),
+             average_duration = mean(ride_length, na.rm = TRUE), 
+             .groups = "drop") %>% 
+   arrange(member_casual, weekday) %>%
+   ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + 
+   geom_col(position = "dodge") +
+   scale_y_continuous(labels = label_number(scale = 1/60, big.mark = ",")) +
+   labs(title = "Average Ride Duration by Membership", 
+        x = "Day of Week",
+        y = "Average Duration (minutes)", 
+        fill = NULL)
```

![Average ride length by membership made in R studio](https://github.com/user-attachments/assets/a1ec317c-20ca-446f-a8a2-40dd68d05343)

The below plot is for total number of riders 

```
  all_trips_v2 %>%
+   mutate(weekday = wday(started_at, label = TRUE)) %>%
+   group_by(member_casual, weekday) %>%
+   summarise(number_of_rides = n(),
+             average_duration = mean(ride_length, na.rm = TRUE),
+             .groups = "drop") %>% # Added .groups = "drop" here
+   arrange(member_casual, weekday) %>%
+   ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
+   geom_col(position = "dodge") +
+   scale_y_continuous(labels = label_comma()) +
+   labs(title = "Number of Riders by Membership",
+        x = "Day of Week",
+        y = "Number of Rides",
+        fill= NULL)
```

![Graph showing the number of riders by membership made in R studio](https://github.com/user-attachments/assets/d7c094c1-344f-45f5-9867-4dd739568a8c)

# Conclusion 
 Through my analysis in R Studio, I was able to determine that anual members use Cyclistic's bike service entirley different from casual riders. Anual members are repsonsible for the majority of riders through the service, however, those rides are much shorter in length, averaging 13 minutes, than those of casual members, averaging 89 minutes. Anual riders appear to be using the service to commute to work, run errands or take short trips downtown while it appears that casual riders are taking longer leisure trips through the city.
 # Reccomendations 
  Based on my findings, I would recommend a marketing strategy based on highlighting leisure activities throughout and city and ensuring that bike rental and return stations are available close to these areas, suggesting to casual riders that getting an anual membership would allow them more convience when using the bikes for their leisure activities. 
