# install packages
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(rio)) install.packages("rio")
if(!require(skimr))install.packages("skimr")
if(!require(rmarkdown))install.packages("rmarkdown")
if(!require(doParallel))install.packages("doParallel")
if(!require(lubridate))install.packages("lubridate")
if(!require(patchwork))install.packages("patchwork")
library(doParallel)
library(skimr)
library(lubridate)
library(rmarkdown)
library(tidyverse)
library(rio)
library(patchwork)
#rio is a newer version of read_csv, and allows you 
#to import all sorts of file extensions. It also 
#correctly imports the date, instead of characters 
#as read_csv.

# do parallel computing in order to make model and 
# plot calculation faster.
c1 <- makePSOCKcluster(5)
registerDoParallel(c1)
options(scipen=999)
#What I wrote below needs to feed into something,
#so I imported the first entry.
data <- import("202101-divvy-tripdata.csv")

for (i in 2:12) {
  if (i >= 10) {
    path = paste("2021", as.character(i), "-divvy-tripdata.csv", sep="")
    newdata <- import(path) }
  else {
    path = paste("20210", as.character(i), "-divvy-tripdata.csv", sep="")
    newdata <- import(path) }
  data = rbind(data,newdata)}



head(data)
str(data)
glimpse(data)
summary(data)
skim(data)
data = na.omit(data)
data = distinct(data)



data = data %>%
  filter(start_station_id != "" & end_station_name != "" &
           start_station_name != "" & end_station_id != "" ) %>%
  mutate(duration = round(difftime(ended_at, started_at, units = "mins"))) %>%
  mutate(month = month(started_at, label = TRUE)) %>%
  mutate(wday = wday(started_at, label = TRUE)) %>% 
  mutate(hour = hour(started_at)) %>% 
  mutate(route = paste(start_station_name, end_station_name, sep = " to ")) %>% 
  select(-c(start_station_name, end_station_name, ride_id,
            start_station_id, end_station_id, start_lat,
            start_lng, end_lat, end_lng))

data <- as.data.frame(unclass(data), stringsAsFactors = TRUE)
data = data %>% 
  filter(duration > 0) %>%
  arrange(-duration)


data %>%
  group_by(month, member_casual,rideable_type) %>%
  summarize(mean = round(mean(duration)), med = median(duration),
            count = n())

#Percentage of casual and member rides over the last year. Members
#have ridden more times than casuals. Lets break this down further.
#1. Members (55%) has ridden more rides than casuals (45%).

data %>% 
  group_by(member_casual)%>%
  summarize(count = n()) %>%
  mutate(percent = round(count*100/sum(count),0))  %>% 
  ggplot(aes(x = "", y = count, fill = member_casual)) + 
  geom_bar(width = 1, stat = "identity", color = "white", show.legend = FALSE) +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = 
                  paste(member_casual, 
                        paste(percent, "%"), 
                        sep = "\n")), 
            position = position_stack(vjust = 0.6), 
            color = "white") + 
  labs(title = "Percentage of Casual and Member Rides Over the Last Year")+ 
  theme_void()

#Breaking down the data further by bike type. For some reason, there are no 
#Docked bikes used by members.
#2. The most popular bike for members and casuals were classic bikes.

data %>% 
  group_by(member_casual, rideable_type)%>%
  summarize(count = n()) %>%
  mutate(percent = round(count*100/sum(count),0))  %>% 
  ggplot(aes(x = "", y = count, fill = member_casual, rideable_type)) + 
  geom_bar(width = 1, stat = "identity", color = "white", show.legend = TRUE) +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = 
                  paste(rideable_type, 
                        paste(percent, "%"), 
                        sep = "\n")), 
            position = position_stack(vjust = 0.5), 
            color = "white") + 
  labs(title = "Percentage of Casual and Member Rides by Bike Type Over the Last Year")+ 
  theme_void()

#Lets do the same plot but now for percentage of duration of bike rides.
#3. Although classic bikes are the most popular, docked bikes had the 
#greatest duration in the casual member groups. They were barely used in the 
#members group.

data %>%
  group_by(member_casual, rideable_type) %>%
  summarize(dur = mean(duration), count = n()) %>%
  ggplot(aes(x = rideable_type, y = dur, 
             color = member_casual, 
             fill = member_casual))+ 
  geom_bar(stat = "identity") + 
  facet_wrap(~member_casual) + 
  labs (title = "Average Ride Duration Per Bike Type Per Member/Casual", 
        x = "Type of Bike", y = "Average Duration of Ride (min)") + 
  theme_minimal() + 
  theme(axis.text.x = 
          element_text(size = 10))

#Lets explore the number of rides per member/casual per month.
#4. Members rode more times than casuals using classic bikes. Members did
#not use docked bikes. In the summer (Jun to Aug), casual rides were greater than
#member rides. This was mostly driven by classic bikes in the casuals group,
# and partly by docked bikes in the casuals group that was absent in the members
# group.

data %>%
  group_by(month, member_casual) %>%
  summarize(count = n()) %>%
  ggplot +
  aes(x=month, y=count, color = member_casual, group = member_casual) +
  geom_point() + geom_line(size = 1) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  labs (title = "Number of Rides by Month by Casual/Member", 
        x = "Month", y = "Number of Rides") + 
  data %>%
  group_by(month, member_casual,rideable_type) %>%
  summarize(count = n()) %>%
  ggplot +
  aes(x=month, y=count, color = member_casual, group = member_casual) +
  geom_point() + geom_line(size = 1) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  labs (title = "Number of Rides by Month by Casual/Member by Bike Type", 
        x = "Month", y = "Number of Rides") + 
  facet_wrap(~rideable_type)

#Lets explore the number of rides per member/casual per weekday.
#6. Members rode the most times in the middle of the week, being fairly consistent
# throughout the whole week. Casuals rode the most on weekends.

data %>%
  group_by(wday, member_casual) %>%
  summarize(count = n()) %>%
  ggplot +
  aes(x=wday, y=count, color = member_casual, group = member_casual) +
  geom_point() + geom_line(size = 1) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  labs (title = "Number of Rides by Weekday by Casual/Member", 
        x = "Weekday", y = "Number of Rides") + 
  data %>%
  group_by(wday, member_casual,rideable_type) %>%
  summarize(count = n()) %>%
  ggplot +
  aes(x=wday, y=count, color = member_casual, group = member_casual) +
  geom_point() + geom_line(size = 1) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  labs (title = "Number of Rides by Weekday by Casual/Member by Bike Type", 
        x = "Weekday", y = "Number of Rides") + 
  facet_wrap(~rideable_type)

#Lets explore the number of rides per member/casual per hour of the day.
#7. Casuals and members took the most rides 3-8pm.

data %>%
  group_by(hour, member_casual) %>%
  summarize(count = n()) %>%
  ggplot +
  aes(x=hour, y=count, color = member_casual, group = member_casual) +
  geom_point() + geom_line(size = 1) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  labs (title = "Starting time of Rides by Hour of the Day by Casual/Member", 
        x = "Hour of the day", y = "Number of Rides") + 
  data %>%
  group_by(hour, member_casual,rideable_type) %>%
  summarize(count = n()) %>%
  ggplot +
  aes(x=hour, y=count, color = member_casual, group = member_casual) +
  geom_point() + geom_line(size = 1) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  labs (title = "Starting time of Rides by Month by Casual/Member by Bike Type", 
        x = "Hour of the day", y = "Number of Rides") + 
  facet_wrap(~rideable_type)

# Same thing, but with duration

#Lets explore the duration of rides per member/casual per month.
#8. Casual rides on average were about 2x longer (~30min) than members throughout the year (~15min).
# The longest rides for casuals were driven by docked bikes and these long bike rides
# were undertaken with the longest duration during February and July.

data %>%
  group_by(month, member_casual) %>%
  summarize(count = mean(duration)) %>%
  ggplot +
  aes(x=month, y=count, color = member_casual, group = member_casual) +
  geom_point() + geom_line(size = 1) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  labs (title = "Duration of Rides by Month by Casual/Member", 
        x = "Month", y = "Duration of Rides  (min)") + 
  data %>%
  group_by(month, member_casual,rideable_type) %>%
  summarize(count = mean(duration)) %>%
  ggplot +
  aes(x=month, y=count, color = member_casual, group = member_casual) +
  geom_point() + geom_line(size = 1) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  labs (title = "Duration of Rides by Month by Casual/Member", 
        x = "Month", y = "Duration of Rides  (min)") + 
  facet_wrap(~rideable_type)

#Lets explore the duration of rides per member/casual per weekday.
#9. Members were consistent in their duration across the week, casuals had the 
#greatest duration on weekends. Duration of Casual Rides increased to ~35min on weekends from ~30 min.

data %>%
  group_by(wday, member_casual) %>%
  summarize(count = mean(duration)) %>%
  ggplot +
  aes(x=wday, y=count, color = member_casual, group = member_casual) +
  geom_point() + geom_line(size = 1) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  labs (title = "Duration of Rides by Weekday by Casual/Member", 
        x = "Weekday", y = "Duration of Rides (min)") + 
  data %>%
  group_by(wday, member_casual,rideable_type) %>%
  summarize(count = mean(duration)) %>%
  ggplot +
  aes(x=wday, y=count, color = member_casual, group = member_casual) +
  geom_point() + geom_line(size = 1) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  labs (title = "Number of Rides by Weekday by Casual/Member by Bike Type", 
        x = "Weekday", y = "Duration of Rides (min)") + 
  facet_wrap(~rideable_type)

#Lets explore the duration of rides per member/casual per hour of the day.
#10. Casuals took the longest rides starting at 2-4am, being mostly driven by
# docked bike usage. Members were consistent with bike ride duration throughout
# all hours.

data %>%
  group_by(hour, member_casual) %>%
  summarize(count = mean(duration)) %>%
  ggplot +
  aes(x=hour, y=count, color = member_casual, group = member_casual) +
  geom_point() + geom_line(size = 1) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  labs (title = "Starting time of Duration of Rides by Hour of the Day by Casual/Member", 
        x = "Hour of the day", y = "Duration of Rides (min)") + 
  data %>%
  group_by(hour, member_casual,rideable_type) %>%
  summarize(count = mean(duration)) %>%
  ggplot +
  aes(x=hour, y=count, color = member_casual, group = member_casual) +
  geom_point() + geom_line(size = 1) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  labs (title = "Starting time of Duration of Rides by Month by Casual/Member by Bike Type", 
        x = "Hour of the day", y = "Duration of Rides (min)") + 
  facet_wrap(~rideable_type)

##Get the top 10 routes for members and casuals
casual_routes <- data %>%
  filter(member_casual == "casual") %>% 
  group_by(route) %>%
  tally(sort = TRUE)

casual_routes = casual_routes[1:10,]

casual_droutes <- data %>%
  filter(member_casual == "casual") %>% 
  filter(rideable_type == "docked_bike") %>% 
  group_by(route) %>%
  tally(sort = TRUE)

casual_droutes = casual_droutes[1:10,]

member_routes <- data %>%
  filter(member_casual == "member") %>% 
  group_by(route) %>%
  tally(sort = TRUE)

member_routes = member_routes[1:10,]

casual_routes %>% 
  ggplot(aes(x = reorder(route, n),
             y = n)) + 
  geom_bar(stat = "identity", 
           fill = "blue") + 
  xlab("Most Frequently Used Top 10 Routes") + 
  ylab("Number of Rides") +
  coord_flip() + 
  labs(title = "Top 10 Routes for Casual Riders") +
  theme_minimal()

casual_droutes %>% 
  ggplot(aes(x = reorder(route, n),
             y = n)) + 
  geom_bar(stat = "identity", 
           fill = "blue") + 
  xlab("Most Frequently Used Top 10 Routes") + 
  ylab("Number of Rides") +
  coord_flip() + 
  labs(title = "Top 10 Routes for Casual Docked Bike Riders") +
  theme_minimal()

member_routes %>% 
  ggplot(aes(x = reorder(route, n),
             y = n)) + 
  geom_bar(stat = "identity", 
           fill = "blue") + 
  xlab("Most Frequently Used Top 10 Routes") + 
  ylab("Number of Rides") +
  coord_flip() + 
  labs(title = "Top 10 Routes for Member Riders") +
  theme_minimal()
