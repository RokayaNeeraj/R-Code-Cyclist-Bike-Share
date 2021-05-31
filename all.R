#Importing Merged Csv file
all <- read_csv("D:/all.csv")

#Cleaning the data where start time is greater than the end time of the ride
all <- all %>% filter('new$started_at' > 'new$ended_at')

#Cleaning data where start_station_name is "HQ QR" which indicates quality check by the company itself
all <- all[!(all$start_station_name == "HQ QR" | all$ride_length<0),]

#Dropping out the blank columns
all <- all[ -c(11,12,13:14) ]

#Adding new metric, ride_length 
all$ride_length <- difftime(all$ended_at,all$started_at)

#Changing the data type of ride_length from object to integer
all$ride_length <- as.integer(all$ride_length)

#Dropping the columns which have empty values
all <- drop_na(all)

#Viewing the dataframe all
glimpse(all)

#Calculating mean, median, max, min of ride_length
mean(all$ride_length)
median(all$ride_length) 
max(all$ride_length) 
min(all$ride_length) 

#Finding the aggregate of mean,median,max and mean of ride_length in  relation to member_casual group
aggregate(all$ride_length ~ all$member_casual, FUN = mean)
aggregate(all$ride_length ~ all$member_casual, FUN = median)
aggregate(all$ride_length ~ all$member_casual, FUN = max)
aggregate(all$ride_length ~ all$member_casual, FUN = min)
aggregate(all$ride_length ~ all$member_casual + all$day, FUN = mean)

#Summarizing the number of rides, average_duaration
all%>% mutate(day = wday(started_at, label = TRUE)) %>% group_by(member_casual,day) %>%  
  summarise(number_of_rides = n() ,average_duration = mean(ride_length)) %>%  
  arrange(member_casual,day)		
# visualize the number of rides by rider type
all%>% 
  mutate(day = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, day) %>% 
  summarise(number_of_rides = n()  ,average_duration = mean(ride_length)) %>%  arrange(member_casual, day)  %>% 
  ggplot(aes(x = day, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


# Visualization for average duration
all%>% 
  mutate(day = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, day) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day)  %>% 
  ggplot(aes(x = day, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#Summing values to count to make further visualization 
count <- aggregate(all$ride_length ~ all$member_casual + 
          all$day, FUN = mean) 
View(count)

#Exporting count
write.csv(counts,"count.csv")
