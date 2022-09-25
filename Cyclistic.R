library(tidyverse)
library(readr)
#1
jan_df <- read_csv("C:/Users/user/Desktop/Case Study/CASESTUDY CYCLING DATA/Cleaned Data/202101-divvy-tripdata_cleaned.csv")
head(jan_df)
#### ordering the days of the week
jan_df$day_of_week1 <- factor(jan_df$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
### Total number of users for each week
jan_df %>%
  ggplot(aes(x = day_of_week1, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Total for each day of week",
       x = "Day of the week", y = "Total")
#2
jan_df %>%
  ggplot(aes(x = day_of_week1, fill = rideable_type)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL BIKE TYPE USAGE FOR EACH DAY OF WEEK",
       x = "Day of the week", y = "Total")

#3
jan_df %>%
  ggplot(aes(x = rideable_type, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL USAGE FOR EACH BIKE TYPE",
       x = "BIKE TYPE", y = "Total")

#4
ggplot(data = jan_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Average ride duration(HRS)")

#5
ggplot(data = jan_df, aes(x = day_of_week1, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR DIFFERENT WEEKDAYS 
       BASED ON CUSTOMER TYPES",
       x = "Week Day", y = "Average ride duration(HRS)")

#6
ggplot(data = jan_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER AVERAGE RIDE DURATION 
       FOR DIFFERENT BIKE TYPES",
       x = "Customer", y = "AVERAGE ride duration(HRS)")

#7
ggplot(data = jan_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Max RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Max ride duration(HRS)")

#8
ggplot(data = jan_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER Max RIDE DURATION FOR 
       DIFFERENT BIKE TYPES",
       x = "Customer", y = "Max ride duration(HRS)")


feb_df <- read_csv("C:/Users/user/Desktop/Case Study/CASESTUDY CYCLING DATA/Cleaned Data/202102-divvy-tripdata_cleaned.csv")
head(feb_df)

#### ordering the days of the week
feb_df$day_of_week2 <- factor(feb_df$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
### Total number of users for each week
feb_df %>%
  ggplot(aes(x = day_of_week2, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Total for each day of week in February",
       x = "Day of the week", y = "Total")
#2
feb_df %>%
  ggplot(aes(x = day_of_week2, fill = rideable_type)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL BIKE TYPE USAGE FOR EACH DAY OF WEEK",
       x = "Day of the week", y = "Total")

#3
feb_df %>%
  ggplot(aes(x = rideable_type, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL USAGE FOR EACH BIKE TYPE",
       x = "BIKE TYPE", y = "Total")


#4
ggplot(data = feb_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Average ride duration(HRS)")


#5
ggplot(data = feb_df, aes(x = day_of_week2, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR DIFFERENT WEEKDAYS 
       BASED ON CUSTOMER TYPES",
       x = "Week Day", y = "Average ride duration(HRS)")

#6
ggplot(data = feb_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER AVERAGE RIDE DURATION 
       FOR DIFFERENT BIKE TYPES",
       x = "Customer", y = "AVERAGE ride duration(HRS)")

#7
ggplot(data = feb_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Max RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Max ride duration(HRS)")

#8
ggplot(data = feb_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER Max RIDE DURATION FOR 
       DIFFERENT BIKE TYPES",
       x = "Customer", y = "Max ride duration(HRS)")


#1
mar_df <- read_csv("C:/Users/user/Desktop/Case Study/CASESTUDY CYCLING DATA/Cleaned Data/202103-divvy-tripdata_cleaned.csv")
head(mar_df)
#### ordering the days of the week
mar_df$day_of_week3 <- factor(mar_df$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
### Total number of users for each week
mar_df %>%
  ggplot(aes(x = day_of_week3, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Total for each day of week",
       x = "Day of the week", y = "Total")
#2
mar_df %>%
  ggplot(aes(x = day_of_week3, fill = rideable_type)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL BIKE TYPE USAGE FOR EACH DAY OF WEEK",
       x = "Day of the week", y = "Total")

#3
mar_df %>%
  ggplot(aes(x = rideable_type, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL USAGE FOR EACH BIKE TYPE",
       x = "BIKE TYPE", y = "Total")

#4
ggplot(data = mar_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Average ride duration(HRS)")

#5
ggplot(data = mar_df, aes(x = day_of_week3, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR DIFFERENT WEEKDAYS 
       BASED ON CUSTOMER TYPES",
       x = "Week Day", y = "Average ride duration(HRS)")

#6
ggplot(data = mar_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER AVERAGE RIDE DURATION 
       FOR DIFFERENT BIKE TYPES",
       x = "Customer", y = "AVERAGE ride duration(HRS)")

#7
ggplot(data = mar_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Max RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Max ride duration(HRS)")

#8
ggplot(data = mar_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER Max RIDE DURATION FOR 
       DIFFERENT BIKE TYPES",
       x = "Customer", y = "Max ride duration(HRS)")


apr_df <- read_csv("C:/Users/user/Desktop/Case Study/CASESTUDY CYCLING DATA/Cleaned Data/202104-divvy-tripdata_cleaned.csv")
head(apr_df)

#### ordering the days of the week
apr_df$day_of_week4 <- factor(apr_df$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
### Total number of users for each week
apr_df %>%
  ggplot(aes(x = day_of_week4, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Total for each day of week",
       x = "Day of the week", y = "Total")
#2
apr_df %>%
  ggplot(aes(x = day_of_week4, fill = rideable_type)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL BIKE TYPE USAGE FOR EACH DAY OF WEEK",
       x = "Day of the week", y = "Total")

#3
apr_df %>%
  ggplot(aes(x = rideable_type, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL USAGE FOR EACH BIKE TYPE",
       x = "BIKE TYPE", y = "Total")


#4
ggplot(data = apr_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Average ride duration(HRS)")


#5
ggplot(data = apr_df, aes(x = day_of_week4, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR DIFFERENT WEEKDAYS 
       BASED ON CUSTOMER TYPES",
       x = "Week Day", y = "Average ride duration(HRS)")

#6
ggplot(data = apr_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER AVERAGE RIDE DURATION 
       FOR DIFFERENT BIKE TYPES",
       x = "Customer", y = "AVERAGE ride duration(HRS)")

#7
ggplot(data = apr_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Max RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Max ride duration(HRS)")

#8
ggplot(data = apr_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER Max RIDE DURATION FOR 
       DIFFERENT BIKE TYPES",
       x = "Customer", y = "Max ride duration(HRS)")


may_df <- read_csv("C:/Users/user/Desktop/Case Study/CASESTUDY CYCLING DATA/Cleaned Data/202105-divvy-tripdata_cleaned.csv")
head(may_df)

#### ordering the days of the week
may_df$day_of_week5 <- factor(may_df$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
### Total number of users for each week
may_df %>%
  ggplot(aes(x = day_of_week5, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Total for each day of week in May",
       x = "Day of the week", y = "Total")
#2
may_df %>%
  ggplot(aes(x = day_of_week5, fill = rideable_type)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL BIKE TYPE USAGE FOR EACH DAY OF WEEK",
       x = "Day of the week", y = "Total")

#3
may_df %>%
  ggplot(aes(x = rideable_type, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL USAGE FOR EACH BIKE TYPE",
       x = "BIKE TYPE", y = "Total")


#4
ggplot(data = may_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Average ride duration(HRS)")


#5
ggplot(data = may_df, aes(x = day_of_week5, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR DIFFERENT WEEKDAYS 
       BASED ON CUSTOMER TYPES",
       x = "Week Day", y = "Average ride duration(HRS)")

#6
ggplot(data = may_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER AVERAGE RIDE DURATION 
       FOR DIFFERENT BIKE TYPES",
       x = "Customer", y = "AVERAGE ride duration(HRS)")

#7
ggplot(data = may_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Max RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Max ride duration(HRS)")

#8
ggplot(data = may_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER Max RIDE DURATION FOR 
       DIFFERENT BIKE TYPES",
       x = "Customer", y = "Max ride duration(HRS)")


#1
jun_df <- read_csv("C:/Users/user/Desktop/Case Study/CASESTUDY CYCLING DATA/Cleaned Data/202106-divvy-tripdata_cleaned.csv")
head(jun_df)
#### ordering the days of the week
jun_df$day_of_week6 <- factor(jun_df$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
### Total number of users for each week
jun_df %>%
  ggplot(aes(x = day_of_week6, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Total for each day of week",
       x = "Day of the week", y = "Total")

#2
jun_df %>%
  ggplot(aes(x = day_of_week6, fill = rideable_type)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL BIKE TYPE USAGE FOR EACH DAY OF WEEK",
       x = "Day of the week", y = "Total")

#3
jun_df %>%
  ggplot(aes(x = rideable_type, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL USAGE FOR EACH BIKE TYPE",
       x = "BIKE TYPE", y = "Total")

#4
ggplot(data = jun_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Average ride duration(HRS)")

#5
ggplot(data = jun_df, aes(x = day_of_week6, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR DIFFERENT WEEKDAYS 
       BASED ON CUSTOMER TYPES",
       x = "Week Day", y = "Average ride duration(HRS)")

#6
ggplot(data = jun_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER AVERAGE RIDE DURATION 
       FOR DIFFERENT BIKE TYPES",
       x = "Customer", y = "AVERAGE ride duration(HRS)")

#7
ggplot(data = jun_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Max RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Max ride duration(HRS)")

#8
ggplot(data = jun_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER Max RIDE DURATION FOR 
       DIFFERENT BIKE TYPES",
       x = "Customer", y = "Max ride duration(HRS)")


jul_df <- read_csv("C:/Users/user/Desktop/Case Study/CASESTUDY CYCLING DATA/Cleaned Data/202107-divvy-tripdata_cleaned.csv")
head(jul_df)

#### ordering the days of the week
jul_df$day_of_week7 <- factor(jul_df$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
### Total number of users for each week
jul_df %>%
  ggplot(aes(x = day_of_week7, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Total for each day of week",
       x = "Day of the week", y = "Total")
#2
jul_df %>%
  ggplot(aes(x = day_of_week7, fill = rideable_type)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL BIKE TYPE USAGE FOR EACH DAY OF WEEK",
       x = "Day of the week", y = "Total")

#3
jul_df %>%
  ggplot(aes(x = rideable_type, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL USAGE FOR EACH BIKE TYPE",
       x = "BIKE TYPE", y = "Total")


#4
ggplot(data = jul_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Average ride duration(HRS)")


#5
ggplot(data = jul_df, aes(x = day_of_week7, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR DIFFERENT WEEKDAYS 
       BASED ON CUSTOMER TYPES",
       x = "Week Day", y = "Average ride duration(HRS)")

#6
ggplot(data = jul_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER AVERAGE RIDE DURATION 
       FOR DIFFERENT BIKE TYPES",
       x = "Customer", y = "AVERAGE ride duration(HRS)")

#7
ggplot(data = jul_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Max RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Max ride duration(HRS)")

#8
ggplot(data = jul_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER Max RIDE DURATION FOR 
       DIFFERENT BIKE TYPES",
       x = "Customer", y = "Max ride duration(HRS)")


aug_df <- read_csv("C:/Users/user/Desktop/Case Study/CASESTUDY CYCLING DATA/Cleaned Data/202108-divvy-tripdata_cleaned.csv")
head(aug_df)

#### ordering the days of the week
aug_df$day_of_week8 <- factor(aug_df$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
### Total number of users for each week
aug_df %>%
  ggplot(aes(x = day_of_week8, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Total for each day of week",
       x = "Day of the week", y = "Total")
#2
aug_df %>%
  ggplot(aes(x = day_of_week8, fill = rideable_type)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL BIKE TYPE USAGE FOR EACH DAY OF WEEK",
       x = "Day of the week", y = "Total")

#3
aug_df %>%
  ggplot(aes(x = rideable_type, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL USAGE FOR EACH BIKE TYPE",
       x = "BIKE TYPE", y = "Total")


#4
ggplot(data = aug_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Average ride duration(HRS)")


#5
ggplot(data = aug_df, aes(x = day_of_week8, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR DIFFERENT WEEKDAYS 
       BASED ON CUSTOMER TYPES",
       x = "Week Day", y = "Average ride duration(HRS)")

#6
ggplot(data = aug_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER AVERAGE RIDE DURATION 
       FOR DIFFERENT BIKE TYPES",
       x = "Customer", y = "AVERAGE ride duration(HRS)")

#7
ggplot(data = aug_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Max RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Max ride duration(HRS)")

#8
ggplot(data = aug_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER Max RIDE DURATION FOR 
       DIFFERENT BIKE TYPES",
       x = "Customer", y = "Max ride duration(HRS)")


#1
sep_df <- read_csv("C:/Users/user/Desktop/Case Study/CASESTUDY CYCLING DATA/Cleaned Data/202109-divvy-tripdata_cleaned.csv")
head(sep_df)
#### ordering the days of the week
sep_df$day_of_week9 <- factor(sep_df$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
### Total number of users for each week
sep_df %>%
  ggplot(aes(x = day_of_week9, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Total for each day of week",
       x = "Day of the week", y = "Total")
#2
sep_df %>%
  ggplot(aes(x = day_of_week9, fill = rideable_type)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL BIKE TYPE USAGE FOR EACH DAY OF WEEK",
       x = "Day of the week", y = "Total")

#3
sep_df %>%
  ggplot(aes(x = rideable_type, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL USAGE FOR EACH BIKE TYPE",
       x = "BIKE TYPE", y = "Total")

#4
ggplot(data = sep_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Average ride duration(HRS)")

#5
ggplot(data = sep_df, aes(x = day_of_week9, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR DIFFERENT WEEKDAYS 
       BASED ON CUSTOMER TYPES",
       x = "Week Day", y = "Average ride duration(HRS)")

#6
ggplot(data = sep_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER AVERAGE RIDE DURATION 
       FOR DIFFERENT BIKE TYPES",
       x = "Customer", y = "AVERAGE ride duration(HRS)")

#7
ggplot(data = sep_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Max RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Max ride duration(HRS)")

#8
ggplot(data = sep_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER Max RIDE DURATION FOR 
       DIFFERENT BIKE TYPES",
       x = "Customer", y = "Max ride duration(HRS)")


oct_df <- read_csv("C:/Users/user/Desktop/Case Study/CASESTUDY CYCLING DATA/Cleaned Data/202110-divvy-tripdata_cleaned.csv")
head(oct_df)

#### ordering the days of the week
oct_df$day_of_week10 <- factor(oct_df$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
### Total number of users for each week
oct_df %>%
  ggplot(aes(x = day_of_week10, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Total for each day of week in October",
       x = "Day of the week", y = "Total")
#2
oct_df %>%
  ggplot(aes(x = day_of_week10, fill = rideable_type)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL BIKE TYPE USAGE FOR EACH DAY OF WEEK",
       x = "Day of the week", y = "Total")

#3
oct_df %>%
  ggplot(aes(x = rideable_type, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL USAGE FOR EACH BIKE TYPE",
       x = "BIKE TYPE", y = "Total")


#4
ggplot(data = oct_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Average ride duration(HRS)")


#5
ggplot(data = oct_df, aes(x = day_of_week10, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR DIFFERENT WEEKDAYS 
       BASED ON CUSTOMER TYPES",
       x = "Week Day", y = "Average ride duration(HRS)")

#6
ggplot(data = oct_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER AVERAGE RIDE DURATION 
       FOR DIFFERENT BIKE TYPES",
       x = "Customer", y = "AVERAGE ride duration(HRS)")

#7
ggplot(data = oct_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Max RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Max ride duration(HRS)")

#8
ggplot(data = oct_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER Max RIDE DURATION FOR 
       DIFFERENT BIKE TYPES",
       x = "Customer", y = "Max ride duration(HRS)")


nov_df <- read_csv("C:/Users/user/Desktop/Case Study/CASESTUDY CYCLING DATA/Cleaned Data/202111-divvy-tripdata_cleaned.csv")
head(nov_df)

#### ordering the days of the week
nov_df$day_of_week11 <- factor(nov_df$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
### Total number of users for each week
nov_df %>%
  ggplot(aes(x = day_of_week11, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Total for each day of week in November",
       x = "Day of the week", y = "Total")
#2
nov_df %>%
  ggplot(aes(x = day_of_week11, fill = rideable_type)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL BIKE TYPE USAGE FOR EACH DAY OF WEEK",
       x = "Day of the week", y = "Total")

#3
nov_df %>%
  ggplot(aes(x = rideable_type, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL USAGE FOR EACH BIKE TYPE",
       x = "BIKE TYPE", y = "Total")


#4
ggplot(data = nov_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Average ride duration(HRS)")


#5
ggplot(data = nov_df, aes(x = day_of_week11, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR DIFFERENT WEEKDAYS 
       BASED ON CUSTOMER TYPES",
       x = "Week Day", y = "Average ride duration(HRS)")

#6
ggplot(data = nov_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER AVERAGE RIDE DURATION 
       FOR DIFFERENT BIKE TYPES",
       x = "Customer", y = "AVERAGE ride duration(HRS)")

#7
ggplot(data = nov_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Max RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Max ride duration(HRS)")

#8
ggplot(data = nov_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER Max RIDE DURATION FOR 
       DIFFERENT BIKE TYPES",
       x = "Customer", y = "Max ride duration(HRS)")


#1
dec_df <- read_csv("C:/Users/user/Desktop/Case Study/CASESTUDY CYCLING DATA/Cleaned Data/202112-divvy-tripdata_cleaned.csv")
head(dec_df)
#### ordering the days of the week
dec_df$day_of_week12 <- factor(dec_df$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
### Total number of users for each week
dec_df %>%
  ggplot(aes(x = day_of_week12, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Total for each day of week",
       x = "Day of the week", y = "Total")
#2
dec_df %>%
  ggplot(aes(x = day_of_week12, fill = rideable_type)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL BIKE TYPE USAGE FOR EACH DAY OF WEEK",
       x = "Day of the week", y = "Total")

#3
dec_df %>%
  ggplot(aes(x = rideable_type, fill = member_casual)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL USAGE FOR EACH BIKE TYPE",
       x = "BIKE TYPE", y = "Total")

#4
ggplot(data = dec_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Average ride duration(HRS)")

#5
ggplot(data = dec_df, aes(x = day_of_week12, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR DIFFERENT WEEKDAYS 
       BASED ON CUSTOMER TYPES",
       x = "Week Day", y = "Average ride duration(HRS)")

#6
ggplot(data = dec_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER AVERAGE RIDE DURATION 
       FOR DIFFERENT BIKE TYPES",
       x = "Customer", y = "AVERAGE ride duration(HRS)")

#7
ggplot(data = dec_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Max RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Max ride duration(HRS)")

#8
ggplot(data = dec_df, aes(x = member_casual, ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  facet_wrap(~rideable_type) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "CUSTOMER Max RIDE DURATION FOR 
       DIFFERENT BIKE TYPES",
       x = "Customer", y = "Max ride duration(HRS)")


#OVER VIEW OF THE ENTIRE YEAR
usertype_df <- read_csv("C:/Users/user/Desktop/Case Study/CASESTUDY CYCLING DATA/Cleaned Data/customer_year_stat.csv")
head(usertype_df)

usertype_df %>%
  ggplot(aes(x = member_casual, total_rides, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "sum",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL PATRONGE FOR CUSTOMER TYPES",
       x = "Customer", y = "Total")

usertype_df %>%
  ggplot(aes(x = member_casual, avg_ride_duration, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "AVERAGE RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Max ride duration(HRS)")

usertype_df %>%
  ggplot(aes(x = member_casual, max_ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "MAX RIDE DURATION FOR CUSTOMER TYPES",
       x = "Customer", y = "Max ride duration(HRS)")

### Ordering the months
usertype_df$ride_month1 <- factor(usertype_df$ride_month, levels = c("January", "February", "March", "April", "May", "June",
                                                                     "July", "August", "September", "October", "November", "December"))
### Total number of users for each month
usertype_df %>%
  ggplot(aes(x = ride_month1,  total_rides, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "sum",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Total for each month",
       x = "Month of Year", y = "Total")

#3
usertype_df %>%
  ggplot(aes(x = ride_month1, avg_ride_duration, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "AVERAGE BIKE DURATION FOR EACH MONTH",
       x = "BIKE TYPE", y = "Avergae duration")



## DAY OF WEEK ANALYSIS FOR THE ENTIRE YEAR
weekday_df <- read_csv("C:/Users/user/Desktop/Case Study/CASESTUDY CYCLING DATA/Cleaned Data/day_of_wee_for _wholeyear.csv")
colnames(weekday_df)

#### ordering the days of the week
weekday_df$day_of_weekt <- factor(weekday_df$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
### Total number of users for each week
weekday_df %>%
  ggplot(aes(x = day_of_weekt, number_of_rides, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "sum",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Total for each Day of Week",
       x = "Day of the Week", y = "Total")

weekday_df %>%
  ggplot(aes(x = day_of_weekt, max_ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Max Ride Duration for each Day of Week",
       x = "Day of the Week", y = "Ride_duration")

### MAX RIDE DURATION WITHOUT DOCKED BIKES
weekday_df %>%
  filter(rideable_type == "classic_bike" | rideable_type == "electric_bike") %>% 
  ggplot(aes(x = day_of_weekt, max_ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Max Ride Duration for each Day of Week",
       x = "Day of the Week", y = "Ride_duration")

View(weekday_df)

weekday_df %>%
  ggplot(aes(x = day_of_weekt, avg_ride_duration, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Avg Ride Duration for each Day of Week",
       x = "Day of the Week", y = "Ride Duration")

### AVERAGE RIDE DURATION  FOR EACH WEEKDAY WITHOUT DOCKED BIKES
weekday_df %>%
  filter(rideable_type == "classic_bike" | rideable_type == "electric_bike") %>% 
  ggplot(aes(x = day_of_weekt, avg_ride_duration, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Avg Ride Duration for each Day of Week",
       x = "Day of the Week", y = "Ride Duration")


weekday_df %>%
  ggplot(aes(x = rideable_type, number_of_rides, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "sum",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Total Usage for Each Bike Type For the Year",
       x = "Bike Type", y = "Total")

weekday_df %>%
  ggplot(aes(x = rideable_type, max_ride_duration_hrs, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "max",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Max Duration for Each Bike Type For the Year",
       x = "Bike Type", y = "Max Duration (HRS)")


weekday_df %>%
  ggplot(aes(x = rideable_type, avg_ride_duration, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean",alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Avg Duration for Each Bike Type For the Year",
       x = "Bike Type", y = "Average Duration (HRS)")


df_ce <- weekday_df %>%
  filter(member_casual == "casual" & rideable_type == "electric_bike")
df_me <- weekday_df %>%
  filter(member_casual == "member" & rideable_type == "electric_bike")
df_cc <- weekday_df %>%
  filter(member_casual == "casual" & rideable_type == "classic_bike")
df_mc <- weekday_df %>%
  filter(member_casual == "member" & rideable_type == "classic_bike")
df_cd <- weekday_df %>%
  filter(member_casual == "casual" & rideable_type == "docked_bike")
df_md <- weekday_df %>%
  filter(member_casual == "member" & rideable_type == "docked_bike")

ggplot() +
  geom_line(data = df_ce, aes(x = day_of_weekt, y = number_of_rides, group = 1, color="casual_electric")) +
  geom_point(data = df_ce, aes(x = day_of_weekt, y = number_of_rides, color="casual_electric")) +
  geom_line(data = df_me, aes(x = day_of_weekt, y = number_of_rides, group = 1, color="member_electric")) +
  geom_point(data = df_me, aes(x = day_of_weekt, y = number_of_rides, color="member_electric")) +
  geom_line(data = df_cc, aes(x = day_of_weekt, y = number_of_rides, group = 1, color="casual_classic")) +
  geom_point(data = df_cc, aes(x = day_of_weekt, y = number_of_rides, color="casual_classic")) +
  geom_line(data = df_mc, aes(x = day_of_weekt, y = number_of_rides, group = 1, color="member_classic")) +
  geom_point(data = df_mc, aes(x = day_of_weekt, y = number_of_rides, color="member_classic")) +
  geom_line(data = df_cd, aes(x = day_of_weekt, y = number_of_rides, group = 1, color="casual_docked")) +
  geom_point(data = df_cd, aes(x = day_of_weekt, y = number_of_rides, color="casual_docked")) +
  geom_point(data = df_md, aes(x = day_of_weekt, y = number_of_rides, color="member_docked")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Total Rides for each Day of Week Per-Bike type",
       x = "Day of the Week", y = "Total Number")

### WEEKLY TRENDS FOR THE ENTIRE YEAR
week_num <- read_csv("C:/Users/user/Desktop/Case Study/CASESTUDY CYCLING DATA/Cleaned Data/weekly_trends_wholeyear.csv")
head(week_num)

df_ce1 <- week_num %>%
  filter(member_casual == "casual" & rideable_type == "electric_bike")
df_me1 <- week_num %>%
  filter(member_casual == "member" & rideable_type == "electric_bike")
df_cc1 <- week_num %>%
  filter(member_casual == "casual" & rideable_type == "classic_bike")
df_mc1 <- week_num %>%
  filter(member_casual == "member" & rideable_type == "classic_bike")
df_cd1 <- week_num %>%
  filter(member_casual == "casual" & rideable_type == "docked_bike")
df_md1 <- week_num %>%
  filter(member_casual == "member" & rideable_type == "docked_bike")

ggplot() +
  geom_line(data = df_ce1, aes(x = week_number, y = number_of_rides, group = 1, color="casual_electric")) +
  geom_point(data = df_ce1, aes(x = week_number, y = number_of_rides, color="casual_electric")) +
  geom_line(data = df_me1, aes(x = week_number, y = number_of_rides, group = 1, color="member_electric")) +
  geom_point(data = df_me1, aes(x = week_number, y = number_of_rides, color="member_electric")) +
  geom_line(data = df_cc1, aes(x = week_number, y = number_of_rides, group = 1, color="casual_classic")) +
  geom_point(data = df_cc1, aes(x = week_number, y = number_of_rides, color="casual_classic")) +
  geom_line(data = df_mc1, aes(x = week_number, y = number_of_rides, group = 1, color="member_classic")) +
  geom_point(data = df_mc1, aes(x = week_number, y = number_of_rides, color="member_classic")) +
  geom_line(data = df_cd1, aes(x = week_number, y = number_of_rides, group = 1, color="casual_docked")) +
  geom_point(data = df_cd1, aes(x = week_number, y = number_of_rides, color="casual_docked")) +
  geom_point(data = df_md1, aes(x = week_number, y = number_of_rides, color="member_docked")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Weekly Total Bike Rides Per-Bike type",
       x = "Week Number", y = "Total Number")


ggplot() +
  geom_line(data = df_ce1, aes(x = week_number, y = avg_ride_duration, group = 1, color="casual_electric")) +
  geom_point(data = df_ce1, aes(x = week_number, y = avg_ride_duration, color="casual_electric")) +
  geom_line(data = df_me1, aes(x = week_number, y = avg_ride_duration, group = 1, color="member_electric")) +
  geom_point(data = df_me1, aes(x = week_number, y = avg_ride_duration, color="member_electric")) +
  geom_line(data = df_cc1, aes(x = week_number, y = avg_ride_duration, group = 1, color="casual_classic")) +
  geom_point(data = df_cc1, aes(x = week_number, y = avg_ride_duration, color="casual_classic")) +
  geom_line(data = df_mc1, aes(x = week_number, y = avg_ride_duration, group = 1, color="member_classic")) +
  geom_point(data = df_mc1, aes(x = week_number, y = avg_ride_duration, color="member_classic")) +
  geom_line(data = df_cd1, aes(x = week_number, y = avg_ride_duration, group = 1, color="casual_docked")) +
  geom_point(data = df_cd1, aes(x = week_number, y = avg_ride_duration, color="casual_docked")) +
  geom_point(data = df_md1, aes(x = week_number, y = avg_ride_duration, color="member_docked")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Average Ride Duration for Each Week Per-Bike type",
       x = "Week Number", y = "Average")


df_y1 <- week_num %>%
  filter(member_casual == "casual")
df_y2 <- week_num %>%
  filter(member_casual == "member")

ggplot() +
  geom_line(data = df_y1, aes(x = week_number, y = number_of_rides, group = 1, color="casual")) +
  geom_point(data = df_y1, aes(x = week_number, y = number_of_rides, color="casual")) +
  geom_line(data = df_y2, aes(x = week_number, y = number_of_rides, group = 1, color="member")) +
  geom_point(data = df_y2, aes(x = week_number, y = number_of_rides, color="member")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOTAL FOR EACH WEEK PER-CUSTOMER TYPE",
       x = "Week Number", y = "Total Number")

