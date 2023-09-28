
#################################  Setting Up the Environment  ################################# 

setwd("/Users/diogosilva/Documents/GitHub/data-science-notebooks/google-data-analytics-capstone-project")
getwd()

# Sys.setenv(PKG_CONFIG_PATH = "/usr/local/Cellar/freetype/2.13.2/lib/pkgconfig/freetype2.pc")
Sys.setenv(PKG_CONFIG_PATH = "/usr/local/Cellar/freetype/2.13.2/lib/pkgconfig/")

# (MacOS) brew install freetype
# (MacOS) brew install libpng

#Install packages
install.packages("systemfonts")
install.packages("textshaping")
install.packages("ragg")


install.packages("tidyverse")
install.packages("ggplot2")
install.packages("skimr")
install.packages("janitor")


#Loading packages
library(tidyverse)
library(ggplot2)
library(skimr)
library(janitor)


#Loading datasets
daily_activity <- read_csv("data/dailyActivity_merged.csv")
sleep <- read_csv("data/sleepDay_merged.csv")
weight <- read_csv("data/weightLogInfo_merged.csv")


#Viewing the datasets
view(daily_activity)
view(sleep)
view(weight)






#################################  Data Cleaning and Transformation  ################################# 

#Remove duplicates
daily_activity[!duplicated(daily_activity),]
sleep[!duplicated(sleep), ]
weight[!duplicated(weight), ]

#Checking data summary
skim_without_charts(daily_activity)
skim_without_charts(sleep)
skim_without_charts(weight)

#Converting Date to date format and renaming it to day
daily_activity <- daily_activity %>% 
  mutate_at(vars(ActivityDate), as.Date, format = "%m/%d/%y") %>% 
  rename("Day" = "ActivityDate")

sleep <- sleep %>% 
  mutate_at(vars(SleepDay), as.Date, format = "%m/%d/%y") %>% 
  rename("Day" = "SleepDay")

weight <- weight %>%
  mutate_at(vars(Date), as.Date,format = "%m/%d/%y") %>%
  rename("Day" = "Date")

#Merging the three datasets
Merged<-sleep %>%
  right_join(daily_activity, by=c("Id","Day")) %>% 
  left_join(weight, by=c("Id", "Day")) %>%
  mutate(Weekday = weekdays(as.Date(Day,"m/%d/%Y")))

#Creating "BMI Weight Range" column
Merged <- Merged %>% 
  mutate(BMIRange = case_when(BMI >= 30 ~ "Obese", BMI >= 25 ~ "Healthy", TRUE ~ "Underweight"))

view(Merged)




#################################  4. ANALYZE  ################################# 

#Data summary
Merged <- Merged %>%
  select(Id, Day,TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed, TotalSteps, TotalDistance,VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes, Calories, WeightKg, BMI, Weekday)
summary(Merged)


## Average Steps and Calories for Each Day of the Week
aggregate(cbind(TotalSteps,Calories) ~ Weekday, Merged, mean) %>% 
  arrange(TotalSteps)


## Average Daily Awake Time in Bed
Merged <-  Merged %>% 
  mutate(AwakeTimeInBed = TotalTimeInBed - TotalMinutesAsleep)
aggregate(cbind(TotalMinutesAsleep, TotalTimeInBed,AwakeTimeInBed)~ Weekday, Merged, mean) %>% 
  arrange(AwakeTimeInBed)


## Average Minutes for Each Activity Category
Merged %>% 
  group_by(Id) %>% 
  summarise(AvgVeryActive = mean(VeryActiveMinutes), AvgFairlyActive = mean(FairlyActiveMinutes), AvgLightlyActive = mean(LightlyActiveMinutes), AvgSedentary = mean(SedentaryMinutes)) %>% 
  arrange(AvgVeryActive) %>%  
  print(n = 33)






#################################  5. SHARE  ################################# 

## Visualizing Total Steps and Calories

ggplot(data = Merged) + 
  geom_point(mapping = aes(x= TotalSteps, y= Calories, color= Calories)) + 
  labs(x= "Steps Taken", y= "Calories Burned", title = "Calories Burned for Every Step Taken")


## Visualizing Total Steps Taken by Day of the Week

ggplot(data = Merged, aes(x= fct_infreq(Weekday), y= TotalSteps)) + 
  geom_bar(stat = "identity", fill= "brown") + 
  labs(x= "Weekday", y= "Steps Taken", title = "Total Steps by Day of the Week")

AvgStep <- Merged %>% 
  group_by(Weekday) %>% 
  summarise(AverageStep = mean(TotalSteps)) %>% 
  select(Weekday, AverageStep)
ggplot(data = AvgStep, aes(x= Weekday, y= AverageStep)) + 
  geom_bar(stat = "identity", fill= "brown") + 
  labs(x= "Weekday", y= "Average Steps", title = "Average Steps by Weekday")


## Visualizing Total Minutes Asleep and Total Time in Bed

ggplot(data = Merged) + 
  geom_point(mapping = aes(x= TotalMinutesAsleep, y=TotalTimeInBed, color= TotalTimeInBed)) + 
  labs(x= "Total Minutes Asleep", y= "Total Time in Bed", title = "Total Minutes Asleep Vs Total Time in Bed")


## Visualizing BMI and Total Steps

ggplot(data = Merged) + 
  geom_point(mapping = aes(x= BMI, y=TotalSteps, color= BMIRange, shape= BMIRange, size= TotalSteps)) + 
  labs(x= "BMI", y= "Total Steps", title = "BMI Vs Total Steps") +
  facet_wrap(~BMIRange)


## Visualizing Total Distance and Total Steps


## Visualizing Activity Categories and Calories Burned

ggplot(Merged, aes(x= Calories, y= VeryActiveMinutes/60, color= Calories)) + 
  geom_point() + 
  geom_smooth(method = "loess", color= "brown") +
  labs(y="Very Active Time(H)", x= "Calories Burned", title = "Very Active Time Vs Calories Burned")