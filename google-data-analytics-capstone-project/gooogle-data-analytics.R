
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





