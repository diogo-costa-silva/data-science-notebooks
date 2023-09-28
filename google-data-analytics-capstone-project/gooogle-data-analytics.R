setwd("/Users/diogosilva/Documents/GitHub/data-science-notebooks/google-data-analytics-capstone-project")
getwd()

# Sys.setenv(PKG_CONFIG_PATH = "/usr/local/Cellar/freetype/2.13.2/lib/pkgconfig/freetype2.pc")
Sys.setenv(PKG_CONFIG_PATH = "/usr/local/Cellar/freetype/2.13.2/lib/pkgconfig/")


# Setting Up the Environment
# (MacOS) brew install freetype

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
daily_activity <- read_csv("dailyActivity_merged.csv")
sleep <- read_csv("sleepDay_merged.csv")
weight <- read_csv("weightLogInfo_merged.csv")

#Viewing the datasets
view(daily_activity)
view(sleep)
view(weight)