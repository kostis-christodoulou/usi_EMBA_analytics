library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(GGally)
library(here)
library(skimr)
library(janitor)
library(broom)
library(huxtable)
library(lubridate)
library(ggfortify)

# Explore the relationship between bikes hired and a bunch of explanatory variables

#read the CSV file
bike <- read_csv(here::here("data", "london_bikes.csv"))

# fix dates using lubridate, and generate new variables for year, month, month_name, day, and day_of _week
bike <- bike %>%   
  mutate(
    year=year(day),
    month = month(day),
    month_name=month(day, label = TRUE),
    day_of_week = wday(day, label = TRUE)) 

# generate new variable season_name to turn seasons from numbers to Winter, Spring, etc
bike <- bike %>%  
  mutate(
    season_name = case_when(
      month_name %in%  c("Dec", "Jan", "Feb")  ~ "Winter",
      month_name %in%  c("Mar", "Apr", "May")  ~ "Spring",
      month_name %in%  c("Jun", "Jul", "Aug")  ~ "Summer",
      month_name %in%  c("Sep", "Oct", "Nov")  ~ "Autumn",
    ),
    season_name = factor(season_name, 
                         levels = c("Winter", "Spring", "Summer", "Autumn"))
  )

# skimr::skim() gives us variable types, summary statistics, missing values
# anything that seems off
skim(bike)

# model building

# start the naive model where you just use the average
favstats(~bikes_hired, data = bike)
# can you create a confidence interval for mean bikes_hired? What is the SE?


model1 <- lm(bikes_hired ~ 1, data= bike)
msummary(model1)

# What is the regression's residual standard error? 
# What is the intercept standard error? 

# build a number of linear regression models where you try to explain
# bikes_hired with (some of) the explanatory variables included in the file.




# produce summary table comparing models using huxtable::huxreg()
