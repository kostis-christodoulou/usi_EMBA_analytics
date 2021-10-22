library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(GGally)
library(here)
library(skimr)
library(janitor)
library(broom)
library(huxtable)

# Explore the relationship between birth weight and mother's smoking habit

# import the data set
smoking_birth_weight <- read_csv(here::here("data", "smoking_birth_weight.csv"))

# skimr::skim() gives us variable types, summary statistics, missing values
# anything that seems off
skimr::skim(smoking_birth_weight)

# how many mothers were smokers vs non smokers in our sample?
smoking_birth_weight %>% 
  count(habit, sort=TRUE) %>% 
  mutate(proportion = n/sum(n))

# lowbirthweight is defined as weight_kg < 2.50
# how can we find the probability of lowbirthweight by status (smoking-nonsmoking)
smoking_birth_weight %>% 
  filter(!is.na(habit)) %>% 
  group_by(habit) %>% 

  summarise(total = n(),
            low = sum(lowbirthweight=="low"),
            prop_low = low/total) %>% 


  mutate(se = sqrt(prop_low*(1-prop_low)/total),
         lower_ci = prop_low - 1.96*se,
         upper_ci = prop_low + 1.96*se
         )



# descriptive statistics on weight by smoking habit
favstats(weight_kg ~ habit, data = smoking_birth_weight)
# the mean difference in weight is 144gm (3.244 - 3.100)

# write down, by hand, the two CIs
t.test(weight_kg ~ habit, data = smoking_birth_weight)


smoking_birth_weight %>% 
  filter(!is.na(habit)) %>% 
  select(weight_kg, habit ) %>% 
  ggpairs()+
  theme_bw()

