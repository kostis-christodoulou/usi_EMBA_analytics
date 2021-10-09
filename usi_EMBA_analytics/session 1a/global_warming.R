library(tidyverse)
library(GGally)
library(skimr)
library(mosaic)

# while it's fine to know about working directories, I suggest 
# you learn to use the package 'here' that makes organising files easy
# https://malco.io/2018/11/05/why-should-i-use-the-here-package/

global_warming_data <- read_csv(here::here('data', 'global_warming_data.csv'))


# summary statistics of delta vs. interval
mosaic::favstats(delta ~ interval, data = global_warming_data)

# hockeystick plot
ggplot(global_warming_data, aes(x=date, y = delta))+
  geom_point()+
  geom_smooth(color="red") +
  theme_bw() +
  labs (
    title = "Weather Anomalies"
  )

ggplot(global_warming_data, aes(x=delta, fill=interval))+
  geom_density(alpha=0.2) +   #density plot with transparency set to 20%
  theme_bw() +                #theme
  labs (
    title = "Density Plot for Monthly Temperature Anomalies",
    y     = "Density"         #changing y-axis label to sentence case
  )

