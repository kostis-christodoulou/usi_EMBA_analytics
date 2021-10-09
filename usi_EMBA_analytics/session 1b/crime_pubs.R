library(tidyverse)
library(GGally)
library(skimr)
library(mosaic)
library(car)
library(ggfortify)

# while it's fine to know about working directories, I suggest 
# you learn to use the package 'here' that makes organising files easy
# https://malco.io/2018/11/05/why-should-i-use-the-here-package/
crime <- read_csv(here::here('data', 'beerhall.csv'))

crime %>% 
  select(criminals, public_houses, school_attendance, worship_attendance) %>% 
  ggpairs()+
  theme_bw()


model1 <- lm(criminals ~ 1, data = crime)
mosaic::msummary(model1)

model2 <- lm(criminals ~ public_houses, data = crime)
mosaic::msummary(model2)

model3 <- lm(criminals ~ public_houses +  school_attendance + worship_attendance, data= crime)
mosaic::msummary(model3)
autoplot(model3)


model4 <- lm(criminals ~ public_houses +  school_attendance, data = crime)
mosaic::msummary(model4)
autoplot(model4)

