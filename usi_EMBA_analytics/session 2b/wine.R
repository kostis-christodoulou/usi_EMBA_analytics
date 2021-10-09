library(tidyverse)
library(mosaic)
library(broom)
library(skimr)
library(here)
library(GGally)
library(ggfortify)
library(huxtable)
options(scipen=999, show.signif.stars=FALSE, digits=4)


wine <- read_csv(here("data", "wine1.csv"))

skimr::skim(wine)

# scatterplot matrix

wine %>% 
  select(-year) %>% 
  ggpairs()


# Regression models
model0 <- lm(price ~ 1, data=wine)
mosaic::msummary(model0)

model1 <- lm(price ~ AGST, data=wine)
mosaic::msummary(model1)
anova(model1)


model2 <- lm(price  ~ AGST + harvest_rain, data=wine)
msummary(model2)
anova(model2)


model3 <- lm(price  ~ ., data=wine)
msummary(model3)

model4 <- lm(price  ~ . -age, data=wine)
msummary(model4)

model5 <- lm(price  ~ . -France_Population, data=wine)
msummary(model5)

huxreg(model0, model1, model2, model3, model4,model5,
       statistics = c('#observations' = 'nobs', 
                      'R squared' = 'r.squared', 
                      'Adj. R Squared' = 'adj.r.squared', 
                      'Residual SE' = 'sigma'), 
       bold_signif = 0.05, 
       stars = NULL
) %>% 
  set_caption('Comparison of models')
