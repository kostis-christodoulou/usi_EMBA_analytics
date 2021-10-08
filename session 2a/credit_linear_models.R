library(tidyverse)
library(GGally)
library(skimr)
library(mosaic)
library(ggfortify)
library(car)

# while it's fine to know about working directories, I suggest 
# you learn to use the package 'here' that makes organising files easy
# https://malco.io/2018/11/05/why-should-i-use-the-here-package/
credit <- read_csv(here::here('data', 'credit.csv'))

# ---------------------------
# Model building

favstats(~balance, data=credit)

model1 <- lm(balance~1, data=credit)
summary(model1)

credit %>% 
  select(income, limit, rating, cards, age, education, balance) %>% 
  ggpairs() +
  theme_bw()


# balance vs income and students: is it parallel slopes or interaction?

ggplot(credit, aes(x=income, y=balance, colour=student))+
  geom_point()+
  geom_smooth()

# balance vs income and own: is it parallel slopes or interaction?

ggplot(credit, aes(x=income, y=balance, colour=married))+
  geom_point()+
  geom_smooth()


model1 <- lm(balance ~ rating, data=credit)
mosaic::msummary(model1)

model2 <- lm(balance ~ income, data=credit)
mosaic::msummary(model2)

model3 <- lm(balance ~ income + rating, data=credit)
mosaic::msummary(model3)

model4 <- lm(balance ~ income + rating + limit, data=credit)
mosaic::msummary(model4)

colinear_model <- lm(balance ~ ., data = credit)
mosaic::msummary(colinear_model)
vif(colinear_model)
autoplot(colinear_model)

model5 <- lm(balance ~ . - limit, data=credit)
mosaic::msummary(model5)

autoplot(model5)
vif(model5)


model6 <- lm(balance ~  income + rating +  age +  married, data=credit)
mosaic::msummary(model6)

autoplot(model6)
vif(model6)



