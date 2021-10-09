library(tidyverse)
library(GGally)
library(skimr)
library(mosaic)
library(car)
library(ggfortify)

# while it's fine to know about working directories, I suggest 
# you learn to use the package 'here' that makes organising files easy
# https://malco.io/2018/11/05/why-should-i-use-the-here-package/
winner <- read_csv(here::here('data', 'Winner_for_R.csv'))


# ---------------------- Have a look at the datafile and calculate summary statistics ? ----------------------

winner %>% 
  skim()

model0 <- lm(salary ~ 1, data = winner)
mosaic::msummary(model0)


# ---------------------- 1. Relationship Salary – Gender ? ----------------------
# Can you conclude that there is a significant difference between the salaries of male and female executives?
  
# Summary statistics and some visualisations of salary by gender
mosaic::favstats(salary ~ gender, data = winner)
# there is a mean sample difference of 62577 - 71008 = -8431

ggplot(winner, aes(x=salary, fill = gender))+
  geom_density(alpha = 0.3) +
  theme_minimal()


ggplot(winner, aes(x=salary, y = gender, fill = gender))+
  geom_boxplot(alpha = 0.3)+
  theme_minimal()+
  theme(legend.position = "none")

# t-test of salary by gender
t.test(salary ~ gender, data = winner)

# regression of salary by gender
model1 <- lm(salary ~ gender, data = winner)
mosaic::msummary(model1)


# ---------------------- 2. Relationship Experience – Gender ? ----------------------
# Can you conclude that there is a significant difference between the experience of male and female executives?

# Summary statistics and some visualisations of experience by gender
mosaic::favstats(experience ~ gender, data = winner)
# there is a mean sample difference of 7.38 - 21.13 = -13.75 years of experience

ggplot(winner, aes(x=experience, fill = gender))+
  geom_density(alpha = 0.3) +
  theme_minimal()


ggplot(winner, aes(x=experience, y = gender, fill = gender))+
  geom_boxplot(alpha = 0.3)+
  theme_minimal()+
  theme(legend.position = "none")

# t-test of salary by gender
t.test(experience ~ gender, data = winner)

# regression of salary by gender
model2 <- lm(experience ~ gender, data = winner)
mosaic::msummary(model2)

# ---------------------- 3. Relationship Salary - Experience ? ----------------------

winner %>% 
  select(experience, salary) %>% 
  ggpairs()+
  theme_bw()

# regression of salary by experience
model3 <- lm(salary ~ experience, data = winner)
mosaic::msummary(model3)

# ---------------------- 4. Relationship Salary - Gender- Experience ? ----------------------

winner %>% 
  select(salary, experience, gender) %>% 
  ggpairs(aes(fill=gender, alpha = 0.2))+
  theme_bw()


model4 <- lm(salary ~ experience + gender, data = winner)
mosaic::msummary(model4)
autoplot(model4)
car::vif(model4)

model5 <- lm(salary ~ sqrt(experience) + gender, data = winner)
mosaic::msummary(model5)
autoplot(model5)
car::vif(model5)

model6 <- lm(salary ~ sqrt(experience), data = winner)
mosaic::msummary(model6)
autoplot(model6)
