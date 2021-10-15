library(tidyverse)
library(skimr)
library(mosaic)

# while it's fine to know about working directories, I suggest 
# you learn to use the package 'here' that makes organising files easy
# https://malco.io/2018/11/05/why-should-i-use-the-here-package/
survey <- read_csv(here::here('data', 'early_careers_survey.csv'))



# Histogram on stress about analytics (stress_analytics) and day-to-day stress (stress_day)
ggplot(survey, aes (x=stress_analytics))+
  geom_histogram(fill="#001e62", alpha = 0.88, boundary=1)+
  theme_minimal()+
  geom_vline(xintercept = 1.5, colour="orange", size = 3, linetype = "dashed")+
  labs(
    title = "Anxiety about analytics",
    subtitle = "1: No anxiety - 5: Very high anxiety",
    x="",
    caption = "Source: LBS Early Careers Analytics Survey"
  )


ggplot(survey, aes (x=stress_day))+
  geom_histogram(fill="tomato", alpha = 0.88, boundary=1)+
  theme_minimal()+
  labs(
    title = "How stressful is your day to day life?",
    subtitle = "1: No stress - 5: Very High Stress",
    x="",
    caption = "Source: LBS Early Careers Analytics Survey"
  )


# summary stats on measures of stress

survey %>% 
  select(stress_analytics, stress_day ) %>% 
  skim()


# Is there a difference in stress_analytics between men and women?

mosaic::favstats(stress_analytics ~ gender, data = survey)

ggplot(survey, aes (x=stress_analytics, y = gender))+
  geom_boxplot()+
  theme_minimal()+
  NULL


# Is there a difference in day-to-day stress between men and women?

mosaic::favstats(stress_day ~ gender, data = survey)

ggplot(survey, aes (x=stress_day, y = gender))+
  geom_boxplot()+
  theme_minimal()+
  NULL


# Histogram on handedness
ggplot(survey, aes (x=handedness))+
  geom_histogram(bins = 20,fill="#001e62", alpha = .87, boundary=1)+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-1,1,by = 0.2))+
  labs(
    title = "Handedness",
    subtitle = "-1: Exclusively left handed, +1: Exclusively right handed",
    x="",
    caption = "LBS Early Careers Analytics Survey"
  )


# --------------------- HAIRCUT SPEND ------------------------
# summary statistics of haircut 
favstats(~last_haircut, data=survey)

# summary statistics oflast_haircut_spend vs. student
mosaic::favstats(last_haircut ~ gender, data = survey)

ggplot(survey, aes (x=last_haircut, y = gender))+
  geom_boxplot()+
  theme_minimal()+
  NULL

ggplot(survey, aes (x=last_haircut, fill = gender))+
  geom_density(alpha = 0.3)+
  theme_minimal()+
  NULL

t.test(last_haircut ~ gender, data = survey)

# --------------------- EXERcISE HOURS ------------------------
# summary statistics 
favstats(~exercise_hrs, data=survey)

# summary statistics by gender
mosaic::favstats(exercise_hrs ~ gender, data = survey)

ggplot(survey, aes (x=exercise_hrs, y = gender))+
  geom_boxplot()+
  theme_minimal()+
  NULL

ggplot(survey, aes (x=exercise_hrs, fill = gender))+
  geom_density(alpha = 0.3)+
  theme_minimal()+
  NULL

t.test(exercise_hrs ~ gender, data = survey)

# --------------------- ONLINE HOURS ------------------------
# summary statistics 
favstats(~online_hrs, data=survey)

# summary statistics by gender
mosaic::favstats(online_hrs ~ gender, data = survey)

ggplot(survey, aes (x=online_hrs, y = gender))+
  geom_boxplot()+
  theme_minimal()+
  NULL

ggplot(survey, aes (x=online_hrs, fill = gender))+
  geom_density(alpha = 0.3)+
  theme_minimal()+
  NULL

t.test(online_hrs ~ gender, data = survey)

# --------------------- SLEEP HOURS ------------------------
# summary statistics 
favstats(~sleep_hours, data=survey)

# summary statistics by gender
mosaic::favstats(sleep_hours ~ gender, data = survey)

ggplot(survey, aes (x=sleep_hours, y = gender))+
  geom_boxplot()+
  theme_minimal()+
  NULL

ggplot(survey, aes (x=sleep_hours, fill = gender))+
  geom_density(alpha = 0.3)+
  theme_minimal()+
  NULL

t.test(sleep_hours ~ gender, data = survey)


# --------------------- FACEBOOK FRIENDS  ------------------------
# summary statistics 
favstats(~facebook_friends, data=survey)

# summary statistics by gender
mosaic::favstats(facebook_friends ~ gender, data = survey)

ggplot(survey, aes (x=facebook_friends, y = gender))+
  geom_boxplot()+
  theme_minimal()+
  NULL

ggplot(survey, aes (x=facebook_friends, fill = gender))+
  geom_density(alpha = 0.3)+
  theme_minimal()+
  NULL

t.test(facebook_friends ~ gender, data = survey)


# --------------------- MOTIVATION  ------------------------
# summary statistics 
favstats(~motivation, data=survey)

# summary statistics by gender
mosaic::favstats(motivation ~ gender, data = survey)

ggplot(survey, aes (x=motivation, y = gender))+
  geom_boxplot()+
  theme_minimal()+
  NULL

ggplot(survey, aes (x=motivation, fill = gender))+
  geom_density(alpha = 0.3)+
  theme_minimal()+
  NULL

t.test(motivation ~ gender, data = survey)

# --------------------- STRESS DURING THE DAY  ------------------------
# summary statistics 
favstats(~stress_day, data=survey)

# summary statistics by gender
mosaic::favstats(stress_day ~ gender, data = survey)

ggplot(survey, aes (x=stress_day, y = gender))+
  geom_boxplot()+
  theme_minimal()+
  NULL

ggplot(survey, aes (x=stress_day, fill = gender))+
  geom_density(alpha = 0.3)+
  theme_minimal()+
  NULL

t.test(stress_day ~ gender, data = survey)

# --------------------- STRESS ANALYTICS  ------------------------
# summary statistics 
favstats(~stress_analytics, data=survey)

# summary statistics by gender
mosaic::favstats(stress_analytics ~ gender, data = survey)

ggplot(survey, aes (x=stress_analytics, y = gender))+
  geom_boxplot()+
  theme_minimal()+
  NULL

ggplot(survey, aes (x=stress_analytics, fill = gender))+
  geom_density(alpha = 0.3)+
  theme_minimal()+
  NULL

t.test(stress_analytics ~ gender, data = survey)


# --------------------- HOMEOPATHY  ------------------------
# summary statistics 
favstats(~homeopathy_works, data=survey)

# summary statistics by gender
mosaic::favstats(homeopathy_works ~ gender, data = survey)

t.test(homeopathy_works ~ gender, data = survey)


# --------------------- LIED ABOUT AGE  ------------------------
# summary statistics 
favstats(~lied_about_age, data=survey)

# summary statistics by gender
mosaic::favstats(lied_about_age ~ gender, data = survey)

t.test(lied_about_age ~ gender, data = survey)


# --------------------- MARIJUANA  ------------------------
# summary statistics 
favstats(~marijuana, data=survey)

# summary statistics by gender
mosaic::favstats(marijuana ~ gender, data = survey)

t.test(marijuana ~ gender, data = survey)





