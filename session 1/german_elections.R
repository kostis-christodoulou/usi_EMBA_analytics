library(tidyverse) # ggplot and the usual goodies
library(lubridate) # to handle conversions from characters to date objects
library(zoo) # to calculate rolling  averages of last k polls

# data sourced from https://en.wikipedia.org/wiki/Opinion_polling_for_the_2021_German_federal_election"
# https://www.economist.com/graphic-detail/who-will-succeed-angela-merkel
# https://www.theguardian.com/world/2021/jun/21/german-election-poll-tracker-who-will-be-the-next-chancellor


# while it's fine to know about working directories, I suggest 
# you learn to use the package 'here' that makes organising files easy
# https://malco.io/2018/11/05/why-should-i-use-the-here-package/
german_polls <- read_csv(here::here('data', 'german_polls.csv'))


my_data <- german_polls %>% 
  mutate(
    
    # get month and week number from the date, if we want to do analysis by month- week, etc.
    month = month(end_date, label=TRUE, abbr=TRUE),
    week = isoweek(end_date),
    party = factor(party, 
                        levels = c("CDU/CSU", "SPD", "GRUNE", "FDP", "AFD", "LINKE")
    )) %>% 
  arrange(end_date)


# use colour codes for parties
# source: https://gist.github.com/Fischaela/0cf760f17672e3eb399193e48d7c6104
# even though party colours is not straight-forward... 
# https://blog.datawrapper.de/partycolors/


my_colour_palette = c(
  "#000000", #CDU
  "#E3000F", #SPD
  "#1AA037", #GRUNE
  "#FFEF00", #FDP
  "#0489DB", #AFD
  "#951d7a"  #LINKE
)



ggplot(my_data, aes(x=end_date, y= percent, colour=party))+
  geom_point(alpha=0.25)+
  scale_colour_manual(values=my_colour_palette)+
  geom_smooth(se=F)+
  theme_minimal()+
  scale_x_date(date_minor_breaks = "1 month")+
  labs(
    title = "Opinion polling for the 2021 German federal election",
    subtitle = "Polls since Jan 2021",
    x = "", y = "",
    caption = "Source: https://en.wikipedia.org/wiki/Opinion_polling_for_the_2021_German_federal_election"
    ) +
  NULL


# calculating a rolling average
number_of_polls <- 7

rolling_mean <- my_data %>% 
  group_by(party) %>% 
  
  #  Use the rollmean() function from the zoo package to get a moving average of the last 10 polls
  # The first argument you want to specify is the variable you're averaging, percent in our case.
  # The second is the number of observations of that variable to average together, k=7
    mutate(rolling_average = zoo::rollmean(percent, 
                                 k = number_of_polls, 
                                 fill = NA, 
                                 align = "right"),
           rolling_sd = zoo::rollapply(percent, 
                              FUN=sd,
                              width = number_of_polls, 
                              fill = NA,
                              align = "right"),
           lower = rolling_average - qt(0.975, number_of_polls - 1) * rolling_sd,
           upper = rolling_average + qt(0.975, number_of_polls - 1) * rolling_sd,
  )


ggplot(rolling_mean, aes(x=end_date, y= rolling_average, colour=party, fill=party))+
  geom_point(alpha=0.2)+
  geom_smooth(se=F)+
  geom_ribbon(aes(ymin = rolling_average - 1.96*rolling_sd,
                  ymax = rolling_average + 1.96*rolling_sd),  alpha = 0.1
              )+
  scale_colour_manual(values=my_colour_palette)+
  scale_fill_manual(values=my_colour_palette)+
  theme_minimal()+
  scale_x_date(date_minor_breaks = "1 month")+
  labs(
    title = "Rolling average of last 7 polls for the 2021 German federal election",
    subtitle = "Polls since Jan 2021",
    x = "", y = "",
    caption = "Source: https://en.wikipedia.org/wiki/Opinion_polling_for_the_2021_German_federal_election"
  ) +
  NULL
  
  
