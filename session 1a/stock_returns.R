library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(here)
library(tidyquant)
library(rvest)    # scrape websites
library(purrr)  
library(lubridate) #to handle dates
library(janitor)
library(ggrepel)
library(plotly)

djia_url <- "https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average"

#get tables that exist on URL
tables <- djia_url %>% 
  read_html() %>% 
  html_nodes(css="table")


# parse HTML tables into a dataframe called djia. 
# Use purr::map() to create a list of all tables in URL
djia <- map(tables, . %>% 
               html_table(fill=TRUE)%>% 
               clean_names())


# constituents
table1 <- djia[[2]] %>% # the second table on the page contains the ticker symbols
  mutate(date_added = ymd(date_added),
         
         # if a stock is listed on NYSE, its symbol is, e.g., NYSE: MMM
         # We will get prices from yahoo finance which requires just the ticker
         
         # if symbol contains "NYSE*", the * being a wildcard
         # then we jsut drop the first 6 characters in that string
         ticker = ifelse(str_detect(symbol, "NYSE*"),
                          str_sub(symbol,7,11),
                          symbol)
         )

# we need a vector of strings with just the 30 tickers + SPY
tickers <- table1 %>% 
  select(ticker) %>% 
  pull() %>% # pull() gets them as a sting of characters
  c("SPY") # and lets us add SPY, the SP500 ETF



myStocks <- tickers %>% 
  tq_get(get  = "stock.prices",
         from = "2007-01-01",
         to   = Sys.Date()) %>% # today
  group_by(symbol) 

glimpse(myStocks) # examine the structure of the resulting data frame


#calculate daily returns
myStocks_returns_daily <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               type       = "log",
               col_rename = "daily_returns",
               cols = c(nested.col))  

#calculate monthly  returns
myStocks_returns_monthly <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic",
               col_rename = "monthly_returns",
               cols = c(nested.col)) 

# histogram for monthly returns for each stock
myStocks_returns_monthly %>% 
  filter(symbol!= "SPY") %>% 
  ggplot(aes(x= monthly_returns))+
  geom_histogram()+
  facet_wrap(~symbol)+
  theme_bw()+
  scale_x_continuous(
    breaks = seq(-0.5, 0.9, by = 0.25),
    labels = scales::percent_format(accuracy=1))+
  labs(
    title = "Monthly returns of the 30 DJIA stocks",
    subtitle = "Jan 2007 - Aug 2021",
    x=""
  )


# boxplot for monthly returns for each stock
myStocks_returns_monthly %>% 
  filter(symbol!= "SPY") %>% 
  ggplot(aes(x= monthly_returns, y=symbol))+
  geom_boxplot()+
  theme_bw()+
  scale_x_continuous(
    breaks = seq(-0.5, 0.9, by = 0.1),
    labels = scales::percent_format(accuracy=.1))+
  labs(
    title = "Monthly returns of the 30 DJIA stocks",
    subtitle = "Jan 2007 - Aug 2021",
    x=""
  )


# ECDF for monthly returns for each stock
myStocks_returns_monthly %>% 
  filter(symbol!= "SPY") %>% 
  ggplot(aes(x= monthly_returns))+
  stat_ecdf()+
  facet_wrap(~symbol, scales = "free")+
  theme_bw()+
  scale_x_continuous(
    labels = scales::percent_format(accuracy=1))+
  labs(
    title = "Monthly returns of the 30 DJIA stocks",
    subtitle = "Jan 2007 - Aug 2021",
    x="", y= ""
  )+
  theme(legend.position="none")


# Create a dataframe that summarises monthly returns for each of the stocks and `SPY`; 
# min, max, median, mean, SD, and CI for mean
monthly_summaries <- myStocks_returns_monthly %>% 
  group_by(symbol) %>% 
  summarise(
    min = min(monthly_returns),
    max = max(monthly_returns), 
    mean_return = mean(monthly_returns),
    sd_return = sd(monthly_returns),
    count = n(),
    se_return = sd_return / sqrt(count),
    t_critical = qt (0.975, count - 1),
    lower_95 = mean_return - t_critical * se_return,
    upper_95 = mean_return + t_critical * se_return,
  ) %>% 
  arrange(desc(mean_return))

monthly_summaries


# Hey this coding stuff is good, can we do it for all SP500 companies???

sp500_url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

#get tables that exist on URL
tables <- sp500_url %>% 
  read_html() %>% 
  html_nodes(css="table")


# parse HTML tables into a dataframe called djia. 
# Use purr::map() to create a list of all tables in URL
sp500 <- map(tables, . %>% 
              html_table(fill=TRUE)%>% 
              clean_names())


# constituents
table1 <- sp500[[1]] %>% # the first table on the page contains the ticker symbols
  mutate(date_added = ymd(date_first_added))

# we need a vector of strings with just the 30 tickers + SPY
tickers <- table1 %>% 
  select(symbol) %>% 
  pull() %>% # pull() gets them as a sting of characters
  c("SPY") # and lets us add SPY, the SP500 ETF

myStocks <- tickers %>% 
  tq_get(get  = "stock.prices",
         from = "2007-01-01",
         to   = Sys.Date()) %>% # today
  group_by(symbol) 

glimpse(myStocks) # examine the structure of the resulting data frame


#calculate monthly  returns
myStocks_returns_monthly <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic",
               col_rename = "monthly_returns",
               cols = c(nested.col)) 


# Create a dataframe that summarises monthly returns for each of the stocks and `SPY`; 
# min, max, median, mean, SD, and CI for mean
monthly_summaries <- myStocks_returns_monthly %>% 
  group_by(symbol) %>% 
  summarise(
    min = min(monthly_returns),
    max = max(monthly_returns), 
    mean_return = mean(monthly_returns),
    sd_return = sd(monthly_returns),
    count = n(),
    se_return = sd_return / sqrt(count),
    t_critical = qt (0.975, count - 1),
    lower_95 = mean_return - t_critical * se_return,
    upper_95 = mean_return + t_critical * se_return,
  ) %>% 
  arrange(desc(mean_return))

monthly_summaries

sp500_plot <- ggplot(monthly_summaries, aes(x=sd_return, y = mean_return, label=symbol))+
  geom_point(size=1)+
  geom_text_repel(size=4, max.overlaps = 18)+
  theme_bw()+
  scale_x_continuous(
    labels = scales::percent_format(accuracy=1))+
  scale_y_continuous(
    labels = scales::percent_format(accuracy=1))+
  labs(
    title = "Risk-Return for all SP5000 stocks",
    subtitle = "Jan 2007 - Aug 2021",
    x="Monthly Risk (SD)", 
    y= "Monthly Return (Mean)"
  )+
  theme(legend.position="none")

sp500_plot 

ggplotly(sp500_plot)

# Only US Stocks? How about the DAX (German stock market) constituents?

dax_url <- "https://en.wikipedia.org/wiki/DAX"

#get tables that exist on URL
tables <- dax_url %>% 
  read_html() %>% 
  html_nodes(css="table")


# parse HTML tables into a dataframe called djia. 
# Use purr::map() to create a list of all tables in URL
dax <- map(tables, . %>% 
             html_table(fill=TRUE)%>% 
             clean_names())


# constituents
table1 <- dax[[4]] %>% # the fourth table on the page contains the ticker symbols
  select(-1) # drop the first column that contains logo of companies

# we need a vector of strings with just the 30 stocks + DAX index (^GDAXI)
tickers <- table1 %>% 
  select(ticker_symbol) %>% 
  pull() %>% # pull() gets them as a sting of characters
  c("^GDAXI") # and lets us add the actual DAX index, ^GDAXI



myStocks <- tickers %>% 
  tq_get(get  = "stock.prices",
         from = "2007-01-01",
         to   = Sys.Date()) %>% # Sys.Date() returns today's date
  group_by(symbol) 

glimpse(myStocks) # examine the structure of the resulting data frame

#calculate monthly  returns
myStocks_returns_monthly <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic",
               col_rename = "monthly_returns",
               cols = c(nested.col)) 


myStocks_returns_monthly %>% 
  filter(symbol != "^GDAXI") %>% 
  separate(symbol, into = c("symbol","exchange")) %>% 
  ggplot(aes(x = monthly_returns)) +
  geom_density(aes(fill = symbol), alpha = 0.3) +
  geom_histogram(aes(fill = symbol), alpha = 0.4, binwidth = 0.005)+
  facet_wrap(~symbol, nrow=7, scales="free")+
  theme_bw(8)+
  theme(legend.position = "none") +
  scale_x_continuous(labels = scales::percent) +
  labs(
    title = "Distribution of monthly returns for DAX stocks",
    subtitle = "Jan 2007 - Aug 2021",
    x = "Monthly returns (%)",
    y = "" )+
  NULL



# Create a dataframe and assign it to a new object, where you summarise monthly returns 
# since 2017-01-01 for each of the stocks and the index; min, max, median, mean, SD.

monthly_summary <- myStocks_returns_monthly %>% 
group_by(symbol) %>% 
  summarise(
    min = min(monthly_returns),
    max = max(monthly_returns), 
    mean_return = mean(monthly_returns),
    sd_return = sd(monthly_returns),
    count = n(),
    se_return = sd_return / sqrt(count),
    t_critical = qt (0.975, count - 1),
    lower_95 = mean_return - t_critical * se_return,
    upper_95 = mean_return + t_critical * se_return,
  ) %>% 
  arrange(desc(mean_return))


daxindex <- myStocks_returns_monthly %>% 
  filter(symbol == "^GDAXI")%>% 
  mutate(year = year(date),
         month=month(date),
         month_name = month(date, label=TRUE)
  ) 


by_year_monthly <- myStocks_returns_monthly %>% 
  mutate(year = year(date),
         month=month(date),
         month_name = month(date, label=TRUE)
  ) %>% 
  separate(symbol, into = c("symbol","exchange")) %>% 
  select(-exchange)


cols <- c("grey10","tomato")

by_year_monthly %>% 
  bind_rows(daxindex) %>% 
  group_by(year,symbol) %>% 
  filter(year>=2018) %>% 
  summarise(mean_return = mean(monthly_returns, na.rm=TRUE),
            sd_return = sd(monthly_returns, na.rm=TRUE),
  ) %>% 
  mutate(index = ifelse(symbol == "^GDAXI", TRUE, FALSE)) %>% 
  
  ggplot(aes(x=sd_return, y = mean_return))+
  geom_point(aes(color = index))+
  geom_text_repel(aes(label = symbol, color = index), size = 3)+
  geom_hline(yintercept = 0, colour="#001e62", size = 1, linetype = "dashed" )+
  theme_bw()+
  scale_colour_manual(values = cols)+
  facet_wrap(~year,nrow = 2)+
  theme(legend.position = "none")+
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Risk-Return tradeoff for DAX stocks",
    subtitle = "Monthly returns, Jan 2017- Aug 2021",
    x = "Risk (SD of monthly returns)",
    y = "Return (Mean)" )+
  NULL

