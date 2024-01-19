rm(list=ls())

library(tidyverse)
library(lubridate)
library(janitor)

# web page with data
browseURL("https://www.drroyspencer.com/latest-global-temperatures/")

df_lower <- read_table2("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")

head(df_lower)
tail(df_lower,14)

# sub-setting a data frame on rows
df_lower <- df_lower[1:which(df_lower$Year %in% "Year")-1,]
tail(df_lower)

# cleaning up the column names
names(df_lower)
df_lower <- df_lower %>% clean_names()

# looking at the structure of the data frame
str(df_lower)

# selecting columns
df_lower %>% select(year, mo, globe)
df_lower %>% select(starts_with("ocean"))

df_lower <- df_lower %>% select(year, mo, globe)
df_lower

# turning whole data frame into numeric
df_lower <- df_lower %>% mutate_all(as.numeric)
df_lower

# create a date variable, lubridate
df_lower <-
  df_lower %>% 
  mutate(date = ymd(paste(year, mo, 1)))

df_lower

# compact the data frame
df_lower <- df_lower %>% select(date, globe)

df_lower

# extraxt year from date, base R
#df_lower$year <- year(df_lower$date)

# extraxt year from date, tidyverse
df_lower <- 
  df_lower %>% 
  mutate(year = year(date))

# extract numeric month from date, tidyverse
df_lower <- 
  df_lower %>% 
  mutate(month = month(date))

df_lower

# extract text month from date, tidyverse
df_lower <- 
  df_lower %>% 
  mutate(month = month(date, label = TRUE))

df_lower

# extract full name of month from date, tidyverse
df_lower <- 
  df_lower %>% 
  mutate(month = month(date, label = TRUE, abbr = FALSE))

df_lower

# plot the data
df_lower %>% 
  ggplot(aes(x = date, y = globe)) +
  geom_line() +
  labs(x = "Year", y = "Temperature anomaly (C)", title = "Lower troposphere temperature anomaly") +
  theme_bw() 

# plot the data and add a smoother
df_lower %>% 
  ggplot(aes(x = date, y = globe)) +
  geom_line() +
  geom_smooth() +
  labs(x = "Year", y = "Temperature anomaly (C)", title = "Lower troposphere temperature anomaly") +
  theme_bw()


# generate a 12 month moving average
df_lower <- 
  df_lower %>% 
  mutate(ma = zoo::rollmean(globe, 12, fill = NA, align = "right"))

head(df_lower,14)

# plot the data and add a smoother
df_lower %>% 
  ggplot(aes(x = date, y = globe)) +
  geom_line() +
  geom_line(aes(x = date, y = ma), color = "red") +
  labs(x = "Year", y = "Temperature anomaly (C)", title = "Lower troposphere temperature anomaly") +
  theme_bw()

# download data from the Mid-Troposphere, and add it to the plot

