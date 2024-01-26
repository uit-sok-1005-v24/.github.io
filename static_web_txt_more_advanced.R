rm(list=ls())

library(tidyverse)
library(lubridate)
library(janitor)

# web page with data
browseURL("https://www.drroyspencer.com/latest-global-temperatures/")

df_lower <- read_table("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")

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
  geom_line(aes(x = date, y = ma), color = "red", size=2) +
  labs(x = "Year", y = "Temperature anomaly (C)", title = "Lower troposphere temperature anomaly") +
  theme_bw()


###----------------------------------------------------------------------------
# download data from the Mid-Troposphere, and add it to the plot
###----------------------------------------------------------------------------

# create a long data frame from the 4 data sets
# use a common variable name, eg, measurement using the mutate fn.
rm(list=ls())

# one dataframe per link/dataset
df_1 <- read_table("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")
df_2 <- read_table("https://www.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt")
df_3 <- read_table("https://www.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt")
df_4 <- read_table("https://www.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt")

# sub-setting a data frame on rows and columns
df_1 <- df_1[1:which(df_1$Year %in% "Year")-1, c(1:3)]
df_2 <- df_2[1:which(df_2$Year %in% "Year")-1, c(1:3)]
df_3 <- df_3[1:which(df_3$Year %in% "Year")-1, c(1:3)]
df_4 <- df_4[1:which(df_4$Year %in% "Year")-1, c(1:3)]

# creating a new variable identifying the measurement level
df_1 <- df_1 %>% mutate(measurement="Lower_Troposphere")
df_2 <- df_2 %>% mutate(measurement="Mid_Troposphere")
df_3 <- df_3 %>% mutate(measurement="Tropopause")
df_4 <- df_4 %>% mutate(measurement="Lower_Stratosphere")

# browse the data frames
df_1
df_2
df_3
df_4

# compact the data frames
longdf <-
  bind_rows(df_1, df_2, df_3, df_4)

# clean up the column names using janitor   
longdf <-
  longdf %>% clean_names()

# convert the date variables to date format
longdf <- 
  longdf %>%
  mutate(date = ymd(paste(year, mo, 1, sep="-")))

# select the variables we want to keep, and turn globe into numeric
longdf <- 
  longdf %>% 
  select(date, globe, measurement) %>% 
  mutate(globe = as.numeric(globe))

# plot the data
longdf %>%
  ggplot(aes(x = date, y = globe, group = measurement)) + 
  geom_line(aes(color = measurement)) +
  theme_bw()

# or 
longdf %>% 
  ggplot(aes(x = date, y = globe, group = measurement, color = measurement)) +
  geom_line() +
  theme_bw()

# plot the data and add a smoother for each measurement
longdf %>%
  ggplot(aes(x = date, y = globe, group = measurement, color = measurement)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE) +
  theme_bw()

#library(zoo)

# add 12 month moving average
longdf %>%
  group_by(measurement) %>%
  mutate(ma = zoo::rollmean(globe, 12, fill = NA, align = "right")) %>% 
  ggplot(aes(x = date, y = globe, color = measurement)) +
  geom_line(alpha=0.6, linetype = "dashed") +
  geom_line(aes(y = ma, color = measurement), size = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Year", y = "Temperature anomaly (C)", title = "Temperature anomaly with 12-month moving average") +
  theme_bw()


# task: copy figure above, but remove the first observation (dec 1978)
longdf

# task: copy figure above, but only include years 2000:2020

# task: copy figure above, but only include years 2000:2020, and only the Lower_Troposphere and Mid_Troposphere

# task: calculate the average of the four temperatures

# task: calculate the average of the four and plot it in the same plot as the four

# from long to wide as an alternative
longdf %>%
  pivot_wider(names_from = measurement,
              values_from = globe) %>% 
  mutate(average  = (Lower_Troposphere+Mid_Troposphere+ Tropopause+ Lower_Stratosphere)/4,
         average_v2 = rowMeans(select(., -date))) %>% 
  select(-average_v2) %>% 
  pivot_longer(-date, names_to = "location", values_to = "globe") %>%
  ggplot(aes(x=date, y=globe, col=location)) + 
  geom_line() +
  theme_bw()

# making average line different
longdf %>%
  pivot_wider(names_from = measurement, values_from = globe) %>% 
  mutate(average = (Lower_Troposphere + Mid_Troposphere + Tropopause + Lower_Stratosphere) / 4) %>% 
  pivot_longer(-date, names_to = "location", values_to = "globe") %>%
  ggplot(aes(x = date, y = globe, col = location, size = location == "average")) + 
  geom_line() +
  scale_size_manual(values = c(0.5, 2)) +  # Adjust 1 and 2 to your preferred sizes
  theme_bw() +
  guides(size = "none")  # This removes the size legend

# making average alone and with uncertainty
longdf %>%
  pivot_wider(names_from = measurement, values_from = globe) %>% 
  mutate(average = rowMeans(select(., -date), na.rm = TRUE),
         sd = apply(select(., -date), 1, sd, na.rm = TRUE)) %>%
  mutate(se = sd / sqrt(4)) %>%
  select(date, average, se) %>%
  ggplot(aes(x = date, y = average)) +
  geom_line() +
  geom_ribbon(aes(ymin = average - 2*se, ymax = average + 2*se), alpha = 0.2) +
  labs(x = "Date", y = "Average Globe Measurement", title = "Average Measurement with 95% CI") +
  theme_bw()


###----------------------------------------------------------------------------
# Another try
###----------------------------------------------------------------------------

rm(list=ls())

# Here are the url's
# In essence, we want to do the same operations 4 times
# "https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt"
# "https://www.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt"
# "https://www.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt"
# "https://www.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt"
# "Lower-Troposphere"
# "Mid-Troposphere"
# "Tropopause"
# "Lower-Stratosphere"

# what to do to write more general code?
test <- read_table("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt") 
# note that the data is now a "." in the pipe
test <- test %>% .[1:which(.$Year %in% "Year")-1, ] %>% clean_names()

tail(test)

# selecting on columns and mutate
test <- 
  test %>% .[ , 1:3] %>% 
  mutate(date = ymd(paste(year, mo, 1, sep="-"))) %>% 
  select(date, globe) %>% 
  mutate_if(is.character, ~as.numeric(.)) %>%
  mutate(ma=zoo:rollmean(globe, 12, fill=NA, align="right"))

head(test, 15)

rm(test)

# Lets create our own function that does the procedure, even more general, without an object name.
# replace the data object with . in the pipe

scrape <- function(url) {
  return(read_table(url) %>% # read data frame
           .[1:which(.$Year %in% "Year")-1, ] %>% # subsetting on rows
           .[ , 1:3] %>%  # selecting on columns
           clean_names() %>% # cleaning up column names
           mutate(date = ymd(paste(.$year, .$mo, 1, sep="-"))) %>% 
           select(date, globe) %>% 
           mutate_if(is.character, ~as.numeric(.)) %>%
           mutate(ma=zoo::rollmean(globe, 12, fill=NA, align="right")))
}

lt <- scrape("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")
head(lt, 15)

rm(lt, scrape)

# now think how we would like to have the final long data frame
#browseURL("https://www.youtube.com/watch?v=sLF31AY25so")
# need to have a variable identifying the where the temperature was measured

scrape.bake <- function(url, location) {
  return(read_table2(url) %>%
           .[1:which(.$Year %in% "Year")-1, 1:3] %>%
           clean_names() %>%
           mutate(date = ymd(paste(.$year, .$mo, 1, sep="-"))) %>% 
           select(date, globe) %>% 
           mutate_if(is.character, ~as.numeric(.)) %>%
           mutate(ma=zoo::rollmean(globe, 12, fill=NA, align="right"),
           measurement=paste0(location)))
}

# scrape the data
lot <- scrape.bake("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt", "Lower-Troposphere")
mit <- scrape.bake("https://www.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt", "Mid-Troposphere")
trp <- scrape.bake("https://www.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt", "Tropopause")
los <- scrape.bake("https://www.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt", "Lower-Stratosphere")

head(lot, 15)

# bind all the dataframes together 
longdf <- bind_rows(lot,mit,trp,los)

# plot
longdf %>% 
  ggplot(aes(x = date, y = globe, group = measurement, color = measurement)) +
  geom_line() +
  ylab(expression("Temperature ("*~degree*C*")")) +
  xlab("") +
  labs(title = "Global Temperature Development",
       subtitle = "Temperature departure from 1981-2010 average") +
  theme_bw()




