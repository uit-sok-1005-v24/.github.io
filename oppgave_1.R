getwd()

library(tidyverse)
library(janitor)
library(lubridate)

# Artikkel
#browseURL("https://www.aftenposten.no/verden/i/EQ8w1a/forskere-med-varsku-ikke-teoretisk-mulig-aa-stoppe-oppvarmingen-paa-15-grader")

# Data

rm(list=ls())

# -----------------------------------------------------------------
# Met Office Hadley Centre observations datasets
#browseURL("https://www.metoffice.gov.uk/hadobs/hadcrut5/data/HadCRUT.5.0.2.0/download.html")

# Time series are presented as temperature anomalies (deg C) relative to 1961-1990. 
# read csv data from web
hadcrut <- read_csv("https://www.metoffice.gov.uk/hadobs/hadcrut5/data/HadCRUT.5.0.2.0/analysis/diagnostics/HadCRUT.5.0.2.0.analysis.summary_series.global.annual.csv")

# clean column names
hadcrut <- clean_names(hadcrut)

hadcrut_year <-
  hadcrut %>% 
  rename(year = time,
         hadcrut = anomaly_deg_c,
         hadcrut_lower = lower_confidence_limit_2_5_percent,
         hadcrut_upper = upper_confidence_limit_97_5_percent)

hadcrut_year

# -----------------------------------------------------------------
# GISS Surface Temperature Analysis (GISTEMP v4)
#browseURL("https://data.giss.nasa.gov/gistemp/")

# read csv data from web
gistemp <- read_csv("https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv", 
                    skip = 1)

gistemp <- clean_names(gistemp)

# select only year and months
gistemp <- gistemp %>% select(1:13) 

giss_year <- 
  gistemp %>% 
  mutate_if(is.character, as.numeric) %>% 
  pivot_longer(-year, names_to = "month", values_to = "temp") %>%
  group_by(year) %>%
  summarize(giss = mean(temp, na.rm = TRUE),
            SD = sd(temp, na.rm = TRUE),
            N = n()) %>%
  mutate(SE = SD / sqrt(N),
         giss_lower = giss - qt(0.975, df=N-1) * SE,
         giss_upper = giss + qt(0.975, df=N-1) * SE)

giss_year <- giss_year %>% select(year, giss, giss_lower, giss_upper)

giss_year

# -----------------------------------------------------------------

# NOAA Global Surface Temperature (NOAAGlobalTemp v5)
#https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/global/time-series

# Monthly data from 1850
#https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/global/time-series/globe/land_ocean/all/1/1850-2023

noaa  <- read_csv("https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/global/time-series/globe/land_ocean/all/1/1850-2023/data.csv",
                  skip = 4)

noaa <- clean_names(noaa)

# Transform the 'year' column to a date format
noaa$date <- ymd(paste0(noaa$year, "01"))  # Appends '01' to make it 'YYYYMMDD' format

# Extract year and month
noaa$year <- year(noaa$date)
noaa$month <- month(noaa$date)

# Optional: Remove the temporary 'date' column if not needed
noaa$date <- NULL

noaa_year <- 
  noaa %>% 
  group_by(year) %>%
  summarize(noaa = mean(anomaly, na.rm = TRUE),
            SD = sd(anomaly, na.rm = TRUE),
            N = n()) %>%
  mutate(SE = SD / sqrt(N),
         noaa_lower = noaa - qt(0.975, df=N-1) * SE,
         noaa_upper = noaa + qt(0.975, df=N-1) * SE)

noaa_year <- noaa_year %>% select(year, noaa, noaa_lower, noaa_upper)

noaa_year

# -----------------------------------------------------------------

# Berkeley Earth Surface Temperature Study (BEST)
#browseURL("https://berkeleyearth.org/data/")
# BEST data is available in monthly, seasonal, and annual resolutions,

# read csv data from web https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Land_and_Ocean_summary.txt
best <- read_delim("https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Land_and_Ocean_summary.txt",
                 skip = 58, delim = "\\s+", trim_ws = TRUE, col_names = FALSE)

# Assuming the first column contains the combined data
# Use separate() to split the data into multiple columns
# You need to specify the number of columns you expect and their names

best <- 
  best %>%
  separate(col = X1, into = c("year", "best", "best_unc"), sep = "\\s+")

best

# make alle vars in best numeric
best <- best %>% mutate_all(as.numeric)

best_year <- 
  best %>% 
  mutate(best_lower = best - best_unc,
         best_upper = best + best_unc) %>%
  select(year, best, best_lower, best_upper)

best_year

# -----------------------------------------------------------------

rm(best,gistemp,hadcrut,noaa)

dframe <- 
  best_year %>% 
  left_join(giss_year) %>% 
  left_join(hadcrut_year) %>% 
  left_join(noaa_year)

dframe

full_join(best_year, giss_year, by = "year") %>% 
  full_join(hadcrut_year, by = "year") %>% 
  full_join(noaa_year, by = "year")

dframe %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = best), color = "red") +
  geom_ribbon(aes(ymin = best_lower, ymax = best_upper), fill = "red", alpha = 0.2) +
  geom_line(aes(y = giss), color = "blue") +
  geom_ribbon(aes(ymin = giss_lower, ymax = giss_upper), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = hadcrut), color = "green") +
  geom_ribbon(aes(ymin = hadcrut_lower, ymax = hadcrut_upper), fill = "green", alpha = 0.2) +
  geom_line(aes(y = noaa), color = "orange") +
  geom_ribbon(aes(ymin = noaa_lower, ymax = noaa_upper), fill = "orange", alpha = 0.2) +
  labs(title = "Global surface temperature anomalies",
       subtitle = "Annual means relative to 1961-1990",
       y = "Temperature anomaly (°C)",
       x = "Year",
       caption = "Data sources: BEST, GISS, HadCRUT, NOAA") +
  theme_bw()

# add a label per series
dframe %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = best), color = "red") +
  geom_line(aes(y = giss), color = "blue") +
  geom_line(aes(y = hadcrut), color = "green") +
  geom_line(aes(y = noaa), color = "orange") +
  geom_text(aes(x = 1950, y = 1.5, label = "BEST"), color = "red") +
  geom_text(aes(x = 1950, y = 1.3, label = "GISS"), color = "blue") +
  geom_text(aes(x = 1950, y = 1.1, label = "HadCRUT"), color = "green") +
  geom_text(aes(x = 1950, y = 0.9, label = "NOAA"), color = "orange") +
  labs(title = "Global surface temperature anomalies",
       y = "Temperature anomaly (°C)",
       x = "Year",
       caption = "Data sources: BEST, GISS, HadCRUT, NOAA") +
  theme_bw()  

dframe %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = best), color = "red") +
  geom_line(aes(y = giss), color = "blue") +
  geom_line(aes(y = hadcrut), color = "green") +
  geom_line(aes(y = noaa), color = "orange") +
  labs(title = "Yearly global surface temperature anomalies",
       y = "Temperature anomaly (°C)",
       x = "Year",
       caption = "Data sources: BEST, GISS, HadCRUT, NOAA") +
  theme_bw()
