# interest rates in Turkey
browseURL("https://e24.no/internasjonal-oekonomi/i/P4MA0z/tyrkia-setter-opp-renten-til-45-prosent")

# load necessary libraries
library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)

# interest rates for banks’ loans
browseURL("https://www.tcmb.gov.tr/wps/wcm/connect/EN/TCMB+EN/Main+Menu/Statistics/Interest+Rate+Statistics/Weighted+Average+Interest+Rates+For+Banks+Loans/")

# Data - Transpose - Export to Excel - Save as EVDS.xlsx

# import data
# https://here.r-lib.org/articles/here.html
getwd()
setwd("C:/Users/omy000/OneDrive - UiT Office 365/Hdisk/omy000 (homer.uit.no)/undervisning/SOK1005/v24")
dir()

intrate <- read_excel("EVDS.xlsx",
                      sheet = "Data", range = "A1:K1152")

str(intrate)

# clean names
intrate <- intrate %>% clean_names()

# data wrangling
intrate %>% 
  select(date, tp_ktf10) %>%
  mutate(date = dmy(date)) %>% 
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarize(i_rate = mean(tp_ktf10))

# data wrangling & plot
intrate %>% 
  select(date, tp_ktf10) %>%
  mutate(date = dmy(date)) %>% 
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarize(i_rate = mean(tp_ktf10)) %>% 
  ggplot(aes(x=date, y=i_rate)) + 
  geom_line() +
  theme_bw()

#' i artikkelen står det:
#' "Etter Erdogans valgseier i slutten av mai endret ting seg derimot.
#' Renten har blitt satt kraftig opp siden Hafize Gaye Erkan kom inn som sentralbanksjef i juni i fjor."

# make a plot with a vertical bar on june 2023
intrate %>% 
  select(date, tp_ktf10) %>%
  mutate(date = dmy(date)) %>% 
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarize(i_rate = mean(tp_ktf10)) %>% 
  ggplot(aes(x=date, y=i_rate)) + 
  geom_line() +
  geom_vline(xintercept = as.numeric(ymd("2023-06-01")), linetype="dashed", color="red", linewidth = 1) +
  theme_bw()

