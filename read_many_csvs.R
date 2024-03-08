#---------------------------
# Reading many csv files 
# inspired by
#browseURL("https://www.gerkelab.com/blog/2018/09/import-directory-csv-purrr-readr/")
# The goal is to import many csv files into a single data frame
#---------------------------

rm(list = ls())

# Load the libraries
library(tidyverse)  
library(fs)
library(janitor)

# For more information about the fs package, see
browseURL("https://fs.r-lib.org/")

# Set the working directory
getwd()
setwd("C:/data/many_csv_files/")

# The location of the unzipped folder in data_dir
data_dir <- "C:/data/many_csv_files"

# List the csv files 
dir()
dir_ls(data_dir)

# Only csv files 
csv_files <- dir_ls(data_dir, regexp = "\\.csv$")
csv_files

# Importing one file, the first 
test <- read_csv(csv_files[1])
head(test)
test <- read_csv(csv_files[1]) %>% clean_names()
head(test)
str(test)
rm(test)

# Importing all the files using map_dfr()
csv_files %>% 
  map_dfr(read_csv) %>% 
  clean_names()

# Modify the settings of read_csv() inside the call to map_dfr()
# Notice that the Month_Year column was imported as a character instead of a date
browseURL("https://www.statmethods.net/data-input/dates.html")

csv_files %>% 
  map_dfr(read_csv, col_types = cols("Month_Year" = col_date(format = "%b-%y"))) 

# View the data
csv_files %>% 
  map_dfr(read_csv, col_types = cols("Month_Year" = col_date(format = "%b-%y"))) %>% View()

# Why are some of the dates NA's?
# Look at the format of the first date (i.e., Aug-15) and last one (21-Nov)
csv_files %>% 
  map_dfr(read_csv) %>% View()

# Fix date parsing 
csv_files %>% 
  map_dfr(read_csv) %>% 
  mutate(date_tmp = my(Month_Year)) %>% # lubridate::my() is a function that parses date in the format "mon-yy"
  mutate(date = if_else(is.na(date_tmp), ym(Month_Year), date_tmp)) # lubridate::ym() is a function that parses date in the format "yy-mon"

# Remove date_tmp and check for NAs
csv_files %>% 
  map_dfr(read_csv) %>% 
  mutate(date_tmp = my(Month_Year)) %>% 
  mutate(date = if_else(is.na(date_tmp), ym(Month_Year), date_tmp)) %>% 
  select(-date_tmp) %>% 
  summarise(across(everything(), ~sum(is.na(.)))) # check for NA's

# Add a source indicator 
csv_files %>% 
  map_dfr(read_csv, .id = "source") %>% 
  mutate(date_tmp = my(Month_Year)) %>% 
  mutate(date = if_else(is.na(date_tmp), ym(Month_Year), date_tmp)) %>% 
  select(-date_tmp)

# Here’s the code we used, all in one
dframe <- 
  data_dir %>% 
  dir_ls(regexp = "\\.csv$") %>% 
  map_dfr(read_csv, .id = "source") %>% 
  mutate(date_tmp = my(Month_Year)) %>% 
  mutate(date = if_else(is.na(date_tmp), ym(Month_Year), date_tmp)) %>%
  clean_names() %>%
  select(-date_tmp)
  
# Reorder the columns
dframe <- 
  dframe %>% 
  select(date, everything())

# View the data
dframe


# ----------------------------------------------------------------------------
# Could also have used vroom package, see
browseURL("https://cran.r-project.org/web/packages/vroom/vignettes/vroom.html")

# Parallel processing, large volumes of data/files
browseURL("https://www.futureverse.org/")

# Manage projects folders/files etc.
browseURL("https://here.r-lib.org/index.html")
