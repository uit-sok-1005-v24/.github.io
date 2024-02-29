rm(list=ls())

library(tidyverse)
library(gapminder)

# Use the gapminder data-set from the gapminder package
data(gapminder)



# Q1
# select 1952 and 2007 then
# group by the 5 continents and the 2 years,
# calculate mean and median for lifeExp and gdpPercap


# Q2
# in Asia, for each year, what is the min and max lifeExp?


# Q3
# measure percentage changes as log differentials between years,
# what has grown most, the min or max lifeExp in Asia?


# Q4
# for each country, find the average lifeExp and gdp
# make a point plot of each variable (x) against country (y)
# also order the point plot above from small to large (x), against country (y)



# Q5
# for each country, find the average lifeExp and gdp
# make a point plot of the average lifeExp (y) against gdp (x)


# Q6
# for each country, calculate the life expectancy gain in years,
# i.e., each (5) year - first year [1952]


# Q7
# within a continent, what country experienced the sharpest 5-year drop in life expectancy?
# within a country, take (lifeExp in year t) - (lifeExp in year t-1)
# positive means lifeExp went up, negative means it went down


### ------------------------

rm(list=ls())

# Q8 
# Read the following "accounting" dataset dframe:

dframe <- tribble(
  ~fk_account_code, ~Ansvar, ~fk_function_code, ~fk_project_code, ~amount,
  101030,40220,"Det",3432,1493.00,
  101030,40220,"Met",3586,2827.00,
  101030,40320,"Det",3456,49440.00,
  101030,40330,"Sal",NA,870716.00,
  101030,40350,"Met",NA,559928.00,
  101030,40360,"Sal",NA,125534.00,
  101030,40280,"Pol",NA,251611.00)

# 1) Remove the "fk_project_code" variable from dframe.

# 2) Sum the "amount" by the 3 first digits of "Ansvar"; 402, 403, etc.

# 3) Make new "labels" for "fk_function_code", where:
#   "Det" and "Sal" is "supplies",
#   "Met" is "inventories" and
#   "Pol" is "other expenses"

rm(list = ls())

# Q9
# Read the following small dataset:

dframe <- tibble(Product = gl(3,10, labels=c("A","B","C")),
                 Year = factor(rep(2010:2019, 3)),
                 Sales = 1:30)

# Calculate the share of sales per product per year. The sum over the 3 shares per year is 100.
# Make two plots, one of the sales and the other of the shares per year per product.



