
#
#
# Data wrangling: Some questions for the class tomorrow  
#
# Learning outcomes:
#learn more on dplyr functions such as group_by, summarise, etc and ggplot2 

rm (list = ls())
library(tidyverse)
library(lubridate)
library(janitor)

superstore <- read_csv("https://github.com/ywchiu/rcookbook/raw/master/chapter7/superstore_sales.csv", 
                       col_types = cols(`Order Date` = col_date(format = "%Y/%m/%d")))

# Question 1:

#' For the last 3 months of 2012, calculate the total Sales by month, 
#' for Alberta and Yukon in the Customer_Segment, Corporate, and Consumer. 
#' This output is output 1.


# Question 2: 

#' Make a plot of the monthly total Sales in British Columbia and 
#' Ontario in 2009, 2010, and 2011. This output is output 2.

 # Questions 3: 
#'  In output 2, identify the months where the total Sales in Ontario
#' is greater than the total Sales in "British Columbia". This output is output 3.






# Question 4: 

#' Use the getSymbols() function from the quantmod package 
#' to download the daily stock prices for Exxon Mobil Corporation (XOM),traded at NYSE 
#' from the Yahoo! Finance site  
browseURL("https://finance.yahoo.com/quote/XOM/history")

#' Use the Adjusted closing price from January 4th 2010 as the starting date. 
#' And calculate the monthly average using trading volume as a weight, 
#' and save this variable as  “exxon”. 
#' Then,plot exxon against the date variable.   






# For the questions below, use the gapminder dataset from the R library. 

library(gapminder) 
data("gapminder")


#' Q1. Calculate the average lifExp of the three Nordic countries
#' (i.e., Norway, Sweden, Denmark)



#' Q2. Calculate the average lifExp of the three Nordic countries,per country. 
#' (i.e., average per country, name variable )



#' Q3. Calculate mean life expectancy per country 
#' (i.e., for every country in the gapminder data set)


#' Q4. Calculate mean life expectancy per continent & also min and max lifeExp


#' Q5. calculate mean life expectancy by continent & country



# Q6. Scatter plot of gdpPercap vs lifeExp by Continent


#' Q7. scatter plot per continent, separate graphs for each continent 


#' Q8:construct histogram of lifeExp by continent



#' Q 9: Find the correlation between lifeExp, andgdpPercap for all countries.
#' Sort according to descending order. Scatter plot only the top two and bottow two.
#'You can read about correlation in links:  
#browseURL("https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html")
#browseURL("https://www.investopedia.com/ask/answers/032515/what-does-it-mean-if-correlation-coefficient-positive-negative-or-zero.asp")



#' Q10. calculate the percentage and logarithmic gdpPercap growth & their compounds (cumulative change)
#'  for the case of Norway 



#' Q11: calculate the logarithmic growth in gdp for all countries


#' Q12: what country has the highest and lowest average logarithmic growth in gdp?  



#' Q13: show the density plot of average logarithmic growth in gdp per continent. 






