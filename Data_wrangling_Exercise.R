
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

str(superstore)
names(superstore)

#' fix names
superstore<- superstore %>% clean_names()

###################
# Question 1:
################
#' For the last 3 months of 2012, calculate the total Sales by month, 
#' for Alberta and Yukon in the Customer_Segment, Corporate, and Consumer. 
#' This output is output_1.


output_1 <- superstore %>%
  mutate(Year = year(order_date),
         Month = month(order_date),
         Day = day(order_date)) %>%
  arrange(Year,Month,Day) %>% 
  filter(Year ==2012,
         Month >=10,
         province %in% c("Yukon","Alberta"),
         customer_segment %in% c("Corporate", "Consumer")) %>% 
  group_by(Month,province,customer_segment) %>% 
  summarise(Tol.sale = sum(sales, na.rm = TRUE)) %>% 
  arrange(province, Month)

#library(knitr)
knitr::kable(output_1)

######################
# Question 2: 
######################
#' Make a plot of the monthly total Sales in British Columbia and 
#' Ontario in 2009, 2010, and 2011. This output is output 2.

output_2 <- superstore %>%
  mutate(Year = year(order_date),
         Month = month(order_date,label = TRUE),
         Day = day(order_date)) %>%
  arrange(Year,Month,Day) %>% 
  filter(Year >=2009 &  Year <=2011, # or filter(Year %in% c("2009", "2010", "2011"),
         province %in% c("British Columbia","Ontario"),
         customer_segment %in% c("Corporate", "Consumer")) %>% 
  group_by(Year,Month,province) %>% 
  summarise(Tot.sales = sum(sales, na.rm = TRUE)) %>%
  mutate(date = make_date(Year,Month)) 

# plots 
output_2 %>% 
  ggplot(aes(x=date,y = Tot.sales, color = province))+
  geom_line()

# Alternatively 
output_2 %>% 
  ggplot(aes(x=Month, y=Tot.sales))+
  geom_col(aes(fill=province), position="dodge")+
  facet_wrap(~Year, nrow=3)+
  theme_bw()+
  labs(x="",
       y="Total sales", 
       title="Total monthly sales in British Columbia, and Ontario", color="province") +
  scale_y_continuous(labels = scales::dollar) +
  scale_fill_manual(values = c("British Columbia" = "#8fc3f8", "Ontario" = "#567595"),
                    name = "Province")+
  theme(legend.position="right") 
  


########################
 # Questions 3: 
#######################
#'  In output 2, identify the months where the total Sales in Ontario
#' is greater than the total Sales in "British Columbia". This output is output 3.

output_3 <- output_2 %>% 
  select(-date) %>%
  pivot_wider(names_from = province, values_from = Tot.sales) %>% 
  filter("Ontario" > "British Columbia") %>% 
  arrange(Year, Month)


knitr::kable(output_3)

###################
# Question 4: 
########################
#' Use the getSymbols() function from the quantmod package 
#' to download the daily stock prices for Exxon Mobil Corporation (XOM),traded at NYSE 
#' from the Yahoo! Finance site  
browseURL("https://finance.yahoo.com/quote/XOM/history")

#' Use the Adjusted closing price from January 4th 2010 as the starting date. 
#' And calculate the monthly average using trading volume as a weight, 
#' and save this variable as  “exxon”. 
#' Then,plot exxon against the date variable.   

library(quantmod)

xom <- getSymbols("XOM", src = "yahoo", auto.assign = FALSE) %>%
  data.frame() %>% 
  clean_names() %>% 
  rownames_to_column(var="date") %>% 
  select(date, xom_adjusted, xom_volume) %>%
  rename(adj_close = xom_adjusted, 
          volume = xom_volume)  %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date >= "2010-01-04" & date <= "2023-12-31") %>% 
  mutate(year = year(date), 
         month = month(date), 
         day = day(date)) %>% 
  group_by(year, month) %>% 
  summarise(exxon = weighted.mean(adj_close, volume)) %>% 
  mutate(date = make_date(year,month)) %>% 
  ungroup() %>% 
  select(date, exxon)

xom %>% 
  ggplot(aes(x= date, y= exxon))+
  geom_line()+
  theme_bw()+
  labs(x="",
       y="Total sales", 
       title="Monthly average ",
       caption = "source: Author plot using data from XXX database" ) 


###############################
#################
#########

# For the questions below, use the gapminder dataset from the R library. 

library(gapminder) 
data("gapminder")

#---------------
#' You can read about the data and the variables  
help("gapminder")

# Data Exploration 
str(gapminder)
glimpse(gapminder)
#-------------

#' Q1. Calculate the average lifExp of the three Nordic countries
#' (i.e., Norway, Sweden, Denmark)

gapminder %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  summarise(av_lfExp = mean(lifeExp))

#' Q2. Calculate the average lifExp of the three Nordic countries,per country. 
#' (i.e., average per country, name variable )
gapminder %>%
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  group_by(country) %>% 
  summarise(av_lfexp=mean(lifeExp))


#' Q3. Calculate mean life expectancy per country 
#' (i.e., for every country in the gapminder data set)
gapminder %>% 
  group_by(country) %>%
  summarise(avlfexp=mean(lifeExp))


#' Q4. Calculate mean life expectancy per continent & also min and max lifeExp

gapminder %>%
  group_by(continent) %>% 
  summarise(avlfexp=mean(lifeExp), 
            min=min(lifeExp), 
            max=max(lifeExp))
  

#' Q5. calculate mean life expectancy by continent & country

gapminder %>% 
  group_by(continent, country) %>%
  summarise(avlfexp=mean(lifeExp))


# Q6. Scatter plot of gdpPercap vs lifeExp by Continent

gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color=continent, shape=continent)) + 
  geom_point(alpha=0.4) +
  ggtitle("Life Expectancy and GDP by Continent")

#' Q7. scatter plot per continent, separate graphs for each continent 

gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) + 
  geom_point() +
  facet_wrap(~continent, ncol=3) +
  ggtitle("Life Expectancy and GDP by Continent")

#' Q8:construct histogram of lifeExp by continent
gapminder %>%
  ggplot(aes(x = lifeExp)) + 
  geom_histogram(binwidth=2) +
  facet_wrap(~continent, ncol=3) +
  ggtitle("Life Expectancy by Continent")



#' Q 9: Find the correlation between lifeExp, andgdpPercap for all countries.
#' Sort according to descending order. Scatter plot only the top two and bottow two.
#' 
#'You can read about correlation in links:  
#browseURL("https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html")
#browseURL("https://www.investopedia.com/ask/answers/032515/what-does-it-mean-if-correlation-coefficient-positive-negative-or-zero.asp")

#' sorted ascending
gapminder %>%
  group_by(country) %>%
  summarise(correlation = cor(lifeExp, gdpPercap)) %>% 
  arrange(correlation)

#' sorted descending
gapminder %>%
  group_by(country) %>%
  summarise(correlation = cor(lifeExp, gdpPercap)) %>% 
  arrange(desc(correlation))

#' top two and bottom two
gapminder %>%
  filter(country %in% c("Kuwait","Madagascar","France","Austria")) %>% 
  ggplot(aes(x = lifeExp, y = gdpPercap, colour = country)) + geom_point()

#' scale is important in plots
gapminder %>%
  filter(country %in% c("Madagascar")) %>% 
  ggplot(aes(x = lifeExp, y = gdpPercap, colour = country)) + geom_point()

#' Q10. calculate the percentage and logarithmic gdpPercap growth & their compounds (cumulative change)
#'  for the case of Norway 

gapminder %>%
  filter(country == "Norway") %>%
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>% 
  mutate(perc_diff = (gdp-dplyr::lag(gdp))/dplyr::lag(gdp),
         log_diff = c(NA, diff(log(gdp))),
         comp_perc = c(1, 1+cumsum(na.omit(perc_diff))),
         comp_log = c(1, 1+cumsum(na.omit(log_diff))))

#------------------------
#' what if we wanted to change 1982 to the base year (=100) for comp_log?
df1 <- gapminder %>%
  filter(country == "Norway") %>%
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>% 
  mutate(log_diff = c(NA, diff(log(gdp))),
         comp_log = c(1, 1+cumsum(na.omit(log_diff))))

df2 <- df1 %>% filter(year==1982)

df1 %>%
  mutate(rebase=100*comp_log/df2$comp_log)
#---------------------

#' Q11: calculate the logarithmic growth in gdp for all countries
gapminder %>%
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>%
  group_by(country) %>%
  mutate(log_diff = c(NA, diff(log(gdp)))) %>% head(.,20)

#' Q12: what country has the highest and lowest average logarithmic growth in gdp?  


#' lowest
gapminder %>%
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>%
  group_by(country) %>%
  mutate(log_diff = c(NA, diff(log(gdp)))) %>%
  group_by(country) %>% 
  summarise(meanGDP=mean(na.omit(log_diff))) %>% arrange(meanGDP)

#' highest
gapminder %>%
  select(country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>%
  group_by(country) %>%
  mutate(log_diff = c(NA, diff(log(gdp)))) %>%
  group_by(country) %>% 
  summarise(meanGDP=mean(na.omit(log_diff))) %>% arrange(desc(meanGDP))


#' Q13: show the density plot of average logarithmic growth in gdp per continent. 


#' density plot
gapminder %>%
  select(continent, country, year, gdpPercap) %>%
  rename(gdp=gdpPercap) %>%
  group_by(country) %>%
  mutate(log_diff = c(NA, diff(log(gdp)))) %>% 
  ggplot() + 
  geom_density(aes(x = log_diff, group=continent, fill=continent), alpha=0.3) + 
  ggtitle("Average GDP by Continent") +
  xlim(c(-0.25,0.5)) +
  xlab("% average growth in GDP")




