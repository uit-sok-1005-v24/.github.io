rm(list=ls())

library(tidyverse)
library(lubridate)
library(janitor)

#' From reading list:
#' Chiu, David
#' R for data science cookbook:
#' over 100 hands-on recipes to effectively solve real-world data problems using
#' the most popular R packages and techniques

superstore <- read_csv("https://github.com/ywchiu/rcookbook/raw/master/chapter7/superstore_sales.csv", 
                       col_types = cols(`Order Date` = col_date(format = "%Y/%m/%d")))

#' This is superstore sales data in different province of Canada.

str(superstore)
names(superstore)

superstore$`Order ID`

#' fix names
superstore <- superstore %>% clean_names()

names(superstore)

str(superstore)
head(superstore)

#' Exploratory Data Analysis
#install.packages("DataExplorer") 
library(DataExplorer)

plot_str(superstore)
plot_missing(superstore)
plot_histogram(superstore)
plot_density(superstore)
plot_correlation(superstore, type = 'continuous')

# For character/factors
plot_bar(superstore)  

#' Summarize the total sales amount by year, month, and province
#' All days in a month set to 1 - long data

length(unique(superstore$order_date))
min(superstore$order_date)
max(superstore$order_date)
length(unique(floor_date(superstore$order_date, unit = "year")))
length(unique(floor_date(superstore$order_date, unit = "month")))
4*12
length(unique(superstore$province))
13*48 # max

superstore %>%
  select(sales, province, order_date) %>% 
  mutate(year_month = floor_date(order_date, unit = "month")) %>%
  group_by(year_month, province) %>%  
  summarise(total_sales = sum(sales)) 

#' From long to wide, use pivot_wider(data, names_from, values_from)
superstore %>% 
  select(sales, province, order_date) %>%
  mutate(year_month = floor_date(order_date, unit = "month")) %>%
  group_by(year_month, province) %>%  
  summarise(total_sales = sum(sales)) %>% 
  pivot_wider(names_from = province, values_from = total_sales) %>% 
  clean_names()

#' Make a plot of the monthly total sales in Alberta and Youkon in 2010 and 2011
superstore %>%
  select(sales, province, order_date) %>% 
  mutate(year_month = floor_date(order_date, unit = "month")) %>%
  group_by(year_month, province) %>% 
  summarise(total_sales = sum(sales)) %>%
  filter(province %in% c("Alberta","Yukon")) %>% 
  mutate(year = year(year_month)) %>%
  filter(year %in% c(2010, 2011)) %>%
  ggplot(., aes(x=year_month, y=total_sales, color=province)) +
  geom_line() +
  xlab("year") + ylab("Total Sales Amount") + 
  ggtitle("Monthly Total Sale Amount By province") 


#' Changing aesthetics
p <- 
  superstore %>%
  select(sales, province, order_date) %>% 
  mutate(year_month = floor_date(order_date, unit = "month")) %>%
  group_by(year_month, province) %>% 
  summarise(total_sales = sum(sales)) %>%
  filter(province %in% c("Alberta","Yukon")) %>% 
  mutate(year = year(year_month)) %>%
  filter(year %in% c(2010, 2011)) %>%
  ggplot(., aes(x=year_month, y=total_sales, color=province))

p  # blank canvas

p + geom_line(linetype="dashed", size=2) +
  xlab("year") + ylab("Sales Amount") + 
  ggtitle('Change Linetype and Size')

# how to increase the number of grids on the x axis (date)?
p + geom_line(linetype="dashed", size=2) + 
  xlab("year") + ylab("Sales Amount") + 
  ggtitle('Change Linetype and Size') + 
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%y")  # compact date format
#browseURL("https://www.r-bloggers.com/2013/08/date-formats-in-r/")

# how to rotate the labels on x axis?
p + geom_line(linetype="dashed", size=2) +
  xlab("year") + ylab("Sales Amount") + 
  ggtitle('Change Linetype and Size') + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#' Stacked bar chart, why?
p + geom_bar(stat = "identity", aes(fill=province) , position = "stack") + 
  xlab("year Month")  + ylab("Sale Amount") + 
  ggtitle('Stack Position') # one bar on top of the other

#' Fill position, why?
p + geom_bar(stat = "identity", aes(fill=province), position = "fill") +
  xlab("year Month") + ylab("Sale Amount") + ggtitle('Fill Position')

#' Dodge position, why?
p + geom_bar(stat = "identity", aes(fill=province), position = "dodge")  + 
  xlab("year Month")  + ylab("Sale Amount") + ggtitle('Dodge Position') +
  scale_fill_brewer(palette=2) + 
  theme_bw() 

p + geom_point(size=3) + xlab("year Month") + ylab("Sale Amount") + ggtitle('Adding Smoother')
p + geom_point(size=3) + xlab("year Month") + ylab("Sale Amount") + geom_smooth() + ggtitle('Adding Smoother')
p + geom_point(size=3) + xlab("year Month") + ylab("Sale Amount") + geom_smooth(se=FALSE) + ggtitle('Adding Smoother')
p + geom_point(size=3) + xlab("year Month") + ylab("Sale Amount") + geom_smooth(method=lm,se=FALSE) + ggtitle('Adding Linear Regression')
p + geom_point(size=3) + xlab("year Month") + ylab("Sale Amount") + geom_point(stat = "summary", fun.y = "mean", colour = "red", size = 4) + ggtitle('Adding Mean Points')

p + geom_point(aes(size=total_sales)) + scale_size_continuous(range=c(1,10)) +xlab("year Month") + ylab("Sale Amount") + ggtitle('Resize The Point')
p + geom_point(aes(colour=total_sales), size=5) + scale_color_gradient() +xlab("year Month") + ylab("Sale Amount")  +ggtitle('Repaint The Point in Gradient Color')

p + geom_point(size = 5) + facet_wrap(~province) +xlab("year Month") + ylab("Sale Amount") + ggtitle('Create Multiple Subplots by province')
p + geom_point(size = 5) + facet_wrap(~province, ncol=1) +xlab("year Month") + ylab("Sale Amount") + ggtitle('Multiple Subplots in Vertical Direction')

p + geom_point(size=5) + theme_bw()+xlab("year Month") + ylab("Sale Amount")  +ggtitle('theme_bw Example')
p + geom_point(size=5) + theme_dark()+ xlab("year Month") + ylab("Sale Amount") +ggtitle('theme_dark Example')

p + geom_point(size=5) + scale_color_manual(values=c("#E69F00", "chartreuse")) +
  theme(
    axis.text = element_text(size = 12),
    legend.background = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = "yellow"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "blue")
  ) + xlab("year Month") + ylab("Sale Amount") +ggtitle('Customized Theme')

library(grid)
grid.newpage()

plot1 <- p + geom_point(size=5) +xlab("year Month") + ylab("Sale Amount") + ggtitle('Scatter Plot')
plot2 <- p + geom_line(size=3) + xlab("year Month") + ylab("Sale Amount") + ggtitle('Line Chart')

pushViewport(viewport(layout = grid.layout(1, 2)))
print(plot1, vp =viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot2, vp =viewport(layout.pos.row = 1, layout.pos.col = 2))

library(gridExtra)
grid.arrange(plot1,plot2, ncol=2)


# ----------------------------------------------

#' Min and Max date
summarise(superstore, min(order_date))
summarise(superstore, max(order_date))

#' Find weekday of date
superstore %>% 
  select(order_date) %>% 
  mutate(weekday = wday(order_date, label=TRUE),
         weekdayno = wday(order_date))

#' Q: Find average sales by weekday, but exclude Saturdays and Sundays.
superstore %>% 
  select(sales, order_date) %>% 
  mutate(weekday = wday(order_date)) %>% 
  filter(!weekday %in% c(1, 7)) %>% 
  group_by(weekday) %>%
  summarise(average_sales=mean(sales))

#' or
superstore %>% 
  select(sales, order_date) %>% 
  mutate(weekday = wday(order_date, label=TRUE)) %>% 
  filter(!weekday %in% c("Sun","Sat")) %>% 
  group_by(weekday) %>%
  summarise(average_sales=mean(sales))


# Average sales on Saturdays and Sundays
superstore %>% 
  select(sales, order_date) %>% 
  mutate(weekday = wday(order_date, label=TRUE)) %>% 
  filter(weekday %in% c("Sun","Sat")) %>% 
  group_by(weekday) %>%
  summarise(average_sales=mean(sales))

#' Q: Find average sales by weekday and Customer Segment. Make one column per Customer Segment.
superstore %>%
  select(sales, order_date, customer_segment) %>%
  mutate(weekday = wday(order_date, label=TRUE)) %>% 
  group_by(weekday, customer_segment) %>%
  summarise(average_sales=mean(sales)) %>% 
  pivot_wider(names_from=customer_segment,
              values_from=average_sales)


#' Your boss loves excel (sigh), and would like a spreadsheet with the weekdays and segments report in it.
#browseURL("https://cran.r-project.org/web/packages/openxlsx/index.html")

library(openxlsx)

weekdays <- 
  superstore %>% 
  select(sales, order_date) %>% 
  mutate(weekday = wday(order_date, label=TRUE)) %>% 
  group_by(weekday) %>%
  summarise(average_sales=mean(sales))

segments <- 
  superstore %>% 
  select(sales, order_date, customer_segment) %>% 
  mutate(weekday = wday(order_date, label=TRUE)) %>% 
  group_by(weekday, customer_segment) %>%
  summarise(average_sales=mean(sales)) %>% 
  pivot_wider(names_from = customer_segment, values_from=average_sales) 

#' Make sure you use a relevant folder on your computer
getwd()
#setwd("filepath")
setwd("C:/temp")
dir()

sheets <- list(weekdays = weekdays, segments = segments)
openxlsx::write.xlsx(sheets, file = "dataset.xlsx")

#' Use the segments data frame and calculate the market share per customer segment and day
segments %>%
   pivot_longer(-weekday, names_to = "province", values_to = "average_sale") %>%
  group_by(weekday) %>%
  mutate(share = 100*average_sale/sum(average_sale)) %>%
  select(-average_sale) %>% 
   pivot_wider(names_from = province, values_from = share)

#' Find average profit per customer segment and product category in 2011, for all provinces except
#' Nunavut, Newfoundland and Manitoba.
#' What segment produced the highest profit?

superstore %>% group_by(province) %>% tally() 

superstore %>%
  select(profit, customer_segment, product_category, order_date, province) %>% 
  mutate(year = year(order_date)) %>%
  filter(year==2011) %>% 
  filter(!province %in% c("Nunavut", "Newfoundland", "Manitoba")) %>% 
  group_by(customer_segment, product_category) %>% 
  summarise(average_profit = mean(profit)) %>%
  arrange(-average_profit)

### ----------------------------------------------------------------------
#' A bit more advanced
### ----------------------------------------------------------------------

#' What customer segment and product category has the highest correlation between
#' unit price and order_quantity?

superstore %>%
  select(customer_segment, order_quantity,unit_price) %>%
  group_by(customer_segment) %>%
  summarize(cor(order_quantity,unit_price))

#browseURL("https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html")

superstore %>% 
    select(unit_price, order_quantity, customer_segment, product_category) %>% 
  group_by(customer_segment, product_category) %>%
  nest() -> nested 

nested

library(broom)

nested %>% 
  mutate(
    test = map(data, ~ cor.test(.x$unit_price, .x$order_quantity)), # S3 list-col
    tidied = map(test, tidy)) %>% 
  unnest(tidied)

#' Make a scatter plot of it
nested$group <- 1:length(nested$customer_segment)

library(glue)

many_plots <- 
  nested %>% 
  mutate(plot = map2(data, 
                     group,
                     ~ ggplot(data = .x, aes(x = order_quantity, y = unit_price)) +
                       ggtitle(glue("Group {.y}")) + 
                       geom_point()))

print(many_plots$plot) 

#install.packages("cowplot")
library(cowplot)
cowplot::plot_grid(plotlist = many_plots$plot)

superstore %>% 
  select(unit_price, order_quantity, customer_segment, product_category) %>% 
  ggplot(aes(x = order_quantity, y = unit_price, color = product_category)) +
  geom_point() 

superstore %>% 
  select(unit_price, order_quantity, customer_segment, product_category) %>% 
  ggplot(aes(x = order_quantity, y = unit_price, color = customer_segment)) +
  geom_point() 
