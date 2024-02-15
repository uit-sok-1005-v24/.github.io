

rm(list=ls())

library(tidyverse)


#  Data Visualization
############################################# 
#  Objective: to get used to some of the fun from ggplot2
# 
#(ggplot2 is often called the grammar of graphics )       
############################################


#The following represents the basic ggplot2 template.
ggplot(data = DATA,mapping = aes(MAPPINGS)) + 
  GEOM_FUNCTION()+...

# or using the %>% operator 
DATA %>% 
  ggplot(mapping = aes(MAPPING)) +
  geom_function()+...

#' The main components include the data we want to plot,  mapping aesthetics, and geom function(s).
#'Notice the + symbol following the ggplot() function. This symbol will precede
# each additional layer of code for the plot, and it is important that it is placed at the end of the
# line. 

# Geom_functions(): 
######################
# scatter plot - geom_point() 
# line plots - geom_line () , geom_path()
# bar plots - geom_bar(), geom_col()
# line modeled to fitted data - geom_smooth()
# heat maps - geom_tile()
# geographic map - geom_polygon(), 
# etc

#ggplot2 provides over 40 geoms, and extension packages provide even more

#The best way to get a comprehensive overview is the ggplot2 cheatsheet 
browseURL("https://www.maths.usyd.edu.au/u/UG/SM/STAT3022/r/current/Misc/data-visualization-2.1.pdf")
# You can also see a number of options pop up when you type geom into the console 

# An aesthetic is a visual property of the objects in your plot
# Mapping aesthetics include some of the following:
# 1. the x and y data arguments
# 2. shapes
# 3. color
# 4. fill
# 5. size
# 6. linetype
# 7. alpha, etc


# Now, let's see some of these templates in practice using the "superstore" data.





library(lubridate)
library(janitor)

# Data
superstore <- read_csv("https://github.com/ywchiu/rcookbook/raw/master/chapter7/superstore_sales.csv", 
                       col_types = cols(`Order Date` = col_date(format = "%Y/%m/%d")))

#' This is superstore sales data in different province of Canada.

str(superstore)
names(superstore)

#' fix names
superstore <- superstore %>% clean_names()

# Average profit over time per province   
 superstore %>%
  mutate(year_month = floor_date(order_date, unit = "month"),
         year.month  = update(order_date, day = 1)) %>% 
  group_by(year_month, province) %>% 
  summarise(mean.profit = mean(profit)) %>% 
  ggplot(mapping=aes(x= year_month, 
                     y= mean.profit,
                     color = province))+ # color/fill by Province 
  geom_point() 

# Not that much helpful, right?  
# Try another 
 
# show whether there is difference in the average total profit per province 
# (i.e., overall average profit per province )
superstore %>% 
  group_by(province) %>% 
  summarise(mean.profit = mean(profit)) %>% 
  ggplot(aes(x=mean.profit, y= province)) +
  geom_point()

# we can arrange according to increasing profit 
superstore %>%
  group_by(province) %>% 
  summarise(mean.profit = mean(profit)) %>% 
  ggplot(aes(x= mean.profit,y= fct_reorder(province,mean.profit)))+
  geom_point() +
  xlab("Average profit")+
  ylab ("Provinces")#+coord_flip() # flip coordinate 


# Now let us pick just only two province, Alberta and Yukon 
p <- superstore %>%
        mutate(year_month = floor_date(order_date, unit = "month")) %>% 
        group_by(year_month, province) %>% 
        summarise(mean.profit = mean(profit)) %>% 
       filter(province %in% c("Alberta","Yukon")) %>% 
        ggplot(., aes(x=year_month, y=mean.profit , color=province)) 

p

# add the type of the geom_fun() you want  + label the axises + title
p + geom_point() +
  xlab("Year") + 
  ylab("Average profit") + 
  ggtitle("Monthly Average profit Amount By province")
  
# more better to add labels and....
p + geom_point()+
  labs(title = "Monthly Average profit Amount By province",
       y = "Average profit",
       x = "Year",
       caption = "Source: This is my my own plot using the data....") 




#ggplot2 will automatically assign colors to the categories in our data
#We can change the default colors by providing an additional layer to our figure.
# Also, increase the size of the points in the plot 

p + geom_point(size=5)+   # increase the size of the points in the plot
  scale_color_manual(values=c("red","black"), # color 
                     labels=c('Alberta','Yukon'),
                     name = "Province")  # Rename the legend title 
#For palette choices: 
RColorBrewer::display.brewer.all() 


# Faceting
#############
#Facets divide a plot into subplots based on the values of one or more discrete variables.  
#There are two primary functions to add facets, facet_wrap() and facet_grid()
#If faceting by a single variable, use facet_wrap(). If multiple variables, use facet_grid().
#The first argument of either function is a formula, with variables separated by a ~ 

?facet_wrap()

p + geom_point(size=5)+   
  scale_color_manual(values=c("red","black"), 
                     labels=c('Alberta','Yukon'),
                     name = "Province") +    
  facet_wrap(~province)#+geom_smooth(method=lm,se=FALSE)  # also vars(province)




# Both plots in the one column 
p + geom_point(size=5)+   
  scale_color_manual(values=c("red","black"), 
                     labels=c('Alberta','Yukon'),
                     name = "Province") +    
  facet_wrap(~province, ncol = 1)


#Facet_grid - would allow us to map even more variables in our data.

# ?facet_grid()

p + geom_point(size=5)+
  scale_color_manual(values=c("red","black"),
                     labels=c('Alberta','Yukon'),
                     name = "Province") +
  facet_grid(customer_segment ~ province)

# hmmmm.....why?---ok,-- summarise? 
# use mutate instead of summarise when you compute the average profit 

q <- superstore %>%
  mutate(year_month = floor_date(order_date, unit = "month")) %>% 
  group_by(year_month, province) %>% 
 mutate(mean.profit = mean(profit)) %>% 
  filter(province %in% c("Alberta","Yukon")) %>% 
  ggplot(., aes(x=year_month, y=mean.profit , color=province))  


q + geom_point(size=5)+
  scale_color_manual(values=c("red","black"),
                     labels=c('Alberta','Yukon'),
                     name = "Province") +
  facet_wrap(customer_segment ~ province)

# more better
q + geom_point(size=1)+
  scale_color_manual(values=c("red","black"),
                     labels=c('Alberta','Yukon'),
                     name = "Province") +
  facet_grid(customer_segment ~ province)



############################

# look at the plot above again 
p + geom_point(size=5)+   
  scale_color_manual(values=c("red","black"), 
                     labels=c('Alberta','Yukon'),
                     name = "Province")   

# Adjust or transform the y-axis
# Also, adjust the background color 
p + geom_point(size=5)+   
  scale_color_manual(values=c("red","black"), 
                     labels=c('Alberta','Yukon'),
                     name = "Province") +  
  scale_y_continuous(trans="log10")+ #log transform the y axis
  theme_bw() +# adjust background color 
  xlab("year") + 
  ylab("Average profit") + 
  ggtitle("Monthly Average profit Amount By province")


# Saving plots (ggsave()
#Finally, we have a quality plot ready to publish. The next step is to save our plot to a file. The
#easiest way to do this with ggplot2 is ggsave().

?ggsave()
#For comparison let’s look at standard base R, next to ggplot2:
ggsave("Plot1.png",width=5.5,height=3.5,units="in",dpi=300)



############################################
# Bar plots, histograms, Piechart, box plots etc
###########################################

# Bar Plot- A barplot is used to display the relationship between a numeric and 
#           a categorical/factor variable.

# base R 
barplot(table(superstore$customer_segment))

superstore %>% 
  ggplot(aes(x = customer_segment)) +
  geom_bar()


# customer segment by province
superstore %>% 
  ggplot(aes(x = customer_segment, fill =province)) +
  geom_bar()

# customer segment by product category 
ggplot(superstore) +
  geom_bar(aes(x = customer_segment,
               fill =product_category ))

# side-by-side 
ggplot(superstore) +
  geom_bar(aes(x = customer_segment,
               fill =product_category ),
           position=position_dodge())

# alternatively (also space between the bins) 
ggplot(superstore) +
  geom_bar(aes(x = customer_segment,
               fill =product_category ),
           position=position_dodge2())


#Now let’s add province into the mix…
ggplot(superstore) +
  geom_bar(aes(x = customer_segment,
               fill = product_category),
           position=position_dodge2()) +
  facet_wrap(~province)

#---------------------------------------------
# Using data stored in R
data("ToothGrowth")
head(ToothGrowth)

help("ToothGrowth")
# A data frame with 60 observations on 3 variables.
# len -	numeric	Tooth length
# supp -	factor	Supplement type (VC or OJ).
# dose -	numeric	Dose in milligrams/day

a1 <- ToothGrowth

a1 %>% 
  ggplot(mapping=aes(x=dose,
                     y=len,color=supp))+
  geom_point(position=position_dodge(width=0.25))


###
ggplot(data=a1)+
  geom_bar(mapping=aes(x=dose,y=len)) # stat="identity"
# We get this error because geom_bar uses stat_count by default where 
#it counts the number of cases at each x position.
#Thus, by default, geom_bar does not require y axis.

a1 %>% 
  ggplot(mapping=aes(x=dose))+
  geom_bar(color="black")
   
# fill color
a1 %>% 
  ggplot(mapping=aes(x=dose, fill = supp))+
  geom_bar(color="black")
           
#By default, geom_bar stacks bars from different groups. If we do not like the arrangement,
# we can use position_dodge to arrange the bars from the OJ and VC groups side-by-side

a1 %>% 
  ggplot(mapping=aes(x=dose, fill = supp))+
  geom_bar(color="black", 
           position=position_dodge())

# stat = identity 
a1 %>% 
  ggplot(mapping=aes(x=dose, y= len,fill = supp))+
  geom_bar(color="black",
           position=position_dodge2(),
           stat="identity") 

#To avoid stacking the values, we can use position_dodge2 in geom_bar to visualize each of
#the 10 measurements taken at each supplement and dose combination arranged side-by-side. 


#Using factor
class(a1$dose)

#dose is really an experimental factor, so if we specify factor(dose) it will be
#interpreted as categorical or discrete. 

#Using the factor function we see that there are three levels for dose (0.5, 1, and 2)
factor(a1$dose)


#To remove the dose of 1.5, we can set the x axis to factor(dose)

a1 %>% 
  ggplot(mapping=aes(x=factor(dose),
                     y=len,
                     fill=supp))+
  geom_bar(stat="identity",
           position=position_dodge2(),
           color="black")


# Arrange the plot based on the levels: 0.5,1,and 2

a1 %>% 
  ggplot(mapping=aes(x=factor(dose,
                              levels=c(2,1,0.5)),
                     y=len,fill=supp))+
  geom_bar(stat="identity",
           position=position_dodge2(),
           color="black")

# Histogram:

#Understanding data distribution can help us decide appropriate downstream steps in analysis
#such as which statistical test to use. 
a1 %>% 
ggplot(mapping=aes(x=len))+
  geom_histogram()

#use the color argument in geom_histogram to assign a border color to help
#distinguish the bins.

#Then we use the fill argument in geom_histogram to change the bars
#associated with the bins to a color other than gray.
a1 %>% 
  ggplot(mapping=aes(x=len))+
  geom_histogram(color="black", 
                 fill="cornflowerblue")

#Let's alter the number of bins
a1 %>% 
  ggplot(mapping=aes(x=len))+
  geom_histogram(color="black", 
                 fill="cornflowerblue",
                 bins = 7)




###############################
#  Piechart
#####################################
x<- superstore %>% 
  group_by(product_category) %>% 
  summarise(mean.profit = mean(profit)) %>% 
  ggplot(aes(x="", y= mean.profit, fill=product_category)) +
  geom_col() +
  coord_polar(theta = "y")


x
#Color of the lines
#The borders of the pie can be changed with the color argument of the geom_bar or geom_col function.

superstore %>% 
  group_by(product_category) %>% 
  summarise(mean.profit = mean(profit)) %>% 
  ggplot(aes(x="", y= mean.profit, fill=product_category)) +
  geom_col(color = "black") +
  coord_polar(theta = "y")

#Adding text

#By default, the values are not displayed inside each slice. 
#You can add them with geom_text. Note that position_stack(vjust = 0.5) will place 
#the labels in the correct position.
superstore %>% 
  group_by(product_category) %>% 
  summarise(mean.profit = round(mean(profit),3)) %>% 
  ggplot(aes(x="", y= mean.profit, fill=product_category)) +
  geom_col(color = "black") +
  geom_text(aes(label = mean.profit),
            position = position_stack(vjust = 0.5)) +

  coord_polar(theta = "y")

#Labels color
#Note that you can change the color of the labels with color.

superstore %>% 
  group_by(product_category) %>% 
  summarise(mean.profit = round(mean(profit),3)) %>% 
  ggplot(aes(x="", y= mean.profit, fill=product_category)) +
  geom_col(color = "black") +
  
  geom_label(aes(label = mean.profit),
             color = "white",
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  
  coord_polar(theta = "y")

#Color customization

#Color palette
#The default color palette can be changed with a predefined color palette, 
#such as the scale_fill_brewer or scale_fill_viridis_d.
superstore %>% 
  group_by(product_category) %>% 
  summarise(mean.profit = round(mean(profit),3)) %>% 
  ggplot(aes(x="", y= mean.profit, fill=product_category)) +
  geom_col(color = "black") +
  geom_text(aes(label = mean.profit ),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_brewer()


#Custom colors

#If you prefer setting your own colors you can make use of scale_fill_manual
# and set the corresponding colors.

superstore %>% 
  group_by(product_category) %>% 
  summarise(mean.profit = round(mean(profit),3)) %>% 
  ggplot(aes(x="", y= mean.profit, fill=product_category)) +
  geom_col(color = "black") +
  geom_text(aes(label = mean.profit ),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#BE2A3E", "#EACF65", "#3C8D53"))


#Theme customization
#The default pie chart styling can be changed in ggplot2 making use of themes.
superstore %>% 
  group_by(product_category) %>% 
  summarise(mean.profit = round(mean(profit),3)) %>% 
  ggplot(aes(x="", y= mean.profit, fill=product_category)) +
  geom_col(color = "black") +
  geom_text(aes(label = mean.profit ),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_brewer()+
  theme_bw()

#Legend customization 
#' Legend title
#The default legend title is the name of the categorical variable of 
#the input data frame. Change it following the example below.

superstore %>% 
  group_by(product_category) %>% 
  summarise(mean.profit = round(mean(profit),3)) %>% 
  ggplot(aes(x="", y= mean.profit, fill=product_category)) +
  geom_col(color = "black") +
  geom_text(aes(label = mean.profit ),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_brewer()+
  guides(fill = guide_legend(title = "This is the legend Title"))


##############################################
# Your turn now 
#####################################################

 # Some questions: using the Titanic" data. 
##############################################
titanic <- read.csv("https://web.stanford.edu/class/archive/cs/cs109/cs109.1166/stuff/titanic.csv")
# More data: # Data https://web.stanford.edu/class/archive/cs/cs109/cs109.1166/stuff/ 
str(titanic)
colnames(titanic) 

# Question 1. 
#Make a simple scatter plot. Is there a relationship between the age of the passenger and the
#passenger fare? 





# Question 2: 
# Color the points from question 3 by Pclass. Remember that Pclass is a proxy for socioeconomic
# status. While the values are treated as numeric upon loading, they are really categorical and
# should be treated as such. You will need to coerce Pclass into a categorical (factor) variable.
# See factor() and as.factor().





# Question 3
# Manually scale the colors in question 4. 1st class = yellow, 2nd class = purple, 3rd class =
#   seagreen. Also change the legend labels (1 = 1st Class, 2 = 2nd Class, 3 = 3rd Class). 




# Question 4
# Facet the plot made in 3 by the column 'Sex'



# Question 5
# Let's use some other geoms. Plot the number of passengers (a simple count) that survived by
# ticket class and facet by sex.




# Question 6
# Add a variable to the data frame called age_cat (child = <12, adolescent = 12-17,adult= 18+).
# Plot the number of passengers (a simple count) that survived by age_cat, fill by Sex, and facet
# by class and survival. 






 #References:

browseURL("https://jhudatascience.org/tidyversecourse/dataviz.html")
browseURL("https://exeter-data-analytics.github.io/AdVis/AdVisHandout.pdf")
browseURL("https://bioinformatics.ccr.cancer.gov/docs/data-visualization-with-r/pdf/combined.pdf")#PCA
browseURL("https://intro2r.library.duke.edu/viz.html") #Animate plots
browseURL("https://gganimate.com/")#Animate plots
browseURL("https://www.hsph.harvard.edu/wp-content/uploads/sites/2488/2022/09/Data-Visualization-in-R.pdf")# World map
browseURL("https://r-charts.com/part-whole/pie-chart-ggplot2/") # Pie chart:
 
