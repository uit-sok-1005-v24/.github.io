rm(list = ls())

library(tidyverse)
library(gapminder)
library(broom)

# gapminder data info, apply the class() function to each column 
gapminder %>% 
  map_chr(class) # we use map:chr() since the output of the class() fun is a character

# unique/distinct items/values in each column 
gapminder %>% 
  map_dbl(n_distinct)

# apply both the class() and n_distinct() function and
# return the results as a data frame
gapminder %>% 
  map_df(~data.frame(n_distinct = n_distinct(.x),
                     class = class(.x)))

# Note that we have lost the variable names/column names
gapminder %>%
  map_df(~data.frame(n_distinct = n_distinct(.x),
                     class = class(.x)),
                       .id = "variable")

########################################
# Maps with multiple input objects
####################################

# Create a list of plots that compare life expectancy and GDP per capita 
# for each continent/year combination

# First, you need to define a vector (or list) of continents and
# a paired vector (or list) of years that you want to iterate through.

# all distinct combinations of continents and years in the data
continent_year <- 
  gapminder %>% 
  distinct(continent, year)

continent_year

# extract the continent and year pairs as separate vectors
continents <- 
  continent_year %>% 
  pull(continent) %>% 
  as.character

years <- 
  continent_year %>%
  pull(year)

# the first in each vector (just for the first iteration, the first continent, and the first year)
.x <- continents[1]
.y <- years[1]

# make a scatter-plot of GDP vs life expectancy in all Asian countries for 1952
gapminder %>% 
  filter(continent == .x,
         year == .y) %>%
  ggplot() +
  geom_point(aes(x = gdpPercap, y = lifeExp)) +
  ggtitle(glue::glue(.x, " ", .y)) +
  theme_bw()

# A list of plots
plot_list <- map2(.x = continents, 
                  .y = years, 
                  .f = ~{
                    gapminder %>% 
                      filter(continent == .x,
                             year == .y) %>%
                      ggplot() +
                      geom_point(aes(x = gdpPercap, y = lifeExp)) +
                      ggtitle(glue::glue(.x, " ", .y)) +
                      theme_bw() 
                  })

plot_list[[1]]
plot_list[[25]]


# list columns and nested data frames
nested <- 
  gapminder %>% 
  group_by(continent) %>% 
  nest()

nested

# this is the same
gapminder %>%
  nest(data = -continent)

# extract the first entry from the data column
nested$data[[1]]

# Using dplyr pluck() function, this can be written as
nested %>% 
  pluck("data", 1)

#'Similarly, the 5th entry in the data column corresponds to the entire gapminder dataset for Oceania.
nested %>% pluck("data", 5)

# Why we would ever want to nest our data frame, while we can use dplyr function such as filter?

# The answer is that nesting is useful when we want to apply a function mutate on average lifeExp per continent 
nested

# Calculate the average life expectancy for each continent
nested %>% 
  mutate(avg_lifeExp = map_dbl(data, ~{mean(.x$lifeExp)}))

# Calculate the correlation btw lifeExp and gdpPercap
nested %>% 
  mutate(correlation = map_dbl(data, ~cor(.x$lifeExp, .x$gdpPercap)))

# Similar to, but without the "data"
gapminder %>% 
  group_by(continent) %>% 
  summarise(mean_lifeExp=mean(lifeExp))

# correlation 
gapminder %>% 
  group_by(continent) %>% 
  summarise(correlation = cor(lifeExp, gdpPercap))


# A correlation test
ct <- cor.test(gapminder$lifeExp, gapminder$gdpPercap)

#' This output presents various data types we might need,
#' including single-value metrics like the p-value and estimate,
#' and multi-value metrics like the confidence interval.
#' To streamline and organize these into a user-friendly format,
#' we can employ the `tidy` function, which transforms the output into a well-structured tibble.

tidy(ct)

# Now on the nested data
nested %>% 
  mutate(correlation = map(data, ~cor.test(.x$lifeExp, .x$gdpPercap) %>%
                             tidy()))

# The output is a list-column, where each element is a tibble.
nested %>%
  mutate(
    correlation = map(data, ~cor.test(.x$lifeExp, .x$gdpPercap)), # S3 list-col
    tidied = map(correlation, tidy)
  )

# Finally, we want to unnest the tidied data frames so we can see the results in a flat tibble.
nested %>%
  mutate(
    correlation = map(data, ~cor.test(.x$lifeExp, .x$gdpPercap)), # S3 list-col
    tidied = map(correlation, tidy)
  ) %>% 
  unnest(tidied)

# A model for the relationship between life expectancy and GDP per capita (in percent)
lm(log(lifeExp) ~ log(gdpPercap), data = gapminder)

tidy(lm(log(lifeExp) ~ log(gdpPercap), data = gapminder))

#' The next example will demonstrate how to fit a model separately for each continent
nested %>% 
  mutate(fit = map(data, ~lm(log(lifeExp) ~ log(gdpPercap), data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied)

# Filter the results to only include the coefficient for log(gdpPercap)
gapminder %>% 
  group_by(continent) %>% 
  nest() %>% 
  mutate(fit = map(data, ~lm(log(lifeExp) ~ log(gdpPercap), data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied) %>% 
  filter(term == "log(gdpPercap)")


# with 95% confidence intervals, ordered by the coefficient estimate      
gapminder %>%
  group_by(continent) %>%
  nest() %>%
  mutate(
    fit = map(data, ~ lm(log(lifeExp) ~ log(gdpPercap), data = .x)),
    tidied = map(fit, tidy),
    ci = map(fit, confint)) %>%
  unnest(tidied) %>%
  filter(term == "log(gdpPercap)") %>%
  mutate(
    conf_low = map_dbl(ci, ~ .x["log(gdpPercap)", "2.5 %"]),
    conf_high = map_dbl(ci, ~ .x["log(gdpPercap)", "97.5 %"])) %>%
  ggplot(aes(x = reorder(continent, estimate), y = estimate, ymin = conf_low, ymax = conf_high)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  labs(y = "Coefficient Estimate", x = "Continent",
       title = "Coefficient of log(gdpPercap) Across Continents",
       subtitle = "With 95% Confidence Intervals") +
  theme_minimal() +
  coord_flip()

