#' Velkommen til kurset  
#' - canvas, github, IDE: rstudio vscode og jupyter.uit.no  
#' - oppgaver  
 
#' - data science prosjektet  

#' - pensum (bok)  
#' - Tutorials for R for Data Science (2e)  

#install.packages("r4ds.tutorials")  
#library(r4ds.tutorials)  

#' chatGPT  

#' Forskjell mellom matematikk og statistikk?  

#' Koding i R  

rm(list=ls()) # fjerner alle objekter i workspace

library(tidyverse)

a <- -1  # Example coefficient, adjust as needed
b <- 2   # Example coefficient, adjust as needed
c <- 5   # Example coefficient, adjust as needed

# make a function for the quadratic equation
f <- function(x) {
  a*x^2 + b*x + c
}

f(0)

data_frame <- tibble(
  x = seq(-10, 10, by = 0.1),  # Adjust the range and step as needed
  y = f(x)
)

# look at the data 
data_frame

# Plot the data, mathematically
data_frame %>% 
  ggplot(aes(x, y)) +
  geom_line() +
  labs(title = "Second Order Polynomial with a Maximum",
       x = "X-axis",
       y = "Y-axis") +
  theme_minimal()

# Add a bit of noise to the data
set.seed(1942)  # for reproducibility

# using tidyverse
data_frame <- 
  data_frame %>% 
  mutate(y_noisy = y + rnorm(length(y), mean = 0, sd = 0.1))

# Estimate a model
model <- lm(y_noisy ~ x + I(x^2), data = data_frame)
summary(model)

# Prepare the data for plotting the fitted model
data_frame$fitted_y <- predict(model, newdata = data_frame)

# Plot original data, noisy data, and fitted model
ggplot(data_frame) +
  geom_point(aes(x, y_noisy), alpha = 0.5) +  # Plot noisy data
  geom_line(aes(x, fitted_y), color = "blue") +  # Plot fitted model
  geom_point(aes(x, y), color = "red") +  # Plot original data in red
  labs(title = "Second Order Polynomial: Original, Noisy Data, and Fitted Model",
       x = "X-axis",
       y = "Y-axis") +
  theme_minimal()

# ----------- Faceting

# Base data frame, clean data
data_frame <- tibble(
  x = seq(-10, 10, by = 0.1),
  y = a*x^2 + b*x + c
)

# Prepare an extended data frame with different noise levels
set.seed(1942)
noise_levels <- c(0.1, 1, 10, 50)
extended_data_frame <-
  map_df(noise_levels, ~mutate(data_frame,
                               y_noisy = y + rnorm(length(y), mean = 0, sd = .x),
                               noise_level = factor(.x)))

# Fit models and add fitted values for each noise level
extended_data_frame <- 
  extended_data_frame %>% 
  group_by(noise_level) %>% 
  do({
    model <- lm(y_noisy ~ x + I(x^2), data = .)
    add_column(., fitted_y = predict(model, newdata = .), .before = "noise_level")
  })

# Plot with faceting
ggplot(extended_data_frame, aes(x, y_noisy)) +
  geom_point(aes(color = noise_level), alpha = 0.5) +
  geom_line(aes(y = fitted_y), color = "blue") +
  geom_point(aes(y = y), color = "red") +
  facet_wrap(~noise_level, scales = "free") +
  labs(title = "Second Order Polynomial with Varying Noise Levels",
       x = "X-axis",
       y = "Y-axis") +
  theme_minimal()

# Gravity example

# $h = \frac{1}{2} g t^2$
# $t = \sqrt{\frac{2h}{g}$

set.seed(1942)  # For reproducibility

# Generating random heights
n <- 100  # Number of observations
heights <- runif(n, 1, 10)  # 10 random heights between 1 and 10 meters

# Calculating times with a small error
g <- 9.81  # True value of gravity
times <- sqrt(2 * heights / g)
times <- times + rnorm(10, 0, 0.01)  # Adding a small random error

# Performing linear regression
dframe <- data.frame(heights, times_squared = times^2)
model <- lm(heights ~ times_squared, data = dframe)

summary(model)

# Estimating g
estimated_g <- coef(model)['times_squared'] * 2
estimated_g

# Plotting the data and the fitted model
ggplot(dframe, aes(times_squared, heights)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Linear Regression of Height vs. Time Squared",
       x = "Time Squared",
       y = "Height") +
  theme_minimal()

