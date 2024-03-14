# Loads tidyquant, lubridate, xts, quantmod, TTR 
library(tidyverse)
library(tidyquant)

#browseURL("https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ01-core-functions-in-tidyquant.html")

# Load stock data
aapl  <- tq_get("AAPL", get = "stock.prices", from = " 2010-01-01")
aapl

msft <- tq_get("MSFT", get = "stock.prices", from = " 2010-01-01")
msft 

goog <- tq_get("GOOG", get = "stock.prices", from = " 2010-01-01")
goog

# Create a function to plot a time series
a_plot <- function(df, var){
  df %>%
    ggplot(aes(x=date)) +
    geom_line(aes(y={{var}})) +
    theme_bw()
}

# Example
a_plot(msft, close)

# Does not work
a_var <- "close"

a_plot(msft, a_var)

# The a_plot function is not working when a_var is a character string. This stems from how ggplot2 handles variable names
# within its aesthetic mappings. When you're using the aes function, it expects bare column names (unquoted) directly or
# through tidy evaluation for dynamic plotting. This is why the first example works, but the second one does not.

# This works
a_plot2 <- function(df, var){
  df %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = !!sym(var))) + # Force evaluation of var as a symbol
    theme_bw()
}

# Example
a_plot2(msft, a_var)

# Combine the data
combined_data <- bind_rows(msft, goog)

# Updated plotting function using the existing 'symbol' column
a_plot_combined <- function(df, var){
  df %>%
    ggplot(aes(x = date, y = !!sym(var), color = symbol)) +  # Use 'symbol' as color differentiation
    geom_line() +
    theme_bw() +
    labs(title = "Stock Prices Over Time", y = "Closing Price", color = "Symbol")
}

# Example usage
a_var <- "close"

a_plot_combined(combined_data, a_var)

# Further combine the data
combined_data <- bind_rows(msft, goog, aapl)

a_plot_combined(combined_data, a_var)

# -----------------------------
# Function to normalize prices to a specific date
# -----------------------------

normalize_prices_to_date <- function(df, normalize_date, var) {
  df %>%
    group_by(symbol) %>%
    mutate(norm_value = 100 * (!!sym(var)) / (!!sym(var))[as.Date(date) == as.Date(normalize_date)]) %>%
    ungroup()
}

# Specific date for normalization
normalize_date <- "2022-11-30"

# Apply the normalization
normalized_data <- normalize_prices_to_date(combined_data, normalize_date, "close")

# Verify normalization for the specific date
normalized_data %>% filter(date == normalize_date)

# Plot
a_plot_combined(normalized_data, "norm_value")

# Updated plotting function with vertical line
a_plot_normalized_with_vline <- function(df, var, highlight_date) {
  highlight_date_numeric <- as.numeric(as.Date(highlight_date))
  
  df %>%
    ggplot(aes(x = date, y = !!sym(var), color = symbol)) + 
    geom_line() +
    geom_vline(xintercept = as.numeric(as.Date(highlight_date)), linetype="dashed", color = "red") +
    theme_bw() +
    labs(title = "Normalized Stock Prices Over Time", y = "Normalized Price", color = "Symbol") +
    annotate("text", x = as.Date(highlight_date), y = Inf, label = highlight_date, vjust = 2, hjust = 1.0, color = "red", angle = 45, size = 3)
}

# Example usage with normalized values and a specific date highlighted
normalize_date <- "2022-11-30"  
a_var <- "norm_value"

a_plot_normalized_with_vline(normalized_data, a_var, normalize_date)

a_plot_normalized_with_vline(filter(normalized_data, year(date)==2022), a_var, normalize_date)


# -----------------------------
# Function to calculate lags
# -----------------------------

calculate_lags <- function(df, var, lags){
  map_lag <- lags %>% map(~partial(dplyr::lag, n = .x))
  return(df %>% mutate(across(.cols = {{var}}, .fns = map_lag, .names = "{.col}_lag{lags}")))
}

# Example usage
aapl %>% calculate_lags(close, 1:3)

a_var <- "close"

aapl %>% calculate_lags(a_var, 1:4)

# Works in pipes
combined_data %>% 
  group_by(symbol) %>%
  calculate_lags(close, 1:3) %>% 
  ungroup()

