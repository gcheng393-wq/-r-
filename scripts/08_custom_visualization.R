# Custom Visualizations in R

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(forestplot)
library(lubridate)

# Function to create coefficient forest plots
create_forest_plot <- function(df, title) {
  p <- ggplot(df, aes(y = term, x = estimate)) + 
    geom_point() + 
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) + 
    labs(title = title, x = "Estimate", y = "Terms") + 
    theme_minimal()
  return(p)
}

# Function for predicted vs actual scatter plot
predicted_vs_actual <- function(actual, predicted) {
  df <- data.frame(Actual = actual, Predicted = predicted)
  p <- ggplot(df, aes(x = Actual, y = Predicted)) + 
    geom_point() + 
    geom_abline(slope = 1, intercept = 0, color='red') + 
    labs(title = "Predicted vs Actual", x = "Actual Values", y = "Predicted Values") + 
    theme_minimal()
  return(p)
}

# Function for time series trend analysis
plot_time_series <- function(df, time_col, value_col) {
  df[[time_col]] <- lubridate::ymd(df[[time_col]])
  p <- ggplot(df, aes_string(x = time_col, y = value_col)) + 
    geom_line() + 
    labs(title = "Time Series Trend Analysis", x = "Time", y = value_col) + 
    theme_minimal()
  return(p)
}

# Function for sensitivity analysis
sensitivity_analysis_plot <- function(sensitivity_data) {
  p <- ggplot(sensitivity_data, aes(x = var, y = value)) + 
    geom_bar(stat='identity') + 
    coord_flip() + 
    labs(title = "Sensitivity Analysis", x = "Variables", y = "Impact") + 
    theme_minimal()
  return(p)
}

# Example of customizable ggplot2 graphics
custom_plot_example <- function(df) {
  p <- ggplot(df, aes(x = x_var, y = y_var)) + 
    geom_point() + 
    labs(title = "Customizable Plot", x = "X-axis", y = "Y-axis") + 
    theme_minimal()
  return(p)
}

# Usage examples (to be replaced with actual data)
# forest_data <- data.frame(term = c("Term1", "Term2"), estimate = c(0.5, 1.2), conf.low = c(0.2, 0.8), conf.high = c(0.8, 1.6))
# print(create_forest_plot(forest_data, "Coefficient Forest Plot"))

# actual <- c(3, 4, 5)
# predicted <- c(2.9, 4.1, 5.1)
# print(predicted_vs_actual(actual, predicted))

# Replace the following line with an actual dataframe for time series
# df_time_series <- data.frame(time_col = c("2023-01-01", "2023-02-01"), value_col = c(10, 15))
# print(plot_time_series(df_time_series, "time_col", "value_col"))

# Example of sensitivity data
# sensitivity_data <- data.frame(var = c('Variable1', 'Variable2'), value = c(10, 20))
# print(sensitivity_analysis_plot(sensitivity_data))