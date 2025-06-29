library(forecast)
library(tidyverse)
library(dplyr)
library(AER)
library(zoo)
library(stargazer)
library(tseries)
library(ggplot2)
cta <- read.csv("~/Desktop/cta.csv") %>% select(service_date, total_rides)
# Convert the service_dat column to Date objects
cta$service_date <- as.Date(cta$service_date, format = "%m/%d/%Y")
# Extract the year and month
cta$year_month <- format(cta$service_date, "%Y-%m")
# Aggregate total rides by month
monthly_rides <- aggregate(total_rides ~ year_month, data = cta, sum)
# Create time series
ts <- ts(monthly_rides$total_rides, frequency = 12, start = c(2001, 1))
# Create the training set (Jan 2001 to Dec 2023)
train <- window(ts, start = c(2001, 1), end = c(2018, 12))
#Visualize time serie
plot(train)
#Stationary test
adf.test(train)
adf.test(diff(train))
#Further inspection
ggtsdisplay(train)
#Optimal ARIMA model
sarima <- forecast(auto.arima(train), h = 12)
autoplot(sarima) +
  labs(
    x = "Time",
    y = "Total Ridership",
  ) +
  theme_light()
# Create the test set (Jan 2024 to Dec 2024)
test <- window(ts, start = c(2019, 1), end = c(2019, 12))
# Create a data frame for plotting
comparison <- tibble(
  Month = 1:12,
  Actual = as.numeric(test),
  SARIMA = as.numeric(sarima$mean),
)
# Visualize the optimal model's performance
ggplot(comparison, aes(x = Month)) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = SARIMA, color = "Predicted"), linewidth = 1, linetype = "dashed") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_color_manual(values = c("Actual" = "steelblue", "Predicted" = "red")) +
  labs(
    title = "Model Performance Against 2019 Test Data",
    x = "Month",
    y = "Total Ridership",
    color = "Legend"
  ) +
  theme_light()
#Accuracy metrics
accuracy(sarima, test)
