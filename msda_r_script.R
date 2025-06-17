# R scripts only
# See QMD file for full analysis

# The astsa library contains the sarima functions
library(astsa)
# ggplot2 for more graph customization
library(ggplot2)
# tidyr for the pivot_longer function
library(tidyr)
# tsibble for yearmonth object type
library(tsibble)
# dplyr for mutate
library(dplyr)


# Load the data from the csv file
ts_data <- read.csv("tickets_sold_monthly_2000_2019.csv")
summary(ts_data)
head(ts_data$Month)
tail(ts_data$Month)


# Convert month data to yearmonth type
ts_data$month <- yearmonth(ts_data$Month)


# Plot the monthly data to visualize it
ggplot(ts_data, aes(x = month, y = Monthly_Sales)) +
  geom_line() +
  labs(title = "Monthly Movie Ticket Sales Data", 
       x = "Month (Year-Month)", 
       y = "Number of Sales") +
  theme_minimal()

# No missing data is confirmed by the length, 
# missingness, and max of the variables
sum(is.na(ts_data))
# No missing values
length(ts_data$month)
# 240 months in the dataset
max(ts_data$month)-min(ts_data$month)+1
# 240 months from first date to last
# +1 for the final month gets 240 total


# Convert data file to ts (time series) format
# Use 12 months for yearly cycles
tickets <- ts(data=ts_data$Monthly_Sales, frequency = 12)
plot(tickets)


# Difference the data to remove any trend 
d_tickets <- diff(tickets)
plot(d_tickets)


# Plot a periodogram of both the base data and the differenced data
spectrum(tickets)
# Not a true spike, but worth checking
# Pull the maximum value from the periodogram and find the frequency of it
freq1 <- spectrum(tickets)$freq[which.max(spectrum(tickets)$spec)]
# Invert the frequency to determine the period
1/freq1
# 0.5 -> half year patterns

# Check the differenced periodogram data too
spectrum(d_tickets)
# Not a true spike, but worth checking
# Pull the maximum value from the periodogram and find the frequency of it
freq2 <- spectrum(d_tickets)$freq[which.max(spectrum(d_tickets)$spec)]
# Invert the frequency to determine the period
1/freq2
# 0.5 -> half year patterns


# Make acf and pacf charts of the data and differenced data
# Long view charts of non-differenced data
acf2(tickets, max.lag = 239)

acf2(d_tickets, max.lag = 238)

acf2(tickets, max.lag = 12)

acf2(d_tickets, max.lag = 12)


# Try SARIMA values

# AR 1, no seasonality
sarima(tickets, p = 1, d = 0, q = 0)
# Good p values, AIC 13, BIC 13.1
# Ljung-Box statistic does not look great
# Should add more complexity


# AR 1, MA 1 no seasonality
sarima(tickets, p = 1, d = 0, q = 1)
# MA 1 p value of 0.63, AIC 13.1, BIC 13.1
# Definitely no MA component


# AR 1, no seasonality, I 1 (integrative for trend removal)
sarima(tickets, p = 1, d = 1, q = 0)
# Bad p values, AIC 13.2, BIC 13.2
# Worse in every way


# AR 1, Seasonal I 1, S = 12
sarima(tickets, p = 1, d = 0, q = 0, P = 0, D = 1, Q = 0, S = 12)
# Good p values, AIC 13.1, BIC 13.1
# Decent but likely still needs more complexity


# AR 1 Seasonal I 1, MA 1, S = 12
sarima(tickets, p = 1, d = 0, q = 0, P = 0, D = 1, Q = 1, S = 12)
# Good p values, AIC 12.6, BIC 12.7
# Finally a good Ljung-Box statistic p value chart
# By far the best so far!


# AR 1 Seasonal AR 1, I 1, MA 1, S = 12
sarima(tickets, p = 1, d = 0, q = 0, P = 1, D = 1, Q = 1, S = 12)
# sar1 p value of 0.92, AIC 12.6, BIC 12.7
# Still a decent model, but the seasonal AR 1 is not helpful


# Try higher orders for comparison
sarima(tickets, p = 1, d = 0, q = 0, P = 0, D = 1, Q = 2, S = 12)
# sma2 p value 0.91
sarima(tickets, p = 2, d = 0, q = 0, P = 0, D = 1, Q = 1, S = 12)
# ar2 p value of 0.157 (just a bit too high)


# Lets go back to our best model and try it with different S values for comparison
sarima(tickets, p = 1, d = 0, q = 0, P = 0, D = 1, Q = 1, S = 6)
# Good p values and AIC/BIC (12.7/12.8)
# But the Ljung-Box statistic is not consistent above the p value threshold
sarima(tickets, p = 1, d = 0, q = 0, P = 0, D = 1, Q = 1, S = 10)
# Model is worse by most metrics


# Comparison against linear regression
linear <- lm(Monthly_Sales ~ month, ts_data)
summary(linear)
AIC(linear)
BIC(linear)


# Train and test data
# Approxmiately 80% training data split
train <- ts(tickets[1:192])
test <- ts(tickets[193:240])

# Use training data and make predictions against the test data
# Predict ahead the same number of values as the remaining test values
length(test)
# n.ahead should be 48
train_preds <- ts(sarima.for(train, n.ahead = 48, 1,0,0,0,1,1,12)$pred)
# Calculate the root mean squared error for the predictions against the test
rmse <- sqrt(mean((train_preds - test)^2))
rmse


# Compare with ARIMA 1 1 0 model
c_train_preds <- ts(sarima.for(train, n.ahead = 48, 1,1,0)$pred)
# Calculate the root mean squared error for the predictions against the test
rmsec <- sqrt(mean((c_train_preds - test)^2))
rmsec


# Plot the two against each other
# Create a data frame for ease in ggplot
plot_data2 <- data.frame(time = 193:240, 
                         Predictions = as.numeric(train_preds), 
                         Actual = as.numeric(test))

# Reshape the data for ggplot2
plot_data2_long <- pivot_longer(plot_data2, cols = c(Predictions, Actual), 
                                names_to = "Series", values_to = "Value")

# Create the plot
ggplot(plot_data2_long, aes(x = time, y = Value, color = Series)) +
  geom_line() +
  labs(title = "Time Series Predictions and Actual Values", x = "Time", y = "Value")



# Predict out 24 months extra
# Create an NA time series with 240 + 24 slots (264)
full_set <- rep(NA, 264)
# Assign the predictions to the last 24 slots
forecast <- sarima.for(tickets, n.ahead = 24, 1,0,0,0,1,1,12)
full_set[241:264] <- forecast$pred
full_set[1:240] <- tickets

# Add the confidence bands
lower_band1 <- ts(rep(NA, 264))
upper_band1 <- ts(rep(NA, 264))
lower_band2 <- ts(rep(NA, 264))
upper_band2 <- ts(rep(NA, 264))
lower_band1[241:264] <- forecast$pred-forecast$se
upper_band1[241:264] <- forecast$pred+forecast$se
lower_band2[241:264] <- forecast$pred-2*forecast$se
upper_band2[241:264] <- forecast$pred+2*forecast$se

# Label the parts of the graph
line_category <- rep(NA,264)
line_category[1:192] <- "Train"
line_category[193:240] <- "Test"
line_category[240:264] <- "Prediction"

# Create a data frame for the forecast
forecast_df <- data.frame(time = 1:264, 
                          forecast = full_set, 
                          lower1 = lower_band1, 
                          upper1 = upper_band1,
                          lower2 = lower_band2,
                          upper2 = upper_band2,
                          color_category = line_category)

# Plot the forecast and confidence intervals
# Color the segments for train/test/prediction
# Add a 1 standard error and 2 standard error range around the prediction
# Move the legend to the top left to save space
ggplot(forecast_df, aes(x = time, y = forecast)) +
  geom_line(aes(color = color_category)) +
  scale_color_manual(values = c("Train" = "darkblue", 
                                "Test" = "darkgreen", 
                                "Prediction" = "darkred")) +
  geom_ribbon(aes(ymin = lower1, ymax = upper1), 
              alpha = 0.2, 
              fill = "red") +
  geom_ribbon(aes(ymin = lower2, ymax = upper2), 
              alpha = 0.1, 
              fill = "red") +
  labs(title = "Train and Test Data with Forecast", 
       x = "Month", 
       y = "Tickets Sold") +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.2, 0.8))



# Accuracy data
acc_data <- plot_data2 %>%
  mutate(Accuracy = 1 - abs((Predictions - Actual)/Actual))

acc_data


# Find the best accuracy and its corresponding time
best <- max(acc_data$Accuracy)
time_best <- acc_data$time[which.max(acc_data$Accuracy)]

cat("Best Accuracy:", best, "at month #:", time_best, "\n")

# Find the worst accuracy and its corresponding time
worst <- min(acc_data$Accuracy)
time_worst <- acc_data$time[which.min(acc_data$Accuracy)]

cat("Worst Accuracy:", worst, "at month #:", time_worst, "\n")
