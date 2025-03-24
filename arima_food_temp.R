library(tidyverse)
library(dplyr)
library(ggplot2) # for graph
library(forecast) # for ARIMA
library(TSA)
library(tseries)
library(lmtest)
library(quantmod)
library(lubridate)

# Load the temperature data. working directory : user - tgs ml - skripsi - data2 - Tempt
Dsuhu <- read.csv("D_suhu_all.csv") 
head(Dsuhu)

############################################################ Data Understanding

# Drop columns PARAMETER & ANN
D_suhu <- subset(Dsuhu, select = -c(PARAMETER, ANN))
D_suhu <- subset(D_suhu, YEAR >= 1984 & YEAR <= 2018)
head(D_suhu)
tail(D_suhu)

### Annual temperature per Latitude & Longitude
# 1: Create a unique ID for each LAT-LON pair
lat_lon_groups <- D_suhu %>% #pipeline operator to pass data to next code
  distinct(LAT, LON) %>%
  mutate(Group_ID = row_number())  # Assign a unique number
print(lat_lon_groups)

# 2: Merge with the original dataset
D_suhu <- D_suhu %>%
  left_join(lat_lon_groups, by = c("LAT", "LON"))
head(D_suhu)

# 3: Compute annual temperature for each group
yearly_temp <- D_suhu %>%
  group_by(YEAR, Group_ID) %>%
  summarise(Yearly_Avg_Temp = mean(c_across(JAN:DEC), na.rm = TRUE), .groups = "drop")

# 5: Plot the graph
ggplot(yearly_temp, aes(x = YEAR, y = Yearly_Avg_Temp, group = Group_ID, color = as.factor(Group_ID))) +
  geom_line() +
  labs(title = "Annual Temperature Trends by Location",
       x = "Year",
       y = "Annual Average Temperature",
       color = "Location Group") +
  theme_minimal()

### Monthly Temperature Across all Lat Lon
# 1: Convert data from wide format (JAN-DEC as column) to long format 
long_temp <- D_suhu %>%
  pivot_longer(cols = JAN:DEC, names_to = "MONTH", values_to = "Temperature")
head(long_temp)

# 2: Convert MONTH names to numeric for correct ordering
month_levels <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
                  "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
long_temp <- long_temp %>%
  mutate(MONTH = match(MONTH, month_levels))  # Convert JAN = 1, FEB = 2, etc.

# 3: Compute average temperature across all lat-lon for each Year-Month
monthly_avg_temp <- long_temp %>%
  group_by(YEAR, MONTH) %>%
  summarise(Avg_Temp = mean(Temperature, na.rm = TRUE), .groups = "drop") %>%
  mutate(Time = as.Date(paste(YEAR, MONTH, "01", sep = "-")))  # Create Date column

# 4: Plot the time series (Without Points)
ggplot(monthly_avg_temp, aes(x = Time, y = Avg_Temp)) +
  geom_line(color = "blue") +  # Single continuous line, no points
  labs(title = "Average Monthly Temperature Over Time",
       x = "Time (Year-Month)",
       y = "Average Temperature (°C)") +
  theme_minimal()

# terlihat bahwa data menunjukkan seasonal pattern tahunan

### Yearly Temperature Across Lat-Lon
# 1: Compute yearly average temperature across all lat-lon
yearly_avg_temp <- long_temp %>%
  group_by(YEAR) %>%
  summarise(Avg_Temp = mean(Temperature, na.rm = TRUE), .groups = "drop")

# 2: Plot the yearly trend
ggplot(yearly_avg_temp, aes(x = YEAR, y = Avg_Temp)) +
  geom_line(color = "red", size = 1) +  # Red line for yearly trend
  labs(title = "Average Yearly Temperature Over Time",
       x = "Year",
       y = "Average Temperature (°C)") +
  theme_minimal()

# Terlihat ada upward trend - data not stationary
# Stationary = has no trend or seasonality
# Agar dapat forecast dengan ARIMA data akan dibuat stasioner
# Akan digunakan data tahunan untuk setiap group Lat Lon

########################################################### Check stationarity

# ADF test
check_stationarity <- function(temp_series) {
  test_result <- adf.test(temp_series)
  return(test_result$p.value)
}

# Apply ADF test for each Group_ID
stationarity_results <- yearly_temp %>%
  group_by(Group_ID) %>%
  group_split() %>%  # Split data into a list by Group_ID
  map_df(~{
    cat("\n--- Group_ID:", unique(.x$Group_ID), "---\n")
    print(.x)  # Print data for each Group_ID
    tibble(Group_ID = unique(.x$Group_ID), 
           p_value = check_stationarity(.x$Yearly_Avg_Temp)) 
  })
# tibble - create small table with Group_ID and its ADF
# map_df(~{...}) - apply function inside {...} to each group
# .x - current data frame being processed. one group at a time

print(stationarity_results)
# H0 : p-value ≤ 0.05 → Temperature data for that group is stationary.
# H1 : p-value > 0.05 → Temperature data is non-stationary (suggests upward trend or seasonality).

# Identify non-stationary groups (p-value > 0.05 means non-stationary)
non_stationary_groups <- stationarity_results %>%
  filter(p_value > 0.05)
print(non_stationary_groups)

# Karena seluruh group memiliki p-val = 0.01 <0.05. 
# Maka terdapat cukup bukti untuk menolak H0.
# Maka data telah stasioner.

###################################### Percobaan membuat data menjadi stasioner

# temp_ts1 <- diff(temp_ts, differences = 1)

# plot.ts(temp_ts1, lty=1, ylab = expression(Ydif[t]), col = "red", 
#        main="Plot Setelah Differencing Pertama")S

# adf.test(temp_ts1, alternative = "stationary") # Non-stationary

################################################### Using Auto ARIMA to Forecast

# ARIMA Forecasting
forecast_arima <- function(temp_series) {
  fit <- auto.arima(temp_series)
  forecast_result <- forecast(fit, h=3)  # Forecast for the next 3 periods
  return(forecast_result)
}
# forecast_arima <- function(temp_series) - defines a function named forecast_arima that takes temp_series
# fit <- auto.arima(temp_series) - determining the optimal number of AR, MA, and differencing terms (p, d, q)
# forecast_result <- forecast(fit, h=3)  # Forecast for the next 3 periods

# Forecast for all Group_IDs (using ARIMA for all)
forecast_all_results <- stationarity_results %>%
  left_join(yearly_temp, by = "Group_ID") %>%
  group_by(Group_ID) %>%
  group_split() %>%
  map_df(~{
    cat("\n--- Forecasting for Group_ID (ARIMA):", unique(.x$Group_ID), "---\n")
    temp_series <- .x$Yearly_Avg_Temp
    forecast_result <- forecast_arima(temp_series)
    tibble(Group_ID = unique(.x$Group_ID), 
           forecast = list(forecast_result$mean),
           lower = list(forecast_result$lower),
           upper = list(forecast_result$upper))
  })

# stationarity_results %>% - takes data
# left_join(yearly_temp, by = "Group_ID") - keep all rows of stationarity result and adds yearly temp based on corresponding Group_ID
# group_split() - split grouped data into a list of data frames
# map_df(~{...}) - apply function to {...}
# cat("...") - print massage which group is being processed
# temp_series <- .x$Yearly_Avg_Temp - extracr temp data for current group (.x)
# forecast_result <- forecast_arima(temp_series) - call forecast arima function
# tibble - create new data frame
# forecast = list(forecast_result$mean),  # Store forecast values
# lower = list(forecast_result$lower),    # Store lower bounds of prediction interval
# upper = list(forecast_result$upper))    # Store upper bounds of prediction interval

# Print forecast results
print(forecast_all_results)


############################################## ARIMA Manual. Model Spesification

# Check ACF and PACF
 
# ACF: Menunjukkan nilai MA. Plot akan bergantian +-.
# kalau plot gradually menyusut brati AR.
# bs utk liat seasonal. misal tiap kelipatan 4 dia mendadak tinggi. 
# jd garis 1-3 menurun, no 4 naik lg. trs next 5-7 menurun yg ke-8 naik lg.
# brati dia ada lag tiap 4 periode waktu.
# kalo dia MA, liat ACF ada berapa lag. brati itu orde p utk MA(p).

# pacf: Menunjukkan nial AR. liat yg diatas titik". garis terjauh sblm cut off.
# kalo bergantian +- = MA. kalo gradually turun = AR
# bs utk liat seasonal. kalo AR liat aja ada berapa lag. 
# cth ada 2 mendadak naik tinggi, brati AR(2).

# kalo di ACF PACF dua2 ny gradually turun brati dia ARMA.
# antar lag dr kedua plot jg akan keliatan makin turun.
# biasa AR(1) atau MA(1).

# EACF: Menunjukkan hasil ARMA/ARIMA. Start liat dr kiri atas. principal of parsimony.

acf(temp_ts) 
# MA(2) no seasonal.

pacf(temp_ts) 
# MA(1) no seasonal

eacf(temp_ts, ar.max = 5, ma.max = 5) 
# ARMA(0,2), ARMA(1,2), ARMA(2,1), ARMA(2,3)

# Fit ARIMA models based on analysis
mod1 <- arima(temp_ts, order=c(0,0,2))
mod2 <- arima(temp_ts, order=c(1,0,2))
mod3 <- arima(temp_ts, order=c(2,0,1))
mod4 <- arima(temp_ts, order=c(2,0,3))

# Compare AIC and BIC values
# lowest AIC indicating a better fit compared to other models.
# lowest BIC indicating a better fit compared to other models.
# higher Adjusted R2 indicates a better fit, suggesting that the model captures more of the variability in the data.

aic_values <- c(mod1$aic, mod2$aic, mod3$aic, mod4$aic)
bic_values <- c(BIC(mod1), BIC(mod2), BIC(mod3), BIC(mod4))

aic_values
bic_values

# Select the best model based on AIC and BIC
best_model <- mod4

# Model diagnostics
coeftest(best_model) 
# Check if the model is significant. 
# Liat bintang ny. yg ada bintang itu pval<alpha. brati parameter signifikan.
# makin byk * makin signifikan.

residuals <- residuals(best_model)
plot(residuals, col="blue")

# Check normality of the residuals
# HO:
# H1: 
hist(residuals)
ks.test(residuals, "pnorm")
# dengan alpha = 0.05, p-value < alpha - Ho ditolak.
qqnorm(residuals)
qqline(residuals)

# Check autocorrelation of the residuals
acf(residuals)
Box.test(residuals, lag=5, type="Ljung")

# Check heteroscedasticity of the residuals
McLeod.Li.test(y=residuals)


#######################################################

# Make predictions for the next 12 months (2019)
forecasts <- forecast(best_model, h=12)

# Extract the actual data for 2019
actual_2019 <- D_suhu %>% filter(YEAR == 2019)

# Since actual_2019 still has YEAR column, we remove it to compare directly
actual_2019 <- actual_2019 %>% select(-YEAR)

# Display the actual 2019 data
actual_2019

###############################################################

# Compare forecasts with actual data
comparison <- data.frame(
  Month = month.abb,
  Forecast = forecasts$mean,
  Actual = as.numeric(actual_2019)
)

# Display the comparison
comparison

# Calculate MAE
mae <- mean(abs(comparison$Forecast - comparison$Actual))

# Calculate MSE
mse <- mean((comparison$Forecast - comparison$Actual)^2)

# Calculate RMSE
rmse <- sqrt(mse)

# Display the error metrics
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")