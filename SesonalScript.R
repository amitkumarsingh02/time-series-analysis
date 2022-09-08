library(zoo)
library(xts)
library(astsa)
library(scales)


# Loading Data
monthly_sales <- read.csv("C:\\Users\\amitk\\Desktop\\MA 641\\Project\\DAUTONSA.csv",header = TRUE, stringsAsFactors = FALSE)
head(monthly_sales)
summary(monthly_sales)

# Check if null
is.na(monthly_sales)

# Split into training and testing data
n = 12
monthly_sales$DATE <- as.Date(monthly_sales$DATE, format = "%Y-%m-%d")
train <- monthly_sales %>% dplyr::filter(DATE<='2018-12-01')
train_ts <- ts(train$DAUTONSA,start=c(2009,1), end=c(2018,12), frequency=n)
test <- monthly_sales %>% dplyr::filter(DATE>='2019-01-01')
test_ts <- ts(test$DAUTONSA,start=c(2019,1), end=c(2019,12), frequency=n)

# Plotting time series 
ts.plot(train_ts, xlab="Time", ylab = "Unit Sold", main="Time vs Unit Sold")

# ACF and PACF of series
acf(train_ts, main = "ACF")
pacf(train_ts, main = "PACF")

# Stationary Test 
# Augmented Dickey Fuller 
adf.test(train_ts)

# Taking diif on time series.
train_diff_ts <- diff(train_ts, differences = 1)

# Plotting time series after taking difference of time series
ts.plot(train_diff_ts, xlab="Time", ylab = "Unit Sold", main="Time vs Unit Sold")

# The ACF/ PACF / EACF plots after taking difference of time series
acf(train_diff_ts, main = "ACF")
pacf(train_diff_ts, main = "PACF")
eacf(train_diff_ts)

# Stationary Test 
# Augmented Dickey Fuller Test after taking difference of time series
adf.test(train_diff_ts)
#################################################################

# ARIMA(2,1,3)X(0,1,1)[12]
model = Arima(train_ts, order=c(2,1,3), seasonal=c(0,1,1))
model

# ACF/PACF of residual
resid = model$resid
acf(resid,lag.max=30, main = "ACF")
pacf(resid,lag.max=30, main = "PACF")

# Ljung-Box Test
Box.test(model$resid, lag = 10, type ="Ljung-Box")

# QQ plot for normality 
qqnorm(residuals(model))
qqline(residuals(model), col=2)

# Histogram plot for normality 
hist(window(rstandard(model)),xlab='Standardized Residuals',  main="Residuals Histogram")

#################################################################3

# ARIMA(6,1,4)X(0,1,1)[12]
model2 = Arima(train_ts, order=c(6,1,4), seasonal=c(0,1,1))
model2

# ACF/PACF of residual
resid2 = model2$resid
acf(resid2,lag.max=30, main = "ACF")
pacf(resid2,lag.max=30, main = "PACF")

# Ljung-Box Test
Box.test(model2$resid, lag = 10, type ="Ljung-Box")

# QQ plot for normality 
qqnorm(residuals(model2))
qqline(residuals(model2), col=2)

# Histogram plot for normality 
hist(window(rstandard(model2)),xlab='Standardized Residuals',  main="Residuals Histogram")

#################################################################################################

# ARIMA(2,1,2)X(0,1,0)[12]
model3 = Arima(train_ts, order=c(2,1,2), seasonal=c(0,1,0))
model3

# ACF/PACF of residual
resid3 = model3$resid
acf(resid3,lag.max=30, main = "ACF")
pacf(resid3,lag.max=30, main = "PACF")

# Ljung-Box Test
Box.test(model3$resid, lag = 10, type ="Ljung-Box")

# QQ plot for normality 
qqnorm(residuals(model3))
qqline(residuals(model3), col=2)

# Histogram plot for normality 
hist(window(rstandard(model3)),xlab='Standardized Residuals',  main="Residuals Histogram")

#################################################################################################

# ARIMA(2,1,1)X(0,1,1)[12]
model4 = Arima(train_ts, order=c(2,1,1), seasonal=c(0,1,1))
model4

# ACF/PACF of residual
resid4 = model4$resid
acf(resid4,lag.max=30, main = "ACF")
pacf(resid4,lag.max=30, main = "PACF")

# Ljung-Box Test
Box.test(model4$resid, lag = 10, type ="Ljung-Box")

# QQ plot for normality 
qqnorm(residuals(model4))
qqline(residuals(model4), col=2)

# Histogram plot for normality 
hist(window(rstandard(model4)),xlab='Standardized Residuals',  main="Residuals Histogram")

#################################################################################################
# Forecasting ARIMA (2,1,3) X (0,1,1) [12] 

forecast <- forecast(model2, h=n)
autoplot(forecast) +
  autolayer(test_ts, series = "Test Data")