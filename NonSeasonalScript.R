library(tseries)
library(forecast)
library(dplyr)
library(TSA)
library(quantmod)

# Loading Data
reliance = read.csv("C:\\Users\\amitk\\Desktop\\MA 641\\Project\\RELIANCE.NS.csv", sep=',', header = T)
summary(reliance)
head(reliance)

# Check if na
is.na(reliance)

# Change to Date
reliance$Date <- as.Date(reliance$Date, format = "%Y-%m-%d")

# Sort by Date
reliance <- reliance %>% 
  select(Date, Adj.Close) %>% 
  arrange(Date)

# Split into test and train data
n=50
train_ts <-  head(Cl(reliance), length(Cl(reliance))-n)
test_data <- tail(Cl(reliance), n)

# Plotting time series 
ts.plot(train_ts, xlab="Date", ylab = "Price", main="Date vs Price")

# ACF and PACF of series
acf(train_ts, lag.max=30, main="ACF")
pacf(train_ts, lag.max=30, main="PACF")

# Stationary Test 
# Augmented Dickey Fuller 
adf.test(train_ts)

# Taking diff on time series.
train_diff_ts <- diff(train_ts, differences = 1)

# Plotting time series after taking difference of time series
ts.plot(train_diff_ts)

# Stationary Test 
# Augmented Dickey Fuller Test after taking difference of time series
adf.test(train_diff_ts, xlab="Date", ylab = "Price", main="Date vs Price")

# The ACF/ PACF / EACF plots after taking difference of time series
acf(train_diff_ts, main="ACF")
pacf(train_diff_ts, main="PACF")
eacf(train_diff_ts)

##################################################################################################

# ARIMA(0,1,0)
model <- Arima(train_ts,order=c(0,1,0))
model

# ACF/PACF of residual
resid = model$resid
acf(resid,lag.max=20, main = "ACF")
pacf(resid,lag.max=20, main = "PACF")

# Ljung-Box Test
Box.test(model$resid, lag = 10, type ="Ljung-Box")

# QQ plot for normality 
qqnorm(residuals(model))
qqline(residuals(model), col=2)

# Histogram plot for normality 
hist(window(rstandard(model)),xlab='Standardized Residuals',  main="Residuals Histogram")
#####################################################################################

# ARIMA(1,1,0)
model2 <- Arima(train_ts,order=c(1,1,0))
model2

# ACF/PACF of residual
resid2 = model$resid
acf(resid2,lag.max=20, main = "ACF")
pacf(resid2,lag.max=20, main = "PACF")

# Ljung-Box Test
Box.test(model2$resid, lag = 10, type ="Ljung-Box")

# QQ plot for normality 
qqnorm(residuals(model2))
qqline(residuals(model2), col=2)

# Histogram plot for normality 
hist(window(rstandard(model2)),xlab='Standardized Residuals',  main="Residuals Histogram")

#####################################################################################

# ARIMA(0,1,1)
model3 <- Arima(train_ts,order=c(0,1,1))
model3

# ACF/PACF of residual
resid3 = model$resid
acf(resid3,lag.max=20, main = "ACF")
pacf(resid3,lag.max=20, main = "PACF")

# Ljung-Box Test
Box.test(model3$resid, lag = 10, type ="Ljung-Box")

# QQ plot for normality 
qqnorm(residuals(model3))
qqline(residuals(model3), col=2)

# Histogram plot for normality 
hist(window(rstandard(model3)),xlab='Standardized Residuals',  main="Residuals Histogram")

#####################################################################################

# ARIMA(2,1,3)
model4 <- Arima(train_ts,order=c(2,1,3))
model4

# ACF/PACF of residual
resid4 = mode4l$resid
acf(resid4,lag.max=20, main = "ACF")
pacf(resid4,lag.max=20, main = "PACF")

# Ljung-Box Test
Box.test(model4$resid, lag = 10, type ="Ljung-Box")

# QQ plot for normality 
qqnorm(residuals(model4))
qqline(residuals(model4), col=2)

# Histogram plot for normality 
hist(window(rstandard(model4)),xlab='Standardized Residuals',  main="Residuals Histogram")

############################################################################################
# Forecasting
forecast <- forecast(model4, h=n)
autoplot(forecast) +
  autolayer(ts(test_ts, start=length(train_ts)), series = "Test Data")

