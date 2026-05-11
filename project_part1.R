#############################################################
# PROJECT: Time Series Analysis of Finland's GDP (1980-2023)
#############################################################

# 1. ENVIRONMENT SETUP & DATA LOADING
# -----------------------------------
library(readxl)
library(tseries)
library(forecast)
library(urca)
library(FinTS)
library(strucchange)

# Load data
data_GDP <- read_excel("data_GDPfin.xlsx", sheet = "finland")
GDP <- ts(data_GDP$GDP, start = 1980, frequency = 1)    # Time series, annual data

# Basic descriptive statistics
summary(GDP)


# 2. EXPLORATORY DATA ANALYSIS (EDA)
# -----------------------------------
# Plotting the original time series and its differences
plot.ts(GDP, main="Annual GDP in Finland (1980-2023)", col=5, lwd=3, ylab="GDP [mil. EUR]", xlab="Year")
plot.ts(diff(GDP), main="First Difference of Finland's GDP (1980-2023)", col=5, lwd=3, ylab="Differenced GDP", xlab="Year")
plot.ts(diff(diff(GDP)), main="Second Difference of Finland's GDP (1980-2023)", col=5, lwd=3, ylab="Differenced GDP", xlab="Year")

# Checking variance to determine the appropriate order of differencing
var(GDP)
var(diff(GDP))
var(diff(diff(GDP))) # higher variance! - indicates over-differencing

# Autocorrelation (ACF) and Partial Autocorrelation (PACF) plots
layout(matrix(1:2,2,1))

acf(GDP, main="ACF for Annual GDP in Finland (1980-2023)")
pacf(GDP, main="PACF for Annual GDP in Finland (1980-2023)")

acf(diff(GDP), main="ACF for First Difference of GDP in Finland")
pacf(diff(GDP), main="PACF for First Difference of GDP in Finland")

acf(diff(diff(GDP)), main="ACF for Second Difference of GDP in Finland")
pacf(diff(diff(GDP)), main="PACF for Second Difference of  GDP in Finland")

layout(matrix(1:1,1,1)) # Reset plot layout

# Testing for ARCH effects (Heteroskedasticity)
ArchTest(GDP)
ArchTest(diff(GDP))


# 3. STRUCTURAL BREAKS ANALYSIS
# -----------------------------------
# Fitting a simple linear trend model
time <- ts(1:44, start = 1980, frequency = 1)
tslm.lin_GDP <- tslm(GDP ~ trend)
summary(tslm.lin_GDP)

plot.ts(GDP, main="Annual GDP in Finland with Linear Trend", ylab="GDP [mil. EUR]", xlab="Year")
lines(fitted(tslm.lin_GDP), col="blue", lwd=2)

# Chow test F-statistic to find potential structural breaks
res1_GDP <- Fstats(GDP ~ 1 + time, from = 0.20) 
sctest(res1_GDP) # Structural break test based on the maximum of Chow test F-statistic
plot(res1_GDP, main="Chow Test F-Statistic")
lines(breakpoints(res1_GDP))
breakpoints(res1_GDP) # Setting and optimizing the break points

# Calculation of optimal number of break points (minimum segment length 25%)
res2_GDP <- breakpoints(GDP ~ 1+time, h=0.25); res2_GDP
summary(res2_GDP)
plot(res2_GDP, main="Choosing the Number of Structural Breaks", xlab="Number of structural breaks")
# more break points

res3_GDP <- confint(res2_GDP); res3_GDP # confidence interval
plot(GDP, main="Visualization of Structural Breaks", ylab="GDP [mil. EUR]", xlab="Year", col="grey", lwd=2)
lines(res2_GDP)
lines(res3_GDP, col=1)
lines(fitted(res2_GDP), col=2, lty=1, lwd=2)

# Trend line parameters for each segment of the time series
coef(res2_GDP)


# 4. STATIONARITY TESTING
# -----------------------------------
# Augmented Dickey-Fuller, Phillips-Perron, and KPSS tests
# Original Series
adf.test(GDP)
pp.test(GDP)
kpss.test(GDP)

# First Difference
adf.test(diff(GDP))
pp.test(diff(GDP))
kpss.test(diff(GDP))

# Second Difference
adf.test(diff(diff(GDP)))
pp.test(diff(diff(GDP)))
kpss.test(diff(diff(GDP)))


# 5. ARIMA MODELING
# -----------------------------------
# Using auto.arima for baseline comparison
GDP_fit=auto.arima(GDP, ic="aic", trace = TRUE)
GDP_fit_diff=auto.arima(diff(GDP), ic="aic", trace = TRUE)
GDPmodeldiff2=auto.arima(diff(diff(GDP)), ic="aic", trace = TRUE)

auto.arima(GDP)
auto.arima(diff(GDP))

# Testing specific ARIMA specifications for Original Series
arima(GDP, order = c(1,0,0))
arima(GDP, order = c(1,1,0))
arima(GDP, order = c(2,0,0))
arima(GDP, order = c(0,1,1), include.mean = TRUE)
arima(GDP, order = c(0,1,2))

# Testing specific ARIMA specifications for First Difference
arima(diff(GDP), order = c(1,0,0))
arima(diff(GDP), order = c(2,0,0))
arima(diff(GDP), order = c(3,0,0))
arima(diff(GDP), order = c(0,0,1), include.mean = TRUE)
arima(diff(GDP), order = c(0,0,2), include.mean = TRUE)

# Final Selected Models
GDP_fit <- arima(GDP, order = c(0,1,1))
summary(GDP_fit)

GDP_diff_fit <- arima(diff(GDP), order = c(0,0,1), include.mean = TRUE)
summary(GDP_diff_fit)


# 6. RESIDUAL DIAGNOSTICS
# -----------------------------------
# Diagnostics for Model 1 (Original Series: ARIMA 0,1,1)
plot.ts(GDP_fit$residuals, main="Residuals Plot (ARIMA 0,1,1)", lwd=2, col=5)
hist(GDP_fit$residuals, main="Residuals Histogram", col=5)
boxplot(GDP_fit$residuals, main="Residuals Boxplot", col=5)

acf(GDP_fit$residuals, main="ACF of Residuals")
pacf(GDP_fit$residuals, main="PACF of Residuals")

# Statistical tests for residuals
Box.test(GDP_fit$residuals, lag = 10, type ="Ljung-Box")
shapiro.test(GDP_fit$residuals)
jarque.bera.test(GDP_fit$residuals)

# Diagnostics for Model 2 (First Difference: ARIMA 0,0,1)
plot.ts(GDP_diff_fit$residuals, main="Residuals Plot (Differenced Model)", lwd=2, col=5)
hist(GDP_diff_fit$residuals, main="Residuals Histogram", col=5)
boxplot(GDP_diff_fit$residuals, main="Residuals Boxplot", col=5)

residuals_diff <- residuals(GDP_diff_fit)
qqnorm(residuals_diff)
qqline(residuals_diff, col=2)

acf(GDP_diff_fit$residuals, main="ACF of Differenced Residuals")
pacf(GDP_diff_fit$residuals, main="PACF of Differenced Residuals")

Box.test(GDP_diff_fit$residuals, lag = 10, type ="Ljung-Box")
shapiro.test(GDP_diff_fit$residuals)
jarque.bera.test(GDP_diff_fit$residuals)
ArchTest(GDP_diff_fit$residuals)

# Plotting actual vs fitted values
plot.ts(GDP, main = "Annual GDP in Finland (1980-2023) - Actual vs Fitted", col = 5, lwd = 3, ylab = "GDP [mil. EUR]", xlab = "Year")
lines(fitted(GDP_fit), col = 2, lwd = 3)
legend("bottomright", legend = c("Actual", "Fitted"), col = c(5, 2), lwd = 2)

# Compare values side-by-side
cbind(GDP, fitted(GDP_fit))


# 7. FORECASTING
# -----------------------------------
# Forecasting the next 5 periods for the differenced model
GDP_fit_diff_forecast <- forecast(GDP_diff_fit, h=5); GDP_fit_diff_forecast
plot(GDP_fit_diff_forecast, main="5-Year Forecast (Differenced GDP)", col=5, lwd=3)

# Forecasting the next 5 periods for the original GDP model
GDP_fit_forecast <- forecast(GDP_fit, h=5); GDP_fit_forecast
plot(GDP_fit_forecast, main="5-Year Forecast for Finland's GDP", col=5, lwd=2)
