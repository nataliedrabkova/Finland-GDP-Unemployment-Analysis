#############################################################
# PROJECT: Multivariate Time Series Analysis (VAR) 
#          Finland's GDP and Unemployment (2009-2023)
#############################################################

# 1. ENVIRONMENT SETUP & DATA LOADING
# -----------------------------------
library(readxl)
library(vars)
library(tsDyn)
library(multiwave)
library(mlVAR)
library(MTS)
library(tseries)
library(forecast)
library(urca)

# Load the first dataset (Levels/Original data)
data_GDP_unemp1 <- read_excel("finland.xlsx", sheet="GDP_unemp")
GDP1 <- ts(data_GDP_unemp1$HDP, start=c(2009,1), frequency=4) # Quarterly data
unemp1 <- ts(data_GDP_unemp1$nezamestnanost, start=c(2009,1), frequency=4)
unemp_diff <- diff(unemp1)
gdp_unemp1 <- cbind(GDP1, unemp1) # Multivariate time series object

# Load the second dataset (Differenced unemployment data)
data_GDP_unemp <- read_excel("finland.xlsx", sheet="GDP_diff_unemp") 
GDP <- ts(data_GDP_unemp$HDP, start=c(2009,2), frequency=4)
unemp <- ts(data_GDP_unemp$diference, start=c(2009,2), frequency=4) # Differenced unemp.
gdp_unemp <- cbind(GDP, unemp)


# 2. EXPLORATORY DATA ANALYSIS (EDA)
# -----------------------------------
# Plotting the original series (Levels)
plot.ts(GDP1, main="Quarter-on-Quarter % Growth of Finland's GDP", col=5, lwd=3, ylab="Percentage (%)", xlab="Quarter")
plot.ts(unemp1, main="Unemployment Rate in Finland (%)", col=5, lwd=3, ylab="% of Labor Force", xlab="Quarter")
plot.ts(ts.union(GDP1, unemp1), main="Multivariate Time Series (Original Data)") 
plot.ts(gdp_unemp1, main="Multivariate Time Series Overlay", col=5, lwd=3) 

# Plotting the differenced series
plot.ts(GDP, main="Quarter-on-Quarter % Growth of Finland's GDP", col=5, lwd=3, ylab="Percentage (%)", xlab="Quarter")
plot.ts(unemp, main="First Difference of Finland's Unemployment Rate", col=5, lwd=3, ylab="% of Labor Force (Differenced)", xlab="Quarter")
plot.ts(ts.union(GDP, unemp), main="Multivariate Time Series (Differenced Data)") 
plot.ts(gdp_unemp, main="Multivariate Time Series Overlay (Differenced Data)", col=5, lwd=3)


# 3. CORRELOGRAMS (ACF & PACF)
# -----------------------------------
layout(matrix(1:2,2,1)) # Set plot layout to 2 rows, 1 column (2x1)

# Original Series
acf(GDP1, main="ACF for QoQ % Growth of Finland's GDP")
pacf(GDP1, main="PACF for QoQ % Growth of Finland's GDP")

acf(unemp1, main="ACF for Unemployment Rate in Finland")
pacf(unemp1, main="PACF for Unemployment Rate in Finland")

# Differenced Series
acf(GDP, main="ACF for QoQ % Growth of Finland's GDP")
pacf(GDP, main="PACF for QoQ % Growth of Finland's GDP")

acf(unemp, main="ACF for First Difference of Unemployment Rate")
pacf(unemp, main="PACF for First Difference of Unemployment Rate")

layout(matrix(1:1,1,1)) # Reset plot layout (1x1)


# 4. STATIONARITY TESTING
# -----------------------------------
# Augmented Dickey-Fuller, Phillips-Perron, and KPSS tests
# Original Series
adf.test(GDP1)
pp.test(GDP1)
kpss.test(GDP1)

adf.test(unemp1)
pp.test(unemp1)
kpss.test(unemp1)

adf.test(diff(unemp1))
pp.test(diff(unemp1))
kpss.test(diff(unemp1))

# Differenced Series (Used for VAR)
adf.test(GDP)
pp.test(GDP)
kpss.test(GDP)

adf.test(unemp)
pp.test(unemp)
kpss.test(unemp)


# 5. CROSS-CORRELATION ANALYSIS
# -----------------------------------
# Multivariate ACF and PACF matrices
acf(gdp_unemp)
pacf(gdp_unemp)

# Cross-correlation function between GDP and Unemployment
ccf(GDP, unemp, main="Cross-Correlation between GDP and Unemployment")


# 6. VAR MODEL SELECTION & ESTIMATION
# -----------------------------------
# Automatic lag selection criteria (AIC, HQ, SC, FPE)
VARselect(gdp_unemp, type = "const")
VARselect(gdp_unemp, type = "trend")
VARselect(gdp_unemp, type = "both")
VARselect(gdp_unemp, type = "none")

# Manual testing of various lag lengths (p) and deterministic terms
# Inspecting models to find the best fit. Most higher lags are discarded (#).
var_test1 <- VAR(gdp_unemp, p = 1, type = "const"); summary(var_test1)
var_test2 <- VAR(gdp_unemp, p = 1, type = "trend"); summary(var_test2)
var_test3 <- VAR(gdp_unemp, p = 1, type = "both"); summary(var_test3)
var_test4 <- VAR(gdp_unemp, p = 1, type = "none"); summary(var_test4)

# var_test1 <- VAR(gdp_unemp, p = 2, type = "const"); summary(var_test1) 
# var_test2 <- VAR(gdp_unemp, p = 2, type = "trend"); summary(var_test2)
# var_test3 <- VAR(gdp_unemp, p = 2, type = "both"); summary(var_test3)
# var_test4 <- VAR(gdp_unemp, p = 2, type = "none"); summary(var_test4)

# var_test1 <- VAR(gdp_unemp, p = 3, type = "const"); summary(var_test1)
# var_test2 <- VAR(gdp_unemp, p = 3, type = "trend"); summary(var_test2)
# var_test3 <- VAR(gdp_unemp, p = 3, type = "both"); summary(var_test3)
# var_test4 <- VAR(gdp_unemp, p = 3, type = "none"); summary(var_test4)

# var_test1 <- VAR(gdp_unemp, p = 4, type = "const"); summary(var_test1)
# var_test2 <- VAR(gdp_unemp, p = 4, type = "trend"); summary(var_test2)
# var_test3 <- VAR(gdp_unemp, p = 4, type = "both"); summary(var_test3)
# var_test4 <- VAR(gdp_unemp, p = 4, type = "none"); summary(var_test4)


# Testing higher order lags (results generally unsatisfactory)

# Removing temporary test models to clear environment
remove(var_test1, var_test2, var_test3, var_test4)

# DEFINING THE FINAL SELECTED MODEL
# Based on the criteria above, we select p=1 and type="none"
var_final <- VAR(gdp_unemp, p = 1, type = "none")
summary(var_final)


# 7. VAR MODEL DIAGNOSTICS
# -----------------------------------
plot(var_final)

# Portmanteau Test for serial correlation in residuals
serial.test(var_final)
plot(serial.test(var_final))

# ARCH (Heteroskedasticity) and Normality tests for residuals
arch.test(var_final)
normality.test(var_final)


# 8. FORECAST ERROR VARIANCE DECOMPOSITION (FEVD)
# -----------------------------------------------
# Analyzes the contribution of each variable's shock to the variance of the forecast error
fevd_model <- fevd(var_final, n.ahead = 10)
plot(fevd_model, main="Variance Decomposition")


# 9. MODEL STABILITY (STRUCTURAL BREAKS)
# --------------------------------------
# Testing the structural stability of the estimated VAR parameters
plot(stability(var_final, type = "Rec-CUSUM"), main="CUSUM Stability Test")


# 10. FORECASTING
# -----------------------------------
# Short-term forecast (1 quarter ahead)
predict(var_final, n.ahead = 1, ci = 0.95)
plot(predict(var_final, n.ahead = 1, ci = 0.95), main="1-Quarter Ahead Forecast")

# Long-term forecast (10 quarters ahead)
predict(var_final, n.ahead = 10, ci = 0.95)
plot(predict(var_final, n.ahead = 10, ci = 0.95), main="10-Quarter Ahead Forecast")


# 11. IMPULSE-RESPONSE FUNCTION (IRF) ANALYSIS
# --------------------------------------------
# Tracing the effect of a one-time shock to one of the innovations on current and future values
# Shock to GDP -> Response in Unemployment
irf_gdp_unemp <- irf(var_final, impulse = "GDP", response = "unemp", boot = TRUE)
plot(irf_gdp_unemp, main="Impulse: GDP, Response: Unemployment")

# Shock to Unemployment -> Response in GDP
irf_unemp_gdp <- irf(var_final, impulse = "unemp", response = "GDP", boot = TRUE)
plot(irf_unemp_gdp, main="Impulse: Unemployment, Response: GDP")


# 12. GRANGER CAUSALITY TESTS
# -----------------------------------
# Testing if one time series is useful in forecasting another
causality(var_final, cause="GDP")
causality(var_final, cause="unemp")




