###########
# PROJEKT #
###########

# DATOVÝ SOUBOR - data_HDP - Finsko
############################################

library(readxl)
library(tseries)
library(forecast)
library(urca)
library(FinTS)

data_HDP <- read_excel("~/MECR/data_HDPfin.xlsx", sheet = "finsko")
HDP <- ts(data_HDP$HDP, start = 1980, frequency = 1)

summary(HDP)

plot.ts(HDP, main="Roční HDP ve Finsku v období 1980-2023", col=5, lwd=3, ylab="HDP v mil. eur", xlab="období")
plot.ts(diff(HDP), main="První diference HDP ve Finsku v období 1980-2023", col=5, lwd=3, ylab="HDP", xlab="období")
plot.ts(diff(diff(HDP)), main="Druhé diference HDP ve Finsku v období 1980-2023", col=5, lwd=3, ylab="HDP", xlab="období")

var(HDP)
var(diff(HDP))
var(diff(diff(HDP)))#vyšší rozptyl!!!

layout(matrix(1:2,2,1))

acf(HDP, main="Autokorelační funkce pro roční HDP ve Finsku v obodbí 1980-2023")
pacf(HDP, main="Parciální autokorelační funkce pro pro roční HDP ve Finsku v obodbí 1980-2023")

acf(diff(HDP), main="Autokorelační funkce pro první diference ročního HDP ve Finsku v obodbí 1980-2023")
pacf(diff(HDP), main="Parciální autokorelační funkce pro první diference ročního HDP ve Finsku v obodbí 1980-2023")

acf(diff(diff(HDP)), main="Autokorelační funkce pro druhé diference ročního HDP ve Finsku v obodbí 1980-2023")
pacf(diff(diff(HDP)), main="Parciální autokorelační funkce pro druhé diference ročního HDP ve Finsku v obodbí 1980-2023")

layout(matrix(1:1,1,1))

ArchTest(HDP)
ArchTest(diff(HDP))

time <- ts(1:44, start = 1980, frequency = 1)
tslm.lin_HDP <- tslm(HDP ~ trend)
summary(tslm.lin_HDP)
plot.ts(HDP, main="Roční HDP Finska")
lines(fitted(tslm.lin_HDP), col="blue", lwd=2)

library(strucchange)
res1_HDP <- Fstats(HDP ~ 1+time, from = 0.20) # F-statistika Chowova testu
sctest(res1_HDP) # test strukturalniho zlomu na zaklade maxima F-statistiky Chowova testu
plot(res1_HDP, main="F-statistika Chowova testu")
#zobrazí vývoj a vyhledá kde by mohl být zlom
lines(breakpoints(res1_HDP))
breakpoints(res1_HDP) # datovani zlomu

res2_HDP <- breakpoints(HDP ~ 1+time, h=0.25); res2_HDP # vypocet optimilniho poctu zlomu, delka segmentu 25 %
summary(res2_HDP)
plot(res2_HDP, main="Volba počtu strukturálních zlomů", xlab="počet strukturálních zlomů")
#zapojení více zlomů

res3_HDP <- confint(res2_HDP); res3_HDP # interval spolehlivosti
plot(HDP, main="Znázornění strukturálních zlomů", ylab="HDP v mil. eur", xlab="období", col="grey", lwd=2)
lines(res2_HDP)
lines(res3_HDP, col=1)
lines(fitted(res2_HDP), col=2, lty=1, lwd=2)
coef(res2_HDP) # parametry trendovych primek v jednotlivych castech casove rady
#rozdělení čř do období s různou směrnicí

adf.test(HDP)
pp.test(HDP)
kpss.test(HDP)

adf.test(diff(HDP))
pp.test(diff(HDP))
kpss.test(diff(HDP))

adf.test(diff(diff(HDP)))
pp.test(diff(diff(HDP)))
kpss.test(diff(diff(HDP)))


HDP_fit=auto.arima(HDP, ic="aic", trace = TRUE)
HDP_fit_diff=auto.arima(diff(HDP), ic="aic", trace = TRUE)
HDPmodeldiff2=auto.arima(diff(diff(HDP)), ic="aic", trace = TRUE)

auto.arima(HDP)
auto.arima(diff(HDP))


arima(HDP, order = c(1,0,0))
arima(HDP, order = c(1,1,0))
arima(HDP, order = c(2,0,0))
arima(HDP, order = c(0,1,1), include.mean = TRUE)
arima(HDP, order = c(0,1,2))

arima(diff(HDP), order = c(1,0,0))
arima(diff(HDP), order = c(2,0,0))
arima(diff(HDP), order = c(3,0,0))
arima(diff(HDP), order = c(0,0,1), include.mean = TRUE)
arima(diff(HDP), order = c(0,0,2), include.mean = TRUE)

remove(HDP_fit_forecast)
HDP_fit <- arima(HDP, order = c(0,1,1))
summary(HDP_fit)

HDP_diff_fit <- arima(diff(HDP), order = c(0,0,1), include.mean = TRUE)
summary(HDP_diff_fit)


plot.ts(HDP_fit$residuals, main="Graf reziduí", lwd=2, col=5)
hist(HDP_fit$residuals, main="Histogram reziduí", col=5)
boxplot(HDP_fit$residuals, main="Krabicový graf reziduí", col=5)

acf(HDP_fit$residuals, main="Autokorelační funkce reziduí")
pacf(HDP_fit$residuals, main="Parciální autokorelační funkce reziduí")

Box.test(HDP_fit$residuals, lag = 10, type ="Ljung-Box")
shapiro.test(HDP_fit$residuals)
jarque.bera.test(HDP_fit$residuals)



plot.ts(HDP_fit_diff$residuals, main="Graf reziduí", lwd=2, col=5)
hist(HDP_fit_diff$residuals, main="Histogram reziduí", col=5)
boxplot(HDP_fit_diff$residuals, main="Krabicový graf reziduí", col=5)

residuals <- residuals(HDP_fit_diff)
qqnorm(residuals)
qqline(residuals, col=2)

acf(HDP_fit_diff$residuals, main="Autokorelační funkce reziduí")
pacf(HDP_fit_diff$residuals, main="Parciální autokorelační funkce reziduí")

Box.test(HDP_fit_diff$residuals, lag = 10, type ="Ljung-Box")
shapiro.test(HDP_fit_diff$residuals)
jarque.bera.test(HDP_fit_diff$residuals)
ArchTest(HDP_fit_diff$residuals)

plot.ts(HDP, main = "Roční HDP ve Finsku v období 1980-2023", col = 5, lwd = 3, ylab = "HDP v mil. eur", xlab = "období")
lines(HDP_fit$fitted, col = 2, lwd = 3)
legend("bottomright", legend = c("skutečné", "vyrovnané"), col = c(5, 2), lwd = 2)

cbind(HDP, HDP_fit$fitted) 


# Predikce časové řady
#---------------------

HDP_fit_diff_forecast <- forecast(HDP_fit_diff, h=5); HDP_fit_diff_forecast
plot(HDP_fit_diff_forecast, col=5, lwd=3)

HDP_fit_forecast <- forecast(HDP_fit, h=5); HDP_fit_forecast
plot(HDP_fit_forecast, col=5, lwd=2)


