###########
# PROJEKT #
###########

# DATOVÝ SOUBOR - data_HDP_nez - Finsko
############################################


library(readxl)
library(vars)
library(tsDyn)
library(multiwave)
library(mlVAR)
library(MTS)
library(tseries)
library(forecast)
library(urca)


data_HDP_nez1 <- read_excel("MECR/finsko.xlsx", sheet="HDP_nez") # načtení datového souboru z Excelu
HDP1 <- ts(data_HDP_nez1$HDP, start=c(2009,1), frequency=4) # data jako časová řada
nezamestnanost1 <- ts(data_HDP_nez1$nezamestnanost, start=c(2009,1), frequency=4) # data jako časová řada
nez_diff <- diff(nezamestnanost1)
hdpnez1 <- cbind(HDP1, nezamestnanost1)


data_HDP_nez <- read_excel("MECR/finsko.xlsx", sheet="HDP_diff_nez") # načtení datového souboru z Excelu
HDP <- ts(data_HDP_nez$HDP, start=c(2009,2), frequency=4) # data jako časová řada
nezamestnanost <- ts(data_HDP_nez$diference, start=c(2009,2), frequency=4) # data jako časová řada
hdpnez <- cbind(HDP, nezamestnanost)


# Grafické zobrazení časové řady
#-------------------------------

plot.ts(HDP1, main="Mezičtvrtletní procentní růsty HDP Finska", col=5, lwd=3, ylab="procenta", xlab="období")
plot.ts(nezamestnanost1, main="Nezaměstnanosti Finska v %", col=5, lwd=3, ylab="procento pracovní síly populace", xlab="období")
plot.ts(ts.union(HDP1, nezamestnanost1), main="Grafické zobrazení vícerozměrné časové řady") # zobrazení vícerozměné časové řady
plot.ts(hdpnez1, main="Grafické zobrazení vícerozměrné časové řady", col=5, lwd=3) # zobrazení vícerozměrné časové řady

plot.ts(HDP, main="Mezičtvrtletní procentní růsty HDP Finska", col=5, lwd=3, ylab="procenta", xlab="období")
plot.ts(nezamestnanost, main="První diference nezaměstnanosti Finska v %", col=5, lwd=3, ylab="procento pracovní síly populace", xlab="období")
plot.ts(ts.union(HDP, nezamestnanost), main="Grafické zobrazení vícerozměrné časové řady") # zobrazení vícerozměné časové řady
plot.ts(hdpnez, main="Grafické zobrazení vícerozměrné časové řady", col=5, lwd=3) # zobrazení vícerozměrné časové řady


# Korelogramy pro jednotlivé řady
#--------------------------------

layout(matrix(1:2,2,1)) # nastavení výstupního okna pro grafy 2x1

acf(HDP1, main="Autokorelační funkce (ACF) pro Mezičtvrtletní procentní růsty HDP Finska")
pacf(HDP1, main="Parciální autokorelační funkce (PACF) pro Mezičtvrtletní procentní růsty HDP Finska")

acf(nezamestnanost1, main="Autokorelační funkce (ACF) pro nezaměstnanost Finska")
pacf(nezamestnanost1, main="Parciální autokorelační funkce (PACF) pro nezaměstnanost Finska")

acf(HDP, main="Autokorelační funkce (ACF) pro Mezičtvrtletní procentní růsty HDP Finska")
pacf(HDP, main="Parciální autokorelační funkce (PACF) pro Mezičtvrtletní procentní růsty HDP Finska")

acf(nezamestnanost, main="Autokorelační funkce (ACF) pro první diference nezaměstnanosti Finska")
pacf(nezamestnanost, main="Parciální autokorelační funkce (PACF) pro první diference nezaměstnanosti Finska")

layout(matrix(1:1,1,1)) # nastavení výstupního okna pro grafy do původni podoby

adf.test(HDP1)
pp.test(HDP1)
kpss.test(HDP1)

adf.test(nezamestnanost1)
pp.test(nezamestnanost1)
kpss.test(nezamestnanost1)

adf.test(diff(nezamestnanost1))
pp.test(diff(nezamestnanost1))
kpss.test(diff(nezamestnanost1))

adf.test(HDP)
pp.test(HDP)
kpss.test(HDP)

adf.test(nezamestnanost)
pp.test(nezamestnanost)
kpss.test(nezamestnanost)

# Graf korelační a parciální korelační maticové funkce
#-----------------------------------------------------

acf(hdpnez)
pacf(hdpnez)


# Křížový korelogram
#-------------------

ccf(HDP, nezamestnanost)


# Automatický návrh VAR modelu
#-----------------------------

VARselect(hdpnez, type = "const")
VARselect(hdpnez, type = "trend")
VARselect(hdpnez, type = "both")
VARselect(hdpnez, type = "none")


 # Odhad VAR modelu
#-----------------

hdpm1.odhad1 <- VAR(hdpnez, p = 1, type = "const"); summary(hdpm1.odhad1)
hdpm1.odhad2 <- VAR(hdpnez, p = 1, type = "trend"); summary(hdpm1.odhad2)
hdpm1.odhad3 <- VAR(hdpnez, p = 1, type = "both"); summary(hdpm1.odhad3)
hdpm1.odhad4 <- VAR(hdpnez, p = 1, type = "none"); summary(hdpm1.odhad4)

hdpm1.odhad1 <- VAR(hdpnez, p = 2, type = "const"); summary(hdpm1.odhad1)   #no
hdpm1.odhad2 <- VAR(hdpnez, p = 2, type = "trend"); summary(hdpm1.odhad2)   #no
hdpm1.odhad3 <- VAR(hdpnez, p = 2, type = "both"); summary(hdpm1.odhad3)    #no
hdpm1.odhad4 <- VAR(hdpnez, p = 2, type = "none"); summary(hdpm1.odhad4)    #no

hdpm1.odhad1 <- VAR(hdpnez, p = 3, type = "const"); summary(hdpm1.odhad1)   #no
hdpm1.odhad2 <- VAR(hdpnez, p = 3, type = "trend"); summary(hdpm1.odhad2)   #no
hdpm1.odhad3 <- VAR(hdpnez, p = 3, type = "both"); summary(hdpm1.odhad3)    #no
hdpm1.odhad4 <- VAR(hdpnez, p = 3, type = "none"); summary(hdpm1.odhad4)    #no

hdpm1.odhad1 <- VAR(hdpnez, p = 4, type = "const"); summary(hdpm1.odhad1)   #no
hdpm1.odhad2 <- VAR(hdpnez, p = 4, type = "trend"); summary(hdpm1.odhad2)   #no
hdpm1.odhad3 <- VAR(hdpnez, p = 4, type = "both"); summary(hdpm1.odhad3)    #no
hdpm1.odhad4 <- VAR(hdpnez, p = 4, type = "none"); summary(hdpm1.odhad4)    #no

hdpm1.odhad1 <- VAR(hdpnez, p = 5, type = "const"); summary(hdpm1.odhad1)   #no
hdpm1.odhad2 <- VAR(hdpnez, p = 5, type = "trend"); summary(hdpm1.odhad2)   #no
hdpm1.odhad3 <- VAR(hdpnez, p = 5, type = "both"); summary(hdpm1.odhad3)    #no
hdpm1.odhad4 <- VAR(hdpnez, p = 5, type = "none"); summary(hdpm1.odhad4)    #no

hdpm1.odhad1 <- VAR(hdpnez, p = 6, type = "const"); summary(hdpm1.odhad1)   #no
hdpm1.odhad2 <- VAR(hdpnez, p = 6, type = "trend"); summary(hdpm1.odhad2)   #no
hdpm1.odhad3 <- VAR(hdpnez, p = 6, type = "both"); summary(hdpm1.odhad3)    #no
hdpm1.odhad4 <- VAR(hdpnez, p = 6, type = "none"); summary(hdpm1.odhad4)    #no

hdpm1.odhad1 <- VAR(hdpnez, p = 7, type = "const"); summary(hdpm1.odhad1)   #no
hdpm1.odhad2 <- VAR(hdpnez, p = 7, type = "trend"); summary(hdpm1.odhad2)   #no
hdpm1.odhad3 <- VAR(hdpnez, p = 7, type = "both"); summary(hdpm1.odhad3)    #no
hdpm1.odhad4 <- VAR(hdpnez, p = 7, type = "none"); summary(hdpm1.odhad4)    #no

hdpm1.odhad1 <- VAR(hdpnez, p = 8, type = "const"); summary(hdpm1.odhad1)   #no
hdpm1.odhad2 <- VAR(hdpnez, p = 8, type = "trend"); summary(hdpm1.odhad2)   #no
hdpm1.odhad3 <- VAR(hdpnez, p = 8, type = "both"); summary(hdpm1.odhad3)    #no
hdpm1.odhad4 <- VAR(hdpnez, p = 8, type = "none"); summary(hdpm1.odhad4)    #no

remove(hdpm1.odhad1, hdpm1.odhad2, hdpm1.odhad3, hdpm1.odhad4)

# Analýza zvoleného VAR modelu
#-----------------------------

plot(hdpm1.odhad4)
serial.test(hdpm1.odhad4)
plot(serial.test(hdpm1.odhad4))
arch.test(hdpm1.odhad4)
normality.test(hdpm1.odhad4)

# Dekompozice chyby předpovědi
#-----------------------------

fevd(hdpm1.odhad4, n.ahead = 10)
plot(fevd(hdpm1.odhad4, n.ahead = 10))


  # Stabilita parameteru VAR modelu
#--------------------------------

plot(stability(hdpm1.odhad4, type = "Rec-CUSUM"))


# Predikce VAR modelu
#--------------------
predict(hdpm1.odhad4, n.ahead = 1, ci = 0.95)
plot(predict(hdpm1.odhad4, n.ahead = 1, ci = 0.95))

predict(hdpm1.odhad4, n.ahead = 10, ci = 0.95)
plot(predict(hdpm1.odhad4, n.ahead = 10, ci = 0.95))


# Impulse-Response analýza
#-------------------------

irf(hdpm1.odhad4, impulse = "HDP", response = "nezamestnanost", boot = TRUE)
plot(irf(hdpm1.odhad4, impulse = "HDP", response = "nezamestnanost", boot = TRUE))

irf(hdpm1.odhad4, impulse = "nezamestnanost", response = "HDP", boot = TRUE)
plot(irf(hdpm1.odhad4, impulse = "nezamestnanost", response = "HDP", boot = TRUE))


# Grangerova kauzalita
#---------------------

causality(hdpm1.odhad4, cause="HDP")
causality(hdpm1.odhad4, cause="nezamestnanost")




