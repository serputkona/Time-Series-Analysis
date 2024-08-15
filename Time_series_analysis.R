getwd()
gs  <- read.csv("GoldSilver.csv")
gs
library(astsa)
library(ggplot2)
library(TTR)
library(forecast)
library(rlang)
#remove.packages("rlang")
#remove.packages("ggplot2")

gs = read.csv('GoldSilver.csv')
data = ts(gs$gold, gs$time, start = c(1990, 12), end = c(2010, 12), frequency = 12)
plot(data)

ggseasonplot(data)
ggseasonplot(data, polar = T)

ggsubseriesplot(data) +  ggtitle("Діаграма сезонних підрядів")

plot.ts(data)
plot.ts(SMA(data, n=3))
plot.ts(SMA(data, n=40))
plot.ts(SMA(data, n=100))

#lam <- BoxCox.lambda(data)
#lam
#Air.2transf <- cbind("Степенева"= BoxCox(data, lam),
 #"Степінь = 0.99" = BoxCox(data, lambda = .99),
 #"Логарифмічна" = BoxCox(data, lambda = 0))
#autoplot(Air.2transf, facets = T)+ xlab("Рік")

DecDATA <- decompose(data)
plot(DecDATA)

#Корелограма та частинна корелограма для часового ряду
acf(data)
pacf(data)

acf(diff(data))
pacf(diff(data))

#Наївні прогнози: простий, зсунутий, сезонний наївний, на базі середнього значення
#data.1 <- meanf(data, h=10)
data.2 <- rwf(data, h=10)
#data.3 <- snaive(data, h=10)
data.4 <- rwf(data, drift = TRUE, h=10)
autoplot(window(data, start=1990)) +
 #autolayer(data.1, series="Mean", PI=FALSE) +
 autolayer(data.2, series="Naïve", PI=FALSE) +
 #autolayer(data.3, series="Seasonal naïve", PI=FALSE) +
 autolayer(data.4, series="Drift method", PI=FALSE) +
 xlab("Years") + ylab("CPI") +
 ggtitle(" ")


#Подвійне експоненційне згладжування(прогноз):
hw1 <- HoltWinters(data, beta = FALSE, gamma =0, seasonal = c("additive"))
hw1
plot(hw1)

data.hw.for <- forecast(hw1, h=12)
plot(data.hw.for)

?HoltWinters


d= diff(data)
arima.d <- auto.arima(d)
autoplot(forecast(arima.d))

res.data.naive <- residuals(snaive(diff(data)))
autoplot(res.data.naive)

gghistogram(res.data.naive) + ggtitle("Гістограма залишків")

ggAcf(res.data.naive)+ ggtitle("Корелограма залишків")

Box.test(res.data.naive, lag=20)

checkresiduals(snaive(diff(data)))
