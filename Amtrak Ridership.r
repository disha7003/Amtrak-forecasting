
Amtrak.data <- read.csv("C:/Users/11000306/Documents/Personal/MiniHack/R-code/Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991,1), end = c(2004, 3), freq = 12)
plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")

#Important Plots

library("forecast")

par(mfrow = c(3, 1))
seasonplot(ridership.ts, ylab="Ridership", 
           xlab="Year", main="Seasonal Plot", year.labels=TRUE)

monthplot(ridership.ts, ylab="Ridership", 
           xlab="Year", main="Seasonal Deviation Plot")

par(mfrow = c(2, 1))

lag.plot(ridership.ts, lags=16)

tsdisplay(ridership.ts)
plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")

plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")

##basic fitting
par(mfrow = c(3, 1))

plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")
ridership.lm <- tslm(ridership.ts ~ poly(trend, 1))
lines(ridership.lm$fitted, lwd = 2)

plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")
ridership.lm <- tslm(ridership.ts ~ poly(trend, 2))
lines(ridership.lm$fitted, lwd = 2)

plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")
ridership.lm <- tslm(ridership.ts ~ poly(trend, 3))
lines(ridership.lm$fitted, lwd = 2)

#differencing
par(mfrow = c(2, 2))

plot(ridership.ts, ylim = c(1300, 2200),  ylab = "Ridership", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", lty = 1)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))

diff.once1.ts <- diff(ridership.ts, lag = 1)

plot(diff.once1.ts, ylim = c(-400, 400),  ylab = "Ridership (Lag 1 Difference)", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", lty = 1)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))

diff.once12.ts <- diff(ridership.ts, lag = 12)

plot(diff.once12.ts, ylim = c(-400, 400),  ylab = "Ridership (Lag 12 Difference)", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", lty = 1)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))

diff.twice.ts <- diff(diff(ridership.ts, lag = 12), lag = 1)

plot(diff.twice.ts, ylim = c(-400, 400),  ylab = "Ridership (Twice-Differenced)", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", lty = 1)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))

par(mfrow = c(1, 1))

nValid <- 36
nTrain <- length(diff.twice.ts) - nValid
train.ts <- window(diff.twice.ts, start = c(1992, 2), end = c(1992, nTrain + 1))
valid.ts <- window(diff.twice.ts, start = c(1992, nTrain + 2), end = c(1992, nTrain + 1 + nValid))
ses <- auto.arima(train.ts)
ses.pred <- forecast(ses, h = nValid)
plot(ses.pred, ylim = c(-250, 300),  ylab = "Ridership (Twice-Differenced)", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ses.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

par(mfrow = c(1, 1))

ma.trailing <- rollmean(ridership.ts, k = 12, align = "right")
ma.centered <- ma(ridership.ts, order = 12)
plot(ridership.ts, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n",
     xlim = c(1991,2004.25), main = "")
axis(1, at = seq(1991, 2004.25, 1), labels = format(seq(1991, 2004.25, 1)))
lines(ma.centered, lwd = 2)
lines(ma.trailing, lwd = 2, lty = 2)
legend(1994,2600, c("Ridership","Centered Moving Average", "Trailing Moving Average"), lty=c(1,1,2),
       lwd=c(1,2,2), bty = "n")

nValid <- 36
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))
ma.trailing <- rollmean(train.ts, k = 12, align = "right")
last.ma <- tail(ma.trailing, 1)
ma.trailing.pred <- ts(rep(last.ma, nValid), start = c(1991, nTrain + 1),
                       end = c(1991, nTrain + nValid), freq = 12)
plot(train.ts, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n",
     xlim = c(1991,2006.25), main = "")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ma.trailing, lwd = 2, col = "blue")
lines(ma.trailing.pred, lwd = 2, col = "blue", lty = 2)
lines(valid.ts)

##Decompositon
fit1 <- decompose(ridership.ts, type="additive")
plot(fit1)

fit2 <- stl(ridership.ts, t.window = 12, s.window=24, robust=TRUE)
plot(fit2)

forecast2 <- forecast(fit2, h=26)
plot(forecast2)

#ETS
hw <- ets(train.ts, model = "MMA", restrict = FALSE)
plot(hw)
str(hw)

ESOpt <- ets(train.ts)
plot(ESOpt)
ESOpt

par(mfrow = c(2, 1))

hw.pred <- forecast(hw, h = nValid, level = 0)
plot(hw.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n",
     xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(hw.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

ESOpt.pred <- forecast(ESOpt, h = nValid, level = 0)
plot(ESOpt.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n",
     xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ESOpt.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

accuracy(hw.pred$mean, valid.ts)
accuracy(ESOpt.pred$mean, valid.ts)

##Arima

tsdisplay(train.ts)

fitARIMA <- arima(train.ts, order = c(1,0,0))
summary(fitARIMA)
Box.test(residuals(fitARIMA), lag=24, fitdf=1, type="Ljung-Box")

residualARIMA <- arima.errors(fitARIMA)
tsdisplay(residualARIMA)

par(mfrow = c(2, 1))
forecastARIMA <- forecast(fitARIMA, level=c(80,95), h=12)
plot(forecastARIMA)

diff.train.ts <- diff(train.ts, lag = 1)

tsdisplay(diff.train.ts)

fitSARIMA <- auto.arima(train.ts)#arima(train.ts, order = c(0,1,0), seasonal=c(1,0,0))
summary(fitSARIMA)
Box.test(residuals(fitSARIMA), lag=24, fitdf=1, type="Ljung-Box")

residualSARIMA <- arima.errors(fitSARIMA)
tsdisplay(residualSARIMA)

forecastSARIMA <- forecast(fitSARIMA, level=c(80,95), h=nValid)
plot(forecastSARIMA)

par(mfrow = c(2, 1))
hist(forecastSARIMA$residuals, ylab = "Frequency", xlab = "Fit Error", bty = "l", main = "")
hist(valid.ts - forecastSARIMA$mean, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")

accuracy(forecastSARIMA$mean, valid.ts)


