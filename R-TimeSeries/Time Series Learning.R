data("AirPassengers")
class(AirPassengers)

# Time series have property with start & end & frequency
start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)   # Show frequency of the times series
summary(AirPassengers)

# Time series plot & lm plot
plot(AirPassengers)
abline(lm(AirPassengers ~ time(AirPassengers)), col = "red")  # plot liner regression line

cycle(AirPassengers)  # Show cycle (12 months in a year) and time series
plot(aggregate(AirPassengers, FUN = mean))  # Agggregate group data by each cycle (year)
boxplot(AirPassengers~cycle(AirPassengers))  # Boxplot by each cycle (month) across years

# Analysis Time Series
# 1) Plot series
plot(AirPassengers)

# 2) test stationary with Dikey-Fuller test
library(tseries)
# Before test, remove varience by taking log & remove trend component by difference the series
adf.test(diff(log(AirPassengers)), alternative = "stationary", k = 0)
# 3) Find optimal parameter (p, d, q) with ACF plot
acf(AirPassengers) 
acf(log(AirPassengers))  # d = 1; difference 1 lag -> stationary
acf(diff(log(AirPassengers)))  # p = 1; ACF show MA series
pacf(diff(log(AirPassengers)))  # q = 1 or 2; ACF show switch side every 1, or 2 lag

# 4) Fit the model with parameter
# p = 0, d = 1, q = 1
fit <- arima(log(AirPassengers), c(0, 1, 1), seasonal = list(order = c(0, 1,1)))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(AirPassengers, 2.718^pred$pred, log = "y", lty = c(1,3))

# A) Moving MA Methods
# A.1) Use decompose with moving ma to decompose the component
de.ts <- decompose(AirPassengers)
plot(de.ts)

# A.2) De-Seasonal with seasonality
AirPassengersDeSeason <- AirPassengers - de.ts$seasonal
par(mfrow = c(2,1))
plot(AirPassengers)
plot(AirPassengersDeSeason)

dev.off()
