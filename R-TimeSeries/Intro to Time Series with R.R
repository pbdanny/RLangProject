# From book : Introductory Time Series with R (Use R) By Paul S.P. Cowpertwait, Andrew V. Metcalfe
# Chapte I
data("AirPassengers")
AP <- AirPassengers
rm(AirPassengers)
class(AP)

# Time Series class = 'ts' with class-methods:- start, end, frequency and summary
start(AP); end(AP); frequency(AP)
summary(AP)

# Create time series plot
plot(AP, ylab = "Passenger (1000's)", xaxt = "n")
axis(1, at = seq(start(AP)[1] , end(AP)[1]+1, by = 1), las = 2)  # Adjust x-tick label

# Aggregate time series resulted in collapse data into annual basis
plot(aggregate(AP))

# Boxplo by cycle 
# cycle = serialized the data into loop between frequency
cycle(AP)
plot(AP ~ cycle(AP))

www <- "http://www.math.pku.edu.cn/teachers/xirb/Courses/TimeSeries/IntroTimeSeriesRData/Maine.dat"
Maine.month <- read.table(www, header = TRUE)

# Transform vector in data frame (Maine.month$unemploy) to time series object with ts()
# Specify start = c(year, # month) and frequency
Maine.month.ts <- ts(Maine.month$unemploy, start = c(1996, 1), frequency = 12)

plot(Maine.month.ts, xaxt = "n")
axis(1, xaxp = c(start(Maine.month.ts)[1], end(Maine.month.ts)[1]+1 , 10), las = 2)

plot(Maine.month.ts, xaxt = "n")
axis(1, at = seq(start(Maine.month.ts)[1], end(Maine.month.ts)[1]+1 , by = 1), las = 2)  # Another way to adjust x-tick label

# Aggrgrate data into annual but need to divided by frequency (no of month in year)
Maine.annual.ts <- aggregate(Maine.month.ts)/frequency(Maine.month.ts)

par(mfcol = c(2,1))
plot(Maine.month.ts, ylab = "unemployed (%)")
# axis(1, at = seq(start(Maine.month.ts)[1] , end(Maine.month.ts)[1]+1, by = 1/12), las = 2, cex.axis = .5)
plot(Maine.annual.ts, ylab = "unemployed (%)")

# Extract each cycle data with window command 
Maine.Feb <- window(Maine.month.ts, start = c(1996, 2), frequency = TRUE)
Maine.Aug <- window(Maine.month.ts, start = c(1996, 8), frequency = TRUE)
plot(Maine.Feb, ylim = c(0, 8))
lines(Maine.Aug, col = "red")

# Find ratio to monthly average by using mean on monthly data
Feb.ratio <- mean(Maine.Feb)/mean(Maine.month.ts)
Aug.ratio <- mean(Maine.Aug)/mean(Maine.month.ts)
Feb.ratio; Aug.ratio

# Plot decomposition of the series
z <- decompose(Maine.month.ts)
plot(z$trend)
plot(z$seasonal)

# US unemployment data
www <- "http://www.math.pku.edu.cn/teachers/xirb/Courses/TimeSeries/IntroTimeSeriesRData/USunemp.dat"
US.month <- read.table(www, header = T)

US.month.ts <- ts(US.month$USun, start = c(1996, 1), frequency = 12)
plot(US.month.ts, ylab = "unemployment (%)", main = "US unemployment (Month)")

# Electricity & Beer and Chocolate data
www <- "http://www.math.pku.edu.cn/teachers/xirb/Courses/TimeSeries/IntroTimeSeriesRData/cbe.dat"
CBE <- read.table(www, header = T)

elec.ts <- ts(CBE$elec, start = 1958, frequency = 12)
beer.ts <- ts(CBE$beer, start = 1958, frequency = 12)
choc.ts <- ts(CBE$choc, start = 1958, frequency = 12)
plot(cbind(elec.ts, choc.ts, beer.ts))

# Intersection of time series analysis
AP.elec <- ts.intersect(AP, elec.ts)
start(AP.elec); end(AP.elec)
head(AP.elec)

# Time Series Plot
par(mfrow = c(2,1))
plot(AP.elec[, 1], ylab = "Air Passenger (1000)")
plot(AP.elec[, 2], ylab = "Electricity Production (MkWh)")
dev.off()

# Scatter plot with regression line
plot(as.vector(AP.elec[, 1]), as.vector(AP.elec[, 2]),
     xlab = "Air Passenger (1000)", ylab = "Electricity Production (MkWh)")
abline(reg = lm(AP.elec[, 2] ~ AP.elec[, 1]), col = "red")
cor(AP.elec[, 2], AP.elec[, 1])

# English Pound & NZ Dolla exchange rate
www <- "http://www.math.pku.edu.cn/teachers/xirb/Courses/TimeSeries/IntroTimeSeriesRData/pounds_nz.dat"
Z <- read.table(www, header = T)
head(Z)
Z.ts <- ts(Z$xrate, start = 1991, frequency = 4)  # Quaterly frequency = 4
plot(Z.ts, xlab = "time/year",
     ylab = "Quarterly exchange rate $NZ / GB pound")
# Trend cut-off at year 1996, split data into 2 time series
Z.92_96 <- window(Z.ts, start = c(1992, 1), end = c(1996, 1))
Z.96_98 <- window(Z.ts, start = c(1996, 1))
layout(1:2)
# par(mfrow = c(2, 1))
plot(Z.92_96)
abline(reg = lm(Z.92_96 ~ time(Z.92_96)), col = "red")
plot(Z.96_98)
abline(reg = lm(Z.96_98 ~ time(Z.96_98)), col = "red")
dev.off()

# Climante change
www <- "http://www.math.pku.edu.cn/teachers/xirb/Courses/TimeSeries/IntroTimeSeriesRData/global.dat"
Global <- scan(www)  # Serialize data into vector
Global.ts <- ts(Global, start = c(1856, 1), end = c(2005, 12), frequency = 12)  # Change data to timeseris
Global.annual <- aggregate(Global.ts, FUN = mean)
plot(Global.ts)
plot(Global.annual)

# Plot on the upward tredn (1970 - )
New.ts <- window(Global.annual, start = 1970)
plot(New.ts); abline(reg = lm(New.ts ~ time(New.ts)), col = "red")

# Decomposition in R
www <- "http://www.math.pku.edu.cn/teachers/xirb/Courses/TimeSeries/IntroTimeSeriesRData/cbe.dat"
CBE <- read.table(www, header = T)

elec.ts <- ts(CBE$elec, start = 1958, frequency = 12)
beer.ts <- ts(CBE$beer, start = 1958, frequency = 12)
choc.ts <- ts(CBE$choc, start = 1958, frequency = 12)

plot(decompose(elec.ts))  # Error (random) varied by time then seasonlity change along time
elec.decom <- decompose(elec.ts, type = "multiplicative")  # Use mulitiplicative for seasonality varied with time
plot(elec.decom)
Trend <- elec.decom$trend
Seasonal <- elec.decom$seasonal
ts.plot(cbind(Trend, Trend * Seasonal), lty = 1:2)

# Exercise 1
plot(choc.ts, main = "Chocolate Production (Monthly)", ylab = "Chocolate (1000's kg)")
plot(aggregate(choc.ts, FUN = mean), main = "Chocolate Production (Annualy)", ylab = "Chocolate (1000's kg)")
boxplot(choc.ts ~ cycle(choc.ts), xaxt = "n", main = "Choclate Production by cycle (Month)", 
        ylab = "Production (1000's kg)")
axis(1, at = 1:12, labels = month.abb)
plot(decompose(choc.ts))
plot(decompose(choc.ts, type = "multi"))
choc.decom <- decompose(choc.ts, type = "multi")
ts.plot(cbind(choc.decom$trend, choc.decom$trend * choc.decom$seasonal), lty = 1:2)

# Exercise 2 : Laspeyre Price Index
car <- data.frame(q = c(.33, .5), p = c(18000, 20000), row.names = c("2000", "2004"))
pet <- data.frame(q = c(2000, 1500), p = c(.8, 1.6), row.names = c("2000", "2004"))
serv <- data.frame(q = c(40, 20), p = c(40, 60), row.names = c("2000", "2004"))
tyre <- data.frame(q = c(3, 2) , p = c(80, 120), row.names = c("2000", "2004"))
clutch <- data.frame(q = c(2, 1), p = c(200, 360), row.names = c("2000", "2004"))

l.base.amt <- sum(car$q[1]*car$p[1], pet$q[1]*pet$p[1], serv$q[1]*serv$p[1], tyre$q[1]*tyre$p[1], clutch$q[1]*clutch$p[1])/5
l.new.amt <- sum(car$q[1]*car$p[2], pet$q[1]*pet$p[2], serv$q[1]*serv$p[2], tyre$q[1]*tyre$p[2], clutch$q[1]*clutch$p[2])/5
l.price.idx = l.new.amt/l.base.amt

# Exercise 3 : Paasche Price Index
p.base.amt <- sum(car$q[2]*car$p[1], pet$q[2]*pet$p[1], serv$q[2]*serv$p[1], tyre$q[2]*tyre$p[1], clutch$q[2]*clutch$p[1])/5
p.new.amt <- mean(car$q[2]*car$p[2], pet$q[2]*pet$p[2], serv$q[2]*serv$p[2], tyre$q[2]*tyre$p[2], clutch$q[2]*clutch$p[2])/5
p.price.idx = p.new.amt/p.base.amt

ir.price.idx <- sqrt(l.price.idx*p.price.idx)
