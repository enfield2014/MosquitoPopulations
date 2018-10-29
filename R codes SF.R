attach(data)
library(tseries)
library(nlme)
library(forecast)

library(readxl)
data <- read_excel("data.xlsx")


plot(data$Year, data$CPG, typ = "o", xlab = "Year", ylab = "Number of Mosquitoes Caught", main = "CPG", xaxt="n", pch=NA, data=data)
axis(side = 1, at=c(1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008))
#this tells the program to create its own x values, does NOT alter y side at all

points(Year, Aedes_tri, col = "red", pch=NA)
lines(Year, Aedes_tri, col = "red")
points(Year, An_punc, col = "blue", pch=NA)
lines(Year, An_punc, col = "blue")
points(Year, Aedes_vex, col = "orange", pch=NA)
lines(Year, Aedes_vex, col = "orange")
points(Year, Cx_tarsalis, col = "green", pch=NA)
lines(Year, Cx_tarsalis, col = "green")
legend("topleft", legend=c("CPG", "Aedes tri","An punc", "Aedes vexans", "Cx tar"), col=c("black", "black", "red","blue", "orange", "orange", "green"), lty=c(1,1,1,1,1), ncol=1, cex=0.70, pch=c(NA,NA,NA,NA,NA))

#AvRain
plot(Year, AvRain, typ = "o", xlab = "Year", ylab = "Average TotRain (in) in capture period", main = "TotRain Trends", xaxt="n", pch=NA)
axis(side = 1, at=c(1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008))

#TotRain

plot(Year, data$TotRain, typ = "o", xlab = "Year", ylab = "Total TotRain (in) in capture period", main = "TotRain Trends", xaxt="n", pch=NA)
axis(side = 1, at=c(1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008))


#heat
plot(Year, data$AvHeat, typ = "o", xlab = "Year", ylab = "Average GDD in capture period", main = "Temperature Trends", xaxt="n", pch=NA)
axis(side = 1, at=c(1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008))




#percentages
plot(Year, data$P_Ae_vex, typ = "o", col = "orange", xlab = "Year", ylab = "% of catch", main = "% Catch, by Species, State Forest IA", pch=NA, xaxt="n", ylim = c(-1, 100))
axis(side = 1, at=c(1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008))


points(Year, P_Ae_tr, col = "red", pch=NA)
lines(Year, P_Ae_tr, col = "red")
points(Year, P_An_punc, col = "blue", pch=NA)
lines(Year, P_An_punc, col = "blue")
points(Year, P_CPG, col = "black", pch=NA)
lines(Year, P_CPG, col = "black")
points(Year, P_Cx_tar, col = "green", pch=NA)
lines(Year, P_Cx_tar, col = "green")

legend("topleft",legend=c("CPG","Aedes tri","An punc", "Aedes vexans", "Cx tar"), col=c("black","red","blue", "orange", "green"), lty=c(1,1,1,1,1), ncol=1, cex=0.70)


## CPG + explanatory variables

plot(Year, CPG, typ = "o", col = "black", xlab = "Year", ylab = "Number caught", main = "Culex Trends, State Forest Nursey IA", pch=NA, xaxt="n")
axis(side = 1, at=c(1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008))



### Linear model fitting

## check for normality of distribution

qqnorm(CPG, pch= 1, frame = FALSE)
qqline(CPG, col = "steelblue", lwd = 2)

library(car)
qqPlot(CPG, main = "QqPlot of CPG")
#some of the points at the end are way off the plot, so this is non-normal

logCPop <- log(CPG)
LogTotRain <- log(data$TotRain)

qqPlot()
qqPlot(logCPop, main="QqPlot of Log CPG")
# log transformation normalizes the data


qqPlot(LogTotRain, main = "QqPlot of Log Total Rainfall")
qqPlot(TotRain, main = "QqPlot of Total Rainfall")
#normal

qqPlot(data$AvHeat, main = "QqPlot of Average GDD")
#normal

t<-seq(1, length(CPG))
plot(t, log(CPG), type = "o", main ="Log CPG plot")

# this relates to time series somehow but no idea what to do with it yet 
acf(CPG, lag.max = 37) #significant lags around 3-4
acf(LogCPoP, lag.max = 37, main = "ACF plot of LogCPG")
#significant lags around 2,3,4,22



# check for correlation of variables of interest (which would be bad)
cor(data$CPG, data$TotRain)  # -0.07717758
plot(data$CPG, data$TotRain)

cor(data$CPG, data$AvHeat)
plot(data$CPG, data$AvHeat) #0.2096717
# there is no correlation, so I can continue

cor(data$TotRain, data$AvHeat) # 0.04203509
plot(data$TotRain, data$AvHeat)


#scatterplot pairing
pairs(data)


#dotplot exploration
dotchart(data$CPG, ylab = "Order of Observations", xlab = "No. CPG", main = "CPG dotplot")

boxplot(data$CPG, ylab= "number of mosquitoes", xlab = "CPG", main = "CPG data Boxplot")




## time series



head(data)

library(tseries)
library(forecast)

tsdisplay(diff(CPG),main="")
tsdisplay(diff(log(CPG)),main="")

acf(residuals(mod13))## lag at 13 I think, maybe one at 9? 
pacf(residuals(mod13))## lag at 9 and 12
Box.test(residuals(mod13), lag=24, fitdf=4, type="Ljung")
# p value 0.02489 shows dependent variables, and we want a large p value :( 




## Subsetting before and after 1992
subset <- data[which(data$Year<=1992),]
View(subset)


mean(subset$CPG) #6913.3
mean(subset$TotRain) #20.618
mean(subset$AvHeat) #18.71266
sd(subset$CPG) #7201.563


POSTsub <- data[which(data$Year>1992),]
View(POSTsub)
mean(POSTsub$CPG) #1225.312
mean(POSTsub$TotRain) #23.35837
mean(POSTsub$AvHeat) #18.62175
sd(POSTsub$CPG) #1334.471



View(POSTsub)

View(data)

PostPop = log(POSTsub$CPG)
PrePop = log(subset$CPG)




# Threw in some AR1 
mod1 <- gls(log(CPG) ~ AvHeat*log(TotRain) + Year, correlation = corAR1 (form = ~1), data=subset)
summary(mod1)
#       AIC      BIC    logLik
#   73.81077   78.76712 -29.90539

mod2 <- lm(log(CPG) ~ Year + log(TotRain)*AvHeat, data=subset)
summary(mod2)
# Multiple R-squared:  0.2041

mod3 <- lm(log(CPG) ~ Year, data=subset)
summary(mod3)
#Multiple R-squared:  0.003703




mod4 <- lm(log(CPG) ~ Year + log(TotRain) + AvHeat, data=data)
summary(mod4)
## there is a downward trend, due to negative year estimate and it's stat significant 
#Multiple R-squared:  0.3344

modA <- gls(log(CPG) ~ AvHeat*log(TotRain) + Year, data=data)
summary(modA)


modB <- gls(log(CPG) ~ AvHeat*log(TotRain) + Year, correlation = corAR1 (form = ~1), data=data)
summary(modB)

View(subset)

## Model 1 plotted

plot(subset$Year, log(subset$CPG), xlab="Year",
     ylab="Log CPG Population", type="l", col=4, lty=2,
     xaxt="n", lwd=2, main = "Model 1 (Interaction + corAR1)")
axis(side = 1, at=c(1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008), labels=levels(data$Year))
lines(subset$Year, mod1$fitted, col=2, lwd=2)
legend("topleft", legend=c("data", "fitted"), lty=c(2,1), col=c(4,2))


plot(subset$Year, log(subset$CPG), xlab="Year",
     ylab="Log CPG Population", type="l", col=4, lty=2,
     xaxt="n", lwd=2, main = "Model 2 (Interaction only)")
axis(side = 1, at=c(1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008), labels=levels(data$Year))
lines(subset$Year, mod2$fitted, col=2, lwd=2)
legend("topleft", legend=c("data", "fitted"), lty=c(2,1), col=c(4,2))



plot(data$Year, log(data$CPG), xlab="Year",
     ylab="Log CPG Population", type="l", col=4, lty=2,
     xaxt="n", lwd=2, main = "Model B (full data set with AR1)")
axis(side = 1, at=c(1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008), labels=levels(data$Year))
lines(data$Year, modB$fitted, col=2, lwd=2)
legend("topleft", legend=c("data", "fitted"), lty=c(2,1), col=c(4,2))

#trying the predict function

newdata <- predict(mod2, POSTsub, interval ="predict")
head(newdata)


lwr = newdata[,2]

head(lwr)

upr = newdata[,3] 

View(upr)

## adding the newdata to a column, PREDICTION VALUES (so POST VALUES)

data$Lognewdata<-NA
View(data)


Item4 <- newdata
Item5 <- as.vector(Item4)


class(data)
data$Lognewdata <- rep(NA, 36)
data$Lognewdata[1:16] <- NA
View(data)
data$Lognewdata[17:36] <- Item5
View(data)





### putting the new values into columns, for plotting 
data$LogLwr<-NA
View(data)


Item4 <- lwr
Item5 <- as.vector(Item4)


class(data)
data$LogLwr <- rep(NA, 36)
data$LogLwr[1:16] <- NA
View(data)
data$LogLwr[17:36] <- Item5
View(data)


## upper

### putting the new values into columns, for plotting 
data$LogUpr<-NA
View(data)


Item4 <- upr
Item5 <- as.vector(Item4)


class(data)
data$LogUpr <- rep(NA, 36)
data$LogUpr[1:16] <- NA
View(data)
data$LogUpr[17:36] <- Item5
View(data)




## checking


CHKdata <- predict(mod2, subset, interval ="predict")
View(CHKdata)

data$LogCHK=NA
View(data)


Item4 <- CHKdata
Item5 <- as.vector(Item4)


data$LogCHK <- rep(NA, 36)
data$Lognewdata[1:16] <- Item5
View(data)

lwrCHK <- CHKdata[,2]
uprCHK <- CHKdata[,3]

View(lwrCHK)



data$CHKlwr=NA


Item4 <- lwrCHK
Item5 <- as.vector(Item4)


class(data)
data$CHKlwr <- rep(NA, 36)
data$LogLwr[1:16] <- Item5
View(data)

data$CHKupr=NA


Item4 <- uprCHK
Item5 <- as.vector(Item4)


class(data)
data$CHKupr <- rep(NA, 36)
data$LogUpr[1:16] <- Item5
View(data)




## all together now! with back and forwards checking! 


plot(data$Year, log(data$CPG), typ = "l", xlab = "Year", ylab = "log Number of Mosquitoes Caught", main = "logCPG in State Forest Nursery, with regression prediction estimates", xaxt="n", pch=NA, lty=1, col = "black", ylim=c(-6,26), lwd=2)
axis(side = 1, at=c(1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008))


points(data$Year, data$LogLwr, col = "blue", pch=NA)
lines(data$Year, data$LogLwr, col = "blue", lty=2, lwd=2)

points(data$Year, data$Lognewdata, col = "red", pch=NA)
lines(data$Year, data$Lognewdata, col = "red", lty=2, lwd=2)

points(data$Year, data$LogUpr, col = "blue", pch=NA)
lines(data$Year, data$LogUpr, col = "blue", lty=2, lwd=2)


legend("topleft", legend=c("historical data", "prediction", "95% CI"), lty=c(1,2, 2), lwd=c(2,2,2), col=c("black", "red", "blue"))

# 93 is a crazy year because they had crazy rainfall counts. 




#THIS STEP NEEDS TO BE AMENDED, BECAUSE BACK TRASNFORMATION GIVES THE MEDIAN VALUE.Plus it makes stuff go crazy. 

data$BackTrans<-NA
View(data)


Item4 <- exp(CHKdata)
Item5 <- as.vector(Item4)


class(data)
data$BackTrans <- rep(NA, 36)
data$BackTrans[1:16] <- Item5
View(data)
data$BackTrans[17:36] <- NA
View(data)

Item4 <- exp(newdata)
Item5 <- as.vector(Item4)


data$BackTrans[17:36] <- Item5
View(data)


## :/// 

plot(data$Year, data$CPG, typ = "o", xlab = "Year", ylab = "Number of Mosquitoes Caught", main = "CPG Trends in State Forest Nursery, IA with Model 2", xaxt="n", pch=NA)
axis(side = 1, at=c(1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008))


points(data$Year, data$BackTrans, col = "red", pch=NA)
lines(data$Year, data$BackTrans, col = "red", lty=2, cex=70)
legend("topleft", legend=c("historical data", "prediction"), lty=c(1,2), col=c("black", "red"))



### ALL DATA CHECK


alldata <- predict(mod4, data, interval ="predict")
View(alldata)


lwr = alldata[,2]

View(lwr)

upr = alldata[,3]

View(upr)

## adding the newdata to a column

data$LogPred<-NA
View(data)


Item4 <- alldata
Item5 <- as.vector(Item4)


class(data)
data$LogPred <- rep(NA, 36)
data$LogPred[1:36] <- Item5
View(data)





### putting the new values into columns, for plotting 
data$LogLwrA<-NA
View(data)


Item4 <- lwr
Item5 <- as.vector(Item4)


class(data)
data$LogLwrA <- rep(NA, 36)
data$LogLwrA[1:36] <- Item5
View(data)


## upper

### putting the new values into columns, for plotting 
data$LogUprA<-NA
View(data)


Item4 <- upr
Item5 <- as.vector(Item4)


class(data)
data$LogUprA <- rep(NA, 36)
data$LogUprA[1:36] <- Item5




plot(data$Year, log(data$CPG), typ = "l", xlab = "Year", ylab = "log Number of Mosquitoes Caught", main = "logCPG in State Forest Nursery, with regression prediction estimates", xaxt="n", pch=NA, lty=1, col = "black", ylim=c(-6,26), lwd=2)
axis(side = 1, at=c(1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008))


points(data$Year, data$LogLwrA, col = "blue", pch=NA)
lines(data$Year, data$LogLwrA, col = "blue", lty=2, lwd=2)

points(data$Year, data$LogPred, col = "red", pch=NA)
lines(data$Year, data$LogPred, col = "red", lty=2, lwd=2)

points(data$Year, data$LogUprA, col = "blue", pch=NA)
lines(data$Year, data$LogUprA, col = "blue", lty=2, lwd=2)


legend("topleft", legend=c("historical data", "prediction", "95% CI"), lty=c(1,2, 2), lwd=c(2,2,2), col=c("black", "red", "blue"))





## validate this prediction http://rcompanion.org/handbook/G_14.html

library(rcompanion)

if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(betareg)){install.packages("betareg")}
if(!require(nlme)){install.packages("nlme")}
if(!require(lme4)){install.packages("lme4")}
if(!require(quantreg)){install.packages("quantreg")}
if(!require(mgcv)){install.packages("mgcv")}
if(!require(MASS)){install.packages("MASS")}
if(!require(robust)){install.packages("robust")}

par(mfrow=c(1,2))
accuracy(list(mod1, mod2),
         plotit=TRUE, digits=2)

par(mfrow=c(1,2)) 
plotPredy(data  = subset,
          x     = CPG,
          y     = AvHeat,
          model = mod1,
          xlab  = "CPG",
          ylab  = "Heat",
          main = "Magnitude of Accuracy Measures, mod1")


plotPredy(data  = subset,
          x     = CPG,
          y     = TotRain,
          model = mod1,
          xlab  = "CPG",
          ylab  = "TotRain")


## MODEL CHECKING

par(mfrow=c(1,1))

##this will show homogeneity
plot(mod2, add.smooth = FALSE, which = 1, main = "Homogeneity, mod2")

E<- resid(mod2)

## this shows normality
hist(E, xlab = "Residuals", main = "Check for normality")

## residuals vs explanatory variables to check independence
par(mfrow=c(1,2))
plot(subset$TotRain, E, xlab= "TotRain", ylab = "Residuals", main = "Check for independence, Rain")

plot(subset$AvHeat, E, xlab= "AvHeat", ylab = "Residuals", main = "Check for independence, Temp")

## Residuasl vs year
par(mfrow=c(1,1))
plot(subset$Year, E, xlab = "Year", ylab = "Residuals", main="residuals over time")



## Faraway's F test
E1 <- E[data$TotRain <= 2.75]
E2 <- E[data$AvHeat > 2.75]
var.test(E1, E2)

#not enough x observations

## looking at the variables
plot(data$Year, data$TotRain, typ = "o", col = "blue", pch=NA, ylab ="Total TotRain, inches", xlab ="Year", xaxt="n", main = "TotRain and Temperature Trends, State Forest Nursery IA")
axis(side = 1, at=c(1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008))
par(new = TRUE)
plot(data$Year, data$AvHeat, typ="o", col = "red", pch=NA, xaxt="n", yaxt="n", ylab ="", xlab ="")
axis(side = 4)



PostPop = POSTsub$CPG
PrePop = subset$CPG


## time series stuff
tsdisplay(diff(PrePop),main="TS plot CPGsubset")
acf(CPGsub) #autolag at 1, nothing elsewhere
pacf(CPGsub) #no lags anywhere
#lag at 1 and that's it





## ARIMA
auto.arima(PrePop)

fitt <- auto.arima(PrePop)
fit1 <- arima(PrePop, c(0,0,1))
fit2 <- arima(PrePop, c(1,0,1))
fit3 <- arima(PrePop, c(1,1,1))
fit4 <- arima(PrePop, c(1,1,0))
fit4 <- arima(PrePop, c(0,1,1))
fit5 <- arima(PrePop, c(0,0,2))
fit6 <- arima(PrePop, c(2,0,2))
fit7 <- arima(PrePop, c(2,2,2))
fit8 <- arima(PrePop, c(2,2,0))
fit9 <- arima(PrePop, c(0,2,2))

fit10 <- arima(PrePop, c(2,1,0))
fit11 <- arima(PrePop, c(2,2,1))
fit12 <- arima(PrePop, c(1,2,2))
fit13 <- arima(PrePop, c(1,1,2))
fit14 <- arima(PrePop, c(1,2,1))
fit15 <- arima(PrePop, c(1,2,2))

library(forecast)
fittt <- auto.arima(PrePop,max.p = 5,max.q = 5,max.P = 5,max.Q = 5,max.d = 3,seasonal = FALSE,ic = 'aicc')
plot(forecast(fittt,h=20))
print(fitt)

AIC(fitt) #54.12
AIC(fit1) #54.36
AIC(fit2) #55.38
AIC(fit3) #51.59
AIC(fit4) #49.59
AIC(fitt4) #50.00
AIC(fit5) #55.18
AIC(fit6)
AIC(fit7)
AIC(fit8) #49.15347 but I don't think this is appropriate

# ARIMA data ## ended up using what auto-arima predicted, the fittt model
acf(residuals(fittt))
adf.test(PrePop, alternative = "stationary")
#Dickey-Fuller = -2.9385, Lag order = 2, p-value = 0.2148
#alternative hypothesis: stationary

Box.test(residuals(fit1), lag=24, fitdf=4, type="Ljung")
#X-squared = NA, df = 20, p-value = NA

plot(forecast(fit14, 7), main = "", ylab = "Population count", xlab = "Catch Year")



data$ARIMAMEAN<-NA
View(data)


Item4 <- 8011.312
Item5 <- as.vector(Item4)


class(data)
data$ARIMAMEAN <- rep(NA, 36)
data$ARIMAMEAN[20:36] <- Item5




plot(data$Year, data$CPG, typ = "o", col = "black", xlab = "Year", ylab = "Number caught", main = "Culex Trends, State Forest Nursey IA with ARIMA", pch=NA, xaxt="n")
axis(side = 1, at=c(1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008))

points(data$Year, data$ARIMAMEAN, col = "red", pch=NA)
lines(data$Year, data$ARIMAMEAN, col = "red")

legend("topleft",legend=c("CPG","CPG forecast"), col=c("black","red"), lty=c(1,2), ncol=1, cex=0.70)

