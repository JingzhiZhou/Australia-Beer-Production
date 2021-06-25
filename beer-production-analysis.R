library(readr)
library(astsa)
par(mfrow=c(1,1))
beer.production <- read_csv("C:/Users/11638/Desktop/5550data/monthly-beer-production-in-austr.csv")
production <- beer.production$`Monthly beer production`
beer <- ts(production,start = c(1956, 1), end = c(1995, 8),frequency = 12)


#TS plot
par(mfrow=c(2,1))
par(mar=c(3,5,1,1), mfrow=c(3,1))
plot(beer,main="Time Series Plot",xlab="Time (Monthly Data)", ylab="Beer Production in Megaliters",cex.main=0.8)
monthplot(beer,main="Monthly Time Series Plot", ylab="Beer Production in Megaliters",cex.main=0.8)
acf(beer,main="ACF of Beer Production",cex.main=0.8)

#diff with lag 12
par(mar=c(3,5,1,1), mfrow=c(2,1))
xt12 <- diff(beer,lag=12)
plot(xt12,main="Seasonal Differencing with Lag 12",ylab="")
acf(xt12,main="ACF plot:Lag 12",xlab="Time in Month")

#diff(diff.lag12)
xt <- diff(xt12)
plot(xt,main="Seasonal Differencing and First-order Difference") 
acf(xt)   #Not necessary

#log
yt <- log(beer)

#diff log
z<-diff(yt,lag=12)
plot(z)
acf(z)


#Model1-linear
par(mar=c(3,5,1,1), mfrow=c(2,1))
Time<-time(beer)
Model1 <- lm(beer~Time)
plot(beer, xlab="Time in Month")
abline(reg=Model1, col="red", lty=2)
plot(ts(resid(Model1), start=1956, end=c(1995,8), frequency=12),main="Detrended: linear regression",ylab="Detrended Value")
abline(h=0, col="blue", lty=3)

#mod1-3rd order  USE THIS
Time<-time(beer)
Time2<-Time^2
Time3<-Time^3
mod1 = lm(beer~Time+Time2+Time3)
summary(mod1)
plot(ts(resid(mod1), start=1956, end=c(1995,8), frequency=12), main="Detrended:  Third-order Polynomial",ylab="Detrended Value")
abline(h=0, col="blue", lty=3)



#Log-linear
Lbeer <- log(beer)
Model2 = lm(Lbeer~time(Lbeer))
plot(Lbeer, xlab="Time (monthly data)", ylab="Beer Production")
abline(reg=Model2, col="red", lty=2)
plot(ts(resid(Model2), start=1956, end=c(1995,8), frequency=12),ylab="Detrended Data")
abline(h=0, col="blue", lty=3)


#Log-3rd order
Time<-time(Lbeer)
Time2<-Time^2
Time3<-Time^3
Model2 = lm(Lbeer~Time+Time2+Time3)
plot(Lbeer,main="Time Series Plot")
plot(ts(resid(Model2), start=1956, end=c(1995,8), frequency=12), ylab="Detrended Data")
abline(h=0, col="blue", lty=3)

monthplot(ts(resid(mod1), start=1956, end=c(1995,8), frequency=12), ylab="Detrended Data")

#seasonal
par(mar=c(2,5,1,1), mfrow=c(2,1))
Month = factor(rep(1:12, 40))
levels(Month) = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
Month = Month[-(477:480)]
Time<-time(beer)
Time2<-Time^2
Time3<-Time^3
mod3 = lm(beer~ 0 + Time2 + Time3 + Time + Month)
plot(beer,main="Model for Trend and Seasonality")
lines(c(time(beer)), c(fitted(mod3)), col="blue", lwd=1, lty=2)

z = ts(resid(mod3), start=1956, end=c(1995,8), frequency=12)
plot(z, ylab="Residuals", main="Detrended and Deseasonalized Plot")
abline(h=0, lty=3)
 
acf(z, lag.max=60,main="", ylim=c(-1,1))
pacf(z, lag.max=60,main="", ylim=c(-1,1))

#ACF plot very bad. even suggests non-stationary. 

arma1 = sarima(z, 1,0,2, no.constant=T) 
arma1
arma1 = sarima(z, 1,0,1, no.constant=T) 
arma1
arma1 = sarima(z, 2,0,1, no.constant=T) 
arma1
arma1 = sarima(z, 2,0,2, no.constant=T) 
arma1 #MA1 not significant           

#all arma models are bad, try other ways



#---------------------------------------------
#deseasonalize+ARIMA
Month = factor(rep(1:12, 40))
levels(Month) = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
Month = Month[-(477:480)]

mod4 = lm(beer~ 0 + Month)
plot(beer, xlab="Time (monthly data)")
lines(c(time(beer)), c(fitted(mod4)), col="blue", lwd=1, lty=2)

de.se1 = ts(resid(mod4), start=1956, end=c(1995,8), frequency=12)
plot(de.se1, ylab="Residuals")
abline(h=0, lty=3)

acf(de.se1, main="", ylim=c(-1,1))
pacf(de.se1, main="", ylim=c(-1,1))

par(mfrow=c(3,1), mar=c(3,4,1,1))
plot(diff(de.se1), ylab="production")
acf(diff(de.se1),lag.max=60, main="diff once ACF") 
pacf(diff(de.se1),lag.max=60, main="diff once PACF")

plot(diff(de.se1, differences = 2), ylab="production")
acf(diff(de.se1, differences = 2),lag.max=60, main="diff once ACF") 
pacf(diff(de.se1 ,differences = 2),lag.max=60, main="diff once PACF")

#first order and second order diff are similar. The PACF plot for 2nd order may be better.
#But the difference in acf and pacf is not so significant to make choise. 
#Do further exploration through SARIMA command. 

#PACF tails off.
#The PACF clearly suggests MA(q); while sample ACF roughly follows the behavior of MA(1).
#The order of Moving Average is hard to define from the ACF plot for sure. 
#So a few plausible ARIMA models were tested. 

sarima(de.se1, 0,2,1, no.constant=T)  

sarima(de.se1, 0,2,2, no.constant=T) 
 
sarima(de.se1, 0,1,1, no.constant=T) 

sarima(de.se1, 0,1,2, no.constant=T) 

sarima(de.se1, 1,1,1, no.constant=T) 

sarima(de.se1, 1,1,2, no.constant=T) 

sarima(de.se1, 1,2,2, no.constant=T) 


#Set ARIMA model
#Seasonal effect mod4  
#Deseasonal model de.se1 
Modela = arima(de.se1, order=c(1,1,2), include.mean=F)
Modela


#------------------------------------------------------
#ARIMA Model Forecast

mod.pr = predict(Modela, n.ahead=24) #Predicted ARIMA
mod.pr

xhat = mod.pr$pr
pi.de.upper = mod.pr$pr + 2*mod.pr$se
pi.de.lower = mod.pr$pr - 2*mod.pr$se


par(mfrow=c(2,1), mar=c(2,2,2,2), cex=0.8)

plot(de.se1, ylab="Residuals", xlim=c(1980, 1998), main=expression("Forecasting "~x[t]))
points(xhat, col="red")
lines(pi.de.upper, lty=2, col="blue")
lines(pi.de.lower, lty=2, col="blue")


# Add back in seasonality
# Need  September 1995 through August 1997
newTime = seq(from=1995+8/12, to=1997+7/12, by=1/12)
newMonths = factor(rep(1:12, 3))
levels(newMonths) = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
newMonths = newMonths[-c(1:8, 33:36)]
newMonths

# Predicts the seasonality
seasonal = predict(mod4, newdata=data.frame(Time=newTime, Month=newMonths))

yhat = seasonal + xhat
pi.y.lower = seasonal + pi.de.lower
pi.y.upper = seasonal + pi.de.upper

plot(beer, xlim=c(1980, 1998), main=expression("Forecasting "~y[t]))
points(yhat, col="red")
lines(pi.y.upper, lty=2, col="blue")
lines(pi.y.lower, lty=2, col="blue")


#Predicted values and CI
yhat 
pi.y.lower
pi.y.upper



#-------------------------------------------------------------
#Sarima

par(mfrow=c(2,1), mar=c(3,4,1,1))
plot(diff(beer,lag=12), ylab="production")
acf(diff(beer,lag=12),lag.max=200, main="seasonal diff") #stationary, but a second diff needed
pacf(diff(beer,lag=12),lag.max=200, main="seasonal diff") 

acf(diff(diff(beer,lag=12)),lag.max=200, main="diff(seasonal)")
pacf(diff(diff(beer,lag=12)),lag.max=200, main="diff(seasonal)")

plot(diff(beer,lag=12,differences=2),ylab="production")
acf(diff(beer,lag=12,differences = 2),lag.max=200, main="seasonal diff") 
pacf(diff(beer,lag=12,differences = 2),lag.max=200, main="seasonal diff") 



#Fit SARIMA Model

sarima(beer,0,0,1,0,2,1,12, no.constant=T)      #better
sarima(beer,0,0,1,0,1,1,12, no.constant=T) 
sarima(beer,0,0,2,1,2,0,12, no.constant=T)
sarima(beer,0,0,2,0,2,1,12, no.constant=T) #normal&P
sarima(beer,1,1,1,1,1,1,12, no.constant=T) #Reduces aic, but has poor improvement on residuals

#The ACF of residuals and p values for Ljung-Box are bad for all SARIMA models
#(0,0,1)*(0,2,1)12 is the most appropraite SARIMA model.


#-----------------------------------------------------------------
#Forecast
sarima.for(beer,p = 0,d = 0,q = 1,P=0,D=2,Q=1,S=12,n.ahead=24)


SARIMA.pr = sarima.for(beer,p = 0,d = 0,q = 1,P=0,D=2,Q=1,S=12,n.ahead=24)
SARIMA.pr
yhat2 = SARIMA.pr$pred
pi.s.upper = SARIMA.pr$pred + 1.96*SARIMA.pr$se
pi.s.lower = SARIMA.pr$pred - 1.96*SARIMA.pr$se
yhat2
pi.s.lower
pi.s.upper

