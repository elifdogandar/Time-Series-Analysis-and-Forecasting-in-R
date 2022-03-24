
library(zoo)
#library(ggfortify)
library(forecast)
library(tseries)

#library(fpp)

my_data = one_br_s_ts

#moving avarages
plot(my_data)
lines(stats::filter(my_data, c(1/4,1/4,1/4,1/4)), col = 2   )

ma_plot <- autoplot(my_data)+
  autolayer(my_data, series = "One Bedroom Apartments")+
  autolayer(stats::filter(my_data, c(1/4,1/4,1/4,1/4)), 
            col = 2, series = "Moving Avarages")+
  xlab("")+
  ylab("")+
  ggtitle("Moving Averages with a Window 4")+
  theme(legend.position = c(0.8, 0.1))+
  theme( text = element_text(size=15))
  
autoplot(stats::filter(my_data, c(rep(1, 12))/12))+
  autolayer(decompose(my_data)$trend, col=2)

#moving standard deviations
plot(rollapply(my_data, 12, sd), col=2)

msd_plot <- autoplot(rollapply(my_data, 12, sd), col = 2)+
  xlab("")+
  ylab("")+
  ggtitle("Moving Standard Deviations with a window 12")+
  theme( text = element_text(size=15))

ggarrange(ma_plot+ggtitle("Moving Averages with a Window 4")  ,
          msd_plot+ggtitle("Moving Standard Deviations with a Window 12") ,
          ncol = 2, nrow = 1)

#decomposition
autoplot(stl(one_br_s_ts, s.window = 13))+
  xlab("")+
  ggtitle("Decomposition of One Bedroom Apartments")+
  theme( text = element_text(size=15))


##############################################################
my_data = one_br_s_ts
training4 <- window(my_data, end=c(2020,11 ))
h <- length(my_data) - length(training4 )
test4 <- window(my_data, start=c(2020,12 ))


#exponential smoothing

dd_hw <- HoltWinters(training4)
dd_pred <- predict(HoltWinters(training4), n.ahead = h)


plot(dd_hw, dd_pred, ylim = range(my_data))+
lines(my_data)

#correlation plots

acf(my_data)
pacf(my_data)
acf(diff(my_data), ylim = c(-1, 1))
pacf(diff(my_data), ylim = c(-1, 1))

##############################################################

#####Avarage method
nd <- window(my_data, end = c(2020,11 ))

plot(meanf(training4,  h ), col=2)
lines(my_data , type="l", lty=2)
##############################################################

#Naive Method, random walk
nd <- window(my_data, end = c(2020,11 ))
plot(naive(nd, length(my_data) - length(nd)))
lines(my_data , type="l", lty=2)
##############################################################

#Seasonal Naive

nd <- window(my_data, end = c(2020,11 ))
plot(snaive(nd, length(my_data) - length(nd )), col=2)
lines(my_data , type="l", lty=2)
##############################################################

#Drift Model (Variation of Naive)
nd <- window(my_data, end = c(2020,11 ))
plot(rwf(my_data, length(my_data) - length(nd ), drift=TRUE), col=2)
lines(my_data , type="l", lty=2)
##############################################################

#Mean, naive, seasonal naive and drift
h=length(my_data) - length(nd )
autoplot(my_data) +
  autolayer(meanf(training4, h=h),
            series="Mean", PI=FALSE) +
  autolayer(naive(training4, h=h), 
            series="Naïve", PI=FALSE) +
  autolayer(snaive(training4, h=h),
            series="Seasonal naïve", PI=FALSE) +
  autolayer(rwf(training4, h=h, drift=TRUE),
            series="Drift", PI=FALSE) +
  ggtitle("Forecasts for Rent Rate") +
  xlab("") + ylab("Rent Rate (dollars)") +
  guides(colour=guide_legend(title="Forecast"))

#Autoregressive model

ar(my_data)

##############################################################

#ARIMA
#applied for my_data, log(my_data), diff(data): diagnostic problems

nd <- window(my_data, end = c(2020, 11))

nd_arima <- auto.arima(nd, trace= TRUE, d=1)

tsdiag(nd_arima)

nd_pred <- predict(nd_arima, n.ahead = 4)
plot(my_data, ylim=c(1700,2200))
lines(nd_pred$pred, col = 2)

#############################
#Residuals

res <- residuals(auto.arima(nd, trace= TRUE, d=2))
autoplot(res) + xlab("Year") + ylab("") +
  ggtitle("Residuals from arima")

gghistogram(res) + ggtitle("Histogram of residuals")
ggAcf(res) + ggtitle("ACF of residuals")

#Testing whether residuals are autocorrelated or not
Box.test(res, lag=15, fitdf=3)   # lag should be T/5 in our case
Box.test(res,lag=15, fitdf=3, type="Lj")

#### All at once
checkresiduals(auto.arima(nd, trace= TRUE, d=2))
###################################

#Accuracy measure
training <- window(my_data, end=c(2020,11 ))
test <- window(my_data, start=c(2020,12 ))
accuracy(arima(training,c(3,2,0)) )
accuracy( predict(  arima(training,c(3,2,0)) , n.ahead = 4) $pred    ,test )



####################################
#Effect of horizon
e <- tsCV(my_data, forecastfunction=naive, h=8)
# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = T)
# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()

##############################################################

##### Trend Method
training4 <- window(my_data, end=c(2020,11 ))
 
test4 <- window(my_data, start=c(2020,12 ))


fc_holt <-  holt(training4, h=h)
plot(my_data)
lines(fc_holt$mean, col = 2)

##############################################################

##### Damped Trend Method
fc_damp_holt <- holt(training4, damped=TRUE, h=h)
plot(my_data)
lines(fc_damp_holt$mean, col = 2)
lines(fc_holt$mean, col = 3)

##############################################################

### Holt Winters Seasonal Method
fc_hw_season_add <- hw(training4,damped = TRUE,seasonal="additive", h=h)
fc_hw_season_mult <- hw(training4,damped = TRUE,seasonal="multiplicative", h=h)
fc_damp_holt <- holt(training4, damped=TRUE, h=h)
fc_holt <-  holt(training4, h=h)
autoplot(my_data) +  
  autolayer( fc_hw_season_add, series="HW additive forecasts", PI=FALSE) +
  autolayer(fc_hw_season_mult, series="HW multiplicative forecasts", PI=FALSE) +
  autolayer(fc_damp_holt, series= "Holt's Damped Trend forecasts", PI=FALSE  )+
  autolayer( fc_holt, series= "Holt's Trend forecasts", PI=FALSE )+
  ylab("Rent Rate") +
  xlab("")+
  guides(colour=guide_legend(title="Forecast"))+ 
  theme(legend.position = c(0.85, 0.2))+
  theme( text = element_text(size=15))


autoplot(my_data)+
  autolayer(my_data, series = "Rental Rates")+
  autolayer(fc_hw_season_add$fitted, series = "HW Seasonal Additive Method")+
  xlab("") +
  ylab("Rent Rate")+ theme(legend.position = c(0.15, 0.9))

autoplot(my_data)+
  autolayer(my_data, series = "Rental Rates")+
  autolayer(fc_holt$fitted, series = "Holt's Trend Method")+
  xlab("") +
  ylab("Rent Rate")+ theme(legend.position = c(0.15, 0.9))

accuracy(fc_holt)
accuracy(fc_damp_holt)
accuracy(fc_hw_season_add)
accuracy(fc_hw_season_mult)

##############################################################

#Automatic exponential smoothing chosen by the algorithm
auto <-  ets(training4)

autoplot(my_data) +
  autolayer(forecast(auto, h=h), series="HW additive forecasts", PI=FALSE)

##############################################################

# ARIMA, AIC is only to choose p and q bec d changes the data

fit <- auto.arima(training4, d=1, seasonal=TRUE, stepwise = FALSE, 
                  approximation = FALSE, ic= "bic", trace=TRUE,max.P = 5,
                  max.Q = 5)
autoplot(my_data) +
  autolayer(forecast(fit, h=h), series="ARIMA(0,1,0)(0,0,1)12 forecasts", PI=FALSE)

acf(fit$residuals)
checkresiduals(fit$residuals)
Box.test(res,lag=15, fitdf=1, type="Lj")

##############################################################

#Combination approach
training4 <- window(my_data, end=c(2020,11 ))
h <- length(my_data) - length(training4 )


ETS <- forecast(ets(training4), h=h)
ARIMA <- forecast(auto.arima(training4, lambda=0, biasadj=TRUE),
                  h=h)
STL <- stlf(training4, lambda=0, h=h, biasadj=TRUE)
NNAR <- nnetar(training4)
NNAR <- forecast(nnetar(training4), h=h)
TBATS <- tbats(training4, biasadj=TRUE)
TBATS <- forecast(tbats(training4, biasadj=TRUE), h=h)
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] +
                  STL[["mean"]] + NNAR[["mean"]] + TBATS[["mean"]])/5


autoplot(my_data) +
  autolayer(ETS, series="ETS", PI=FALSE) +
  autolayer(ARIMA, series="ARIMA", PI=FALSE) +
  autolayer(STL, series="STL", PI=FALSE) +
  autolayer(NNAR, series="NNAR", PI=FALSE) +
  autolayer(TBATS, series="TBATS", PI=FALSE) +
  autolayer(Combination, series="Combination") +
  xlab("Year") + ylab("dollars") +
  ggtitle("Rent Rate")

##############################################################

my_data = one_br_s_ts
  
adf.test(my_data)  #my data is stationary

pp.test(my_data, type = "Z(t_alpha)") #my data is non-stationary

kpss.test(my_data) #my data is non-stationary


adf.test(diff(my_data))  #my data is stationary

pp.test(diff(my_data), type = "Z(t_alpha)") #my data is stationary

kpss.test(diff(my_data)) #my data is stationary

#Number of differences
ndiffs(my_data)

#Number of seasonal differences suggested
nsdiffs(my_data)

#So, I should be taking first differences

#studio and 1 br are not cointegrated
po.test(ts(cbind(std_s_ts, one_br_s_ts),
           start=c( year(unit_rent_master$StartDate)[1], 12),  frequency=12 ) )
#studio and 2 br are not cointegrated
po.test(ts(cbind(std_s_ts, two_br_ts),
           start=c( year(unit_rent_master$StartDate)[1], 12),  frequency=12 ) )
#2 br and 1 br are not cointegrated
po.test(ts(cbind(one_br_s_ts, two_br_ts),
           start=c( year(unit_rent_master$StartDate)[1], 12),  frequency=12 ) )

#studio_s and studio_m are cointegrated
po.test(ts(cbind(std_s_ts, std_m_ts),
           start=c( year(unit_rent_master$StartDate)[1], 12),  frequency=12 ) )

dd_struct <- StructTS(my_data)
plot(cbind(fitted(dd_struct), residuals(dd_struct)))

mp <- garch((my_data), c(2,2),grad = "numerical")
 summary(mp)
plot(mp)
# 
# 
# ggtsdiag(auto.arima(std_s_ts))
# ggfreqplot(std_s_ts)
# ggfreqplot(std_s_ts, freq = 4)
# 

