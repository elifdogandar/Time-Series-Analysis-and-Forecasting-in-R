#install.packages("vars")
#install.packages("hts")

my_data = std_s_ts
training4 =  window(my_data, end=c(2020,11 ))
test4 <- window(my_data, start=c(2020,12 ))

cpi_data=c(231.033,
231.612,232.432,233.052,233.236,233.462,233.640,233.792,234.258,234.782,235.162,235.243,235.000,
235.367,236.075,236.913,237.509,238.029,238.157,238.138,238.296,238.841,239.413,239.248,238.775,
239.248,240.083,241.067,241.802,242.119,242.354,242.436,242.651,243.359,243.985,244.075,243.779,
244.528,245.680,246.358,246.992,247.544,247.794,247.744,248.278,248.731,249.218,249.227,249.134,
250.083,251.143,251.290,251.642,251.835,252.014,251.936,252.460,252.941,253.638,253.492,253.558,
254.638,255.783,256.610,257.025,257.469,257.697,257.867,258.012,258.429,259.063,259.105,259.083,
260.122,261.114,261.836,262.332,262.590,263.177,263.566,264.169,264.522,265.059,265.108,264.935,
266.004,267.268,267.312,266.089,265.799,266.302,267.703,268.756,269.054,269.328,269.473,269.226,
269.755,270.696,271.713)
cpi = ts(cpi_data, start= c(2014,12), end=c(2021,03) ,freq=12 )
autoplot(cpi)+
  ggtitle("Consumer Price Index for All Urban Consumers in America")

years = c(rep(0,26),rep(1,50))
years[years==0] = "early"
years[years==1] = "late"

ts_all <- ts(cbind(rental_rate = std_s_ts, occupancy = occ_df_monthly_std$occupancy[34:109], CPI=cpi
                   ,years = years    ),
              start=c( 2014, 12),  frequency=12) 

ts_all <- ts(cbind(rental_rate = std_s_ts, occupancy = occ_df_monthly_std$occupancy[34:109],
                   CPI=cpi    ),
             start=c( 2014, 12),  frequency=12) 

###### Time vs exploratory variables
autoplot(ts_all[,1:2], facets=TRUE) +
  xlab("") + ylab("") +
  ggtitle("")

qplot( occupancy,std_s_ts, data=as.data.frame(ts_all), aes(color=years) ) + 
  ylab("Rental Rate") + xlab("Occupancy")

GGally::ggpairs(as.data.frame(ts_all[,1:3]))

occ_std <- occ_df_monthly_std$occupancy
occ_one <- occ_df_monthly_1br$occupancy
occ_two <- occ_df_monthly_2br$occupancy

occ_gen = ts(occ_df_monthly$occupancy[34:109],start= c(2014,12), end=c(2021,03) ,freq=12  )
occ_std = ts(occ_std[34:109] ,start= c(2014,12), end=c(2021,03) ,freq=12  )
occ_one = ts(occ_one[34:109] ,start= c(2014,12), end=c(2021,03) ,freq=12  )
occ_two = ts(occ_two[34:109] ,start= c(2014,12), end=c(2021,03) ,freq=12  )

ggarrange(autoplot(occ_std*600)+
            ggtitle("Studio Apartments Occupancy")+
            xlab("")+
            ylab(""),
          autoplot(std_s_ts)+
            ggtitle("Studio Apartments Rental Rates")+
            xlab("")+
            ylab("") ,
          ncol = 1, nrow = 2)

ggarrange(autoplot(occ_one*400)+
            ggtitle("One Bedroom Apartments Occupancy")+
            xlab("")+
            ylab(""),
          autoplot(one_br_s_ts)+
            ggtitle("One Bedroom Apartments Rental Rates")+
            xlab("")+
            ylab("") ,
          ncol = 1, nrow = 2)

ggarrange(autoplot(occ_two*100)+
            ggtitle("Two Bedroom Apartments Occupancy")+
            xlab("")+
            ylab(""),
          autoplot(two_br_ts)+
            ggtitle("Two Bedroom Apartments Rental Rates")+
            xlab("")+
            ylab("") ,
          ncol = 1, nrow = 2)

xreg1 <- occ_std
xreg2 = cbind( occ_std , cpi)

xreg1_training4   <- window(occ_std, end=c(2020,11 ))
xreg1_test4 <-  window(occ_std, start=c(2020,12 ))

cpi_training4 <- window(cpi, end=c(2020,11 ))
cpi_test4 <-  window(cpi, start=c(2020,12 ))
xreg2_training4 <- cbind(xreg1_training4,cpi_training4 )
xreg2_test4 <- cbind(xreg1_test4,cpi_test4 )
#Arima with regressors

arima1 <- auto.arima(training4, xreg = xreg1_training4 )
checkresiduals(arima1)

autoplot(std_s_ts)+
 
  autolayer(arima1$fitted, series = "ARIMA fit")+
  autolayer(forecast(arima1, xreg= xreg1_test4  ), series="forecast")+
  autolayer(std_s_ts, series= "Rental Rates")+
  theme(legend.position = c(0.7, 0.85))+
  xlab("")+
  ylab("")
  

arima2d1 <- auto.arima(training4, xreg = xreg2_training4, d=1 )
checkresiduals(arima1d1)
accuracy(arima2d1)

arima2 <- auto.arima(training4, xreg = xreg2_training4 )
checkresiduals(arima2)
  
cbind("Regression Errors" = residuals(arima1, type="regression"),
      "ARIMA errors" = residuals(arima1, type="innovation")) %>%
  autoplot(facets=TRUE)

checkresiduals(fit)

training <- window(my_data, end=c(2020,12 ))
xreg_training <- window(xreg, end=c(2020,12 ))
training2 <- window(my_data, end=c(2020,6 )) 
xreg_training2 <- window(xreg ,end=c(2020,6 )) 
test <- window(my_data, start=c(2021,1 ))
test2 <- window(my_data, start=c(2020,7 ))

fcast <- forecast(arima1, xreg= xreg1_test4  )
autoplot(fcast) + 
  autolayer(my_data)+
  xlab("") +
  ylab("Rent Rate")

#Dynamic Harmonic regression 

plots <- list()
for (i in seq(6)) {
  fit <- auto.arima(training4, xreg = fourier(training4 , K = i),
                    seasonal = TRUE, lambda = 0)
  checkresiduals(fit)
  print(accuracy)
  plots[[i]] <- autoplot(forecast(fit,
                                  xreg=fourier(training4, K=i, h=9))) +
    autolayer(my_data)+
    xlab(paste("K=",i,"   AICC=",round(fit[["aicc"]],2))) +
    ylab("") 
}
gridExtra::grid.arrange(
  plots[[1]],plots[[2]],plots[[3]],
  plots[[4]],plots[[5]],plots[[6]], nrow=3)

#Lagged Predictors
 ts_both <- ts(cbind(training2, xreg_training2),
             start=c( 2014, 12),  frequency=12) 
autoplot(ts_both, facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("")

#it seems like there is lag one
occ <- cbind(
  occlag0 = xreg_training2,
  occlag1 = stats::lag(xreg_training2,-1),
  occlag2 = stats::lag(xreg_training2,-2),
  occlag3 = stats::lag(xreg_training2,-3)) %>%
  head(NROW(ts_both))

# Restrict data so models use same fitting period
fit1 <- auto.arima(ts_both[4:67,1], xreg=occ[4:67,1],
                   stationary=TRUE)
fit2 <- auto.arima(ts_both[4:67,1], xreg=occ[4:67,1:2],
                   stationary=TRUE)
fit3 <- auto.arima(ts_both[4:67,1], xreg=occ[4:67,1:3],
                   stationary=TRUE)
fit4 <- auto.arima(ts_both[4:67,1], xreg=occ[4:67,1:4],
                   stationary=TRUE)
c(fit1[["aicc"]],fit2[["aicc"]],fit3[["aicc"]],fit4[["aicc"]])

(fit <- auto.arima(ts_both[,1], xreg=occ[,1:2],
                   stationary=TRUE))

fc <- forecast(fit, h=9,
                xreg=cbind(occlag0 = rep(mean( occ[1:67,1]   )      ,9),
                           occlag1 = c(occ[67,1], rep( mean( occ[1:67,1]   )     ,8))))
autoplot(fc) +
  autolayer(my_data)+
  ylab("") +
  ggtitle("")

#Hierarchical time series
library(hts)
all.hts <- hts(ts_all, nodes=list(3))
all.hts %>% aggts(levels=1) %>%
  autoplot(facet=TRUE) +
  xlab("Year") + ylab("dollars") + ggtitle("RentRate")
all.hts %>% aggts(levels=1) %>%
  +   autoplot(facet=TRUE) +
  +   xlab("Year") + ylab("dollars") + ggtitle("RentRate")

#Complex Seasonality
my_data %>% mstl(seasonal.periods = c(4,12)) %>%
  autoplot()

#Vector autoregressions

ts_all <- ts(cbind(rental_rate = std_s_ts, occupancy = occ_df_monthly_std$occupancy[34:109], CPI=cpi
                       ),
             start=c( 2014, 12),  frequency=12) 
training_all <- window(ts_all,
                       end=c( 2020, 11),  frequency=12 )
library(vars)
VARselect(ts_all, lag.max=8,
          type="const")[["selection"]]

var5 <- VAR(training_all, p=5, type="const")

future <- predict(var5, n.ahead=4)
mae = ( sum( abs( future$fcst$rental_rate[,1]-std_s_ts[73:76])) + sum( abs(fitted(var5)[,1]- std_s_ts[3:72])))/74

#1567.643 1540.698 1524.980 1549.680
serial.test(var2, lags.pt=10, type="PT.asymptotic")

forecast(var4) %>%
  autoplot() + xlab("Year")+autolayer(my_data)

#Neural Network models
fit <- nnetar(training2, lambda=0)
fit2 <- nnetar(training2, p=4, P=1,size=15)
autoplot(forecast(fit2,h=9, PI=FALSE))+
  autolayer(my_data)

##########################################################################################
#one bedroom

my_data = std_s_ts
training4 =  window(my_data, end=c(2020,11 ))
test4 <- window(my_data, start=c(2020,12 ))

Box.test(diff(my_data), type="Lj")



ts_all_2 <- ts(cbind(rental_rate = std_s_ts, occupancy = occ_df_monthly_std$occupancy[34:109], CPI=cpi,
                    sensitivity_std_monthly= new_sens_std  ),
             start=c( 2014, 12),  frequency=12) 

ts_all_1 <- ts(cbind(rental_rate = one_br_s_ts, occupancy = occ_df_monthly_1br$occupancy[34:109],
                   CPI=cpi    ),
             start=c( 2014, 12),  frequency=12) 

###### Time vs exploratory variables
autoplot(ts_all_2[,1:4], facets=TRUE) +
  xlab("") + ylab("") +
  ggtitle("")

qplot( occupancy ,rental_rate , data=as.data.frame(ts_all_2) ) +
  ylab("Rental Rate") + xlab("Occupancy")

GGally::ggpairs(as.data.frame(ts_all_2[,1:4]))

occ_std <- occ_df_monthly_std$occupancy

occ_gen = ts(occ_df_monthly$occupancy[34:109],start= c(2014,12), end=c(2021,03) ,freq=12  )
occ_std = ts(occ_std[34:109] ,start= c(2014,12), end=c(2021,03) ,freq=12  )

ggarrange(autoplot(occ_one)+
            ggtitle("One Bedroom Apartments Occupancy")+
            xlab("")+
            ylab(""),
          autoplot(one_br_s_ts)+
            ggtitle("One Bedroom Apartments Rental Rates")+
            xlab("")+
            ylab("") ,
          ncol = 1, nrow = 2)




xreg3 <- cbind(occ_std, cpi, new_sens_std )

xreg3_training4   <- window(xreg3, end=c(2020,11 ))
xreg3_test4 <-  window(xreg3, start=c(2020,12 ))

#cpi_training4 <- window(cpi, end=c(2020,11 ))
#cpi_test4 <-  window(cpi, start=c(2020,12 ))
#xreg2_training4 <- cbind(xreg1_training4,cpi_training4 )
#xreg2_test4 <- cbind(xreg1_test4,cpi_test4 )
#Arima with regressors

arima3 <- auto.arima(training4, xreg = xreg3_training4, d=1 )
checkresiduals(arima3)
accuracy(arima3)

autoplot(one_br_s_ts )+
  autolayer(arima3$fitted, series = "ARIMA fit")+
  autolayer(forecast(arima1, xreg= xreg3_test4  ), series="forecast")+
  autolayer(one_br_s_ts, series= "Rental Rates")+
  theme(legend.position = c(0.7, 0.85))+
  xlab("")+
  ylab("")

#occ, cpi


arima1_2 <- auto.arima(training4  , xreg = xreg3_training4[,1:2], d=1 )
checkresiduals(arima1_2)
accuracy(arima1_2)

autoplot(one_br_s_ts )+
  autolayer(arima1_2$fitted, series = "ARIMA fit")+
  autolayer(forecast(arima1_2, xreg= xreg3_test4[,1:2]  ), series="forecast")+
  autolayer(one_br_s_ts, series= "Rental Rates")+
  theme(legend.position = c(0.7, 0.85))+
  xlab("")+
  ylab("")

#occ, sens
arima1_3 <- auto.arima(training4  , xreg = xreg3_training4[,c(1,3) ], d=1 )
checkresiduals(arima1_3)
accuracy(arima1_3)

autoplot(one_br_s_ts )+
  autolayer(arima1_3$fitted, series = "ARIMA fit")+
  autolayer(forecast(arima1_3, xreg= xreg3_test4[,c(1,3)]  ), series="forecast")+
  autolayer(one_br_s_ts, series= "Rental Rates")+
  theme(legend.position = c(0.7, 0.85))+
  xlab("")+
  ylab("")

#cpi, sens
arima2_3 <- auto.arima(training4  , xreg = xreg3_training4[,c(2,3) ],d=1 )
checkresiduals(arima2_3)
accuracy(arima2_3)

autoplot(one_br_s_ts )+
  autolayer(arima2_3$fitted, series = "ARIMA fit")+
  autolayer(forecast(arima2_3, xreg= xreg3_test4[,c(2,3)]  ), series="forecast")+
  autolayer(one_br_s_ts, series= "Rental Rates")+
  theme(legend.position = c(0.7, 0.85))+
  xlab("")+
  ylab("")

#occ
arima_occ <- auto.arima(training4, xreg = xreg3_training4[,1] , d=1 )
checkresiduals(arima_occ)
accuracy(arima_occ)

autoplot(one_br_s_ts )+
  autolayer(arima_occ$fitted, series = "ARIMA fit")+
  autolayer(forecast(arima_occ, xreg= xreg3_test4[,1]  ), series="forecast")+
  autolayer(one_br_s_ts, series= "Rental Rates")+
  theme(legend.position = c(0.7, 0.85))+
  xlab("")+
  ylab("")

#sens
arima_sens <- auto.arima(training4, xreg = xreg3_training4[,3] , d=1 )
checkresiduals(arima_sens)
accuracy(arima_sens)

autoplot(one_br_s_ts )+
  autolayer(arima_sens$fitted, series = "ARIMA fit")+
  autolayer(forecast(arima_occ, xreg= xreg3_test4[,3]  ), series="forecast")+
  autolayer(one_br_s_ts, series= "Rental Rates")+
  theme(legend.position = c(0.7, 0.85))+
  xlab("")+
  ylab("")




cbind("Regression Errors" = residuals(arima_sens, type="regression"),
      "ARIMA errors" = residuals(arima_sens, type="innovation")) %>%
  autoplot(facets=TRUE)




#Dynamic Harmonic regression 

plots <- list()
for (i in seq(6)) {
  fit <- auto.arima(training4, xreg = fourier(training4 , K = i),
                    seasonal = TRUE, lambda = 0)
  checkresiduals(fit)
  print(accuracy(fit))
  plots[[i]] <- autoplot(forecast(fit,
                                  xreg=fourier(training4, K=i, h=4))) +
    autolayer(my_data)+
    xlab(paste("K=",i,"   AICC=",round(fit[["aicc"]],2))) +
    ylab("") 
}
gridExtra::grid.arrange(
  plots[[1]],plots[[2]],plots[[3]],
  plots[[4]],plots[[5]],plots[[6]], nrow=3)




#Vector autoregressions
ts_all <- ts(cbind(rental_rate = std_s_ts, occupancy = occ_df_monthly_std$occupancy[34:109], 
                     sensitivity_std_monthly= new_sens_std  ),
               start=c( 2014, 12),  frequency=12) 

ts_all <- ts(cbind(rental_rate = one_br_s_ts, 
                   occupancy = occ_df_monthly_1br$occupancy[34:109],
                   CPI=cpi,sensitivity=
                     sensitivity_1br_monthly 
),
start=c( 2014, 12),  frequency=12)  

ts_all <- ts(cbind(rental_rate = one_br_s_ts, 
                   occupancy = occ_df_monthly_1br$occupancy[34:109],
                   sensitivity=sensitivity_1br_monthly 
),
start=c( 2014, 12),  frequency=12)  

training_all <- window(ts_all,
                       end=c( 2020, 11),  frequency=12 )
library(vars)
VARselect(ts_all, lag.max=8,
          type="const")[["selection"]]

var1 <- VAR(training_all, p=1, type="const")
summary(var1)
acf(residuals(var1))
roots(var1) #not stable, they should be less than one


var2 <- VAR(training_all, p=2, type="const")
summary(var2)
acf(residuals(var2))
roots(var2)#not stable, they should be less than one

var3 <- VAR(training_all, p=3, type="const")
summary(var3)
acf(residuals(var3))
roots(var3)#not stable, they should be less than one

var4 <- VAR(training_all, p=4, type="const")
summary(var4)
acf(residuals(var4))
roots(var3)#not stable, they should be less than one

future1 <- predict(var1, n.ahead=4)
#future2<- predict(var2, n.ahead=4)
#future3 <- predict(var3, n.ahead=4)

mae1 = (   sum( abs(fitted(var1)[,1]- std_s_ts [2:72] )  ) )/71
mae2 = (   sum( abs(fitted(var2)[,1]- std_s_ts[3:72] )  ) )/70
mae3 = (   sum( abs(fitted(var3)[,1]- std_s_ts[4:72] )  ) )/69

autoplot(one_br_s_ts )+
  autolayer(ts(fitted(var1)[,1], end=c(2020,11), frequency = 12 )   , series = "VAR(1) fit")+
  autolayer( ts( c(1971.655, 1983.266,1990.577, 1995.044 )    , start = c(2020,12) , frequency = 12     )  , series="forecast")+
  autolayer(one_br_s_ts, series= "Rental Rates")+
  theme(legend.position = c(0.6, 0.85))+
  xlab("")+
  ylab("")



#mae2 = ( sum( abs( future2$fcst$rental_rate[,1]-one_br_s_ts [73:76])) + sum( abs(fitted(var2)[,1]- one_br_s_ts[3:72])))/74
#mae3 = ( sum( abs( future3$fcst$rental_rate[,1]-one_br_s_ts [73:76])) + sum( abs(fitted(var3)[,1]- one_br_s_ts[3:72])))/74

acf(residuals(var1))


#1567.643 1540.698 1524.980 1549.680
#serial.test(var2, lags.pt=10, type="PT.asymptotic")

#forecast(var4) %>%
 # autoplot() + xlab("Year")+autolayer(my_data)

#Neural Network models
fit <- nnetar(training4, lambda=0)
fit2 <- nnetar(training4, p=4, P=1,size=15)
autoplot(forecast(fit2,h=4, PI=FALSE))+
  autolayer(my_data)
