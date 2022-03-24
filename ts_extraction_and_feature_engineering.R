#install.packages("tidyr")
#install.packages("dplyr")

#library(tidyr)

library(dplyr) #for arrange and %>%
library(ggplot2)
library(forecast)

#RentalMaster additions

merged <- merge(rental_master,unit_master[,c("UnitNumber","UnitType")], by= "UnitNumber")
merged_std <- merged[merged$UnitType=="STD",]
merged_1br <- merged[merged$UnitType=="1BR",]
merged_2br <- merged[merged$UnitType=="2BR",]


#How old the customer is overall

customer_duration <- merged %>%
  group_by(CustomerNo) %>%
  summarise(first_appearance = first(StartDate , order_by = StartDate),
            last_appearance = last(EndDate, order_by = EndDate  )) %>%
  mutate( history = interval(last_appearance,first_appearance) ) %>%
  mutate( length = -time_length(history,"month")) %>% 
  arrange(desc(length))

ggplot(customer_duration, aes(length))+
  geom_histogram(binwidth = 1)+
  xlab("Customer Duration in Months")+
  ylab("Number of Customers")+
  theme(text = element_text(size=15))


#How old the customer is (std)
customer_duration_std <-  merged[merged$UnitType=="STD",]%>%
  group_by(CustomerNo) %>%
  summarise(first_appearance = first(StartDate , order_by = StartDate),
            last_appearance = last(EndDate, order_by = EndDate  )) %>%
  mutate( history = interval(last_appearance,first_appearance) ) %>%
  mutate( length = -time_length(history,"month")) %>% 
  arrange(desc(length))

ggplot(customer_duration_std, aes(length))+
  geom_histogram(binwidth = 1)+
  xlab("Customer Duration in Months")+
  ylab("Number of Customers")+
  theme( text = element_text(size=15))


#How old the customer is (1br)
customer_duration_one <-  merged[merged$UnitType=="1BR",]%>%
  group_by(CustomerNo) %>%
  summarise(first_appearance = first(StartDate , order_by = StartDate),
            last_appearance = last(EndDate, order_by = EndDate  )) %>%
  mutate( history = interval(last_appearance,first_appearance) ) %>%
  mutate( length = -time_length(history,"month")) %>% 
  arrange(desc(length))

ggplot(customer_duration_one, aes(length))+
  geom_histogram(binwidth = 1)+
  xlab("Customer Duration in Months")+
  ylab("Number of Customers")+
  theme( text = element_text(size=15))


#How old the customer is (2br)
customer_duration_two <-  merged[merged$UnitType=="2BR",]%>%
  group_by(CustomerNo) %>%
  summarise(first_appearance = first(StartDate , order_by = StartDate),
            last_appearance = last(EndDate, order_by = EndDate  )) %>%
  mutate( history = interval(last_appearance,first_appearance) ) %>%
  mutate( length = -time_length(history,"month")) %>% 
  arrange(desc(length))

ggplot(customer_duration_two, aes(length))+
  geom_histogram(binwidth = 1)+
  xlab("Customer Duration in Months")+
  ylab("Number of Customers")+
  theme( text = element_text(size=15))



#monthly occupancy, renewal, move-in, move-outs

start_coln <- seq(ymd('2012-03-01'),ymd('2021-03-01'), by = 'month')
end_coln <- ymd("2012-04-01")+ months(0:108)-days(1)
occ_df_monthly <-  cbind.data.frame(start_coln,end_coln)
occ_df_monthly$interval <- interval(start_coln, end_coln)
occ_df_monthly_std <- occ_df_monthly
occ_df_monthly_1br <- occ_df_monthly
occ_df_monthly_2br <- occ_df_monthly

########replace nas with end date
merged$EndDate = as.character(merged$EndDate)
merged$EndDate[is.na(merged$EndDate)] = max(merged$EndDate,na.rm = TRUE)
merged$EndDate = as.Date(merged$EndDate)
merged$interval <- interval(merged$StartDate, merged$EndDate)


occ_fun <- function(data_occ, series){
  if(series=="general" ){df = rental_master}
  if(series=="std" ){df <- merged[merged$UnitType=="STD",]
  rownames(df) <-  1:nrow(df)}
  if(series=="1br" ){df <- merged[merged$UnitType=="1BR",]
  rownames(df) <-  1:nrow(df)}
  if(series=="2br" ){df <- merged[merged$UnitType=="2BR",]
  rownames(df) <-  1:nrow(df)}
  
  #Monthly move-in
  data_occ = data_occ %>%                                  
    rowwise() %>% 
    mutate(move_in = dim(df[ df$StartDate  %within% interval     ,])[1]  )
  
  #Monthly move-out
  data_occ = data_occ %>%                                  
    rowwise() %>% 
    mutate(move_out = dim(df[ (df$EndDate  %within% interval) &  !is.na(df$EndDate)        ,]   )[1]  )  
  
  #Monthly-renewal
  data_occ$renewal = 0
  for(i in 1:dim(data_occ)[1] ){
    interval= data_occ[i,"interval"][[1]]
    for(j in 1:(dim(df)[1]-1) ){
      if(is.na(df[j,"EndDate"]  )   ) {next}
      if( !(df[j,"EndDate"] %within%interval) ){next}
      for(k in c(setdiff(as.numeric(rownames(df[df$UnitNumber == df[j,"UnitNumber"]&
                                                df$CustomerNo == df[j,"CustomerNo"], ])),j))){
        if(   df[k,"StartDate"] %within% interval  ){
          data_occ[i,"renewal"] = data_occ[i,"renewal"]+1
        }
      }
    }
  }

  #Monthly occupancy
  data_occ = data_occ %>%                                  
    rowwise() %>% 
    mutate(occupancy = ((length( unique(df[ ( !is.na(df$EndDate) 
                                              &  df$StartDate <= end_coln & df$EndDate>=start_coln       )  
                                            |   ( is.na(df$EndDate) & df$StartDate <=end_coln   ), "UnitNumber"     ]   )   )    
    )-renewal )     )
  
  return(data_occ)
    
}

occ_df_monthly = occ_fun(occ_df_monthly, series="general")
occ_df_monthly_std = occ_fun(occ_df_monthly_std, series="std")
occ_df_monthly_1br = occ_fun(occ_df_monthly_1br, series= "1br")
occ_df_monthly_2br = occ_fun(occ_df_monthly_2br, series= "2br")


merged <- merge(rental_master,unit_master[,c("UnitNumber","UnitType")], by= "UnitNumber")
gen_na <- sum(is.na(merged$EndDate ))
std_na <-  sum(is.na(merged[merged$UnitType=="STD",]$EndDate ))
one_na <-  sum(is.na(merged[merged$UnitType=="1BR",]$EndDate ))
two_na <- sum(is.na(merged[merged$UnitType=="2BR",]$EndDate ))

occ_df_monthly_std$move_out[length( occ_df_monthly_std$move_out )]  <- tail(occ_df_monthly_std$move_out,1)    -std_na
occ_df_monthly_1br$move_out[length( occ_df_monthly_1br$move_out )]  <- tail(occ_df_monthly_1br$move_out,1)    -one_na
occ_df_monthly_2br$move_out[length( occ_df_monthly_2br$move_out )]  <- tail(occ_df_monthly_2br$move_out,1)    -two_na


df = occ_df_monthly_2br

autoplot(ts(df$move_in))+
  autolayer(ts(df$move_in), series = "Move-in")+
  autolayer(ts(df$move_out), series = "Move-out")+
  autolayer(ts(df$renewal), series = "Renewals")+
  xlab("")+
  ylab("Count") +
  theme(legend.position = c(0.15, 0.85), text = element_text(size=15))

autoplot(ts(df$move_in))+
  xlab("")+
  ylab("Count") +
  ggtitle("Move-in")+
  theme(legend.position = c(0.15, 0.85), text = element_text(size=15))

autoplot(ts(df$move_out))+
  xlab("")+
  ylab("Count") +
  ggtitle("Move-out")+
  theme(legend.position = c(0.15, 0.85), text = element_text(size=15))

autoplot(ts(df$renewal))+
  xlab("")+
  ylab("Count") +
  ggtitle("Renewal")+
  theme(legend.position = c(0.15, 0.85), text = element_text(size=15))


autoplot(ts(occ_df_monthly$occupancy[34:109]))+
  autolayer(ts(occ_df_monthly$occupancy[34:109]), series = "Occupancy overall")+
  xlab("")+
  ylab("")+
  autolayer(ts(df$occupancy[34:109]), series = "Occupancy: Two Bedroom Apartments")+
  theme(legend.position = c(0.7, 0.6), text = element_text(size=15))


a <- autoplot(ts(df$occupancy[34:109] ,start= c(2014,12), end=c(2021,03) ,freq=12 ))
b <- autoplot(two_br_ts)
library(ggpubr)     #for ggarrange
ggarrange(a+ggtitle("Occupancy for Two Bedroom Apartments")+ylab("")  ,
          b+ggtitle("Rental Rates of Two Bedroom Apartments ")+ylab("") ,
          ncol = 1, nrow = 2)


plot(df$move_out)

##############Outliers
df = occ_df_monthly_1br

tsoutliers(df$renewal)
tsoutliers(df$occupancy)
tsoutliers(df$move_in)
tsoutliers(df$move_out)

unit_master %>% count(UnitPlan)



unit_rent_master %>%
  group_by(UnitPlan) %>%
  summarise_at(vars(RentRate), list(mean = mean))



occ_df_monthly_std$year = year(occ_df_monthly_std$start_coln)
  
occ_yearly_std <- occ_df_monthly_std %>%
  group_by(year) %>%
  summarize(sum = sum(occupancy))

#write.table(occ_yearly_std$sum , file = "yearly_occ_std.csv", sep=",", 
#            row.names= occ_yearly_std$year   )

unit_rent_master %>%
  group_by(UnitPlan) %>%
  summarise_at(vars(RentRate), list(mean = mean))



rental_master %>% count(StartDate)

 
day <- seq(ymd('2014-12-01'),ymd('2021-03-31'), by = 'day')
month <- month(day)
year <- year(day)
sense <- rep(NA, length(day))  
out <- rep(0, length(day))  
possible <- rep(0, length(day))  
sensitivity_std <- cbind.data.frame(day, month,year,sense,out,possible)
sensitivity_1br <- cbind.data.frame(day, month,year,sense, out,possible)
sensitivity_2br <- cbind.data.frame(day, month,year,sense,out, possible)


for ( i in 1: length(day)   ){
  end = sensitivity_std[i,"day"]
  if( end <= ymd("2015-02-28") ){
    check <- ( (end)- years(0:10))
    sensitivity_std[sensitivity_std$day == end,"out"] = dim(merged_std[merged_std$EndDate == end 
                         & !is.na(merged_std$EndDate) ,]   )[1]
    sensitivity_std[sensitivity_std$day == end,"possible"] = dim(merged_std[ merged_std$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  }  
  
  if(  end >= ymd("2015-03-01")  &  end <= ymd("2016-02-28")   ){
    check <- ( (end)- years(0:10) -days(1))
    sensitivity_std[sensitivity_std$day == end,"out"] = dim(merged_std[merged_std$EndDate == end 
                                                                       & !is.na(merged_std$EndDate) ,]   )[1]
    sensitivity_std[sensitivity_std$day == end,"possible"] = dim(merged_std[ merged_std$StartDate %in% check 
                                                                             ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  }   
  
  if( end ==  ymd("2016-02-29") ){
    check <- c ("2015-03-01", "2014-03-01", "2013-03-01", "2012-03-01",  "2015-02-28", "2014-02-28", "2013-02-28" ,"2012-02-29")
    check = ymd(check)
    sensitivity_std[sensitivity_std$day == end,"out"] = dim(merged_std[merged_std$EndDate == end 
                                                                       & !is.na(merged_std$EndDate) ,]   )[1]
    sensitivity_std[sensitivity_std$day == end,"possible"] = dim(merged_std[ merged_std$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  }  
  
  if( end >= ymd("2016-03-01") &  end <= ymd("2017-02-28")  ){
    check <- ( (end)- years(0:10) +days(1))
    sensitivity_std[sensitivity_std$day == end,"out"] = dim(merged_std[merged_std$EndDate == end 
                                                                       & !is.na(merged_std$EndDate) ,]   )[1]
    sensitivity_std[sensitivity_std$day == end,"possible"] = dim(merged_std[ merged_std$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  } 
  if( end >= ymd("2017-03-01") &  end <= ymd("2019-02-15")  ){
    check <- ( (end)- years(0:10) )
    sensitivity_std[sensitivity_std$day == end,"out"] = dim(merged_std[merged_std$EndDate == end 
                                                                       & !is.na(merged_std$EndDate) ,]   )[1]
    sensitivity_std[sensitivity_std$day == end,"possible"] = dim(merged_std[ merged_std$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  } 
  
  if( end >= ymd("2019-03-02") &  end <= ymd("2020-02-17")  ){
    check <- ( (end)- years(0:10) -days(1))
    sensitivity_std[sensitivity_std$day == end,"out"] = dim(merged_std[merged_std$EndDate == end 
                                                                       & !is.na(merged_std$EndDate) ,]   )[1]
    sensitivity_std[sensitivity_std$day == end,"possible"] = dim(merged_std[ merged_std$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  } 
  
  if( end >= ymd("2020-03-01") &  end <= ymd("2021-02-17")  ){
    check <- ( (end)- years(0:10) +days(1))
    sensitivity_std[sensitivity_std$day == end,"out"] = dim(merged_std[merged_std$EndDate == end 
                                                                       & !is.na(merged_std$EndDate) ,]   )[1]
    sensitivity_std[sensitivity_std$day == end,"possible"] = dim(merged_std[ merged_std$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  } 
  
  if( end >= ymd("2021-03-01") &  end <= ymd("2021-03-06")  ){
    check <- ( (end)- years(0:10) )
    sensitivity_std[sensitivity_std$day == end,"out"] = dim(merged_std[merged_std$EndDate == end 
                                                                       & !is.na(merged_std$EndDate) ,]   )[1]
    sensitivity_std[sensitivity_std$day == end,"possible"] = dim(merged_std[ merged_std$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  } 
  
}

sensitivity_std$sense = sensitivity_std$out/sensitivity_std$possible
sensitivity_std$gun = weekdays(sensitivity_std$day)

sensitivity_std_monthly <- sensitivity_std %>%
  group_by(year,month) %>%
  summarize_at(vars(out,possible),sum) %>%
  mutate(sense= out/possible)
  
sensitivity_std_monthly <- ts(sensitivity_std_monthly$sense ,start= c(2014,12), end=c(2021,03) ,freq=12 )

a <- autoplot(sensitivity_std_monthly)+ xlab("")+theme( text = element_text(size=15))
b <- autoplot(std_s_ts)+xlab("")+theme( text = element_text(size=15))

library(ggpubr)     #for ggarrange
ggarrange(a+ggtitle("Sensitivity")+ylab("")  ,
          b+ggtitle("Studio Apartments ")+ylab("Rent Rate (dollars)") ,
          ncol = 1, nrow = 2)



my_data = cbind(sensitivity_std_monthly,std_s_ts)
my_data = as.data.frame(my_data)


ggplot( my_data , aes(x=sensitivity_std_monthly, y=std_s_ts ))+
  xlab("Sensitivity")+
  ylab("Rental Rate (dollars)")+
  geom_point( )+
  geom_label(label=rownames(my_data[,]))+
  theme( text = element_text(size=15))


fitlm = lm(std_s_ts ~ sensitivity_std_monthly , my_data[13:76,] )
predlm = predict(fitlm, newdata = my_data[13:76,])

ggplot(my_data, aes(y = std_s_ts, x = sensitivity_std_monthly ) ) +
  geom_point() +
  geom_line(data = my_data[13:76,], aes(y =  predlm     ), size = 1)+
  geom_label(label=rownames(my_data[,]))+
  xlab("Sensitivity")+
  ylab("Rent Rate (dollars)")+
  theme( text = element_text(size=15))

  
cc <- coef(fitlm)
xnew <- (my_data[1:12,2]   -cc[1])/cc[2]

my_data[1:12,1] = xnew

new_sens_std <-   ts(my_data[,1]  ,start= c(2014,12), end=c(2021,03) ,freq=12 )


a <- autoplot(new_sens_std)+theme( text = element_text(size=15))+xlab("")
b <- autoplot(std_s_ts)+theme( text = element_text(size=15))+xlab("")
library(ggpubr)     #for ggarrange
ggarrange(a+ggtitle("New sensitivity")+ylab("")  ,
          b+ggtitle("Studio Apartments ")+ylab("Rent Rate (dollars)") ,
          ncol = 1, nrow = 2)


###########################################################
#sensitivity 1 br


day <- seq(ymd('2014-12-01'),ymd('2021-03-31'), by = 'day')
month <- month(day)
year <- year(day)
sense <- rep(NA, length(day))  
out <- rep(0, length(day))  
possible <- rep(0, length(day))  
sensitivity_1br <- cbind.data.frame(day, month,year,sense, out,possible)


for ( i in 1: length(day)   ){
  end = sensitivity_1br[i,"day"]
  if( end <= ymd("2015-02-28") ){
    check <- ( (end)- years(0:10))
    sensitivity_1br[sensitivity_1br$day == end,"out"] = dim(merged_1br[merged_1br$EndDate == end 
                                                                       & !is.na(merged_1br$EndDate) ,]   )[1]
    sensitivity_1br[sensitivity_1br$day == end,"possible"] = dim(merged_1br[ merged_1br$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  }  
  
  if(  end >= ymd("2015-03-01")  &  end <= ymd("2016-02-28")   ){
    check <- ( (end)- years(0:10) -days(1))
    sensitivity_1br[sensitivity_1br$day == end,"out"] = dim(merged_1br[merged_1br$EndDate == end 
                                                                       & !is.na(merged_1br$EndDate) ,]   )[1]
    sensitivity_1br[sensitivity_1br$day == end,"possible"] = dim(merged_1br[ merged_1br$StartDate %in% check 
                                                                             ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  }   
  
  if( end ==  ymd("2016-02-29") ){
    check <- c ("2015-03-01", "2014-03-01", "2013-03-01", "2012-03-01",  "2015-02-28", "2014-02-28", "2013-02-28" ,"2012-02-29")
    check = ymd(check)
    sensitivity_1br[sensitivity_1br$day == end,"out"] = dim(merged_1br[merged_1br$EndDate == end 
                                                                       & !is.na(merged_1br$EndDate) ,]   )[1]
    sensitivity_1br[sensitivity_1br$day == end,"possible"] = dim(merged_1br[ merged_1br$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  }  
  
  if( end >= ymd("2016-03-01") &  end <= ymd("2017-02-28")  ){
    check <- ( (end)- years(0:10) +days(1))
    sensitivity_1br[sensitivity_1br$day == end,"out"] = dim(merged_1br[merged_1br$EndDate == end 
                                                                       & !is.na(merged_1br$EndDate) ,]   )[1]
    sensitivity_1br[sensitivity_1br$day == end,"possible"] = dim(merged_1br[ merged_1br$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  } 
  if( end >= ymd("2017-03-01") &  end <= ymd("2019-02-15")  ){
    check <- ( (end)- years(0:10) )
    sensitivity_1br[sensitivity_1br$day == end,"out"] = dim(merged_1br[merged_1br$EndDate == end 
                                                                       & !is.na(merged_1br$EndDate) ,]   )[1]
    sensitivity_1br[sensitivity_1br$day == end,"possible"] = dim(merged_1br[ merged_1br$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  } 
  
  if( end >= ymd("2019-03-02") &  end <= ymd("2020-02-17")  ){
    check <- ( (end)- years(0:10) -days(1))
    sensitivity_1br[sensitivity_1br$day == end,"out"] = dim(merged_1br[merged_1br$EndDate == end 
                                                                       & !is.na(merged_1br$EndDate) ,]   )[1]
    sensitivity_1br[sensitivity_1br$day == end,"possible"] = dim(merged_1br[ merged_1br$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  } 
  
  if( end >= ymd("2020-03-01") &  end <= ymd("2021-02-17")  ){
    check <- ( (end)- years(0:10) +days(1))
    sensitivity_1br[sensitivity_1br$day == end,"out"] = dim(merged_1br[merged_1br$EndDate == end 
                                                                       & !is.na(merged_1br$EndDate) ,]   )[1]
    sensitivity_1br[sensitivity_1br$day == end,"possible"] = dim(merged_1br[ merged_1br$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  } 
  
  if( end >= ymd("2021-03-01") &  end <= ymd("2021-03-06")  ){
    check <- ( (end)- years(0:10) )
    sensitivity_1br[sensitivity_1br$day == end,"out"] = dim(merged_1br[merged_1br$EndDate == end 
                                                                       & !is.na(merged_1br$EndDate) ,]   )[1]
    sensitivity_1br[sensitivity_1br$day == end,"possible"] = dim(merged_1br[ merged_1br$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  } 
  
}

sensitivity_1br$sense = sensitivity_1br$out/sensitivity_1br$possible
sensitivity_1br$gun = weekdays(sensitivity_1br$day)

sensitivity_1br_monthly <- sensitivity_1br %>%
  group_by(year,month) %>%
  summarize_at(vars(out,possible),sum) %>%
  mutate(sense= out/possible)

sensitivity_1br_monthly <- ts(sensitivity_1br_monthly$sense ,start= c(2014,12), end=c(2021,03) ,freq=12 )

a <- autoplot(sensitivity_1br_monthly)+ xlab("")+theme( text = element_text(size=15))
b <- autoplot(one_br_s_ts)+xlab("")+theme( text = element_text(size=15))

library(ggpubr)     #for ggarrange
ggarrange(a+ggtitle("Sensitivity")+ylab("")  ,
          b+ggtitle("One Bedroom Apartments ")+ylab("Rent Rate (dollars)") ,
          ncol = 1, nrow = 2)



my_data = cbind(sensitivity_1br_monthly, one_br_s_ts )
my_data = as.data.frame(my_data)


ggplot( my_data , aes(x=sensitivity_1br_monthly, y=one_br_s_ts ))+
  xlab("Sensitivity")+
  ylab("Rental Rate (dollars)")+
  geom_point( )+
  geom_label(label=rownames(my_data[,]))+
  theme( text = element_text(size=15))


fitlmall = lm(one_br_s_ts ~ sensitivity_1br_monthly , my_data[1:76,] )
predlmall = predict(fitlmall, newdata = my_data[1:76,])

fitlm = lm(one_br_s_ts ~ sensitivity_1br_monthly , my_data[13:76,] )
predlm = predict(fitlm, newdata = my_data[13:76,])

ggplot(my_data, aes(y = one_br_s_ts, x = sensitivity_1br_monthly ) ) +
  geom_point() +
  geom_line(data = my_data[1:76,], aes(y =  predlmall     ), size = 1)+
  geom_line(data = my_data[13:76,], aes(y =  predlm     ), size = 1, col=2)+
  geom_label(label=rownames(my_data[,]))+
  xlab("Sensitivity")+
  ylab("Rent Rate (dollars)")+
  theme( text = element_text(size=15))


###########################################################
#sensitivity 2 br


day <- seq(ymd('2014-12-01'),ymd('2021-03-31'), by = 'day')
month <- month(day)
year <- year(day)
sense <- rep(NA, length(day))  
out <- rep(0, length(day))  
possible <- rep(0, length(day))  
sensitivity_2br <- cbind.data.frame(day, month,year,sense, out,possible)


for ( i in 1: length(day)   ){
  end = sensitivity_2br[i,"day"]
  if( end <= ymd("2015-02-28") ){
    check <- ( (end)- years(0:10))
    sensitivity_2br[sensitivity_2br$day == end,"out"] = dim(merged_2br[merged_2br$EndDate == end 
                                                                       & !is.na(merged_2br$EndDate) ,]   )[1]
    sensitivity_2br[sensitivity_2br$day == end,"possible"] = dim(merged_2br[ merged_2br$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  }  
  
  if(  end >= ymd("2015-03-01")  &  end <= ymd("2016-02-28")   ){
    check <- ( (end)- years(0:10) -days(1))
    sensitivity_2br[sensitivity_2br$day == end,"out"] = dim(merged_2br[merged_2br$EndDate == end 
                                                                       & !is.na(merged_2br$EndDate) ,]   )[1]
    sensitivity_2br[sensitivity_2br$day == end,"possible"] = dim(merged_2br[ merged_2br$StartDate %in% check 
                                                                             ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  }   
  
  if( end ==  ymd("2016-02-29") ){
    check <- c ("2015-03-01", "2014-03-01", "2013-03-01", "2012-03-01",  "2015-02-28", "2014-02-28", "2013-02-28" ,"2012-02-29")
    check = ymd(check)
    sensitivity_2br[sensitivity_2br$day == end,"out"] = dim(merged_2br[merged_2br$EndDate == end 
                                                                       & !is.na(merged_2br$EndDate) ,]   )[1]
    sensitivity_2br[sensitivity_2br$day == end,"possible"] = dim(merged_2br[ merged_2br$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  }  
  
  if( end >= ymd("2016-03-01") &  end <= ymd("2017-02-28")  ){
    check <- ( (end)- years(0:10) +days(1))
    sensitivity_2br[sensitivity_2br$day == end,"out"] = dim(merged_2br[merged_2br$EndDate == end 
                                                                       & !is.na(merged_2br$EndDate) ,]   )[1]
    sensitivity_2br[sensitivity_2br$day == end,"possible"] = dim(merged_2br[ merged_2br$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  } 
  if( end >= ymd("2017-03-01") &  end <= ymd("2019-02-15")  ){
    check <- ( (end)- years(0:10) )
    sensitivity_2br[sensitivity_2br$day == end,"out"] = dim(merged_2br[merged_2br$EndDate == end 
                                                                       & !is.na(merged_2br$EndDate) ,]   )[1]
    sensitivity_2br[sensitivity_2br$day == end,"possible"] = dim(merged_2br[ merged_2br$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  } 
  
  if( end >= ymd("2019-03-02") &  end <= ymd("2020-02-17")  ){
    check <- ( (end)- years(0:10) -days(1))
    sensitivity_2br[sensitivity_2br$day == end,"out"] = dim(merged_2br[merged_2br$EndDate == end 
                                                                       & !is.na(merged_2br$EndDate) ,]   )[1]
    sensitivity_2br[sensitivity_2br$day == end,"possible"] = dim(merged_2br[ merged_2br$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  } 
  
  if( end >= ymd("2020-03-01") &  end <= ymd("2021-02-17")  ){
    check <- ( (end)- years(0:10) +days(1))
    sensitivity_2br[sensitivity_2br$day == end,"out"] = dim(merged_2br[merged_2br$EndDate == end 
                                                                       & !is.na(merged_2br$EndDate) ,]   )[1]
    sensitivity_2br[sensitivity_2br$day == end,"possible"] = dim(merged_2br[ merged_2br$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  } 
  
  if( end >= ymd("2021-03-01") &  end <= ymd("2021-03-06")  ){
    check <- ( (end)- years(0:10) )
    sensitivity_2br[sensitivity_2br$day == end,"out"] = dim(merged_2br[merged_2br$EndDate == end 
                                                                       & !is.na(merged_2br$EndDate) ,]   )[1]
    sensitivity_2br[sensitivity_2br$day == end,"possible"] = dim(merged_2br[ merged_2br$StartDate %in% check ,])[1]
    #if(!possible==0){sensitivity_std[sensitivity_std$day == end,"sense"] <- out/possible }
  } 
  
}

sensitivity_2br$sense = sensitivity_2br$out/sensitivity_2br$possible
sensitivity_2br$gun = weekdays(sensitivity_2br$day)

sensitivity_2br_monthly <- sensitivity_2br %>%
  group_by(year,month) %>%
  summarize_at(vars(out,possible),sum) %>%
  mutate(sense= out/possible)

sensitivity_2br_monthly <- ts(sensitivity_2br_monthly$sense ,start= c(2014,12), end=c(2021,03) ,freq=12 )

a <- autoplot(sensitivity_2br_monthly)+ xlab("")+theme( text = element_text(size=15))
b <- autoplot(two_br_ts )+xlab("")+theme( text = element_text(size=15))

library(ggpubr)     #for ggarrange
ggarrange(a+ggtitle("Sensitivity")+ylab("")  ,
          b+ggtitle("Two Bedroom Apartments ")+ylab("Rent Rate (dollars)") ,
          ncol = 1, nrow = 2)



my_data = cbind(sensitivity_2br_monthly, two_br_ts )
my_data = as.data.frame(my_data)


ggplot( my_data , aes(x=sensitivity_2br_monthly, y=two_br_ts ))+
  xlab("Sensitivity")+
  ylab("Rental Rate (dollars)")+
  geom_point( )+
  geom_label(label=rownames(my_data[,]))+
  theme( text = element_text(size=15))


fitlmall = lm(two_br_ts ~ sensitivity_2br_monthly , my_data[1:76,] )
predlmall = predict(fitlmall, newdata = my_data[1:76,])



ggplot(my_data, aes(y = two_br_ts , x = sensitivity_2br_monthly ) ) +
  geom_point() +
  geom_line(data = my_data[1:76,], aes(y =  predlmall     ), size = 1)+
  geom_label(label=rownames(my_data[,]))+
  xlab("Sensitivity")+
  ylab("Rent Rate (dollars)")+
  theme( text = element_text(size=15))




