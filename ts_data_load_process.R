
#install.packages("lubridate")

library(lubridate)  #for year() 

rental_master  = read.csv("RentalMaster.txt",header=TRUE,sep="" )
unit_master = read.csv("UnitMaster.txt", header=TRUE, sep="")
unit_rent_master = read.csv("UnitRentMaster.txt",header=TRUE, sep="" )

rental_master$StartDate = as.Date( rental_master$StartDate , format= "%m/%d/%Y")
rental_master$EndDate = as.Date( rental_master$EndDate , format= "%m/%d/%Y")
unit_rent_master$StartDate = as.Date( unit_rent_master$StartDate , format= "%m/%d/%Y")
unit_rent_master$EndDate = as.Date( unit_rent_master$EndDate , format= "%m/%d/%Y")

unit_master$UnitPlan = as.factor(unit_master$UnitPlan)
unit_master$UnitType = as.factor(unit_master$UnitType)
unit_rent_master$UnitPlan = as.factor(unit_rent_master$UnitPlan)

str(rental_master)
str(unit_master)
str(unit_rent_master)

summary(rental_master)
summary(unit_master)
summary(unit_rent_master)


std_s_ts <- unit_rent_master[unit_rent_master$UnitPlan=="STD-S&A" , "RentRate",drop=FALSE]
std_s_ts <- ts(std_s_ts[,1], start=c( year(unit_rent_master$StartDate)[1], 12),  frequency=12)

std_m_ts <- unit_rent_master[unit_rent_master$UnitPlan=="STD-M&A" , "RentRate",drop=FALSE]
std_m_ts <- ts(std_m_ts[,1], start=c( year(unit_rent_master$StartDate)[1], 12),  frequency=12)

std_l_ts <- unit_rent_master[unit_rent_master$UnitPlan=="STD-L" , "RentRate",drop=FALSE]
std_l_ts <- ts(std_l_ts[,1], start=c( year(unit_rent_master$StartDate)[1], 12),  frequency=12)

one_br_s_ts <- unit_rent_master[unit_rent_master$UnitPlan=="1BR-S&A" , "RentRate",drop=FALSE]
one_br_s_ts <- ts(one_br_s_ts[,1], start=c( year(unit_rent_master$StartDate)[1], 12),  frequency=12)

one_br_l_ts <- unit_rent_master[unit_rent_master$UnitPlan=="1BR-L&A" , "RentRate",drop=FALSE]
one_br_l_ts <- ts(one_br_l_ts[,1], start=c( year(unit_rent_master$StartDate)[1], 12),  frequency=12)

two_br_ts <- unit_rent_master[unit_rent_master$UnitPlan=="2BR-S" ,"RentRate",drop=FALSE]
two_br_ts <- ts(two_br_ts[,1], start=c( year(unit_rent_master$StartDate)[1], 12),  frequency=12)


ts_all <- ts(cbind(std_s_ts, one_br_s_ts, two_br_ts),
             start=c( year(unit_rent_master$StartDate)[1], 12),  frequency=12) 

