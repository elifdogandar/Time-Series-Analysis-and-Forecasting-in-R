
library(ggplot2)  #for autoplot
library(forecast) #using autoplot and autolayer for ts objects
#library(ggfortify) #using autoplot for ts objects

#library(magrittr)
#library(dplyr) 

#library(GGally)

# detach("package:ggfortify") 

# library(ggplot2)
# library(forecast)
# library(ggfortify)

#Relation between std_s and std_m
(std_m_ts/std_s_ts)
(std_m_ts/std_s_ts)/(65/45)

 autoplot(std_s_ts* (65/45)*0.85) +
   autolayer(std_s_ts* (65/45)*0.85)+
   autolayer(std_m_ts,col=2)+
   xlab("")+
   ylab("Rent Rate (dollars)") +
   theme(legend.position = c(0.15, 0.85), text = element_text(size=15))
 
#Relation between one_br_s_ts and one_br_l_ts
(one_br_l_ts/one_br_s_ts)

 autoplot(one_br_s_ts* 1.1 ) + 
   autolayer(one_br_s_ts* 1.1)+
   autolayer(one_br_l_ts,col=2)+
   xlab("")+
   ylab("Rent Rate (dollars)") +
   theme(legend.position = c(0.15, 0.85), text = element_text(size=15))
 


#######################################################################################
#              ALL PLOTS
#######################################################################################
ts_all_6 <- ts(cbind("STD-S" = std_s_ts, "STD-M" = std_m_ts , "STD-L" = std_l_ts, 
                   "1-BR-S" = one_br_s_ts, "1-BR-L" = one_br_l_ts, 
                   "2-BR" = two_br_ts),
             start=c( year(unit_rent_master$StartDate)[1], 12),  frequency=12) 

 autoplot(ts_all_6, facets = TRUE)+
   ylab("Rent Rate (dollars)")+
   xlab("")+
   theme(text = element_text(size=15))
 
 
 ts_all_3 <- ts(cbind("STD" = std_s_ts, 
                   "1-BR" = one_br_s_ts, 
                   "2-BR" = two_br_ts),
             start=c( year(unit_rent_master$StartDate)[1], 12),  frequency=12) 


 autoplot(std_m_ts)+
  autolayer( std_m_ts, series = "STD-M")+
  autolayer( std_s_ts*(65/45)*0.852, col=2, series = "STD-S*(65/45)*0.852", lty=2)+
  ylab("Rent Rate (dollars)")+
  xlab("")+
  theme(legend.position = c(0.15, 0.85), text = element_text(size=15))
 

#######################################################################################
#              ONE PLOT AT A TIME
#######################################################################################

#my_ts <- std_s_ts
#my_name <- "Studio Apartments"
#my_ts <- one_br_s_ts
#my_name <- "One Bedroom Apartments"
my_ts <- two_br_ts
my_name <- "Two Bedroom Apartments"

# Plot ts objects
autoplot(my_ts) +
  ylab("Rent Rate (dollars)")+
  xlab("")+
  theme( text = element_text(size=15))


#Seasonal plots
library(ggrepel) #for geom_label_repel

ggseasonplot(my_ts ) +
  scale_color_discrete(name= "Year" ) +
  ylab("Rent Rate (dollars)") +
  ggtitle(paste ("Seasonal plot:", my_name   ))+ 
  geom_label_repel( label = c(2014, 2015, rep("",10), 2015, 2016, rep("",10), 
                              2016, 2017, rep("",10), 2017, 2018, rep("",10), 2018,
                              2019, rep("",10), 2019,  2020, rep("",10), 2020, 2021, "",""     ))+
  xlab("")+
  theme( text = element_text(size=15))

#Polar seasonal plot
ggseasonplot(my_ts, polar=TRUE) +
  ylab("Rent Rate (dollars)") +
  ggtitle(paste("Polar seasonal plot:", my_name  ))+
  xlab("")+
  theme( text = element_text(size=15))

#Seasonal subseries plots
ggsubseriesplot((my_ts)) +
  ylab("Rent Rate (dollars)") +
  ggtitle(paste("Seasonal subseries plot:", my_name ))+
  xlab("")+
  theme( text = element_text(size=15))

#Lag plot
gglagplot(my_ts)+
  ylab("Rent Rate (dollars)") +
  ggtitle(paste("Lag Plot of", my_name ))+
  theme( text = element_text(size=8))

#Correllalogram


acf_plot <- ggAcf( my_ts)
pacf_plot <-ggPacf(my_ts)

Box.test(my_ts, type="Lj")
Box.test(diff(my_ts), type="Lj")
Box.test(diff(my_ts,12), type="Lj")
Box.test(diff(diff(my_ts,12)), type="Lj")


library(ggpubr)     #for ggarrange

ggarrange(acf_plot +ggtitle(paste("ACF Plot of", my_name ) )  ,
          pacf_plot +ggtitle(paste("PACF Plot of", my_name )) ,
          ncol = 2, nrow = 1)


#Decomposition
autoplot(stl(my_ts, s.window = "periodic"))+
  ylab("Rent Rate (dollars)") +
  xlab("")+
  ggtitle(paste("Decomposition of", my_name))+
  theme( text = element_text(size=17))


# Identify change points in mean 
library(changepoint)  #change point
library(ggfortify)

a <- my_ts %>%
  changepoint:: cpt.mean() %>%  # Identify change points
  autoplot(x.label.fmt = "%y/%m")+
  theme( text = element_text(size=15))
  


# Identify change points in variance
b<- my_ts %>%
  changepoint:: cpt.var() %>%  # Identify change points
  autoplot()+
  theme( text = element_text(size=15))


# Detect jump in a data
library(strucchange)  #structure change

c <- strucchange::breakpoints(my_ts ~ 1) %>%
  autoplot()+
  theme( text = element_text(size=15))



ggarrange(a + ggtitle("Change in Mean"),
          b + ggtitle("Change in Variance"),
          c + ggtitle("Structure Change"),
          ncol = 3, nrow = 1)

#Peaks and Valleys 
library(ggpmisc) #for peaks and valleys

ggplot(my_ts , as.numeric = FALSE) +
  geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", angle = 60,
             vjust = -0.5, x.label.fmt = "%y/%m") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue", angle = 60,
               vjust = 1.5, hjust = 1,  x.label.fmt = "%y/%m"  )+
  #ylim(1350, 1610)+  #for std
  #ylim(1750, 2150)+    #for one br
  ylim(1650, 2450)+    #for two br
  ylab("Rent Rate (dollars)") +
  xlab("")+
  theme( text = element_text(size=15))

#Outliers

tsoutliers( my_ts)

