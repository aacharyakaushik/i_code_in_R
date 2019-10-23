#########################################
#       AG WEATHER DATA ANALYSIS        #
#                                       #
#########################################

library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

plot_check <- "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/Output_data/Plots/check/"

AG_weather <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/input_data/AWN_T_P_DAILY/AWN_T_P_DAILY.csv",stringsAsFactor = FALSE)
colnames(AG_weather_Pougue)


## cleaning the data and setting it to the needs
AG_weather_Pougue <- subset(AG_weather, AG_weather$STATION_NAME =="Pogue.Flat")
AG_weather_Pougue <- select(AG_weather_Pougue,STATION_NAME, JULDATE, MAX_AIR_TEMP_F, MIN_AIR_TEMP)
sapply(AG_weather_Pougue, class)
AG_weather_Pougue$JULDATE <- as.Date(AG_weather_Pougue$JULDATE, format = "%Y-%m-%d")
head(AG_weather_Pougue)
write.csv(AG_weather_Pougue, file = "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/input_data/AWN_T_P_DAILY/Pogue.csv")

# the year is decide between 2006 to 
AG_weather_Pougue <- subset(AG_weather_Pougue, format(as.Date(JULDATE),"%Y")%in%(2006:2014))
names(AG_weather_Pougue) <- c("Station", "Date", "t_max","t_min")
names(AG_weather_Pougue)
AG_weather_Pougue <- select(AG_weather_Pougue, Date,t_max,t_min)

# converting tmax and tmin to celcius
library(weathermetrics)
AG_weather_Pougue$t_min <- fahrenheit.to.celsius(AG_weather_Pougue$t_min)
AG_weather_Pougue$t_max <- fahrenheit.to.celsius(AG_weather_Pougue$t_max)


# observed weather data of Pogue
Omak_weather_obs <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/observed/output_observed_historical_data_47.40625_-120.34375.csv",stringsAsFactors = FALSE)
Omak_weather_obs <- subset(Omak_weather_obs, Omak_weather_obs$year %in% (2006:2014)) 
Omak_weather_obs <- select(Omak_weather_obs, Date,t_max,t_min)
Omak_weather_obs$Date <- as.Date(Omak_weather_obs$Date, format = "%Y-%m-%d")
head(Omak_weather_obs)


# merging the two frames
Omak_weather_diff <- merge(Omak_weather_obs, AG_weather_Pougue,by = "Date")
Omak_weather_diff$tmin_diff <- Omak_weather_diff$t_min.x - Omak_weather_diff$t_min.y
Omak_weather_diff
sapply(Omak_weather_diff, class)

Omak <- ggplot()+
  geom_line(data = Omak_weather_obs, aes(x = Omak_weather_obs$Date, y = Omak_weather_obs$t_min, color = "obs"))+
  geom_line(data = AG_weather_Pougue, aes(x = AG_weather_Pougue$Date, y = AG_weather_Pougue$t_min, color = "AG"))+
  geom_col(data = Omak_weather_diff, aes(x = Omak_weather_diff$Date, y = Omak_weather_diff$tmin_diff, color = "diff"))+
  xlab('Date')+
  ylab('Temperature')+
  ggtitle('Omak')

ggsave(filename = paste0(plot_check, "Omak_comp.png"),plot = Omak , height = 10, width =10)


###### 