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
write.csv(AG_weather_Pougue, file = "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/input_data/AWN_T_P_DAILY/Omak.csv")

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
head(Omak_weather_diff)
write.csv(Omak_weather_diff, file = "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/input_data/AWN_T_P_DAILY/Omak_diff.csv")
nrow(Omak_weather_diff)
sapply(Omak_weather_diff, class)

Omak <- ggplot()+
  geom_line(data = Omak_weather_obs, aes(x = Omak_weather_obs$Date, y = Omak_weather_obs$t_min, color = "obs"))+
  geom_line(data = AG_weather_Pougue, aes(x = AG_weather_Pougue$Date, y = AG_weather_Pougue$t_min, color = "AG"))+
  geom_col(data = Omak_weather_diff, aes(x = Omak_weather_diff$Date, y = Omak_weather_diff$tmin_diff, color = "diff"))+
  xlab('Date')+
  ylab('Temperature')+
  ggtitle('Omak')
Omak
ggsave(filename = paste0(plot_check, "Omak_comp.png"),plot = Omak , height = 10, width =10)


###### Richland comparision ##############
AG_weather_Richland <- subset(AG_weather, AG_weather$STATION_NAME =="Richland.N")
AG_weather_Richland <- select(AG_weather_Richland,STATION_NAME, JULDATE, MAX_AIR_TEMP_F, MIN_AIR_TEMP)
sapply(AG_weather_Richland, class)
AG_weather_Richland$JULDATE <- as.Date(AG_weather_Richland$JULDATE, format = "%Y-%m-%d")
head(AG_weather_Richland)
write.csv(AG_weather_Richland, file = "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/input_data/AWN_T_P_DAILY/Richland.csv")
head(AG_weather_Richland)


# the year is decide between 1995 to 
AG_weather_Richland <- subset(AG_weather_Richland, format(as.Date(JULDATE),"%Y")%in%(1996:2014))
names(AG_weather_Richland) <- c("Station", "Date", "t_max","t_min")
names(AG_weather_Richland)
AG_weather_Richland <- select(AG_weather_Richland, Date,t_max,t_min)

# converting tmax and tmin to celcius
library(weathermetrics)
AG_weather_Richland$t_min <- fahrenheit.to.celsius(AG_weather_Richland$t_min)
AG_weather_Richland$t_max <- fahrenheit.to.celsius(AG_weather_Richland$t_max)


# observed weather data of Pogue
Richland_weather_obs <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/observed/output_observed_historical_data_46.28125_-119.34375.csv",stringsAsFactors = FALSE)
Richland_weather_obs <- subset(Richland_weather_obs, Richland_weather_obs$year %in% (1996:2014)) 
Richland_weather_obs <- select(Richland_weather_obs, Date,t_max,t_min)
Richland_weather_obs$Date <- as.Date(Richland_weather_obs$Date, format = "%Y-%m-%d")
head(Richland_weather_obs)


# merging the two frames
Richland_weather_diff <- merge(Richland_weather_obs, AG_weather_Richland,by = "Date")
Richland_weather_diff$tmin_diff <- Richland_weather_diff$t_min.x - Richland_weather_diff$t_min.y
head(Richland_weather_diff)
write.csv(Richland_weather_diff, file = "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/input_data/AWN_T_P_DAILY/Richland_diff.csv")
nrow(Richland_weather_diff)
sapply(Richland_weather_diff, class)

Richland <- ggplot()+
  geom_line(data = Richland_weather_obs, aes(x = Richland_weather_obs$Date, y = Richland_weather_obs$t_min, color = "obs"))+
  geom_line(data = AG_weather_Richland, aes(x = AG_weather_Richland$Date, y = AG_weather_Richland$t_min, color = "AG"))+
  geom_col(data = Richland_weather_diff, aes(x = Richland_weather_diff$Date, y = Richland_weather_diff$tmin_diff, color = "diff"))+
  xlab('Date')+
  ylab('Temperature')+
  ggtitle('Richland')
Richland
ggsave(filename = paste0(plot_check, "Richland_comp.png"),plot = Richland , height = 10, width =20)


########## Yakima Comparision ############################

AG_weather_Yakima <- subset(AG_weather, AG_weather$STATION_NAME =="Parker")
AG_weather_Yakima <- select(AG_weather_Yakima,STATION_NAME, JULDATE, MAX_AIR_TEMP_F, MIN_AIR_TEMP)
sapply(AG_weather_Yakima, class)
AG_weather_Yakima$JULDATE <- as.Date(AG_weather_Yakima$JULDATE, format = "%Y-%m-%d")
head(AG_weather_Yakima)
write.csv(AG_weather_Yakima, file = "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/input_data/AWN_T_P_DAILY/Yakima.csv")
head(AG_weather_Yakima)


# the year is decide between 1990 to 
AG_weather_Yakima <- subset(AG_weather_Yakima, format(as.Date(JULDATE),"%Y")%in%(1990:2014))
names(AG_weather_Yakima) <- c("Station", "Date", "t_max","t_min")
names(AG_weather_Yakima)
AG_weather_Yakima <- select(AG_weather_Yakima, Date,t_max,t_min)

# converting tmax and tmin to celcius
library(weathermetrics)
AG_weather_Yakima$t_min <- fahrenheit.to.celsius(AG_weather_Yakima$t_min)
AG_weather_Yakima$t_max <- fahrenheit.to.celsius(AG_weather_Yakima$t_max)


# observed weather data of Pogue
Yakima_weather_obs <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/observed/output_observed_historical_data_46.59375_-120.53125.csv",stringsAsFactors = FALSE)
Yakima_weather_obs <- subset(Yakima_weather_obs, Yakima_weather_obs$year %in% (1990:2014)) 
Yakima_weather_obs <- select(Yakima_weather_obs, Date,t_max,t_min)
Yakima_weather_obs$Date <- as.Date(Yakima_weather_obs$Date, format = "%Y-%m-%d")
head(Yakima_weather_obs)


# merging the two frames
Yakima_weather_diff <- merge(Yakima_weather_obs, AG_weather_Yakima,by = "Date")
Yakima_weather_diff$tmin_diff <- Yakima_weather_diff$t_min.x - Yakima_weather_diff$t_min.y
head(Yakima_weather_diff)
write.csv(Yakima_weather_diff, file = "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/input_data/AWN_T_P_DAILY/Yakima_diff.csv")
nrow(Yakima_weather_diff)
sapply(Yakima_weather_diff, class)

Yakima <- ggplot()+
  geom_line(data = Yakima_weather_obs, aes(x = Yakima_weather_obs$Date, y = Yakima_weather_obs$t_min, color = "obs"))+
  geom_line(data = AG_weather_Yakima, aes(x = AG_weather_Yakima$Date, y = AG_weather_Yakima$t_min, color = "AG"))+
  geom_col(data = Yakima_weather_diff, aes(x = Yakima_weather_diff$Date, y = Yakima_weather_diff$tmin_diff, color = "diff"))+
  xlab('Date')+
  ylab('Temperature')+
  ggtitle('Yakima')
Yakima
ggsave(filename = paste0(plot_check, "Yakima_comp.png"),plot = Yakima , height = 10, width =20)


############# WEnatchee Comparision #####################

AG_weather_Wenatchee <- subset(AG_weather, AG_weather$STATION_NAME =="Wenatch.W")
AG_weather_Wenatchee <- select(AG_weather_Wenatchee,STATION_NAME, JULDATE, MAX_AIR_TEMP_F, MIN_AIR_TEMP)
sapply(AG_weather_Wenatchee, class)
AG_weather_Wenatchee$JULDATE <- as.Date(AG_weather_Wenatchee$JULDATE, format = "%Y-%m-%d")
head(AG_weather_Wenatchee)
write.csv(AG_weather_Wenatchee, file = "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/input_data/AWN_T_P_DAILY/Wenatchee.csv")
head(AG_weather_Wenatchee)


# the year is decide between 1993 to 
AG_weather_Wenatchee <- subset(AG_weather_Wenatchee, format(as.Date(JULDATE),"%Y")%in%(1994:2014))
names(AG_weather_Wenatchee) <- c("Station", "Date", "t_max","t_min")
names(AG_weather_Wenatchee)
AG_weather_Wenatchee <- select(AG_weather_Wenatchee, Date,t_max,t_min)

# converting tmax and tmin to celcius
library(weathermetrics)
AG_weather_Wenatchee$t_min <- fahrenheit.to.celsius(AG_weather_Wenatchee$t_min)
AG_weather_Wenatchee$t_max <- fahrenheit.to.celsius(AG_weather_Wenatchee$t_max)


# observed weather data of Pogue
Wenatchee_weather_obs <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/observed/output_observed_historical_data_47.40625_-120.34375.csv",stringsAsFactors = FALSE)
Wenatchee_weather_obs <- subset(Wenatchee_weather_obs, Wenatchee_weather_obs$year %in% (1994:2014)) 
Wenatchee_weather_obs <- select(Wenatchee_weather_obs, Date,t_max,t_min)
Wenatchee_weather_obs$Date <- as.Date(Wenatchee_weather_obs$Date, format = "%Y-%m-%d")
head(Wenatchee_weather_obs)


# merging the two frames
Wenatchee_weather_diff <- merge(Wenatchee_weather_obs, AG_weather_Wenatchee,by = "Date")
Wenatchee_weather_diff$tmin_diff <- Wenatchee_weather_diff$t_min.x - Wenatchee_weather_diff$t_min.y
head(Wenatchee_weather_diff)
write.csv(Wenatchee_weather_diff, file = "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/input_data/AWN_T_P_DAILY/Wenatchee_diff.csv")
nrow(Wenatchee_weather_diff)
sapply(Wenatchee_weather_diff, class)

Wenatchee <- ggplot()+
  geom_line(data = Wenatchee_weather_obs, aes(x = Wenatchee_weather_obs$Date, y = Wenatchee_weather_obs$t_min, color = "obs"))+
  geom_line(data = AG_weather_Wenatchee, aes(x = AG_weather_Wenatchee$Date, y = AG_weather_Wenatchee$t_min, color = "AG"))+
  geom_col(data = Wenatchee_weather_diff, aes(x = Wenatchee_weather_diff$Date, y = Wenatchee_weather_diff$tmin_diff, color = "diff"))+
  xlab('Date')+
  ylab('Temperature')+
  ggtitle('Wenatchee')
Wenatchee
ggsave(filename = paste0(plot_check, "Wenatchee_comp.png"),plot = Wenatchee , height = 10, width =20)


################# Walla Walla ######################

AG_weather_Walla <- subset(AG_weather, AG_weather$STATION_NAME =="WallaWalla")
AG_weather_Walla <- select(AG_weather_Walla,STATION_NAME, JULDATE, MAX_AIR_TEMP_F, MIN_AIR_TEMP)
sapply(AG_weather_Walla, class)
AG_weather_Walla$JULDATE <- as.Date(AG_weather_Walla$JULDATE, format = "%Y-%m-%d")
head(AG_weather_Walla)
write.csv(AG_weather_Walla, file = "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/input_data/AWN_T_P_DAILY/Walla.csv")
head(AG_weather_Walla)


# the year is decide between 1993 to 
AG_weather_Walla <- subset(AG_weather_Walla, format(as.Date(JULDATE),"%Y")%in%(1993:2014))
names(AG_weather_Walla) <- c("Station", "Date", "t_max","t_min")
names(AG_weather_Walla)
AG_weather_Walla <- select(AG_weather_Walla, Date,t_max,t_min)

# converting tmax and tmin to celcius
library(weathermetrics)
AG_weather_Walla$t_min <- fahrenheit.to.celsius(AG_weather_Walla$t_min)
AG_weather_Walla$t_max <- fahrenheit.to.celsius(AG_weather_Walla$t_max)


# observed weather data of Pogue
Walla_weather_obs <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/observed/output_observed_historical_data_46.03125_-118.34375.csv",stringsAsFactors = FALSE)
Walla_weather_obs <- subset(Walla_weather_obs, Walla_weather_obs$year %in% (1993:2014)) 
Walla_weather_obs <- select(Walla_weather_obs, Date,t_max,t_min)
Walla_weather_obs$Date <- as.Date(Walla_weather_obs$Date, format = "%Y-%m-%d")
head(Walla_weather_obs)


# merging the two frames
Walla_weather_diff <- merge(Walla_weather_obs, AG_weather_Walla,by = "Date")
Walla_weather_diff$tmin_diff <- Walla_weather_diff$t_min.x - Walla_weather_diff$t_min.y
head(Walla_weather_diff)
write.csv(Walla_weather_diff, file = "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/input_data/AWN_T_P_DAILY/Walla_diff.csv")
nrow(Walla_weather_diff)
sapply(Walla_weather_diff, class)

Walla <- ggplot()+
  geom_line(data = Walla_weather_obs, aes(x = Walla_weather_obs$Date, y = Walla_weather_obs$t_min, color = "obs"))+
  geom_line(data = AG_weather_Walla, aes(x = AG_weather_Walla$Date, y = AG_weather_Walla$t_min, color = "AG"))+
  geom_col(data = Walla_weather_diff, aes(x = Walla_weather_diff$Date, y = Walla_weather_diff$tmin_diff, color = "diff"))+
  xlab('Date')+
  ylab('Temperature')+
  ggtitle('Walla')
Walla
ggsave(filename = paste0(plot_check, "Walla_comp.png"),plot = Walla , height = 10, width =20)





