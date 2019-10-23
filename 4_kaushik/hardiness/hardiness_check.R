############################

# This is cross check page for
# the results of the 
# hardidness project

###########################

library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(reshape2)


## CHeck for the year 2009 because there were 5 incidents

out_dates_model <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/CanESM2/rcp85/output_data_45.84375_-120.84375.csv", stringsAsFactors = FALSE)
print(head(out_dates_model))
print(sapply(out_dates_model, class))



# out_dates$Date
# worked, Hurray 
out_dates_model$Date <- as.Date(out_dates_model$Date, format = "%Y-%m-%d")
# out_dates$Date
print(sapply(out_dates_model$Date,class))
print(head(out_dates_model))


year_2009 <- subset(out_dates_model, out_dates_model$hardiness_year == "2009" )
year_2010 <- subset(out_dates_model, out_dates_model$hardiness_year == "2010" )
write.csv(year_2009, file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/year_2009_2010.csv")

ggplot()+
  geom_line(data = year_2009, aes(x= year_2009$Date, y= year_2009$predicted_Hc, color ="obs"))+
  geom_line(data = year_2009, aes(x= year_2009$Date, y= year_2009$t_min, color ="tmin"))+
  geom_line(data = year_2009, aes(x= year_2009$Date, y= year_2009$t_max, color ="tmax"))+
  xlab('Date')+
  ylab('HC')

ggplot()+
  geom_line(data = year_2010, aes(x= year_2010$Date, y= year_2010$predicted_Hc, color ="obs"))+
  geom_line(data = year_2010, aes(x= year_2010$Date, y= year_2010$t_min, color ="tmin"))+
  geom_line(data = year_2010, aes(x= year_2010$Date, y= year_2010$t_max, color ="tmax"))+
  xlab('Date')+
  ylab('HC')

### check for indivial 3 months

year_2050_2099 <- subset(out_dates_model, out_dates_model$year %in% c(2020:2090))

plot_path <- "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/check/"


## For month of Jan
year_2050_2099_jan <- subset(year_2050_2099, format(year_2050_2099$Date, "%m")=="01")
year_2050_2099_jan

ggplot()+
  geom_line(data = year_2050_2099_jan, aes(x= year_2050_2099_jan$Date, y= year_2050_2099_jan$predicted_Hc, color ="obs"))+
  geom_line(data = year_2050_2099_jan, aes(x= year_2050_2099_jan$Date, y= year_2050_2099_jan$t_min, color ="tmin"))+
  geom_line(data = year_2050_2099_jan, aes(x= year_2050_2099_jan$Date, y= year_2050_2099_jan$t_max, color ="tmax"))+
  ggtitle('January')+
  xlab('Date')+
  ylab('Temp')
ggsave(paste0(plot_path, "Jan",".png"))

## For month of feb
year_2050_2099_feb <- subset(year_2050_2099, format(year_2050_2099$Date, "%m")=="02")
year_2050_2099_feb

ggplot()+
  geom_line(data = year_2050_2099_feb, aes(x= year_2050_2099_feb$Date, y= year_2050_2099_feb$predicted_Hc, color ="obs"))+
  geom_line(data = year_2050_2099_feb, aes(x= year_2050_2099_feb$Date, y= year_2050_2099_feb$t_min, color ="tmin"))+
  geom_line(data = year_2050_2099_feb, aes(x= year_2050_2099_feb$Date, y= year_2050_2099_feb$t_max, color ="tmax"))+
  ggtitle('February')+
  xlab('Date')+
  ylab('Temp')
ggsave(paste0(plot_path, "Feb",".png"))

## For month of march
year_2050_2099_march <- subset(year_2050_2099, format(year_2050_2099$Date, "%m")=="03")
year_2050_2099_march

ggplot()+
  geom_line(data = year_2050_2099_march, aes(x= year_2050_2099_march$Date, y= year_2050_2099_march$predicted_Hc, color ="obs"))+
  geom_line(data = year_2050_2099_march, aes(x= year_2050_2099_march$Date, y= year_2050_2099_march$t_min, color ="tmin"))+
  geom_line(data = year_2050_2099_march, aes(x= year_2050_2099_march$Date, y= year_2050_2099_march$t_max, color ="tmax"))+
  ggtitle('March')+
  xlab('Date')+
  ylab('Temp')
ggsave(paste0(plot_path, "Mar",".png"))



## for month of apr
year_2050_2099_apr <- subset(year_2050_2099, format(year_2050_2099$Date, "%m")=="04")
year_2050_2099_apr

ggplot()+
  geom_line(data = year_2050_2099_apr, aes(x= year_2050_2099_apr$Date, y= year_2050_2099_apr$predicted_Hc, color ="obs"))+
  geom_line(data = year_2050_2099_apr, aes(x= year_2050_2099_apr$Date, y= year_2050_2099_apr$t_min, color ="tmin"))+
  geom_line(data = year_2050_2099_apr, aes(x= year_2050_2099_apr$Date, y= year_2050_2099_apr$t_max, color ="tmax"))+
  ggtitle('April')+
  xlab('Date')+
  ylab('Temp')
ggsave(paste0(plot_path, "Apr",".png"))

  ## for month of may
year_2050_2099_may <- subset(year_2050_2099, format(year_2050_2099$Date, "%m")=="05")
year_2050_2099_may

ggplot()+
  geom_line(data = year_2050_2099_may, aes(x= year_2050_2099_may$Date, y= year_2050_2099_may$predicted_Hc, color ="obs"))+
  geom_line(data = year_2050_2099_may, aes(x= year_2050_2099_may$Date, y= year_2050_2099_may$t_min, color ="tmin"))+
  geom_line(data = year_2050_2099_may, aes(x= year_2050_2099_may$Date, y= year_2050_2099_may$t_max, color ="tmax"))+
  ggtitle('May')+
  xlab('Date')+
  ylab('Temp')
ggsave(paste0(plot_path, "May",".png"))


## for month of june
year_2050_2099_jun <- subset(year_2050_2099, format(year_2050_2099$Date, "%m")=="06")
year_2050_2099_jun

ggplot()+
  geom_line(data = year_2050_2099_jun, aes(x= year_2050_2099_jun$Date, y= year_2050_2099_jun$predicted_Hc, color ="obs"))+
  geom_line(data = year_2050_2099_jun, aes(x= year_2050_2099_jun$Date, y= year_2050_2099_jun$t_min, color ="tmin"))+
  geom_line(data = year_2050_2099_jun, aes(x= year_2050_2099_jun$Date, y= year_2050_2099_jun$t_max, color ="tmax"))+
  ggtitle('June')+
  xlab('Date')+
  ylab('Temp')
ggsave(paste0(plot_path, "Jun",".png"))

## for month of july
year_2050_2099_jul <- subset(year_2050_2099, format(year_2050_2099$Date, "%m")=="07")
year_2050_2099_jul

ggplot()+
  geom_line(data = year_2050_2099_jul, aes(x= year_2050_2099_jul$Date, y= year_2050_2099_jul$predicted_Hc, color ="obs"))+
  geom_line(data = year_2050_2099_jul, aes(x= year_2050_2099_jul$Date, y= year_2050_2099_jul$t_min, color ="tmin"))+
  geom_line(data = year_2050_2099_jul, aes(x= year_2050_2099_jul$Date, y= year_2050_2099_jul$t_max, color ="tmax"))+
  ggtitle('July')+
  xlab('Date')+
  ylab('Temp')
ggsave(paste0(plot_path, "Jul",".png"))

## for month of august
year_2050_2099_aug <- subset(year_2050_2099, format(year_2050_2099$Date, "%m")=="08")
year_2050_2099_aug

ggplot()+
  geom_line(data = year_2050_2099_aug, aes(x= year_2050_2099_aug$Date, y= year_2050_2099_aug$predicted_Hc, color ="obs"))+
  geom_line(data = year_2050_2099_aug, aes(x= year_2050_2099_aug$Date, y= year_2050_2099_aug$t_min, color ="tmin"))+
  geom_line(data = year_2050_2099_aug, aes(x= year_2050_2099_aug$Date, y= year_2050_2099_aug$t_max, color ="tmax"))+
  ggtitle('August')+
  xlab('Date')+
  ylab('Temp')
ggsave(paste0(plot_path, "Aug",".png"))

## for month of Sep
year_2050_2099_Sep <- subset(year_2050_2099, format(year_2050_2099$Date, "%m")=="09")
year_2050_2099_Sep

ggplot()+
  geom_line(data = year_2050_2099_Sep, aes(x= year_2050_2099_Sep$Date, y= year_2050_2099_Sep$predicted_Hc, color ="obs"))+
  geom_line(data = year_2050_2099_Sep, aes(x= year_2050_2099_Sep$Date, y= year_2050_2099_Sep$t_min, color ="tmin"))+
  geom_line(data = year_2050_2099_Sep, aes(x= year_2050_2099_Sep$Date, y= year_2050_2099_Sep$t_max, color ="tmax"))+
  ggtitle('September')+
  xlab('Date')+
  ylab('Temp')
ggsave(paste0(plot_path, "Sep",".png"))

## for month of Oct
year_2050_2099_oct <- subset(year_2050_2099, format(year_2050_2099$Date, "%m")=="10")
year_2050_2099_oct

ggplot()+
  geom_line(data = year_2050_2099_oct, aes(x= year_2050_2099_oct$Date, y= year_2050_2099_oct$predicted_Hc, color ="obs"))+
  geom_line(data = year_2050_2099_oct, aes(x= year_2050_2099_oct$Date, y= year_2050_2099_oct$t_min, color ="tmin"))+
  geom_line(data = year_2050_2099_oct, aes(x= year_2050_2099_oct$Date, y= year_2050_2099_oct$t_max, color ="tmax"))+
  ggtitle('October')+
  xlab('Date')+
  ylab('Temp')
ggsave(paste0(plot_path, "Oct",".png"))

## for month of Nov
year_2050_2099_nov <- subset(year_2050_2099, format(year_2050_2099$Date, "%m")=="11")
year_2050_2099_nov

ggplot()+
  geom_line(data = year_2050_2099_nov, aes(x= year_2050_2099_nov$Date, y= year_2050_2099_nov$predicted_Hc, color ="obs"))+
  geom_line(data = year_2050_2099_nov, aes(x= year_2050_2099_nov$Date, y= year_2050_2099_nov$t_min, color ="tmin"))+
  geom_line(data = year_2050_2099_nov, aes(x= year_2050_2099_nov$Date, y= year_2050_2099_nov$t_max, color ="tmax"))+
  ggtitle('November')+
  xlab('Date')+
  ylab('Temp')
ggsave(paste0(plot_path, "Nov",".png"))

## for month of Dec
year_2050_2099_dec <- subset(year_2050_2099, format(year_2050_2099$Date, "%m")=="11")
year_2050_2099_dec

ggplot()+
  geom_line(data = year_2050_2099_dec, aes(x= year_2050_2099_dec$Date, y= year_2050_2099_dec$predicted_Hc, color ="obs"))+
  geom_line(data = year_2050_2099_dec, aes(x= year_2050_2099_dec$Date, y= year_2050_2099_dec$t_min, color ="tmin"))+
  geom_line(data = year_2050_2099_dec, aes(x= year_2050_2099_dec$Date, y= year_2050_2099_dec$t_max, color ="tmax"))+
  ggtitle('December')+
  xlab('Date')+
  ylab('Temp')
ggsave(paste0(plot_path, "Dec",".png"))



############### checking plots for Yakima, richland and Omak ######
# out_dates_omak <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/CanESM2/rcp85/output_data_45.84375_-120.84375.csv", stringsAsFactors = FALSE)


library(ggplot2)

plot_path <- "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/"
models <- c("bcc-csm1-1","bcc-csm1-1-m","BNU-ESM","CanESM2",
            "CCSM4","CNRM-CM5","CSIRO-Mk3-6-0","GFDL-ESM2G","GFDL-ESM2M",
            "HadGEM2-CC365","HadGEM2-ES365", "inmcm4", "IPSL-CM5A-LR", "IPSL-CM5A-MR",
            "IPSL-CM5B-LR","MIROC5", "MIROC-ESM-CHEM", "MRI-CGCM3", "NorESM1-M")

for (names in models)
{
  out_dates_omak <- read.csv(file = paste0("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/",names,"/rcp85/output_data_48.40625_-119.53125.csv"), stringsAsFactors = FALSE)
  out_dates_omak_obs <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/observed/output_observed_historical_data_48.40625_-119.53125.csv", stringsAsFactors = FALSE)

  
  out_dates_omak$Date <- as.Date(out_dates_omak$Date, format = "%Y-%m-%d")
  # out_dates$Date
  print(sapply(out_dates_omak$Date,class))
  print(head(out_dates_omak))
  
  out_dates_omak_obs$Date <- as.Date(out_dates_omak_obs$Date, format = "%Y-%m-%d")
  
  
  
  ggplot()+
    geom_line()+
    geom_line(data = out_dates_omak_obs , aes(x = out_dates_omak_obs$Date, y = out_dates_omak_obs$predicted_Hc, color = "predicted HC"))+
    geom_line(data = out_dates_omak_obs, aes(x = out_dates_omak_obs$Date, y = out_dates_omak_Obs$t_min, color = "tmin"))+
    geom_line(data = out_dates_omak, aes(x = out_dates_omak_obs$Date, y = out_dates_omak_obs$t_max, color = "tmax"))
  ggsave(paste0(plot_path, names, ".png"))  
}

# plots for the location of 5 places
source_path = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/plot_core.R"
source(source_path)

omak <- plot_obs_trends_by_location("data_48.40625_-119.53125", "Omak")
wenatchee <- plot_obs_trends_by_location("data_47.40625_-120.34375", "Wenatchee")
richland <- plot_obs_trends_by_location("data_46.28125_-119.34375","Richland")
yakima <- plot_obs_trends_by_location("data_46.59375_-120.53125","Yakima")
walla <- plot_obs_trends_by_location("data_46.03125_-118.34375","Walla")





# print(name)
plot_path <- "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/"

out_dates_loc_obs <- read.csv(file = paste0("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/observed/output_observed_historical_data_48.40625_-119.53125.csv"), stringsAsFactors = FALSE)
# out_dates_loc_obs

out_dates_loc_obs$Date <- as.Date(out_dates_loc_obs$Date, format = "%Y-%m-%d")
# print(out_dates_loc_obs)
out_dates_loc_obs

out_dates_loc_obs <- subset(out_dates_loc_obs, out_dates_loc_obs$hardiness_year %in% (1979:2014)) 

# set the counter from 1 to 259 for year of hardiness
setDT(out_dates_loc_obs)[, counter := seq_len(.N), by=rleid(hardiness_year)]

# out_dates_loc_obs$MONTH <- format(out_dates_loc_obs$Date, format = "%m%d")
# sapply(out_dates_loc_obs, class)
# out_dates_loc_obs$MONTH <- as.numeric(out_dates_loc_obs$MONTH)
write.csv(out_dates_loc_obs, file = paste0(plot_path,"checking.csv"))

ggplot()+
  geom_line(data = out_dates_loc_obs, aes(x = out_dates_loc_obs$counter, y = out_dates_loc_obs$predicted_Hc, color = "predicted HC"))+
  geom_line(data = out_dates_loc_obs, aes(x = out_dates_loc_obs$counter, y = out_dates_loc_obs$t_min, color = "tmin"))+
  geom_line(data = out_dates_loc_obs, aes(x = out_dates_loc_obs$counter, y = out_dates_loc_obs$t_max, color = "tmax"))+
  facet_wrap(~ hardiness_year)
  
  
ggsave(paste0(plot_path,name,".png"), width = 15, height = 10)




####################### AG weather check Omak ######################

plot_check <- "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/check/"

Omak <- read.csv("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/observed/output_observed_historical_data_48.40625_-119.53125.csv")
Omak$Date <- as.Date(Omak$Date, format = "%Y-%m-%d")
Omak_2008 <- subset(Omak, Omak$year == 2008)
Omak_2008$t_min

# Year 2008
library(weathermetrics)
AG_2008 <- read.csv(file = paste0("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/input_data/AGweather/Daily_pogue_onak_2008.csv"), stringsAsFactors = FALSE)
AG_2008$Date <- as.Date(AG_2008$DATE, format = "%m/%d/%Y")
AG_2008$MIN_AIR_TEMP <- fahrenheit.to.celsius(AG_2008$MIN_AIR_TEMP)
AG_2008$MAX_AIR_TEMP <- fahrenheit.to.celsius(AG_2008$MAX_AIR_TEMP)
AG_2008 <- na.omit(AG_2008)

comp_2008 <- ggplot()+
  geom_line(data = Omak_2008, aes(x= Omak_2008$Date, y= Omak_2008$t_min, color = "tmin"))+
  geom_line(data = AG_2008, aes(x=AG_2008$Date, y = AG_2008$MIN_AIR_TEMP, color = "AG_tmin"))+
  geom_line(data = Omak_2008, aes(x= Omak_2008$Date, y= Omak_2008$t_max, color = "tmax"))+
  geom_line(data = AG_2008, aes(x=AG_2008$Date, y = AG_2008$MAX_AIR_TEMP, color = "AG_tmax"))+
  xlab('Date')+
  ylab('Temperature')+
  ggtitle('2008')
comp_2008


# Year 2009
Omak_2009 <- subset(Omak, Omak$year == 2009)
Omak_2009$t_min

AG_2009 <- read.csv(file = paste0("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/input_data/AGweather/Daily_pogue_onak_2009.csv"), stringsAsFactors = FALSE)
AG_2009$Date <- as.Date(AG_2009$DATE, format = "%m/%d/%Y")
AG_2009$MIN_AIR_TEMP <- fahrenheit.to.celsius(AG_2009$MIN_AIR_TEMP)
AG_2009$MAX_AIR_TEMP <- fahrenheit.to.celsius(AG_2009$MAX_AIR_TEMP)
AG_2009 <- na.omit(AG_2009)

comp_2009 <- ggplot()+
  geom_line(data = Omak_2009, aes(x= Omak_2009$Date, y= Omak_2009$t_min, color = "tmin"))+
  geom_line(data = AG_2009, aes(x=AG_2009$Date, y = AG_2009$MIN_AIR_TEMP, color = "AG_tmin"))+
  geom_line(data = Omak_2009, aes(x= Omak_2009$Date, y= Omak_2009$t_max, color = "tmax"))+
  geom_line(data = AG_2009, aes(x=AG_2009$Date, y = AG_2009$MAX_AIR_TEMP, color = "AG_tmax"))+
  xlab('Date')+
  ylab('Temperature')+
  ggtitle('2009')
comp_2009

library(gridExtra)
grid.arrange(comp_2008,comp_2009,nrow = 2, ncol = 1)
# 2736 ? 1824
Comp_2008_2009 <- arrangeGrob(comp_2008,comp_2009,nrow = 2, ncol = 1)
Comp_2008_2009
ggsave(paste0(plot_check, "Comp_2008_2009.png"), height = 10, width =10,Comp_2008_2009)

#############  Adding barplot for the difference between temperatures ##########

  


Omak_2008 <- subset(Omak_2008, select = c(Date,t_min))
Omak_2008

sapply(AG_2008, class)
AG_2008 <- subset(AG_2008, select = c(DATE, MIN_AIR_TEMP))
AG_2008 <- na.omit(AG_2008)
colnames(AG_2008) <- c("Date", "t_min")
colnames(AG_2008)
AG_2008$Date <- as.Date(AG_2008$Date, format = "%m/%d/%Y")

nrow(Omak_2008)
nrow(AG_2008)
AG_2008

Omak_diff <- merge(Omak_2008,AG_2008, by= "Date")
Omak_diff$diff <- Omak_diff$t_min.x - Omak_diff$t_min.y
Omak_diff



Comp_2008_tmin <- ggplot()+
  geom_line(data = Omak_2008, aes(x= Omak_2008$Date, y= Omak_2008$t_min, color = "tmin"))+
  geom_line(data = AG_2008, aes(x=AG_2008$Date, y = AG_2008$t_min, color = "AG_tmin"))+
  geom_col(data = Omak_diff, aes(x=Omak_diff$Date, y= Omak_diff$diff, color = "diff"))+
  xlab('Date')+
  ylab('Temperature')+
  ggtitle('Tmin Comaparision for the year 2008')
Comp_2008_tmin
ggsave(paste0(plot_check, "Comp_2008_tmin.png"), height = 10, width =10)

### For tmx

Omak_2008_tmax <- subset(Omak_2008, select = c(Date, t_max))
Omak_2008_tmax

AG_2008_tmax <- subset(AG_2008, select = c(DATE, MAX_AIR_TEMP))
AG_2008_tmax
colnames(AG_2008_tmax) <- c("Date", "t_max")
colnames(AG_2008_tmax)
AG_2008_tmax$Date <- as.Date(AG_2008_tmax$Date, format = "%m/%d/%Y")

nrow(Omak_2008_tmax)
nrow(AG_2008_tmax)

Omak_2008_tmax_diff <- merge(Omak_2008_tmax,AG_2008_tmax, by= "Date")
Omak_2008_tmax_diff$diff <- Omak_2008_tmax_diff$t_max.x - Omak_2008_tmax_diff$t_max.y
Omak_2008_tmax_diff


Comp_2008_tmax <- ggplot()+
  geom_line(data = Omak_2008_tmax, aes(x= Omak_2008_tmax$Date, y= Omak_2008_tmax$t_max, color = "tmax"))+
  geom_line(data = AG_2008_tmax, aes(x=AG_2008_tmax$Date, y = AG_2008_tmax$t_max, color = "AG_tmax"))+
  geom_col(data = Omak_2008_tmax_diff, aes(x=Omak_2008_tmax_diff$Date, y= Omak_2008_tmax_diff$diff, color = "diff"))+
  xlab('Date')+
  ylab('Temperature')+
  ggtitle('Tmax Comaparision for the year 2008')
Comp_2008_tmax
ggsave(paste0(plot_check, "Comp_2008_tmax.png"), height = 10, width =10)



################################################################################

###################
check <- read.csv("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/checking.csv", stringsAsFactors = FALSE)
check$Date <- as.Date(check$Date, format = "%Y-%m-%d")
head(check)
subset_check<-subset(check, check$hardiness_year %in% c("1979","1980"))
subset_check$Date <- as.Date(subset_check$Date, format = "%Y-%m-%d")

head(subset_check)
ggplot()+
  geom_line(data = subset_check, aes(x= subset_check$Date, y = subset_check$predicted_Hc))+
  facet_wrap(~ hardiness_year)

################ Plotting locations by hardiness year using facet ################

Location_hard <- read.csv()


##################### checking the grid file by plotting ##############

crop_grid <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/parameters/wsda_vic_irr_acres.csv", stringsAsFactors = FALSE)
crop_grid <- crop_grid[-1,]
crop_grid

# creating washington map 
states <- map_data("state")
Washington <- subset(states, region == "washington")
Washington

Washington_base <- ggplot(data = Washington, mapping = aes(x=long, y=lat, group = group))+
  coord_fixed(1.3)+
  geom_polygon(color = "black", fill = "gray" )
Washington_base


# Adding counties
counties <- map_data("county")
Washington_counties <- subset(counties, region == "washington")
Washington_counties

#wroking code for map with predicted Hc
Washington_base+
  geom_point(data =crop_grid,aes(x=long, y= lat,size=0.0001),inherit.aes = FALSE)+
  geom_polygon(data = Washington_counties, fill =NA, color= "black")+
  ggtitle('Grid')+
  scale_color_gradient(low ="white", high = "red")
ggsave(paste0(map_plot_location, "Probability.png"), height = 10, width =10)  


Washington_base+
  geom_polygon(data = crop_grid, aes(x=long, y = lat), inherit.aes = FALSE)
