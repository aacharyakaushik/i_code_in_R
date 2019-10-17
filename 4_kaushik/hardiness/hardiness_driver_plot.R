.libPaths("/data/hydro/R_libs35")
.libPaths()


library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(reshape2)

options(digits=9)

# source_path = "/home/kraghavendra/hardiness/hardiness_core.R"
# source_path = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/hardiness_core.R"
source_path = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/plot_core.R"
source(source_path)

# location of the output file to read
out_dates_obs <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/observed/output_observed_historical_data_45.84375_-120.84375.csv", stringsAsFactors = FALSE)
print(head(out_dates_obs))
print(sapply(out_dates_obs, class))


out_dates_model <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/CanESM2/rcp85/output_data_45.84375_-120.84375.csv", stringsAsFactors = FALSE)
print(head(out_dates_model))
print(sapply(out_dates_model, class))

mean_plot_location <- "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/mean_plots/"
density_plot_location <- "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/density_plots/"
# # to plot end of month values for hardiness, converting the the date comun to Dat format for easier operations
# sapply(out_dates_obs$Date,class) # check the datatype of the column
# # out_dates$Date
# out_dates_obs$Date <- as.Date(out_dates_obs$Date, format ="%m/%d/%Y") # worked, Hurray 
# # out_dates$Date
# sapply(out_dates_obs$Date,class) # check wether the conversion has worked
# 
# out_dates_model$Date <- as.Date(out_dates_model$Date, format ="%m/%d/%Y")
# sapply(out_dates_model$Date,class)
# 
# # Calulating mean every month for the plots
# mean_months <- aggregate(data = out_dates_obs, predicted_Hc ~ format.Date(Date, "%m%Y"),FUN = mean)
# mean_months_t_min <- aggregate(data = out_dates_obs, t_min ~ format.Date(Date, "%m%Y"), FUN = mean)
# mean_months_t_min
# mean_months <- mean_months %>% mutate(t_min = mean_months_t_min$t_min)
# mean_months
# # mean_months_sep <- subset(mean_months, format.Date(Date, "%m") == "09")
# sapply(mean_months, class) # checking the class
# names(mean_months)[1] <- "Date" # cahnging the name of the column according to the date 
# colnames(mean_months)
# # mean_months$Date <- as.Date(mean_months$Date, format = "%m%Y")
# mean_months <- data.table(mean_months)
# mean_months

mean_plot <- plot_mean_hardiness(out_dates_obs,out_dates_model, mean_plot_location)

# find highest of every month over the years



trend_plot <- plot_trend_max(out_dates_obs, out_dates_model, mean_plot_location)


density_plot <- plot_density_hardiness(out_dates_obs,out_dates_model, density_plot_location)









# Plotting CDI events by month
print(sapply(out_dates_obs$Date,class)) # check the datatype of the column
# out_dates$Date
out_dates_obs$Date <- as.Date(out_dates_obs$Date, format ="%Y-%m-%d")# worked, Hurray 
out_dates_model$Date <- as.Date(out_dates_model$Date, format = "%Y-%m-%d")
# out_dates$Date
print(sapply(out_dates_obs$Date,class)) # check wether the conversion has worked
print(head(out_dates_obs))

print(sapply(out_dates_model$Date,class))

print(head(out_dates_model))

out_dates_obs$MONTH <- format(out_dates_obs$Date, "%m")
print(head(out_dates_obs))

ggplot(data = out_dates_obs)+
  geom_point(data= out_dates_obs, aes(x = Date, y=CDI, color = "obs"))+
  facet_wrap(~ MONTH)




# box plot
sapply(out_dates_obs, class)

obs_box<- out_dates_obs %>% group_by(year) %>%
summarize(CDI = sum(CDI))
obs_box
 
obs_box_month <- out_dates_obs %>% group_by(format(Date, "%Y%m")) %>%
summarize(CDI = sum(CDI))
names(obs_box_month)[1] <- "year"
colnames(obs_box_month)

ggplot(obs_box_month$CDI)+
  geom_boxplot()

cat_year <- c(2016,2050,2075,2099)
label_year <- c("<2050","2050-2075","75-99")



out_dates_model_exp <- subset(out_dates_model, out_dates_model$year %in% c(2016:2099))
sapply(out_dates_model_exp,class)
out_dates_model_exp$bins <- cut(x=out_dates_model_exp$year,breaks = cat_year,labels = label_year ,include.lowest = TRUE)
head(out_dates_model_exp)


  
write.csv(out_dates_model_exp, file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/model_bin.csv")

# cut(x=consolidated_obs$total_CDI, breaks=seq(from=0, to= 550, by = 50),include.lowest =TRUE)

ggplot()+
  geom_boxplot(data= out_dates_model_exp, aes(x = out_dates_model_exp$bins, y= out_dates_model_exp$CDI))


out_dates_obs
ggplot()+
  geom_boxplot(data = out_dates_obs, aes(x =out_dates_obs$Date, y = out_dates_obs$predicted_Hc, color = "obs"))+
  geom_boxplot(data = out_dates_model, aes(x= out_dates_model$Date, y =out_dates_model$predicted_Hc, color = "model"))










print(sapply(out_dates_obs$Date,class)) # check the datatype of the column
# out_dates$Date
out_dates_obs$Date <- as.Date(out_dates_obs$Date, format ="%Y-%m-%d")# worked, Hurray 
out_dates_model$Date <- as.Date(out_dates_model$Date, format = "%Y-%m-%d")
# out_dates$Date
print(sapply(out_dates_obs$Date,class)) # check wether the conversion has worked
print(head(out_dates_obs))

print(sapply(out_dates_model$Date,class))

print(head(out_dates_obs))


sapply(out_dates_obs, class)

# highest of every month
out_dates_obs$year_month <- format(out_dates_obs$Date, "%Y-%m")
# obs_highest<-tapply(out_dates_obs$predicted_Hc,out_dates_obs$year_month, max )
obs_highest <- data.table(out_dates_obs)
obs_highest <- obs_highest[, max(predicted_Hc), by = year_month]
obs_highest
names(obs_highest)[2] <-"predicted_Hc"
colnames(obs_highest)
print(obs_highest)

sapply(obs_highest, class)
# obs_highest$year_month <- as.Date(obs_highest$year_month, format = "%Y-%m")
# obs_highest$YEAR <- format(obs_highest$year_month, "%Y")
obs_highest$year_month <- paste0(obs_highest$year_month, "-01")
obs_highest$year_month <- as.Date(obs_highest$year_month)
obs_highest$MONTH <- format(obs_highest$year_month,"%m")

write.csv(obs_highest, file ="C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/highest.csv")

ggplot()+
  geom_line(data=obs_highest, aes(x=obs_highest$year_month, y =obs_highest$predicted_Hc, color ="obs" ))+
  facet_wrap(~MONTH)




out_dates_check <- subset()

year_1999 <- subset(out_dates_obs, out_dates_obs$year == "1999")
year_2003 <- subset(out_dates_obs, out_dates_obs$year == "2003")
year_2005 <- subset(out_dates_obs, out_dates_obs$year == "2005")
year_1996 <- subset(out_dates_obs, out_dates_obs$year == "1996")


year_1999$MONTH <- format(year_1999$Date, "%m")
year_1995_2005 <- subset(out_dates_obs, out_dates_obs$year %in% c(1995:2006))
year_1995_2005

year_1979_2005 <- subset(out_dates_obs, out_dates_obs$year %in% c(1979:2006))
year_1979_2005

ggplot()+
  geom_line(data = year_1995_2005, aes(x= year_1995_2005$Date, y= year_1995_2005$predicted_Hc, color ="obs"))+
  geom_line(data = year_1995_2005, aes(x= year_1995_2005$Date, y= year_1995_2005$t_min, color ="tmin"))+
  geom_line(data = year_1995_2005, aes(x= year_1995_2005$Date, y= year_1995_2005$t_max, color ="tmax"))+
  xlab('Date')+
  ylab('HC')

ggplot()+
  geom_line(data = year_1995_2005, aes(x= year_1995_2005$Date, y= year_1995_2005$t_min, color ="tmin"))+
  geom_line(data = year_1995_2005, aes(x= year_1995_2005$Date, y= year_1995_2005$t_max, color ="tmax"))+
  xlab('Date')+
  ylab('temperature')

ggplot()+
  geom_line(data = year_1979_2005, aes(x= year_1979_2005$Date, y= year_1979_2005$predicted_Hc, color ="obs"))+
  xlab('Date')+
  ylab('HC')
ggsave(paste0(mean_plot_location,"year_1979_2005", ".png"), width=25, dpi =300)

  

ggplot()+
  geom_line(data = year_1999, aes(x = year_1999$jday, y = year_1999$predicted_Hc, color = "1999"))+
  geom_line(data = year_2003, aes(x = year_2003$jday, y = year_2003$predicted_Hc, color = "2003"))+
  geom_line(data = year_2005, aes(x = year_2005$jday, y = year_2005$predicted_Hc, color = "2005"))+
  geom_line(data = year_2000, aes(x = year_2000$jday, y = year_2000$predicted_Hc, color = "2000"))+
  geom_line(data = year_1996, aes(x = year_1996$jday, y = year_1996$predicted_Hc, color = "1996"))+
  xlab('Date')+
  ylab('predicted Hc')
ggsave(paste0(mean_plot_location,"year_1999", ".png"))

ggplot()+
  # geom_line(data = year_1999, aes(x = year_1999$Date, y = year_1999$predicted_Hc, color = "1999"))+
  geom_line(data = year_2003, aes(x = year_2003$jday, y = year_2003$predicted_Hc, color = "2003"))+
  xlab('Date')+
  ylab('predicted Hc')
ggsave(paste0(mean_plot_location,"year_2003", ".png"))

  
# Sep_plot <- plot_mean_hardiness(mean_months,"09",mean_plot_location)
# Oct_plot <- plot_mean_hardiness(mean_months,"10",mean_plot_location)
# Nov_plot <- plot_mean_hardiness(mean_months,"11",mean_plot_location)
# Dec_plot <- plot_mean_hardiness(mean_months,"12",mean_plot_location)
# Jan_plot <- plot_mean_hardiness(mean_months,"01",mean_plot_location)
# Feb_plot <- plot_mean_hardiness(mean_months,"02",mean_plot_location)
# Mar_plot <- plot_mean_hardiness(mean_months,"03",mean_plot_location)
# Apr_plot <- plot_mean_hardiness(mean_months,"04",mean_plot_location)
# May_plot <- plot_mean_hardiness(mean_months,"05",mean_plot_location)
# 
# 
# density_1979 <- subset(out_dates,out_dates$hardiness_year == "1979")
# density_1979
# # for October
# 
# ggplot()+
#   geom_density(data = density_1979,aes(density_1979$predicted_Hc,color = "predicted Hc"))+
#   geom_density(data = density_1979,aes(density_1979$t_min,color = "Tmin"))+
# xlab('Predicted Hc')
# ggsave(paste0(density_plot_location,"desnity_1979.png"))
# out_dates_obs
# 
# out_dates_obs_Sep <- subset(out_dates_obs,format.Date(Date, "%m")=="09")
# out_dates_obs_Sep
# 
# ggplot()+
#   geom_density(data = out_dates_obs,aes(out_dates_obs$predicted_Hc,color = "Observed Predicted Hc",fill="Observed Predicted Hc"))+#,alpha =0.7))+
#   geom_density(data = out_dates_model,aes(out_dates_model$predicted_Hc, color ="Model Predicted Hc",fill = "Model Predicted Hc", alpha =0.7))+
#   xlab('Predicted Hc')
# 
# 
# d = density(density_1979$predicted_Hc, n=1e6)
# i = which.max(d$y)
# sprintf('Predicted_Hc peak is %f with probability %f', d$x[i],d$y[i])
# d = density(density_1979$t_min, n=1e6)
# i = which.max(d$y)
# sprintf('Tmin peak is %f with probability %f', d$x[i],d$y[i])
# 
# 
# # Density plots year wise 
# density_1980 <- subset(out_dates_obs,out_dates_obs$hardiness_year == "1980")
# density_1980
# # for October
# 
# ggplot()+
#   geom_density(data = density_1980,aes(density_1980$predicted_Hc,color = "predicted Hc"))+
#   geom_density(data = density_1980,aes(density_1980$t_min,color = "Tmin"))+
#   xlab('Predicted Hc')
# ggsave(paste0(density_plot_location,"desnity_1980.png"))
# 
# 
# d = density(density_1980$predicted_Hc, n=1e6)
# i = which.max(d$y)
# sprintf('Predicted_Hc peak is %f with probability %f', d$x[i],d$y[i])
# 
# d = density(density_1980$t_min, n=1e6)
# i = which.max(d$y)
# sprintf('Tmin peak is %f with probability %f', d$x[i],d$y[i])
# 
# density_1981 <- subset(out_dates,out_dates$hardiness_year == "1981")
# density_1981
# # for October
# 
# 
# 
# ggplot()+
#   geom_density(data = density_1981,aes(density_1981$predicted_Hc,color = "predicted Hc"))+
#   geom_density(data = density_1981,aes(density_1981$t_min,color = "Tmin"))+
# xlab('Predicted Hc')
# ggsave(paste0(density_plot_location,"desnity_1981.png"))
# 
# d = density(density_1981$predicted_Hc, n=1e6)
# i = which.max(d$y)
# sprintf('Predicted_HC peak is %f with probability %f', d$x[i],d$y[i])
# 
# d = density(density_1981$t_min, n=1e6)
# i = which.max(d$y)
# sprintf('Tmin peak is %f with probability %f', d$x[i],d$y[i])
