library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(reshape2)

########################### Probability CDI based on golnaz paper ######################


probab_path <- "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/probability/"

probability_data <- data.frame(Timeframe = character(),probability = numeric(), stringsAsFactors = FALSE)
sapply(cluster_data, class)


out_dates_obs <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/observed/output_observed_historical_data_46.28125_-119.34375.csv", stringsAsFactors = FALSE)
print(head(out_dates_obs))
print(sapply(out_dates_obs, class))

probab_golnaz_obs <- subset(out_dates_obs, out_dates_obs$hardiness_year %in% (1979:2014)) 
# write.csv(probab_golnaz_obs, file ="C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/golnaz_obs.csv")
total_years <- length(unique(probab_golnaz_obs$hardiness_year))
total_years

# selecting  rows with only 1 in CDI
probab_golnaz_obs <- subset(probab_golnaz_obs, probab_golnaz_obs$CDI == 1)
probab_golnaz_obs

unique_years <- length(unique(probab_golnaz_obs$hardiness_year))
unique_years

probab_year_obs <- unique_years / total_years
probab_year_obs

row_add <- data.frame("1979-2014", probab_year_obs)
names(row_add)<- c("Timeframe","probability")

probability_data <- rbind(probability_data, row_add)
probability_data

###### Diffrent models ######

models <- c("bcc-csm1-1","bcc-csm1-1-m","BNU-ESM","CanESM2",
            "CCSM4","CNRM-CM5","CSIRO-Mk3-6-0","GFDL-ESM2G","GFDL-ESM2M",
            "HadGEM2-CC365","HadGEM2-ES365", "inmcm4", "IPSL-CM5A-LR", "IPSL-CM5A-MR",
            "IPSL-CM5B-LR","MIROC5", "MIROC-ESM-CHEM", "MRI-CGCM3", "NorESM1-M")
models

models_path <- "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/"




for(names in models)
{
    out_dates_model <- read.csv(file = paste0(models_path,names,"/rcp85/output_data_48.40625_-119.53125.csv"), stringsAsFactors = FALSE)
    print(head(out_dates_model))
    print(sapply(out_dates_model, class))
    
    probab_golnaz_model <- subset(out_dates_model, out_dates_model$hardiness_year %in% (2026:2098))
    probab_golnaz_model
    
    # Seperating into bins
    golnaz_model_2026_2050 <- subset(probab_golnaz_model, probab_golnaz_model$hardiness_year %in% (2026:2050))
    golnaz_model_2026_2050
    total_years <- length(unique(golnaz_model_2026_2050$hardiness_year))
    total_years
    
    golnaz_model_2026_2050 <- subset(golnaz_model_2026_2050, golnaz_model_2026_2050$CDI == 1)
    golnaz_model_2026_2050
    
    
    unique_years <- length(unique(golnaz_model_2026_2050$hardiness_year))
    unique_years
    
    probab_year_model <- unique_years / total_years
    probab_year_model
    
    row_add <- data.frame("2026-2050", probab_year_model)
    names(row_add)<- c("Timeframe","probability")
    
    probability_data <- rbind(probability_data, row_add)
    probability_data
    
    # for the time frame 2051-2075
    golnaz_model_2051_2075 <- subset(probab_golnaz_model, probab_golnaz_model$hardiness_year %in% (2051:2075))
    
    golnaz_model_2051_2075
    total_years <- length(unique(golnaz_model_2051_2075$hardiness_year))
    total_years
    
    golnaz_model_2051_2075 <- subset(golnaz_model_2051_2075, golnaz_model_2051_2075$CDI == 1)
    golnaz_model_2051_2075
    
    
    unique_years <- length(unique(golnaz_model_2051_2075$hardiness_year))
    unique_years
    
    probab_year_model <- unique_years / total_years
    probab_year_model
    
    row_add <- data.frame("2051-2075", probab_year_model)
    names(row_add)<- c("Timeframe","probability")
    
    probability_data <- rbind(probability_data, row_add)
    probability_data
    
    
    
    
    # for the time frame 2076-2098
    golnaz_model_2076_2098 <- subset(probab_golnaz_model, probab_golnaz_model$hardiness_year %in% (2076:2098))
    # write.csv(golnaz_model_2076_2098, file="C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/output_data_46.28125_-119.34375.csv" )
    total_years <- length(unique(golnaz_model_2076_2098$hardiness_year))
    total_years
    
    golnaz_model_2076_2098 <- subset(golnaz_model_2076_2098, golnaz_model_2076_2098$CDI == 1)
    golnaz_model_2076_2098
    
    
    unique_years <- length(unique(golnaz_model_2076_2098$hardiness_year))
    unique_years
    
    probab_year_model <- unique_years / total_years
    probab_year_model
    
    row_add <- data.frame("2076-2098", probab_year_model)
    names(row_add)<- c("Timeframe","probability")
    
    probability_data <- rbind(probability_data, row_add)
    probability_data
    
    
  
  

  
}
probability_data

# ggplot()+
#   geom_point(data = probability_data, aes (x = probability_data$Timeframe, y= probability_data$probability, color = "probabilty"))+
#   xlab("Time frames")+
#   ylab("Probability")

# graph for Walla Walla
CDI_walla <- ggplot(data = probability_data, aes(x = probability_data$Timeframe, y= probability_data$probability, color = "probabilty"))+
  geom_boxplot()+
  geom_jitter()+
  xlab("Time frames")+
  ylab("Probability")+
  ggtitle("CDI for Walla Walla(WA)")
CDI_walla
ggsave("Probab_Walla.png",plot = CDI_walla, path = probab_path)

ggsave(paste0(probab_path, "golnaz_one_location_Walla_Walla.png"))


# graph for Yakima
CDI_Yakima <- ggplot(data = probability_data, aes(x = probability_data$Timeframe, y= probability_data$probability, color = "probabilty"))+
  geom_boxplot()+
  geom_jitter()+
  xlab("Time frames")+
  ylab("Probability")+
  ggtitle("CDI for Yakima(WA)")
CDI_Yakima
ggsave("Probab_Yakima.png",plot = CDI_Yakima, path = probab_path)


# graph for Richland
CDI_Richland <- ggplot(data = probability_data, aes(x = probability_data$Timeframe, y= probability_data$probability, color = "probabilty"))+
  geom_boxplot()+
  geom_jitter()+
  xlab("Time frames")+
  ylab("Probability")+
  ggtitle("CDI for Richland(WA)")
CDI_Richland
ggsave("Probab_Richland.png",plot = CDI_Richland, path = probab_path)

# graph for Wenatchee
CDI_Wenatchee <- ggplot(data = probability_data, aes(x = probability_data$Timeframe, y= probability_data$probability, color = "probabilty"))+
  geom_boxplot()+
  geom_jitter()+
  xlab("Time frames")+
  ylab("Probability")+
  ggtitle("CDI for Wenatchee(WA)")
CDI_Wenatchee
ggsave("Probab_Wenatchee.png",plot = CDI_Wenatchee, path = probab_path)

#graph for Omak
CDI_Omak <- ggplot(data = probability_data, aes(x = probability_data$Timeframe, y= probability_data$probability, color = "probabilty"))+
  geom_boxplot()+
  geom_jitter()+
  xlab("Time frames")+
  ylab("Probability")+
  ggtitle("CDI for Omak(WA)")
CDI_Omak
ggsave("Probab_Omak.png",plot = CDI_Omak, path = probab_path)
# All of them together
library(gridExtra)
grid.arrange(CDI_Omak,CDI_Richland,CDI_Wenatchee,CDI_walla,nrow = 3,ncol = 2 )

library(cowplot)
plot_grid(CDI_Omak,CDI_Richland,CDI_Wenatchee,CDI_walla,CDI_Yakima,ncol = 2, align = "v")


############################ Lesser time frames ###################################

probab_path <- "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/probability/"

probability_data_10 <- data.frame(Timeframe = character(),probability = numeric(), stringsAsFactors = FALSE)
sapply(probability_data_10, class)


out_dates_obs <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/observed/output_observed_historical_data_48.40625_-119.53125.csv", stringsAsFactors = FALSE)
print(head(out_dates_obs))
print(sapply(out_dates_obs, class))

probab_golnaz_obs <- subset(out_dates_obs, out_dates_obs$hardiness_year %in% (1979:2014)) 
# write.csv(probab_golnaz_obs, file ="C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/golnaz_obs.csv")
total_years <- length(unique(probab_golnaz_obs$hardiness_year))
total_years

# selecting  rows with only 1 in CDI
probab_golnaz_obs <- subset(probab_golnaz_obs, probab_golnaz_obs$CDI == 1)
probab_golnaz_obs

unique_years <- length(unique(probab_golnaz_obs$hardiness_year))
unique_years

probab_year_obs <- unique_years / total_years
probab_year_obs

row_add <- data.frame("1979-2014", probab_year_obs)
names(row_add)<- c("Timeframe","probability")

probability_data_10 <- rbind(probability_data_10, row_add)
probability_data_10

###### Diffrent models ######

models <- c("bcc-csm1-1","bcc-csm1-1-m","BNU-ESM","CanESM2",
            "CCSM4","CNRM-CM5","CSIRO-Mk3-6-0","GFDL-ESM2G","GFDL-ESM2M",
            "HadGEM2-CC365","HadGEM2-ES365", "inmcm4", "IPSL-CM5A-LR", "IPSL-CM5A-MR",
            "IPSL-CM5B-LR","MIROC5", "MIROC-ESM-CHEM", "MRI-CGCM3", "NorESM1-M")
models

models_path <- "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/"

for(names in models)
{
  out_dates_model <- read.csv(file = paste0(models_path,names,"/rcp85/output_data_48.40625_-119.53125.csv"), stringsAsFactors = FALSE)
  print(head(out_dates_model))
  print(sapply(out_dates_model, class))
  
  probab_golnaz_model <- subset(out_dates_model, out_dates_model$hardiness_year %in% (2020:2060))
  probab_golnaz_model
  
  # Seperating into bins
  golnaz_model_2020_2030 <- subset(probab_golnaz_model, probab_golnaz_model$hardiness_year %in% (2020:2030))
  golnaz_model_2020_2030
  total_years <- length(unique(golnaz_model_2020_2030$hardiness_year))
  total_years
  
  golnaz_model_2020_2030 <- subset(golnaz_model_2020_2030, golnaz_model_2020_2030$CDI == 1)
  golnaz_model_2020_2030
  
  
  unique_years <- length(unique(golnaz_model_2020_2030$hardiness_year))
  unique_years
  
  probab_year_model <- unique_years / total_years
  probab_year_model
  
  row_add <- data.frame("2020-2030", probab_year_model)
  names(row_add)<- c("Timeframe","probability")
  
  probability_data_10 <- rbind(probability_data_10, row_add)
  probability_data_10
  
  # for the time frame 2051-2075
  golnaz_model_2031_2040 <- subset(probab_golnaz_model, probab_golnaz_model$hardiness_year %in% (2031:2040))
  
  golnaz_model_2031_2040
  total_years <- length(unique(golnaz_model_2031_2040$hardiness_year))
  total_years
  
  golnaz_model_2031_2040 <- subset(golnaz_model_2031_2040, golnaz_model_2031_2040$CDI == 1)
  golnaz_model_2031_2040
  
  
  unique_years <- length(unique(golnaz_model_2031_2040$hardiness_year))
  unique_years
  
  probab_year_model <- unique_years / total_years
  probab_year_model
  
  row_add <- data.frame("2031-2040", probab_year_model)
  names(row_add)<- c("Timeframe","probability")
  
  probability_data_10 <- rbind(probability_data_10, row_add)
  probability_data_10
  
  
  
  
  # for the time frame 2076-2098
  golnaz_model_2041_2050 <- subset(probab_golnaz_model, probab_golnaz_model$hardiness_year %in% (2041:2050))
  # write.csv(golnaz_model_2076_2098, file="C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/output_data_46.28125_-119.34375.csv" )
  total_years <- length(unique(golnaz_model_2041_2050$hardiness_year))
  total_years
  
  golnaz_model_2041_2050 <- subset(golnaz_model_2041_2050, golnaz_model_2041_2050$CDI == 1)
  golnaz_model_2041_2050
  
  
  unique_years <- length(unique(golnaz_model_2041_2050$hardiness_year))
  unique_years
  
  probab_year_model <- unique_years / total_years
  probab_year_model
  
  row_add <- data.frame("2041-2050", probab_year_model)
  names(row_add)<- c("Timeframe","probability")
  
  probability_data_10 <- rbind(probability_data_10, row_add)
  probability_data_10
  
  
}
probability_data_10

CDI_walla_10 <- ggplot(data = probability_data_10, aes(x = probability_data_10$Timeframe, y= probability_data_10$probability, color = "probabilty"))+
  geom_boxplot()+
  geom_jitter()+
  xlab("Time frames")+
  ylab("Probability")+
  ggtitle("CDI for Walla Walla(WA)")
CDI_walla_10
ggsave("Probab_Walla_10.png",plot = CDI_walla_10, path = probab_path)

CDI_Yakima_10 <- ggplot(data = probability_data_10, aes(x = probability_data_10$Timeframe, y= probability_data_10$probability, color = "probabilty"))+
  geom_boxplot()+
  geom_jitter()+
  xlab("Time frames")+
  ylab("Probability")+
  ggtitle("CDI for Yakima(WA)")
CDI_Yakima_10
ggsave("Probab_Yakima_10.png",plot = CDI_Yakima_10, path = probab_path)


CDI_Richland_10 <- ggplot(data = probability_data_10, aes(x = probability_data_10$Timeframe, y= probability_data_10$probability, color = "probabilty"))+
  geom_boxplot()+
  geom_jitter()+
  xlab("Time frames")+
  ylab("Probability")+
  ggtitle("CDI for Richland(WA)")
CDI_Richland_10
ggsave("Probab_Richland_10.png",plot = CDI_Richland_10, path = probab_path)

CDI_Wenatchee_10 <- ggplot(data = probability_data_10, aes(x = probability_data_10$Timeframe, y= probability_data_10$probability, color = "probabilty"))+
  geom_boxplot()+
  geom_jitter()+
  xlab("Time frames")+
  ylab("Probability")+
  ggtitle("CDI for Wenatchee(WA)")
CDI_Wenatchee_10
ggsave("Probab_Richland_10.png",plot = CDI_Wenatchee_10, path = probab_path)

CDI_Omak_10 <- ggplot(data = probability_data_10, aes(x = probability_data_10$Timeframe, y= probability_data_10$probability, color = "probabilty"))+
  geom_boxplot()+
  geom_jitter()+
  xlab("Time frames")+
  ylab("Probability")+
  ggtitle("CDI for Omak(WA)")
CDI_Omak_10
ggsave("Probab_Omak_10.png",plot = CDI_Omak_10, path = probab_path)

grid.arrange(CDI_Omak_10, CDI_Wenatchee_10,CDI_Yakima_10, CDI_Richland_10 , ncol =2)
