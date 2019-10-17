
library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(reshape2)


consolidated_model <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/CanESM2/rcp85/consolidated_CanESM2_rcp85.csv")
print(head(consolidated_model))
consolidated_obs <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/observed/consolidated_observed_historical.csv",stringsAsFactors = FALSE)
print(head(consolidated_obs))

consolidated_obs <- data.table(consolidated_obs)
print(head(consolidated_obs))
print(sapply(consolidated_obs, class))

########################### Probability CDI based on golnaz paper ######################

probability_data <- data.frame(Timeframe = character(),probability = numeric(), stringsAsFactors = FALSE)
sapply(cluster_data, class)


out_dates_obs <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/observed/output_observed_historical_data_46.03125_-118.34375.csv", stringsAsFactors = FALSE)
print(head(out_dates_obs))
print(sapply(out_dates_obs, class))


out_dates_model <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/CanESM2/rcp85/output_data_46.03125_-118.34375.csv", stringsAsFactors = FALSE)
print(head(out_dates_model))
print(sapply(out_dates_model, class))

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
# write.csv(golnaz_model_2076_2098, file="C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/76-98.csv" )
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


ggplot()+
  geom_point(data = probability_data, aes (x = probability_data$Timeframe, y= probability_data$probability, color = "probabilty"))+
  xlab("Time frames")+
  ylab("Probability")


plot(probability_data$Timeframe, probability_data$probability)


###### Diffrent models ######

models <- c("bcc-csm1-1","bcc-csm1-1-m","BNU-ESM","CanESM2","CCSM4","CNRM-CM5","CSIRO-Mk3-6-0","GFDL-ESM2G","GFDL-ESM2M","HadGEM2-CC365")
models

models_path <- "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/"

for(names in models)
{
  out_dates_model <- read.csv(file = paste0(models_path,names,"/rcp85/output_data_45.84375_-120.84375.csv"), stringsAsFactors = FALSE)
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
  write.csv(golnaz_model_2076_2098, file="C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/output_data_45.84375_-120.84375.csv" )
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

ggplot()+
  geom_point(data = probability_data, aes (x = probability_data$Timeframe, y= probability_data$probability, color = "probabilty"))+
  xlab("Time frames")+
  ylab("Probability")


ggplot()+
  geom_boxplot(data = probability_data, aes(x = probability_data$Timeframe, y= probability_data$probability, color = "probabilty"))+
  xlab("Time frames")+
  ylab("Probability")


#################### Probability CDI ########################

out_dates_obs <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/observed/output_observed_historical_data_46.03125_-118.34375.csv", stringsAsFactors = FALSE)
print(head(out_dates_obs))
print(sapply(out_dates_obs, class))


out_dates_model <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/CanESM2/rcp85/output_data_46.03125_-118.34375.csv", stringsAsFactors = FALSE)
print(head(out_dates_model))
print(sapply(out_dates_model, class))

# read the file name
basename("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/CanESM2/rcp85/output_data_46.03125_-118.34375.csv")

### think about removing the first 9 months of the ile, it can be misleading
probability_obs <- subset(out_dates_obs, out_dates_obs$hardiness_year %in% (1979:2014))
probability_obs
# write.csv (probability_obs, file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/prob_obs.csv" ) 

probability_model <- subset(out_dates_model, out_dates_model$hardiness_year %in% (2026:2098))
probability_model
# write.csv(probability_model, file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/prob_model.csv")



# calculation sum yearly
# prob_obs <- tapply(probability_obs$CDI, probability_obs$hardiness_year, FUN = sum)

prob_obs <- aggregate(probability_obs$CDI, by=list(hardiness_year=probability_obs$hardiness_year), FUN=sum)

names(prob_obs)[2] <- "yearly_CDI"
sapply(prob_obs, class)

prob_obs$probability <- ifelse(prob_obs$hardiness_year %% 4 == 0, prob_obs$yearly_CDI/366, prob_obs$yearly_CDI/365)
prob_obs


# Calucation for model
prob_model <- aggregate(probability_model$CDI, by=list(hardiness_year=probability_model$hardiness_year), FUN=sum)
prob_model
names(prob_model)[2] <- "yearly_CDI"
sapply(prob_model, class)

prob_model$probability <- ifelse ((prob_model$hardiness_year %% 4) == 0, prob_model$yearly_CDI/366, prob_model$yearly_CDI/365)

# Seperating into bins
prob_model_2026_2050 <- subset(prob_model, prob_model$hardiness_year %in% (2026:2050))
prob_model_2026_2050
prob_model_2051_2075 <- subset(prob_model, prob_model$hardiness_year %in% (2061:2075))
prob_model_2076_2098 <- subset(prob_model, prob_model$hardiness_year %in% (2076:2098))


# ploting the graphs
p1 <- ggplot()+
  geom_line(data = prob_obs, aes(x = prob_obs$hardiness_year, y = prob_obs$probability))+
  xlab('Time (1979-2016)')+
  ylab('probability')
p1

p2 <- ggplot()+
  geom_line(data = prob_model_2026_2050, aes(x = prob_model_2026_2050$hardiness_year, y = prob_model_2026_2050$probability))+
  xlab('Time (2026-2050)')+
  ylab('probability')

p3 <- ggplot()+
  geom_line(data = prob_model_2051_2075, aes(x = prob_model_2051_2075$hardiness_year, y = prob_model_2051_2075$probability))+
  xlab('Time frame (2051-2075)')+
  ylab('probability')


p4 <- ggplot()+
  geom_line(data = prob_model_2076_2098, aes(x = prob_model_2076_2098$hardiness_year, y = prob_model_2076_2098$probability))+
  xlab('Time frame (2076-2098)')+
  ylab('probability')


# to arrange the graphs
library(gridExtra)
grid.arrange(p1,p2,p3,p4,nrow =2, ncol = 2 )
ggsave()

# boxplot
# ggplot()+
#   geom_boxplot()


# # probability Calculation
# total_rows <- nrow(probability_obs)
# total_rows
# CDI_rows <- sum(probability_obs$CDI == "1" )
# CDI_rows

############### Clustering  code ####################################


# creating a clustering data frame for highest average predicted HC value
cluster_data <- data.frame(Location= character(),location_mean = numeric(), stringsAsFactors = FALSE)
sapply(cluster_data, class)

# List of files in the loaction

file_location <- "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/observed/"

files_Total <- list.files(path= file_location)
files_Total

for (names in files_Total)
{
  if (names != "consolidated_observed_historical.csv"){
    
    print(names)
    
    # accessing the second file name
    files_Total[2]
    
    # Adding it to a varibale
    file_name <- names#files_Total[2]
    file_name 
    
    # seperating the "_" to concatenate lat and long
    file_name <- unlist(strsplit(file_name, "_"))
    file_name <- paste0(file_name[[5]],file_name[[6]])
    
    # removing the .csv from the file name
    library(stringr)
    file_name <- str_extract(file_name, '.*(?=\\.csv)')
    
    
    read_file <- paste0(file_location,files_Total[2])
    read_file
    
    file_read <- read.csv(file = read_file, stringsAsFactors = FALSE)
    sapply(file_read, class)
    
    file_read_dup <- data.table(file_read)
    file_read_dup <- subset(file_read_dup, file_read_dup$hardiness_year %in% (1979:2005))
    sapply(file_read_dup, class)
    head(file_read_dup)
    
    file_read_dup <- file_read_dup[, max(predicted_Hc),by = hardiness_year]
    colnames(file_read_dup)
    highest_mean <- colMeans(file_read_dup)
    
    highest_mean[["V1"]]
    
    row_add <- data.frame(file_name, highest_mean[["V1"]])
    names(row_add)<- c("Location","location_mean")
    
    cluster_data <- rbind(cluster_data, row_add)
  
    
  }
}
write.csv(cluster_data, file = paste0(file_location, "cluster.csv"))


cluster_data
# accessing the second file name
files_Total[2]

# Adding it to a varibale
file_name <- files_Total[2]
file_name 

# seperating the "_" to concatenate lat and long
file_name <- unlist(strsplit(file_name, "_"))
file_name <- paste0(file_name[[5]],file_name[[6]])

# removing the .csv from the file name
library(stringr)
file_name <- str_extract(file_name, '.*(?=\\.csv)')


read_file <- paste0(file_location,files_Total[2])
read_file

file_read <- read.csv(file = read_file, stringsAsFactors = FALSE)
sapply(file_read, class)

file_read_dup <- data.table(file_read)
file_read_dup <- subset(file_read_dup, file_read_dup$hardiness_year %in% (1979:2005))
sapply(file_read_dup, class)
head(file_read_dup)

file_read_dup <- file_read_dup[, max(predicted_Hc),by = hardiness_year]
colnames(file_read_dup)
highest_mean <- colMeans(file_read_dup)

highest_mean[["V1"]]

row_add <- data.frame(file_name, highest_mean[["V1"]])
names(row_add)<- c("Location","location_mean")

cluster_data <- rbind(cluster_data, row_add)
cluster_data


############ CDI time series plot over the years ###########################
obs <- subset(consolidated_obs, select = -c(Time_elapsed))
obs <- melt(obs)
obs
sapply(obs,class)
# The column varibale is in factor form , chanaging it to character
obs$variable<- as.character(obs$variable)

# removing x from the year(e.gx2016) 
obs$variable <- substring(obs$variable,2)

# changing it to numeric
obs$variable <- as.numeric(obs$variable)

model <- subset(consolidated_model, select = -c(Time_elapsed, Model, Scenario))
model <- melt(model)





model

# The column varibale is in factor form , chanaging it to character
model$variable<- as.character(model$variable)

# removing x from the year(e.gx2016) 
model$variable <- substring(model$variable,2)

# changing it to numeric
model$variable <- as.numeric(model$variable)


geom

# checking for trend line - dont use later
model <- subset(model, model$variable %in% (2006:2061))

count_CDI_plot <- rbind(obs[-c(28:38)],model)
count_CDI_plot

ggplot(data = count_CDI_plot, aes(x = variable, y = value))+
  geom_line(data = obs, aes(x=variable, y =value, color ="obs"))+
  geom_line(data = model, aes(x= variable, y = value,color = "model"))+
  xlab('YEAR')+
  ylab('#CDI')+
  geom_smooth(method = "lm", aes(color ="trend"), se = TRUE)
  
  # geom_smooth(method = "lm")
ggplot()+
  # geom_line(data = obs, aes(x=variable, y =value, color ="obs"))+
  geom_line(data = model, aes(x= variable, y = value,color = "model"))+
  xlab('YEAR')+
  ylab('#CDI')+
  # geom_smooth(method = "lm", aes(color ="trend"), se = TRUE)


ggplot(data = model, aes(x= as.numeric(variable), y = value,color = "model"))+
  geom_line()+
  geom_smooth(method = "lm",aes(color ="trend"), se = FALSE)

colnames(consolidated_obs)
sapply(consolidated )
consolidated_obs$X2016 <- as.numeric(consolidated_obs$X2016)


############### Frequency graph #########################
consolidated_obs$total_CDI <-  rowSums(consolidated_model[, 3:39])
sapply(consolidated_obs, class)        
# dim(consolidated_obs$total_CDI)
print(consolidated_obs)
sapply(consolidated_obs,max, na.rm = TRUE)

consolidated_obs$bin <-  cut(x=consolidated_obs$total_CDI, breaks=seq(from=0, to= 550, by = 50),include.lowest =TRUE)
write.csv(consolidated_obs, file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/CDI.csv")


ggplot(data = consolidated_obs, aes(x = consolidated_obs$bin,fill= ..count..))+
  geom_bar(color= 'black', alpha =0.8)+
  stat_count(geom="text", aes(label=..count..), hjust=-0.1) +
  theme_bw() + 
  labs(y="Total CDI",x="Mean Education Values") +
  coord_flip() +
  ylim(0,5000) +
  scale_x_discrete(drop=FALSE)
  
reg <- coefficients()

############# CDI for all models ##########################
CDI_models <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/parameters/config.csv")
CDI_models_Omak<- subset(CDI_models, CDI_models$Scenario == "rcp85" & CDI_models$Location == "data_48.40625_-119.53125")
CDI_models_Omak

CDI_models_Wenatchee <- subset(CDI_models, CDI_models$Scenario == "rcp85" & CDI_models$Location == "data_47.40625_-120.34375")
CDI_models_Wenatchee

CDI_models_Richland <- subset(CDI_models, CDI_models$Scenario == "rcp85" & CDI_models$Location == "data_46.28125_-119.34375")
CDI_models_Richland 


CDI_models_Yakima <- subset(CDI_models, CDI_models$Scenario == "rcp85" & CDI_models$Location == "data_46.59375_-120.53125")
CDI_models_Yakima


CDI_models_Walla <- subset(CDI_models, CDI_models$Scenario == "rcp85" & CDI_models$Location == "data_46.03125_-118.34375")
CDI_models_Walla



