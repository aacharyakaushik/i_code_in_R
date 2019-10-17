library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

args = '1'#commandArgs(trailingOnly = TRUE)

source_path = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/hardiness_core.R"
source(source_path)

#Reading a .dbf file
library(foreign)
VIC_file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/VIC_file/VICID_CO.DBF"
VIC_file <- read.dbf(VIC_file, as.is = FALSE)


param_dir = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/parameters/"
# read parameters
options(digits=9)
input_params  = data.table(read.csv(paste0(param_dir, "input_parameters", ".csv")))
variety_params = data.table(read.csv(paste0(param_dir, "variety_parameters", ".csv")))

climate_model = data.table(read.csv(paste0(param_dir,"climate_model_config",".csv")))
climate_model 
climate_model <- climate_model[climate_model$ARRAYID == args,]



output_dir <- "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/output_data/"

#column with state ID's
VIC_file

# Filtered columns of latitude and longitude for the states ID, WA, OR and MT  
VIC_file_filtered <- subset(VIC_file, STATE %in% c("ID", "WA", "OR", "MT"))[,5:7]

# count(VIC_file_filtered) # take a count to cross-verify later

#Concatenation of 2 columns to a column using mutate dunction
library(tidyverse)
VIC_file_filtered_mutate <- VIC_file_filtered %>% mutate(locations = paste('data', VIC_file_filtered$VICCLAT,VIC_file_filtered$VICCLON, sep = "_"))

# count(VIC_file_filtered_mutate) # cross-verifying count here, if the number of rows is same kudos!!! You havent screwed up, YET!!!

# Write it into a csv file - writing only the locations, so that they can be used to extract files from aeolus for running the model
# write.csv(VIC_file_filtered_mutate$locations, file = "lat_long_list.csv",row.names = FALSE)
VIC_file_filtered_mutate

# df <-(VIC_file_filtered_mutate$locations)[1:10,] - This was written to select only location from the df

df <- VIC_file_filtered_mutate[1,] %>% select("locations", "COUNTY")
# df <- df[1:10,]
df
# count(df) - check the number of rows in df
 
input_file_path = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/input_data/"
input_file_path <- paste0(input_file_path,climate_model$MODEL,"/",climate_model$SCENARIO,"/")
input_file_path

for (filename in df$locations){
  # print(filename)
  # setwd(input_file_path)
  
  file_found <- list.files(path= input_file_path,pattern = filename)
  # file_found<-paste0("Yayyy!!!! I found this file",files)
  # print(file_found)
  file_found <- paste0(input_file_path,file_found)
  # print(file_found)
  
  meta_data <- data.table(read_binary(file_path = file_found, hist = hist, no_vars= 4))
  # print(count(meta_data))
  
  # mutate the table to have desired columns
  # Really proud of this code, but this can be  better
  # To calculate the jday, we combined the column of day, month and year and then converted it into date format, then into jday format and then to numeric
  # It was converted to numeric because the "%j" format is from 1-366, we needed 0-365, hence subtracting 1 from the number obtained
  # Try a more cleaner way, if you find time and still have interest
  
  meta_data <- meta_data %>% mutate(Date = paste0(meta_data$day,"/",meta_data$month,"/",meta_data$year))
  meta_data <- meta_data %>% mutate(jday = as.numeric(format(as.Date(meta_data$Date,format = "%d/%m/%y"),"%j"))-1) 
  # print(count(meta_data))
  
  
  # Add T_mean for the table, calculationg mean from the two rows of tmax and tmin
  meta_data <- meta_data %>% mutate(T_mean = (meta_data$tmax + meta_data$tmin)/2)
  # print(count(meta_data))
  
  # Add loaction, adding COUNTY name for now
  # print(filename)
  meta_data <- meta_data %>% mutate(Location = df$COUNTY[df$locations == filename])
  
  
  meta_data <- meta_data %>% select("Date", "year", "jday","Location", "T_mean", "tmax", "tmin")
  # print(head(meta_data))
  
  print(meta_data)
  
  
  output <- hardiness_model(data = meta_data, input_params = input_params,
                            variety_params = variety_params)
  write.csv(output,
            file = paste0(output_dir, "model_output_",filename,".csv"),
            row.names=FALSE)
  
  
  
  
  
  # This is the code for plotting the map
  # source_path = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/plot_core.R"
  # source(source_path)
  # 
  # plot_dir <- output_dir
  # out_name = paste0("model_output",filename)
  # plot_hardiness(output, plot_dir, out_name)
  
}


