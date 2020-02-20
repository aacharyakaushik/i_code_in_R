.libPaths("/data/hydro/R_libs35")
.libPaths()


library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

options(digits=9)

# source_path = "/home/kraghavendra/hardiness/hardiness_core.R"
source_path = "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/hardiness_core.R"
source(source_path)

### Seeting the AGweater CSV file to usable format
# Read the AGweather net file

param_dir = "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/parameters/"
input_params  = data.table(read.csv(paste0(param_dir, "input_parameters", ".csv")))
variety_params = data.table(read.csv(paste0(param_dir, "variety_parameters", ".csv")))


input_file = read.csv("C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/input_data/AWN_T_P_DAILY/AWN_T_P_DAILY.csv")

head(input_file)

#############################################################################
# Preparing the file for the function hardiness_model_AGweather
############################################################################

# changing column names to suit the function
names(input_file)
colnames(input_file) <- c("Station_ID", "Station_name", "latitude", "longitude",
                          "Elevation_feet", "Date", "tmax", "tmin", "precip")


#the temperatures are in Farhrenheit, converting to celcius
library(weathermetrics)
input_file$tmax <- fahrenheit.to.celsius(input_file$tmax)
input_file$tmin <- fahrenheit.to.celsius(input_file$tmin)

# create a mean temperature value for hardiness calculation
input_file <- input_file %>% mutate(T_mean = (input_file$tmax + 
                                                input_file$tmin)/2)

# Adding year column
input_file$Date = as.Date(input_file$Date, format = "%Y-%m-%d")

# checking for NA values in Dates , if present removing the row
sum(is.na(input_file$Date))
input_file <- na.omit(input_file)

# Doing this inside the loop
# # Seperating year to form a column
# input_file$year = as.numeric(format(input_file$Date,"%Y"))
# 
# # adding jday
# input_file$jday = as.numeric(format(as.Date(input_file$Date,format = "%Y-%m-%d"), "%j"))-1
  
# meta_data <- meta_data %>% mutate(jday = as.numeric(format(as.Date(meta_data$Date,format = "%m/%d/%y"),"%j"))-1)   
# print(meta_data)


# checking the file for one last time, to see I have done it right
head(input_file)


###############################################################
# Code to find missing Dates and fill wit with value
###############################################################


# compare the two
library(compare)



# number of unique Stations
length(unique(input_file$Station_name))

# Adding the name of the locations to a vector
AG_locations <- unique(input_file$Station_name)
# AG_locations <- AG_locations[1:5]
AG_locations

# 
# # checking for NA values and removing the rows
# sum(is.na(AG_weather_data$Date))
# AG_weather_data <- na.omit(AG_weather_data)


#########################################
# Running the hardiness Model
#########################################
output_dir <- "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/Output_data/AG_weather/"

# AG_output = hardiness_model_AGweather(AG_data = AG_weather_data, input_params = input_params, variety_params = variety_params)

AG_output = data.frame()

# loop over the locations
for (place in AG_locations){
  
  # seperating one station for now
  AG_weather_data = subset(input_file, input_file$Station_name == place)
  print(head(AG_weather_data))
  
  ###################################
  # Code for missing date
  ##################################
  # Selecting one location to start off
  # AG_location_one <- subset(input_file, input_file$Station_ID == 100140)
  # AG_location_one
  
  # get start date and end date
  start_date <- head(AG_weather_data$Date,1)
  start_date
  end_date <- tail(AG_weather_data$Date,1)
  end_date
  
  library(tidyr)
  
  
  AG_weather_data_mod <- AG_weather_data %>%
    mutate(Date = as.Date(Date)) %>%
    complete(Date = seq.Date(min(Date), max(Date), by = "day"))%>%
    fill('Station_ID','Station_name','latitude','longitude','Elevation_feet',
         'latitude','longitude','tmax', 'tmin', 'precip', 'T_mean')
  
  
  # creating jday and year after making these changes
  AG_weather_data_mod$jday = as.numeric(format(as.Date(AG_weather_data_mod$Date,format = "%Y-%m-%d"), "%j"))-1
  
  # year column
  AG_weather_data_mod$year = as.numeric(format(AG_weather_data_mod$Date,"%Y"))
  
  # missing dates
  missing_dates <- anti_join(AG_weather_data_mod, AG_weather_data)
  
  # check
  # head(AG_location_one)
  write.table(missing_dates, file = paste0(output_dir,"missing_dates.csv"),
              col.names = !file.exists(paste0(output_dir,"missing_dates.csv")), sep = ',',row.names = FALSE,
              append = TRUE)
  
  # write.table(output_CDI,paste0(output_dir,subDir,"consolidated_",
  #                               climate_model$Model,"_",climate_model$Scenario,".csv"),
  #             sep = ",",col.names = !file.exists(paste0(output_dir,subDir,
  #                                                       "consolidated_",
  #                                                       climate_model$Model,"_",
  #                                                       climate_model$Scenario,".csv")
  #                                                ),row.names= FALSE,append = TRUE )  
  # 
  # write.csv(AG_location_one, file = paste0(output_dir, "one.csv"))
  
  # checking for NA values in Dates , if present removing the row
  # sum(is.na(AG_location_check))
  # colnames(AG_location_check)
  
  
  output = hardiness_model_AGweather(AG_data = AG_weather_data_mod, input_params = input_params, variety_params = variety_params)
  output_frame <- data.frame(output)
  AG_output <- rbind(AG_output, output_frame) 
}

head(AG_output)

# adding hardiness year to the dataframe
AG_output$hardiness_year <- ifelse(leap_year(AG_output$year),ifelse(AG_output$jday <=136,AG_output$year-1,
                                                              ifelse(AG_output$jday >=244 & AG_output$jday < 367,AG_output$year,0)),
                                ifelse(AG_output$jday <=135,AG_output$year-1,
                                       ifelse(AG_output$jday >=243 & AG_output$jday < 366,AG_output$year,0)))


####################################
# Write to a output file
####################################

write.csv(AG_output, file = paste0(output_dir, "AGweather.csv"))

saveRDS(AG_output, file = paste0(output_dir, "AGweather.rds"))


Summary_dates<- read.csv(paste0(output_dir,"missing_dates.csv"))

dim(Summary_dates)
head(Summary_dates)

