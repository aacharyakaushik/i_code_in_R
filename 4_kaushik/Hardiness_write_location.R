#Reading a .dbf file
library(foreign)
VIC_file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/VICID_COUNTY_Table/VICID_CO.DBF"
VIC_file <- read.dbf(VIC_file, as.is = FALSE)


#column with state ID's
VIC_file

# Filtered columns of latitude and longitude for the states ID, WA, OR and MT  
VIC_file_filtered <- subset(VIC_file, STATE %in% c("WA"))[,6:7]

count(VIC_file_filtered) # take a count to cross-verify later

#Concatenation of 2 columns to a column using mutate dunction
library(tidyverse)
VIC_file_filtered_mutate <- VIC_file_filtered %>% mutate(locations = paste('data', VIC_file_filtered$VICCLAT,VIC_file_filtered$VICCLON, sep = "_"))

# count(VIC_file_filtered_mutate) # cross-verifying count here, if the number of rows is same kudos!!! You havent screwed up, YET!!!

# Write it into a csv file - writing only the locations, so that they can be used to extract files from aeolus for running the model
write.csv(VIC_file_filtered_mutate[,3], file = "lat_long_list.csv",row.names = FALSE)

df <-(VIC_file_filtered_mutate$locations)[1:10]
length(df)

input_file_path = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/input_parameters/"

for (filename in df){
  # print(filename)
  # setwd(input_file_path)

  file_found <- list.files(path= input_file_path,pattern = filename)
  # file_found<-paste0("Yayyy!!!! I found this file",files)
  print(file_found)
  
  
  
}




