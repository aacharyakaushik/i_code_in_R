library(data.table)
library(tidyverse)

file_path ="C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/abc"
data_PPT = read.table(file_path,header = FALSE, sep = " ")[,1]
data_PPT
data_PPT = file(file_path,"rb")
data_TMAX = read.table(file_path, header = FALSE, sep = " ")[,2]
data_TMAX = file(file_path,"rb")

data_TMIN = file(file_path,"rb")
data_TMIN <- read.table(file_path, header = FALSE, sep = " ")[,3]
data_TMIN

#data = read.table(file_path, header = FALSE, sep="")
#summary(data)
#varnames = readBin(data, "raw",n=800)


varnames_PPT = (readBin(data_PPT,"integer",signed = FALSE, size = 1, n=100))
varnames_PPT
varnames_PPT <- transform((varnames_PPT)/40)
varnames_PPT
varnames_TMAX = transform((readBin(data_TMAX,"int", signed = TRUE,size =1, n =100))/100)
varnames_TMAX
varnames_TMIN = (readBin(data_TMIN,"integer",signed = TRUE, size = 2, n=100))
varnames_TMIN
varnames_TMIN <- transform((varnames_TMIN)/100)
varnames_TMIN
close(data_PPT)
close(data_TMAX)
close(data_TMIN)
#readRDS("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/abc.",refhook = NULL)

path = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/_data.rds"
path <- readRDS("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/_data.rds")
path

#working code for conversion of binary file, the problem is I am not able to divide the file column wise to covert them seperatly based on columns.
read.filename <- file(file_path,"rb")
column.names <-readBin(read.filename, integer(),signed = TRUE,size = 2, n = 64)
column.names <-transform(column.names/100)

column.names <- matrix()
column.names
close(read.filename)

#Reading a .dbf file
library(foreign)
dbf_file_VIC = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/VICID_COUNTY_Table/VICID_CO.DBF"
dbf_VIC <- read.dbf(dbf_file_VIC, as.is = FALSE)

#column with state ID's
dbf_VIC
#dbf_VIC <- dbf_VIC[dbf_VIC$STATE == 'WA'|dbf_VIC$STATE == 'ID',] --> This did not serve the purpose

# Filtered columns of latitude and longitude for the states ID, WA, OR and MT  
dbf_VIC_Filtered <- subset(dbf_VIC, STATE %in% c("ID", "WA", "OR", "MT"))[,6:7]
dbf_VIC_Filtered

# Write it into a csv file.
write.csv(dbf_VIC_Filtered, file = "lat_long_list.csv")

# Concatenating the 2 columns to form a column using unite function
dbf_VIC_Filtered_unite <- dbf_VIC_Filtered %>% unite (locations, c("VICCLAT","VICCLON")) # observed some rows deleted from the original table --> Insvestigate why?
dbf_VIC_Filtered_unite

#Concatenation of 2 columns to a column using mutate dunction
dbf_VIC_Filtered_mutate <- dbf_VIC_Filtered %>% mutate(locations = paste(dbf_VIC_Filtered$VICCLAT,dbf_VIC_Filtered$VICCLON, sep = "_"))
dbf_VIC_Filtered_mutate

# Concatenate data_ to the column name, so the file names will be exact to that of the files in aeolus
dbf_VIC_Filtered_mutate_final <- dbf_VIC_Filtered %>% mutate(locations = paste('data', dbf_VIC_Filtered$VICCLAT,dbf_VIC_Filtered$VICCLON, sep = "_"))
dbf_VIC_Filtered_mutate_final$locations
summary(dbf_VIC_Filtered_mutate_final)


# Reading dbf file VIC grid
dbf_file_VIC_grid = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/vic_grid_cover_conus/vic_grid.dbf"
dbf_VIC_grid <- read.dbf(dbf_file_VIC_grid, as.is = FALSE)
dbf_VIC_grid




# reading the dbf file in aeolus location, finally decided to the use the fuck me commented code(because it was working just fine)
library(data.table)
data_file_path = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/data_49.03125_-122.15625"
data_file <- file(data_file_path, "rb")


# Convert date into jday
tmp <- as.Date("6/7/2019", format ="%d/%m/%y")
jday <- as.numeric(format (tmp, "%j")) -1 # because the range is 1-366
jday

