


###############################################################
# Preparing a Lat Long list for both grid Data and AG weather
###############################################################

# Creating a table of lat long locations from Grid Data

grid_table <- read.csv("C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/Output_data/freezing_observed.csv")
head(grid_table)

grid_table <- grid_table[, c("lat", "long")]


# creating a table of lat long locations from Agweather
AG_table <- read.csv("C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/input_data/AWN_T_P_DAILY/AWN_T_P_DAILY.csv")
head(AG_table)

AG_table <- AG_table[,c("STATION_ID","LAT", "LNG")]

# changing the column names to be consistent across data
colnames(AG_table) <- c("Station_ID", "lat", "long")

# selecting unique stations as the we need only 199 values
AG_table <- AG_table[!duplicated(AG_table[,c("Station_ID")]),]

# checking the length of the unique values
length(AG_table$Station_ID)

# resetting the index
rownames(AG_table) <- NULL


##########################################
# Finding nearest neighbors of lat long
#########################################
library(rgeos)


###################### DOesnt Work ######################################
################# Beacuse the dataframes are not equal #################
# grid_table_sp <- SpatialPoints(grid_table)
# AG_table_sp <- SpatialPoints(AG_table)
# 
# AG_table$nearest_in_grid <- apply(gDistance(AG_table_sp,grid_table_sp,
#                                            byid = TRUE),1,which.min)
#######################################################################


library(Imap)

nearest_location <- data.table(matrix(NA, nrow=dim(AG_table)[1], ncol=6))
colnames(nearest_location) <- c("Station_ID","AG_lat","AG_long",
                                "grid_lat", "grid_long", "dist")

# create a dataframe to store the values
nearest_location <- data.frame(Station_ID = character(),
                               AG_lat = numeric(),
                               AG_long = numeric(),
                               grid_lat = numeric(),
                               grid_long = numeric(),
                               dist = numeric ())
nearest_location

# getting the number of rows for the loop 
n_rows = dim(AG_table)[1]
n_rows


################################################################
# following code is to calculate the minimum distance using 
# gdist 
###############################################################
for (row_count in 1:n_rows){
  
  Lat <- AG_table[row_count,2]
  Long <- AG_table[row_count,3]
  
  
  grid_table$dist <- gdist(lat.1 = grid_table$lat,
                           lon.1 = grid_table$long,
                           lat.2 = Lat,
                           lon.2 = Long)
  
  
  nearest_location_grid <- grid_table[grid_table$dist == min(grid_table$dist),]
  nearest_location_grid
  
  # Adding values o the data frame
  nearest_location$Station_ID[row_count] = AG_table$Station_ID[row_count]
  nearest_location$AG_lat[row_count] = Lat
  nearest_location$AG_long[row_count] = Long
  nearest_location$grid_lat[row_count] = nearest_location_grid$lat
  nearest_location$grid_long[row_count] = nearest_location_grid$long
  nearest_location$dist[row_count] = nearest_location_grid$dist
  
  
}
dim(nearest_location)

############################################
# Writing Output toa csv and rds file
############################################
output_dir <- "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/Output_data/AG_weather/"

# writing into CSV
write.csv(nearest_location, file = paste0(output_dir, "grid_AG_compare.csv"))

# writing into RDS
saveRDS(nearest_location, file = paste0(output_dir,"grid_AG_compare.rds"))

####################################
# Read the files
#####################################

locations <- read.csv("C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/Output_data/AG_weather/grid_AG_compare.csv")
dim(locations)
head(locations)

locations <- locations %>% select(grid_lat, grid_long)
locations$location <- paste0("output_observed_historical_data_", locations$grid_lat,"_",locations$grid_long,".csv")

length(unique(locations$location))
files_needed <- unique(locations$location)
files_needed

downloaded_file <- list.files("C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/for_Vlad/")
length(unique(downloaded_file))

files_different <- setdiff(files_needed, downloaded_file)
unique(files_different)

write.csv(files_different, "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/for_Vlad/missing_files.csv")
