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

Hc_initial<- NA
if (grepl("1/1",meta_data$Date[1])== TRUE)
{
  Hc_initial <- input_params$Hc_initial[1]
  Hc_min<- input_params$Hc_min[1]
  Hc_max <- input_params$Hc_max[1] 
}
Hc_initial

# to plot end of month values for hardiness
out_dates <- read.csv(file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/bcc-csm1-1/rcp45/output_data_45.59375_-122.53125.csv")
sapply(out_dates$Date,class)
out_dates$Date
# out_dates$Date <-strptime(as.character(out_dates$Date), "%d/%m/%Y")
# sapply(out_dates$Date, class)
# ymd(out_dates$Date)
out_dates$Date <- as.Date(out_dates$Date, format ="%m/%d/%Y")
# out_dates$Date <- strptime(out_dates$Date, format = "%d%m%y")
out_dates$Date
sapply(out_dates$Date,class)

# code for calculation of last day from the data frame
month_end <- aggregate(out_dates["Date"],list(month = substr(out_dates$Date,1,7)),max)
month_end$Date

# Calaculation of end dates for all month
end_month_values <- subset(out_dates, out_dates$Date %in% month_end$Date)
head(end_month_values)

# making a data frame for individual 
end_Sep <- subset(end_month_values, format.Date(Date, "%m")=="09")
end_Sep

end_Oct <- subset(end_month_values,format.Date(Date, "%m")=="10")
end_Oct 

end_Nov <- subset(end_month_values, format.Date(Date, "%m")=="11")
end_Nov

end_Dec <- subset(end_month_values, format.Date(Date,"%m")=="12")
end_Dec

end_Jan <- subset(end_month_values, format.Date(Date,"%m")=="01")
end_Jan

end_Feb <- subset(end_month_values, format.Date(Date,"%m")=="02")
end_Feb

end_Mar <- subset(end_month_values, format.Date(Date,"%m")=="03")
end_Mar

end_Apr <- subset(end_month_values, format.Date(Date,"%m")=="04")
end_Apr

end_May <- subset(end_month_values, format.Date(Date,"%m")=="05")
end_May

# test to see whether i can plot the plots, and turns out i can
qplot(end_Sep$Date, end_Sep$predicted_Hc)
i <- ggplot(end_Sep, aes(end_Sep$Date, end_Sep$predicted_Hc))
i+geom_line()


# Plotting the end of each month over the years 2006 to 2099, kirti asked for this
ggplot()+
  geom_line(data = end_Sep,aes(x = end_Sep$Date,y = end_Sep$predicted_Hc,color = "Sep"))+
  xlab('Date')+
  ylab('Predicted Hc')+
  ggtitle('Spetember')+
  labs(color = "Legend")
ggsave("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/Sep_end_plot.png")

# for the month of October
ggplot()+
  geom_line(data = end_Oct,aes(x = end_Oct$Date,y = end_Oct$predicted_Hc,color = "Oct"))+
  xlab('Date')+
  ylab('Predicted Hc')+
  ggtitle('October')+
  labs(color = "Legend")
ggsave("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/Oct_end_plot.png")


# for the month of Novemeber
ggplot()+
  geom_line(data = end_Nov,aes(x = end_Nov$Date,y = end_Nov$predicted_Hc,color = "Nov"))+
  xlab('Date')+
  ylab('Predicted Hc')+
  ggtitle('November')+
  labs(color = "Legend")
ggsave("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/Nov_end_plot.png")

# for the  month of december
ggplot()+
  geom_line(data = end_Dec,aes(x = end_Dec$Date,y = end_Dec$predicted_Hc,color = "Dec"))+
  xlab('Date')+
  ylab('Predicted Hc')+
  ggtitle('December')+
  labs(color = "Legend")
ggsave("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/Dec_end_plot.png")


# for the month of January
ggplot()+
  geom_line(data = end_Jan,aes(x = end_Jan$Date,y = end_Jan$predicted_Hc,color = "Jan"))+
  xlab('Date')+
  ylab('Predicted Hc')+
  ggtitle('January')+
  labs(color = "Legend")
ggsave("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/Jan_end_plot.png")

# for the month of February 
ggplot()+
  geom_line(data = end_Feb,aes(x = end_Feb$Date,y = end_Feb$predicted_Hc,color = "Mar"))+
  xlab('Date')+
  ylab('Predicted Hc')+
  ggtitle('February')+
  labs(color = "Legend")
ggsave("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/Feb_end_plot.png")

# for the month of March
ggplot()+
  geom_line(data = end_Mar,aes(x = end_Mar$Date,y = end_Mar$predicted_Hc,color = "Mar"))+
  xlab('Date')+
  ylab('Predicted Hc')+
  ggtitle('March')+
  labs(color = "Legend")
ggsave("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/Mar_end_plot.png")


# for the month of Apr
ggplot()+
  geom_line(data = end_Apr,aes(x = end_Apr$Date,y = end_Apr$predicted_Hc,color = "Apr"))+
  xlab('Date')+
  ylab('Predicted Hc')+
  ggtitle('April')+
  labs(color = "Legend")
ggsave("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/Apr_end_plot.png")


# for the month of May
ggplot()+
  geom_line(data = end_May,aes(x = end_May$Date,y = end_May$predicted_Hc,color = "May"))+
  xlab('Date')+
  ylab('Predicted Hc')+
  ggtitle('May')+
  labs(color = "Legend")
ggsave("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/May_end_plot.png")


#   
#   
#   
#   
#   

# seperating months to plot for desnity plot for september
head(out_dates)
den_Sep <- subset(out_dates, format.Date(Date,"%m")=="09")

ggplot()+
  geom_density(data = den_Sep,aes(den_Sep$predicted_Hc,color = "Sep"))+
  xlab('Predicted Hc')
ggsave("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/Sep_dense_plot.png")

density_2006 <- subset(out_dates,out_dates$hardiness_year == "2006")
density_2006
# for October

ggplot()+
  geom_density(data = density_2006,aes(density_2006$predicted_Hc,color = "2006"))+
  xlab('Predicted Hc')
ggsave("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/2006_dense_plot.png")

# calculation of PEak for density 
d = density(density_2006$predicted_Hc, n=1e6)
i = which.max(d$y)
sprintf('peak is %f with probability %f', d$y[i],d$x[i])
# step plot for the same             

mean_months <- aggregate(data = out_dates, predicted_Hc ~ format.Date(Date, "%m%Y"),FUN = mean)
mean_months_t_min <- aggregate(data = out_dates, t_min ~ format.Date(Date, "%m%Y"), FUN = mean)
mean_months_t_min
mean_months <- mean_months %>% mutate(t_min = mean_months_t_min$t_min)
mean_months
# mean_months_sep <- subset(mean_months, format.Date(Date, "%m") == "09")
sapply(mean_months, class)
names(mean_months)[1] <- "Date"
colnames(mean_months)
# mean_months$Date <- as.Date(mean_months$Date, format = "%m%Y")

library(stringr)

# calculating mean of every month and plotting it over the years
# Sep <- grep("^09", mean_months$Date)
# Sep
mean_months_Sep <- subset(mean_months, startsWith(mean_months$Date, "09"))
mean_months_Sep$Date <- sub("^", "01", mean_months_sep$Date)
mean_months_Sep$Date<- as.Date(mean_months_sep$Date,format = "%m%d%Y")
mean_months_Sep

ggplot()+
  geom_line(data = mean_months_Sep, aes(mean_months_Sep$Date, mean_months_Sep$predicted_Hc,color = "Predicted Hc"))+
  geom_line(data = mean_months_Sep, aes(mean_months_Sep$Date, mean_months_Sep$t_min, color = "Tmin"))+
  xlab('Date')+
  ylab('Precidted Hc')+
  ggtitle('Mean Predicted Hc  Sepetember over the years')
ggsave("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/Sep_mean_plot.png")

# Same for Oct
mean_months_Oct <- subset(mean_months, startsWith(mean_months$Date, "10"))
mean_months_Oct$Date <- sub("^", "01", mean_months_Oct$Date)
mean_months_Oct$Date<- as.Date(mean_months_Oct$Date,format = "%m%d%Y")
mean_months_Oct

ggplot()+
  geom_boxplot(data = mean_months_Oct, aes( mean_months_Oct$predicted_Hc,color = "Predicted Hc"))+
  # geom_line(data = mean_months_Sep, aes(mean_months_Oct$Date, mean_months_Oct$t_min, color = "Tmin"))+
  xlab('Date')+
  ylab('Precidted Hc')+
  ggtitle('Mean Predicted Hc  Sepetember over the years')
ggsave("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/Sep_mean_plot.png")





out_dates$Date <- as.Date(out_dates$Date, format ="%m/%d/%Y")




ggplot()+
  # geom_density(data = end_Sep,aes(end_Sep$predicted_Hc,color = "Sep"))+
  geom_density(data = end_Oct,aes(end_Oct$predicted_Hc,color = "Oct"))+
  geom_density(data = end_Nov,aes(end_Nov$predicted_Hc,color = "Nov"))+
  geom_density(data = end_Dec,aes(end_Dec$predicted_Hc,color = "Dec"))+
  geom_density(data = end_Jan,aes(end_Jan$predicted_Hc,color = "Jan"))+
  xlab("Predicted Hc")
  labs(color = "Legend")

# meta_data <- meta_data %>% mutate(Date = paste0(meta_data$day,"/",meta_data$month,"/",meta_data$year))
tapply(output$Date,substr(output$Date,1,7), max)



consolidated <- read.csv("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/observed/consolidated_observed_historical.csv")
consolidated <- data.table(consolidated)
consolidated <- subset(consolidated, select = -Time_elapsed)
consolidated

consolidated_plot <- melt(consolidated, id = c("Location"))
consolidated_plot
ggplot()+
  geom_dotplot(data = consolidated_plot, x= consolidated_plot$Location, y =value)


sapply(meta_data$Date,class)
meta_data$Date <- as.Date(meta_data$Date)
abc<- format.Date(meta_data$Date, "%m")=="09"

months(as.Date(meta_data$Date[8280]))
meta_data$Date <- as.Date(meta_data$Date, format = "%m/%d/%Y")
if(format(meta_data$Date[8280], "%m")=="09" && format(meta_data$Date[8280], "%d")=="01")
{
  # if (format.Date(as.Date(meta_data$Date[8280]), "%d")== "01")
  # {
    print(meta_data$Date)
  # }
  
}

