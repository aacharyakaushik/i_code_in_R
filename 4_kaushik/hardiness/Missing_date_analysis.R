
library(dplyr)

#######################################
# Analysis of Missing dates
#######################################

missing_dates <- read.csv("C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/Output_data/AG_weather/missing_dates.csv")



head(missing_dates)
dim(missing_dates)

names(missing_dates)



AG_locations <- unique(missing_dates$Station_name)
AG_locations

missing_dates_analysis <- data.frame()

for(place in AG_locations){
  
  missing_data <- subset(missing_dates, missing_dates$Station_name == place)
  
  missing_data <-  missing_data %>% 
    select(Date, Station_name, latitude, longitude)
  
  missing_data$count <- 1
  
  missing_data$Date <- as.POSIXct(missing_data$Date)
  
  mo <- strftime(missing_data$Date, "%m")
  yr <- strftime(missing_data$Date, "%Y")
  
  
  
  missing_data.agg <- aggregate(count ~ mo + yr, missing_data, FUN = sum)
  missing_data.agg$date <- as.POSIXct(paste(missing_data.agg$yr, 
                                            missing_data.agg$mo, "01", sep = "-"))
  
  missing_data.agg
  head(missing_data)
  
  dim(missing_data.agg)
  missing_data.agg$Station_name <- place  
  
  missing_frame <- data.frame(missing_data.agg)
  missing_dates_analysis <- rbind(missing_dates_analysis, missing_frame)
  
}

head(missing_dates_analysis)
dim(missing_dates_analysis)

output_dir <- "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/Output_data/AG_weather/"

write.csv(missing_dates_analysis,paste0(output_dir,"missing_analysis.csv"))


############################################
# Trying to plot bar graph
############################################




miss <- subset(missing_dates_analysis, missing_dates_analysis$yr > 2008 & 
                 missing_dates_analysis$yr < 2016)
dim(miss)
unique(miss$Station_name)

head(miss)

miss_3 <- subset(miss, miss$count > 3)
dim(miss)
unique(miss$Station_name)

miss_3 <- miss_3 %>% select(Station_name,mo,yr, count)

summary1<-miss_3 %>% group_by(Station_name, yr) %>% summarise(cnt = n())
View(summary1)
summary2<-miss_3 %>% count(Station_name, yr)

wideformsummary<-dcast(summary1, Station_name ~ yr)
View(wideformsummary)
View(summary2)
miss_3$ count_month <- 1


reshape(summary1, idvar = "Station_name", timevar = yr, direction = "wide")


head(miss_3)

# miss_3 <- data.frame(miss_3, value = TRUE)

names(miss_3)
View(miss_3)
miss_3$date <- as.Date(miss_3$date, format = "%Y-%m-%d")
miss_3[, .N, by = year(date)] 


  locations <- unique(missing_dates_analysis$Station_name)[10:20]
locations

missing_dates_sub <- subset(missing_dates_analysis, 
                            missing_dates_analysis$Station_name %in% locations)
dim(missing_dates_sub)

missing_plot <- ggplot()+
  geom_bar(data = missing_dates_sub, aes(missing_dates_sub$Station_name,
                                         missing_dates_sub$count, 
                                              fill = yr), stat ="identity")+
  xlab('Station name')+
  ylab('# missing days')+
  ggtitle('Missing dates analysis by Station')

analysis_plot <- missing_plot + theme(plot.title = element_text(size=16, face="bold"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.spacing=unit(.25, "cm"),
                       legend.title = element_text(face="plain", size=16),
                       legend.text = element_text(size=14),
                       legend.position = "bottom",
                       legend.key.size = unit(.65, "cm"),
                       strip.text = element_text(size=16, face="bold", color="black"),
                       axis.text = element_text(face="bold", size=14, color="black"),
                       axis.ticks = element_line(color = "black", size = .2),
                       axis.title.x = element_text(face="bold", size=16, margin=margin(t=10, r=0, b=0, l=0), color="black"),
                       axis.title.y = element_text(face="bold", size=16, margin=margin(t=0, r=10, b=0, l=0), color="black"))

analysis_plot

ggsave(plot = analysis_plot, paste0(plot_location,"Missing_days.PNG"), 
       height = 15, width = 20)

# missing_dates <- subset(missing_dates, missing_dates$Station_name == "100Circles" )
# 
# missing_dates <-  missing_dates %>% select(Date, Station_name, latitude, longitude)
# 
# missing_dates$count <- 1
# 
# 
# missing_dates$Date <- as.POSIXct(missing_dates$Date)
# 
# 
# mo <- strftime(missing_dates$Date, "%m")
# yr <- strftime(missing_dates$Date, "%Y")
# 
# 
# 
# missing_dates.agg <- aggregate(count ~ mo + yr, missing_dates, FUN = sum)
# missing_dates.agg$date <- as.POSIXct(paste(missing_dates.agg$yr, 
#                                            missing_dates.agg$mo, "01", sep = "-"))
# 
# missing_dates.agg
# head(missing_dates)
