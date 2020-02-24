 
library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)




####################################
# read the lat long comaprer file
####################################

input_dir <- "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/"

grid_AG_compare = readRDS(paste0(input_dir,"Output_data/AG_weather/grid_AG_compare.rds"))

dim(grid_AG_compare)


####### checking for zeros in file and analysis #######
zero_compare <- subset(grid_AG_compare, grid_AG_compare$AG_lat == 0)
length(zero_compare$Station_ID)


# seperating the 9 location with no info on lat and long
grid_AG_compare <- subset(grid_AG_compare, grid_AG_compare$AG_lat != 0)
dim(grid_AG_compare)

# locations seperated for into vector for the for loop
grid_AG_5 <- grid_AG_compare$Station_ID[1:5]
grid_AG_5

# grid_AG

for (stations in grid_AG_5){
  
  input_AG <- readRDS("C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/Output_data/AG_weather/AGweather.rds")
  
  # seperating one AGweather location 
  input_AG <- subset(input_AG, input_AG$Station_ID == 100140)#place)
  dim(input_AG) 
  head(input_AG)
  
  # converting Date column into date format
  input_AG$Date <- as.Date(input_AG$Date, format = "%Y-%m-%d")
  sapply(input_AG, class)
  
  # selecting start date and end date for the grid data
  start_date <- head(input_AG$Date, 1)
  start_date
  end_date <- tail(input_AG$Date, 1)
  end_date
  
  
  # location of grid data
  grid_location <- "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/Output_data/observed/"
  
  # nearest lat long from compare table 
  lat <- grid_AG_compare$grid_lat[1]
  lat
  long <- grid_AG_compare$grid_long[1]
  long
  
  # Data concerning the nearest lat long location
  input_grid <- read.csv(paste0(grid_location,"output_observed_historical_data_",
                                lat,"_", long,".csv"))
  
  dim(input_grid)
  head(input_grid) 
  
  # converting date format of grid data
  input_grid$Date <- as.Date(input_grid$Date, format = "%Y-%m-%d")
  sapply(input_grid, class)
  
  # check whether AG end date is lesser or greater than grid data
  if (end_date < tail(input_grid$Date,1)){
    print("you are good")
    
    # Date of AGweather is lesser
    # Reduce the grid data to end_date
    
  }else {
    print("you are fucked")
    
    # Date of grid data lesser
    # therefore change the end to that of grid data
    # reduce the Agweather data
    end_date <- tail(input_grid$Date, 1)
    end_date  
    
    # reducing the Ag weather data
    input_AG <- input_AG[input_AG$Date <= end_date,]
    head(input_AG$Date,1)
    tail(input_AG$Date,1)
    
    # Grid data with the dates
    input_grid <- input_grid[input_grid$Date >= start_date & input_grid$Date <= end_date,]
    head(input_grid$Date,1)
    tail(input_grid$Date,1)
    
  }
  
  
  
  #######################################################
  # Custom Theme
  #######################################################
  
  custom_theme <- function () {
    theme_gray() %+replace%
      theme(plot.title = element_text(size=16, face="bold"),
            # panel.background = element_blank(),
            # panel.border = element_blank(),
            panel.spacing=unit(.25, "cm"),
            legend.title = element_text(face="plain", size=36),
            legend.text = element_text(size=10),
            legend.position = "bottom",
            legend.key.size = unit(.65, "cm"),
            strip.text = element_text(size= 24, face="bold", color="black"),
            axis.text = element_text(face="bold", size=24, color="black"),
            axis.ticks = element_line(color = "black", size = .2),
            axis.title.x = element_text(face="bold", size=36, margin=margin(t=10, r=0, b=0, l=0), color="black"),
            axis.title.y = element_text(face="bold", size=36, margin=margin(t=0, r=10, b=0, l=0), color="black",angle = 90))
    }
  
  
  dim(input_AG)
  dim(input_grid)

  
  ##############################################################
  # Plots for comparision between two data
  ##############################################################
  
  # Plot Location
  plot_location <- "C:/Users/Kaushik Acharya/Documents/R Scripts/i_code_in_R/4_kaushik/hardiness/Output_data/Plots/Comparison/"
  
  
  
  # comparison between the two dataframes
  dim(input_AG)
  dim(input_grid)
  
  # merging the table for difference calculation
  Merge_diff <- merge(input_grid,input_AG, by = "Date")
  dim(Merge_diff)
  names(Merge_diff)
  sapply(Merge_diff, class)
  
  # Calculating differences to plot later
  Merge_diff$max <- Merge_diff$t_max.x - Merge_diff$t_max.y
  Merge_diff$min <- Merge_diff$t_min.x - Merge_diff$t_min.y
  Merge_diff$mean <- Merge_diff$t_mean.x - Merge_diff$t_mean.y
  Merge_diff$Hc <- Merge_diff$predicted_Hc.x - Merge_diff$predicted_Hc.y
  
  # check if there are CDI values
  sum(Merge_diff$CDI.x)
  sum(Merge_diff$CDI.y)
  
  # plot for comparison of Tmax
  Max_temp_compare <- ggplot()+
    geom_line(data = input_AG, aes(x = input_AG$Date, y = input_AG$t_max,
                                   color = "AG weather"))+
    geom_line(data = input_grid, aes(x = input_grid$Date, y = input_grid$t_max,
                                     color = "grid data"))+
    geom_col(data = Merge_diff, aes(x = Merge_diff$Date, y = Merge_diff$max,
                                    color = "diff"))+
    
    xlab('Date')+
    ylab('Temperature')+
    ggtitle('Tmax Comparision')
  
  Max_temp_compare
  
  # save the plot
  ggsave(plot = Max_temp_compare, paste0(plot_location,"Tmax_comparison.PNG"), 
         height = 10, width = 20)
  
  # plot for comparison of Tmin
  Min_temp_compare <-ggplot()+
    geom_line(data = input_AG, aes(x = input_AG$Date, y = input_AG$t_min,
                                   color = "AG weather"))+
    geom_line(data = input_grid, aes(x = input_grid$Date, y = input_grid$t_min,
                                     color = "grid data"))+
    geom_col(data = Merge_diff, aes(x = Merge_diff$Date, y = Merge_diff$min,
                                    color = "diff"))+
    xlab('Date')+
    ylab('Temperature')+
    ggtitle('Tmin Comparision')
  
  Min_temp_compare
  
  # save the plot
  ggsave(plot = Min_temp_compare, paste0(plot_location,"Tmin_comparison.PNG"), 
         height = 10, width = 20)
  
  # plot for comaprison of Tmean
  Mean_temp_compare <- ggplot()+
    geom_line(data = input_AG, aes(x = input_AG$Date, y = input_AG$t_mean,
                                   color = "AG weather"))+
    geom_line(data = input_grid, aes(x = input_grid$Date, y = input_grid$t_mean,
                                     color = "grid data"))+
    geom_col(data = Merge_diff, aes(x = Merge_diff$Date, y = Merge_diff$mean,
                                    color = "diff"))+
    xlab('Date')+
    ylab('Temperature')+
    ggtitle('Tmean Comparision')
  
  Mean_temp_compare
  
  # save the plot
  ggsave(plot = Mean_temp_compare, paste0(plot_location,"Tmean_comparison.PNG"), 
         height = 10, width = 20)
  
  # plot for comparison of Preddicted_Hc
  Hc_temp_compare <-  ggplot()+
    geom_line(data = input_AG, aes(x = input_AG$Date, y = input_AG$predicted_Hc,
                                   color = "AG weather"))+
    geom_line(data = input_grid, aes(x = input_grid$Date, y = input_grid$predicted_Hc,
                                     color = "grid data"))+
    geom_col(data = Merge_diff, aes(x = Merge_diff$Date, y = Merge_diff$Hc,
                                    color = "diff"))+
    
    facet_wrap(~ input_AG$hardiness_year)+
    xlab('Date')+
    ylab('Temperature')+
    ggtitle('Hardiness Comparision')
  
  Hc_temp_compare
  
  # save the plot
  ggsave(plot = Hc_temp_compare, paste0(plot_location,"Hc_comparison.PNG"), 
         height = 10, width = 20)
  
  
  ############################################################
  # For the purpose of facet wrap
  ##########################################################
  Merge_diff$Date <- as.Date(Merge_diff$Date, format = "%Y-%m-%d")
  
  head(Merge_diff)
  
  Merge_diff <- subset(Merge_diff, Merge_diff$hardiness_year.x != 0)
  
  # creating a column called counter to faciliatate facet plotting
  # as counter is common for all points
  setDT(Merge_diff)[, counter := seq_len(.N), by=rleid(hardiness_year.x)]
  
  dim(Merge_diff)
  length(Merge_diff$CDI.y)
  
  # subset of CDI counts greater than 0
  
  # preparing table for CDI count for AG weather
  Merge_count_AG <- subset(Merge_diff, Merge_diff$CDI.y > 0)
  Merge_count_AG$CDI.y <- Merge_count_AG$predicted_Hc.y
  Merge_count_AG <- Merge_count_AG %>% select (counter, CDI.y,hardiness_year.x)
  Merge_count_AG
  
  
  # preparing table for CDI count for grid weather
  Merge_count_grid <- subset(Merge_diff, Merge_diff$CDI.x > 0)
  Merge_count_grid$CDI.x <- Merge_count_grid$predicted_Hc.x
  Merge_count_grid <- Merge_count_grid %>% select (counter, CDI.x,hardiness_year.x)
  Merge_count_grid
  
  # facet plot for hardiness
  Hc_temp_facet <-  ggplot()+
    geom_line(data = Merge_diff, aes(x = Merge_diff$counter, y = Merge_diff$predicted_Hc.x,
                                   color = "AG weather"))+
    geom_line(data = Merge_diff, aes(x = Merge_diff$counter, y = Merge_diff$predicted_Hc.y,
                                     color = "grid data"))+
    geom_col(data = Merge_diff, aes(x = Merge_diff$counter, y = Merge_diff$Hc,
                                    color = "diff"))+
    geom_point(data = Merge_count_AG, aes(x = Merge_count_AG$counter, y = Merge_count_AG$CDI.y),
                                          shape = 24, size = 3, fill = "yellow")+
    geom_point(data = Merge_count_grid, aes(x = Merge_count_grid$counter, y = Merge_count_grid$CDI.x),
                                          shape = 21, size = 3, fill = "blue")+
    facet_wrap(~ hardiness_year.x)+
    xlab('Date')+
    ylab('Temperature')+
    ggtitle('Hardiness Comparision')
  
    Hc_facet <- Hc_temp_facet +  theme(plot.title = element_text(size=16, face="bold"),
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
  
  # save the plot
  ggsave(plot = Hc_facet, paste0(plot_location,"Hc_comparison_facet.PNG"), 
         height = 10, width = 10)
  
  
  
  
  
  
  # facet plot for Tmax 
  Max_temp_facet <-  ggplot()+
    geom_line(data = Merge_diff, aes(x = Merge_diff$counter, y = Merge_diff$t_max.x,
                                     color = "AG weather"))+
    geom_line(data = Merge_diff, aes(x = Merge_diff$counter, y = Merge_diff$t_max.y,
                                     color = "grid data"))+
    geom_col(data = Merge_diff, aes(x = Merge_diff$counter, y = Merge_diff$max,
                                    color = "diff"))+
    facet_wrap(~ hardiness_year.x)+
    xlab('Date')+
    ylab('Temperature')+
    ggtitle('Tmax Comparision')
  
  Max_temp_facet
  
  # save the plot
  ggsave(plot = Max_temp_facet, paste0(plot_location,"Max_comparison_facet.PNG"), 
         height = 10, width = 10)
  
  
  # facet plot for Tmin
  Min_temp_facet <-  ggplot()+
    geom_line(data = Merge_diff, aes(x = Merge_diff$counter, y = Merge_diff$t_min.x,
                                     color = "AG weather"))+
    geom_line(data = Merge_diff, aes(x = Merge_diff$counter, y = Merge_diff$t_min.y,
                                     color = "grid data"))+
    geom_col(data = Merge_diff, aes(x = Merge_diff$counter, y = Merge_diff$min,
                                    color = "diff"))+
    facet_wrap(~ hardiness_year.x)+
    xlab('Date')+
    ylab('Temperature')+
    ggtitle('Tmin Comparision')
  
  Min_temp_facet
  
  # save the plot
  ggsave(plot = Min_temp_facet, paste0(plot_location,"Min_comparison_facet.PNG"), 
         height = 10, width = 10)
  
  # facet plot Tmean
  Mean_temp_facet <-  ggplot()+
    geom_line(data = Merge_diff, aes(x = Merge_diff$counter, y = Merge_diff$t_mean.x,
                                     color = "AG weather"))+
    geom_line(data = Merge_diff, aes(x = Merge_diff$counter, y = Merge_diff$t_mean.y,
                                     color = "grid data"))+
    geom_col(data = Merge_diff, aes(x = Merge_diff$counter, y = Merge_diff$mean,
                                    color = "diff"))+
    facet_wrap(~ hardiness_year.x)+
    xlab('Date')+
    ylab('Temperature')+
    ggtitle('Tmean Comparision')
  
  Mean_temp_facet
  
  # save the plot
  ggsave(plot = Mean_temp_facet, paste0(plot_location,"Mean_comparison_facet.PNG"), 
         height = 10, width = 10)
  
  
  # Bar plots woth difference
  Tmax_diff<-ggplot()+
    geom_bar(data = Merge_diff, aes(x = Merge_diff$Date, y = Merge_diff$max, 
                                    color = "Tmax" ), stat = "identity")+
    # geom_bar(data = Merge_diff, aes(x = Merge_diff$counter, y = Merge_diff$min,
    #                                 color = "Tmin"), stat = "identity", alpha = 0.3, 
    #                                       position = "dodge")+
    xlab('Date')+
    ylab('Temperature')+
    ggtitle('Tmax difference')#+
    # facet_wrap(~ hardiness_year.x)
  
  Tmax_diff
  
  Tmin_diff <- ggplot()+
    geom_bar(data = Merge_diff, aes(x = Merge_diff$Date, y = Merge_diff$min,
                                               color = "Tmin"), stat = "identity")+
    xlab('Date')+
    ylab('Temperature')+
    ggtitle('Tmin difference')#+
    # facet_wrap(~ hardiness_year.x)
  
  Tmin_diff
  
  library(gridExtra)
  grid_plot <- grid.arrange(Tmax_diff,Tmin_diff,ncol = 1)
  
  ggplot(data = Merge_diff)+
    grid_plot+ facet_wrap(hardiness_year.x)
  
  
  # trying per year scheme
  total_years <- unique(Merge_diff$year.x)
  total_years
  
  
  ###########################################################
  # Facet Data with difference comparision
  ##########################################################
  
  names(Merge_diff)
  comp_Merge <- Merge_diff %>% select(Date, max, min, mean, hardiness_year.x, counter)
  
  comp_Merge_melt <- melt(comp_Merge, id = c("Date","hardiness_year.x","counter"))
  head(comp_Merge_melt)
  
  Multi_grid_compare<- ggplot()+
    geom_bar(data = comp_Merge_melt, aes(x = comp_Merge_melt$counter, 
                                         y = comp_Merge_melt$value, fill = factor(variable))
                                        , stat = "identity")+
    facet_grid( ~ hardiness_year.x ~variable, scales = "free")+
    xlab('Date')+
    ylab('Temperature')+
    ggtitle('Comparision')
  
  max_mean_mean_facet <- Multi_grid_compare + theme(plot.title = element_text(size=16, face="bold"),
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
  
  ggsave(plot = max_mean_mean_facet, paste0(plot_location,"Temperature_difference_facet_long.PNG"), 
           height = 40 , width = 20)
    
  
  ################################################
  # Stacked area plot
  ###############################################
  
  comp_area <- ggplot()+
    geom_area(data = comp_Merge_melt, aes(counter, value, fill = factor(variable)),
                                          position = 'stack')+
    facet_wrap(~ hardiness_year.x,scales = "free")+
    scale_fill_manual(values = c("max" = "red", "min" = "blue", "mean" = "yellow"))+
    xlab('Date')+
    ylab('Temperature')+
    ggtitle('Area plot for temperaure difference')
  
  comp_area
  
  
  comp_area <- comp_area + custom_theme()
  
  comp_area <- comp_area + theme(plot.title = element_text(size=16, face="bold"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.spacing=unit(.25, "cm"),
                    legend.title = element_text(face="plain", size=16),
                    legend.text = element_text(size=14),
                    legend.position = "bottom",
                    legend.key.size = unit(.65, "cm"),
                    strip.text = element_text(size=16, face="bold", color="black"),
                    # axis.text = element_text(face="bold", size=14, color="black"),
                    axis.ticks = element_line(color = "black", size = .2),
                    axis.title.x = element_text(face="bold", size=16, margin=margin(t=10, r=0, b=0, l=0), color="black"),
                    axis.title.y = element_text(face="bold", size=16, margin=margin(t=0, r=10, b=0, l=0), color="black"))
  
  
  ggsave(plot = comp_area, paste0(plot_location,"Area_plot.PNG"), 
         height = 15 , width = 20)
  
  
  #######################################################
  # Predicted HC with different colors and CDI only
  #######################################################
  
  names(Merge_diff)
  head(Merge_diff)
  just_Hc_diff <- Merge_diff %>% select (Date, year.x, Hc, counter, hardiness_year.x)
  dim(just_Hc_diff)
  head(just_Hc_diff, 50)
  
  # adding a column to check for postives and negatives
  just_Hc_diff$sign <- ifelse(just_Hc_diff$Hc >= 0, "positive", "negative")
  
  # checking for critical events and plotting for 
  
  CDI_grid <- Merge_diff %>% select (Date, counter, CDI.x, hardiness_year.x)
  CDI_grid <- subset(CDI_grid, CDI_grid$CDI.x > 0)
  CDI_grid
  
  CDI_AG <- Merge_diff %>% select (Date, counter, CDI.y, hardiness_year.x)
  CDI_AG <- subset(CDI_AG, CDI_AG$CDI.y > 0)
  CDI_AG
  
  head(just_Hc_diff$Hc,50)
  
  # plot for difference of Hc with critical days
  just_hc <- ggplot()+
    geom_bar(data = just_Hc_diff, aes(x = just_Hc_diff$counter, y = just_Hc_diff$Hc,
                                      fill = sign), stat = "identity")+
    scale_fill_manual(values = c("positive" = "#56B4E9", "negative" = "#E69F00"))+
    geom_point(data = CDI_AG, aes(x = CDI_AG$counter, y = CDI_AG$CDI.y),
               shape = 24, size = 3, fill = "yellow")+
    geom_point(data = CDI_grid, aes(x = CDI_grid$counter, y = CDI_grid$CDI.x),
               shape = 21, size = 3, fill = "blue")+
    facet_wrap(~ hardiness_year.x,scales = "free")+
    xlab('Days')+
    ylab('Temperature')+
    ggtitle('Predicted HC diferrence with critical days')
   
  just_hc <- just_hc + custom_theme()
  
  just_hc
  
  ggsave(plot = just_hc, paste0(plot_location,"HC_difference.png"), 
          dpi = "print", scale = 10)
   
   # This is manual work not needed good that you wrote, but there was a better way
   # for (one_year in total_years){
   #   one_year_data <- subset(Merge_diff, Merge_diff$year.x == one_year)
   #   print(dim(one_year_data))
   #   
   #   Tmax_one <- ggplot()+
   #     geom_bar(data = one_year_data, aes(x = one_year_data$counter, y = one_year_data$max,
   #                                     color = "Tmax"), stat = "identity")+
   #     xlab('Date')+
   #     ylim(-15,15)+
   #     ylab('Temperature')
   #     # ggtitle('Tmin difference')
   #   
   #   Tmin_one <- ggplot()+
   #     geom_bar(data = one_year_data, aes(x = one_year_data$counter, y = one_year_data$min,
   #                                        color = "Tmin"), stat = "identity")+
   #     xlab('Date')+
   #     ylim(-15,15)+
   #     ylab('Temperature')
   #     # ggtitle('Tmin difference')
   #   
   #   Tmean_one <- ggplot()+
   #     geom_bar(data = one_year_data, aes(x = one_year_data$counter, y = one_year_data$mean,
   #                                        color = "Tmean"), stat = "identity")+
   #     xlab('Date')+
   #     ylim(-15,15)+
   #     ylab('Temperature')
   #     
   #     grid_one <- grid.arrange(Tmax_one,Tmin_one,Tmean_one, ncol = 1)
   #     # ggsave(output_dir)
   #     
   #     ggsave (grid_one, file = paste0(plot_location, one_year, ".png"),
   #             height = 10, width = 8)
   #    
   #   
   #     
   # }
   # grid_one
   

  }

