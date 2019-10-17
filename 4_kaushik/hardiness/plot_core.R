options(digits=9)
options(digit=9)


plot_mean_hardiness<- function(out_dates_obs = out_dates_obs,out_dates_model =out_dates_model, path){
  
  # to plot end of month values for hardiness, converting the the date comun to Dat format for easier operations
  print(sapply(out_dates_obs$Date,class)) # check the datatype of the column
  # out_dates$Date
  out_dates_obs$Date <- as.Date(out_dates_obs$Date, format ="%m/%d/%Y") # worked, Hurray 
  out_dates_model$Date <- as.Date(out_dates_model$Date, format = "%m/%d/%Y")
  # out_dates$Date
  print(sapply(out_dates_obs$Date,class)) # check wether the conversion has worked
  
  print(sapply(out_dates_model$Date,class))
  
  ggplot()+
    
    
  
  # Calulating mean every month for the plots
  mean_months_obs <- aggregate(data = out_dates_obs, predicted_Hc ~ format.Date(Date, "%m%Y"),FUN = mean)
  mean_months_model <- aggregate(data = out_dates_model, predicted_Hc ~ format.Date(Date, "%m%Y"),FUN = mean)
  # mean_months_t_min <- aggregate(data = out_dates_obs, t_min ~ format.Date(Date, "%m%Y"), FUN = mean)
  # mean_months_t_min
  # mean_months <- mean_months %>% mutate(t_min = mean_months_t_min$t_min)
  print(head(mean_months_obs))
  # mean_months_sep <- subset(mean_months, format.Date(Date, "%m") == "09")
  print(sapply(mean_months_obs, class)) # checking the class
  print(sapply(mean_months_model, class))
  
  
  names(mean_months_obs)[1] <- "Date" # cahnging the name of the column according to the date 
  print(colnames(mean_months_obs))
  names(mean_months_model)[1] <- "Date"
  # mean_months$Date <- as.Date(mean_months$Date, format = "%m%Y")
  mean_months_obs <- data.table(mean_months_obs)
  mean_months_model <- data.table(mean_months_model)
  
  print(head(mean_months_obs))
  print(head(mean_months_model))
  
  mean_data_obs <- mean_months_obs
  mean_data_model <- mean_months_model
  
  
  # checking facets for graphs
  mean_data_obs_exp <- mean_data_obs
  mean_data_obs_exp$Date <- sub( '(?<=.{2})', '01', mean_data_obs_exp$Date, perl=TRUE )
  
  mean_data_obs_exp$Date <- as.Date(mean_data_obs_exp$Date, format ="%m%d%Y")
  mean_data_obs_exp$MONTH <- format(mean_data_obs_exp$Date, "%m")
  write.csv(mean_data_obs_exp, file ="C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/observed.csv")
  
  # checking facets for graphs
  mean_data_model_exp <- mean_data_model
  mean_data_model_exp$Date <- sub( '(?<=.{2})', '01', mean_data_model_exp$Date, perl=TRUE )
  
  mean_data_model_exp$Date <- as.Date(mean_data_model_exp$Date, format ="%m%d%Y")
  mean_data_model_exp$MONTH <- format(mean_data_model_exp$Date, "%m")
  write.csv(mean_data_model_exp, file = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/model.csv")
  
  overall <- ggplot()+
    geom_line(data = mean_data_obs_exp, aes(x= mean_data_obs_exp$Date ,y = mean_data_obs_exp$predicted_Hc, color = "observed"))+
    geom_line(data = mean_data_model_exp, aes(x= mean_data_model_exp$Date ,y = mean_data_model_exp$predicted_Hc, color = "model"))+
    facet_wrap(~ MONTH)
  ggsave(paste0(path,"exp", ".png"), plot = overall)
  # 
  # # creating data frames fro month od september
  # mean_obs_Sep <- plot_mean_hardiness_month(mean_data_obs,"09")
  # print(head(mean_obs_Sep))
  # mean_model_Sep <- plot_mean_hardiness_month(mean_months_model,"09")
  # print(head(mean_model_Sep))
  # 
  # plot_Sep <- hardiness_plot_month(mean_obs_Sep, mean_model_Sep)
  # 
  # 
  # # creating data for the month of october 
  # mean_obs_Oct <- plot_mean_hardiness_month(mean_data_obs,"10")
  # print(head(mean_obs_Oct))
  # mean_model_Oct <- plot_mean_hardiness_month(mean_months_model,"10")
  # print(head(mean_model_Oct))
  # 
  # plot_Oct <- hardiness_plot_month(mean_obs_Oct, mean_model_Oct)
  # 
  # 
  # # mon <- as.numeric("09")
  # # # mean_data = data.table(matrix(data, nrow=dim(data)[1],ncol = dim(data)[2]))
  # # # colnames(mean_data) <- c("Date","predicted_Hc","t_min")
  # # # mean_data <- get(data)
  # # # mean_data = data#.table(matrix(NA,nrow=dim(data)[1],ncol = dim(data)[2]))
  # # # output = data.table(matrix(NA, nrow=dim(data)[1], ncol=11))
  # # # print(head(mean_data))
  # # # print(dim(mean_data))
  # # # print(mean_data$Date)
  # # mean_data_obs <- subset(mean_data_obs, startsWith(mean_data_obs$Date, "09"))
  # # print(head(mean_data_obs))
  # # mean_data_obs$Date <- sub("^", "01", mean_data_obs$Date)# adding date , so as to make it easy for using as.Date()
  # # print(head(mean_data_obs))
  # # mean_data_obs$Date<- as.Date(mean_data_obs$Date,format = "%m%d%Y")
  # # print(head(mean_data_obs))
  # # 
  # # 
  # # # Code for model to do the same as observed
  # # mean_data_model <- subset(mean_data_model, startsWith(mean_data_model$Date, "09"))
  # # print(head(mean_data_model))
  # # mean_data_model$Date <- sub("^", "01", mean_data_model$Date)# adding date , so as to make it easy for using as.Date()
  # # print(head(mean_data_model))
  # # mean_data_model$Date<- as.Date(mean_data_model$Date,format = "%m%d%Y")
  # # print(head(mean_data_model))
  # 
  # 
  # # ggplot()+
  # #   geom_line(data = mean_obs_Sep, aes(mean_obs_Sep$Date, mean_obs_Sep$predicted_Hc,color = "obs Predicted Hc"))+
  # #   # geom_line(data = mean_data, aes(mean_data$Date, mean_data$t_min, color = "Tmin"))+
  # #   geom_line(data = mean_model_Sep, aes(mean_model_Sep$Date, mean_model_Sep$predicted_Hc,color = "model Predicted Hc"))+
  # #   xlab('Date')+
  # #   ylab('Precidted Hc')+
  # #   ggtitle("Mean Predicted Hc for the month")
  # ggsave(paste0(path,"mean",".png"),plot=plot_Sep)
  # 
  return(TRUE)
}

plot_mean_hardiness_month <- function(mean_data,month){
  print(mean_data)
  mon <- as.numeric(month)
  mean_data <- subset(mean_data, startsWith(mean_data[["Date"]], month))
  print(head(mean_data))
  mean_data[["Date"]] <- sub("^", "01", mean_data[["Date"]])# adding date , so as to make it easy for using as.Date()
  print(head(mean_data))
  mean_data[["Date"]]<- as.Date(mean_data[["Date"]],format = "%m%d%Y")
  print(head(mean_data))
  
  return(mean_data)
}

hardiness_plot_month <- function(plot_obs,plot_model){
  month_plot <-  ggplot()+
    geom_line(data = plot_obs, aes(plot_obs[["Date"]], plot_obs[["predicted_Hc"]],color = "obs Predicted Hc"))+
    # geom_line(data = mean_data, aes(mean_data$Date, mean_data$t_min, color = "Tmin"))+
    geom_line(data = plot_model, aes(plot_model[["Date"]], plot_model[["predicted_Hc"]],color = "model Predicted Hc"))+
    xlab('Date')+
    ylab('Precidted Hc')+
    ggtitle("Mean Predicted Hc for the month")
  # ggsave(paste0(path,"mean",".png"), width = 10, height = 8, dpi = 300)
  
  return(month_plot) 
}


plot_density_hardiness <- function(out_dates_obs = out_dates_obs,out_dates_model =out_dates_model, path){
  
  # to plot end of month values for hardiness, converting the the date comun to Dat format for easier operations
  print(sapply(out_dates_obs$Date,class)) # check the datatype of the column
  # out_dates$Date
  out_dates_obs$Date <- as.Date(out_dates_obs$Date, format ="%Y-%m-%d") # worked, Hurray 
  out_dates_model$Date <- as.Date(out_dates_model$Date, format ="%Y-%m-%d")
  # out_dates$Date
  # print(sapply(out_dates_obs$Date,class)) # check wether the conversion has worked
  print(head(out_dates_obs))
  print(head(out_dates_model))
  # print(sapply(out_dates_model$Date,class))
  
  
  
  out_dates_obs$MONTH <- format(out_dates_obs$Date, "%m")
  print(head(out_dates_obs))
  out_dates_model$MONTH <- format(out_dates_model$Date, "%m")
  print(head(out_dates_model))
  
  
  
  density_plot <- ggplot()+
    geom_density(data = out_dates_obs, aes(out_dates_obs$predicted_Hc, color ="observed", fill ="observed", alpha =0.7))+
    geom_density(data = out_dates_model, aes(out_dates_model$predicted_Hc, color ="model", fill ="model", alpha =0.7))+
    xlab("predicted HC")+
    facet_wrap(~ MONTH)
  
  ggsave(paste0(path, "density",".png"), plot = density_plot)
}

plot_trend_max <- function(out_dates_obs = out_dates_obs,out_dates_model =out_dates_model, path){
  
  print(sapply(out_dates_obs$Date,class)) # check the datatype of the column
  # out_dates$Date
  # the following code is to read the date from ouput file
  # the date in the output file i sof the format 2012-12-26
  # changing for both observed and model future
  out_dates_obs$Date <- as.Date(out_dates_obs$Date, format ="%Y-%m-%d")# worked, Hurray 
  out_dates_model$Date <- as.Date(out_dates_model$Date, format = "%Y-%m-%d")
  # out_dates$Date
  
  #  cross checking the changes made
  # check wether the conversion has worked
  # print(sapply(out_dates_obs$Date,class)) 
  print(head(out_dates_obs))
  
  # print(sapply(out_dates_model$Date,class))
  
  print(head(out_dates_model))
  
  
  # sapply(out_dates_obs, class)
  
  # HIGHEST OF EVERY MONTH
  
  # This section is for OBSERVED
  # seperating the year and month , so that i can  calculate the highest value for every month of every year
  out_dates_obs$year_month <- format(out_dates_obs$Date, "%Y-%m")
  
  # using tappy didnt work , beacuse it didnt create in table format
  # obs_highest<-tapply(out_dates_obs$predicted_Hc,out_dates_obs$year_month, max )
  
  # used data.table to find max of every month each year
  obs_highest <- data.table(out_dates_obs)
  obs_highest <- obs_highest[, max(predicted_Hc), by = year_month]
  obs_highest
  
  #changing column to help in naming graphs
  names(obs_highest)[2] <-"predicted_Hc"
  colnames(obs_highest) # cross checking the change
  # print(obs_highest)
  
  # sapply(obs_highest, class)
  # obs_highest$year_month <- as.Date(obs_highest$year_month, format = "%Y-%m")
  # obs_highest$YEAR <- format(obs_highest$year_month, "%Y")
  
  # Added date at the end so that its easy for considering the variable as Date and plot
  obs_highest$year_month <- paste0(obs_highest$year_month, "-01")
  obs_highest$year_month <- as.Date(obs_highest$year_month)
  obs_highest$MONTH <- format(obs_highest$year_month,"%m")
  
  
  # cross check the varibales of the changes made
  write.csv(obs_highest, file ="C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/highest_model.csv")
  
  
  # This section is for MODEL FUTURE
  # seperating the year and month , so that i can  calculate the highest value for every month of every year
  out_dates_model$year_month <- format(out_dates_model$Date, "%Y-%m")
  
  print(head(out_dates_model))
  
  # using tappy didnt work , beacuse it didnt create in table format
  # obs_highest<-tapply(out_dates_obs$predicted_Hc,out_dates_obs$year_month, max )
  
  # used data.table to find max of every month each year
  model_highest <- data.table(out_dates_model)
  model_highest <- model_highest[, max(predicted_Hc), by = year_month]
  # model_highest
  
  #changing column to help in naming graphs
  names(model_highest)[2] <-"predicted_Hc"
  colnames(model_highest) # cross checking the change
  # print(obs_highest)
  
  # sapply(obs_highest, class)
  # obs_highest$year_month <- as.Date(obs_highest$year_month, format = "%Y-%m")
  # obs_highest$YEAR <- format(obs_highest$year_month, "%Y")
  
  # Added date at the end so that its easy for considering the variable as Date and plot
  model_highest$year_month <- paste0(model_highest$year_month, "-01")
  print(head(model_highest))
  model_highest$year_month <- as.Date(model_highest$year_month)
  model_highest$MONTH <- format(model_highest$year_month,"%m")
  
  
  # cross check the varibales of the changes made
  write.csv(model_highest, file ="C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/highest_model.csv")
  
  highest_merge <- rbind(obs_highest, model_highest)
  
  trend_plot  <- ggplot(data=highest_merge, aes(x=highest_merge$year_month, y =highest_merge$predicted_Hc, color ="model" ))+
    geom_line(data=obs_highest, aes(x=obs_highest$year_month, y =obs_highest$predicted_Hc, color ="obs" ))+
    geom_line(data=model_highest, aes(x=model_highest$year_month, y =model_highest$predicted_Hc, color ="model" ))+
    facet_wrap(~MONTH)+
    stat_smooth(aes(color ="trend"))+
    xlab('YEAR')+
    ylab('Predicted HC')
  ggsave(paste0(path, "trend",".png"), plot = trend_plot)
  
}

plot_obs_trends_by_location <- function(latlong, name)
{
  print(latlong)
  # print(name)
  plot_path <- "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/Plots/"
  
  out_dates_loc_obs <- read.csv(file = paste0("C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/Output_data/observed/output_observed_historical_",latlong,".csv"), stringsAsFactors = FALSE)
  # out_dates_loc_obs
  
  out_dates_loc_obs$Date <- as.Date(out_dates_loc_obs$Date, format = "%Y-%m-%d")
  # print(out_dates_loc_obs)
  
  out_dates_loc_obs <- subset(out_dates_loc_obs, out_dates_loc_obs$hardiness_year %in% (1979:2014)) 
  setDT(out_dates_loc_obs)[, counter := seq_len(.N), by=rleid(hardiness_year)]
  
  # write.csv(out_dates_loc_obs, file = paste0(plot_path,"checking.csv"))
  
  ggplot()+
    geom_line(data = out_dates_loc_obs, aes(x = out_dates_loc_obs$counter, y = out_dates_loc_obs$predicted_Hc, color = "predicted HC"))+
    geom_line(data = out_dates_loc_obs, aes(x = out_dates_loc_obs$counter, y = out_dates_loc_obs$t_min, color = "tmin"))+
    geom_line(data = out_dates_loc_obs, aes(x = out_dates_loc_obs$counter, y = out_dates_loc_obs$t_max, color = "tmax"))+
    facet_wrap(~ hardiness_year)
  ggsave(paste0(plot_path,name,".png"), width = 15, height = 10)
  
}

