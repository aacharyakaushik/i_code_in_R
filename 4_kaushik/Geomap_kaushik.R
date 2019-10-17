
# ggarrange can be used to arrange plots in a panel


geo_map_of_diffs <- function(dt, col_col, minn, maxx, ttl, subttl){
  color_limit <- max(abs(minn), abs(maxx))
  x <- sapply(dt$location, 
              function(x) strsplit(x, "_")[[1]], 
              USE.NAMES=FALSE)
  lat <- as.numeric(x[1, ]); long <- as.numeric(x[2, ])
  dt$lat <- lat; dt$long <- long;
  
  states <- map_data("state")
  states_cluster <- subset(states, 
                           region %in% c("washington"))
  WA_counties <- map_data("county", "washington")
  
  dt %>%
  ggplot() +
  geom_polygon(data = states_cluster, 
               aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  geom_polygon(data=WA_counties, 
               aes(x=long, y=lat, group = group), 
               fill = NA, colour = "grey60", size=.3) + 
  geom_point(aes_string(x = "long", y = "lat", color = col_col), 
             alpha = 1, size=.3) +
  guides(fill = guide_colourbar(barwidth = .1, barheight = 20))+
  # scale_color_viridis_c(option = "plasma", 
  #                       name = "storm", direction = -1,
  #                       limits = c(min, max),
  #                       # begin = 0.5, end = 1,
  #                       breaks = pretty_breaks(n = 3)) +
  
  # scale_color_gradient2(breaks = c((as.integer(minn*0.6)), 
  #                                   0,
  #                                   (as.integer(maxx*0.9)), 
  #                                   (as.integer((maxx)*0.9))),
                        
  #                       labels = c((as.integer(minn*0.6)), 
  #                                  0, 
  #                                  (as.integer(maxx*0.9)),
  #                                  (as.integer((maxx)*0.9))),

  #                       low = "red", high = "blue", mid = "white",
  #                       space="Lab"
  #                       ) +

  # Look at this. This may work, if you spend time on it.
  # problem is that we have to create one of these
  # manually for each map? We do not want to do this.
  # There are too many maps, one function per map is just ...
  #
  # scale_fill_gradientn(name="CPU Utilization",
  #       colours=c("darkgreen","green","red","darkred","red","green","darkgreen"),
  #       values=c(0, 0.19, 0.2, 0.5, 0.8, 0.81, 1),
  #       limits=c(-color_limit, color_limit),
  #       breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100))

  scale_color_gradient2(midpoint = 0, mid = "white", 
                        high = muted("blue"), low = muted("red"), 
                        guide = "colourbar", space = "Lab",
                        limit = c(-color_limit, color_limit)) + 
  # scale_color_continuous(breaks = c(as.integer(minn+1), 0, as.integer(maxx-1)),
  #                        labels = c(as.integer(minn+1), 0, as.integer(maxx-1)),
  #                        low = "red", high = "blue") + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 8, face="plain"),
        legend.title = element_blank(),
        # legend.justification = c(.93, .9),
        # legend.position = c(.93, .9),
        legend.position = "top",
        strip.text = element_text(size=14, face="bold"))+
  ggtitle(ttl, subtitle=subttl)
}



observed_hist_map <- function(min, max, month_col) {
  
  stats_comp %>%
    filter(model == "observed") %>%
    ggplot() +
    geom_polygon(data = states_cluster, 
                 aes(x = long, y = lat, group = group),
                 fill = "grey", color = "black") +
    
    geom_point(aes_string(x = "long", y = "lat",
                          color = month_col), alpha = 0.4) +
    scale_color_viridis_c(option = "plasma", 
                          name = "Median", direction = -1,
                          limits = c(min, max),
                          breaks = pretty_breaks(n = 4)) +

    coord_fixed(xlim = c(-124.5, -111.4),  ylim = c(41, 50.5), ratio = 1.3) +
    facet_wrap(~ time_period, nrow = 1) +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.y = element_blank(), 
          axis.ticks.x = element_blank(),
          strip.text = element_text(size=12, face="bold")) +
    ggtitle("Observed historical")
}


plot_dens <- function(data, month_name){
    color_ord = c("grey47", "dodgerblue", "olivedrab4", "khaki2")
    iof_breaks = c(-Inf, -2, 4, 6, 8, 13, 16, Inf)
    x_breaks = c(-30, -20, -10, -2, 0, 4, 6, 8, 10, 13, 16, 20, 30, 40, 50)
    the_theme <- theme(plot.margin = unit(c(t=0.4, r=0.3, b=.3, l=0.1), "cm"),
                       panel.border = element_rect(fill=NA, size=.3),
                       panel.grid.major = element_line(size = 0.05),
                       panel.grid.minor = element_blank(),
                       panel.spacing=unit(.3, "cm"),
                       legend.position="bottom", 
                       legend.title = element_blank(),
                       legend.key.size = unit(1.3, "line"),
                       legend.margin=margin(t= -0.1, r=0, b=0, l=0, unit='cm'),
                       legend.spacing.x = unit(.08, 'cm'),
                       strip.text.x = element_text(face="bold", size=30),
                       strip.text.y = element_text(face="bold", size=30),
                       axis.ticks = element_line(size=.2, color="black"),
                       legend.text=element_text(size=24),
                       plot.title = element_text(size=36, face="bold"),
                       axis.title.x = element_text(size=28, face = "bold", margin = margin(t=8, r=0, b=0, l=0)),
                       axis.title.y = element_text(size=28, face = "bold", margin = margin(t=0, r=8, b=0, l=0)),
                       axis.text.x = element_text(size =16, face = "plain", color="black", angle=-90),
                       axis.text.y = element_text(size =22, face = "bold", color="black")
                       )
    
    if (month == "sept_thru_dec_modeled"){
      gtitle = paste0("The density of hourly temp. from Sept. to Dec. 31 ")
     } else {
      gtitle = paste0("The density of hourly temp. from Sept. to Jan. 31 ")
    }
    obs_plot = ggplot(data, aes(x=Temp, fill=factor(time_period))) + 
               geom_density(alpha=.5, size=.1) + 
               geom_vline(xintercept = iof_breaks, 
                          linetype = "solid", color = "red", size = 0.2) +
               facet_grid( ~ city) +
               xlab("hourly temp.") + 
               ggtitle(label = gtitle) +
               scale_fill_manual(values=color_ord,
                      name="Time\nPeriod", 
                      labels=c("Historical", "2025-2050", "2051-2075", "2076-2099")) + 
               scale_color_manual(values=color_ord,
                       name="Time\nPeriod", 
                       limits = color_ord,
                       labels=c("Historical", "2025-2050", "2051-2075", "2076-2099")) + 
               scale_x_continuous(name="hourly temp.", breaks=x_breaks, limits=c(-30, 50)) + 
               the_theme
    return(obs_plot)
}

