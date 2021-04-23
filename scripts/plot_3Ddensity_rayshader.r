#works only on mac
library(rayshader)

#list of centroid tracking .rds files in data folder
files_to_process <- "/Users/fpreuss/Desktop/OP50_centroid_tracking_bins.RDS"

#Until how many hours
show_until <- 12

#load data
data <- map_dfr(files_to_process,readRDS) %>%
  mutate(hours = minutes / 60) %>%
  #round per half an hour
  # mutate(hours_rounded= floor(hours * 2) / 2) %>%
  #round per hour
  mutate(hours_rounded = ceiling(hours)) %>%
  #filter out first 30 mins
  filter(hours > 0.5) %>%
  na.omit() %>%
  # scale velocity to mm/s
  mutate(p_mean_velocity = p_mean_velocity / 1000) %>%
  group_by(annotation, hours_rounded) %>%
  filter(hours_rounded <= show_until) %>%
  #count tracks per hour per condition
  mutate(number_of_tracks_in_this_hour = n_distinct(dataset_ID,tp,TrackID)) %>%
  ungroup()

#simple density scatterplot
plot_scatterdensity <- function(data_to_plot){
  ggplot(data_to_plot,aes(x = p_mean_angle, y = p_mean_velocity))+
    theme_classic()+
    ylab("velocity (mm/s)") + xlab("angular velocity (degree/s)")+
    scale_y_log10()+
    scale_x_continuous(limits=c(0,180))+
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text.x = element_blank())
}

#list of minutes, this defines the number of timesteps
minutes <- seq(60,max(data$minutes),5)
#vector for camera rotation
thetavec <- seq(0,45,length.out = length(minutes))

#new image for each timestep
for (i in seq(1:length(minutes))){
  
  #rolling average of 30 mins to include data for each timestep
  low <-  minutes[i]-30
  high <- minutes[i]+30
  #get hour for title
  c_hour <- floor(minutes[i]/60)
  #get minute for title
  c_minute <- round(((minutes[i]/60)%%1)*60,0)
  
  #do the plot
  plot <- data %>%
    filter(annotation == "OP50") %>%
    #filter out data within rolling average
    filter(minutes <= high & minutes > low) %>%
    plot_scatterdensity(.) +
      scale_fill_viridis(option = "turbo")

    plot_gg(plot,width=3,height=3,windowsize=c(3200,3200))
    render_camera(theta=thetavec[i],phi=30)
    render_snapshot(paste0(unique(data$annotation),minutes[i],".png"),
                    title_text = paste0(c_hour, "h",c_minute),
                    title_font = "Helvetica",
                    title_position = "North",clear = TRUE)
}
    
