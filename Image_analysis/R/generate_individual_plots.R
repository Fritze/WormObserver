#Generate plots (after running calculate_overview_statistics(offset,duration))



#How many worms per timepoint?
dfp %>%
  select(minutes, dataset_ID, TrackID) %>%
  group_by(minutes, dataset_ID) %>%
  summarise(.,n = n_distinct(TrackID)) %>%
  ggplot(.,aes(x=minutes,y=n,fill=dataset_ID,colour=dataset_ID))+
  scale_colour_manual(values = wes_palette("Darjeeling1"))+
  geom_line()+
  geom_smooth(method="loess")+
  ylab("Number of tracked worms")+
  theme_black()

#How long tracks are the per timepoint?
data %>%
  select(minutes,dataset_ID, TrackID, Duration_of_track) %>%
  group_by(minutes, dataset_ID) %>%
  summarise(., mean_duration =mean(Duration_of_track)/25) %>%
  ggplot(.,aes(x=minutes,y=mean_duration,fill=dataset_ID,colour=dataset_ID))+
  scale_colour_manual(values = wes_palette("Darjeeling1"))+
  geom_line()+
  geom_smooth(method="loess")+
  geom_hline(yintercept=15,color="red")+
  ylab("Mean duration of tracks (seconds)")+
  theme_black()





dfp2 <-  dfp %>%
  group_by(minutes,dataset_ID)%>%
  summarise(size_median_per_minute=eccentricity_median_per_minute=median(mean_eccentricity),angle_median_per_minute=median(mean_angle),velocity_median_per_minute=median(mean_velocity), sd_velocity_median_per_minute=median(sd_velocity),displacement_median_per_minute=median(mean_displacement))
params <- c("eccentricity_median_per_minute","angle_median_per_minute", "velocity_median_per_minute", "sd_velocity_median_per_minute", "displacement_median_per_minute")
for (i in params){
  pdf(file.path("/media/ella/raid5/WormObserver/Image_analysis/R/plots",paste0(i, ".pdf")))
  print(ggplot(dfp2, aes_string(x="minutes",y=i,fill="dataset_ID",colour="dataset_ID"))+
      #geom_ribbon(aes(ymin =  median_angle_per_minute - iqr_angle_per_minute, ymax = median_angle_per_minute + iqr_angle_per_minute))+
      geom_line(size=1)+
      ggtitle(i)+
      # ylim(c(0,180))+
      theme_black()
  )
  dev.off()
}


ggplot(dfp2, aes_string(x="minutes",y="angle_median_per_minute",colour="dataset_ID"))+
  #geom_ribbon(aes(ymin =  median_angle_per_minute - iqr_angle_per_minute, ymax = median_angle_per_minute + iqr_angle_per_minute))+
  geom_line()+
  ggtitle("angle_median_per_minute")+
  # ylim(c(0,180))+
  theme_black()



#velocity against angle
dfp %>%
  ggplot(.,aes(x=mean_angle, y=mean_velocity,colour=minutes))+
  scale_color_viridis(option="B")+
  geom_point(alpha=0.5)+
  scale_x_reverse()+
  theme_black()


dfp %>%
  ggplot(aes(x = mean_angle, y = mean_velocity))+
  stat_density2d(geom="raster", aes(fill = ..density..),contour = FALSE)+
  scale_fill_viridis(option = "inferno")+
  scale_x_reverse(limits=c(180,0),
                  breaks = seq(180, 0, by = -36))+
  scale_y_continuous(limits=c(0,15),
                     breaks = seq(0,15, by = 5))+
  theme_classic2()


##########################################################################################################################################
#### The distribution of the average path angle per timepoint


library(viridis)
dfp %>%
  filter(dataset_ID %in% c("2018-12-13_17-12-17")) %>%
  ggplot(., aes(x=mean_velocity, group=minutes, col=minutes))+
  geom_density(position='dodge',size=1.5)+
  theme_classic()+
  scale_color_viridis(option="A")+
  xlab("Average path angle")+
  ylab("density")+
  guides(fill = guide_legend(keywidth = 20, keyheight = 1))+
  theme(axis.title.x = element_text(size=40, face="bold"),
        axis.title.y = element_text(size=40, face="bold"),
        axis.text.x = element_text(size=35, face="bold"),
        axis.text.y = element_text(size=35, face="bold"),
        legend.text = element_text(size=30),
        legend.title = element_text(size=30, face="bold")
  )