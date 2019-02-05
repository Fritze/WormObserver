#Generate plots (after running calculate_overview_statistics(offset,duration))



#How many worms per timepoint?
dfp %>%
  filter(dataset_ID %in% c("2018-12-07_16-00-07","2018-12-13_17-12-17")) %>%
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
  filter(dataset_ID %in% c("2018-12-07_16-00-07","2018-12-13_17-12-17")) %>%
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

#angle velocity
dfp %>%
  filter(dataset_ID %in% c("2018-12-07_16-00-07","2018-12-13_17-12-17")) %>%
  ggplot(., aes(x=minutes,y=ang_velocity,fill=dataset_ID,colour=dataset_ID))+
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  geom_point(aes(fill=dataset_ID),alpha=0.05)+
  geom_smooth(method="loess")+
  scale_y_log10()+
  theme_black()

dfp %>%
  ggplot(., aes(x=minutes,y=ang_velocity,fill=dataset_ID,colour=dataset_ID))+
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  geom_point(aes(fill=dataset_ID),alpha=0.05)+
  geom_smooth(method = "loess")+
  scale_y_log10()+
  theme_black()


#velocity
dfp %>%
  filter(dataset_ID %in% c("2018-12-07_16-00-07","2018-12-13_17-12-17")) %>%
  ggplot(aes(x=minutes,y=mean_velocity,group=minutes,fill=dataset_ID,colour=dataset_ID))+
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  geom_boxplot()+
  theme_black()

dfp %>%
  filter(dataset_ID %in% c("2018-12-07_16-00-07","2018-12-13_17-12-17")) %>%
  group_by(minutes,dataset_ID)%>%
  summarise(mean_velocity_per_minute=mean(mean_velocity))%>%
  ggplot(aes(x=minutes,y=mean_velocity_per_minute,color=dataset_ID))+
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  geom_line()+
  ylim(c(0, 1.5))+
  theme_black()



dfp %>%
  filter(dataset_ID %in% c("2018-12-07_16-00-07","2018-12-13_17-12-17")) %>%
  ggplot(aes(x=minutes,y=mean_velocity,fill=dataset_ID,colour=dataset_ID))+
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  geom_smooth(method="loess")+
  ylim(c(0, 1.5))+
  theme_black()




#angle
dfp %>%
  filter(dataset_ID %in% c("2018-12-07_16-00-07","2018-12-13_17-12-17")) %>%
  ggplot(., aes(x=minutes,y=mean_angle,fill=dataset_ID,colour=dataset_ID))+
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  geom_point(aes(fill=dataset_ID),alpha=0.05)+
  geom_smooth(method="loess")+
  scale_y_log10()+
  theme_black()

dfp %>%
  filter(dataset_ID %in% c("2018-12-07_16-00-07","2018-12-13_17-12-17")) %>%
  filter(mean_displacement > 2) %>%
  group_by(minutes,dataset_ID)%>%
  summarise(mean_angle_per_minute=mean(mean_angle))%>%
  ggplot(., aes(x=minutes,y=mean_angle_per_minute,fill=dataset_ID,colour=dataset_ID))+
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  geom_line()+
  ylim(0,180)+
  theme_black()

dfp %>%
  filter(dataset_ID %in% c("2018-12-07_16-00-07","2018-12-13_17-12-17")) %>%
  ggplot(., aes(x=minutes,y=mean_angle,fill=dataset_ID,colour=dataset_ID))+
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  geom_smooth(method="loess")+
  theme_black()

dfp %>%
  filter(dataset_ID %in% c("2018-12-14_17-27-46")) %>%
  ggplot(., aes(x=minutes,y=mean_angle,fill=dataset_ID,colour=dataset_ID))+
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  geom_point(aes(fill=dataset_ID),alpha=0.05)+
  geom_smooth(method="loess")+
  scale_y_log10()+
  theme_black()

#######

dfp %>%
  filter(dataset_ID %in% c("2018-12-07_16-00-07")) %>%
  ggplot(aes(x=minutes,y=mean_angle,group=tp))+
  geom_boxplot()+
  theme_black()

dfp %>%
  filter(dataset_ID %in% c("2018-12-13_17-12-17")) %>%
  ggplot(aes(x=minutes,y=mean_angle,group=tp))+
  geom_boxplot()+
  theme_black()

dfp %>%
  filter(dataset_ID %in% c("2018-12-14_17-27-46")) %>%
  ggplot(aes(x=minutes,y=mean_angle,group=tp))+
  geom_boxplot(alpha=0.001)+
  scale_y_log10()+
  theme_black()

#######

dfp %>%
  ggplot(., aes(x=minutes,y=mean_angle,fill=dataset_ID,colour=dataset_ID))+
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  geom_point(aes(fill=dataset_ID),alpha=0.05)+
  geom_smooth(method = "loess")+
  scale_y_log10()+
  theme_black()

#displacement
dfp %>%
  filter(dataset_ID %in% c("2018-12-07_16-00-07","2018-12-13_17-12-17")) %>%
  filter(mean_displacement > 2) %>%
  group_by(minutes,dataset_ID)%>%
  summarise(mean_displacement_per_minute=mean(mean_displacement))%>%
  ggplot(., aes(x=minutes,y=mean_displacement_per_minute,fill=dataset_ID,colour=dataset_ID))+
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  geom_line()+
  ylim(c(0,300))+
  theme_black()

dfp %>%
  filter(mean_displacement > 2) %>%
  ggplot(., aes(x=minutes,y=mean_displacement,fill=dataset_ID,colour=dataset_ID))+
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  geom_point(alpha=0.075)+
  geom_smooth(method = "loess")+
  scale_y_log10()+
  theme_black()

dfp %>%
  filter(dataset_ID %in% c("2018-12-07_16-00-07","2018-12-13_17-12-17")) %>%
  ggplot(aes(x=minutes,y=mean_displacement,group=minutes,fill=dataset_ID,colour=dataset_ID))+
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  geom_smooth(method="loess")+
  scale_y_log10()+
  theme_black()

#ang_velocity against angle
dfp %>%
  filter(dataset_ID %in% c("2018-12-07_16-00-07","2018-12-13_17-12-17")) %>%
  ggplot(.,aes(x=ang_velocity, y=mean_velocity,colour=minutes))+
  scale_color_viridis(option="B")+
  geom_point(alpha=0.5)+
  scale_x_reverse()+
  theme_black()

dfp %>%
  filter(dataset_ID %in% c("2018-12-07_16-00-07","2018-12-13_17-12-17")) %>%
  ggplot(aes(x = ang_velocity, y = mean_velocity))+
  stat_density2d(geom="raster", aes(fill = ..density..),contour = FALSE)+
  scale_fill_viridis(option = "inferno")+
  scale_x_reverse()+
  # geom_abline(intercept = 3, slope = -3.04,colour="red")+
  theme_classic2()


#velocity against angle
dfp %>%
  filter(dataset_ID %in% c("2018-12-07_16-00-07","2018-12-13_17-12-17")) %>%
  ggplot(.,aes(x=mean_angle, y=mean_velocity,colour=minutes))+
  scale_color_viridis(option="B")+
  geom_point(alpha=0.5)+
  scale_x_reverse()+
  theme_black()


dfp %>%
  filter(dataset_ID %in% c("2018-12-07_16-00-07","2018-12-13_17-12-17")) %>%
  ggplot(aes(x = mean_angle, y = mean_velocity))+
  stat_density2d(geom="raster", aes(fill = ..density..),contour = FALSE)+
  scale_fill_viridis(option = "inferno")+
  scale_x_reverse()+
  # geom_abline(intercept = 3, slope = -3.04,colour="red")+
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