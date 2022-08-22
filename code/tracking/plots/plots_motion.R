# Use this script for plotting motion measurements.
# Needed: "...centroid_tracking_bins.RDS" as written by the "motion.R" script

# To run the script type: Rscript plots_motion.R "the location of your motion data folder" 
# e.g. Rscript plots_motion.R /Users/fpreuss/Desktop/paper/data/motion/

#for everything good
library(tidyverse)
#for colors
library(wesanderson)
library(viridis)
#for extended options for facet_wrap
library(ggh4x)

############### functions ###############
#1 Plotting functions

plot_basis <- function(data_to_plot){
  ggplot(data_to_plot,aes(x = p_mean_angle, y = p_mean_velocity))+
    theme_classic()+
    ylab("velocity (mm/s)") + xlab("angular velocity (degree/s)")+
    scale_y_log10()+
    annotation_logticks(base = 10,sides = "lr")+
    # scale_x_continuous(limits=c(0,180))+
    theme(legend.direction="horizontal",
          legend.position = "top",
          strip.background = element_rect(colour = "white", fill = "white"),
          panel.spacing = unit(1, "lines"),
          strip.text.x = element_text(size=25,colour = "black", face = "bold"),
          axis.text.x = element_text(size=30),
          axis.title.x = element_text(size=45),
          axis.text.y = element_text(size=35),
          axis.title.y = element_text(size=45))
}


plot_scatterpoints <- function(data_to_plot){
  plot_basis(data_to_plot)+
    geom_point(shape=21, fill="grey",alpha=0.25)
}


plot_scatterdensity <- function(data_to_plot){
  plot_basis(data_to_plot)+
    stat_density_2d(aes(fill = ..ndensity..), geom = "raster", contour = FALSE) +
    geom_rug(col="black",alpha=.01)+
    scale_fill_gradient(low="white",high="black")
}


#######################################################

#define base path  
# base_path <- commandArgs(trailingOnly = TRUE)[1]
base_path <- "/Users/fpreuss/Desktop/paper/data/motion/"
#define save path
save_path <- file.path(dirname(dirname(base_path)), "plots", "motion")
dir.create(save_path,recursive = TRUE)


#list of centroid tracking .rds files in data folder
files_to_process <- list.files(base_path, "tracked",full.names = TRUE, ignore.case = TRUE)

#Until how many hours
show_until <- 12

#lower velocity limit for mode == roaming
velocity_r <- 0.01
#upper angle limit for mode == roaming
angle_r <- 15
# upper velocity limit for mode == still
velocity_s <- 0.001

data <- map_dfr(files_to_process,readRDS) %>%
  mutate(hours = minutes / 60) %>%
  #round per half an hour
  # mutate(hours_rounded= floor(hours * 2) / 2) %>%
  #round to next hour up
  # mutate(hours_rounded = ceiling(hours)) %>%
  mutate(hours_rounded = round(hours)) %>%
  #filter out first 30 mins
  filter(hours_rounded > 0) %>%
  mutate(annotation = gsub("\\s", "_",annotation))%>% 
  na.omit() %>%
  # scale velocity to mm/s
  mutate(p_mean_velocity = p_mean_velocity / 1000) %>%
  group_by(annotation, hours_rounded) %>%
  filter(hours_rounded <= show_until) %>%
  #count tracks per hour per condition
  mutate(number_of_tracks_in_this_hour = n_distinct(dataset_ID,tp,TrackID)) %>%
  ungroup()

#plot track durations for all datasets
selected_annotations <- c("HB101", "Agar","daf2_OP50", "OP50", "OP50_w_Az" )
data %>%
  filter(annotation %in% selected_annotations) %>%
  group_by(annotation, hours_rounded) %>%
  mutate(mean_duration = mean(Duration_of_track/2)) %>%
  ggplot(., aes(Duration_of_track/2))+
    geom_histogram(binwidth=25)+
    geom_vline(aes(xintercept = mean_duration),color="red",linetype="dotted",size=2)+
    facet_wrap(vars(annotation,hours_rounded),ncol = 12)+
    xlab("track duration (s)")+
    theme_bw()
    ggsave(file.path(save_path,paste0("overview_duration_of_tracks.png")),height=15,width=35)


# unique(data$annotation)

#########################################

save_path_temp <- file.path(save_path,"scatterpoints","per_annotation")
dir.create(save_path_temp,recursive=TRUE)

#plot all 10s windows per annotation AND dataset_ID (both as points and density)
for (a in unique(data$annotation)){
  data_to_plot <- data %>%
    filter(annotation == a)
  
  plot_scatterpoints(data_to_plot)+
    facet_wrap2(vars(annotation,dataset_ID,hours_rounded),nrow=length(unique(data_to_plot$dataset_ID)),axes="all",remove_labels = "y")
  ggsave(file.path(save_path_temp,paste0("scatterpoints_",a,".png")),height=15,width=35)
  
  
}

save_path_temp <- file.path(save_path,"scatterdensity", "per_annotation")
dir.create(save_path_temp,recursive=TRUE)

for (a in unique(data$annotation)){
  data_to_plot <- data %>%
    filter(annotation == a)
  
  plot_scatterdensity(data_to_plot)+
    facet_wrap2(vars(annotation,dataset_ID,hours_rounded),nrow=length(unique(data_to_plot$dataset_ID)),axes="all",remove_labels ="y")+
    scale_fill_viridis(option = "turbo")
  
  ggsave(file.path(save_path_temp,paste0("scatterdensity_",a,".png")),height=15,width=35)
  
}


#plot annotated scatterpoints
#as seperate file for every annotation
save_path_temp <- file.path(save_path,"scatterpoints_annotated","grouped_individually")
dir.create(save_path_temp,recursive=TRUE)


for (a in unique(data$annotation)){
  data_to_plot <- data %>%
    filter(annotation == a) %>%
    mutate(mode = ifelse(p_mean_velocity > velocity_r & p_mean_angle < angle_r,"roaming","dwelling")) %>%
    mutate(mode=ifelse(p_mean_velocity < velocity_s, "still", mode))
  
  plot_basis(data_to_plot)+
    geom_point(aes(fill=mode),shape=21 ,alpha=0.25)+
    geom_rug(col="black",alpha=.01)+
    scale_fill_manual(values = wes_palette("BottleRocket2"))+
    facet_wrap2(vars(annotation,hours_rounded),ncol=length(unique(data$hours_rounded)),axes="all",remove_labels ="y")
  ggsave(file.path(save_path_temp,paste0("scatterpoints_annotated_",a,".png")),height=7.5,width=45)
  
}  


#plot the relative density for all annotations, timepoints and datasets
#additionally also as seperate file for every annotation
save_path_temp <- file.path(save_path,"scatterdensity","grouped_all")
dir.create(save_path_temp)

#all annotations in one image
plot_scatterdensity(data)+
  facet_wrap2(vars(annotation,hours_rounded),ncol=length(unique(data$hours_rounded)),axes="all",remove_labels ="y")
ggsave(file.path(save_path_temp,paste0("scatterdensity_all",".png")),bg="black",height=30,width=40)

plot_scatterdensity(data)+
  facet_wrap2(vars(annotation,hours_rounded),ncol=length(unique(data$hours_rounded)),axes="all",remove_labels ="y")+
  scale_fill_viridis(option = "viridis")
ggsave(file.path(save_path_temp,paste0("scatterdensity_all_viridis",".png")),bg="black",height=30,width=40)

plot_scatterdensity(data)+
  facet_wrap2(vars(annotation,hours_rounded),ncol=length(unique(data$hours_rounded)),axes="all",remove_labels ="y")+
  scale_fill_viridis(option = "turbo")
ggsave(file.path(save_path_temp,paste0("scatterdensity_all_turbo",".png")),bg="black",height=30,width=40)




#########

save_path_temp <- file.path(save_path,"scatterdensity","grouped_individually")
dir.create(save_path_temp, recursive = TRUE)


#seperate image for each annotation
for (a in unique(data$annotation)){
  data_to_plot <- data %>%
    filter(annotation == a) %>%
    mutate(hours_rounded = paste0(hours_rounded, " \n",number_of_tracks_in_this_hour))

  
  
  plot_scatterdensity(data_to_plot)+
    facet_wrap2(vars(factor(hours_rounded,levels=str_sort(unique(data_to_plot$hours_rounded),numeric=TRUE))),ncol=length(unique(data$hours_rounded)),axes="all",remove_labels ="y")
  ggsave(file.path(save_path_temp,paste0("scatterdensity_bw_",a,".png")),height=6,width=45)
  
  plot_scatterdensity(data_to_plot)+
    facet_wrap2(vars(factor(hours_rounded,levels=str_sort(unique(data_to_plot$hours_rounded),numeric=TRUE))),ncol=length(unique(data$hours_rounded)),axes="all",remove_labels ="y")+
    scale_fill_viridis(option = "viridis")
  ggsave(file.path(save_path_temp,paste0("scatterdensity_viridis_",a,".png")),height=6,width=45)
  
  plot_scatterdensity(data_to_plot)+
    scale_y_continuous()+
    facet_wrap2(vars(factor(hours_rounded,levels=str_sort(unique(data_to_plot$hours_rounded),numeric=TRUE))),ncol=length(unique(data$hours_rounded)),axes="all",remove_labels ="y")+
    scale_fill_viridis(option = "viridis")
  ggsave(file.path(save_path_temp,paste0("scatterdensity_viridis_",a,"_NOT_LOG.png")),height=6,width=45)
  
  plot_scatterdensity(data_to_plot)+
    facet_wrap2(vars(factor(hours_rounded,levels=str_sort(unique(data_to_plot$hours_rounded),numeric=TRUE))),ncol=length(unique(data$hours_rounded)),axes="all",remove_labels ="y")+
    scale_fill_viridis(option = "turbo")
  ggsave(file.path(save_path_temp,paste0("scatterdensity_turbo_",a,".png")),height=6,width=45)
  
}
