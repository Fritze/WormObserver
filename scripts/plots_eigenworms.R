# Use this script after running the "filter_skeletons.R" script that filters skeletons for a given condition.
# Needed: "...skeletons_filtered.rds"
# as computed by the "filtered_skeletons.R" script
# please also speficy the dataset you want to analyse/plot and the conversion factor.
# you also need to speficy the two timeranges you want to compare (i.e. 1-4h and 10-12, please type 1 4 10 12)
# add "p" to process data from scratch or add "ap" as last argument to use data that was already processed (i.e. filtered to select for dispersal phenotype)
# To run the script type: Rscript plots_eigenworms.R "the location of your filtered skeletonized data folder" "dataset" "coversion factor"
# e.g. Rscript plots_eigenworms.R /Users/fpreuss/Desktop/paper/data/skeletonized/filtered/ OP50 6.25 1 4 10 12 p

library(tidyverse)
library(patchwork)
library(viridis)
options(dplyr.summarise.inform=FALSE)
library("reshape2")

###### functions ######

#small helper functions
# used to calculate angles describing the worm path
angle1 <- function(x1, y1, x2, y2) {
  values <- list(x1, y1, x2, y2)
  if(anyNA(values)) {
    "NA"
  } else {
    x <- c(x1, y1)
    y <- c(x2, y2)
    dot.prod <- x%*%y
    norm.x <- norm(x,type="2")
    norm.y <- norm(y,type="2")
    theta <- acos(dot.prod / (norm.x * norm.y))
    as.numeric(theta)
  }
}

# used to calculate angles between worm segments
angle2 <- function(x1, y1, x2, y2) {
  atan2(y2,x2) - atan2(y1,x1) 
}

# used to calculate distance change for estimating speed
distance <- function(x1, y1, x2, y2) {
  values <- list(x1, y1, x2, y2)
  if(anyNA(values)) {
    "NA"
  } else {
    length <- sqrt((x1-x2)^2+(y1-y2)^2)
    as.numeric(length)
  }
}

#plotting functions
plot_skeleton_movie <- function(data_to_plot,X,Y,second){
  
  data_to_plot_temp <- data_to_plot %>%
    filter(seconds == second)
  
  ggplot(data_to_plot_temp, aes_string(x=X,y=Y,color="angle"))+
    geom_point(size=2.5) +
    geom_point(data=filter(data_to_plot_temp, index==1), aes(x=X,y=Y), color="gold",shape=17,size=4)+
    scale_x_continuous(limits=c(min(data_to_plot$X),max(data_to_plot$X)))+
    scale_y_continuous(limits=c(min(data_to_plot$Y),max(data_to_plot$Y)))+
    # geom_path(size=2,lineend = "round") +
    theme_void()+
    coord_fixed(ratio = 1)+
    scale_color_gradient2(low = "deepskyblue", mid = "lavender",
                          high = "deeppink",midpoint=0,limits=c(-1,1),na.value="grey") +
    theme(plot.title = element_text(face = "bold"),
          strip.text.x = element_blank()) +
    facet_wrap(vars(dataset_trackID),ncol=1)
  
}


plot_wave <- function(dataset){
  
  data_to_plot <<- dataset %>%
    filter(!index %in% c(1,2,25,26))
  
  
  ggplot(data_to_plot, aes(x=seconds,y=index,fill=angle))+
    geom_tile()+
    scale_x_continuous(limits=c(0,max(data_to_plot$seconds)))+
    scale_y_continuous(trans="reverse")+
    scale_fill_gradient2(low = "cornflowerblue",mid ="white",high = "brown2",midpoint=0,na.value="white") +
    theme_classic()+
    theme(
      legend.position = "top")+
    labs(x="seconds")+
    theme(strip.text.x = element_text(size = 10))+
    labs(fill = "Intersegment angle (rad)") +
    facet_wrap(vars(dataset_trackID))+
    coord_equal()
}

plot_wave_movie <- function(data_to_plot,second){
  
  ggplot(data_to_plot, aes(x=seconds,y=index,fill=angle))+
    geom_tile()+
    geom_vline(aes(xintercept=second),linetype="longdash")+
    scale_x_continuous(limits=c(0,max(data_to_plot$seconds)))+
    scale_y_continuous(trans="reverse")+
    scale_fill_gradient2(low = "cornflowerblue",mid ="white",high = "brown2",midpoint=0,na.value="white") +
    theme_classic()+
    theme(
      legend.position = "top")+
    labs(x="seconds")+
    theme(strip.text.x = element_text(size = 10))+
    labs(fill = "Intersegment angle (rad)") +
    facet_wrap(vars(dataset_trackID))+
    coord_equal()
}

plot_PCs_movie <- function(data_to_plot,second){
  
  data_to_plot_temp <- data_to_plot %>%
    filter(seconds <= second)
  
  ggplot(data_to_plot_temp, aes(x=PC1, PC2,color=seconds))+
    scale_x_continuous(limits=c(min(data_to_plot$PC1),max(data_to_plot$PC1)))+
    scale_y_continuous(limits=c(min(data_to_plot$PC2),max(data_to_plot$PC2)))+
    geom_path()+
    geom_point(size=1.5)+
    facet_wrap(vars(time),ncol=1)+
    scale_color_viridis(option="mako")+
    theme_classic()
  
}


plot_PCs_linear_movie <- function(data_to_plot,second){
  
  data_to_plot_temp <- data_to_plot %>%
    group_by(ID,seconds,PC1,PC2,PC3) %>%
    summarise() %>%
    select(ID,matches("PC1|PC2|PC3"),seconds) %>%
    pivot_longer(!c(ID,seconds)) %>%
    arrange(seconds)
  
  ggplot(data_to_plot_temp, aes(x=seconds,y=value,color=name))+
    geom_path()+
    geom_vline(aes(xintercept=second),linetype="longdash")+
    scale_x_continuous(limits=c(0,max(data_to_plot$seconds)))+
    scale_y_continuous(trans="reverse")+
    scale_fill_gradient2(low = "cornflowerblue",mid ="white",high = "brown2",midpoint=0,na.value="white") +
    theme_classic()+
    theme(
      legend.position = "top")+
    labs(x="seconds")+
    theme(strip.text.x = element_text(size = 10),
          legend.position = "none")+
    labs(fill = "amplitude") +
    facet_wrap(vars(name),ncol=1)
}


################################################################################


#define base path  
base_path <- commandArgs(trailingOnly = TRUE)[1]
#define save path
save_path <- file.path(dirname(dirname(dirname(base_path))), "plots", "skeletons")
dir.create(save_path,recursive = TRUE)
#raw path
dataraw_file_path <- file.path(dirname(dirname(dirname(base_path))), "data","raw")

#define dataset to plot
selected_annotation <-commandArgs(trailingOnly = TRUE)[2]
already_processed <- commandArgs(trailingOnly = TRUE)[8]


if(already_processed == "p"){

  #define conversion factor
  conversion_factor <- as.numeric(commandArgs(trailingOnly = TRUE)[3])
  #define timeranges
  timeranges <- list(c(as.numeric(commandArgs(trailingOnly = TRUE)[4]):as.numeric(commandArgs(trailingOnly = TRUE)[5])),c(as.numeric(commandArgs(trailingOnly = TRUE)[6]):as.numeric(commandArgs(trailingOnly = TRUE)[7])))

  #parameters for selecting only dispersing worms
  velocity_limit <- 0.01
  angle_limit <- 15
  length_track_limit <- 20


  #get files that have to be loaded
  file_to_process <- grep(paste0(".+\\/",selected_annotation,"\\_skeletonized_filtered.rds"),list.files(base_path,full.names = TRUE),value=TRUE)


  #loading data
  cat(paste0("\n\nloading dataset ", file_to_process,"\n\n"))
  skeleton_data_filtered <- file_to_process %>%
    map_df(., function(x) readRDS(x))

  #this is a workaround for attaching the correct offset times (i.e. minutes worms have spent on the plate prior to imaging) to the data
  #we open the grouped raw RDS for this. this is slow.
  #ideally, this should be done in the skeletonization script
  cat(paste0("\n\nfetching offsets for ",file_to_process,"\n\n"))

  #function for fetching offsets from grouped raw RDS file
  read_offset <- function(x,y){
    print(x)
    print(y)
    file_path <- file.path(dataraw_file_path, paste0(x,"_raw_data.rds"))
    cat(paste0("\n\nFetching offsets from ", file_path))
    temp <- readRDS(file_path) %>%
      filter(dataset_ID %in% y) %>%
      group_by(dataset_ID,time_elapsed,timepoint_length, timestep_length) %>%
      summarise()
    return(temp)
  }

  offsets <- read_offset(selected_annotation,unique(skeleton_data_filtered$dataset_ID))

  #new skeleton data filtered table now with offsets
  skeleton_data_filtered <- skeleton_data_filtered %>%
    left_join(offsets,by="dataset_ID") %>%
    #accounting for offsets
    mutate(minutes = time_elapsed + ((tp-1)*timepoint_length+(tp-1)*timestep_length)) %>%
    mutate(hours_rounded = round(minutes/60)) %>%
    #filter out first 30 mins
    filter(hours_rounded > 0) %>%
    filter(hours_rounded %in% unlist(timeranges)) %>%
    mutate(ID = paste0(dataset_ID, "_",tp,"_",TrackID,"_",frame)) %>%
    group_by(annotation,ID,dataset_ID, tp,hours_rounded, TrackID,frame,location_x,location_y) %>%
    nest() %>%
    group_by(annotation,dataset_ID,tp,hours_rounded,TrackID) %>%
    mutate(x_lag=lag(location_x,n=1) - location_x, y_lag = lag(location_y, n=1) - location_y) %>%
    mutate(x_lead=lead(location_x, n=1) - location_x, y_lead=lead(location_y, n=1) - location_y) %>%
    # calculate the angle and convert to degrees
    mutate(angle_track = suppressWarnings(180 - (as.numeric(mapply(angle1,x_lag,y_lag,x_lead,y_lead)))*180/pi)) %>%
    # measure distance between current point and 1 second before
    mutate(local_distance=suppressWarnings(as.numeric(mapply(distance,lag(location_x,n=2),lag(location_y,n=2),location_x,location_y)))) %>%
    #this will result in velocity based on local distance (mm/s)
    mutate(velocity = local_distance*conversion_factor/2/1000)

  annotation <- unique(skeleton_data_filtered$annotation)

  cat(paste0("\n\nnow filtering ",file_to_process))

  #filtering data
  #filter out data to have only moving worms and tracks over a certain length
  skeleton_data_filtered2 <- skeleton_data_filtered %>%
    group_by(annotation,dataset_ID,tp,TrackID) %>%
    #filter only dispersing worms
    filter(velocity > velocity_limit) %>%
    filter(angle_track < angle_limit) %>%
    #the track should not have any missing frames
    mutate(length_track = n(), length_track2=last(frame)-first(frame)+1) %>%
    filter(length_track == length_track2) %>%
    #only tracks longer than 10 secs (> 20 frames == 10 seconds if 2 fps)
    filter(length_track > length_track_limit) %>%
    select(-c(length_track2,local_distance, x_lag, x_lead)) %>%
    unnest(cols= c(data)) %>%
    mutate(dataset_trackID = paste0(dataset_ID,"_", tp,"_", TrackID)) %>%
    #attach timeranges for later
    mutate(timerange_early_min=min(timeranges[[1]]),
           timerange_early_max=max(timeranges[[1]]),
          timerange_late_min=min(timeranges[[2]]),
           timerange_late_min=max(timeranges[[2]]))

  saveRDS(skeleton_data_filtered2,file.path(base_path, paste0(selected_annotation,"_skelelton_filtered_dispersal.rds")))

}else if(already_processed == "ap"){
  path_already_processed <- file.path(base_path, paste0(selected_annotation,"_skelelton_filtered_dispersal.rds"))
  cat("\n\n loading already processed data ", path_already_processed)
  skeleton_data_filtered2 <- readRDS(path_already_processed)
} else {
  cat("wrong argument provided, please see comments in the script.")
}



#number sampled tracks for downsampling to have equal amount of tracks for both timeranges
number_sampled_tracks <- skeleton_data_filtered2 %>%
  mutate(time=ifelse(hours_rounded <= timerange_early_max,"early", "late")) %>%
  group_by(dataset_trackID,time) %>%
  summarise() %>%
  group_by(time) %>%
  summarise(n=n()) %>%
  pull(n) %>%
  min() %>%
  plyr::round_any(10,floor)
  


#number sampled tracks for downsampling to have equal amount of tracks for both timeranges
number_sampled_skeletons <- skeleton_data_filtered2 %>%
  mutate(time=ifelse(hours_rounded <= timerange_early_max,"early", "late")) %>%
  group_by(ID,time) %>%
  summarise() %>%
  group_by(time) %>%
  summarise(n=n()) %>%
  pull(n) %>%
  min() %>%
  plyr::round_any(100,floor)

cat(paste0("\n\nsampling ", number_sampled_skeletons, " skeletons from ",number_sampled_tracks, " tracks.\n\n"))

#How many tracks per hours_rounded?
number_tracks_per_hour <- skeleton_data_filtered2 %>%
  group_by(dataset_trackID,hours_rounded) %>%
  nest() %>%
  group_by(hours_rounded) %>%
  summarise(number_tracks_per_hour=n())




#from the filtered dataset, for each skeleton get ID and local velocity
#this will be added back to the skeletons after PCA
IDvelo <- skeleton_data_filtered2 %>%
  group_by(ID,velocity) %>%
  nest() %>%
  select(c(ID, velocity))

#get back offsets to re-attach them later
offsets <- skeleton_data_filtered2 %>%
  group_by(dataset_ID,time_elapsed,timepoint_length, timestep_length) %>%
  summarise()

cat(paste0("\n\ncalculating PCA.\n\n"))

#perform ONE pca for all skeletons in filtered dataset
#here we don't downsample yet
pca <- skeleton_data_filtered2 %>%
  dcast(ID ~ index,value.var = "angle") %>%
  #make IDs the rowname so that this column is not part of the clustering data
  tibble::column_to_rownames(var = "ID") %>%
  #take out first and last angle
  select(-c(1,ncol(.))) %>%
  prcomp(., center=TRUE)

timerange_early_max <- unique(skeleton_data_filtered2$timerange_early_max)
annotation <- unique(skeleton_data_filtered2$annotation)

# fviz_eig(pca)

#one row = one skeleton of a track at a given frame
#after pca, get back ID, tp, TrackID etc
#add information for each track and subset n tracks per timepoint (early vs late)
PC_values <- pca$x %>%
  #convert_to_dataframe
  as.data.frame() %>%
  #new variable from rowname
  tibble::rownames_to_column("ID") %>%
  #keep only first 6 eigenworms
  select(!matches("(PC[1-9][0-9]|PC[7-9])")) %>%
  #split ID to get information
  separate(ID, c("dataset_ID_date","dataset_ID_time","tp","TrackID_track","TrackID_ID","frame"),sep = "([\\_])",remove=FALSE) %>%
  #get types right
  mutate(frame = as.numeric(frame))%>%
  #since splitting happens on "_", some variables (dataset_ID and TrackID) have to be glued together again
  #indeed, not very elegant
  unite(dataset_ID,dataset_ID_date:dataset_ID_time) %>%
  unite(TrackID, TrackID_track:TrackID_ID) %>%
  #add ID column that is specific for a TRACK
  mutate(dataset_trackID = paste0(dataset_ID,"_", tp,"_", TrackID)) %>%
  #number each frame relatively for each track individually
  group_by(dataset_trackID) %>%
  mutate(numbering =1:n()) %>%
  # length of a track (in frames)
  mutate(length_frames=max(numbering)) %>%
  #convert this to seconds
  mutate(min_frame = min(frame)) %>%
  #the division by 2 corresponds to fps == 2
  mutate(seconds = (frame - min_frame)/2) %>%
  ungroup() %>%
  mutate(tp=as.numeric(tp)) %>%
  left_join(offsets,by="dataset_ID") %>%
  #accounting for offsets
  mutate(minutes = time_elapsed + ((tp-1)*timepoint_length+(tp-1)*timestep_length)) %>%
  mutate(hours_rounded = round(minutes/60)) %>%
  #filter out first 30 mins
  filter(hours_rounded > 0) %>%
  #round to full hours steps
  mutate(hours_rounded = ceiling(minutes/60)) %>%
  mutate(time=ifelse(hours_rounded <= timerange_early_max,"early", "late")) %>%
  group_by(time, dataset_trackID) %>%
  nest() %>%
  #sample tracks per timerange (variable: number_sampled_tracks)
  group_by(time) %>%
  sample_n(number_sampled_tracks) %>%
  unnest(cols=c(data)) %>%
  #add local velocity
  left_join(IDvelo, by="ID") %>%
  #the following calculates "phase velocity" based on the first two principal components (for each track)
  group_by(dataset_trackID) %>%
  #+ and - 1 frame equals 1s step (if 2fps)
  mutate(next_PC1 = lead(PC1,n=1), next_PC2 = lead(PC2,n=1)) %>%
  mutate(prev_PC1 = lag(PC1,n=1),prev_PC2 = lag(PC2,n=1)) %>%
  mutate(diff_prev_PC1 =  PC1 - prev_PC1, diff_prev_PC2 = PC2 - prev_PC2) %>%
  mutate(diff_next_PC1 = next_PC1 - PC1, diff_next_PC2 = next_PC2 - PC2) %>%
  #calculate phase velocity
  mutate(phase_velo=suppressWarnings(as.numeric(mapply(angle2,diff_prev_PC1,diff_prev_PC2,diff_next_PC1,diff_next_PC2)))) %>%
  #normalize to the range (-pi,pi), this will keep directionality, i.e. clockwise will be negative
  mutate(phase_velo=ifelse(phase_velo < -pi,phase_velo+2*pi,phase_velo)) %>%
  mutate(phase_velo=ifelse(phase_velo > pi, phase_velo-2*pi, phase_velo)) %>%
  ungroup() %>%
  left_join(number_tracks_per_hour,by="hours_rounded")

#sample random number of skeletons from the sampled tracks
PC_values_sub <- PC_values %>%
  group_by(time) %>%
  sample_n(number_sampled_skeletons) %>%
  ungroup()


saveRDS(PC_values_sub, file.path(base_path,paste0(annotation,"_", number_sampled_tracks,"_tracks_",number_sampled_skeletons,"_skeletons_PC_values.RDS")))

cat("\n\nnow plotting eigenworms.\n\n")

save_path_temp <- file.path(save_path,"eigenworms")
dir.create(save_path_temp)

plot_size = 5

six_eigenworms <- melt(pca$rotation[,1:6]) %>%
  rename(index = 1, eigenworm = 2, angle = 3)

plot_eigenworms <- six_eigenworms %>%
   ggplot(., aes(index,angle)) +
   # geom_point(size=2)+
   geom_path(size=3,lineend="round")+
   scale_y_continuous(limits=c(-0.5,0.5))+
   facet_wrap(vars(eigenworm),ncol=6)+
   labs(x="# segment angle", y="angle (rad)")+
   theme_classic()+
   coord_fixed(ratio=10)+
   theme(
     legend.position = "none"
   )

pca_con <- setNames(data.frame(1:length(pca$sdev),pca$sdev^2),c("eigenworm","value")) %>%
  mutate(cumsum_eigenvalue = cumsum(value)) %>%
  mutate(var_contr_cumsum = cumsum_eigenvalue/sum(value)) %>%
  mutate(var_contr_rel = value/sum(value)) %>%
  filter(eigenworm %in% c(1:6))

plot_contr <- ggplot(pca_con,aes(eigenworm,var_contr_cumsum*100)) +
  geom_bar(aes(x = eigenworm,y = var_contr_rel*100),stat="identity", position = "dodge",color="black",fill="white",lwd=1.5)+
  geom_line(size=1) +
  geom_point(size=5,color="black")+
  scale_y_continuous(limits=c(0,100),breaks=c(0,10,20,30,40,50,60,70,80,90,100)) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6)) +
  labs(x="# eigenworm", y="variance contribution (%)") +
  theme_classic()+
  theme(aspect.ratio=1,
        legend.position = "top",
        legend.direction="horizontal",
        strip.background = element_rect(colour = "white", fill = "white"),
        panel.spacing = unit(1, "lines"),
        strip.text.x = element_text(size=15,colour = "black", face = "bold"),
        axis.text.x = element_text(size=30),
        axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=30),
        axis.title.y = element_text(size=20),
        axis.ticks=element_line(size=1.5),
        axis.ticks.length=unit(0.25,"cm"),
        axis.line = element_line(colour = 'black', size = 1.5))


plot_eigenworms /
  plot_contr +
  plot_layout(heights = unit(c(5,15), c('cm', 'cm')))
ggsave(file.path(save_path_temp,paste0(annotation,"_", number_sampled_tracks,"_tracks_",number_sampled_skeletons,"_skeletons_","eigenworms_with_contributions.png")),height=10,width=20)




cat("\n\nnow plotting local velocity.\n\n")


#plotting distribution of local velocity (same subsampled tracks per time range)
save_path_temp <- file.path(save_path,"velocity_distris")
dir.create(save_path_temp)

plot_size = 3

#checking again if we have equal number of tracks per condition
# PC_values %>%
#   group_by(time) %>%
#   summarise(n_tracks = n_distinct(dataset_trackID))


#get list of velocities from tracks in early timepoints
ee <- PC_values_sub %>%
  filter(time == "early") %>%
  pull(velocity)

#get list of velocities from tracks in early timepoints
ll <- PC_values_sub %>%
  filter(time == "late") %>%
  pull(velocity)

#do a wilcox test to check significance
wilcox_t <- wilcox.test(ll,ee,paired=FALSE) %>%
  broom::tidy() %>%
  write.csv(file.path(save_path_temp, paste0(annotation,"_", number_sampled_tracks,"_tracks_",number_sampled_skeletons,"_skeletons_","velocity_wilcox.txt")))

#the plot
ggplot(PC_values_sub,aes(x=velocity,fill=time))+
  geom_density(alpha=0.5)+
  theme_bw()+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))
ggsave(file.path(save_path_temp, paste0(annotation,"_", number_sampled_tracks,"_tracks_",number_sampled_skeletons,"_skeletons_","velocity_density.png")),width=plot_size,height=plot_size-1)



cat("\n\nnow plotting distribution of phase velocity and local velocity.\n\n")

#plotting distribution of phase velocity and local velocity vs phase velocity (same subsampled n tracks per time range)

save_path_temp <- file.path(save_path,"phase_velocity")
dir.create(save_path_temp)

plot_size = 3


#get list of phase velocities from early timepoints
ee <- PC_values_sub %>%
  filter(time == "early") %>%
  pull(phase_velo)

#get list of phase velocities from late timepoints
ll <- PC_values_sub %>%
  filter(time == "late") %>%
  pull(phase_velo)

#do a wilcox test to check significance
wilcox_t <- wilcox.test(ll,ee,paired=FALSE) %>%
  broom::tidy() %>%
  write.csv(file.path(save_path_temp, paste0(annotation,"_", number_sampled_tracks,"_tracks_",number_sampled_skeletons,"_skeletons_","phase_velocity.txt")))


PC_values_sub %>%
  #filter out the first and last frames of each track as the corresponding angles for phase velocity will be NA
  filter(numbering != length_frames) %>%
  filter(numbering != 1) %>%
  ggplot(., aes(x=phase_velo,fill=time))+
  geom_density(alpha=0.5)+
  theme_bw()+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))
ggsave(file.path(save_path_temp, paste0(annotation,"_", number_sampled_tracks,"_tracks_",number_sampled_skeletons,"_skeletons_","phase_velocity_density.png")),width=plot_size,height=plot_size-1)

#for correlation plots only look at phase velocity > 0 (forward moving worms)
PC_values_sub %>%
  filter(phase_velo > 0) %>%
  ggplot(., aes(x=velocity, y=phase_velo,fill=time))+
  geom_point(alpha=0.01,shape=21)+
  theme_bw()+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  facet_wrap(vars(time))
ggsave(file.path(save_path_temp, paste0(annotation,"_", number_sampled_tracks,"_tracks_",number_sampled_skeletons,"_skeletons_","phase_velocity_correlation_colored.png")),width=plot_size+2,height=plot_size)

PC_values_sub %>%
  filter(phase_velo > 0) %>%
  ggplot(., aes(x=velocity, y=phase_velo,fill=time))+
  stat_density_2d(aes(fill = ..ndensity..), geom = "raster", contour = FALSE) +
  theme_classic()+
  scale_fill_viridis(option="viridis")+
  facet_wrap(vars(time))
ggsave(file.path(save_path_temp, paste0(annotation,"_", number_sampled_tracks,"_tracks_",number_sampled_skeletons,"_skeletons_","phase_velocity_correlation_viridis.png")),width=plot_size+2,height=plot_size)



cat("\n\nnow plotting distribution of angles.\n\n")


#plotting distribution of angles for the two timeranges 


save_path_temp <- file.path(save_path,"angle_distris")
dir.create(save_path_temp)



plot_size = 5


#by starting from the PC_values_sub we obtain the same n sampled tracks that are used for PCA
#we join with skeleton_data_filtered2 to get skeleton angles back
skeletons_subsampled <- PC_values_sub %>%
  left_join(skeleton_data_filtered2,by = c("ID")) %>%
  # filter out first and last segment as these will have angle  == NA
  filter(index != 1 ) %>%
  filter(index != 26)

#get list of absolute angles from early timepoints
ee <- skeletons_subsampled %>%
  filter(time == "early") %>%
  mutate(angle_abs = abs(angle)) %>%
  pull(angle_abs)

#get list of absolute angles from late timepoints
ll <- skeletons_subsampled %>%
  filter(time == "late") %>%
  mutate(angle_abs = abs(angle)) %>%
  pull(angle_abs)

#save the means of both distributations, so we can use this later as basis for our simulations
e <- mean(ee)
l <- mean(ll)

#do a wilcox test to check significance
wilcox_t <- wilcox.test(ll,ee,paired=FALSE) %>%
  broom::tidy() %>%
  write.csv(file.path(save_path_temp, paste0(annotation,"_", number_sampled_tracks,"_tracks_",number_sampled_skeletons,"_skeleton_angles_velocity.txt")))

skeletons_subsampled %>%
  ggplot(., aes(x=angle,fill=time))+
  geom_histogram(size=1,binwidth = 0.05) +
  scale_x_continuous(limits = c(-1.5,1.5),breaks=c(-1.5,-1,-0.5,0,0.5,1,1.5))+
  facet_wrap(vars(time)) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))
ggsave(file.path(save_path_temp, paste0(annotation,"_", number_sampled_tracks,"_tracks_",number_sampled_skeletons,"_skeleton_angles_histo.png")),width=plot_size,height=plot_size)

skeletons_subsampled %>%
  ggplot(., aes(x=angle,fill=time))+
  geom_density(alpha=0.5) +
  scale_x_continuous(limits = c(-1.5,1.5),breaks=c(-1.5,-1,-0.5,0,0.5,1,1.5))+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))
ggsave(file.path(save_path_temp, paste0(annotation,"_", number_sampled_tracks,"_tracks_",number_sampled_skeletons,"_skeleton_angles_density.png")),width=plot_size,height=plot_size)

skeletons_subsampled %>%
  mutate(angle_abs = abs(angle)) %>%
  ggplot(., aes(x=angle_abs,fill=time,..scaled..))+
  geom_density(alpha=0.5) +
  scale_x_continuous(limits = c(0,1.6),breaks=c(0,0.5,1,1.5))+
  theme_bw()+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))
ggsave(file.path(save_path_temp, paste0(annotation,"_", number_sampled_tracks,"_tracks_",number_sampled_skeletons,"skeleton_angles_absolute_values_density.png")),width=plot_size,height=plot_size)







#All vs all PC plots

save_path_temp <- file.path(save_path,"PC_correlations")
dir.create(save_path_temp)

plot_size = 5

#plot all PC_values against each other
PCs <- c("PC1","PC2","PC3","PC4","PC5","PC6")

for (j in PCs){
  for (k in PCs){

  #define plot
  density_plot <- PC_values %>%
    ggplot(., aes_string(x=j, y=k))+
    scale_x_continuous(breaks=c(-1.5,-1,-0.5,0,0.5,1,1.5))+
    scale_y_continuous(breaks=c(-1.5,-1,-0.5,0,0.5,1,1.5))+
    stat_density_2d(aes(fill = ..ndensity..), geom = "raster", contour = FALSE)+
    facet_wrap(vars(time))+
    theme_classic()+
    coord_equal()

  #rocket
  density_plot +
    scale_fill_viridis(option="rocket")
  ggsave(file.path(save_path_temp, paste0(annotation, "_", j, "_vs_", k, "_rocket.png")),width=plot_size, height=plot_size)

  #turbo
  density_plot +
    scale_fill_viridis(option="turbo")
  ggsave(file.path(save_path_temp, paste0(annotation, "_", j, "_vs_", k, "_turbo.png")),width=plot_size, height=plot_size)

  #viridis
  density_plot +
    scale_fill_viridis(option="turbo")
  ggsave(file.path(save_path_temp, paste0(annotation, "_", j, "_vs_", k, "_viridis.png")),width=plot_size, height=plot_size)

 }
}


for (m in PCs){
  for (n in PCs){

  #colored
  dot_plot <- PC_values %>%
    ggplot(., aes_string(x=m, y=n))+
      scale_x_continuous(breaks=c(-1.5,-1,-0.5,0,0.5,1,1.5))+
      scale_y_continuous(breaks=c(-1.5,-1,-0.5,0,0.5,1,1.5))+
      geom_point(aes(fill=time),alpha=0.01,shape=21)+
      facet_wrap(vars(time))+
      theme_classic()+
      coord_equal()+
      scale_fill_manual(values=c("#E69F00", "#56B4E9"))

  dot_plot
  ggsave(file.path(save_path_temp, paste0(annotation, "_", m, "_vs_", n, "_colored.png")),width=plot_size, height=plot_size)

  }
}

#Density distribution plots for PC_values

save_path_temp <- file.path(save_path,"PC_value_distris")
dir.create(save_path_temp)

plot_size = 5

#check if both timeranges have roughly equal number of skeletons
number_skeletons <- PC_values %>%
  group_by(time) %>%
  summarise(n=n())



PC_values %>%
  select(ID, time,matches("^PC1$|^PC2$|^PC3$|^PC4$|^PC5$|^PC6$")) %>%
  pivot_longer(!c(ID,time)) %>%
  ggplot(., aes(x=value,fill=time)) +
    geom_density(alpha=0.5,size=1)+
    theme_classic()+
    facet_wrap(vars(name))+
    scale_fill_manual(values=c("#E69F00", "#56B4E9"))
  ggsave(file.path(save_path_temp, paste0(annotation,"_density.png")),width=plot_size, height=plot_size)


#1D heatmap per PC for eigenvalue distribution


save_path_temp <- file.path(save_path,"PC_value_distris_heat")
dir.create(save_path_temp)

plot_size = 7

#here we divide in 50 equally spaced bins c(value,50)
#and heatmap based on this
PCs <- c("^PC1$","^PC2$","^PC3$","^PC4$","^PC5$","^PC6$")

#bin data (all PCs) into 50 equally spaced bins
 binned <- PC_values %>%
   select(ID, time,matches(PCs)) %>%
   pivot_longer(!c(ID,time)) %>%
   mutate(class = cut(value, 50))


  for (i in PCs){

   data_to_plot <- binned %>%
     select(ID, time,name, class, value) %>%
     filter(grepl(i, name)) %>%
     group_by(time, name,class) %>%
     summarise(n_class = n())

   ggplot(data_to_plot, aes(x=class,y=time,fill=n_class)) +
     geom_tile() +
     facet_wrap(vars(name),ncol=1) +
     scale_fill_viridis(option="viridis")+
     theme_classic()+
     coord_equal()+
     theme(axis.text.x = element_text(angle = 90))
   
   ggsave(file.path(save_path_temp, paste0(annotation,"_",i,"_density.png")),width=plot_size, height=plot_size)

  }

 
#generate overview videos/plot for one track (linear skeleton and PCs)

# save_path <- file.path(base_path,"plots","PC_video_one_track")
# dir.create(save_path)
# 
# #sample 1 track
# sampled_track <- PC_values %>%
#   filter(length_frames > 100) %>%
#   sample_n(1) %>%
#   pull(dataset_trackID)
# 
# 
# PC_values %>%
#   filter(dataset_trackID %in% sampled_track)
# 
# one_track <- skeleton_data_filtered2 %>%
#   filter(dataset_trackID %in% sampled_track) %>%
#   left_join(PC_values,by = intersect(names(skeleton_data_filtered2), names(PC_values))) %>%
#   ungroup()
# 
# ggplot(one_track, aes(x=PC1, PC2,color=seconds))+
#     scale_x_continuous(limits=c(-1.5,1.5))+
#     scale_y_continuous(limits=c(-1.5,1.5))+
#     geom_path()+
#     geom_point(size=1.5)+
#     facet_wrap(vars(time),ncol=1)+
#     scale_color_viridis(option="mako")+
#     theme_classic()+
#     coord_equal()
# 
# ggplot(one_track, aes(x=PC1, PC2,color=angle_velo))+
#     scale_x_continuous(limits=c(min(one_track$angle_velo),max(one_track$angle_velo)))+
#     scale_y_continuous(limits=c(min(one_track$angle_velo),min(one_track$angle_velo)))+
#     geom_path()+
#     geom_point(size=1.5)+
#     scale_color_viridis(option="turbo",limits=c(min(one_track$angle_velo),max(one_track$angle_velo)))+
#     # scale_color_viridis(option="turbo",limits=c(0,180))+
#     theme_classic()+
#     coord_equal()
# 
# 
# i<-0
# for (s in unique(one_track$seconds)){
#   
#   
#   plot_wave <- plot_wave_movie(one_track,s)+
#     theme(legend.position = "none")
#   plot_skeleton <- plot_skeleton_movie(one_track,"X","Y", s)+
#     theme(legend.position = "none")
#   plot_PCs <- plot_PCs_movie(one_track,s)
#   plot_PCs_linear <- plot_PCs_linear_movie(one_track,s)
#   
#   (plot_skeleton / 
#     plot_wave /
#     plot_PCs_linear)+
#   plot_layout(heights = c(0.5, 1,1))+
#   ggsave(file.path(save_path, paste0("PC_video_",paste(sampled_track,collapse="_"),"_",i , ".png")),width=5, height=10)
# 
#   
#   i <- i+ 1
#   
# }




#generate overview videos for two tracks (i.e. one from each timerange)

# 
# save_path <- file.path(base_path,"plots","PC_video_two_tracks")
# dir.create(save_path)
# 
# #sample 1 track per timepoint (early vs late)
# sampled_tracks <- PC_values %>%
#   filter(length_frames > 100) %>%
#   group_by(dataset_trackID,time) %>%
#   nest() %>%
#   group_by(time) %>%
#   sample_n(1) %>%
#   pull(dataset_trackID)
# 
# 
# two_tracks <- skeleton_data_filtered2 %>%
#   filter(dataset_trackID %in% sampled_tracks) %>%
#   left_join(PC_values,by = intersect(names(skeleton_data_filtered2), names(PC_values)))
# 
# ggplot(two_tracks, aes(x=PC1, PC2,color=seconds))+
#     scale_x_continuous(limits=c(min(two_tracks$PC1),max(two_tracks$PC1)))+
#     scale_y_continuous(limits=c(min(two_tracks$PC2),max(two_tracks$PC2)))+
#     geom_path()+
#     geom_point(size=1.5)+
#     facet_wrap(vars(time),ncol=1)+
#     scale_color_viridis(option="mako")+
#     theme_classic()+
#     coord_equal()
# 
# i<-0
# for (s in unique(two_tracks$seconds)){
#   
#   
#   plot_wave <- plot_wave_movie(two_tracks, s)
#   # plot_wave
#   
#   plot_skeleton <- plot_skeleton_movie(two_tracks,"X","Y", s)
#   # plot_skeleton
#   
#   plot_PCs <- plot_PCs_movie(two_tracks,s)
#   # plot_PCs
#   
#   plot_skeleton +
#     plot_PCs +
#     ggsave(file.path(save_path, paste0("PC_video_",paste(sampled_tracks,collapse="_"),"_",i , ".png")))
#   
#   i <- i+ 1
#   
# }



# "2020-06-22_17-58-37_21_track_133"
#"2020-06-25_17-58-07_20_track_216"
#"2020-06-23_17-58-40_20_track_97"
#"2020-06-22_17-58-37_17_track_28"

#"2020-06-25_17-58-07_8_track_77" #late type

#"2020-06-22_17-58-37_57_track_47" 
# "2020-06-23_17-58-40_82_track_48"

# skeleton_data_filtered2 %>%
#   mutate(dataset_trackID = paste0(dataset_ID,"_", tp,"_", TrackID)) %>%
#   filter(dataset_trackID %in% sampled_tracks) %>%
#   plot_wave()

# 
# early_s <- simulate(0.2,2,60,0.05,1) %>%
#   mutate(time = "early")
# 
# late_s <- simulate(0.4,2,60,0.05,1) %>%
#   mutate(time = "late")
# 
# 
# two_simulated <- rbind(early_s,late_s) %>%
#   #define initial values needed for estimating position from angles
#   mutate(new_angle=0, X=1, Y=1) %>%
#   group_by(time,seconds) %>%
#   #estimate the positions for each clusterID independently
#   group_modify(~ estimate_positions_from_angles(.x)) %>%
#   #turn the coordinates so that the worm's orientations are similar
#   group_modify(~ turn_worm(.x)) %>%
#   select(-new_angle) %>%
#   ungroup()
# 
# i<-0
# for (s in unique(two_simulated$seconds)){
# 
#   plot_skeleton_movie(two_simulated,"X","Y", s) +
#     facet_wrap(vars(dataset_trackID),ncol=2)+
#     ggtitle(s)+
#     ggsave(file.path(save_path, paste0(i, ".png")))
#   i <- i + 1 
# }

# 
# 
# #sample 1 track per timepoint (early vs late)
# two_tracks_sim <- PC_values_sim %>%
#   group_by(dataset_trackID,time) %>%
#   nest() %>%
#   group_by(time) %>%
#   sample_n(1) %>%
#   unnest(cols=c(data)) %>%
#   #bring back angles
#   left_join(simulated_m,by = intersect(names(simulated_m), names(PC_values_sim))) %>%
#   #define initial values needed for estimating position from angles
#   mutate(new_angle=0, X=1, Y=1) %>%
#   group_by(dataset_trackID,seconds) %>%
#   #estimate the positions for each clusterID independently
#   group_modify(~ estimate_positions_from_angles(.x)) %>%
#   #turn the coordinates so that the worm's orientations are similar
#   group_modify(~ turn_worm(.x)) %>%
#   select(-new_angle) %>%
#   ungroup()
# 
# 
# 
# ggplot(two_tracks_sim, aes(x=PC1, PC2,color=seconds))+
#     scale_x_continuous(limits=c(min(two_tracks$PC1),max(two_tracks$PC1)))+
#     scale_y_continuous(limits=c(min(two_tracks$PC2),max(two_tracks$PC2)))+
#     geom_path()+
#     geom_point(size=1.5)+
#     facet_wrap(vars(time),ncol=1)+
#     scale_color_viridis(option="mako")+
#     theme_classic()+
#     coord_equal()
# 
# i<-0
# for (s in unique(two_tracks_sim$seconds)){
#   
#    
#   plot_skeleton <- plot_skeleton_movie(two_tracks_sim,"X","Y", s) +
#     theme_void()+
#     facet_wrap(vars(time),ncol=1)+
#     theme(panel.spacing.y = unit(15, "lines"))
#   
#   # plot_skeleton
#   
#   plot_PCs <- plot_PCs_movie(two_tracks_sim,s)
#   # plot_PCs
#   
#   plot_skeleton +
#     plot_PCs +
#     ggsave(file.path(save_path, paste0("PC_video_sim_",i , ".png")))
#   
#   i <- i + 1
#   
# }


# 
# PC_values %>%
#   filter(dataset_trackID %in% sampled_tracks) %>%
#   ggplot(.,aes(x=numbering))+
#     geom_line(aes(y=PC1),color="red")+
#     geom_line(aes(y=PC2),color="lightblue")+
#     geom_line(aes(y=PC3),color="gold")+
#     facet_wrap(vars(dataset_trackID),ncol = 5)+
#   ggsave(file.path(base_path,"test.png"),height=10,width=10)
# 
# 
# sampled_tracks <- sample(unique(PC_values_v$dataset_trackID),1)
# PC1 <- PC_values_v %>%
#   filter(dataset_trackID %in% sampled_tracks)
# PC1 <- ggplot(PC1, aes(x=numbering, y=PC1))+
#   geom_line()+
#   ggtitle(PC1$ID) +
#   scale_y_continuous(limits=c(-1.5,1.5))
# 
# 
# 
# PC2 <- PC_values_v %>%
#   filter(dataset_trackID %in% sampled_tracks) %>%
#   ggplot(., aes(x=numbering, y=PC2))+
#     geom_line() +
#     scale_y_continuous(limits=c(-1.5,1.5))
# 
# 
# PC3 <- PC_values_v %>%
#   filter(dataset_trackID %in% sampled_tracks) %>%
#   ggplot(., aes(x=numbering, y=PC3))+
#     geom_line() +
#     scale_y_continuous(limits=c(-1.5,1.5))
# 
# velocity <- PC_values_v %>%
#   filter(dataset_trackID %in% sampled_tracks) %>%
#   ggplot(., aes(x=numbering, y=velocity))+
#     geom_line()
# 
# 
# 
# 
# PC1 / PC2 / PC3 / velocity +
#   ggsave(file.path(base_path,"test.png"),height=7)
# 
# hist(PC_values$Duration_of_track)

