# Use this script after running the "filter_skeletons.R" script that filters skeletons for a given condition.
# Needed: "...skeletons_filtered.rds"
# as computed by the "filtered_skeletons.R" script
# please also speficy the dataset you want to analyse/plot and the conversion factor.
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




#define conversion factor
conversion_factor <- as.numeric(commandArgs(trailingOnly = TRUE)[3])

#parameters for selecting only dispersing worms
velocity_limit <- 0.01
angle_limit <- 50
length_track_limit <- 10 # minimum frames

#get files that have to be loaded
file_to_process <- grep(paste0(".+\\/",selected_annotation,"\\_skeletonized_filtered.rds"),list.files(base_path,full.names = TRUE),value=TRUE)


#loading data
cat(paste0("\n\nloading dataset ", file_to_process,"\n\n"))
skeleton_data_filtered <- file_to_process %>%
    map_df(., function(x) readRDS(x))

#this is a workaround for attaching the correct offset times (i.e. minutes worms have spent on the plate prior to imaging) to the data
#we open a previously exported txt file, specific for each annotation
#ideally, this should be done in the skeletonization script
cat(paste0("\n\nfetching offsets for ",file_to_process,"\n\n"))

#function for fetching offsets from grouped raw RDS file
read_offset <- function(x,y){
  file_path <- file.path(dataraw_file_path, paste0(x,"_timeoffsets.txt"))
  cat(paste0("\n\nFetching offsets from ", file_path))
  temp <- read_csv(file_path,show_col_types = FALSE)
  return(temp)
}

offsets <- read_offset(selected_annotation)

#new skeleton data filtered table now with offsets
skeleton_data_filtered_wf <- skeleton_data_filtered %>%
  left_join(offsets,by="dataset_ID") %>%
  #accounting for offsets
  mutate(minutes = time_elapsed + ((tp-1)*timepoint_length+(tp-1)*timestep_length)) %>%
  mutate(hours_rounded = round(minutes/60)) %>%
  #filter out first 30 mins
  filter(hours_rounded > 0) %>%
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
skeleton_data_filtered2 <- skeleton_data_filtered_wf %>%
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
  mutate(dataset_trackID = paste0(dataset_ID,"_", tp,"_", TrackID))
  
# saveRDS(skeleton_data_filtered2,file.path(base_path, paste0(selected_annotation,"_skeletonized_filtered_dispersal.rds")))






  
#How many tracks per hours_rounded?
number_tracks_per_hour <- skeleton_data_filtered2 %>%
  group_by(dataset_trackID,hours_rounded) %>%
  nest() %>%
  group_by(hours_rounded) %>%#
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
  # group_by(time, dataset_trackID) %>%
  # nest() %>%
  # #sample tracks per timerange (variable: number_sampled_tracks)
  # group_by(time) %>%
  # sample_n(number_sampled_tracks) %>%
  # unnest(cols=c(data)) %>%
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



# saveRDS(PC_values_sub, file.path(base_path,paste0(annotation,"_", number_sampled_tracks,"_tracks_",number_sampled_skeletons,"_skeletons_PC_values.RDS")))

cat("\n\nnow plotting eigenworms.\n\n")

save_path_temp <- file.path(save_path,"eigenworms")
dir.create(save_path_temp)

plot_size = 5

six_eigenworms <- melt(pca$rotation[,1:6]) %>%
  rename(index = 1, eigenworm = 2, angle = 3)

plot_eigenworms <- six_eigenworms %>%
   ggplot(., aes(index,angle)) +
   geom_point(size=2)+
   geom_path(size=2,lineend="round")+
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
ggsave(file.path(save_path_temp,paste0(annotation,"_","eigenworms_with_contributions.png")),height=10,width=20)


cat("\n\nnow plotting PCA plots.\n\n")

save_path_temp <- file.path(save_path,"PC_correlations")
dir.create(save_path_temp)

plot_size = 5

#plot all PC_values against each other
# PCs <- c("PC1","PC2","PC3","PC4","PC5","PC6")
#only first three PCs
PCs <- c("PC1","PC2","PC3")


for (j in PCs){
  for (k in PCs){
    
    #define plot
    density_plot <- PC_values %>%
      ggplot(., aes_string(x=j, y=k))+
      scale_x_continuous(breaks=c(-1.5,-1,-0.5,0,0.5,1,1.5))+
      scale_y_continuous(breaks=c(-1.5,-1,-0.5,0,0.5,1,1.5))+
      stat_density_2d(aes(fill = ..ndensity..), geom = "raster", contour = FALSE)+
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
      scale_fill_viridis(option="viridis")
    ggsave(file.path(save_path_temp, paste0(annotation, "_", j, "_vs_", k, "_viridis.png")),width=plot_size, height=plot_size)
    
  }
}