# Use this script for plotting motion modes and comparisons between timepoints and conditions.
# Needed: "...centroid_tracking_bins.RDS" as written by the "motion.R" script

# To run the script type: Rscript plots_motion_modes.R "the location of your motion data folder" 
# e.g. Rscript plots_motion_modes.R /Users/fpreuss/Desktop/data/motion/

#for everything good
library(tidyverse)
#for colors
library(wesanderson)
library(viridis)
#for wilcox test in tidy
library(broom)
#for statistic testing
library(rstatix)


############### functions ###############
#1 Plotting functions
se <- function(x) sqrt(var(x)/length(x))

plot_mean_w_error <- function(data_to_plot, X, Y,colored_by){
  ggplot(data_to_plot,aes_string(x = X ,y =Y,color=colored_by,fill=colored_by,group=colored_by)) +
    geom_ribbon(aes_string(ymin="minse", ymax="maxse",group=colored_by),fill="lightgray", color="lightgray", alpha=.8)+
    geom_line(size=1.2)+
    geom_point(shape=21,size=2.5,color="black")+
    scale_x_continuous(breaks=(seq(1,show_until)))+
    theme_bw()+
    labs(x="time (hours)", "relative fraction")+
    theme(strip.background = element_rect(colour = "white", fill = "white"),
          strip.text.x = element_text(size=20,colour = "black", face = "bold"),
          axis.text.x = element_text(size=15, face="bold"),
          axis.title.x = element_text(size=20, face="bold"),
          axis.text.y = element_text(size=15, face="bold"),
          axis.title.y = element_text(size=20, face="bold"),
          axis.line = element_line(colour = 'black', size = 1.2),
          axis.ticks = element_line(colour = "black", size = 1.2))+
    scale_fill_manual(values = pal)+
    scale_color_manual(values = pal)
}



plot_omega <- function(input_data, selected_mode, selected_annotations){
  data_bsf <<- input_data %>%
    filter(annotation %in% selected_annotations) %>%
    filter(mode %in% selected_mode) %>%
    mutate(annotation = factor(annotation, levels = selected_annotations)) %>%
    arrange(annotation) 
  
  data_bsfs <- data_bsf %>%
    group_by(annotation,hours_rounded) %>%
    summarise(
      sd = sd(perc_omega, na.rm=TRUE),
      # sem = se(perc),
      perc_omega = median(perc_omega)
    )
  
  ggplot(data_bsf,aes(x=time,y=perc_omega,fill=annotation))+
    geom_boxplot(width=0.5,lwd=1.2)+
    # geom_violin()+
    # geom_point(position = position_jitterdodge(jitter.width=0.25),color="black",shape=21,size=2.5,alpha=0.5)+
    # geom_pointrange(data = data_bsfs,aes(ymin = perc_omega-sd, ymax = perc_omega+sd),color="black",position = position_dodge(0.75))+
    scale_fill_manual(values=pal)+
    xlab("hours") + ylab(paste0("fraction omega turns"))+
    theme_classic()+
    theme(
      legend.direction="horizontal",
      legend.position = "top",
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
}


################# load data #########################

#define base path  
base_path <- commandArgs(trailingOnly = TRUE)[1]
#define save path
save_path <- file.path(dirname(base_path), "plots", "motion")
dir.create(save_path,recursive = TRUE)


#pixel to µm
conversion_factor <- 6.25
#fps we downsampled to (in KNIME)
downsampled_to = 2
#lower velocity limit for mode == roaming (mm/s)
velocity_r <- 0.01
#upper angle limit for mode == roaming (mm/s)
angle_r <- 15 
# upper velocity limit for mode == still (mm/s)
velocity_s <- 0.001

#list of centroid tracking .rds files in data folder
files_to_process <- list.files(base_path, "tracked",full.names = TRUE, ignore.case = TRUE)

#Until how many hours
show_until <- 12

data <- map_dfr(files_to_process,readRDS) %>%
  mutate(hours = minutes / 60) %>%
  #round per half an hour
  # mutate(hours_rounded= floor(hours * 2) / 2) %>%
  #round to next hour up
  # mutate(hours_rounded = ceiling(hours)) %>%
  mutate(hours_rounded = round(hours)) %>%
  #filter out first 30 mins
  filter(hours > 0) %>%
  mutate(annotation = gsub("\\s", "_",annotation))%>% 
  na.omit() %>%
  # scale velocity to mm/s
  mutate(p_mean_velocity = p_mean_velocity / 1000) %>%
  group_by(annotation, hours_rounded) %>%
  filter(hours_rounded <= show_until) %>%
  #count tracks per hour per condition
  mutate(number_of_tracks_in_this_hour = n_distinct(dataset_ID,tp,TrackID)) %>%
  ungroup()





data_binarized <- data %>%
  mutate(mode = ifelse(p_mean_velocity > velocity_r & p_mean_angle < angle_r,"roaming","dwelling")) %>%
  mutate(mode=ifelse(p_mean_velocity < velocity_s, "still", mode)) %>%
  group_by(dataset_ID, tp, TrackID, binning) %>%
  #if there is at least one omega turn in a bin, make it 1 otherwise 0
  mutate(omega = ifelse(p_omega_rf > 0, 1, 0)) %>%
  group_by(dataset_ID, tp, TrackID) %>%
  mutate(distance_per_track = sum(p_traveled_distance)) %>%
  ungroup()

#here we will summarise by tp (==video timestep)
#so later in the plots one dot = one (in our case 8 mins long) video 
data_bs <- data_binarized %>%
  group_by(annotation,hours_rounded,tp,mode,dataset_ID) %>%
  #summarise over tp
  #this will be the mean of the bins contained within that tp and mode
  #counts is the number of bins of a certain mode
  #mean_omega is the number of bins with at least one detected omega turn divided by the total number of bins
  summarise(counts=n(),mean_velocity = mean(p_mean_velocity), mean_angle = mean(p_mean_angle),perc_omega = sum(omega)/n()) %>%
  group_by(annotation,hours_rounded,tp,dataset_ID) %>%
  #total counts
  mutate(sum=sum(counts)) %>%
  #relative occurence of mode
  mutate(perc = counts/sum) %>%
  ungroup()

# here we will summarise per track
# so later in the plots one dot = one track
data_bst <- data_binarized %>%
  group_by(annotation,hours_rounded,tp,TrackID,dataset_ID,mode,binning,omega) %>%
  #summarise over individual tracks
  #this will be the mean of the bins contained within that track
  #mean_omega is the mean number of tracks with at one omega turn, normalized per s
  summarise(mean_velocity = mean(p_mean_velocity), mean_angle = mean(p_mean_angle)) %>%
  ungroup()

# unique(data$annotation)




############### plot modes ###############

save_path_temp <- file.path(save_path,"modes")
dir.create(save_path_temp)


##### line plots #####

selected_annotations <- c("Agar","OP50_w_Az","OP50", "HB101")
selected_mode <- c("roaming")
pal <- wes_palette("Darjeeling1")[c(1,2,5,5)]
data_bs %>%
  filter(annotation %in% selected_annotations) %>%
  arrange(match(annotation, selected_annotations)) %>%
  mutate(annotation=factor(annotation, levels=selected_annotations)) %>%
  group_by(hours_rounded,annotation,mode) %>%
  summarise(mean_perc= mean(perc), maxse=mean(perc)+sd(perc),minse=mean(perc)-sd(perc)) %>%
  filter(mode %in% selected_mode) %>%
  plot_mean_w_error(., "hours_rounded", "mean_perc","annotation") +
  facet_wrap(vars(annotation),ncol=1)+
  scale_y_continuous(limits=c(0,NA))
ggsave(file.path(save_path_temp,paste0("modes_relative_per_mode_seperate_sd.png")),height=7,width=4,dpi=600)


##### range plots #####

selected_annotations <- c("Agar","OP50_w_Az","OP50", "HB101")
selected_mode <- c("roaming")
pal <- wes_palette("Darjeeling1")[c(1,2,5,4)]
selected_tps <- c(1,3,6,9,12)

data_temp <- data_bs %>%
  filter(annotation %in% selected_annotations) %>%
  arrange(match(annotation, selected_annotations)) %>%
  mutate(annotation=factor(annotation, levels=selected_annotations)) %>%
  filter(mode == selected_mode) %>%
  filter(hours_rounded %in% selected_tps)


annotations <- unique(data_temp$annotation)
i <- 1
pvalues <- NULL

while (i < length(selected_tps)) {
  for (ann in annotations){
    # print(c(selected_tps[i],selected_tps[i+1]))
    p_temp <- data_temp %>%
      filter(annotation == ann) %>%
      filter(hours_rounded %in% c(selected_tps[i],selected_tps[i+1])) %>%
      wilcox_test(perc ~ hours_rounded,exact=FALSE) %>%
      add_significance() %>%
      mutate(hours_compared= paste0(selected_tps[i], "vs", selected_tps[i+1])) %>%
      mutate(annotation = ann)
    pvalues <- rbind(pvalues, p_temp)
  }
  i <- i+1
}

write.csv2(pvalues,file=file.path(save_path_temp,paste0("dispersal_fraction_wilcox.txt")))

  
data_to_plot <- data_temp %>% 
  mutate(hours_rounded = as.character(hours_rounded)) %>%
  mutate(hours_rounded = factor(hours_rounded, levels = c("1","3","6","9","12"))) %>%
  group_by(annotation, hours_rounded) %>%
  summarise(mean_perc= mean(perc), maxsd=mean(perc)+sd(perc),minsd=mean(perc)-sd(perc))


ggplot(data_to_plot, aes(x=hours_rounded, y=mean_perc,color=annotation))+
  geom_pointrange(aes(ymin=minsd, ymax=maxsd),position = position_dodge(1),size=1.5)+
  scale_y_continuous(limits=c(0,1.1),breaks=c(0,0.25,0.5,0.75,1))+
  theme_bw()+
  labs(x="time (hours)", "relative fraction")+
  theme(strip.background = element_rect(colour = "white", fill = "white"),
        strip.text.x = element_text(size=20,colour = "black", face = "bold"),
        axis.text.x = element_text(size=15, face="bold"),
        axis.title.x = element_text(size=20, face="bold"),
        axis.text.y = element_text(size=15, face="bold"),
        axis.title.y = element_text(size=20, face="bold"),
        axis.line = element_line(colour = 'black', size = 1.2),
        axis.ticks = element_line(colour = "black", size = 1.2))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)

ggsave(file.path(save_path_temp,paste0("dispersal_fraction_sd_rangeplots.png")),height=6,width=8,dpi=600)

####################################################


################# plots omega turns #######################
#compare omega turn occurences across conditions

save_path_temp <- file.path(save_path,"omega_turns")
dir.create(save_path_temp)

selected_annotations <- c("Agar","OP50","HB101")
selected_hours <- c(1,9)
selected_mode <- c("roaming")
pal <- wes_palette("Darjeeling1")[c(1,5,4)]

data_bs %>%
  filter(hours_rounded %in% selected_hours) %>%
  mutate(time=ifelse(hours_rounded <= 3,"early",NA)) %>%
  mutate(time=ifelse(hours_rounded > 6, "late",time)) %>%
  plot_omega(., selected_mode,selected_annotations)+
  geom_signif(comparisons = list(c("early", "late")))
ggsave(file.path(save_path_temp,paste0("omega_median",paste(selected_annotations, collapse="_"),"_",paste(selected_hours, collapse="_"),".png")),height=5,width=5,dpi=600)

############################################


################# plots mean velocity #######################
save_path_temp <- file.path(save_path,"mean_velocity")
dir.create(save_path_temp)


selected_annotations <- c("Agar", "HB101")
selected_hours <- c("12","3")
pal <- wes_palette("Darjeeling1")[c(1,4)]

data_temp <- data_bst %>%
  mutate(hours_rounded = as.character(hours_rounded)) %>%
  filter(hours_rounded %in% selected_hours) %>%
  mutate(hours_rounded = factor(hours_rounded, levels = selected_hours)) %>%
  filter(annotation %in% selected_annotations)
  
#calculate significances for both timepoints seperately
first_hour_pvalue <- filter(data_temp,hours_rounded == selected_hours[1]) %>%
  wilcox_test(mean_velocity ~ annotation,detailed = TRUE) %>%
  add_significance()

second_hour_pvalue <- filter(data_temp,hours_rounded == selected_hours[2]) %>%
  wilcox_test(mean_velocity ~ annotation,exact=FALSE,detailed = TRUE) %>%
  add_significance()

write.csv2(rbind(first_hour_pvalue,second_hour_pvalue),file=file.path(save_path_temp,paste0("tracks_mean_velocity.txt",paste(selected_annotations, collapse="_"),"_",paste(selected_hours, collapse="_"),".txt")))

ggplot(data_temp,aes(y=factor(hours_rounded, levels = selected_hours),x=mean_velocity,fill=annotation))+
    geom_violin(color=NA,alpha=0.5)+
    geom_boxplot(width=0.1,size=0.5,position=position_dodge(0.9),outlier.shape = NA)+
    scale_x_log10(limits=c(NA,0.3))+
    theme_bw()+
    theme(strip.background = element_rect(colour = "white", fill = "white"),
        strip.text.x = element_text(size=20,colour = "black", face = "bold"),
        axis.text.x = element_text(size=15, face="bold"),
        axis.title.x = element_text(size=20, face="bold"),
        axis.text.y = element_text(size=15, face="bold"),
        axis.title.y = element_text(size=20, face="bold"),
        axis.line = element_line(colour = 'black', size = 1.2),
        axis.ticks = element_line(colour = "black", size = 1.2))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)
ggsave(file.path(save_path_temp,paste0("tracks_mean_velocity",paste(selected_annotations, collapse="_"),"_",paste(selected_hours, collapse="_"),".png")),height=4,width=7,dpi=600)

############################################
