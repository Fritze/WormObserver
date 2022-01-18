# Use this script after running the "cluster_skeletons.R" script that clusters skeletons for a given condition.
# Needed: "...cluster_centers_reduced.RDS" &
          "..._clustering.RDS" &
          "...skeletons_filtered_clustered.RDS"
#as computed by the "cluster_skeletons.R" script

# To run the script type: Rscript plots_postures.R "the location of your clustered skeletonized data folder" 
# e.g. Rscript plots_postures.R /Users/fpreuss/Desktop/data/skeletonized/clustered/

library(tidyverse)

#define base path  
base_path <- commandArgs(trailingOnly = TRUE)[1]
#define save path
save_path <- file.path(dirname(dirname(dirname(base_path))), "plots", "postures")
dir.create(save_path,recursive = TRUE)

#list of datasets that have been clustered and will be plotted now
datasets_to_process <- gsub("(.+)\\_clustering.+","\\1",basename(list.files(base_path, "_clustering.rds", full.names = TRUE, ignore.case = TRUE)))


i <- datasets_to_process[1]


for (i in datasets_to_process){
  
  
  skeletons_filtered_clustered_filepath <-file.path(base_path,paste0(i,"_skeletons_filtered_clustered.RDS"))
  cluster_centers_reduced_filepath <- file.path(base_path,paste0(i,"_cluster_centers_reduced.RDS"))
  
  #load data
  skeleton_data_clustered <- skeletons_filtered_clustered_filepath %>%
    readRDS(.)
  
  cluster_centers_reduced <- cluster_centers_reduced_filepath %>%
    readRDS(.)
  
  cat(paste0("plotting ",unique(skeleton_data_clustered$annotation,"\n")))
  
  
  #add clusters to initial skeleton dataset that was used for kmeans clustering
  #also calculate relative occurences of postures (i.e. clusters) both total and per annotation
  freq_postures <- skeleton_data_clustered %>%
    #select only relevant columns
    select(newID,annotation) %>%
    #do this now for every newID and annotation independently
    group_by(newID, annotation) %>%
    summarise(counts_annotation = n()) %>%
    #do this now for every annotation independently
    group_by(annotation) %>%
    #occurences of all clusters together == total_annotation
    mutate(total_annotation = sum(counts_annotation)) %>%
    mutate(perc_annotation = counts_annotation/total_annotation * 100) %>%
    group_by(newID) %>%
    mutate(counts_all = sum(counts_annotation)) %>%
    mutate(total_all = sum(total_annotation)) %>%
    ungroup() %>%
    mutate(perc_all = counts_all/total_all * 100)
  
  
  #calculate posture occurences
  freq_postures_overall <- freq_postures %>%
    group_by(newID,perc_all) %>%
    summarise() %>%
    arrange(desc(perc_all)) %>%
    ungroup() %>%
    mutate(rank_all = 1:n())
  
  ggplot(freq_postures_overall,aes(x=rank_all,y=perc_all))+
    geom_col()+
    labs(x="Posture Rank", y="Probability (%)")+
    theme_linedraw()
  
  
  #order by overall occurence
  order_overall <- freq_postures_overall$newID
  
  
  
  #Calculate relative occurence of postures per hour
  max_hours_rounded <- 12
  
  angle_data_postures_first <-  skeleton_data_clustered %>%
    #15 mins as average offset
    mutate(minutes = 15 + ((tp-1)*8+(tp-1)*0)) %>%
    #round to half hour steps
    mutate(hours_rounded = ceiling(minutes/60*2)/2) %>%
    #round to full hour steps
    # mutate(hours_rounded = ceiling(minutes/60)) %>%
    mutate(dataset_TrackID =  paste0(dataset_ID,"_",tp,"_",TrackID)) %>%
    ###
    # group_by(annotation,dataset_TrackID,hours_rounded) %>%
    # nest() %>%
    # group_by(annotation,hours_rounded) %>%
    # sample_n(80) %>%
    # unnest(cols=c(data)) %>%
    ## group_by(annotation,hours_rounded,dataset_TrackID) %>%
    ## summarise(n_tracks = n_distinct(dataset_TrackID),n_total = n())
    # group_by(annotation,ID,dataset_TrackID,hours_rounded) %>%
    # nest() %>%
    # group_by(annotation,hours_rounded,dataset_TrackID) %>%
  ## summarise(n=n_distinct(ID))
  # sample_n(20) %>%
  # unnest(cols=c(data)) %>%
  # group_by(annotation,hours_rounded) %>%
  # summarise(n_tracks = n_distinct(dataset_TrackID),n_postures = n_distinct(ID))
  ###
  #reduce dataset
  group_by(ID,newID,annotation,hours_rounded,dataset_ID,dataset_TrackID) %>%
    summarise() %>%
    #do this for every newID, timepoint and annotation
    group_by(newID,hours_rounded,annotation) %>%
    #number of clusters per timepoint and experiment
    mutate(number_of_cluster_members_per_tp=n_distinct(ID)) %>%
    group_by(newID,hours_rounded,annotation,dataset_ID) %>%
    #number of clusters per timepoint and experiment
    mutate(number_of_cluster_members_per_tp_per_dataset=n_distinct(ID)) %>%
    arrange(newID) %>%
    #do this for every timepoint (=hours_rounded) and annotation
    group_by(hours_rounded,annotation) %>%
    #total number of postures per timepoint and annotation
    mutate(postures_per_tp=n_distinct(ID)) %>%
    #total number of tracks per timepoint and annotation
    mutate(tracks_per_tp=n_distinct(dataset_TrackID)) %>%
    #percentage per annotation
    mutate(perc = number_of_cluster_members_per_tp/postures_per_tp) %>%
    #do this for every timepoint and annotation
    group_by(hours_rounded,dataset_ID) %>%
    #total number of postures per timepoint and dataset_ID
    mutate(postures_per_tp_per_dataset=n_distinct(ID)) %>%
    #total number of tracks per timepoint and dataset_ID
    mutate(tracks_per_tp_per_dataset=n_distinct(dataset_TrackID)) %>%
    #percentage per dataset_ID  
    mutate(perc_per_dataset = number_of_cluster_members_per_tp_per_dataset/postures_per_tp_per_dataset) %>%
    group_by_at(vars(-ID,-dataset_TrackID)) %>%
    summarise() %>%
    ungroup()
  
  #Posture quantified over time per annotation (i.e. type of experiment)
  angle_data_postures <- angle_data_postures_first %>%
    select(-dataset_ID) %>%
    select(-contains("per_dataset")) %>%
    group_by_all() %>%
    summarise() %>%
    #do this for every cluster and annotation  (i.e. over all timepoints)
    group_by(newID,annotation) %>%
    mutate(sd_perc = sd(perc)) %>%
    mutate(rM = rollmean(perc, 2, fill=NA)) %>%
    mutate(sd_rM = sd(rM,na.rm = TRUE)) %>%
    mutate(mean_perc = mean(perc)) %>%
    mutate(mean_rM = mean(rM, na.rm=TRUE)) %>%
    mutate(perc_norm = (perc-mean_perc)/sd_perc) %>%
    mutate(rM_norm = (rM-mean_rM)/sd_rM) %>%
    ungroup() %>%
    filter(hours_rounded <= 12) %>%
    arrange(newID,hours_rounded,annotation) %>%
    group_by(hours_rounded,annotation) %>%
    mutate(tracks_per_hours_rounded = first(tracks_per_tp))
  
  #Posture quantified over time per dataset_ID (i.e. per experiment replicate)
  angle_data_postures_per_dataset <- angle_data_postures_first %>%  
    #do this for every cluster and dataset_ID (not grouped as annotation)  (i.e. over all timepoints)
    group_by(newID,dataset_ID) %>%
    mutate(sd_perc_per_dataset = sd(perc_per_dataset)) %>%
    mutate(rM_per_dataset = rollmean(perc_per_dataset, 2, fill=NA)) %>%
    mutate(sd_rM_per_dataset = sd(rM_per_dataset,na.rm = TRUE)) %>%
    mutate(mean_perc_per_dataset = mean(perc_per_dataset)) %>%
    mutate(mean_rM_per_dataset = mean(rM_per_dataset, na.rm=TRUE)) %>%
    mutate(perc_norm_per_dataset = (perc_per_dataset-mean_perc_per_dataset)/sd_perc_per_dataset) %>%
    mutate(rM_norm_per_dataset = (rM_per_dataset-mean_rM_per_dataset)/sd_rM_per_dataset) %>%
    ungroup() %>%
    filter(hours_rounded <= 12) %>%
    arrange(newID,hours_rounded,annotation,dataset_ID)
  
  annotation <- unique(angle_data_postures$annotation)
  
  
  
  #Plot heatmaps for relative occurence of posture
  #for each dataset will be ploted:
  #  Heatmap combining all datasets with posture overview (in right order after hierarchical clustering of temporally resolved frequencies)
  #  Individual heatmaps for each dataset and also for both datasets together
  #  All of the two above with different ways of calculating frequencies:
  #  1- frequency
  #  2- frequency, normalized (z-score)
  #  3- frequency, normalized (z-score) with rolling mean
  
  #create specific folder location that will be used to save the heatmaps
  save_path <- file.path(base_path,"plots","posture","heatmaps")
  dir.create(save_path)
  
  #defines the skeleton example plot that will appear below the heatmap
  plot_posture_examples <- function(data_to_plot,X,Y){
    ggplot(data_to_plot, aes_string(x=X,y=Y,color="angle"))+
      geom_point(size=3.5) +
      geom_path(size=2.5,lineend = "round") +
      theme_void()+
      coord_fixed(ratio = 1)+
      scale_color_gradient2(low = "deepskyblue", mid = "lavender",
                            high = "deeppink",midpoint=0,limits=c(-1,1),na.value="grey") +
      # scale_color_viridis(option = "plasma",limits=c(-1,1))+
      theme(plot.title = element_text(face = "bold"))
  }
  
  #get all annotations
  annotations <- unique(angle_data_postures$annotation)
  #define the normalizations underlying the heatmap plots
  normalizations <- c("perc_norm")
  
  
  for (a in annotations){
    for (n in normalizations){
      #prepare clustering
      temporal_clustering <- angle_data_postures %>%
        filter(annotation %in% a[1]) %>%
        na.omit() %>%
        dcast(newID ~ hours_rounded,value.var = n) %>%
        tibble::column_to_rownames(var = "newID")
      
      #cluster the temporal posture patterns with the current normalization
      temporal_clustering <- temporal_clustering[order(as.numeric(rownames(temporal_clustering))),,drop=FALSE]
      #this ordering from the hclsut is the rowID, it is NOT the actual posture ID
      ordering <- hclust(dist(temporal_clustering, method = "euclidean"), method = "average")$order
      #get posture ID by their rownumber
      #and append a new ordering ("newID2") so that posture numbers correspond to their position in the clustering
      ordering <- data.frame(as.numeric(rownames(temporal_clustering)[ordering]),1:length(ordering))
      colnames(ordering) <- c("newID","newID2")
      
      #generate heatmap data
      #normalization column will be the one for the current normalization
      heatmap_data_to_plot <- angle_data_postures %>%
        #append newID2
        left_join(ordering, by=c("newID")) %>%
        filter(annotation %in% a) %>%
        rename(normalization =  all_of(n)) %>%
        ungroup() %>%
        mutate(hours_rounded_w_numbers = paste0(hours_rounded, " (",tracks_per_hours_rounded,")"))
      
      #check if the current selected annotation(s) is one or several
      #if only one annotation then we plot hours_rounded_w_numbers as y axis
      if(length(a) == 1){
        #generate heatmap
        #x-axis will be newID2 (as character), but with sorted order
        heatmap <- ggplot(heatmap_data_to_plot,aes(x=factor(as.character(newID2),levels=sort(unique(heatmap_data_to_plot$newID2))),y=factor(hours_rounded_w_numbers,levels=unique(hours_rounded_w_numbers))))
      }else{
        #if multiple annotations we facet_wrap by annotation and plot hours_rounded as y to have a common y axis between annotations
        heatmap <- ggplot(heatmap_data_to_plot,aes(x=factor(as.character(newID2),levels=sort(unique(heatmap_data_to_plot$newID2))),y=factor(hours_rounded,levels=unique(hours_rounded))))
      }
      heatmap <- heatmap +  
        theme_classic()+
        labs(x="posture IDs",y = "hours",fill="Z-score") +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
        theme(axis.text.y = element_text(size = 30,face = "bold"),
              axis.text.x = element_text(size = 30),
              axis.title.x = element_text(size=45),
              axis.title.y = element_text(size=45),
              strip.text = element_text(size=45),
              legend.text=element_text(size=50),
              legend.title = element_text(size=50),
              legend.key.size = unit(2, "cm"),
              legend.position = "right",
              legend.direction='vertical')+
        coord_equal()+
        facet_wrap(vars(annotation),ncol=2)+
        geom_tile(aes(fill=normalization))+
        scale_fill_distiller(type = "div",limits=c(-1,1) * max(abs(select(heatmap_data_to_plot,normalization))))
      
      #generate heatmap data per dataset
      heatmap_data_to_plot_per_dataset <- angle_data_postures_per_dataset %>%
        ungroup() %>%
        #append newID2
        left_join(ordering, by=c("newID")) %>%
        filter(annotation %in% a)  %>%
        rename(normalization_per_dataset =  paste0(all_of(n),"_per_dataset"))
      
      ggplot(heatmap_data_to_plot_per_dataset,aes(x=factor(as.character(newID2),levels=sort(unique(heatmap_data_to_plot$newID2))),y=factor(hours_rounded,levels=unique(hours_rounded)))) +
        geom_tile(aes(fill= normalization_per_dataset))+
        labs(x="posture IDs",y = "hours",fill="Z-score") +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
        coord_equal()+
        scale_fill_distiller(type = "div",limits=c(-1,1) * max(abs(select(heatmap_data_to_plot_per_dataset,normalization_per_dataset))))+
        facet_wrap(vars(dataset_ID),ncol=1)+
        # scale_y_continuous(breaks=seq(1,max(heatmap_data_to_plot_per_dataset$hours_rounded),2))+
        theme_classic()+ 
        theme(axis.text.y = element_text(size = 30,face = "bold"),
              axis.text.x = element_text(size = 30),
              axis.title.x = element_text(size=45),
              axis.title.y = element_text(size=45),
              strip.text = element_text(size=45),
              legend.text=element_text(size=50),
              legend.title = element_text(size=50),
              legend.key.size = unit(2, "cm"),
              legend.position = "right",
              legend.direction='vertical')
      
      ggsave(file.path(save_path,paste0("heatmap_",paste0(a,collapse="+"),"_",n,"_per_dataset",".png")),width=49,height=30)
      
      #levels must be changed for ggplot to take new order into account
      cluster_centers_reduced_ordered <- cluster_centers_reduced %>%
        #append newID2
        left_join(ordering, by=c("newID"))
      #order based on natural order of newID2
      cluster_centers_reduced_ordered$newID2 = factor(cluster_centers_reduced_ordered$newID2, levels=sort(unique(heatmap_data_to_plot$newID2)))
      
      #generate all postures, for the mirrored ones show only newID_sub 1
      skeletons <- plot_posture_examples(filter(cluster_centers_reduced_ordered,newID_sub == 1), "X","Y") +
        geom_point(data=filter(cluster_centers_reduced_ordered,index == 2 & newID_sub == 1), aes(X,Y),color="orange",size=5,shape=17) +
        facet_wrap(vars(newID2),ncol=10) +
        theme(strip.text.x = element_text(size = 45,face="bold"))
      
      heatmap /
        skeletons + 
        plot_layout(widths = c(1,3),heights = c(1,3))+
        ggsave(file.path(save_path,paste0("heatmap_",paste0(a,collapse="+"),"_",n,".png")),width=49,height=30)
      
      
      #for each annotation save data frame with relative occurence of posters and newID2 (ordering like in heatmap)
      saveRDS(heatmap_data_to_plot_per_dataset, file.path(target_folder,paste0(annotation,"_",n,"_angle_data_postures_per_dataset.RDS")))
    }
    
  }
}




#Plot every frame of a random track
#plotted will be the original skeleton (with original X and Y location), as well as the alligned skeleton and the corresponding posture
# 
# save_path <- file.path(base_path,"plots","posture","posture_comparison_plots")
# dir.create(save_path)
# 
# plot_posture_examples <- function(data_to_plot,X,Y){
#   ggplot(data_to_plot, aes_string(x=X,y=Y,color="angle"))+
#     geom_point(size=0.75) +
#     geom_path(size=2,lineend = "round") +
#     theme_void()+
#     coord_fixed(ratio = 1)+
#     scale_color_gradient2(low = "deepskyblue", mid = "lavender",
#                           high = "deeppink",midpoint=0,limits=c(-1,1),na.value="grey") +
#     # scale_color_viridis(option = "plasma",limits=c(-1,1))+
#     theme(plot.title = element_text(face = "bold"))
#   
# }
# 
# #list of all tracks
# list_of_tracks <- skeleton_data_clustered %>%
#   mutate(dataset_TrackID =  paste0(dataset_ID,"_",tp,"_",TrackID)) %>%
#   distinct(dataset_TrackID) %>%
#   pull(dataset_TrackID)
# 
# 
# #select a random track from this list
# random_track <- sample(list_of_tracks,1)
# 
#                                       
# #subset random track and turn the skeletons
# skeleton_data_clustered_subset <- skeleton_data_clustered %>%
#   mutate(dataset_TrackID =  paste0(dataset_ID,"_",tp,"_",TrackID)) %>%
#   filter(dataset_TrackID == random_track) %>%
#   group_by(ID) %>%
#   #keep old X and Y positions (before turning) to compare when plotting
#   mutate(X_original = X, Y_original = Y) %>%
#   group_modify(~ turn_worm(.x))
#   
# 
# 
# #plot all frames from list of frames of the random track
# for (i in unique(skeleton_data_clustered_subset$frame)){
#    
#   selected_frame <- i 
#   #temporary subset (only skeleton of selected frame)
#   skeleton_data_clustered_subset_temp <- skeleton_data_clustered_subset %>%
#     filter(frame == selected_frame)
#   
#   #clusterID of selected skeleton
#   clusterID_to_plot <- unique(skeleton_data_clustered_subset_temp$clusterID)
#   #corresponding cluster center
#   cluster_centers_reduced_temp <- cluster_centers_reduced %>%
#     filter(clusterID == clusterID_to_plot)
#   
#   #plot raw with old X and Y coordinates
#   plot_raw <- plot_posture_examples(skeleton_data_clustered_subset_temp, "X_original", "Y_original")+
#    geom_point(data=filter(skeleton_data_clustered_subset_temp,index == 2), aes_string("X_original","Y_original"),color="orange",size=8,shape=16) + 
#     theme(plot.caption = element_text(vjust = -3),
#           legend.position = "none")+
#     ggtitle(paste0("Raw ", random_track,", frame ", selected_frame))
#     
#   #plot turned worm
#   plots_alligned <- plot_posture_examples(skeleton_data_clustered_subset_temp,"X","Y") +
#     geom_point(data=filter(skeleton_data_clustered_subset_temp,index == 2), aes(X,Y),color="orange",size=5,shape=16) +
#     ggtitle(paste0("Alligned"))+
#     theme(plot.caption = element_text(vjust = -3),
#           legend.position = "none")
#   
#   #plot corresponding cluster center
#   plot_corresponding_cluster_mean <- plot_posture_examples(cluster_centers_reduced_temp,"X","Y") +
#     geom_point(data=filter(cluster_centers_reduced_temp,index == 2), aes_string("X","Y"),color="orange",size=5,shape=16) +
#     ggtitle(paste0("Posture ", clusterID_to_plot))+
#     theme(plot.caption = element_text(vjust = -3),
#           legend.position = "none")
#   
#   #bring all together
#   ((plot_raw + plot_spacer()) |
#     plots_alligned/
#     plot_corresponding_cluster_mean) +
#     plot_layout(heights = unit(c(6,1,8,8), c('cm', 'cm','cm','cm')))+
#     # plot_layout(widths=c(1,3,3), heights = c(1,3, 3))
#      ggsave(file.path(save_path,paste0("posture_frame",selected_frame,".png")),height=15,width=10)
# 
# }


