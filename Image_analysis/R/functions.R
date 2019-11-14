#functions

library("plyr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("rmarkdown")
library("knitr")
library("lubridate")
library("wesanderson")
library("readr")
library("purrr")
library("viridis")
library("png")
library("grid")
library("ggpubr")
library("RColorBrewer")
library("shiny")
library("shinyWidgets")
library("plotly")
library("ggridges")
library("gganimate")
library("cowplot")
library("here")
library("ggbeeswarm")
library("ggsci")
library("reshape2")
library("pracma")

#functions



angle <- function(x1, y1, x2, y2) {
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

angle2 <- function(x1, y1, x2, y2) {
  atan2(y2,x2) - atan2(y1,x1) 
}


distance <- function(x1, y1, x2, y2) {
  values <- list(x1, y1, x2, y2)
  if(anyNA(values)) {
    "NA"
  } else {
    length <- sqrt((x1-x2)^2+(y1-y2)^2)
    as.numeric(length)
  }
}

# the function which will change all values of a given column to NA, except those rows that are consecutive for y times
replace_f <- function(x,y){
  subs <- rle(x)
  subs$values[subs$lengths < y] <- NA
  inverse.rle(subs)
}

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))



data_filtering <- function(data_input,conversion_factor, offset,binning_factor, max_gaps, duration){
  data_input %>%
    mutate(grouping = paste0(dataset_ID, "_", tp, "_", TrackID)) %>%
    filter(Number_of_gaps <= max_gaps) %>%
    filter(Duration_of_track >= duration) %>%
    group_by(grouping) %>%
    mutate(Track_displacement=Track_displacement * conversion_factor) %>%
    mutate(seconds=frame / downsampled_to) %>%
    mutate(x_lag=location_x - lag(location_x,n=offset), y_lag = location_y -lag(location_y, n=offset)) %>%
    mutate(x_lead=lead(location_x, n=offset)-location_x, y_lead=lead(location_y, n=offset)-location_y) %>%
    mutate(location_x_norm = (location_x/1024)) %>%
    mutate(location_y_norm = (location_y/1024)) %>%
    mutate(angle=suppressWarnings(as.numeric(mapply(angle,x_lag,y_lag,x_lead,y_lead)))*180/pi) %>%
    mutate(local_distance=suppressWarnings(as.numeric(mapply(distance,x_lag,y_lag,x_lead,y_lead)))) %>%
    # mutate(local_distance = local_distance * conversion_factor) %>%
    mutate(aspect_ratio=Minor/Major) %>%
    na.omit() %>%
    mutate(mean_local_distance=mean(local_distance)) %>%
    mutate(state= if_else(local_distance < 2,"pause","straight")) %>%
    mutate(state= if_else(mean_local_distance < 2.5,"pause",state)) %>%
    mutate(omega_turn_parameter_based = if_else(angle > 120,"turn","no")) %>%
    mutate(state=if_else((omega_turn_parameter_based == "turn" & state != "pause"), "turn",state)) %>%
    #set up binning depending on binning_factor
    mutate(binning=rep(0:n(),each=binning_factor,length.out=n())) %>%
    ungroup() %>%
    group_by(grouping,binning) %>%
    #for every bin do majority vote
    mutate(state_binned = names(table(state))[which.max(table(state))]) %>%
    mutate(state_binned=if_else((state == "turn" & Circularity > 0.4), "omega_turn",state)) %>%
    ungroup()
}
params_statistics <- function(input_data,how_to){
  if (how_to == "mutate"){
    input_data %>%
      na.omit() %>%
      #this will be computed on a "per track basis"
      mutate(p_mean_angle = mean(angle),
             p_sd_local_angle = sd(angle),
             p_displacement = first(Track_displacement*conversion_factor),
             p_displacement_distance_ratio = mean(Track_displacement) / sum(local_distance),
             p_mean_velocity = mean(local_distance*conversion_factor/downsampled_to),
             p_track_length_secs = mean(Duration_of_track/downsampled_to),
             p_sum_turns = sum(omega_turn_parameter_based == "turn"),
             p_sum_turns_ml = sum(Prediction == "turn"),
             p_turns_per_s = p_sum_turns / p_track_length_secs,
             p_turns_ml_per_s = p_sum_turns_ml / p_track_length_secs,
             p_eccentricity = mean(Eccentricity),
             p_size = mean(Size),
             p_length=mean(Major*conversion_factor),
             p_major_minor_ratio = mean(Major)/mean(Minor))
  } else if (how_to == "summarise"){
    input_data %>%
      na.omit() %>%
      summarise(p_mean_angle = mean(angle),
                p_sd_local_angle=sd(angle),
                p_displacement=first(Track_displacement*conversion_factor),
                p_displacement_distance_ratio= mean(Track_displacement) / sum(local_distance),
                p_mean_velocity = mean(local_distance*conversion_factor/downsampled_to),
                p_track_length_secs = mean(Duration_of_track/downsampled_to),
                p_sum_turns = sum(omega_turn_parameter_based == "turn"),
                p_sum_turns_ml = sum(Prediction == "turn"),
                p_turns_per_s = p_sum_turns / p_track_length_secs,
                p_turns_ml_per_s = p_sum_turns_ml / p_track_length_secs,
                p_eccentricity = mean(Eccentricity),
                p_size = mean(Size),
                p_length=mean(Major*conversion_factor),
                p_major_minor_ratio = mean(Major)/mean(Minor),
                p_perc_turn = sum(state_binned == "turn")/n(),
                p_perc_omega_turn = sum(state_binned == "omega_turn")/n(),
                p_perc_straight = sum(state_binned == "straight")/n(),
                p_perc_pause = sum(state_binned == "pause")/n())
  }
}

minutes_to_corresponding_tp <- function(input_data,time_input_type,time_input){
  if(time_input_type == "minutes"){
    all_tps_in_minutes <- unique(input_data$minutes)
    corresponding_minute <- all_tps_in_minutes[which(abs(all_tps_in_minutes-time_input)==min(abs(all_tps_in_minutes-time_input)))]
    selected_tp <<- unique(input_data[input_data$minutes == corresponding_minute,"tp"])
  }else{
    selected_tp <<- time_input
  }
}

plot_paths <- function(selected_tp,selected_frame,which_dataset,color_path,image){  
  p <- ggplot(data_for_plotting)+
    theme(aspect.ratio = 1024/1024)+
    coord_cartesian(ylim=c(0, 1023))+
    scale_x_continuous(expand = c(0, 0), limits= c(0,1023)) + scale_y_continuous(expand = c(0, 0),trans="reverse")+
    ggtitle(paste0("tp_",selected_tp))
  
  if(image == "with_image"){
    tp <- paste0("tp_",selected_tp)
    frame_png <- paste0(sprintf("%03d", selected_frame),".png")
    location_dataset <- target_folder_general[grep(which_dataset, target_folder_general)]
    img <- readPNG(file.path(target_folder_base,location_dataset,tp,frame_png))
    p <- p + background_image(img) + theme_black() + theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), plot.background = element_rect(fill = "black"),legend.direction="vertical")
  }else{
    p <- p  + theme_black() + theme(legend.direction="vertical") + theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), plot.background = element_rect(fill = "black"))
  }
  if(color_path == "mean_angle"){
    sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 180))
    p <- p +
      sc + 
      geom_path(aes(x=location_x, y=location_y,col=p_mean_angle, group=grouping,size=path_size), alpha=0.75)
  }else if(color_path == "ID"){
    p <- p +
      geom_path(aes(x=location_x, y=location_y,col=TrackID, group=grouping,size=path_size), alpha=0.75)+
      theme(
        legend.position = "none")
  }else if(color_path == "frame"){
    sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 960))
    p <- p +
      sc +
      geom_path(aes(x=location_x, y=location_y,col=frame, group=grouping,sizepath_size), alpha=0.75)
  }else if(color_path == "displacement"){
    sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 6300))
    p <- p +
      sc +
      geom_path(aes(x=location_x, y=location_y,col=p_displacement, group=grouping,size=path_size), alpha=0.75)
  }else if(color_path == "velocity"){
    sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 50))
    p <- p +
      sc +
      geom_path(aes(x=location_x, y=location_y,col=p_mean_velocity, group=grouping,size=path_size), alpha=0.75)
  }else if(color_path == "length"){
    sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 480))
    p <- p +
      sc +
      geom_path(aes(x=location_x, y=location_y,col=p_track_length_secs, group=grouping,size=path_size), alpha=0.75)
  }else if(color_path == "displacement_distance_ratio"){
    sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 1))
    p <- p +
      sc +
      geom_path(aes(x=location_x, y=location_y,col=p_displacement_distance_ratio, group=grouping,size=0.6), alpha=0.5)
  }else if(color_path == "sd_local_angle"){
    sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 75))
    p <- p +
      sc +
      geom_path(aes(x=location_x, y=location_y,col=p_sd_local_angle, group=grouping,size=path_size), alpha=0.75)
  }else if(color_path == "eccentricity"){
    sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, max(0.9)))
    p <- p +
      sc +
      geom_path(aes(x=location_x, y=location_y,col=p_eccentricity, group=grouping,size=path_size), alpha=0.75)
  }else if(color_path == "local_velocity"){
    sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 150))
    p <- p +
      sc +
      geom_path(aes(x=location_x, y=location_y,col=local_distance, group=grouping,size=path_size), alpha=0.75)
  }else if(color_path == "turns"){
    p <- p +
      scale_colour_hue() +
      geom_path(aes(x=location_x, y=location_y,col= omega_turn_parameter_based, group=grouping,size=path_size), alpha=0.75)
  }else if(color_path == "turns_ml"){
    p <- p +
      scale_colour_hue() +
      geom_path(aes(x=location_x, y=location_y,col=Prediction, group=grouping,size=path_size), alpha=0.75)
  }else if(color_path == "path_class_binned"){
    p <- p +
      scale_colour_brewer(palette="Set1")+
      geom_path(aes(x=location_x, y=location_y,col=state_binned, group=grouping),size=1, alpha=0.4)
  }else{
    sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 180))
    p <- p +
      sc+
      geom_path(aes(x=location_x, y=location_y,col=angle, group=grouping,size=path_size), alpha=0.75)
  }
}


plot_image_with_path <- function(which_tracks,which_track,offset,binning_factor,conversion_factor,time_input_type,time_input,which_dataset,max_number_gaps,duration,selected_frame,color_path,image) {
  sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 180))
  data_sub_dataset <- data %>%
    filter(dataset_ID == which_dataset)
  minutes_to_corresponding_tp(data_sub_dataset,time_input_type,time_input)
  if(which_tracks == "current_frame"){
    data_sub_dataset_tp <- data_sub_dataset %>%
      filter(tp == selected_tp) %>%
      group_by(TrackID) %>%
      filter(any(frame == selected_frame))
  }else if(which_tracks == "until_this_frame"){
    data_sub_dataset_tp <- data_sub_dataset %>%
      filter(tp == selected_tp) %>%
      filter(frame <= selected_frame)
  }else if(which_tracks == "until_and_current"){
    data_sub_dataset_tp <- data_sub_dataset %>%
      filter(tp == selected_tp) %>%
      group_by(TrackID) %>%
      filter(any(frame == selected_frame+10)) %>%
      ungroup() %>%
      filter(frame <= selected_frame+10)
  }else{
    data_sub_dataset_tp <- data_sub_dataset %>%
      filter(tp == selected_tp)
  }
  data_for_plotting <<- data_filtering(data_sub_dataset_tp,conversion_factor, offset,binning_factor, max_number_gaps,duration) %>%
    group_by(TrackID) %>%
    params_statistics(., "mutate") %>%
    mutate(path_size=if_else(TrackID == which_track, "selected", "not_selected"))
  
  print(plot_paths(selected_tp,selected_frame,which_dataset,color_path,image)+scale_size_discrete(range = c(1, 2)))
}


save_plot_image_with_path <- function(which_tracks,which_track,offset,binning_factor,conversion_factor,time_input_type,time_input,which_dataset,max_number_gaps,duration,selected_frame,color_path,image) {
  for(i in seq(1, selected_frame)){
    selected_frame <- i
    path <- here("Image_analysis/R/plots", which_dataset,time_input,color_path)
    dir.create(path,recursive = TRUE,showWarnings=FALSE)
    #png(file=file.path(path,paste0("tp_",selected_tp,"_",round(selected_frame,3),".png")))
    plot_image_with_path(which_tracks,which_track,offset,binning_factor,conversion_factor,time_input_type,time_input,which_dataset,max_number_gaps,duration,selected_frame,color_path,image)+
      ggsave(file=file.path(path,paste0("tp_",selected_tp,"_",sprintf("%03d",selected_frame),".png")),height = 200, width = 200,units="mm")
    
    #dev.off()
  }
}

plot_image_with_path_centered <- function(which_tracks,offset,binning_factor,conversion_factor,time_input_type,time_input,which_dataset,max_gaps,duration,selected_frame,color_path,image) {
  sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 180))
  data_sub_dataset <- data %>% filter(dataset_ID == which_dataset)
  minutes_to_corresponding_tp(data_sub_dataset, time_input_type, time_input)
  if(which_tracks == "all_frames"){
    data_sub_dataset_tp <- data_sub_dataset %>%
      filter(tp == selected_tp)
  }else{
    data_sub_dataset_tp <- data_sub_dataset %>%
      filter(tp == selected_tp) %>%
      filter(frame == selected_frame)
  }
  
  
  data_for_plotting <<- data_filtering(data_sub_dataset_tp, conversion_factor, offset, binning_factor, max_gaps, duration) %>%
    group_by(TrackID) %>%
    na.omit() %>%
    params_statistics(., "mutate") %>%
    mutate(first_x = first(location_x),first_y = first(location_y)) %>%
    mutate(location_x = location_x - first_x, location_y=location_y - first_y)
  
  pc <<- plot_paths(selected_tp,selected_frame,which_dataset,color_path,image)+
    coord_cartesian(ylim=NULL,xlim=NULL)+
    scale_x_continuous(limits= c(-1024,1024)) +
    scale_y_continuous(limits= c(-1024,1024))
  print(pc)
  
}

plot_single_path_states <-  function(which_track,conversion_factor,binning_factor,offset,time_input_type,time_input,selected_frame,which_dataset,max_gaps,duration,color_path,image){
  data_sub_dataset <- data %>% filter(dataset_ID == which_dataset)
  minutes_to_corresponding_tp(data_sub_dataset, time_input_type, time_input)
  data_sub_dataset_tp <- data_sub_dataset %>%
    filter(tp == selected_tp)
  data_for_plotting <<- data_filtering(data_sub_dataset_tp, conversion_factor, offset, binning_factor,max_gaps, duration) %>%
    filter(TrackID == which_track) %>%
    na.omit() %>%
    mutate(first_x = first(location_x),first_y = first(location_y)) %>%
    mutate(location_x = location_x - first_x, location_y=location_y - first_y) %>%
    mutate(location_x = location_x * conversion_factor, location_y=location_y * conversion_factor)%>%
    mutate(path_size=0.1)
  #fix colors
  myColors <-  brewer.pal(4,"Set1")
  names(myColors) <- c("pause","straight","turn","omega_turn")
  colScale <- scale_colour_manual(name = "state_binned",values = myColors)
  p1 <- ggplot(data_for_plotting,aes(x=seconds,y=angle,xend=lead(seconds),yend=lead(angle),color=state_binned)) +
    geom_segment()+
    colScale+
    scale_y_continuous(limits=c(0,180))+
    theme_black()+
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          axis.title.x=element_blank(),
          axis.text.x=element_blank())
  # geom_vline(xintercept=selected_frame/2, linetype="dashed", color = "white")
  
  p2 <- ggplot(data_for_plotting,aes(x=seconds, y=local_distance,xend=lead(seconds),yend=lead(local_distance),color=state_binned))+
    geom_segment()+
    colScale+
    scale_y_continuous(limits=c(0,200))+
    theme_black()+
    theme(legend.position = "none",
          axis.text.x=element_blank(),
          axis.title.x=element_blank())
  p3 <- ggplot(data_for_plotting,aes(x=seconds, y=Circularity,xend=lead(seconds),yend=lead(Circularity),color=state_binned))+
    geom_segment()+
    colScale+
    scale_y_continuous(limits=c(0,0.8))+
    theme_black()+
    theme(legend.position = "none")
  pz <- plot_paths(selected_tp,selected_frame,which_dataset,color_path,image)+
    colScale+
    theme(legend.position = "none",
          axis.text=element_text(size=0.05),
          axis.ticks=element_line(color="white",size=0.5),
          axis.title=element_text(size=0.2))+
    coord_cartesian(ylim=NULL,xlim=NULL)+
    scale_x_continuous(limits= c(min(data_for_plotting$location_x),max(data_for_plotting$location_x))) +
    scale_y_continuous(limits= c(min(data_for_plotting$location_y),max(data_for_plotting$location_y))) +
    labs(x = "x (microns)", y= "y (microns)")
  
  left_column <- plot_grid(p1,p2,p3,align = 'hv',axis='l',nrow = 3,rel_heights = c(1,1,1))
  plot_grid(left_column, pz,rel_heights=c(1,20),ncol=2)
  
  
}



calculate_overview_statistics <- function(data,conversion_factor,offset,binning_factor,max_gaps,duration) {
  data_filtering(data, conversion_factor, offset, binning_factor, max_gaps,duration) %>%
    group_by(TrackID, tp, minutes, hours,dataset_ID,file_name,worm_type,plate_type,Duration_of_track) %>%
    na.omit() %>%
    params_statistics(., "summarise")
  
  
}

theme_black = function(base_size = 12, base_family = "") {
  
  theme_classic(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_line(colour = "white"),  
      axis.text.x = element_text(size = base_size, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = NA,  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "horizontal",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_blank(),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),  
      panel.spacing = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_blank(), #(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white", face = "italic"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}





#the function that orders pixels and finds the head
skeleton <- function(datasetIDs, TrackIDs, selected_timepoints,frames){
  skeleton_data_all <<- data.frame() #set global variable to 0
  #for every dataset ...
  for(timelapse in datasetIDs){
    dataset_selected <- data %>%
      filter(dataset_ID == timelapse)
    for (timepoint in selected_timepoints){
      #for every timepoint ...
      ds_tp <- dataset_selected %>%
        filter(tp == timepoint)
      #for every track ...
      for(t in TrackIDs){
        cat("\n")
        print(t)
        if(t %in% unique(ds_tp$TrackID)){
          ds_tp_track <- ds_tp %>%
            filter(TrackID == t) %>%
            #compute the location of the centroid of the worm after one second
            #this gives us an idea about the direction
            mutate(x_lead=lead(location_x, n=2)-location_x, y_lead=lead(location_y, n=2)-location_y) %>%
            mutate(x_lag=lag(location_x, n=2)-location_x, y_lag=lag(location_y, n=2)-location_y) %>%
            # mutate(local_distance=suppressWarnings(as.numeric(mapply(distance,x_lag,y_lag,x_lead,y_lead))))
            mutate(x_dir = ifelse(x_lead < 0, "left","right")) %>%
            mutate(y_dir = ifelse(y_lead < 0, "up","down")) %>%
            mutate(direction = paste(x_dir,y_dir,sep="-"))
          #only keep the direction when direction is stable over 5 seconds 
          ds_tp_track$direction <- replace_f(ds_tp_track$direction,10)
          
          #for every frame
          for(f in frames){
            # foreach (f=frames) %dopar% {
            #only if this track contains the given frame number
            if (f %in% ds_tp_track$frame) {
              #get dimension from table, variables bitmask_dim_Y and bitmask_dim_X
              dim_Y <- as.numeric(filter(ds_tp_track,frame == f) %>% select(bitmask_dim_Y))
              dim_X <- as.numeric(filter(ds_tp_track,frame == f) %>% select(bitmask_dim_X))
              #import the data from bitmask column and transform to matrix with right dimensions
              bitmask <- filter(ds_tp_track,frame == f) %>% select(bitmask)
              bitmask <- strsplit(as.character(unlist(bitmask)),",")
              bitmask <- as.numeric(unlist(lapply(bitmask,gsub,pattern=".*(\\d+)\\..+", replacement="\\1")))
              data_imported <- matrix(bitmask,nrow=dim_Y, byrow=TRUE)
              
              #add 2 columns and 2 frames (as a "frame" around the bitmask)
              data_imported <- rbind(data_imported, rep(0,ncol(data_imported)))
              data_imported <- rbind(rep(0,ncol(data_imported)),data_imported)
              data_imported <- cbind(rep(0, nrow(data_imported)),data_imported)
              data_imported <- cbind(data_imported,rep(0, nrow(data_imported)))
              
              #if dimension X is bigger than Y (== landscape), make it portrait
              # if(dim_X > dim_Y){
              #   #swap dimension
              #   #add two to get the new dimensions right
              #   dim_Y <- as.numeric(filter(ds_tp_track,frame == f) %>% select(bitmask_dim_X))+2
              #   dim_X <- as.numeric(filter(ds_tp_track,frame == f) %>% select(bitmask_dim_Y))+2
              #   #here is where the transformation around 90° happens
              #   data_imported <- t(data_imported) %>%
              #     melt(.) %>%
              #     rename(Y = 1, X = 2,value = 3) %>%
              #     mutate(next_px_1_x = 0, next_px_1_y = 0,
              #            next_px_2_x = 0, next_px_2_y = 0)
              #   
              # } else {
              #add two to get the new dimensions right
              dim_X <- as.numeric(filter(ds_tp_track,frame == f) %>% select(bitmask_dim_X))+2
              dim_Y <- as.numeric(filter(ds_tp_track,frame == f) %>% select(bitmask_dim_Y))+2
              #here we don't transform
              data_imported <- data_imported %>%
                melt(.) %>%
                rename(Y = 1, X = 2,value = 3) %>%
                mutate(next_px_1_x = 0, next_px_1_y = 0,
                       next_px_2_x = 0, next_px_2_y = 0)
              # }
              
              
              #this is a ggplot to check if the skeleton was imported right
              # ggplot(data_imported %>% filter(value==1), aes(x=X,y=Y))+
              #     coord_equal() +
              #     geom_raster(aes(fill="black"))+
              #     scale_x_continuous(expand = c(0, 0), limits= c(0,dim_X)) +
              #     scale_y_continuous(expand = c(0, 0),trans="reverse") +
              #     # theme_void()+
              #     theme(legend.position="none")
              
              #create an empty list
              #here for each pixel we will find the surounding pixels (max. 8) that are having value "1"
              pixel_list <- vector(mode="list",length=0)
              for(i in seq(1,nrow(data_imported)-dim_Y)){
                if(data_imported[i,"value"] == 1){
                  if(data_imported[i+1,"value"] == 1){#0°
                    pixel_list <- append(pixel_list,paste(data_imported[i+1,"X"],data_imported[i+1,"Y"], sep=","))}
                  if(data_imported[i+dim_Y+1,"value"] == 1){#45°
                    pixel_list <- append(pixel_list,paste(data_imported[i+dim_Y+1,"X"],data_imported[i+dim_Y+1,"Y"], sep=","))}
                  if(data_imported[i+dim_Y,"value"] == 1){#90°
                    pixel_list <- append(pixel_list,paste(data_imported[i+dim_Y,"X"],data_imported[i+dim_Y,"Y"], sep=","))}
                  if(data_imported[i+dim_Y-1,"value"] == 1){#135°
                    pixel_list <- append(pixel_list,paste(data_imported[i+dim_Y-1,"X"],data_imported[i+dim_Y-1,"Y"], sep=","))}
                  if(data_imported[i-1,"value"] == 1){#180°
                    pixel_list <- append(pixel_list,paste(data_imported[i-1,"X"],data_imported[i-1,"Y"], sep=","))}
                  if(data_imported[i-dim_Y-1,"value"] == 1){#225°
                    pixel_list <- append(pixel_list,paste(data_imported[i-dim_Y-1,"X"],data_imported[i-dim_Y-1,"Y"], sep=","))}
                  if(data_imported[i-dim_Y,"value"] == 1){#270°
                    pixel_list <- append(pixel_list,paste(data_imported[i-dim_Y,"X"],data_imported[i-dim_Y,"Y"], sep=","))}
                  if(data_imported[i-dim_Y+1,"value"] == 1){#315°
                    pixel_list <- append(pixel_list,paste(data_imported[i-dim_Y+1,"X"],data_imported[i-dim_Y+1,"Y"], sep=","))}
                }
                #if only one neighbouring pixel is found then it most likely corresponds to the end (head or tail of the worm)
                if(length(pixel_list) == 1 ){
                  data_imported[i,"next_px_1_x"] <- as.numeric(unlist(strsplit(pixel_list[[1]],split=","))[1])
                  data_imported[i,"next_px_1_y"] <- as.numeric(unlist(strsplit(pixel_list[[1]],split=","))[2])
                  data_imported[i,"is_end"] <- "YES"
                  pixel_list <- vector(mode="list",length=0)
                  #if there are more than one neighbouring pixels than the pixel in question sits somewhere in the middle
                  #so far we only account for more than new two neighbours ...
                } else if(length(pixel_list) > 1){
                  data_imported[i,"next_px_1_x"] <- as.numeric(unlist(strsplit(pixel_list[[1]],split=","))[1])
                  data_imported[i,"next_px_1_y"] <- as.numeric(unlist(strsplit(pixel_list[[1]],split=","))[2])
                  data_imported[i,"next_px_2_x"] <- as.numeric(unlist(strsplit(pixel_list[[2]],split=","))[1])
                  data_imported[i,"next_px_2_y"] <- as.numeric(unlist(strsplit(pixel_list[[2]],split=","))[2])
                  data_imported[i,"is_end"] <- "NO"
                  pixel_list <- vector(mode="list",length=0)
                }
              }
              
              #if there is no pixel found with any neighbours set all pixels to is_end == "NO"
              if (is.null(data_imported$is_end)){
                data_imported$is_end <- "NO"
              }
              
              data_w_angles <<- data_imported %>%
                #only pixels that have value 1 (== no background)
                filter(value == 1) %>%
                #make sure that one endpoint is at the top of the table
                arrange(desc(is_end)) %>%
                #group positions (actual and neighbouring ones to one string)
                mutate(pos = paste(X,Y,sep=",")) %>%
                mutate(next_pos_1 = paste(next_px_1_x,next_px_1_y,sep=",")) %>%
                mutate(next_pos_2 = paste(next_px_2_x,next_px_2_y,sep=",")) %>%
                select(-c(next_px_1_x,next_px_1_y,next_px_2_x,next_px_2_y))
              
              #only do the following for worms that have 2 ends
              if(length(data_w_angles[data_w_angles$is_end == "YES","is_end"]) == 2){
                
                # start from one end (because we ordered by is_end above)
                pos_list <- data_w_angles[1, "pos"]
                
                
                # looks for the next position until the next end is found
                # if there is a round structure this can go into a endless loop, therefore don't make the list longer than the total number of pixels with value == 1
                while(tail(pos_list, n=1) != "0,0" && length(pos_list) < length(data_w_angles$value)){
                  next_pos <- data_w_angles[which(data_w_angles$pos == tail(pos_list, n=1)),"next_pos_1"]
                  if(next_pos %in% pos_list){
                    pos_list <- append(pos_list, data_w_angles[which(data_w_angles$pos == tail(pos_list, n=1)),"next_pos_2"])
                  }else{
                    pos_list <- append(pos_list, next_pos)
                  }
                  if(length(unique(pos_list)) != length(pos_list)){
                    pos_list <- NA
                    break
                  }
                }
                
                
                
                #only save those if there are more than 26 pixels and smaller than 60
                if(length(pos_list) > 26 & length(pos_list) < 80){
                  
                  #order pixels in the right order 
                  data_ordered <- data_w_angles[match(pos_list, data_w_angles$pos),] %>%
                    na.omit
                  
                  #downsample to 26
                  knots_Y <- linspace(data_ordered$Y[1],data_ordered$Y[nrow(data_ordered)],nrow(data_ordered))
                  knots_X <- linspace(data_ordered$X[1],data_ordered$X[nrow(data_ordered)],nrow(data_ordered))
                  tryCatch({
                    cfit <<- curvefit(knots_X, data_ordered$X, data_ordered$Y,8)
                    px <- c(cfit$px)
                    py <- c(cfit$py)
                    downsampled_knots <<- linspace(data_ordered$X[1],data_ordered$X[nrow(data_ordered)],26)
                  },
                  error = function(e){
                    cfit <<- "no X fit"
                  }
                  )
                  
                  if (!is.list(cfit)){
                    tryCatch({
                      cfit <<- curvefit(knots_Y, data_ordered$X, data_ordered$Y,8)
                      px <- c(cfit$px)
                      py <- c(cfit$py)
                      downsampled_knots <<- linspace(data_ordered$Y[1],data_ordered$Y[nrow(data_ordered)],26)
                      
                    },
                    error  = function(e2){
                      cfit <<- "no X and Y fit"  
                    }
                    )
                  }
                  
                  if (!is.list(cfit)){
                    print(paste0(timelapse,"_","tp_",timepoint,"_",t,"_","frame","_",f," ","error when fitting polynomial."))
                    data_dummy <<- data_w_angles %>%
                      filter(NA) %>%
                      add_row() %>%
                      select(-c(pos,value,next_pos_1,next_pos_2)) %>%
                      mutate(index=NA, angle = NA, dim_X = dim_X, dim_Y = dim_Y, prev_px_x = NA, prev_px_y = NA,next_px_x = NA, next_px_y = NA, diff_next_px_x = NA, diff_next_px_y = NA, diff_prev_px_x = NA, diff_prev_px_y = NA, is_head = NA, is_tail = NA, dataset_ID = timelapse, tp = timepoint, frame = f, TrackID = t,TrackCheck = NA)
                    
                    skeleton_data_all <<- rbind(skeleton_data_all,data_dummy)
                    
                  } else {
                    
                    xs <- polyval(px, downsampled_knots)
                    ys <- polyval(py, downsampled_knots)
                    
                    #get first and last position to is_end == "YES" (order should be the same as before so we can assume end of dataframe=end of worm)
                    data_ordered_downsampled <- data.frame(cbind(xs,ys))
                    data_ordered_downsampled$is_end <- NA
                    data_ordered_downsampled$is_end[1] <- "YES"
                    data_ordered_downsampled$is_end[26] <- "YES"
                    
                    #add index number for each pixel
                    data_ordered_for_plotting <- data_ordered_downsampled %>%
                      rename("X" = 1, "Y" = 2) %>%
                      mutate(index = row_number()) %>%
                      mutate(next_px_x = lead(X,n=1), next_px_y = lead(Y,n=1)) %>%
                      mutate(prev_px_x = lag(X,n=1),prev_px_y = lag(Y,n=1)) %>%
                      mutate(diff_prev_px_x =  X - prev_px_x, diff_prev_px_y = Y - prev_px_y) %>%
                      mutate(diff_next_px_x = next_px_x - X, diff_next_px_y = next_px_y - Y) %>%
                      mutate(angle=suppressWarnings(as.numeric(mapply(angle2,diff_prev_px_x,diff_prev_px_y,diff_next_px_x,diff_next_px_y))))%>%
                      mutate(dataset_ID = timelapse) %>%
                      mutate(tp = timepoint) %>%
                      mutate(frame = f, dim_X = dim_X, dim_Y = dim_Y) %>%
                      mutate(TrackID = t) %>%
                      mutate(is_head = NA) %>%
                      mutate(is_tail = NA) %>%
                      mutate(TrackCheck = t)
                    
                    
                    
                    
                    #if for that frame we have an idea about the direction in which the worm moves (see above), we will try finding a head structure based on this
                    if(!is.na(ds_tp_track[ds_tp_track$frame == f,"direction"])){
                      
                      #for this frame get moving direction
                      x_dir <- ds_tp_track[ds_tp_track$frame == f,"x_dir"]
                      y_dir <- ds_tp_track[ds_tp_track$frame == f,"y_dir"]
                      # get only pixels that describe the end of the worm
                      worm_ends <- filter(data_ordered_for_plotting, is_end == "YES")
                      #here depending on the direction in which the worm moves in the field of view (for X= right or left, for Y=up or down) we translate this information to the bitmask and identify the pixel that is closest to this side 
                      if (x_dir == "right" && y_dir == "up"){
                        data_ordered_for_plotting <- data_ordered_for_plotting %>%
                          mutate(is_head = NA) %>%
                          mutate(is_head = ifelse(X == max(worm_ends$X) & Y == min(worm_ends$Y)  & is_end == "YES","YES", NA)) 
                      } else if (x_dir == "left" && y_dir == "up"){ 
                        data_ordered_for_plotting <- data_ordered_for_plotting %>%
                          mutate(is_head = NA) %>%
                          mutate(is_head = ifelse(X == min(worm_ends$X) & Y == min(worm_ends$Y)  & is_end == "YES","YES", NA)) 
                      } else if (x_dir == "right" && y_dir == "down"){ 
                        data_ordered_for_plotting <- data_ordered_for_plotting %>%
                          mutate(is_head = NA) %>%
                          mutate(is_head = ifelse(X == max(worm_ends$X) & Y == max(worm_ends$Y)  & is_end == "YES","YES", NA)) 
                      } else if (x_dir == "left" && y_dir == "down"){ 
                        data_ordered_for_plotting <- data_ordered_for_plotting %>%
                          mutate(is_head = NA) %>%
                          mutate(is_head = ifelse(X == min(worm_ends$X) & Y == max(worm_ends$Y)  & is_end == "YES","YES", NA))
                      }
                      
                      if(!is.na(data_ordered_for_plotting[nrow(data_ordered_for_plotting),"is_head"])){
                        data_ordered_for_plotting$index <- nrow(data_ordered_for_plotting):1
                      }
                      
                      print(paste0(timelapse,"_","tp_",timepoint,"_",t,"_","frame","_",f," ","head found"))
                      #add data for this frame to global list
                      skeleton_data_all <<- rbind(skeleton_data_all, data_ordered_for_plotting)
                    } else {
                      #if we do not have a good indication about the direction in this frame, we try to get it from the last frame
                      if (!all(is.na(skeleton_data_all[skeleton_data_all$frame == f-1 & skeleton_data_all$TrackID == t,"is_head"]))){
                        latest_head <- filter(skeleton_data_all, tp == timepoint & frame == f-1 & TrackID == t & is_head == "YES")
                        if (nrow(latest_head) == 0){
                          print(paste0(timelapse,"_","tp_",timepoint,"_",t,"_","frame","_",f," ","no direction, no previous found"))
                          skeleton_data_all <<- rbind(skeleton_data_all, data_ordered_for_plotting)
                        } else {
                          latest_head_x <- latest_head$X
                          latest_head_y <- latest_head$Y
                          #here we are looking for the closest pixel that is also an end in the actual frame with respect to the previous frame
                          closest <- data_ordered_for_plotting %>%
                            mutate(latest_head_x = latest_head_x) %>%
                            mutate(latest_head_y = latest_head_y) %>%
                            mutate(distance_to_latest_head=suppressWarnings(as.numeric(mapply(distance,X,Y,latest_head_x,latest_head_y)))) %>%
                            filter(is_end == "YES") %>%
                            mutate(is_head = ifelse(distance_to_latest_head == min(distance_to_latest_head),"YES",NA)) %>%
                            filter(is_head == "YES")
                          
                          
                          data_ordered_for_plotting$is_head[closest$index] <- "YES"
                          
                          if(!is.na(data_ordered_for_plotting[nrow(data_ordered_for_plotting),"is_head"])){
                            data_ordered_for_plotting$index <- nrow(data_ordered_for_plotting):1
                          }
                          
                          print(paste0(timelapse,"_","tp_",timepoint,"_",t,"_","frame","_",f," ","no direction, head estimated from previous frame"))
                          skeleton_data_all <<- rbind(skeleton_data_all, data_ordered_for_plotting)
                          
                        }
                        
                      } else {
                        #but only it that last frame contains a head. if not (i.e. if that was missegmented), don't
                        print(paste0(timelapse,"_","tp_",timepoint,"_",t,"_","frame","_",f," ","no direction, no previous found"))
                        skeleton_data_all <<- rbind(skeleton_data_all, data_ordered_for_plotting)
                        
                      }
                    }
                  }
                  
                  #if skeleton contains less than 26 pixels
                } else {
                  print(paste0(timelapse,"_","tp_",timepoint,"_",t,"_","frame","_",f," ","under 26 px"))
                  data_dummy <<- data_w_angles %>%
                    filter(NA) %>%
                    add_row() %>%
                    select(-c(pos,value,next_pos_1,next_pos_2)) %>%
                    mutate(index=NA, angle = NA, dim_X = dim_X, dim_Y = dim_Y, prev_px_x = NA, prev_px_y = NA,next_px_x = NA, next_px_y = NA, diff_next_px_x = NA, diff_next_px_y = NA,diff_prev_px_x = NA, diff_prev_px_y = NA, is_head = NA, is_tail = NA, dataset_ID = timelapse, tp = timepoint, frame = f, TrackID = t,TrackCheck = NA)
                  
                  skeleton_data_all <<- rbind(skeleton_data_all,data_dummy)
                }
                #if worm has not two ends
              } else {
                print(paste0(timelapse,"_","tp_",timepoint,"_",t,"_","frame","_",f," ","has not 2 ends"))
                data_dummy <<- data_w_angles %>%
                  filter(NA) %>%
                  add_row() %>%
                  select(-c(pos,value,next_pos_1,next_pos_2)) %>%
                  mutate(index=NA, angle = NA, dim_X = dim_X, dim_Y = dim_Y, prev_px_x = NA, prev_px_y = NA, next_px_x = NA, next_px_y = NA, diff_next_px_x = NA, diff_next_px_y = NA, diff_prev_px_x = NA, diff_prev_px_y = NA, is_head = NA, is_tail = NA, dataset_ID = timelapse, tp = timepoint, frame = f, TrackID = t,TrackCheck = NA)
                
                skeleton_data_all <<- rbind(skeleton_data_all,data_dummy)
              }
              #if that track does not span this frame number
            } else {
              print(paste0(timelapse,"_","tp_",timepoint,"_",t,"_","frame","_",f," ","track not detected in this frame"))
              data_dummy <<- skeleton_data_all %>%
                filter(NA) %>%
                mutate(tp = timepoint) %>%
                add_row(frame=f:(max(frames))) %>%
                mutate(index=NA, angle = NA, dim_X = dim_X, dim_Y = dim_Y, is_head = NA, is_tail = NA, dataset_ID = timelapse, tp = timepoint, TrackID = t,TrackCheck = NA)
              
              skeleton_data_all <<- rbind(skeleton_data_all,data_dummy)
              break
            }
          }
        } else {
          print(paste0(t, " not present in ", timelapse, "_tp_", timepoint))
        }
      }
    }
  }
}

skeleton_direction <- function(x){
  mutate(x,x_lead=lead(location_x, n=2)-location_x, y_lead=lead(location_y, n=2)-location_y) %>%
    mutate(x_lag=lag(location_x, n=2)-location_x, y_lag=lag(location_y, n=2)-location_y) %>%
    mutate(local_distance=suppressWarnings(as.numeric(mapply(distance,x_lag,y_lag,x_lead,y_lead)))) %>%
    mutate(x_dir = ifelse(x_lead < 0, "left","right")) %>%
    mutate(y_dir = ifelse(y_lead < 0, "up","down")) %>%
    mutate(direction = paste(x_dir,y_dir,sep="-")) %>%
    #only keep the direction when direction is stable over 5 seconds
    mutate(direction = replace_f(direction,10))
  
}

skeleton_frame <- function(data, dataset_name, tp_name, TrackID_name,frame_number){
  #get dimension from table, variables bitmask_dim_Y and bitmask_dim_X
  dim_Y <- data$bitmask_dim_Y
  dim_X <- data$bitmask_dim_X
  # #import the data from bitmask column and transform to matrix with right dimensions
  bitmask <- data$bitmask
  bitmask <- strsplit(as.character(unlist(bitmask)),",")
  bitmask <- as.numeric(unlist(lapply(bitmask,gsub,pattern=".*(\\d+)\\..+", replacement="\\1")))
  data_imported <- matrix(bitmask,nrow=dim_Y, byrow=TRUE)

  # #add 2 columns and 2 frames (as a "frame" around the bitmask)
  data_imported <- rbind(data_imported, rep(0,ncol(data_imported)))
  data_imported <- rbind(rep(0,ncol(data_imported)),data_imported)
  data_imported <- cbind(rep(0, nrow(data_imported)),data_imported)
  data_imported <- cbind(data_imported,rep(0, nrow(data_imported)))
  #
  #
  #add two to get the new dimensions right
  dim_X <- data$bitmask_dim_X + 2
  dim_Y <- data$bitmask_dim_Y + 2

  data_imported <- data_imported %>%
    melt(.) %>%
    rename(Y = 1, X = 2,value = 3) %>%
    mutate(next_px_1_x = 0, next_px_1_y = 0,
    next_px_2_x = 0, next_px_2_y = 0)


  #this is a ggplot to check if the skeleton was imported right
  # ggplot(data_imported %>% filter(value==1), aes(x=X,y=Y))+
  #     coord_equal() +
  #     geom_raster(aes(fill="black"))+
  #     scale_x_continuous(expand = c(0, 0), limits= c(0,dim_X)) +
  #     scale_y_continuous(expand = c(0, 0),trans="reverse") +
  #     # theme_void()+
  #     theme(legend.position="none")
    pixel_list <- vector(mode="list",length=0)
    rows_with_value1 <- data_imported %>%
      mutate(original_rownumber=1:n()) %>%
      filter(value == 1) %>%
      pull(original_rownumber)

    for(i in rows_with_value1){

        if(data_imported[i+1,"value"] == 1){#0°
          pixel_list <- append(pixel_list,paste(data_imported[i+1,"X"],data_imported[i+1,"Y"], sep=","))}
        if(data_imported[i+dim_Y+1,"value"] == 1){#45°
          pixel_list <- append(pixel_list,paste(data_imported[i+dim_Y+1,"X"],data_imported[i+dim_Y+1,"Y"], sep=","))}
        if(data_imported[i+dim_Y,"value"] == 1){#90°
          pixel_list <- append(pixel_list,paste(data_imported[i+dim_Y,"X"],data_imported[i+dim_Y,"Y"], sep=","))}
        if(data_imported[i+dim_Y-1,"value"] == 1){#135°
          pixel_list <- append(pixel_list,paste(data_imported[i+dim_Y-1,"X"],data_imported[i+dim_Y-1,"Y"], sep=","))}
        if(data_imported[i-1,"value"] == 1){#180°
          pixel_list <- append(pixel_list,paste(data_imported[i-1,"X"],data_imported[i-1,"Y"], sep=","))}
        if(data_imported[i-dim_Y-1,"value"] == 1){#225°
          pixel_list <- append(pixel_list,paste(data_imported[i-dim_Y-1,"X"],data_imported[i-dim_Y-1,"Y"], sep=","))}
        if(data_imported[i-dim_Y,"value"] == 1){#270°
          pixel_list <- append(pixel_list,paste(data_imported[i-dim_Y,"X"],data_imported[i-dim_Y,"Y"], sep=","))}
        if(data_imported[i-dim_Y+1,"value"] == 1){#315°
          pixel_list <- append(pixel_list,paste(data_imported[i-dim_Y+1,"X"],data_imported[i-dim_Y+1,"Y"], sep=","))}


  # create an empty list
  # here for each pixel we will find the surounding pixels (max. 8) that are having value "1"
  # pixel_list <- vector(mode="list",length=0)
  # for(i in seq(1,nrow(data_imported)-dim_Y)){
  #     if(data_imported[i,"value"] == 1){
  #       if(data_imported[i+1,"value"] == 1){#0°
  #         pixel_list <- append(pixel_list,paste(data_imported[i+1,"X"],data_imported[i+1,"Y"], sep=","))}
  #       if(data_imported[i+dim_Y+1,"value"] == 1){#45°
  #         pixel_list <- append(pixel_list,paste(data_imported[i+dim_Y+1,"X"],data_imported[i+dim_Y+1,"Y"], sep=","))}
  #       if(data_imported[i+dim_Y,"value"] == 1){#90°
  #         pixel_list <- append(pixel_list,paste(data_imported[i+dim_Y,"X"],data_imported[i+dim_Y,"Y"], sep=","))}
  #       if(data_imported[i+dim_Y-1,"value"] == 1){#135°
  #         pixel_list <- append(pixel_list,paste(data_imported[i+dim_Y-1,"X"],data_imported[i+dim_Y-1,"Y"], sep=","))}
  #       if(data_imported[i-1,"value"] == 1){#180°
  #         pixel_list <- append(pixel_list,paste(data_imported[i-1,"X"],data_imported[i-1,"Y"], sep=","))}
  #       if(data_imported[i-dim_Y-1,"value"] == 1){#225°
  #         pixel_list <- append(pixel_list,paste(data_imported[i-dim_Y-1,"X"],data_imported[i-dim_Y-1,"Y"], sep=","))}
  #       if(data_imported[i-dim_Y,"value"] == 1){#270°
  #         pixel_list <- append(pixel_list,paste(data_imported[i-dim_Y,"X"],data_imported[i-dim_Y,"Y"], sep=","))}
  #       if(data_imported[i-dim_Y+1,"value"] == 1){#315°
  #         pixel_list <- append(pixel_list,paste(data_imported[i-dim_Y+1,"X"],data_imported[i-dim_Y+1,"Y"], sep=","))}
  #     }
    
    #if only one neighbouring pixel is found then it most likely corresponds to the end (head or tail of the worm)
  if(length(pixel_list) == 1 ){
    data_imported[i,"next_px_1_x"] <- as.numeric(unlist(strsplit(pixel_list[[1]],split=","))[1])
    data_imported[i,"next_px_1_y"] <- as.numeric(unlist(strsplit(pixel_list[[1]],split=","))[2])
    data_imported[i,"is_end"] <- "YES"
    pixel_list <- vector(mode="list",length=0)
    #if there are more than one neighbouring pixels than the pixel in question sits somewhere in the middle
    #so far we only account for more than  two neighbours ...
  } else if(length(pixel_list) > 1){
    data_imported[i,"next_px_1_x"] <- as.numeric(unlist(strsplit(pixel_list[[1]],split=","))[1])
    data_imported[i,"next_px_1_y"] <- as.numeric(unlist(strsplit(pixel_list[[1]],split=","))[2])
    data_imported[i,"next_px_2_x"] <- as.numeric(unlist(strsplit(pixel_list[[2]],split=","))[1])
    data_imported[i,"next_px_2_y"] <- as.numeric(unlist(strsplit(pixel_list[[2]],split=","))[2])
    data_imported[i,"is_end"] <- "NO"
    pixel_list <- vector(mode="list",length=0)
  }
}

# if there is no pixel found with any neighbours set all pixels to is_end == "NO"
if (is.null(data_imported$is_end)){
  data_imported$is_end <- "NO"
}

data_w_angles <- data_imported %>%
  #only pixels that have value 1 (== no background)
  filter(value == 1) %>%
  #make sure that one endpoint is at the top of the table
  arrange(desc(is_end)) %>%
  #group positions (actual and neighbouring ones to one string)
  mutate(pos = paste(X,Y,sep=",")) %>%
  mutate(next_pos_1 = paste(next_px_1_x,next_px_1_y,sep=",")) %>%
  mutate(next_pos_2 = paste(next_px_2_x,next_px_2_y,sep=",")) %>%
  mutate(direction = data$direction) %>%
  mutate(direction = ifelse(is.na(direction), "no direction",direction)) %>%
  mutate(x_dir = data$x_dir) %>%
  mutate(x_dir = ifelse(is.na(x_dir), "no direction",x_dir)) %>%
  mutate(y_dir = data$y_dir) %>%
  mutate(y_dir = ifelse(is.na(y_dir), "no direction",y_dir)) %>%
  select(-c(next_px_1_x,next_px_1_y,next_px_2_x,next_px_2_y))

#only do the following for worms that have 2 ends
if(length(data_w_angles[data_w_angles$is_end == "YES","is_end"]) == 2){
  #start from one end (because we ordered by is_end above)
  pos_list <- data_w_angles[1, "pos"]


  # looks for the next position until the next end is found
  #if there is a round structure this can go into a endless loop, therefore don't make the list longer than the total number of pixels with value == 1
  while(tail(pos_list, n=1) != "0,0" && length(pos_list) < length(data_w_angles$value)){
    next_pos <- data_w_angles[which(data_w_angles$pos == tail(pos_list, n=1)),"next_pos_1"]
    if(next_pos %in% pos_list){
      pos_list <- append(pos_list, data_w_angles[which(data_w_angles$pos == tail(pos_list, n=1)),"next_pos_2"])
    }else{
      pos_list <- append(pos_list, next_pos)
    }
    if(length(unique(pos_list)) != length(pos_list)){
      pos_list <- NA
      break
    }
  }



  #only save those if there are more than 26 pixels and smaller than 60
    if(length(pos_list) > 26 & length(pos_list) < 80){

    #order pixels in the right order
      data_ordered <- data_w_angles[match(pos_list, data_w_angles$pos),] %>%
        na.omit

    #downsample to 26
      knots_Y <- linspace(data_ordered$Y[1],data_ordered$Y[nrow(data_ordered)],nrow(data_ordered))
      knots_X <- linspace(data_ordered$X[1],data_ordered$X[nrow(data_ordered)],nrow(data_ordered))
      tryCatch({
        cfit <<- curvefit(knots_X, data_ordered$X, data_ordered$Y,8)
        px <- c(cfit$px)
        py <- c(cfit$py)
        downsampled_knots <<- linspace(data_ordered$X[1],data_ordered$X[nrow(data_ordered)],26)
      },
      error = function(e){
        cfit <<- "no X fit"
      })

     if (!is.list(cfit)){
       tryCatch({
        cfit <<- curvefit(knots_Y, data_ordered$X, data_ordered$Y,8)
        px <- c(cfit$px)
        py <- c(cfit$py)
        downsampled_knots <<- linspace(data_ordered$Y[1],data_ordered$Y[nrow(data_ordered)],26)
        },
        error  = function(e2){
          cfit <<- "no X and Y fit"
        })
      }

      if (!is.list(cfit)){
        data_w_angles %>%
          filter(NA) %>%
          add_row() %>%
          select(-c(pos,value,next_pos_1,next_pos_2)) %>%
          mutate(index=NA, angle = NA, dim_X = dim_X, dim_Y = dim_Y, prev_px_x = NA, prev_px_y = NA,next_px_x = NA, next_px_y = NA, diff_next_px_x = NA, diff_next_px_y = NA, diff_prev_px_x = NA, diff_prev_px_y = NA, is_head = NA, is_tail = NA, dataset_ID = dataset_name, tp = tp_name, frame = frame_number, TrackID = TrackID_name,TrackCheck = NA) %>%
          mutate(status = "error when fitting polynomial")
      } else {
        xs <- polyval(px, downsampled_knots)
        ys <- polyval(py, downsampled_knots)

        #get first and last position to is_end == "YES" (order should be the same as before so we can assume end of dataframe=end of worm)
        data_ordered_downsampled <- data.frame(cbind(xs,ys))
        data_ordered_downsampled$is_end <- NA
        data_ordered_downsampled$is_end[1] <- "YES"
        data_ordered_downsampled$is_end[26] <- "YES"

        #add index number for each pixel
        data_ordered_downsampled %>%
          rename("X" = 1, "Y" = 2) %>%
          mutate(index = row_number()) %>%
          mutate(next_px_x = lead(X,n=1), next_px_y = lead(Y,n=1)) %>%
          mutate(prev_px_x = lag(X,n=1),prev_px_y = lag(Y,n=1)) %>%
          mutate(diff_prev_px_x =  X - prev_px_x, diff_prev_px_y = Y - prev_px_y) %>%
          mutate(diff_next_px_x = next_px_x - X, diff_next_px_y = next_px_y - Y) %>%
          mutate(angle=suppressWarnings(as.numeric(mapply(angle2,diff_prev_px_x,diff_prev_px_y,diff_next_px_x,diff_next_px_y))))%>%
          mutate(dataset_ID = dataset_name) %>%
          mutate(tp =tp_name) %>%
          mutate(frame = frame_number, dim_X = dim_X, dim_Y = dim_Y) %>%
          mutate(TrackID = TrackID_name) %>%
          mutate(is_head = NA) %>%
          mutate(is_tail = NA) %>%
          mutate(TrackCheck = TrackID_name) %>%
          mutate(direction = data$direction) %>%
          mutate(x_dir = data$x_dir) %>%
          mutate(y_dir = data$y_dir) %>%
          mutate(status = "downsampled to 26 pixels with 2 ends")
      }
    } else {
      data_w_angles %>%
        filter(NA) %>%
        add_row() %>%
        select(-c(pos,value,next_pos_1,next_pos_2)) %>%
        mutate(index=NA, angle = NA, dim_X = dim_X, dim_Y = dim_Y, prev_px_x = NA, prev_px_y = NA,next_px_x = NA, next_px_y = NA, diff_next_px_x = NA, diff_next_px_y = NA,diff_prev_px_x = NA, diff_prev_px_y = NA, is_head = NA, is_tail = NA, dataset_ID = dataset_name, tp = tp_name, frame = frame_number, TrackID = TrackID_name,TrackCheck = NA) %>%
        mutate(status = "under 26 px")
    }
  } else {
    data_w_angles %>%
      filter(NA) %>%
      add_row() %>%
      select(-c(pos,value,next_pos_1,next_pos_2)) %>%
      mutate(index=NA, angle = NA, dim_X = dim_X, dim_Y = dim_Y, prev_px_x = NA, prev_px_y = NA,next_px_x = NA, next_px_y = NA, diff_next_px_x = NA, diff_next_px_y = NA,diff_prev_px_x = NA, diff_prev_px_y = NA, is_head = NA, is_tail = NA, dataset_ID = dataset_name, tp = tp_name, frame = frame_number, TrackID = TrackID_name,TrackCheck = NA) %>%
      mutate(status = "not 2 ends")
  }
}


skeleton_head <- function(data){
  #if for that frame we have an idea about the direction in which the worm moves (see above), we will try finding a head structure based on this
  if(!is.na(unique(data$direction))){

    #for this frame get moving direction
    x_dir <- unique(data$x_dir)
    y_dir <- unique(data$y_dir)
    #get only pixels that describe the end of the worm
    worm_ends <- filter(data, is_end == "YES")

     #here depending on the direction in which the worm moves in the field of view (for X= right or left, for Y=up or down) we translate this information to the bitmask and identify the pixel that is closest to this side
     if (x_dir == "right" && y_dir == "up"){
       data <- data %>%
         mutate(is_head = NA) %>%
         mutate(is_head = ifelse(X == max(worm_ends$X) & Y == min(worm_ends$Y)  & is_end == "YES","YES", NA))

    } else if (x_dir == "left" && y_dir == "up"){
      data <- data %>%
        mutate(is_head = NA) %>%
        mutate(is_head = ifelse(X == min(worm_ends$X) & Y == min(worm_ends$Y)  & is_end == "YES","YES", NA))

    } else if (x_dir == "right" && y_dir == "down"){
      data <- data %>%
        mutate(is_head = NA) %>%
        mutate(is_head = ifelse(X == max(worm_ends$X) & Y == max(worm_ends$Y)  & is_end == "YES","YES", NA))

      } else if (x_dir == "left" && y_dir == "down"){
      data <- data %>%
        mutate(is_head = NA) %>%
        mutate(is_head = ifelse(X == min(worm_ends$X) & Y == max(worm_ends$Y)  & is_end == "YES","YES", NA))
    }

    #if head is at then end then switch index positions
    if(!is.na(data[nrow(data),"is_head"])){
      data$index <- nrow(data):1
    }
    
  if(all(is.na(data$is_head))){
    data <- data %>%
      mutate(status = "Head position unclear.")  
  } else {
    data <- data %>%
      mutate(status = "Head detected.")
  }

   
  } else {
  data <- data
  }
}


skeleton_head_estimate <- function(data) {
  n <- filter(data, head_detected == "NO") %>%
    select(frame) %>%
    distinct() %>%
    pull()
  
  ny <- filter(data,head_detected == "not yet") %>%
    select(frame) %>%
    distinct() %>%
    pull()
  
  
  
  for (i in ny){
    #detect frames where head is detected (for particular track)
    
    s <- filter(data,!is.na(is_head)) %>%
      select(frame) %>%
      distinct() %>%
      pull()
    
    #list of adjacent frames to ones with head
    list_of_frames <- c(setdiff(s-1,s),setdiff(s+1,s))
    list_of_frames_to_be_detected <- setdiff(list_of_frames,n)
    if (length(list_of_frames_to_be_detected) > 0){
      f <- list_of_frames_to_be_detected[1]
      #select frame where to estimate head
      adjacent_f <- s[which(abs(s-f)==min(abs(s-f)))][1]
      
      
      XX <- filter(data, frame == adjacent_f & is_head == "YES") %>% pull(X)
      YY <- filter(data, frame == adjacent_f & is_head == "YES") %>% pull(Y)
      
      data <- data %>%
        mutate(distance_to_latest_head=if_else(frame == f  & !is.na(is_end) ,suppressWarnings(as.numeric(mapply(distance,X,Y,XX,YY))),NA_real_ )) %>%
        mutate(is_head = if_else(distance_to_latest_head == suppressWarnings(min(distance_to_latest_head,na.rm=TRUE)) & frame == f, "YES", is_head)) %>%
        mutate(distance_to_latest_head = NA) %>%
        mutate(head_detected=if_else(frame == f,"YES",head_detected)) %>%
        mutate(status=if_else(frame == f,"Head estimated",status))
    }else{
      data <- data %>%
        mutate(status = if_else(head_detected == "not yet","no head estimation possible",status)) %>%
        mutate(head_detected = if_else(status == "no head estimation possible","NO", head_detected)) %>%
        as_tibble()
      break
    }
  }
}
# skeleton_head_propagation <- function(data){
# #if we do not have a good indication about the direction in this frame, we try to get it from the last frame
# first <- 
  
  
  
# if (!all(is.na(skeleton_data_all[skeleton_data_all$frame == f-1 & skeleton_data_all$TrackID == t,"is_head"]))){
#   latest_head <- filter(skeleton_data_all, tp == timepoint & frame == f-1 & TrackID == t & is_head == "YES")
#                         if (nrow(latest_head) == 0){
#                           print(paste0(timelapse,"_","tp_",timepoint,"_",t,"_","frame","_",f," ","no direction, no previous found"))
#                           skeleton_data_all <<- rbind(skeleton_data_all, data_ordered_for_plotting)
#                         } else {
#                           latest_head_x <- latest_head$X
#                           latest_head_y <- latest_head$Y
#                           #here we are looking for the closest pixel that is also an end in the actual frame with respect to the previous frame
#                           closest <- data_ordered_for_plotting %>%
#                             mutate(latest_head_x = latest_head_x) %>%
#                             mutate(latest_head_y = latest_head_y) %>%
#                             mutate(distance_to_latest_head=suppressWarnings(as.numeric(mapply(distance,X,Y,latest_head_x,latest_head_y)))) %>%
#                             filter(is_end == "YES") %>%
#                             mutate(is_head = ifelse(distance_to_latest_head == min(distance_to_latest_head),"YES",NA)) %>%
#                             filter(is_head == "YES")
# 
# 
#                           data_ordered_for_plotting$is_head[closest$index] <- "YES"
# 
#                           if(!is.na(data_ordered_for_plotting[nrow(data_ordered_for_plotting),"is_head"])){
#                             data_ordered_for_plotting$index <- nrow(data_ordered_for_plotting):1
#                           }
# 
#                           print(paste0(timelapse,"_","tp_",timepoint,"_",t,"_","frame","_",f," ","no direction, head estimated from previous frame"))
#                           skeleton_data_all <<- rbind(skeleton_data_all, data_ordered_for_plotting)
# 
#                         }
# 
#                       } else {
#                         #but only it that last frame contains a head. if not (i.e. if that was missegmented), don't
#                         print(paste0(timelapse,"_","tp_",timepoint,"_",t,"_","frame","_",f," ","no direction, no previous found"))
#                         skeleton_data_all <<- rbind(skeleton_data_all, data_ordered_for_plotting)
# 
#                       }
#                     }
#                   }
# 
#                
# }