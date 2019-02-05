#functions

import <- function (file){
  ds <- read_csv(file,col_types = cols(.default = col_double(),TrackID = col_character(),frame = col_integer(),tp = col_integer()))
  #append column with file name
  ds$file_name <- file
  #extract the timelapse name from the folder structure
  ds$dataset_ID <- gsub(".+\\/([0-9]+\\-[0-9]+\\-[0-9]+\\_[0-9]+\\-[0-9]+\\-[0-9]+)\\/.+", "\\1",ds$file_name)
  return(ds)
}

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

distance <- function(x1, y1, x2, y2) {
  values <- list(x1, y1, x2, y2)
  if(anyNA(values)) {
    "NA"
  } else {
    length <- sqrt((x1-x2)^2+(y1-y2)^2)
    as.numeric(length)
  }
}


plot_image_with_path <- function(offset,selected_minute,which_dataset,max_number_gaps,duration,which_frame,color_path,with_image) {
  
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 180)) 
  data.sub <- data %>% filter(dataset_ID == which_dataset)
  all_tps_in_minutes <- unique(data.sub$minutes)
  corresponding_minute <- all_tps_in_minutes[which(abs(all_tps_in_minutes-selected_minute)==min(abs(all_tps_in_minutes-selected_minute)))]
  selected_tp <- unique(data[data$minutes == corresponding_minute,"tp"])
  data_for_plotting <<- data %>%
    filter(tp == selected_tp, dataset_ID == which_dataset) %>%
    filter(Number_of_gaps < max_number_gaps) %>%
    mutate(grouping = paste0(dataset_ID, "_",TrackID, "_", tp)) %>%
    filter(Duration_of_track>duration) %>%
    group_by(TrackID, tp, dataset_ID) %>%
    mutate(x_lag=location_x - lag(location_x,n=offset), y_lag = location_y -lag(location_y, n=offset)) %>%
    mutate(x_lead=lead(location_x, n=offset)-location_x, y_lead=lead(location_y, n=offset)-location_y) %>%
    mutate(angle=180-suppressWarnings(as.numeric(mapply(angle,x_lag,y_lag,x_lead,y_lead)))*180/pi) %>%
    mutate(local_velocity=suppressWarnings(as.numeric(mapply(distance,x_lag,y_lag,x_lead,y_lead)))) %>%
    group_by(TrackID) %>%
    na.omit() %>%
    mutate(mean_angle = mean(angle),sd_local_angle=sd(angle),displacement=mean(Track_displacement),mean_velocity=Mean_velocity,track_length_secs=mean(Duration_of_track/downsampled_to),local_velocity=local_velocity,sd_local_velocity=sd(local_velocity))
  
  p <- ggplot(data_for_plotting)+
    theme(aspect.ratio = 1024/1024)+
    coord_cartesian(ylim=c(0, 1023))+
    scale_x_continuous(expand = c(0, 0), limits= c(0,1023)) + scale_y_continuous(expand = c(0, 0),trans="reverse")+
    theme_void()+
    ggtitle(paste0("tp_",selected_tp))
  if(with_image == "with_image"){
    tp <- paste0("tp_",selected_tp)
    frame_png <- paste0(sprintf("%03d", which_frame),".png")
    location_dataset <- target_folder_general[grep(which_dataset, target_folder_general)]
    img <- readPNG(file.path(location_dataset,tp,frame_png))
    p <- p + background_image(img)
  }else{
    p
  }
  if(color_path == "mean_angle"){
    sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 180))
    p <- p +
      sc + 
      geom_path(aes(x=location_x, y=location_y,col=mean_angle, group=grouping),size=0.8, alpha=0.75)
  }else if(color_path == "ID"){
    p <- p +
      geom_path(aes(x=location_x, y=location_y,col=TrackID, group=grouping),size=0.8, alpha=0.75)+
      theme(
        legend.position = "none")
  }else if(color_path == "frame"){
    sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, max(data$frame)))
    p <- p +
      sc +
      geom_path(aes(x=location_x, y=location_y,col=frame, group=grouping),size=0.8, alpha=0.75)
  }else if(color_path == "displacement"){
    sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, max(data$Track_displacement)))
    p <- p +
      sc +
      geom_path(aes(x=location_x, y=location_y,col=displacement, group=grouping),size=0.8, alpha=0.75)
  }else if(color_path == "velocity"){
    sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, max(data$Mean_velocity)))
    p <- p +
      sc +
      geom_path(aes(x=location_x, y=location_y,col=mean_velocity, group=grouping),size=0.8, alpha=0.75)
  }else if(color_path == "length"){
    sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, max(data$Duration_of_track)/downsampled_to))
    p <- p +
      sc +
      geom_path(aes(x=location_x, y=location_y,col=length, group=grouping),size=0.8, alpha=0.75)
  }else if(color_path == "local_velocity"){
    sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, max(data_for_plotting$local_velocity)))
    p <- p +
      sc +
      geom_path(aes(x=location_x, y=location_y,col=local_velocity, group=grouping),size=0.8, alpha=0.75)
  }else if(color_path == "sd_local_velocity"){
    sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, max(data_for_plotting$sd_local_velocity)))
    p <- p +
      sc +
      geom_path(aes(x=location_x, y=location_y,col=sd_local_velocity, group=grouping),size=0.8, alpha=0.75)
  }else if(color_path == "sd_local_angle"){
    sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, max(data_for_plotting$sd_local_angle)))
    p <- p +
      sc +
      geom_path(aes(x=location_x, y=location_y,col=sd_local_angle, group=grouping),size=0.8, alpha=0.75)
  }else{
    sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 180))
    p <- p +
      sc+
      geom_path(aes(x=location_x, y=location_y,col=angle, group=grouping),size=0.8, alpha=0.75)
  }
  p
}


calculate_overview_statistics <- function(offset,max_number_gaps,duration) {
  dfp <<- data %>%
    mutate(grouping = paste0(dataset_ID, "_",TrackID, "_", tp)) %>%
    filter(Number_of_gaps < max_number_gaps) %>%
    filter(Duration_of_track>duration) %>%
    group_by(grouping) %>%
    mutate(angle=180-suppressWarnings(as.numeric(mapply(angle,x_lag,y_lag,x_lead,y_lead)))*180/pi) %>%
    group_by(grouping) %>%
    na.omit() %>%
    mutate(mean_angle = mean(angle), na.rm=TRUE) %>%
    group_by(TrackID, tp, minutes, dataset_ID,file_name,Duration_of_track) %>%
    summarise(mean_angle=mean(angle), mean_velocity=mean(Mean_velocity),mean_displacement=mean(Track_displacement))
  mutate(ang_velocity=mean_angle/Duration_of_track)
  
}