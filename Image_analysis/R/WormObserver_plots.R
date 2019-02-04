rm(list = ls())

#load plyr before dplyr otherwise there will be an issue with the summarise function in dplyr
library("plyr")
library("dplyr")
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
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
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

target_folder_general <- c("/media/ella/raid5/timelapses/2018-12-07_16-00-07")
target_folder_analysis <- file.path(target_folder_general, "analysis")
target_folder_analysis_csvs <- list.files(target_folder_analysis,pattern=".+\\.csv", full.names = TRUE)


#### This is the for loop that pastes all .csv files together
##### _Additionally, time differences are calculated and transformed to minutes
#load metadata
metadata_file <- list.files(target_folder_general,pattern=".+\\_metadata.txt", full.names = TRUE)
metadata <- data.frame(sapply(metadata_file, function (x) readLines(x, warn=FALSE)), stringsAsFactors = FALSE)
colnames(metadata) <- as.character(gsub(".+\\/([0-9]+\\-[0-9]+\\-[0-9]+\\_[0-9]+\\-[0-9]+\\-[0-9]+)\\/.+", "\\1",target_folder_analysis))

time_elapsed <- apply(metadata,2, function(x) as.numeric(gsub(".+\\s([0-9]+).*", "\\1",x[grepl("Time elpased.+\\:\\s(.+)",x)])))
time_elapsed <- as.data.frame(time_elapsed)
time_elapsed$dataset <- gsub(".+\\/([0-9]+\\-[0-9]+\\-[0-9]+\\_[0-9]+\\-[0-9]+\\-[0-9]+)\\/.+", "\\1",rownames(time_elapsed))


#timepoint length, in minutes
#attention to divide everything by 60, because this value is entered in seconds
timepoint_length <- apply(metadata,2, function(x) as.numeric(gsub(".+\\:\\s(.+)", "\\1",x[grepl("Timepoint length\\:\\s(.+)\\.",x)])))/60
timepoint_length <- as.data.frame(timepoint_length)
timepoint_length$dataset <- gsub(".+\\/([0-9]+\\-[0-9]+\\-[0-9]+\\_[0-9]+\\-[0-9]+\\-[0-9]+)\\/.+", "\\1",rownames(timepoint_length))

#timestep, in minutes
timestep_length <- apply(metadata,2, function(x) as.numeric(gsub(".+every(.+)minutes\\.", "\\1",x[grepl(".+every(.+)minutes\\.",x)])))
timestep_length <- as.data.frame(timestep_length)
timestep_length$dataset <- gsub(".+\\/([0-9]+\\-[0-9]+\\-[0-9]+\\_[0-9]+\\-[0-9]+\\-[0-9]+)\\/.+", "\\1",rownames(timestep_length))

#load all .csv files from "list" into one dataframe and calculate elapsed minutes per tp based on the metadata parameteres extracted before
#first write a function that reads in a .csv file
import <- function (file){
  ds <- read_csv(file,col_types = cols(.default = col_double(),TrackID = col_character(),frame = col_integer(),tp = col_integer()))
 #append column with file name
  ds$file_name <- file
 #extract the timelapse name from the folder structure
  ds$dataset_ID <- gsub(".+\\/([0-9]+\\-[0-9]+\\-[0-9]+\\_[0-9]+\\-[0-9]+\\-[0-9]+)\\/.+", "\\1",ds$file_name)
  return(ds)
}

#apply the function defined above to all csv files (from all timelapses)
data <- ldply(.data=target_folder_analysis_csvs, .fun=import) %>%
  #replace all whitespaces in column names with "_"
  rename_all(funs(gsub("\\s", "_",.))) %>%
  inner_join(., time_elapsed,by = c("dataset_ID" = "dataset")) %>%
  inner_join(., timepoint_length,by = c("dataset_ID" = "dataset")) %>%
  inner_join(., timestep_length,by = c("dataset_ID" = "dataset")) %>%
  mutate(minutes = time_elapsed + ((tp-1)*timepoint_length+(tp-1)*timestep_length))


#### Calculate vector angles
##### from https://stackoverflow.com/questions/1897704/angle-between-two-vectors-in-r


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
# this takes a while...
#calculate_overview_statistics(offset,max_number_gaps,duration)
#angle between 1s, only minimum 10s tracked
#calculate_overview_statistics(25, 250)

#function(offset,selected_minute,which_dataset,max_number_gaps,duration,which_frame,color_path,with_image) 
#plot_image_with_path(25, 100, "2018-12-07_16-00-07",2,250,10,"frame","with_image")
#plot_image_with_path(25, 100, "2018-12-07_16-00-07",2,250,10,"no_mean","with_image")
plot_image_with_path(25, 200, "2018-12-07_16-00-07",5,0,10,"sd_local_angle","with_image")
#plot_image_with_path(25, 100, "2018-12-07_16-00-07",2,250,10,"mean","with_image")
#plot_image_with_path(25, 100, "2018-12-07_16-00-07",2,250,10,"ID","with_image")
#plot_image_with_path(25, 100, "2018-12-07_16-00-07",2,250,10,"displacement","with_image")
#plot_image_with_path(25, 100, "2018-12-07_16-00-07",2,250,10,"velocity","with_image")
#plot_image_with_path(25, 100, "2018-12-07_16-00-07",2,250,10,"length","with_image")
#plot_image_with_path(25, 200, "2018-12-07_16-00-07",2,250,10,"local_velocity","with_image")
plot_image_with_path(25, 200, "2018-12-07_16-00-07",2,250,10,"sd_local_velocity","with_image")


# test angle function
xv <- c(20,40,30)
yv <- c(40,40,60)
test <- data.frame(xv,yv)

test %>%
  mutate(x_lag=xv[2] - xv[1], y_lag = yv[2] - yv[1]) %>%
  mutate(x_lead=xv[3]-xv[2], y_lead=yv[3]-yv[2]) %>%
  mutate(angle=180-as.numeric(mapply(angle,x_lag,y_lag,x_lead,y_lead))*180/pi)

data %>%
  mutate(x_lag=location_x - lag(location_x,n=offset), y_lag = location_y -lag(location_y, n=offset)) %>%
  mutate(x_lead=lead(location_x, n=offset)-location_x, y_lead=lead(location_y, n=offset)-location_y) %>%
  mutate(local_velocity=suppressWarnings(as.numeric(mapply(distance,x_lag,y_lag,x_lead,y_lead))))

################################################################################

ui <- fixedPage(
           titlePanel("WormObserver analysis."),
           selectInput(inputId = "selected_dataset",
                       label="Which dataset?",
                       choices=unique(data$dataset_ID)),
           h4(textOutput("timepoint_number")),
           h4(textOutput("frame_number")),
           fluidRow(
             column(8,
                    plotOutput(outputId = "pathPlot")
            )
            ),
           br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
           textOutput("click_info"),
           br(),
           fluidRow(
                      column(6,
                             helpText("Select the timepoint you want to have a look at (in minutes)."),
                             br(),
                             uiOutput("timepoint_slider"),
                      br(),
                      helpText("Select offset for angle calculation."),
                      br(),
                      numericInput(inputId = "selected_offset",
                                  label = "Offset",
                                  min = 1,
                                  max = 50,
                                  value = 25),
                      br(),
                      radioButtons("path_color", "Color the path by:",
                                   c("Track ID" = "ID",
                                     "Mean angle" = "mean_angle",
                                     "SD angle" = "sd_local_angle",
                                     "Local angle" = "angle",
                                     "Mean velocity" = "velocity",
                                     "SD velocity" = "sd_local_velocity",
                                     "Local velocity" = "local_velocity",
                                     "Track displacement" = "displacement",
                                     "Track length (s)" = "length",
                                     "Frame"= "frame"
                                     ))
                      ),
                      column(6,
                             helpText("Select the frame number."),
                             br(),
                             br(),
                             sliderInput(inputId = "selected_frame",
                                         label = "Frames",
                                         min = 1,
                                         max = max(data$frame),
                                         value = 10,
                                         ticks=FALSE),
                             helpText("Chose the minimal duration for paths to be shown (frames)."),
                            numericInput(inputId = "selected_duration",
                                         label = "Duration",
                                         min = 1,
                                         max = max(data$frame),
                                         value = 50),
                      br(),
                      numericInput(inputId = "max_number_gaps",
                                   label="Max. gaps in track",
                                   min=0,
                                   max=10,
                                   value=5)
                    )
             )
    )



server <- function(input, output,session) {
  
  #the timepoint slider is dynmaically render as it's maximum value is dependent on the chosen dataset
  output$timepoint_slider <- renderUI({
    data.sub <- data %>% filter(dataset_ID == input$selected_dataset) 
    sliderTextInput(inputId = "selected_minute",
              label = "Timepoint (minutes)",
              choices= sort(unique(data.sub$minutes))
    )
  })
  output$pathPlot <- renderPlot({
    selected_dataset <- input$selected_dataset
    max_number_gaps <- input$max_number_gaps
    path_color <- input$path_color
    selected_minute <- input$selected_minute
    selected_frame <-input$selected_frame
    selected_offset <- input$selected_offset
    selected_duration <- input$selected_duration
    plot_image_with_path(selected_offset,selected_minute,selected_dataset,max_number_gaps, selected_duration,selected_frame,path_color,"with_image")
  },height = 800, width = 800)
 
  #output$click_info <- renderPrint({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    #nearPoints(data_for_plotting,xvar="location_x",yvar="location_y", input$plot_click, threshold = 100, maxpoints = 1,addDist = TRUE)
 #})
  

  # output$timepoint_number <- renderText({
  #   data.sub <- data %>% filter(dataset_ID == input$selected_dataset)
  #   selected_minute <- input$selected_minute
  #   all_tps_in_minutes <- unique(data.sub$minutes)
  #   corresponding_minute <- all_tps_in_minutes[which(abs(all_tps_in_minutes-selected_minute)==min(abs(all_tps_in_minutes-selected_minute)))]
  #   paste("Minute shown:", corresponding_minute)
  #   
  # })
  
  output$frame_number <- renderText({
    selected_frame <- input$selected_frame
    paste("Frame number shown:", selected_frame)
    
  })
  
}

shinyApp(ui, server)

