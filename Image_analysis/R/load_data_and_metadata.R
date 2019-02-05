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


#apply the import function defined above to all csv files (from all timelapses)
data <- ldply(.data=target_folder_analysis_csvs, .fun=import) %>%
  #replace all whitespaces in column names with "_"
  rename_all(funs(gsub("\\s", "_",.))) %>%
  inner_join(., time_elapsed,by = c("dataset_ID" = "dataset")) %>%
  inner_join(., timepoint_length,by = c("dataset_ID" = "dataset")) %>%
  inner_join(., timestep_length,by = c("dataset_ID" = "dataset")) %>%
  mutate(minutes = time_elapsed + ((tp-1)*timepoint_length+(tp-1)*timestep_length))

