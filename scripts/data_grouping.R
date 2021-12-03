# Use this script after running the KNIME image analysis workflow for all datasets you want to analyse.
# Needed: A metadata file that is located in the parent folder (i.e. one directory above of the timelapses folder that contains the analysed datasets)
# This file should be named "timelapses_metadata.txt" and contain the information about the condition in the following format: dataset_ID condition
# To run the script type: Rscript data_grouping.R "the location of your timelapses folder", e.g. Rscript data_grouping.R /media/fpreuss/raid5/timelapses/analysis/paper/timelapses

# This script does four things:
# 1: for each experiment, it groups all timepoint .zip files (as given out by the KNIME workflow) and appends them to one large table
# 2: for each experiment, it appends the metadata information from the metadata.txt file (that is created on the Raspberry Pi at the beginning of the acquisition) to the table
# 3: All experiments of the same condition (as indicated by the timelapses_metadata.txt file) are grouped together
# 4: For each condition, one RDS file with the ending "_raw_data.rds" is created.

library("tidyverse")
library("data.table")

#get file path from command line
timelapses_file_path <- commandArgs(trailingOnly = TRUE)[1]
#create file path for saving
save_path <- file.path(dirname(timelapses_file_path),"data","raw")
#catch timelapses_metadata.txt location
metadata_file_path <- file.path(file.path(dirname(timelapses_file_path), "timelapses_metadata.txt"))
#create the save path directories
dir.create(save_path)

#extract list of datasets to process from "timelapses_metadata.txt" file
list_of_datasets <- as.matrix(read.table(metadata_file_path))
# print(list_of_datasets)

colnames(list_of_datasets) <- c("dataset_ID", "annotation")

nested_list_of_datasets <- list_of_datasets %>%
  as.data.frame(.) %>%
  nest(data=c(dataset_ID))


for (row in 1:nrow(nested_list_of_datasets)){
  target_folder_general <-as.character(pull(unnest(nested_list_of_datasets[row,2],cols=c(data))))
  annotation <- as.character(pull(nested_list_of_datasets[row,1]))
  cat("\n\nCurrently processing ", annotation, "\n\n")
  target_folder_analysis <- file.path(timelapses_file_path, target_folder_general, "analysis")

  #load metadata (for all datasets)
  metadata_file <- list.files(file.path(timelapses_file_path,target_folder_general),pattern=".+\\_metadata.txt$", full.names = TRUE)
  metadata <- data.frame(sapply(metadata_file, function (x) read.delim(x,header=FALSE,check.names = FALSE)))
  colnames(metadata) <- metadata_file
  colnames(metadata) <- as.character(gsub(".+\\/", "",colnames(metadata)))
  colnames(metadata) <- as.character(gsub("_metadata.txt", "",colnames(metadata)))
  time_elapsed <- apply(metadata,2, function(x) as.numeric(gsub(".+\\s([0-9]+).*", "\\1",x[grepl("Time elapsed.+\\:\\s(.+)",x)])))
  time_elapsed <- as.data.frame(time_elapsed)
  time_elapsed$dataset <- colnames(metadata)


  #timepoint length, in minutes
  #attention to divide everything by 60, because this value is entered in seconds
  timepoint_length <- apply(metadata,2, function(x) as.numeric(gsub(".+\\:\\s(.+)", "\\1",x[grepl("Timepoint length\\:\\s(.+)\\.",x)])))/60
  timepoint_length <- as.data.frame(timepoint_length)
  timepoint_length$dataset <- colnames(metadata)


  #timestep, in minutes
  timestep_length <- apply(metadata,2, function(x) as.numeric(gsub(".+every(.+)minutes\\.", "\\1",x[grepl(".+every(.+)minutes\\.",x)])))
  timestep_length <- as.data.frame(timestep_length)
  timestep_length$dataset <- colnames(metadata)

  #plate type
  plate_type <- apply(metadata,2, function(x) gsub(".+\\,(.+)\\,.+", "\\1",x[grepl("Timelapse plate type\\:",x)]))
  plate_type <- as.data.frame(plate_type)
  plate_type$dataset <- colnames(metadata)

  #worm type
  worm_type <- apply(metadata,2, function(x) gsub(".+\\:\\s(.+)", "\\1",x[grepl("Used line\\:",x)]))
  worm_type <- as.data.frame(worm_type)
  worm_type$dataset <- colnames(metadata)


  #list zip files in target_folder_analysis
  find_zip_files <- function(target_folder_analysis){
    files <- sort(as.numeric(gsub("(\\d+)\\..+","\\1",list.files(target_folder_analysis,pattern=".zip"))))
    file.path(target_folder_analysis,paste0(files, ".csv.zip"))
  }


  imported_data <-  unlist(map(target_folder_analysis,find_zip_files)) %>%
    #we select only certain columns to be read in to reduce memory usage
    map_df(.,function(x) fread(cmd=paste0("unzip -cq ", x),head=TRUE, select=c(1:23,37:41, 44:47)) %>%
             mutate(file_name=x)) %>%
    mutate(dataset_ID = gsub(".+\\/([0-9]+\\-[0-9]+\\-[0-9]+\\_[0-9]+\\-[0-9]+\\-[0-9]+)\\/.+", "\\1",file_name)) %>%
    #replace all whitespaces in column names with "_"
    rename_all(list(~gsub("\\s", "_",.))) %>%
    inner_join(., time_elapsed,by = c("dataset_ID" = "dataset")) %>%
    inner_join(., timepoint_length,by = c("dataset_ID" = "dataset")) %>%
    inner_join(., timestep_length,by = c("dataset_ID" = "dataset")) %>%
    inner_join(., plate_type, by = c("dataset_ID" = "dataset")) %>%
    inner_join(., worm_type, by = c("dataset_ID" = "dataset")) %>%
    mutate(timestep_length = ifelse(timepoint_length > timestep_length, 0,timestep_length-timepoint_length)) %>%
    mutate(minutes = time_elapsed + ((tp-1)*timepoint_length+(tp-1)*timestep_length)) %>%
    na.omit() %>%
    mutate(annotation = annotation) %>%
    select(c(TrackID, Number_of_gaps, Mean_velocity, Maximal_velocity, Minimal_velocity, Median_velocity,
             Velocity_standard_deviation, Duration_of_track, Track_start, Track_stop, Track_displacement,
             location_x, location_y, Size, Circularity, Prediction, Prediction_confidence,
             frame, recorded_fps, downsampled_to, bitmask, bitmask_dim_X, bitmask_dim_Y, tp, file_name,
             dataset_ID, time_elapsed, timepoint_length, timestep_length, plate_type, worm_type,
             minutes, annotation))


  annotation_underscored <- gsub(" ", "_", annotation)
  created_file_path <- file.path(save_path,paste0(annotation_underscored, "_raw_data.rds"))
  saveRDS(imported_data, file = created_file_path)
  cat(paste0("\n\ndatasets ",paste(unique(imported_data$dataset_ID), collapse=" & "), " were processed \nand saved under: ", created_file_path))
  rm(imported_data)
  gc()

}
