

target_folder_general <- c("/media/ella/raid5/timelapses/2019-01-22_15-48-35")
target_folder_analysis <- file.path(target_folder_general, "analysis")
target_folder_analysis_csvs <- list.files(target_folder_analysis,pattern=".+\\.csv", full.names = TRUE)


#### This is the for loop that pastes all .csv files together
##### _Additionally, time differences are calculated and transformed to minutes
#load metadata
metadata_file <- list.files(target_folder_general,pattern=".+\\_metadata.txt", full.names = TRUE)
metadata <- data.frame(sapply(metadata_file, function (x) readLines(x, warn=FALSE)), stringsAsFactors = FALSE)
colnames(metadata) <- as.character(gsub(".+\\/([0-9]+\\-[0-9]+\\-[0-9]+\\_[0-9]+\\-[0-9]+\\-[0-9]+)\\/.+", "\\1",target_folder_analysis))

time_elapsed <- apply(metadata,2, function(x) as.numeric(gsub(".+\\s([0-9]+).*", "\\1",x[grepl("Time elapsed.+\\:\\s(.+)",x)])))
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

