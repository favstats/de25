library(tidyverse)
library(tools) # For file hashing
library(glue)

# "data/media/vid_hash//ff9c7c36f665757db1606b182543bcbe.mp4" %>% 
#   str_split("/") %>% 
#   unlist() %>% 
#   .[5] 

process_video <- function(video_path, output_dir) {
  # Check if video file exists
  if (!file.exists(video_path)) {
    stop("The specified video file does not exist.")
  }
  
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  video_hash <- str_split(video_path, "/") %>% 
    unlist() %>% 
    .[5] %>% 
    str_remove("\\.mp4")
  
  # Extract video duration
  video_info <- system(glue("ffmpeg -i '{video_path}' 2>&1"), intern = TRUE)
  duration_line <- grep("Duration: ", video_info, value = TRUE)
  
  if (length(duration_line) == 0) {
    stop("Could not extract video duration. Ensure the video is valid and ffmpeg is installed.")
  }
  
  duration <- sub(".*Duration: ([0-9:.]+),.*", "\\1", duration_line)
  time_parts <- as.numeric(strsplit(duration, ":")[[1]])
  
  if (length(time_parts) != 3) {
    stop("Invalid duration format. Expected HH:MM:SS.")
  }
  
  total_seconds <- sum(time_parts * c(3600, 60, 1))
  
  # Generate 3 random frame timestamps
  frame_timestamps <- sort(sample(seq(0, total_seconds - 1, by = 1), 3))
  
  # Extract 3 frames at the selected timestamps
  frame_files <- paste0(output_dir, "/", video_hash, "_", seq_along(frame_timestamps), ".png")
  ffmpeg_commands <- map2(
    frame_timestamps, frame_files,
    ~ glue("ffmpeg -ss {.x} -i '{video_path}' -vframes 1 '{.y}' -hide_banner -loglevel error")
  )
  
  # Execute the commands
  walk(ffmpeg_commands, ~ system(.x))
  
  # Return the paths to the sampled frames
  tibble(
    video_hash = video_hash,
    frame_number = seq_along(frame_timestamps),
    frame_path = frame_files
  )
}

library(glue)

dir("data/media/vid_hash/", full.names = T) %>% 
  # .[1] %>% 
  walk(~process_video(.x, "output_frames2"), .progress = T)

# # Example usage
# result <-
# print(result)

