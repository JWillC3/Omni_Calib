#-----------
# Libraries
library(tidyverse)
library(plyr)
library(faraway)
library(cowplot)
library(reshape2)
library(corrplot)
library(scales)
library(ggthemes)
library(gridExtra)
library(patchwork)
library(gghighlight)
library(ggdark)
library(viridis)
library(DT)
library(plotly)
library(readxl)
library(magick)
library(corrr)
library(googlesheets4)
library(grid)
library(knitr)
library(lubridate)


#-----------
#read Sheets
sheets_data <- read_sheet("https://docs.google.com/spreadsheets/d/1vqvEg71AyTTMJ6N4CC7I44UeyPgq4c12NMA-rjXQj7o/edit?gid=763198327#gid=763198327",
                   "Calibration", col_types = "c")

#read Omni data
omni_dir <- "data/omni_data" #path for omni data

omni_files <- list.files(path = omni_dir, pattern = "\\.csv$",
                         full.names = TRUE)

# Function to process each Omni CSV
process_omni_file <- function(file_path, sheets_data) {
  # Extract device_id from filename
  filename <- basename(file_path) #START HERE WHEN YOU RETURN!!!
  device_id <- str_extract(basename(file_path), "^\\d+")
  
  # Read the Omni CSV
  df <- read_csv(file_path)
  
  # Rename timestamp column to 'time'
  names(df)[1] <- "time"
  
  # Convert 'time' to POSIXct
  df <- df %>%
    mutate(time = mdy_hm(time, tz = "America/Denver"),
           device_id = device_id)
  
  # Match to corresponding row in sheets_data
  sheet_row <- sheets_data %>% filter(Device_ID == device_id)
  
  if (nrow(sheet_row) == 0) {
    warning(paste("No match found in sheets_data for device", device_id))
    return(NULL)
  }
  
  # Combine date and time into POSIXct start/end values
  start_time <- mdy_hm(paste(sheet_row$Start_Date, sheet_row$Start_Time), tz = "America/Denver")
  end_time <- mdy_hm(paste(sheet_row$End_Date, sheet_row$End_Time), tz = "America/Denver")
  
  # Filter data between start and end time
  df_filtered <- df %>%
    filter(time >= start_time, time <= end_time)
  
  return(df_filtered)
}

# Process all Omni files and combine
omni_master <- map_dfr(omni_files, ~process_omni_file(.x, sheets_data))

#read reference sensor data
