#----
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
library(purrr)


#----
#read Sheets
sheets_data <- read_sheet("https://docs.google.com/spreadsheets/d/1vqvEg71AyTTMJ6N4CC7I44UeyPgq4c12NMA-rjXQj7o/edit?gid=763198327#gid=763198327",
                   "Calibration", col_types = "c")
sheets_data <- sheets_data %>%
  mutate(Start_DateTime = parse_date_time(Start_DateTime,
                  orders = c("mdy HM", "mdy HMS", "mdy IMp", "mdy"),
                  tz = "America/Denver"),
         End_DateTime = parse_date_time(End_DateTime,
                  orders = c("mdy HM", "mdy HMS", "mdy IMp", "mdy"),
                  tz = "America/Denver"))

#Function to read and clean Omni files
read_and_clean_omni <- function(file_path) {
  #Extract device_id from filename
  device_id <- str_extract(basename(file_path), "^[^_]+")
  
  #Read device .csv
  df <- read_csv(file_path, show_col_types = FALSE) %>%
    select(-any_of(c("score", "pm10"))) %>%  #Remove unwanted columns
    rename(time = `timestamp(America/Denver)`) %>%
    mutate(device_id = device_id, .before = 1)  #Add device_id
  
  return(df)
}

#Omni data files location
omni_dir <- "data/omni_data"

#List of all Omni .csv
omni_files <- list.files(path = omni_dir, pattern = "\\.csv$",
                         full.names = TRUE)

#Apply the function to all Omni files and create new df
omni_master <- map_dfr(omni_files, read_and_clean_omni)

#Convert time zone
omni_master <- omni_master %>%
  mutate(time = with_tz(time, tzone = "America/Denver"))

#----
#Trim master by Google Sheets times
#Return only the rows from omni_master that fall within chamber step periods
omni_data_trim <- function(omni_master, sheets_data) {
  sheets_data <- sheets_data %>%
    mutate(Start_DateTime_floor = floor_date(Start_DateTime,
                                             unit = "5 minutes"),
      End_DateTime_ceil = ceiling_date(End_DateTime, unit = "5 minutes"))
  
  #Join and filter for rows within chamber test step timeframes
  omni_with_step <- omni_master %>%
    left_join(sheets_data %>%
        select(Device_ID, Chamber_Step, Start_DateTime_floor,
               End_DateTime_ceil),
      by = c("device_id" = "Device_ID")) %>%
    filter(time >= Start_DateTime_floor & time <= End_DateTime_ceil) %>%
    mutate(step = Chamber_Step) %>%
    select(-Start_DateTime_floor, -End_DateTime_ceil, -Chamber_Step)
  
  return(omni_with_step)
}

omni_master_trimmed <- omni_data_trim(omni_master, sheets_data)
#----



