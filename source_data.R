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

#----
#Function to read in Omni files
read_and_clean_omni <- function(file_path) {
  #Extract device_id from filename
  device_id <- str_extract(basename(file_path), "^[^_]+")
  
  #Read .csv and force timestamp as character
  df <- read_csv(file_path, show_col_types = FALSE, 
                 col_types = cols(
                   `timestamp(America/Denver)` = col_character())
                 ) %>%
    select(-any_of(c("score", "pm10"))) %>%
    mutate(device_id = device_id,time_raw = `timestamp(America/Denver)`,
      
      #Account for multiple date time formats from Omni .csv data
      time = parse_date_time(time_raw, orders = c("ymd HMS", "ymd HM",
                                                  "mdy HMS", "mdy HM",
                                                  "dmy HMS", "dmy HM"),
        tz = "America/Denver")) %>%
    relocate(device_id, time) %>%
    select(-time_raw, -`timestamp(America/Denver)`)
  
  return(df)
  
}

#Omni .csv data files location
omni_dir <- "data/omni_data"

#List of all Omni .csv
omni_files <- list.files(path = omni_dir, pattern = "\\.csv$",
                         full.names = TRUE)

#Apply the function to all Omni .csv files and create new df
omni_master <- map_dfr(omni_files, read_and_clean_omni)



