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

# Function to read and clean a single file
read_and_clean_omni <- function(file_path) {
  # Extract device_id from filename
  device_id <- str_extract(basename(file_path), "^[^_]+")
  
  # Read the CSV file
  df <- read_csv(file_path, show_col_types = FALSE) %>%
    select(-any_of(c("score", "pm10"))) %>%  # Remove unwanted columns
    rename(time = `timestamp(America/Denver)`) %>%
    mutate(device_id = device_id, .before = 1)  # Add device_id as first column
  
  return(df)
}

# Directory where Omni CSVs are stored
omni_dir <- "data/omni_data"

# Get list of all CSV files
omni_files <- list.files(path = omni_dir, pattern = "\\.csv$",
                         full.names = TRUE)

# Apply the function to all files and combine into one dataframe
omni_master <- map_dfr(omni_files, read_and_clean_omni)


#----
#Trim master by Google Sheets times



