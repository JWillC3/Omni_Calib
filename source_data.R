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
#Omni .csv data files location
omni_dir <- "data/omni_data"

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

#List of all Omni .csv
omni_files <- list.files(path = omni_dir, pattern = "\\.csv$",
                         full.names = TRUE)

#Apply the function to all Omni .csv files and create new df
omni_master <- map_dfr(omni_files, read_and_clean_omni)

#----
#Batch plot functions
p_chamber_batch <- function(data, test_end, title_suffix) {
  # Ensure Chamber_Step is a factor in correct order
  data <- data %>%
    mutate(Chamber_Step = factor(Chamber_Step, levels = c("150.4",
                                                          "55.4",
                                                          "35.4",
                                                          "12",
                                                          "0")))
  # Get step window bands from data
  step_bands <- data %>%
    group_by(Chamber_Step) %>%
    summarise(xmin = min(time), xmax = max(time), .groups = "drop") %>%
    filter(xmin <= test_end)
  
  # Define color palette for chamber steps
  step_colors <- c(
    "150.4" = "#d8bce6",  # light purple
    "55.4"  = "#f4cccc",  # light red
    "35.4"  = "#fce5cd",  # light orange
    "12"    = "#fff2cc",  # light yellow
    "0"     = "#d9ead3"   # light green
  )
  
  # Create plot
  p <- ggplot(data, aes(x = time, y = pm25, color = device_id)) +
    geom_rect(data = step_bands, inherit.aes = FALSE,
              aes(xmin = xmin, xmax = pmin(xmax, test_end), ymin = -Inf,
                  ymax = Inf, fill = Chamber_Step), alpha = 0.5) +
    geom_line(linewidth = 1.5, alpha = 0.5) +
    scale_fill_manual(values = step_colors, guide = "none") +
    scale_color_viridis_d(option = "turbo") +
    scale_x_datetime(
      limits = c(min(data$time), test_end),
      breaks = function(x) {
        times <- unique(c(x[1], step_bands$xmin, step_bands$xmax, test_end))
        sort(times[times >= x[1] & times <= x[2]])
      },
      labels = function(x) {
        ifelse(format(x, "%H:%M:%S") == "00:00:00",
               format(x, "%b %d"),
               format(x, "%H:%M"))
      },
      expand = c(0, 0)
    ) +
    labs(title = paste("PM 2.5 Chamber Test:", title_suffix), x = "Time",
      y = "Concentration (µg/m³)", color = "Device_ID") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "gray90"),
      plot.title = element_text(face = "bold"), legend.position = "right",
      legend.box.background = element_rect(color = "black", linewidth = 0.5),
      legend.box.margin = margin(5, 5, 5, 5),
      axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}



