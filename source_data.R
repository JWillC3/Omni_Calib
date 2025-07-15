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


#-----------
#read omni data


#read Sheets
sheets_data <- read_sheet("https://docs.google.com/spreadsheets/d/1vqvEg71AyTTMJ6N4CC7I44UeyPgq4c12NMA-rjXQj7o/edit?gid=763198327#gid=763198327",
                   "Calibration", col_types = "c")

#read reference sensor data
