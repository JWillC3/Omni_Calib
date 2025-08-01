#load data
source("source_data.R")


#----
#Phase 1, match Omni date to batch number from Google Sheets

#Step 1: Get device IDs from batch 1
batch_1_ids <- sheets_data %>%
  filter(batch == "1") %>%
  pull(Device_ID)

#Step 2: Filter 'omni_master' to just batch 1 devices
omni_batch_1 <- omni_master %>%
  filter(device_id %in% batch_1_ids)
 unique(omni_batch_1$device_id)
 


#----
 #Phase 2, trim the Omni data to match time windows for chamber steps
 
 #Step 1: Get time windows for each chamber step
 step_windows <- sheets_data %>%
   filter(batch == "1") %>%
   group_by(Chamber_Step) %>%
   summarise(
     step_start = min(Start_DateTime),
     step_end = max(End_DateTime),
     .groups = "drop"
   )
 
 #Step 2: For each step, filter 'omni_batch_1' using the time window
 omni_steps_list <- step_windows %>%
   group_split(Chamber_Step) %>%
   map(~ {
     step_info <- .
     step_val <- step_info$Chamber_Step[1]
     start <- step_info$step_start[1]
     end <- step_info$step_end[1]
     
     #Filter Omni data to only this time window
     omni_batch_1 %>%
       filter(time >= start, time <= end) %>%
       mutate(Chamber_Step = step_val)
   })
 
 # Step 3: Combine all steps back into one df
 omni_trimmed <- bind_rows(omni_steps_list) 
 
 
#----
#Function to trim Omni data to match chamber batches and step times
 trim_omni <- function(omni_data, sheet_data, batch_number) {
   
   #Step 1: Get device IDs for the batches
   batch_ids <- sheet_data %>%
     filter(batch == as.character(batch_number)) %>%
     pull(Device_ID)
   
   #Step 2: Filter Omni data
   batch_omni <- omni_data %>%
     filter(device_id %in% batch_ids)
   
   #Step 3: Get step time windows
   step_windows <- sheet_data %>%
     filter(batch == as.character(batch_number)) %>%
     group_by(Chamber_Step) %>%
     summarise(
       step_start = min(Start_DateTime),
       step_end = max(End_DateTime),
       .groups = "drop"
     )
   
   #Step 4: Split and trim by step
   step_data <- step_windows %>%
     group_split(Chamber_Step) %>%
     map(~ {
       step_info <- .
       step_val <- step_info$Chamber_Step[1]
       start <- step_info$step_start[1]
       end <- step_info$step_end[1]
       
       batch_omni %>%
         filter(time >= start, time <= end) %>%
         mutate(Chamber_Step = step_val)
     })
   
   #Step 5: Combine all steps back
   bind_rows(step_data)
 }

#Create each batch
batch_1 <- trim_omni(omni_master, sheets_data, batch_number = 1) 
batch_2 <- trim_omni(omni_master, sheets_data, batch_number = 2)
batch_3 <- trim_omni(omni_master, sheets_data, batch_number = 3)
batch_4 <- trim_omni(omni_master, sheets_data, batch_number = 4)
batch_5 <- trim_omni(omni_master, sheets_data, batch_number = 5)

#Combine all batches
all_batches <- unique(sheets_data$batch)

omni_all_trimmed <- map_dfr(all_batches, function(batch_num) {
  
  trim_omni(omni_master, sheets_data, batch_number = batch_num) %>%
    mutate(batch = batch_num)
})

#----
#Batch plots

#odering chamber steps for plotting
batch_1 <- batch_1 %>% 
  mutate(Chamber_Step = factor(Chamber_Step, levels = c("150.4", "55.4",
                                                        "35.4", "12", "0")))
#Create bands for plot area
step_bands <- batch_1 %>% 
  group_by(Chamber_Step) %>% 
  summarise(xmin = min(time),
            xmax = max(time),
            .groups = "drop")

#Define color for bands
step_colors <- c("150.4" = "#d8bce6", "55.4" = "#f4cccc", "35.4" = "#fce5cd",
                 "12" = "#fff2cc", "0" = "#d9ead3")

#Plot
p_batch_1 <- batch_1 %>% 
  ggplot(aes(x = time, y = pm25, color = device_id)) +
  geom_rect(data = step_bands, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
                fill = Chamber_Step), alpha = 0.5) +
  geom_line(linewidth = 0.8) +
  scale_fill_manual(values = step_colors, guide = "none") +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Chamber Test Pm 2.5 Concentration by Device ID",
        x = "Time", y = "Concentration (µg/m³)", color = "Device_ID") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "gray90"),
        plot.title = element_text(face = "bold"),
        legend.position = "right")

p_batch_1
