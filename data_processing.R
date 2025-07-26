#load data
source("source_data.R")

#----
#Phase 1, match Omni date to batch number from Google Sheets
# Step 1: Get device IDs from batch 1
batch_1_ids <- sheets_data %>%
  filter(batch == "1") %>%
  pull(Device_ID)

# Step 2: Filter omni_master to just batch 1 devices
omni_batch_1 <- omni_master %>%
  filter(device_id %in% batch_1_ids)
 unique(omni_batch_1$device_id)
 


 #----
 #Phase 2, trim the omni data to match time windows for chamber steps
 # Step 1: Get time windows for each chamber step
 step_windows <- sheets_data %>%
   filter(batch == "1") %>%
   group_by(Chamber_Step) %>%
   summarise(
     step_start = min(Start_DateTime),
     step_end = max(End_DateTime),
     .groups = "drop"
   )
 
 # Step 2: For each step, filter omni_batch_1 using the time window
 omni_steps_list <- step_windows %>%
   group_split(Chamber_Step) %>%
   map(~ {
     step_info <- .
     step_val <- step_info$Chamber_Step[1]
     start <- step_info$step_start[1]
     end <- step_info$step_end[1]
     
     # Filter omni data to only this time window
     omni_batch_1 %>%
       filter(time >= start, time <= end) %>%
       mutate(Chamber_Step = step_val)
   })
 
 # Step 3: Combine all steps back into one data frame
 omni_trimmed <- bind_rows(omni_steps_list) 
 