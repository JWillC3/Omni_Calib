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
batch_6 <- trim_omni(omni_master, sheets_data, batch_number = 6)

#Combine all batches
all_batches <- unique(sheets_data$batch)

omni_all_trimmed <- map_dfr(all_batches, function(batch_num) {
  
  trim_omni(omni_master, sheets_data, batch_number = batch_num) %>%
    mutate(batch = batch_num)
})

#----
#Batch plots function name is: p_chamber_batch

#Define test end time
test_end <- as.POSIXct("2025-07-14 23:59", tz = "America/Denver")
#Call the function
p_batch_1 <- p_chamber_batch(batch_1, test_end, "July 14, 2025")
#Display plot
p_batch_1

#Define test end time
test_end <- as.POSIXct("2025-07-31 23:59", tz = "America/Denver")
#Call the function
p_batch_5 <- p_chamber_batch(batch_5, test_end, "July 31, 2025")
#Display plot
p_batch_5

#Define test end time
test_end <- as.POSIXct("2025-07-25 23:59", tz = "America/Denver")
#Call the function
p_batch_3 <- p_chamber_batch(batch_3, test_end, "July 25, 2025")
#Display plot
p_batch_3

test_end <- as.POSIXct("2025-07-18 23:59", tz = "America/Denver")
#Call the function
p_batch_2 <- p_chamber_batch(batch_2, test_end, "July 18, 2025")
#Display plot
p_batch_2

test_end <- as.POSIXct("2025-07-30 23:59", tz = "America/Denver")
#Call the function
p_batch_4 <- p_chamber_batch(batch_4, test_end, "July 30, 2025")
#Display plot
p_batch_4

test_end <- as.POSIXct("2025-08-06 23:59", tz = "America/Denver")
#Call the function
p_batch_6 <- p_chamber_batch(batch_6, test_end, "Aug 6, 2025")
#Display plot
p_batch_6

ggplotly(p_batch_6)
