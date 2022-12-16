### Author: AHz
### Date: 10/11/2022
### Written in: R version 4.2.1
### Purpose: Load brewery data


# set working directory to file location
source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)

library(pacman)
p_load(tidyverse)
p_load(googlesheets4)
p_load(googledrive)
p_load(janitor)
p_load(lubridate)
p_load(viridis)


###############################################################################
# 1. FUNCTIONS TO CLEAN RAW DATA #########################################
###############################################################################

separate_time <- function(dat){
  dat %>% 
    mutate(month = month(date_time),
           day = day(date_time),
           hour = hour(date_time),
           minute = minute(date_time))
}

#clean HOBO data (without CO2 logger)
clean_hobo_dat <- function(dat){
  dat %>% 
    clean_names() %>% 
    rename_all(~str_remove_all(., "_lgr.*")) %>% 
    rename_with(~"date_time", contains("date_time")) %>% 
    select(x:rh) %>%
    mutate(date_time = mdy_hms(date_time),
           date = ymd(str_remove_all(as.character(date_time), "\\d*:\\d*:\\d*")),
           time = hms(str_extract(as.character(date_time), "\\d*:\\d*:\\d*"))) %>%
    separate_time() %>% 
    pivot_longer(names_to = "metric", values_to = "Result", temp_f:rh) %>%
    mutate(metric = factor(metric, levels = c("temp_f", "rh"),
                           labels = c("Temperature (F)", "Relative Humidity (%)")))
  
}

#clean HOBO CO2 logger data
clean_hobo_co2 <- function(dat){
  dat %>% 
    clean_names() %>% 
    rename_all(~str_remove_all(., "_lgr.*")) %>% 
    rename_with(~"date_time", contains("date_time")) %>% 
    select(date_time:co2_ppm) %>% 
    mutate(date_time = mdy_hms(date_time),
           date = ymd(str_remove_all(as.character(date_time), "\\d*:\\d*:\\d*")),
           time = hms(str_extract(as.character(date_time), "\\d*:\\d*:\\d*"))) %>%
    separate_time() %>% 
    pivot_longer(names_to = "metric", values_to = "Result", temp_f:co2_ppm) %>%
    filter(metric == "co2_ppm") %>%
    mutate(metric = factor(metric, levels = c("co2_ppm"),
                           labels = c("CO2 (ppm)")))
}

#clean sidepak data
clean_sp <- function(dat){
  dat %>% 
    rename(Result = 3) %>% 
    mutate(
      Date = mdy(Date),
      Time = hms(Time),
      date_time = ymd_hms(paste0(Date, " ", Time)),
      Result = as.numeric(Result),
      metric = "") %>% 
    separate_time() %>% 
    rename(date = Date, 
           time = Time)
}

#average 1-sec interval measurements to 1 min 
avg_to_1min <- function(dat){
  dat %>% 
    mutate(date_time = format(date_time,format='%Y-%m-%d %H:%M')) %>% 
    group_by(location, metric, date, date_time) %>% 
    summarize(Result = mean(Result)) %>% 
    mutate(date_time = ymd_hm(date_time),
           time = hms(str_extract(as.character(date_time), "\\d*:\\d*:\\d*"))) %>% 
    separate_time() 
}


###############################################################################
# 2. LOAD DATA FROM GOOGLE DRIVE  #########################################
###############################################################################


##### PULL IN MEASUREMENT DATA FROM GOOGLE DRIVE #########

keep_cols <- c("date_time", "date", "time", "month", "day",
               "hour", "minute", "metric", "Result")

#get df with all files in the google drive "Data" folder
tbt_data_files <- drive_ls(path = "https://drive.google.com/drive/folders/1c61caoNIFOnZfeCkFZ479jEYATyj66C5") %>%
  filter(str_detect(name, "\\d{4}-\\d{2}-\\d{2}"))

data_files_list <- list()

for (i in seq(1:length(tbt_data_files$id))){

  file_link <- tbt_data_files$drive_resource[[i]]$webViewLink
  file_name <- tbt_data_files$name[i]

  if(str_detect(file_name, "hobo_co2")){
    tbt_dat <- drive_read_string(file_link) %>%
      read.csv(text = ., skip = 1) %>%
      clean_hobo_co2()
  }
  if(str_detect(file_name, "hobo_(?!co2)")){
    tbt_dat <- drive_read_string(file_link) %>%
      read.csv(text = ., skip = 1) %>%
      clean_hobo_dat()
  }
  if(str_detect(file_name, "sp_tap")){
    tbt_dat <- drive_read_string(file_link) %>%
      read_csv(.,skip = 28)

    tbt_dat <- tbt_dat[-1,] %>%
      clean_sp()
  }
  if(str_detect(file_name, "sp_bp")){
    tbt_dat <- drive_read_string(file_link) %>%
      read.csv(text = ., skip = 29)

    tbt_dat <- tbt_dat[-1,] %>%
      clean_sp()
  }

  data_files_list[[file_name]] <- tbt_dat %>%
    select(keep_cols)
}

all_dat <- bind_rows(data_files_list, .id = "file_name") %>%
  mutate(location = case_when(str_detect(file_name, "bp") ~ "Brew Pit",
                              str_detect(file_name, "tap") ~ "Taproom"),
         metric = case_when(str_detect(file_name, "pm2") ~ "PM2.5 (mg/m3)",
                            str_detect(file_name, "pm10") ~ "PM10 (mg/m3)",
                            TRUE ~ metric))



##### PULL IN FIELD LOG INFO FROM GOOGLE DRIVE #########

sample_start_stop <- read_sheet("https://docs.google.com/spreadsheets/d/1ZwZ9FuUgRcFDR1VFsHrO3JPZXF2ScWM0gCfwbjwI4fk/edit#gid=1915312888",
                                sheet = "sample_start_stop", col_types = "c") %>% 
  mutate(date = ymd(`Date (YYYY/MM/DD)`), 
         start_time = ymd_hm(paste(`Date (YYYY/MM/DD)`, " " ,`Start Time (0:00 - 23:5)`)),
         end_time =  ymd_hm(paste(`Date (YYYY/MM/DD)`, " " ,`End Time (0:00 - 23:5)`)),
         samp_interval = interval(start = start_time, end = end_time),
         location = Instrument) %>% 
  select(date, location, `Sampling Status`, samp_interval)


field_log <- read_sheet("https://docs.google.com/spreadsheets/d/1ZwZ9FuUgRcFDR1VFsHrO3JPZXF2ScWM0gCfwbjwI4fk/edit#gid=1915312888",
                        sheet = "Sampling Log.v3", col_types = "c") %>% 
  mutate(date = ymd(`Date (YYYY/MM/DD)`), 
         start_time = ymd_hms(paste0(`Date (YYYY/MM/DD)`, " " ,`Start Time (0:00 - 23:5)`, ":00")),
         end_time =  ymd_hms(paste0(`Date (YYYY/MM/DD)`, " " ,`End Time (0:00 - 23:5)`, ":59")),
         end_time = end_time-minutes(1),
         samp_interval = interval(start = start_time, end = end_time),
         `Sampling Stage` = factor(`Sampling Stage`, levels = ,c( "Precision Check","Background Run",
                                                                  "Grain Pouring", "Mashing", "Mash Rest","Solids Settling",
                                                                  "Recirculating of Solids", "Transfer", "Scooping Out Spent Grains",
                                                                  "Boiling", "Cooling", "Hops added", "Other"),
                                   labels = c( "1. Precision Check","2. Background Run",
                                               "3. Grain Pouring", "4. Mashing", "5. Mash Rest","6. Solids Settling",
                                               "7. Recirculating of Solids", "8. Transfer", "9. Scooping Out Spent Grains",
                                               "10. Boiling", "11. Cooling", "12. Hops added", "Other")),
         samp_stage_label = str_extract(`Sampling Stage`, "\\d*")
  ) 



###############################################################################
# 3. PROCESS DATA  #########################################
###############################################################################

##### PROCESS RAW DATA + FILTER TO SAMPLING EVENTS #########


all_dat_avg <- all_dat %>% 
  #drop all files where the instrument was accidentally measuring 1-sec intervals
  filter(!str_detect(file_name, "sp_bp_pm.*_2022-11-03")) %>% 
  filter(!str_detect(file_name, "hobo_tap_2022-11-08.csv")) %>% 
  filter(!str_detect(file_name, "hobo_tap_2022-11-09.csv")) %>% 
  #bind rows with 1 min averaged datasets
  bind_rows(all_dat %>% 
              filter(str_detect(file_name, "sp_bp_pm.*_2022-11-03")) %>% 
              avg_to_1min()) %>% 
  bind_rows(all_dat %>% 
              filter(str_detect(file_name, "hobo_tap_2022-11-08.csv") | str_detect(file_name, "hobo_tap_2022-11-09.csv")) %>% 
              avg_to_1min())  %>% 
  #drop seconds from the measurement time (makes matching activity much easier and clearer)
  mutate(date_time = format(date_time,format='%Y-%m-%d %H:%M'),
         date_time = ymd_hm(date_time),
         time = hms(str_extract(as.character(date_time), "\\d*:\\d*:\\d*"))) %>% 
  #join with the start stop log by date and location
  left_join(sample_start_stop) %>% 
  mutate(keep = case_when(date_time %within% samp_interval ~ "yes",
                          TRUE ~ "no")) %>% 
  filter(keep == "yes") %>% 
  select(-samp_interval, -keep) %>% 
  clean_names() %>% 
  mutate(intervention = case_when(as.character(date) %in% c("2022-11-03", "2022-11-08", 
                                                            "2022-11-09", "2022-11-17", "2022-11-21") ~ "Normal Brewing",
                                  as.character(date) %in% c("2022-11-18") ~ "Portable Air Cleaner", 
                                  TRUE ~ "Normal Brewing"),
         sampling_status = factor(sampling_status, levels = c("Precision Check", "Background Run", "Brewing")))




# write_csv(all_dat_avg, "all_measurement_data.csv")
write_rds(all_dat_avg, "all_measurement_data.rds")


drive_upload(media = "all_measurement_data.rds",
             name = "all_measurement_data.rds",
             path = as_id("1c61caoNIFOnZfeCkFZ479jEYATyj66C5"),
             overwrite = TRUE)


all_measurement_data <- all_dat_avg
save(all_dat_avg, field_log, sample_start_stop, 
     file = "all_tbt_data_0.RData")





#### GARBAGE <3 ##########



# all_dat_avg <- all_dat %>%
#   filter(!(date == "2022-11-03" & str_detect(metric, "PM"))) %>%
#   filter(!str_detect(file_name, "hobo_tap_2022-11-08.csv")) %>%
#   filter(!str_detect(file_name, "hobo_tap_2022-11-09.csv")) %>%
#   bind_rows(all_dat %>%
#               filter(date == "2022-11-03") %>%
#               filter(str_detect(metric, "PM")) %>%
#               avg_to_1min()) %>%
#   bind_rows(all_dat %>%
#               filter(str_detect(file_name, "hobo_tap_2022-11-08.csv") | str_detect(file_name, "hobo_tap_2022-11-09.csv")) %>%
#               avg_to_1min())

# all_dat_w_log <- all_dat_avg %>% 
#   left_join(sample_start_stop) %>% 
#   mutate(keep = case_when(date_time %within% samp_interval ~ "yes",
#                           TRUE ~ "no")) %>% 
#   filter(keep == "yes") %>% 
#   select(-samp_interval, -keep) %>% 
#   mutate(intervention = case_when(as.character(date) %in% c("2022-11-03", "2022-11-08", 
#                                                             "2022-11-09", "2022-11-17", "2022-11-21") ~ "Normal Brewing",
#                                   as.character(date) %in% c("2022-11-18") ~ "Portable Air Cleaner", 
#                                   TRUE ~ "Normal Brewing"))


# checking on merges etc 
# cr<-anti_join(all_measurement_data, all_dat_avg) %>% 
#   group_by(metric, location) %>% 
#   count()
# cr_rev <-anti_join( all_dat_avg, all_measurement_data)  %>% 
#   group_by(metric, location) %>% 
#   count()
# 
# cr_all_dat <- all_dat_avg %>% 
#   group_by(date, metric, location) %>% 
#   count() %>% 
#   mutate(df = "all_dat_avg") %>% 
#   bind_rows( all_measurement_data %>% 
#                group_by(date, metric, location) %>% 
#                count() %>% 
#                mutate(df = "all_measurement_data")) %>% 
#   pivot_wider(names_from = "df", values_from = "n") %>% 
#   mutate(check = ifelse(all_dat_avg != all_measurement_data, "!", ""))
# 
# 
# 
# 
# cr1 <- all_dat_avg %>% 
#   filter(str_detect(file_name, "sp_tap_pm.*_2022-11-03")) %>% 
#   mutate(date_time = format(date_time,format='%Y-%m-%d %H:%M'),
#          date_time = ymd_hm(date_time),
#          time = hms(str_extract(as.character(date_time), "\\d*:\\d*:\\d*"))) %>% 
#   select(-file_name)
# 
# cr2 <- all_measurement_data %>% 
#   # #filter(str_detect(file_name, "sp_bp_pm.*_2022-11-03")) %>% 
#   filter(date == "2022-11-03") %>%
#   filter(str_detect(metric, "PM")) %>%
#   filter(location == "Taproom") %>% 
#   select(-file_name)
# # avg_to_1min()
# 
# cr3 <- anti_join(cr2, cr1)

# cr2 <- all_dat %>% 
# filter(str_detect(file_name, "sp_tap_pm.*_2022-11-03")) %>%
# bind_rows(all_dat %>% 
#             filter(str_detect(file_name, "sp_bp_pm.*_2022-11-03")) %>% 
#             avg_to_1min()
# )
