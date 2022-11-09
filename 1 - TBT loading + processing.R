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
# 1. FUNCTIONS  #########################################
###############################################################################

clean_hobo_dat <- function(dat){
  dat %>% 
    clean_names() %>% 
    rename_all(~str_remove_all(., "_lgr.*")) %>% 
    rename_with(~"date_time", contains("date_time")) %>% 
    select(x:rh) %>%
    mutate(date_time = mdy_hms(date_time),
           date = ymd(str_remove_all(as.character(date_time), "\\d*:\\d*:\\d*")),
           time = hms(str_extract(as.character(date_time), "\\d*:\\d*:\\d*")),
           month = month(date_time),
           day = day(date_time),
           hour = hour(date_time),
           minute = minute(date_time)) %>%
    pivot_longer(names_to = "metric", values_to = "Result", temp_f:rh) %>%
    mutate(metric = factor(metric, levels = c("temp_f", "rh"),
                           labels = c("Temperature (F)", "Relative Humidity (%)")))
  
}

clean_hobo_co2 <- function(dat){
  dat %>% 
    clean_names() %>% 
    rename_all(~str_remove_all(., "_lgr.*")) %>% 
    rename_with(~"date_time", contains("date_time")) %>% 
    select(date_time:co2_ppm) %>% 
    mutate(date_time = mdy_hms(date_time),
           date = ymd(str_remove_all(as.character(date_time), "\\d*:\\d*:\\d*")),
           time = hms(str_extract(as.character(date_time), "\\d*:\\d*:\\d*")),
           month = month(date_time),
           day = day(date_time),
           hour = hour(date_time),
           minute = minute(date_time)) %>% 
    pivot_longer(names_to = "metric", values_to = "Result", temp_f:co2_ppm) %>%
    filter(metric == "co2_ppm") %>%
    mutate(metric = factor(metric, levels = c("co2_ppm"),
                           labels = c("CO2 (ppm)")))
}

clean_sp <- function(dat){
  dat %>% 
    rename(Result = 3) %>% 
    mutate(#Time = hms(str_extract(as.character(Time), "\\d*:\\d*:\\d*")), 
      Date = mdy(Date),
      Time = hms(Time),
      #Date = ymd(str_remove_all(as.character(Date), "\\d*:\\d*:\\d*")), 
      date_time = ymd_hms(paste0(Date, " ", Time)),
      month = month(date_time),
      day = day(date_time),
      hour = hour(date_time),
      minute = minute(date_time),
      Result = as.numeric(Result),
      metric = "") %>% 
    rename(date = Date, 
           time = Time)
}

avg_to_1min <- function(dat){
  dat %>% 
    mutate(date_time = format(date_time,format='%Y-%m-%d %H:%M')) %>% 
    group_by(location, metric, date, date_time) %>% 
    summarize(Result = mean(Result)) %>% 
    mutate(date_time = ymd_hm(date_time),
           time = hms(str_extract(as.character(date_time), "\\d*:\\d*:\\d*")), 
           month = month(date_time),
           day = day(date_time),
           hour = hour(date_time),
           minute = minute(date_time))
}


###############################################################################
# 2. LOAD DATA FROM GOOGLE DRIVE  #########################################
###############################################################################


##### PULL IN DATA FROM GOOGLE DRIVE #########

keep_cols <- c("date_time", "date", "time", "month", "day", 
               "hour", "minute", "metric", "Result")

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
                              str_detect(file_name, "tap") ~ "Tap Room"),
         metric = case_when(str_detect(file_name, "pm2") ~ "PM2.5 (ppm)",
                            str_detect(file_name, "pm10") ~ "PM10 (ppm)",
                            TRUE ~ metric)) 



all_dat_day1avg <- all_dat %>% 
  filter(!(date == "2022-11-03" & str_detect(metric, "PM"))) %>% 
  filter(!str_detect(file_name, "hobo_tap_2022-11-08.csv")) %>% 
  filter(!str_detect(file_name, "hobo_tap_2022-11-09.csv")) %>% 
  bind_rows(all_dat %>% 
              filter(date == "2022-11-03") %>% 
              filter(str_detect(metric, "PM")) %>% 
              avg_to_1min()) %>% 
  bind_rows(all_dat %>% 
              filter(str_detect(file_name, "hobo_tap_2022-11-08.csv") | str_detect(file_name, "hobo_tap_2022-11-09.csv")) %>% 
              avg_to_1min())  %>% 
  filter(date_time %within% interval(ymd_hms("2022-11-03 09:15:00"), ymd_hms("2022-11-03 15:15:00")) |
           date_time  %within% interval(ymd_hms("2022-11-08 06:40:00"), ymd_hms("2022-11-08 12:15:00")) |
           date ==  as_date(now()))


write_csv(all_dat_day1avg, "all_measurement_data.csv")
write_rds(all_dat_day1avg, "all_measurement_data.rds")


drive_upload(media = "all_measurement_data.rds",
             name = "all_measurement_data.rds", 
             path = as_id("1c61caoNIFOnZfeCkFZ479jEYATyj66C5"),
             overwrite = TRUE)


#if you don't want to run all the code above to get the most recent data, just ask AHz to 
#push the latest version and then download and run the all_measurement_data.rds
all_measurement_data <- readRDS("all_measurement_data.rds")

###############################################################################
# 3. QAQC CHECKS  #########################################
###############################################################################


check_gaps <- all_dat_day1avg %>% 
  group_by(metric, location) %>% 
  arrange(date_time) %>% 
  mutate(rec_after = lead(date_time, order_by = date_time, 
                          default = max(date_time)),
         check_lag = minute - lag(minute(rec_after)))

table(check_gaps$check_lag, useNA = "ifany")
#all lag = 0, 9 NA for first measurements for ((4 metrics *2 locations) + (1metric *1 location)) -- all good!




######### COMPARE ACROSS DIFFERENT SAMPLING DATES #############


compare_dates <- all_dat_day1avg %>% 
  group_by(date, location, metric) %>% 
  arrange(date_time) %>% 
  mutate(rec_num = seq_along(1:n()))

ggplot(compare_dates, aes(x = rec_num/60, y = Result, color = as.character(date))) + 
  geom_line() + 
  viridis::scale_color_viridis(discrete = TRUE, end = 0.75, name = "Date") +
  facet_grid(metric~location, scales = "free") +
  ggthemes::theme_pander() +
  xlab("# of hours passed")+
  ylab("Concentration") + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "snow2"),
        strip.text.x = element_text(color = "black", face = "bold"),
        text = element_text(family = "Arial"))


######### REDO POWER CALC #############

#redo power calc
all_dat_day1avg %>% 
  filter(str_detect(metric, "PM")) %>% 
  group_by(location, date, metric) %>% 
  summarize(mean_PM = mean(Result),
            sd_PM = sd(Result)) %>% 
  bind_rows(all_dat_day1avg %>% 
              filter(str_detect(metric, "PM")) %>% 
              group_by(location, metric) %>% 
              summarize(mean_PM = mean(Result),
                        sd_PM = sd(Result))) %>% 
  mutate(date = case_when(is.na(date) ~ "All Samples",
                          TRUE ~ as.character(date))) %>% 
  mutate(sample_size = ((1.96+.842)/((mean_PM-(mean_PM*.7))/(sd_PM)))^2,
         hours_needed = (sample_size*30)/60) %>% 
  filter(date == "All Samples")
