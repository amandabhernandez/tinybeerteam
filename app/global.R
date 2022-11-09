### AUTHOR: AHz
### LAST EDIT: 2022-11-03
### WRITTEN IN: R version 4.2.1
### Purpose: Stardrop Brewing sampling day 1


###############################################################################
# 0. Load packages   #########################################
###############################################################################
library(tidyverse)
library(lubridate)
library(janitor)

library(googlesheets4)
library(googledrive)

options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets"
)
# googledrive::drive_auth()
# googlesheets4::gs4_auth()


# # Check that the non-interactive authentication works by first deauthorizing:
# gs4_deauth()
# 
# # Authenticate using token. If no browser opens, the authentication works.
# gs4_auth(cache = ".secrets", email = "amandabhernandez@gmail.com")

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)

library(plotly)
library(fontawesome)
library(viridis)

###############################################################################
# 1. DATA CLEANING   #########################################
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
           date ==  as_date(now())) #%>% 
  #mutate(Result = format(Result, scientific = FALSE, big.mark = ","))



###############################################################################
# 2. APP/PLOT FUNCTIONS  #########################################
###############################################################################

sample_dates <- ""
#list of dates
for(i in seq_along(1:length(unique(all_dat_day1avg$date)))){
  sample_dates[i] <- paste0("Day ",i, ": ",rev(unique(all_dat_day1avg$date))[i])
}
  
  
  

### FOR APP
time_subplot <- function(ggdat, facet){
  sub_dat <- ggdat %>% 
    filter(metric == facet)
  
  sub_plot <- ggplot(sub_dat) +
    geom_line(aes(x = date_time, y = Result ,
                  color = location,
                  label = time,
                  label2 = date)) +
    scale_x_time(labels = scales::label_time(format = '%H:%M')) + 
    viridis::scale_color_viridis(discrete = TRUE, end = 0.75) +
    facet_wrap(~metric, ncol = 1, scales = "free_y") +
    ggthemes::theme_pander() +
    xlab("Time")+
    ylab("Concentration") +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "snow2"),
          strip.text.x = element_text(color = "black", face = "bold"),
          text = element_text(family = "Arial"))
  
  sub_plotly <- ggplotly(sub_plot,
                         toolftip = c("y", "label", "label2"))
  
  sub_box <- ggplot(sub_dat) + 
    geom_boxplot(aes(x = location, y = Result, fill = location)) + 
    ggthemes::theme_pander() +
    viridis::scale_fill_viridis(discrete = TRUE, end = 0.75) +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "snow2"), 
          text = element_text(family = "Arial"))
  
  sub_box_ly <- ggplotly(sub_box)
  
  subplot(list(sub_plotly, sub_box_ly), widths = c(.9, .1), 
          nrows = 1, shareY = TRUE, margin = 0)
}


dens_plotly <- function(ggdat, facet){
  dens_dat <- ggdat %>% 
    filter(metric == facet)
  
  dens_plot <- ggplot(dens_dat, aes(x = Result)) +
    geom_histogram(aes(y = ..density..),
                   color = "black",
                   fill = "white",
                   bins = 30) +
    geom_density(fill = "red", alpha = 0.25) + 
    facet_grid(location~metric) + 
    ggthemes::theme_pander() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "snow2"),
          strip.text.x = element_text(color = "black", face = "bold"),
          text = element_text(family = "Arial"))
  
  #print figure
  ggplotly(dens_plot)
}

