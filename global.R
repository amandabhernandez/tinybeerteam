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
    select(number:rh_percent) %>% 
    mutate(date_time = mdy_hms(date_time),
           date = ymd(str_remove_all(as.character(date_time), "\\d*:\\d*:\\d*")), 
           time = hms(str_extract(as.character(date_time), "\\d*:\\d*:\\d*")),
           month = month(date_time),
           day = day(date_time),
           hour = hour(date_time),
           minute = minute(date_time)) %>% 
    pivot_longer(names_to = "metric", values_to = "Result", temp_f:rh_percent) %>% 
    mutate(metric = factor(metric, levels = c("temp_f", "rh_percent"),
                           labels = c("Temperature (F)", "Relative Humidity (%)")))
  
}

clean_sp <- function(dat){
  dat %>% 
    mutate(#Time = hms(str_extract(as.character(Time), "\\d*:\\d*:\\d*")), 
           Date = mdy(Date),
           Time = hms(Time),
           #Date = ymd(str_remove_all(as.character(Date), "\\d*:\\d*:\\d*")), 
           date_time = ymd_hms(paste0(Date, " ", Time)),
           month = month(date_time),
           day = day(date_time),
           hour = hour(date_time),
           minute = minute(date_time),
           Aerosol = as.numeric(Aerosol))%>% 
    rename(Result = Aerosol,
           date = Date, 
           time = Time)
}


##### PULL IN HOBO DATA #########

# hobo_test_files <- drive_find(pattern = "EH263-H")
# 
# hobo_test_list <- list()
# 
# for (i in seq(1:length(hobo_test_files$id))){
#   
#   file_link <- hobo_test_files$drive_resource[[i]]$webViewLink
#   file_name <- hobo_test_files$name[i]
#   
#   sdb_dat <- read_sheet(file_link, skip = 1) %>% 
#     clean_names() %>% 
#     rename_all(~str_remove_all(., "_lgr.*")) %>% 
#     rename_with(~"date_time", contains("date_time")) %>% 
#     select(date_time:rh_percent) 
#   
#   hobo_test_list[[file_name]] <- sdb_dat
#   
# }


### LOCAL TEST DATA #### 

keep_cols <- c("date_time", "date", "time", "month", "day", 
               "hour", "minute", "metric", "Result", "location")

pm10_tap <- read_csv("data/sp_tap_pm10_2022-11-03.csv", skip = 28)[-1,] %>% 
  clean_sp() %>% 
  mutate(metric = "PM10",
         location = "Tap Room") 



pm2.5_tap <- read_csv("data/sp_tap_pm2.5_2022-11-03.csv", skip = 28)[-1,] %>% 
  mutate(metric = "PM2.5",
         location = "Tap Room") %>% 
  clean_sp()

pm10_bp <- read_csv("data/sp_bp_pm10_2022-11-03.csv", skip = 29)[-1,] %>%
  mutate(#Time = hms(str_extract(as.character(Time), "\\d*:\\d*:\\d*")), 
    Date = mdy(Date),
    Time = hms(Time),
    #Date = ymd(str_remove_all(as.character(Date), "\\d*:\\d*:\\d*")), 
    date_time = ymd_hms(paste0(Date, " ", Time)),
    month = month(date_time),
    day = day(date_time),
    hour = hour(date_time),
    minute = minute(date_time),
    MC = as.numeric(MC))%>% 
  rename(Result = MC,
         date = Date, 
         time = Time) %>% 
  mutate(metric = "PM10",
         location = "Brew Pit")

pm2.5_bp <- read_csv("data/sp_bp_pm25_2022-11-03.csv", skip = 29)[-1,] %>%
  mutate(#Time = hms(str_extract(as.character(Time), "\\d*:\\d*:\\d*")), 
    Date = mdy(Date),
    Time = hms(Time),
    #Date = ymd(str_remove_all(as.character(Date), "\\d*:\\d*:\\d*")), 
    date_time = ymd_hms(paste0(Date, " ", Time)),
    month = month(date_time),
    day = day(date_time),
    hour = hour(date_time),
    minute = minute(date_time),
    MC = as.numeric(MC))%>% 
  rename(Result = MC,
         date = Date, 
         time = Time) %>% 
  mutate(metric = "PM2.5",
         location = "Brew Pit")


day1_pm <- bind_rows(pm10_tap, pm2.5_tap, pm10_bp, pm2.5_bp)


co2 <- read_csv("data/hobo_co2_bp_2022-11-03.csv", skip = 1) %>%
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
                         labels = c("CO2 (ppm)")),
         location = "Brew Pit")


hobo04 <- read_csv("data/hobo_tap_2022-11-03.csv", skip = 1) %>% 
  clean_hobo_dat()%>% 
  filter(date_time > mdy_hms("11-03-22 09:00:00"))  %>%  
  mutate(location = "Tap Room")
  
hobo15 <- read_csv("data/hobo_bp_2022-11-03.csv", skip = 1) %>%
  clean_hobo_dat() %>%
  mutate(location = "Brew Pit")


day1_hobo_dat <- bind_rows(hobo04,hobo15)


all_dat <- bind_rows(day1_pm, day1_hobo_dat[, keep_cols], co2[, keep_cols])



###############################################################################
# 2. APP/PLOT FUNCTIONS  #########################################
###############################################################################



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
    xlab("Date")+
    ylab("Concentration") +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "snow2"),
          strip.text.x = element_text(color = "black", face = "bold"),
          text = element_text(family = "Arial"))
  
  sub_plotly <- ggplotly(sub_plot,
                         toolftip = c("y", "label", "label2"))
  
  # sub_violin_ly <- chronicle::make_violin(sub_dat, value = 'Result', 
  #                                         plot_palette = "#ec7f78")
  # 
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
    facet_wrap(~metric) + 
    ggthemes::theme_pander() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "snow2"),
          strip.text.x = element_text(color = "black", face = "bold"),
          text = element_text(family = "Arial"))
  
  #print figure
  ggplotly(dens_plot)
}

