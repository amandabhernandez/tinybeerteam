### Author: AHz
### Date: 10/11/2022
### Written in: R version 4.2.1
### Purpose: Load brewery data


# set working directory to file location
source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)

library(pacman)
p_load(tidyverse)

p_load(janitor)
p_load(lubridate)
p_load(viridis)
p_load(gtsummary)


#you can run all the code in "0 - TBT data fetcher.R" if you want to get the most up to date data, but you can 
#also just ask amanda to run and push a new RDS file to the google drive if it's not already up to date. then just 
#download that file and put it where ever you want and load it here

all_measurement_data <- readRDS("all_measurement_data.rds")

brewing_measurement_data <- all_measurement_data %>% 
  filter(sampling_status == "Brewing") 

background_precision <- all_measurement_data %>% 
  filter(sampling_status %in% c("Precision Check", "Background Run")) 


###############################################################################
# 3. QAQC CHECKS  #########################################
###############################################################################

######### CHECK DATA FOR ISSUES AND COMPLETENESS #############

samples_collected_summary <- all_measurement_data %>% 
  group_by(date, metric, location) %>% 
  summarize(min_timestamp = min(date_time), 
            max_timestamp = max(date_time), 
            n = n())
#check in with Ruby/study team -- do we only want to keep "complete sets" ? 
#having a different "min_timestamp" is fine, but maybe change cut off to be the
#same end time? 

######### LOOK AT GAPS #############
check_gaps <- all_measurement_data %>% 
  group_by(date, metric, location) %>% 
  arrange(date_time) %>% 
  mutate(rec_after = lead(date_time, order_by = date_time, 
                          default = max(date_time)),
         check_lag = minute - lag(minute(rec_after))) 


stopifnot(sum(check_gaps$check_lag, na.rm = TRUE) == 0 & 
            sum(is.na(check_gaps$check_lag)) == 9*length(unique(check_gaps$date))-1)
#table(check_gaps$check_lag, useNA = "ifany")
#all lag = 0, 9 NA for first measurements for 
#((4 metrics *2 locations) + (1metric *1 location)*number of sampling dates) - 
#(1 missing data from taproom PM10)-- all good!



#are there any samples that didn't collect?
table(is.na(all_measurement_data$result), all_measurement_data$metric)
#some HOBO measurements
#AHz: went back and spot checked these -- no good explanation for it, but another measurement
#was made during the same minute, it just recorded at two different second intervals but one
#was empty... weird but fine. they all get dropped because they're outside the start-stop 
#time... basically it looks like it happens when we plug it in

######### LOOK AT PRECISION CHECKS #############

precision_check <- background_precision %>% 
  filter(str_detect(metric, "PM")) %>% 
  select(-file_name, -date_time, -time) %>% 
  pivot_wider(names_from = location, values_from = result) %>% 
  mutate(RPD = abs(`Taproom`-`Brew Pit`)/((`Taproom`+`Brew Pit`)/2),
         date_time = ymd_hm(paste(date, (paste0(hour, ":", minute))))) %>%
  filter(!is.na(`Brew Pit`) & !is.na(`Taproom`)) %>% 
  group_by(date, metric) %>% 
  arrange(date_time) %>% 
  mutate(rec_num = seq_along(1:n())-1) 


precision_check_sum <- precision_check %>% 
  filter(rec_num <= 5) %>% 
  group_by(date, metric) %>% 
  summarize(min_tap = min(`Taproom`),
            max_tap = max(`Taproom`), 
            min_bp = min(`Brew Pit`), 
            max_bp = max(`Brew Pit`),
            mean_RPD = mean(RPD)) %>% 
  pivot_longer(names_to = "name", values_to = "value", min_tap:max_bp) %>% 
  separate(name, into = c("min_max", "location"), sep = "_") %>% 
  pivot_wider(names_from = min_max, values_from = value)  %>% 
  group_by(date, metric, mean_RPD) %>% 
  summarize(min = min(min), 
            max = max(max))


precision_check %>% 
  filter(rec_num <= 5) %>% 
  ggplot() + 
  geom_point(aes(x = rec_num, y = `Brew Pit`, 
                 color = as.character(date))) + 
  geom_point(aes(x = rec_num, y = `Taproom`, 
                 color = as.character(date))) + 
  geom_linerange(aes(x = rec_num, ymin = `Taproom`, ymax = `Brew Pit`, 
                     color = as.character(date))) + 
  geom_text(precision_check_sum, mapping = aes(x = 2, y = 0.04, 
                                               label = paste0("Mean RPD: ", paste0(round(mean_RPD*100, 0), "%")),
                                               color = as.character(date))) + 
  viridis::scale_color_viridis(discrete = TRUE, end = 0.75, name = "Date") +
  theme_bw() +
  xlim(0, 5) + 
  xlab("# of minutes") +
  ylab("Measurement (ppm)") +
  theme(legend.position = "none", 
        axis.text = element_text(size=14),
        strip.text = element_text(size = 16, face = "bold")) +
  facet_grid(metric~date) + 
  ggtitle("Measurement and mean RPD for sidepak measurements during sampling precision check")








###############################################################################
# 4. LOOK AT DATA  #########################################
###############################################################################


######### COMPARE ACROSS DIFFERENT SAMPLING DATES #############


compare_dates <- brewing_measurement_data %>% 
  filter(str_detect(metric, "PM") | str_detect(metric, "CO2")) %>% 
  group_by(date, location, metric) %>% 
  arrange(date_time) %>% 
  mutate(rec_num = seq_along(1:n()))

ggplot(compare_dates, aes(x = rec_num/60, y = result, color = as.character(date))) + 
  geom_line(size = 1) + 
  viridis::scale_color_viridis(discrete = TRUE, end = 0.75, name = "Date") +
  #scale_alpha_manual(values = c(0.25, 1), name = "Intervention") + 
  #scale_color_manual(values = c("#ff5722ff", "#4285f4ff", "#2d3b45ff"), name = "Date")+
  facet_grid(metric~location, scales = "free") +
  theme_bw(base_size = 22) + 
  #ggthemes::theme_pander(base_size = 22) +
  xlab("\n# of hours passed")+
  ylab("Concentration\n") + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "snow2"),
        #strip.background = element_blank(),
        strip.text = element_text(color = "black", face = "bold", size = 18),
        #legend.background = element_rect(fill = "#ffc263"),
        #legend.key = element_rect(fill = "#ffc263"),
        panel.spacing = unit(1, "lines"),
        #plot.background = element_rect(fill = "#ffc263"),
        text = element_text(family = "Arial"),
        plot.margin = margin(1,1,1.5,1.2, "cm")
  ) + 
  guides(colour = guide_legend(override.aes = list(size=3)))



ggsave("PM and CO2 measurements normalized to hours of sampling_asof_2022-11-25.png", 
       width = 18, height = 10, units = "in")




######### GET SUMMARY STATS #############


gtsummary::tbl_strata(data = brewing_measurement_data %>% filter(str_detect(metric, "PM")), 
                      strata = metric,
                      .tbl_fun = ~.x %>%
                        tbl_summary(
                          by = location, 
                          digits = everything() ~ 2,
                          include = c("result"),
                          type = list("result" ~ 'continuous2'),
                          statistic = all_continuous() ~ c("{median} ({min} - {max})", 
                                                           "{mean} ({sd})",
                                                           "{p90}")), 
                      .combine_with = "tbl_stack"
)



######### REDO POWER CALC #############

hours_collected <- brewing_measurement_data %>% 
  group_by(date, metric, location) %>% 
  summarize(sampling_time = as.duration(interval(start = min(date_time), 
                                                 end = max(date_time)))) %>% 
  group_by(metric, location) %>% 
  summarize(hours_collected = (sum(sampling_time)/60)/60)




#redo power calc
power_calc_update <- brewing_measurement_data %>% 
  filter(str_detect(metric, "PM")) %>% 
  group_by(location, date, metric) %>% 
  summarize(mean_PM = mean(result),
            PM_90 = quantile(result, .9), 
            sd_PM = sd(result)) %>% 
  bind_rows(brewing_measurement_data %>% 
              filter(str_detect(metric, "PM")) %>% 
              group_by(location, metric) %>% 
              summarize(mean_PM = mean(result),
                        PM_90 = quantile(result, .9), 
                        sd_PM = sd(result))) %>% 
  bind_rows(brewing_measurement_data %>% 
              filter(str_detect(metric, "PM")) %>% 
              group_by(metric) %>% 
              summarize(mean_PM = mean(result),
                        PM_90 = quantile(result, .9), 
                        sd_PM = sd(result))) %>% 
  mutate(date = case_when(is.na(date) ~ "All Samples",
                          TRUE ~ as.character(date))) %>% 
  mutate(#sample_size_needed = ((1.96+.842)/((mean_PM-(mean_PM*0.7))/(sd_PM)))^2,
         samp_size_needed = (((1.96+0.842)^2)*2*sd_PM^2)/((mean_PM-mean_PM*0.6)^2),
         hours_needed = (samp_size_needed*15)/60) %>% 
  filter(date == "All Samples") %>% 
  left_join(brewing_measurement_data %>% 
              group_by(date, metric, location) %>% 
              summarize(sampling_time = as.duration(interval(start = min(date_time), end = max(date_time)))) %>% 
              group_by(metric, location) %>% 
              summarize(min_collected = (sum(sampling_time)/60),
                        samples_collected = min_collected/15, 
                        hours_collected = min_collected/60)
  ) 


#roughly how many hours of sampling do we have? or how many 15 min samples? 
brewing_measurement_data %>% 
  group_by(date, metric, location) %>% 
  summarize(sampling_time = as.duration(interval(start = min(date_time), end = max(date_time)))) %>% 
  group_by(metric, location) %>% 
  summarize(sum_hrs = (sum(sampling_time)/60)/60,
            n_samples_collected = (sum(sampling_time)/60)/15)


power_calc_update %>% 
  ungroup() %>% 
  mutate(`Mean (SD)` = paste0(round(mean_PM, 4), " (", round(sd_PM, 4), ")")) %>% 
  select(location, metric, `Mean (SD)`, hours_needed, hours_collected) %>% 
  flextable::flextable()


###############################################################################
# 5. AVERAGE TO 15 MIN SAMPLES  #########################################
###############################################################################


brewing_15min_sample <- brewing_measurement_data %>%
  group_by(location, date, metric, date_time = cut(date_time, breaks = "15 mins")) %>% 
  summarize(avg_15_min = mean(result),
            n = n()) %>% 
  #do we keep only "full sets" 
  filter(n == 15)


compare15min <-  brewing_15min_sample %>% 
  group_by(date, location, metric) %>% 
  arrange(date_time) %>% 
  mutate(rec_num = seq_along(1:n()))
# 
# brewing_15min_sample <- brewing_measurement_data %>% 
#   group_by(location, date, metric) %>% 
#   arrange(date_time) %>% 
#   mutate(rec_num = seq_along(1:n()),
#          rec_group = as.numeric(str_extract(as.character(rec_num/15-0.0001), "\\d*"))) %>% 
#   group_by(location, date, metric, rec_group) %>% 
#   summarize(avg_15_min = mean(result),
#             n = n()) %>% 
#   filter(n == 15)

ggplot(compare15min %>% 
         filter(str_detect(metric, "PM")), aes(x = rec_num, y = avg_15_min, color = as.character(date))) + 
  geom_point() + 
  geom_line() + 
  viridis::scale_color_viridis(discrete = TRUE, end = 0.75, name = "Date") +
  theme_bw() + 
  facet_grid(metric~location, scales = "free")



###############################################################################
# 6. MERGE WITH FIELD LOG  #########################################
###############################################################################

measurement_w_log_nomatch <- all_measurement_data %>% 
  left_join(field_log) %>% 
  mutate(keep_fl = case_when(date_time %within% samp_interval ~ "yes",
                             TRUE ~ "no")) %>% 
  group_by(metric, date_time, location,  keep_fl) %>% 
  count() %>% 
  pivot_wider(names_from = keep_fl, values_from = n)  %>% 
  filter(is.na(yes)) %>%
  mutate(samp_key = paste0(date_time, metric, location))


measurement_w_log <- all_measurement_data %>% 
  mutate(samp_key = paste0(date_time, metric, location)) %>% 
  left_join(field_log) %>% 
  filter(date_time %within% samp_interval) %>% 
  bind_rows(all_measurement_data %>% 
              mutate(samp_key = paste0(date_time, metric, location)) %>% 
              filter(samp_key %in% measurement_w_log_nomatch$samp_key)) %>% 
  select(-samp_interval)


check_time_merges <- measurement_w_log %>% 
  group_by(date, metric, location) %>% 
  summarize(min = min(date_time),
            max = max(date_time)) %>% 
  mutate(dat = "log") %>% 
  bind_rows(all_measurement_data %>% 
              group_by(date, metric, location) %>% 
              summarize(min = min(date_time),
                        max = max(date_time)) %>% 
              mutate(dat = "all")) %>% 
  pivot_wider(names_from = dat, values_from = c(min, max)) %>% 
  mutate(min_diff = as.duration(interval(min_all, min_log)),
         max_diff = as.duration(interval(max_all, max_log))) 

stopifnot(sum(check_time_merges$min_diff) == 0 & sum(check_time_merges$max_diff) == 0)



write_rds(measurement_w_log, "measurement_data_w_log.rds")

# drive_upload(media = "measurement_data_w_log.rds",
#              name = "measurement_data_w_log.rds",
#              path = as_id("1c61caoNIFOnZfeCkFZ479jEYATyj66C5"),
#              overwrite = TRUE)



measurement_w_log_wide <- measurement_w_log %>% 
  group_by(date_time, location, metric, `Sampling Stage`) %>% 
  count() %>% 
  pivot_wider(names_from = `Sampling Stage`, values_from = n) %>% 
  mutate(sum = rowSums(across(where(is.numeric)), na.rm = TRUE)) 

write_rds(measurement_w_log_wide, "measurement_data_w_log_wide.rds")


plot_dates <- c("2022-11-03", "2022-11-08","2022-11-09","2022-11-17","2022-11-18",
                "2022-11-21","2022-11-23", "2022-11-25")


field_log_plot <- list()
for(i in plot_dates){
  
  i <- "2022-11-08"
  
  field_log_plot[[i]] <- all_measurement_data %>% 
    filter(date == i)  %>% 
    filter(str_detect(metric, "PM")) %>% 
    ggplot() + 
    geom_rect(data = field_log %>%
                filter(as.character(date) == i) %>% 
                filter(start_time < ymd_hms("2022-11-08 11:30:00")) %>% 
                filter(`Sampling Stage` != "Other"), 
              mapping = aes(xmin = start_time,
                            xmax = end_time,
                            ymin = 0, ymax = Inf, 
                            fill = `Sampling Stage`),
              alpha = 0.3) +
    geom_text(data = field_log %>%
                filter(as.character(date) == i) %>% 
                filter(start_time < ymd_hms("2022-11-08 11:30:00")) %>% 
                filter(`Sampling Stage` != "Other"), mapping = aes(x = start_time,
                                                                   y = 0, 
                                                                   label = samp_stage_label)) + 
    geom_line(aes(x = date_time, y = result, color = location), size = 1) + 
    #viridis::scale_color_viridis(discrete = TRUE, end = 0.75, name = "Location") +
    scale_color_manual(values = c("#ff5722ff", "#4285f4ff"), name = "Location") + 
    scale_alpha(guide = "none") + 
    # scale_fill_brewer(palette = "Set2", drop = FALSE, na.translate = FALSE) + 
    scale_fill_discrete(drop = FALSE, na.translate = FALSE) +
    #viridis:scale_fill_viridis(discrete = TRUE, na.value = "grey50", option = "magma") + 
    #scale_color_manual(values = c("#ff5722ff", "#4285f4ff", "#2d3b45ff"), name = "Date")+
    facet_wrap(~metric, scales = "free_y", ncol = 1) +
    theme_bw(base_size = 22) + 
    ggtitle(paste0("Sampling Date: ", i)) + 
    #ggthemes::theme_pander(base_size = 22) +
    xlab("\nTime")+
    ylab("Concentration (ppm)\n") + 
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "snow2"),
          #strip.background = element_blank(),
          strip.text = element_text(color = "black", face = "bold", size = 18),
          #legend.background = element_rect(fill = "#ffc263ff"),
          #legend.key = element_rect(fill = "#ffc263ff"),
          panel.spacing = unit(1, "lines"),
          #plot.background = element_rect(fill = "#ffc263ff"),
          text = element_text(family = "Arial"),
          plot.margin = margin(1,1,1.5,1.2, "cm")
    ) + 
    guides(colour = guide_legend(override.aes = list(size=3)))# + 

  
  print(field_log_plot[[i]])
  ggsave(paste0(i," PM and CO2 measurements with field log.png"), width = 18, height = 10, units = "in")
  
  
}

####### ADD WEATHER DATA ####### 

weather_data <- drive_read_string("https://drive.google.com/file/d/1cJWjM6izcUBZnGsOsLSmYPK63wikAaFU/view?usp=share_link") %>% 
  read.csv(text = .) %>% 
  mutate(valid = mdy_hm(valid),
         date = ymd(str_remove_all(as.character(valid), "\\d*:\\d*:\\d*")),
         # time = hms(str_extract(as.character(valid), "\\d*:\\d*:\\d*")),
         hour = hour(hms(str_extract(as.character(valid), "\\d*:\\d*:\\d*"))))


measurement_log_weather <- measurement_w_log %>% 
  left_join(weather_data)




###############################################################################
# 7. AHz DATA EXPLORING  #########################################
###############################################################################


brewing_step_comp <- measurement_w_log %>% 
  filter(`Sampling Stage` != "Other") %>% 
  mutate(samp_stage_test = case_when(!`Sampling Stage` %in% c("10. Boiling", "4. Mashing")  ~ "Other Brewing Step",
                                     TRUE ~ as.character(`Sampling Stage`)))




gtsummary::tbl_strata(data = brewing_step_comp %>% 
                        filter(str_detect(metric, "PM")) %>% 
                        filter(location == "Brew Pit"), 
                      strata = metric,
                      .tbl_fun = ~.x %>%
                        tbl_summary(
                          by = samp_stage_test, 
                          digits = everything() ~ 2,
                          include = c("result"),
                          type = list("result" ~ 'continuous2'),
                          statistic = all_continuous() ~ c("{median} ({min} - {max})", 
                                                           "{mean} ({sd})",
                                                           "{p90}")), 
                      .combine_with = "tbl_stack"
)

gtsummary::tbl_strata(data = all_measurement_data,  #%>% 
                        #filter(location == "Brew Pit"), 
                      strata = metric,
                      .tbl_fun = ~.x %>%
                        tbl_summary(
                          by = location, 
                          digits = everything() ~ 2,
                          include = c("result"),
                          type = list("result" ~ 'continuous2'),
                          statistic = all_continuous() ~ c("{median} ({min} - {max})", 
                                                           "{mean} ({sd})",
                                                           "{p90}")), 
                      .combine_with = "tbl_stack"
)

ggplot(brewing_measurement_data %>% 
         filter(str_detect(metric, "PM")),
       aes(x = location, y = result, color = location)) + 
  ggforce::geom_sina(alpha = 0.5) +
  geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, size = 1, color = "#3a3838") +
  scale_color_manual(values = c("#ff5722ff", "#4285f4ff")) + 
  #scale_color_brewer(palette = "Set2") + 
  # scale_color_viridis_d(begin = 0.25, end = 0.75, option = "magma") + 
  facet_wrap(~metric, scales = "free") + 
  theme_bw(base_size = 22) + 
  xlab("\nLocation")+
  ylab("Concentration (ppm)\n") + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "snow2"),
        strip.background = element_blank(),
        strip.text = element_text(color = "black", face = "bold", size = 18),
        legend.position = "none",
        #legend.background = element_rect(fill = "#ffc263ff"),
        #legend.key = element_rect(fill = "#ffc263ff"),
        panel.spacing = unit(1, "lines"),
        #plot.background = element_rect(fill = "#ffc263ff"),
        text = element_text(family = "Arial"),
        plot.margin = margin(1,1,1.5,1.2, "cm")
  ) + 
  guides(colour = guide_legend(override.aes = list(size=3)))

ggsave("boxplots all active brewing.png", width = 18, height = 10, units = "in")



ggplot(measurement_w_log %>% 
         filter(str_detect(metric, "PM")) %>% 
         filter(!`Sampling Stage` %in% c("1. Precision Check", "2. Background Run","Other") & !is.na(`Sampling Stage`)) %>% 
         mutate(`Sampling Stage` = factor(`Sampling Stage`, 
                                          levels = c("1. Precision Check",
                                                     "2. Background Run",
                                                     "3. Grain Pouring",
                                                     "4. Mashing" ,
                                                     "5. Mash Rest" ,
                                                     "6. Solids Settling",
                                                     "7. Recirculating of Solids" ,
                                                     "8. Transfer",
                                                     "9. Scooping Out Spent Grains",
                                                     "10. Boiling",
                                                     "11. Cooling" ,
                                                     "12. Hops added",
                                                     "Other"),
                                          labels = c("Precision Check",
                                                     "Background Run",
                                                     "Grain\nPouring",
                                                     "Mashing" ,
                                                     "Mash\nRest" ,
                                                     "Solids\nSettling",
                                                     "Recirculating\nof Solids" ,
                                                     "Transfer",
                                                     "Scooping Out\nSpent Grains",
                                                     "Boiling",
                                                     "Cooling" ,
                                                     "Hops\nadded",
                                                     "Other"))), #%>% 
         # mutate(samp_stage_label = factor(samp_stage_label, levels = c(as.character(1:12), "NA")),
         #        `Sampling Stage` = str_wrap(as.character(`Sampling Stage`), 10),
         #        `Sampling Stage` = fct_reorder(`Sampling Stage`, as.character(samp_stage_label), min)), 
       aes(x = `Sampling Stage`, y = result, color = metric)) + 
  ggforce::geom_sina(alpha = 0.5) +
  geom_boxplot(width = 0.2, guides = FALSE, outlier.shape = NA, size = 0.5, color = "#3a3838") +
  scale_color_brewer(palette = "Set2") + 
  #scale_color_manual(values = c("#ff5722ff", "#4285f4ff")) + 
  #scale_color_brewer(palette = "Set2") + 
  # scale_color_viridis_d(begin = 0.25, end = 0.75, option = "magma") + 
  facet_wrap(~metric, scales = "free", ncol = 1) + 
  theme_bw(base_size = 22) + 
  xlab("\nBrewing Stage")+
  ylab("Concentration (ppm)\n") + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "snow2"),
        strip.background = element_blank(),
        strip.text = element_text(color = "black", face = "bold", size = 18),
        legend.position = "none",
        #legend.background = element_rect(fill = "#ffc263ff"),
        #legend.key = element_rect(fill = "#ffc263ff"),
        #panel.spacing = unit(1, "lines"),
        #plot.background = element_rect(fill = "#ffc263ff"),
        text = element_text(family = "Arial"),
        plot.margin = margin(1,1,1.5,1.2, "cm")
  ) + 
  guides(colour = guide_legend(override.aes = list(size=3)))

ggsave("boxplots sampling stage.png", width = 18, height = 10, units = "in")


ggplot(measurement_w_log %>% 
         filter(str_detect(metric, "PM")) %>% 
         filter(location == "Brew Pit") %>% 
         filter(`Sampling Stage` %in% c("3. Grain Pouring", "4. Mashing", "10. Boiling")) %>% 
         #filter(!`Sampling Stage` %in% c("1. Precision Check", "2. Background Run","Other") & !is.na(`Sampling Stage`)) %>% 
         mutate(`Sampling Stage` = factor(`Sampling Stage`, 
                                          levels = c("1. Precision Check",
                                                     "2. Background Run",
                                                     "3. Grain Pouring",
                                                     "4. Mashing" ,
                                                     "5. Mash Rest" ,
                                                     "6. Solids Settling",
                                                     "7. Recirculating of Solids" ,
                                                     "8. Transfer",
                                                     "9. Scooping Out Spent Grains",
                                                     "10. Boiling",
                                                     "11. Cooling" ,
                                                     "12. Hops added",
                                                     "Other"),
                                          labels = c("Precision Check",
                                                     "Background Run",
                                                     "Grain\nPouring",
                                                     "Mashing" ,
                                                     "Mash\nRest" ,
                                                     "Solids\nSettling",
                                                     "Recirculating\nof Solids" ,
                                                     "Transfer",
                                                     "Scooping Out\nSpent Grains",
                                                     "Boiling",
                                                     "Cooling" ,
                                                     "Hops\nadded",
                                                     "Other"))) %>% 
         group_by(date, location, metric, `Sampling Stage`) %>% 
         arrange(date_time) %>% 
         mutate(rec_num = seq_along(1:n())),
        #%>% 
       # mutate(samp_stage_label = factor(samp_stage_label, levels = c(as.character(1:12), "NA")),
       #        `Sampling Stage` = str_wrap(as.character(`Sampling Stage`), 10),
       #        `Sampling Stage` = fct_reorder(`Sampling Stage`, as.character(samp_stage_label), min)), 
       aes(x = rec_num, y = result, color = as.character(date)), group = location) + 
  geom_line() + 
  # ggforce::geom_sina(alpha = 0.5) +
  # geom_boxplot(width = 0.2, guides = FALSE, outlier.shape = NA, size = 0.5, color = "#3a3838") +
  #scale_color_brewer(palette = "Set2") + 
  #scale_color_manual(values = c("#ff5722ff", "#4285f4ff")) + 
  #scale_color_brewer(palette = "Set2") + 
  scale_color_viridis_d( end = 0.75, name = "Date") + 
  facet_grid(metric~`Sampling Stage`, scales = "free") + 
  theme_bw(base_size = 22) + 
  xlab("\n# of hours passed")+
  ylab("Concentration (ppm)\n") + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "snow2"),
        strip.background = element_blank(),
        strip.text = element_text(color = "black", face = "bold", size = 18),
        #legend.position = "none",
        #legend.background = element_rect(fill = "#ffc263ff"),
        #legend.key = element_rect(fill = "#ffc263ff"),
        #panel.spacing = unit(1, "lines"),
        #plot.background = element_rect(fill = "#ffc263ff"),
        text = element_text(family = "Arial"),
        plot.margin = margin(1,1,1.5,1.2, "cm")
  ) + 
  guides(colour = guide_legend(override.aes = list(size=3)))

# 
# measurement_w_log %>% 
#   filter(str_detect(metric, "PM")) %>% 
#   filter(!`Sampling Stage` %in% c("1. Precision Check", "2. Background Run","Other") & !is.na(`Sampling Stage`)) %>% 
#   group_by(metric, `Sampling Stage`) %>% 
#   summarize(median = median(result))%>%
#   arrange(desc(median))

# brewing_step_comp_test <- brewing_step_comp %>% 
#   filter(str_detect(metric, "PM")) %>% 
#   filter(location == "Brew Pit")
# 
# tidy(kruskal.test(brewing_step_comp_test$result, brewing_step_comp_test$samp_stage_test))


ggplot(brewing_step_comp %>% 
         filter(str_detect(metric, "PM") | str_detect(metric, "CO2")),
         aes(x = samp_stage_test, y = result, color = samp_stage_test)) + 
  ggforce::geom_sina(alpha = 0.5) +
  geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, size = 1, color = "#3a3838") +
  scale_color_brewer(palette = "Set2") + 
  # scale_color_viridis_d(begin = 0.25, end = 0.75, option = "magma") + 
  facet_grid(metric~location, scales = "free_y") + 
  theme_bw()


brewing_measurement_data %>% 
  select(date, hour, minute, metric, result, location, sampling_status) %>% 
  pivot_wider(names_from = metric, values_from = result) %>% 
  filter(!is.na(`PM2.5 (ppm)`) & !(is.na(`PM10 (ppm)`))) %>% 
  # group_by(metric) %>% 
  mutate(PM2.5_quantiles = cut(`PM2.5 (ppm)`, breaks = c(quantile(`PM2.5 (ppm)`, probs = seq(0,1, by = 0.25))),
                         labels = c(1:4), include.lowest = TRUE, right = FALSE),
         PM10_quantiles = cut(`PM10 (ppm)`, breaks = c(quantile(`PM10 (ppm)`, probs = seq(0,1, by = 0.25))),
                               labels = c(1:4), include.lowest = TRUE, right = FALSE)) %>% 
  pivot_longer(names_to = "PM", values_to = "quantile", PM2.5_quantiles:PM10_quantiles) %>% 
ggplot(aes(x = as.character(quantile), y = `Temperature (F)`)) + 
  geom_boxplot() + 
  facet_wrap(~PM)







get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}



brewing_bp_corr <- brewing_measurement_data %>% 
  filter(location == "Brew Pit") %>% 
  select(-file_name) %>% 
  pivot_wider(names_from = "metric", values_from = "result") 


cor.matrix_bp <- cor(brewing_bp_corr[,11:15], method = "spearman",
                  use = "complete.obs")

reshape2::melt(get_upper_tri(cor.matrix_bp)) %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(label = round(value,2))) +
  scale_fill_gradient2(limit = c(-1,1), breaks = c(-1, -.75 ,-.5, -.25, 0, .25,.5, .75, 1),
                       low = "#29af7f", high =  "#b8de29", mid = "white",
                       name = "Cor value") +
  scale_x_discrete(position = "top") +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12)) +
  xlab("")+
  ylab("") + 
  ggtitle("Spearman Corr Brew Pit")


brewing_tap_corr <- brewing_measurement_data %>% 
  filter(location == "Taproom") %>% 
  select(-file_name) %>% 
  pivot_wider(names_from = "metric", values_from = "result") 


cor.matrix_tap <- cor(brewing_tap_corr[,11:14], method = "spearman",
                     use = "complete.obs")

reshape2::melt(get_upper_tri(cor.matrix_tap)) %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(label = round(value,2))) +
  scale_fill_gradient2(limit = c(-1,1), breaks = c(-1, -.75 ,-.5, -.25, 0, .25,.5, .75, 1),
                       low = "#29af7f", high =  "#b8de29", mid = "white",
                       name = "Cor value") +
  scale_x_discrete(position = "top") +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12)) +
  xlab("")+
  ylab("") + 
  ggtitle("Spearman Corr Taproom")



all_weather_corr <- brewing_measurement_data %>% 
  left_join(weather_data) %>% 
  select(-file_name) %>% 
  pivot_wider(names_from = "metric", values_from = "result") 

cor.matrix.weather <- cor(all_weather_corr[,13:19], method = "spearman",
                          use = "complete.obs")
reshape2::melt(get_upper_tri(cor.matrix.weather)) %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(label = round(value,2))) +
  scale_fill_gradient2(limit = c(-1,1), breaks = c(-1, -.75 ,-.5, -.25, 0, .25,.5, .75, 1),
                       low = "#29af7f", high =  "#b8de29", mid = "white",
                       name = "Cor value") +
  scale_x_discrete(position = "top") +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12)) +
  xlab("")+
  ylab("") + 
  ggtitle("Spearman Corr weather (all")


reshape2::melt(get_upper_tri(cor(all_weather_corr[, 13:19], method = "spearman",
                                 use = "complete.obs"))) %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(label = round(value,2))) +
  scale_fill_gradient2(limit = c(-1,1), breaks = c(-1, -.75 ,-.5, -.25, 0, .25,.5, .75, 1),
                       low = "#29af7f", high =  "#b8de29", mid = "white",
                       name = "Cor value") +
  scale_x_discrete(position = "top") +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12)) +
  xlab("")+
  ylab("") + 
  ggtitle("Spearman Corr weather (Brew Pit)")




ggpubr::ggscatter(all_weather_corr, x = "PM2.5 (ppm)", y = "tmpf", 
                   color = as.character("date"), scales = "free", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "spearman",
                  xlab = "Indoor", ylab = "Outdoor")


##### GARBAGE <3 ################

# ggplot(precision_check_sum) + 
#   geom_linerange(aes(x = as.character(date), ymin = min, ymax = max, 
#                      color = as.character(date)), 
#                  position = position_dodge(width = 0.5)) + 
#   geom_text(aes(x = as.character(date), y = (max+0.005), label = paste0(round(mean_RPD*100, 2), "%"),
#                 color = as.character(date)), position = position_dodge(width = 0.5)) + 
#   viridis::scale_color_viridis(discrete = TRUE, end = 0.75, name = "Date") +
#   theme_bw() +
#   xlab("# of minutes") +
#   ylab("Result (ppm)") +
#   theme(
#     axis.text = element_text(size=14),
#     strip.text = element_text(size = 16, face = "bold")) +
#   facet_wrap(~metric)

# rpd_calc <- function(value1, value2){
#   abs(value1-value2)/((value1+value2)/2)
# }
#   
#   
#   # brewing_measurement_data %>% 
#   filter(str_detect(metric, "PM")) %>% 
#   group_by(location, metric) %>% 
#   summarize(mean_PM = mean(result),
#             sd_PM = sd(result)) %>%  
#   left_join(hours_collected) %>%  
#   mutate(power_calc = (mean_PM-((2.802*sd_PM)/sqrt(hours_collected*15))),
#          pwr_cl = power_calc/mean_PM)
#   
#   
#   # mean_RPD <- precision_check %>% 
#   filter(rec_num <= 5) %>% 
#   group_by(date, metric) %>% 
#   summarize(mean_RPD = mean(RPD))
#   
#   
#   
#   
# precision_check %>% 
#   filter(rec_num <= 5) %>% 
#   ggplot() + 
#   geom_point(aes(x = rec_num, y = `Brew Pit`, 
#                  color = as.character(date))) + 
#   geom_point(aes(x = rec_num, y = `Tap Room`, 
#                  color = as.character(date))) + 
#   geom_linerange(aes(x = rec_num, ymin = `Brew Pit`, ymax = `Tap Room`, 
#                      color = as.character(date))) + 
#   # geom_text(aes(x = rec_num, y = 0.05, 
#   #               label = paste0(round(RPD*100, 0), "%"), 
#   #               color = as.character(date)), show.legend = F) + 
#   #geom_label(mean_RPD, mapping = aes(x = 15, y = 0, label = paste0("Mean RPD: ", paste0(round(mean_RPD*100, 0), "%")))) + 
#   geom_text(precision_check_sum, mapping = aes(x = 0.5, y = 0.04, 
#                                                label = paste0("Mean RPD: ", paste0(round(mean_RPD*100, 0), "%")),
#                                                color = as.character(date))) + 
#   # geom_point(shape = 2) +
#   # geom_point(aes(x = rec_num, y = `Brew Pit`, color = as.character(date)), shape = 1) + 
#   viridis::scale_color_viridis(discrete = TRUE, end = 0.75, name = "Date") +
#   #scale_x_time(labels = scales::label_time(format = '%H:%M')) +
#   #scale_y_continuous(labels = scales::percent) +
#   theme_bw() +
#   xlim(0, 5) + 
#   xlab("# of minutes") +
#   ylab("Measurement (ppm)") +
#   theme(legend.position = "none", 
#         axis.text = element_text(size=14),
#         strip.text = element_text(size = 16, face = "bold")) +
#   facet_grid(metric~date) + 
#   ggtitle("Measurement and mean RPD for sidepak measurements during sampling precision check")
#   
#   # ggplot(compare_dates, aes(x = rec_num/60, y = result, color = as.character(date), alpha = intervention)) + 
#   geom_line(size = 1) + 
#   viridis::scale_color_viridis(discrete = TRUE, end = 0.75, name = "Date") +
#   scale_alpha_manual(values = c(0.25, 1), name = "Intervention") + 
#   #scale_color_manual(values = c("#ff5722ff", "#4285f4ff", "#2d3b45ff"), name = "Date")+
#   facet_grid(metric~location, scales = "free") +
#   theme_bw(base_size = 22) + 
#   #ggthemes::theme_pander(base_size = 22) +
#   xlab("\n# of hours passed")+
#   ylab("Concentration\n") + 
#   theme(panel.grid.major.y = element_blank(),
#         panel.grid.major.x = element_line(color = "snow2"),
#         #strip.background = element_blank(),
#         strip.text = element_text(color = "black", face = "bold", size = 18),
#         #legend.background = element_rect(fill = "#ffc263"),
#         #legend.key = element_rect(fill = "#ffc263"),
#         panel.spacing = unit(1, "lines"),
#         #plot.background = element_rect(fill = "#ffc263"),
#         text = element_text(family = "Arial"),
#         plot.margin = margin(1,1,1.5,1.2, "cm")
#         ) + 
#   guides(colour = guide_legend(override.aes = list(size=3)))

# ggplot(brewing_measurement_data %>% 
#          filter(str_detect(metric, "PM")) %>% 
#          filter(date == "2022-11-08"), aes(x = date_time, y = result, color = location)) + 
#   geom_line(size = 1) + 
#   viridis::scale_color_viridis(discrete = TRUE, end = 0.75, name = "Location") +
#   #scale_color_manual(values = c("#ff5722ff", "#4285f4ff", "#2d3b45ff"), name = "Date")+
#   facet_wrap(~metric, scales = "free_y", ncol = 1) +
#   theme_bw(base_size = 22) + 
#   #ggthemes::theme_pander(base_size = 22) +
#   xlab("\nTime")+
#   ylab("Concentration (ppm)\n") + 
#   theme(panel.grid.major.y = element_blank(),
#         panel.grid.major.x = element_line(color = "snow2"),
#         #strip.background = element_blank(),
#         strip.text = element_text(color = "black", face = "bold", size = 18),
#         #legend.background = element_rect(fill = "#ffc263"),
#         #legend.key = element_rect(fill = "#ffc263"),
#         panel.spacing = unit(1, "lines"),
#         #plot.background = element_rect(fill = "#ffc263"),
#         text = element_text(family = "Arial"),
#         plot.margin = margin(1,1,1.5,1.2, "cm")
#   ) + 
#   guides(colour = guide_legend(override.aes = list(size=3)))
# 
# 
# ggsave("PM measurements day 2.png", width = 18, height = 10, units = "in")
