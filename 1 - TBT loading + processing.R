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
  filter(`Sampling Stage` == "Brewing") 

background_precision <- all_measurement_data %>% 
  filter(`Sampling Stage` %in% c("Precision Check", "Background Run")) 


###############################################################################
# 3. QAQC CHECKS  #########################################
###############################################################################

#are there any gaps? 
check_gaps <- all_measurement_data %>% 
  group_by(date, metric, location) %>% 
  arrange(date_time) %>% 
  mutate(rec_after = lead(date_time, order_by = date_time, 
                          default = max(date_time)),
         check_lag = minute - lag(minute(rec_after)))

stopifnot(sum(check_gaps$check_lag, na.rm = TRUE) == 0 & sum(is.na(check_gaps$check_lag)) == 9*length(unique(check_gaps$date)))
#table(check_gaps$check_lag, useNA = "ifany")
#all lag = 0, 9 NA for first measurements for 
#((4 metrics *2 locations) + (1metric *1 location)*number of sampling dates) -- all good!



#are there any samples that didn't collect?
table(is.na(all_measurement_data$Result), all_measurement_data$metric)
#two HOBO measurements
#AHz: went back and spot checked these -- no good explanation for it, but another measurement
#was made during the same minute, it just recorded at two different second intervals but one
#was empty... weird but fine. 

######### LOOK AT PRECISION CHECKS #############

precision_check <- background_precision %>% 
  filter(str_detect(metric, "PM")) %>% 
  select(-file_name, -date_time, -time) %>% 
  pivot_wider(names_from = location, values_from = Result) %>% 
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
  geom_text(precision_check_sum, mapping = aes(x = 0.5, y = 0.04, 
                                               label = paste0("Mean RPD: ", paste0(round(mean_RPD*100, 0), "%")),
                                               color = as.character(date))) + 
  viridis::scale_color_viridis(discrete = TRUE, end = 0.75, name = "Date") +
  theme_bw() +
  xlim(0, 15) + 
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

ggplot(compare_dates, aes(x = rec_num/60, y = Result, color = as.character(date), alpha = intervention)) + 
  geom_line(size = 1) + 
  viridis::scale_color_viridis(discrete = TRUE, end = 0.75, name = "Date") +
  scale_alpha_manual(values = c(0.25, 1), name = "Intervention") + 
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

ggsave("PM and CO2 measurements normalized to hours of sampling_asof_2022-11-21.png", 
       width = 18, height = 10, units = "in")


ggplot(brewing_measurement_data %>% 
         filter(str_detect(metric, "PM")) %>% 
         filter(date == "2022-11-17"), aes(x = date_time, y = Result, color = location)) + 
  geom_line(size = 1) + 
  viridis::scale_color_viridis(discrete = TRUE, end = 0.75, name = "Location") +
  #scale_color_manual(values = c("#ff5722ff", "#4285f4ff", "#2d3b45ff"), name = "Date")+
  facet_wrap(~metric, scales = "free_y", ncol = 1) +
  theme_bw(base_size = 22) + 
  #ggthemes::theme_pander(base_size = 22) +
  xlab("\nTime")+
  ylab("Concentration (ppm)\n") + 
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


ggsave("PM measurements day 2.png", width = 18, height = 10, units = "in")


######### GET SUMMARY STATS #############


gtsummary::tbl_strata(data = brewing_measurement_data %>% filter(str_detect(metric, "PM")), 
                      strata = metric,
                      .tbl_fun = ~.x %>%
                        tbl_summary(
                          by = location, 
                          digits = everything() ~ 2,
                          include = c("Result"),
                          type = list("Result" ~ 'continuous2'),
                          statistic = all_continuous() ~ c("{median} ({min} - {max})", 
                                                           "{mean} ({sd})",
                                                           "{p")), 
                      .combine_with = "tbl_stack"
)



######### REDO POWER CALC #############

hours_collected <- brewing_measurement_data %>% 
  group_by(date, metric, location) %>% 
  summarize(sampling_time = as.duration(interval(start = min(date_time), end = max(date_time)))) %>% 
  group_by(metric, location) %>% 
  summarize(hours_collected = (sum(sampling_time)/60)/60)




#redo power calc
power_calc_update <- brewing_measurement_data %>% 
  filter(str_detect(metric, "PM")) %>% 
  group_by(location, date, metric) %>% 
  summarize(mean_PM = mean(Result),
            PM_90 = quantile(Result, .9), 
            sd_PM = sd(Result)) %>% 
  bind_rows(brewing_measurement_data %>% 
              filter(str_detect(metric, "PM")) %>% 
              group_by(location, metric) %>% 
              summarize(mean_PM = mean(Result),
                        PM_90 = quantile(Result, .9), 
                        sd_PM = sd(Result))) %>% 
  bind_rows(brewing_measurement_data %>% 
              filter(str_detect(metric, "PM")) %>% 
              group_by(metric) %>% 
              summarize(mean_PM = mean(Result),
                        PM_90 = quantile(Result, .9), 
                        sd_PM = sd(Result))) %>% 
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
  group_by(location, date, metric) %>% 
  arrange(date_time) %>% 
  mutate(rec_num = seq_along(1:n()),
         rec_group = as.numeric(str_extract(as.character(rec_num/15-0.0001), "\\d*"))) %>% 
  group_by(location, date, metric, rec_group) %>% 
  summarize(avg_15_min = mean(Result),
            n = n()) %>% 
  filter(n == 15)

ggplot(brewing_15min_sample %>% 
         filter(str_detect(metric, "PM")), aes(x = rec_group, y = avg_15_min, color = as.character(date))) + 
  geom_point() + 
  geom_line() + 
  viridis::scale_color_viridis(discrete = TRUE, end = 0.75, name = "Date") +
  theme_bw() + 
  facet_grid(metric~location, scales = "free")

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
#   summarize(mean_PM = mean(Result),
#             sd_PM = sd(Result)) %>%  
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

