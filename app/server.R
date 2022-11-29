#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library(shiny)


shinyServer(function(input, output) {

  
  output$date_select <-  renderUI({
    pickerInput("date_select", "Select Date", 
                #choices = c("2022-11-08"), 
                #selected = c("2022-11-08"),
                choices = c(sample_dates),
                selected = max(sample_dates),
                options = list(`actions-box` = TRUE,
                               title = "Select"),
                multiple = FALSE)})
  
  sampling_date_dat <- eventReactive(input$date_select, {
    all_dat_avg %>% 
      filter(date == ymd(str_extract(input$date_select, "(?<=: ).*"))) 
    }
    
  )
    

  
  output$airplot <- renderPlotly({
    
    new_col <- function(dat){
      dat %>% 
        group_by(.) %>% 
        mutate(rec_num = seq_along(1:n()))
      
    }
    
    plot_dat <- all_dat_avg %>% 
      filter(`Sampling Status` == "Brewing") %>% 
      group_by(date, location, metric) %>% 
      mutate(rec_num = row_number()/60)


      # air_plot <- ggplot(plot_dat, aes(x = date_time, y = Result ,
      #                                  label = time,
      #                                  label2 = date)) +
      #   geom_line(aes(color = location), size = 1) +
      #   scale_x_time(labels = scales::label_time(format = '%H:%M')) + 
      #   viridis::scale_color_viridis(discrete = TRUE, end = 0.75) +
      #   facet_wrap(~metric, ncol = 1, scales = "free_y") +
      #   ggthemes::theme_pander() +
      #   xlab("Date")+
      #   ylab("Concentration") +
      #   theme(#legend.position = "none",
      #         panel.grid.major.y = element_blank(),
      #         panel.grid.major.x = element_line(color = "snow2"),
      #         strip.text.x = element_text(color = "black", face = "bold"),
      #         text = element_text(family = "Arial"))
      
      air_plot <- ggplot(plot_dat %>% 
                           mutate(`Hours Passed` = round(rec_num, 2),
                                  `Concentration Measured` = round(Result, 2)) %>% 
                           highlight_key( ~intervention, "Intervention"), 
                         aes(x = rec_num, y = Result, 
                             color = as.character(date),
                             label4 = date,
                             label = `Sampling Status`,
                             label2 = `Hours Passed`,
                             label3 = `Concentration Measured`,
                             alpha = intervention
                         )) +
        geom_line() + 
        viridis::scale_color_viridis(discrete = TRUE, end = 0.75, name = "Date") +
        scale_alpha_manual(values = c(0.75, 1))+
        #scale_linetype_manual(values=c("dotted", "solid"))+
        facet_grid(metric~location, scales = "free") +
        ggthemes::theme_pander() +
        xlab("# of hours passed")+
        ylab("Concentration") + 
        theme(panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(color = "snow2"),
              strip.text.x = element_text(color = "black", face = "bold"),
              text = element_text(family = "Arial"))
      
      color_highlight <- toRGB(viridis::viridis(n = 4, begin = 0.75, direction = -1))
  #print figure
  ggplotly(air_plot,
           tooltip = c("label4","label2", "label3"),
           height = length(unique(plot_dat$metric))*300) %>% 
    highlight(dynamic = TRUE, selectize = TRUE, color = color_highlight)
  
  })
  
  #temperature page 
  output$tempplot <- renderPlotly({
    time_subplot(sampling_date_dat() %>% 
                   filter(metric == "Temperature (F)"), "Temperature (F)")
    
    
  })
  output$temp_density <- renderPlotly({
    dens_plotly(sampling_date_dat()%>% 
                  filter(metric == "Temperature (F)"), "Temperature (F)")
  })
  
  
  #humidity page 
  output$humplot <- renderPlotly({
    time_subplot(sampling_date_dat()%>% 
                   filter(metric == "Relative Humidity (%)"), "Relative Humidity (%)")
    
  })
  output$hum_density <- renderPlotly({
    dens_plotly(sampling_date_dat() %>% 
                  filter(metric == "Relative Humidity (%)"), "Relative Humidity (%)")
  })
  
  
  #CO2 page 
  output$co2plot <- renderPlotly({
    time_subplot(sampling_date_dat() %>% filter(metric == "CO2 (ppm)"), "CO2 (ppm)")
    
    
  })
  output$co2_density <- renderPlotly({
    dens_plotly(sampling_date_dat() %>% filter(metric == "CO2 (ppm)"), "CO2 (ppm)")
    
  })
  
  
  
  #PM page
  output$pmplot <- renderPlotly({
    
    pm_plot_dat <- sampling_date_dat()
    
    sep_pm_plot <- ggplot(pm_plot_dat %>% filter(str_detect(metric, "PM"))) +
      geom_line(aes(x = date_time, y = Result ,
                    color = location,
                    label = time,
                    label2 = date)) +
      viridis::scale_color_viridis(discrete = TRUE, end = 0.75) +   
      facet_wrap(~metric, ncol = 1, scales = "free_y") +
      ggthemes::theme_pander() +
      xlab("Time")+
      ylab("Concentration (ppm)") +
      theme(legend.position = "none",
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "snow2"),
            strip.text.x = element_text(color = "black", face = "bold"),
            text = element_text(family = "Arial"), 
            panel.spacing = unit(2, "lines"))
    
    
    sep_pm_plotly <- ggplotly(sep_pm_plot, 
                              tooltip = c("y", "label", "label2"))
    
    sep_pm_box <-  ggplot(pm_plot_dat %>% filter(str_detect(metric, "PM"))) + 
      geom_boxplot(aes(x = location, y = Result, fill = location)) + 
      viridis::scale_fill_viridis(discrete = TRUE, end = 0.75, alpha = 0.5) +   
      facet_wrap(~metric, ncol = 1) + 
      ggthemes::theme_pander() +
      theme(legend.position = "none",
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "snow2"), 
            strip.text.x = element_blank(),
            text = element_text(family = "Arial"),
            panel.spacing = unit(2, "lines"))
    
    sep_pm_boxly <- ggplotly(sep_pm_box)
    
    
    subplot(list(sep_pm_plotly, sep_pm_boxly), widths = c(.8, .2), margin = 0)
    
    
  })
  output$pm_density <- renderPlotly({
    dens_plotly(sampling_date_dat() %>% filter(str_detect(metric, "PM")), c("PM2.5 (ppm)", "PM10 (ppm)"))
    
  })
  
  output$effect_size <- renderUI({
    #numericInput("effect_size", "Effect size: ", 30, min = 1, max = 100)
    numericInputIcon(
      inputId = "effect_size",
      label = "Effect Size",
      value = 30,
      step = 5, 
      max = 100,
      min = 0, 
      icon = list(NULL, icon("percent")),
      width = 100, 
    )
  })
  output$samp_interval <- renderUI({
    numericInputIcon(
      inputId = "samp_interval",
      label = "# of minutes per sample",
      value = 15,
      step = 2.5, 
      max = 60,
      min = 0, 
      icon = list(NULL, icon("clock")),
      width = 100, 
    )
  })
  
  output$pwr_calc_table <- DT::renderDataTable({
    
    req(input$effect_size)
    req(input$samp_interval)
    
    all_dat_avg %>% 
      # filter(intervention == "Normal Brewing") %>% 
      # filter(`Sampling Status` == "Brewing") %>% 
      # filter(str_detect(metric, "PM")) %>%
      # group_by(location, date, metric) %>% 
      # summarize(mean_PM = mean(Result),
      #           sd_PM = sd(Result)) %>% 
      #bind_rows(all_dat_avg %>% 
                  filter(intervention == "Normal Brewing") %>% 
                  filter(`Sampling Status` == "Brewing") %>% 
                  filter(str_detect(metric, "PM")) %>% 
                  group_by(location, metric) %>% 
                  summarize(mean_PM = mean(Result),
                            sd_PM = sd(Result)) %>% 
    #) %>% 
      # bind_rows(all_dat_avg %>% 
      #             filter(intervention == "Normal Brewing") %>% 
      #             filter(`Sampling Status` == "Brewing") %>% 
      #             filter(str_detect(metric, "PM"))  %>% 
      #             group_by(metric) %>% 
      #             summarize(mean_PM = mean(Result),
      #                       sd_PM = sd(Result))) %>% 
      # mutate(date = case_when(is.na(date) ~ "All Active Brewing Samples",
      #                         TRUE ~ as.character(date))) %>% 
      mutate(#sample_size_needed = ((1.96+.842)/((mean_PM-(mean_PM*0.7))/(sd_PM)))^2,
        samp_size_needed = (((1.96+0.842)^2)*2*sd_PM^2)/((mean_PM-mean_PM*(1-(input$effect_size/100)))^2),
        hours_needed = (samp_size_needed*input$samp_interval)/60) %>% 
      #filter(date == "All Active Brewing Samples" & !is.na(location)) %>% 
      left_join(all_dat_avg %>% 
                  filter(intervention == "Normal Brewing") %>% 
                  group_by(date, metric, location) %>% 
                  summarize(sampling_time = as.duration(interval(start = min(date_time), end = max(date_time)))) %>% 
                  group_by(metric, location) %>% 
                  summarize(min_collected = (sum(sampling_time)/60),
                            samples_collected = min_collected/input$samp_interval, 
                            hours_collected = min_collected/60)
      ) %>% 
      mutate(`Mean (SD)` = paste0(round(mean_PM, 3), " (", round(sd_PM, 3), ")"),
             `Status` = case_when(hours_collected > hours_needed ~ as.character(icon("thumbs-up", lib = "font-awesome")),
                                     TRUE ~ as.character(icon("thumbs-down", lib = "font-awesome")))) %>% 
      select(location, metric, -min_collected, -mean_PM, -sd_PM, `Mean (SD)`,
             samp_size_needed, hours_needed, samples_collected, hours_collected, Status) %>% 
      rename(`Sample Size Needed` = samp_size_needed,
             `Hours Needed` = hours_needed,
             `Samples Collected` = samples_collected,
             `Hours Collected` = hours_collected) %>% 
      DT::datatable(escape = FALSE) %>%
     DT::formatRound(columns=c(4:7), digits=3)
    })
  

  output$pm_boxplot_all <- renderPlotly({
    pm_box <- all_dat_avg %>% 
      filter(`Sampling Status` == "Brewing") %>% 
      filter(intervention == "Normal Brewing") %>% 
      filter(str_detect(metric, "PM")) %>% 
      ggplot() + 
      geom_boxplot(aes(x = location, y = Result, fill = location)) + 
      ggthemes::theme_pander() +
      viridis::scale_fill_viridis(discrete = TRUE, end = 0.75, alpha = 0.5) +
      facet_grid(~metric) + 
      theme(legend.position = "none",
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "snow2"), 
            text = element_text(family = "Arial"))
  })
  
  output$pm_density_all <- renderPlotly({
    ggplot(all_dat_avg %>% 
             filter(`Sampling Status` == "Brewing") %>%
             filter(intervention == "Normal Brewing") %>% 
             filter(str_detect(metric, "PM")) 
             , aes(x = Result)) +
      geom_histogram(aes(y = ..density..),
                     color = "black",
                     fill = "white",
                     bins = 30) +
      geom_density(fill = "red", alpha = 0.25) + 
      facet_wrap(location~metric) + 
      ggthemes::theme_pander() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "snow2"),
            strip.text.x = element_text(color = "black", face = "bold"),
            text = element_text(family = "Arial"))
  })
  
})


