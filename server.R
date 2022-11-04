#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$airplot <- renderPlotly({

      air_plot <- ggplot(all_dat, aes(x = date_time, y = Result ,
                                       label = time,
                                       label2 = date)) +
        geom_line(aes(color = location), size = 1) +
        scale_x_time(labels = scales::label_time(format = '%H:%M')) + 
        viridis::scale_color_viridis(discrete = TRUE, end = 0.75) +
        facet_wrap(~metric, ncol = 1, scales = "free_y") +
        ggthemes::theme_pander() +
        xlab("Date")+
        ylab("Concentration") +
        theme(#legend.position = "none",
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(color = "snow2"),
              strip.text.x = element_text(color = "black", face = "bold"),
              text = element_text(family = "Arial"))
      
  
  #print figure
  ggplotly(air_plot,
           tooltip = c("y", "label", "label2"),
           height = length(unique(all_dat$metric))*300)
  
  })
  
  #temperature page 
  output$tempplot <- renderPlotly({
    time_subplot(all_dat%>% 
                   filter(metric == "Temperature (F)"), "Temperature (F)")
    
    
  })
  output$temp_density <- renderPlotly({
    dens_plotly(all_dat%>% 
                  filter(metric == "Temperature (F)"), "Temperature (F)")
  })
  
  
  #humidity page 
  output$humplot <- renderPlotly({
    time_subplot(all_dat%>% 
                   filter(metric == "Relative Humidity (%)"), "Relative Humidity (%)")
    
  })
  output$hum_density <- renderPlotly({
    dens_plotly(all_dat %>% 
                  filter(metric == "Relative Humidity (%)"), "Relative Humidity (%)")
  })
  
  
  #CO2 page 
  output$co2plot <- renderPlotly({
    time_subplot(all_dat %>% filter(metric == "CO2 (ppm)"), "CO2 (ppm)")
    
    
  })
  output$co2_density <- renderPlotly({
    dens_plotly(all_dat %>% filter(metric == "CO2 (ppm)"), "CO2 (ppm)")
    
  })
  
  
  
  #PM page
  output$pmplot <- renderPlotly({
    
    
    
    sep_pm_plot <- ggplot(all_dat %>% filter(str_detect(metric, "PM"))) +
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
    
    sep_pm_box <-  ggplot(all_dat %>% filter(str_detect(metric, "PM"))) + 
      geom_boxplot(aes(x = location, y = Result, fill = location)) + 
      viridis::scale_color_viridis(discrete = TRUE, end = 0.75) +   
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
    dens_plotly(all_dat %>% filter(str_detect(metric, "PM")), c("PM2.5", "PM10"))
    
  })
  

})


