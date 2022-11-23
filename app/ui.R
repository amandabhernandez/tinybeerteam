### AUTHOR: AHz
### LAST EDIT: 2022-11-02
### WRITTEN IN: R version 4.0.5
### Purpose: TBT Explorer dashboard


dashboardPage(
  
  # Application title
  dashboardHeader(title = "TBT Explorer"),
  
  
  
  
  
  # Sidebar with a slider input for number of bins
  dashboardSidebar(
   
    
    br(),
    sidebarMenu(menuItem("Dashboard", tabName = "dashboard", icon = icon("beer"))),
    br(),

    sidebarMenu(menuItem("Metrics", tabname = "metrics", icon = icon("area-chart"), startExpanded = FALSE,
                         uiOutput("date_select"), 
                         menuSubItem("Temperature",
                                     tabName = "temp"),
                         menuSubItem("Humidity",
                                     tabName = "humidity"),
                         menuSubItem("CO2",
                                     tabName = "co2"),
                         menuSubItem("PM",
                                     tabName = "pm"))
                ),
    sidebarMenu(menuItem("Study Design", tabname = "study_design", icon = icon("users"), startExpanded = FALSE,
                         menuSubItem("Power Calculations",
                                     tabName = "pwr")
                         )
    ),
    br(),
    sidebarMenu(menuItem("Source code", 
                         icon = icon("github"), 
                         href = "https://github.com/amandabhernandez/tinybeerteam"))
    
  ),
  
  # Show a plot of the generated distribution
  dashboardBody(
    tags$head(includeHTML(("google-analytics.html"))),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(h3(" Time Series Graphs"), 
                       uiOutput("metric"),
                       #uiOutput("roavg"),
                       plotlyOutput("airplot"))
      ),
      tabItem(tabName = "temp",
              fluidRow(box(title = "Time Series Graph",
                           plotlyOutput("tempplot"), width = 12, status = "primary", 
                           solidHeader = TRUE),
                       box(width = 6, status = "info", 
                           solidHeader = TRUE, 
                           title = "Distribution of temperature measurements", 
                           plotlyOutput("temp_density")),
                       )),
      tabItem(tabName = "humidity",
              fluidRow(box(title = "Time Series Graph",
                           plotlyOutput("humplot"), width = 12, status = "primary", 
                           solidHeader = TRUE),
                       box(width = 6, status = "info", 
                           solidHeader = TRUE, 
                           title = "Distribution of humidity measurements", 
                           plotlyOutput("hum_density")),
                       )),
      tabItem(tabName = "co2",
              fluidRow(box(title = "Time Series Graph",
                           plotlyOutput("co2plot"), width = 12, status = "primary", 
                           solidHeader = TRUE),
                       box(width = 6, status = "info", 
                           solidHeader = TRUE, 
                           title = "Distribution of CO2 measurements", 
                           plotlyOutput("co2_density")),
                       )),
      tabItem(tabName = "pm",
              fluidRow(box(title = "Time Series Graph",
                           plotlyOutput("pmplot"), width = 12, status = "primary", 
                           solidHeader = TRUE),
                       box(width = 6, status = "info", 
                           solidHeader = TRUE, 
                           title = "Distribution of PM measurements", 
                           plotlyOutput("pm_density")),
                       )),
      tabItem(tabName = "pwr",
              fluidRow(box(title = "Up to date power calculation",
                           uiOutput("effect_size"), 
                           uiOutput("samp_interval"), 
                           DT::dataTableOutput("pwr_calc_table"), width = 12, status = "primary", 
                           solidHeader = TRUE),
                       box(width = 6, status = "info", 
                           solidHeader = TRUE, 
                           title = "Distribution of PM measurements", 
                           plotlyOutput("pm_density_all")),
                       box(width = 6, status = "info", 
                           solidHeader = TRUE, 
                           title = "Boxplot of PM measurements", 
                           plotlyOutput("pm_boxplot_all")),
              ))

                       ))

  
)

