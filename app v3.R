
#' input$sigma
#' input$sumstat
#' input$method
#' input$width

# 1. Source ----

source("global.R")
#shiny::runApp(display.mode="showcase")
library(shiny.stats)
library(RSQLite)


#Getting the user for tracking shiny stats
#get_user <- function(session){
#  parseQueryString(isolate(session$clientData$url_search))$username
#}


#Connecting for shiny statistics
#connection <- odbc::dbConnect(RSQLite::SQLite(), dbname = "user_stats.sqlite")

#print(connection)

# 2. Header ----

header <- dashboardHeader(title = "Anomaly Detection POC")



# 3. Sidebar ----

sidebar <- dashboardSidebar(
  
  tags$style(HTML("")),                # Customize sidebar
  
  ############################## Filter options ##############################
  sidebarMenu(id = "main_menu",
              
              useShinyjs(),
              
              menuItem(text = "Filters",
                       tabName = "filters", 
                       
                       h5(HTML("<b>Primary</b>")),
                       
                       pickerInput(inputId = "site", 
                                   label = "Site", 
                                   choices = sort(dim_site()$Site_Short_Name),  
                                   selected = "AZ",
                                   multiple = TRUE,
                                   options = list(`actions-box` = TRUE)),
                       
                       dateRangeInput(inputId = "date", 
                                      label = "Date Range", 
                                      min = date_min, 
                                      max = date_max, 
                                      start = date_min,
                                      end = date_max),
                       
                       br(),
                       
                       h5(HTML("<b>Secondary</b>")),
                       
                       pickerInput(inputId = "facility", 
                                   label = "Facility", 
                                   choices = "Loading Choices...", 
                                   selected = "Loading Choices...",
                                   multiple = TRUE,
                                   options = list(`actions-box` = TRUE)),
                       
                       pickerInput(inputId = "feed", 
                                   label = "Feed", 
                                   choices = "Loading Choices...", 
                                   selected = "Loading Choices...",
                                   multiple = TRUE,
                                   options = list(`actions-box` = TRUE)),
                       
                       pickerInput(inputId = "vendor", 
                                   label = "Vendor", 
                                   choices = "Loading Choices...", 
                                   selected = "Loading Choices...",
                                   multiple = TRUE,
                                   options = list(`actions-box` = TRUE)),
                       
                       pickerInput(inputId = "parent", 
                                   label = "Parent Organization", 
                                   choices = "Loading Choices...", 
                                   selected = "Loading Choices...",
                                   multiple = TRUE,
                                   options = list(`actions-box` = TRUE)),
                       
                       pickerInput(inputId = "type", 
                                   label = "Facility Type", 
                                   choices = "Loading Choices...", 
                                   selected = "Loading Choices...",
                                   multiple = TRUE,
                                   options = list(`actions-box` = TRUE)),
                       # pickerInput(inputId = "element", 
                       #             label = "Data Element", 
                       #             choices = c("Discharge_Disposition", 
                       #                         "Admit_Date_Time", "Insurance_Coverage"), 
                       #             selected = "Discharge_Disposition",
                       #             multiple = TRUE),
                       pickerInput(inputId = "element", 
                                   label = "Data Element", 
                                   choices = keeps_vals, 
                                   selected = "Discharge_Disposition",
                                   multiple = TRUE),
                       
                       br()
                       
              ),
              
              menuItem(text = "Detection Options",
                       tabName = "options",
                       
                       useShinyjs(),
                       
                       radioButtons(inputId = "method", 
                                    label = "Detection Method",
                                    choices = c("None",
                                                "CUSUM",
                                                "EWMA", 
                                                "Trend"),  
                                    selected = "EWMA"),
                       
                       shinyjs::hidden(shiny::div(id = "cusum_options_div",
                                                  
                                                  radioButtons(inputId = "sumstat", 
                                                               label = "Summary Statistic",
                                                               choices = c("Mean",
                                                                           "Median"),  
                                                               selected = "Mean"),
                                                  
                                                  sliderInput(inputId = "width", 
                                                              label = "Range Width", 
                                                              min = 1, 
                                                              max = 60, 
                                                              value = 14, 
                                                              step = 1),
                                                  
                                                  sliderInput(inputId = "lim_warn", 
                                                              label = "Warning Sigma", 
                                                              min = 1, 
                                                              max = 3, 
                                                              value = 2, 
                                                              step = 0.5),
                                                  
                                                  sliderInput(inputId = "lim_alert", 
                                                              label = "Alert Sigma", 
                                                              min = 1, 
                                                              max = 3, 
                                                              value = 3, 
                                                              step = 0.5),
                                                  
                                                  sliderInput(inputId = "lim_outlier", 
                                                              label = "Anomaly Proportion Limit", 
                                                              min = 0.0, 
                                                              max = 1.0, 
                                                              value = 0.7, 
                                                              step = 0.01)
                                                  
                       )), 
                       
                       shinyjs::hidden(shiny::div(id = "essence_options_div",
                                                  
                                                  sliderInput(inputId = "baseline", 
                                                              label = "Baseline", 
                                                              min = 1, 
                                                              max = 90, 
                                                              value = 28, 
                                                              step = 1),
                                                  
                                                  sliderInput(inputId = "guardband", 
                                                              label = "Guardband", 
                                                              min = 0, 
                                                              max = 7, 
                                                              value = 2, 
                                                              step = 1),
                                                  
                                                  sliderInput(inputId = "smoothing", 
                                                              label = "Smoothing", 
                                                              min = 0, 
                                                              max = 1, 
                                                              value = 0.40, 
                                                              step = 0.01),
                                                  
                       )),
                       
                       
                       br()),
              
              menuItem(text = "Graphics Options", 
                       tabName = "graphics", 
                       
                       useShinyjs(),
                       
                       pickerInput(inputId = "stratification", 
                                   label = "Y-Axis & Color Variable", 
                                   choices = c("Feed_Name", 
                                               "Vendor_Name", 
                                               "Parent_Organization", 
                                               "Facility_Type"), 
                                   selected = "Facility_Type",
                                   multiple = FALSE),
                       
                       pickerInput(inputId = "facet", 
                                   label = "Faceting Variable", 
                                   choices = c("Feed_Name", 
                                               "Vendor_Name", 
                                               "Parent_Organization", 
                                               "Facility_Type"), 
                                   selected = "Parent_Organization",
                                   multiple = FALSE)
                                                  
                       )),
                       
                       br())
              
              
  



# 4. Body ----

body <- dashboardBody(
  
  setShadow(class = "dropdown-menu"),
  setShadow(class = "box"),
  
  tabPanel(title = "Validity", 
           value = "tab_validity", 
           
           h3("Validity Over Time"),
           
           helpText(HTML("<b>Validity Tab.</b> Validity by Arrived_Date. Aggregated on line color by overall or on select variables.")),
           
           helpText(HTML("<i>Unaffected by custom graph type or mode.</i>")),
           
           echarts4rOutput("validity_ts"),
           
           DT::dataTableOutput("validity_dt")
           
           
           
  )
  
)




# 5. UI ----

ui <- dashboardPage(header = header, 
                    sidebar = sidebar, 
                    body = body, 
                    skin = "blue",
                    #tags$head(
                      #tags$script(HTML(js)))
                    )




# 6. Server ----


server <- function(input, output, session) {
  
  app <- reactive({
    basename(session$clientData$url_hostname)
    })
  
  # Store in a convenience variable
  cdata <- session$clientData
  

  print(cdata$url_pathname)
    

  print(app)
  #print(input$myBrowser)
  
  #ids <- c("facility", "site")
 
  #auditR_log_API(input)
  
  
  #Actual server for app
  my_data <- reactiveValues()
  
  observe({
    ############################## Updating filters ##############################
    if (input$lim_warn >= input$lim_alert){
      
      updateSliderInput(inputId = "lim_warn", 
                        label = "Warning Sigma", 
                        min = 1, 
                        max = 3, 
                        value = 2, 
                        step = 0.5)
      
      updateSliderInput(inputId = "lim_alert", 
                        label = "Alert Sigma", 
                        min = 1, 
                        max = 3, 
                        value = 3, 
                        step = 0.5)
      
      sendSweetAlert(title = "Custom Threshold Error", 
                     session = getDefaultReactiveDomain(),
                     type = "error",
                     text = "Alert sigma must exceed warning sigma. Restoring defaults.")
      
    }
    
  })
  
  observe({
    
    # This was for when some filters would show or hide based on what method chosen.
    # I didn't end up needing any of those for how simple this app is, but I left the
    # section in case we want to add it back.
    shinyjs::useShinyjs()
    

  })
  
  # Input Choice Auto-Populate ----
  
  
  observeEvent({input$site},{
    
    req(input$site)
    
    skey <- get_skey(x = input$site)
    
    ############################## Filling in the reactive filters ##############################
    
    updatePickerInput(session = getDefaultReactiveDomain(), 
                      inputId = "facility", 
                      choices = pull_choices(skey = skey, 
                                             var = "Facility_Name"),
                      selected = pull_choices(skey = skey, 
                                              var = "Facility_Name"))
    
    updatePickerInput(session = getDefaultReactiveDomain(), 
                      inputId = "feed", 
                      choices = pull_choices(skey = skey, 
                                             var = "Feed_Name"),
                      selected = pull_choices(skey = skey, 
                                              var = "Feed_Name"))
    
    updatePickerInput(session = getDefaultReactiveDomain(), 
                      inputId = "vendor", 
                      choices = pull_choices(skey = skey, 
                                             var = "Vendor_Name"),
                      selected = pull_choices(skey = skey, 
                                              var = "Vendor_Name"))
    
    updatePickerInput(session = getDefaultReactiveDomain(), 
                      inputId = "parent", 
                      choices = pull_choices(skey = skey, 
                                             var = "Parent_Organization"),
                      selected = pull_choices(skey = skey, 
                                              var = "Parent_Organization"))
    
    updatePickerInput(session = getDefaultReactiveDomain(), 
                      inputId = "type", 
                      choices = pull_choices(skey = skey, 
                                             var = "Facility_Type"),
                      selected = pull_choices(skey = skey, 
                                              var = "Facility_Type"))

    
  })
  
  # __Dataflow Load ----
  
  
  observe({
    ############################## Getting data ##############################
    skey <- get_skey(x = input$site)
    
    ops <- operations_summary(skey) %>% filter(Date >= input$date[1] & Date <= input$date[2])
    ess <- essence_summary(skey)    %>% filter(Date >= input$date[1] & Date <= input$date[2])
    pro <- processed_summary(skey)  %>% filter(Date >= input$date[1] & Date <= input$date[2])
    flt <- filters(skey)            %>% filter(Date >= input$date[1] & Date <= input$date[2])
    exc <- exceptions(skey)         %>% filter(Date >= input$date[1] & Date <= input$date[2])
    
    if (length(input$facility) > 0){
      
      ops <- ops %>% filter(Facility_Name %in% input$facility)
      ess <- ess %>% filter(Facility_Name %in% input$facility)
      pro <- pro %>% filter(Facility_Name %in% input$facility)
      
    }
    
    if (length(input$feed) > 0){
      
      ops <- ops %>% filter(Feed_Name %in% input$feed)
      ess <- ess %>% filter(Feed_Name %in% input$feed)
      pro <- pro %>% filter(Feed_Name %in% input$feed)
      flt <- flt %>% filter(Feed_Name %in% input$feed)
      exc <- exc %>% filter(Feed_Name %in% input$feed)
      
    }
    
    if (length(input$vendor) > 0){
      
      ops <- ops %>% filter(Vendor_Name %in% input$vendor)
      ess <- ess %>% filter(Vendor_Name %in% input$vendor)
      pro <- pro %>% filter(Vendor_Name %in% input$vendor)
      
    }
    
    if (length(input$parent) > 0){
      
      ops <- ops %>% filter(Parent_Organization %in% input$parent)
      ess <- ess %>% filter(Parent_Organization %in% input$parent)
      pro <- pro %>% filter(Parent_Organization %in% input$parent)
      
    }
    
    if (length(input$type) > 0){
      
      ops <- ops %>% filter(Facility_Type %in% input$type)
      ess <- ess %>% filter(Facility_Type %in% input$type)
      pro <- pro %>% filter(Facility_Type %in% input$type)
      
    }
    
    loaded_data$operations_summary <- ops
    loaded_data$essence_summary <- ess
    loaded_data$processed_summary <- pro
    loaded_data$filters <- flt
    loaded_data$exceptions <- exc
    
  })
  
  
  
  # __ Validity Load ----
  
  
  observe({
    req({input$feed != "Loading Choices..."})
    req({nrow(loaded_data$filters) > 0})
    
    
    skey <- get_skey(x = input$site)
    
    
    df_all <- validity_summary(skey = skey) %>%
      
      as.data.table() %>%
      
      .[, lapply(.SD, sum, 
                 na.rm = TRUE), 
        by = .(Date, 
               Site_Skey, 
               Site_ID, 
               Facility_Name, 
               Facility_Type, 
               Week_Of_Year, 
               Parent_Organization, 
               C_BioSense_Facility_ID, 
               Feed_Name, 
               Vendor_Name)] %>%
      
      melt.data.table(measure.vars = patterns('^Valid','^InValid','^Missing','^Total'), 
                      variable.name = 'Element', value.name = c('Valid','Invalid','Missing','Total')) %>%
      .[, Element := factor(Element, labels = unique(gsub('.*_Count_','',
                                                          names(validity_summary(skey = skey))[grepl('.*_Count_', 
                                                                                                     names(validity_summary(skey = skey)))])))] %>%
      .[Element %in% keeps_vals] %>%
      .[order(Element, Week_Of_Year)]
    
    
    df_all <- df_all %>% 
      
      filter(Date >= input$date[1],
             Date <= input$date[2])
    
    if (length(input$parent   ) > 0){df_all <- df_all %>% filter(Parent_Organization %in% input$parent)}
    if (length(input$facility ) > 0){df_all <- df_all %>% filter(Facility_Name %in% input$facility)}
    if (length(input$type     ) > 0){df_all <- df_all %>% filter(Facility_Type %in% input$type)}
    if (length(input$vendor   ) > 0){df_all <- df_all %>% filter(Vendor_Name %in% input$vendor)}
    if (length(input$feed     ) > 0){df_all <- df_all %>% filter(Feed_Name %in% input$feed)}
    if (length(input$element  ) > 0){df_all <- df_all %>% filter(Element %in% input$element)}
    
    loaded_data$validity <- df_all
    
    
  })
  
  
  # ____ Validity Plot: Validity Over Time ----
  
  
  observe({
    req({input$feed != "Loading Choices..."})
    req({nrow(loaded_data$filters) > 0})
    
    ############################## CUSUM ##############################
    if (input$method == "CUSUM"){
      
      ts_prep <- loaded_data$validity %>% 
        
        rename(Grouping = input$stratification) %>%                   # Reassign y-axis variable here
        
        group_by(Date) %>% 
        summarize(Validity = sum(Valid,
                                 na.rm = TRUE) / sum(Total, 
                                                     na.rm = TRUE)) %>% 
        ungroup() %>%
        
        arrange(desc(Date))
      
      ts_prep <- as.data.frame(ts_prep)
      
      
      ts_anom <- ts_prep %>% 
        
        detect_alerts(date = "Date", 
                      value = "Validity", 
                      method = "cusum",
                      #group = input$stratification,
                      #facet = input$facet,
                      width = input$width,
                      guardband = input$guardband,
                      impute = input$sumstat,
                      B = input$baseline) 
      
      
      
      n_days <- as.numeric(max(ts_anom$Date, na.rm = TRUE) - min(ts_anom$Date, na.rm = TRUE))
      
      
      anom_index <- ts_anom %>% 
        group_by(Date) %>% 
        summarise(has_alert = as.numeric(log_alert)) %>% 
        ungroup() %>%
        unique() %>% 
        filter(has_alert != 0)
      
      
      p_max <- input$lim_outlier
      
      
      prop_index <- anom_index %>% 
        summarise(n = n(),
                  p = n / n_days) %>% 
        
        filter(p <= p_max)
      
      
      
      ts_plot <- ts_anom
      
      
      loaded_data$vals_ts_cusum <- prop_index
      
      
      output$validity_ts <- renderEcharts4r({
        
        if (length(loaded_data$vals_ts_cusum) == 0){
          
          return()
          
        }
        
        #Creating new variables for Warning and Alert that contain the Validity
        #value. Subsetting on the existing binary warning and alert values doesn't
        #seem to work, so I am going to try this instead.
        
        ts_plot$alert <- ifelse(ts_plot$log_alert == TRUE, ts_plot$Value, NA)
        
        
        
        e_chart(data = ts_plot, Date) %>% 
          e_line(Value) %>% 
          e_effect_scatter(alert,
                           name = "Alert") %>% 
          
          e_y_axis(formatter = e_axis_formatter("percent")) %>% 
          e_title("Data Validity by Visit") 
        
        
      })
      
      dt_vals <- ts_plot %>%
        filter(log_alert == TRUE) %>% 
        select(Date,
               Validity = Value,
               Severity = exceeded)
      
      output$validity_dt <- DT::renderDataTable({
        
        if (length(loaded_data$vals_ts_cusum) == 0){
          
          empty_dt <- data.frame(Undetected = 'No anomalies were detected with this stratification.')
          
          datatable(data = empty_dt, options = list(dom = 't'))
          
        } else {
          
          poc_dt(dt_vals)
          
        }
        
      })
      
    }
    
    ############################## EWMA ##############################
    if (input$method == "EWMA"){
      
      ewma_prep <- data.frame(loaded_data$validity) %>% 
        
        rename(Grouping = input$stratification) %>%  
        
        rename(Facet = input$facet) %>% 
        
        #group_by(Date, Grouping, Facet) %>% 
        
        group_by(Date) %>% 
        
        summarize(Validity = sum(Valid,
                                 na.rm = TRUE) / sum(Total, 
                                                     na.rm = TRUE)) %>% 
        
        ungroup() %>% 
        mutate(Date = as.Date(Date))
      
      
      
      
      
      
      
      
      vals_sum <- detect_alerts(df = data.frame(ewma_prep), date = "Date", 
                      value = "Validity", 
                      method = "ewma",
                      #group = "Grouping",
                      #facet = "Facet", 
                      ewma_lim = 0.01,
                      width = input$width,
                      guardband = input$guardband,
                      impute = input$sumstat,
                      B = input$baseline)
      
      
      if (length(vals_sum$Validity) >= 14){
        
        rolling <- seismicRoll::roll_median(x = vals_sum$Validity,
                                            n = 14)
        
        rolling_SD <- seismicRoll::roll_sd(vals_sum$Validity,
                                           n = 14,
                                           align = "right")
        
        vals_sum <- cbind(vals_sum, rolling)
        
        vals_sum <- cbind(vals_sum, rolling_SD)
        
      } else {
        
        reset(id = "baseline")
        reset(id = "guardband")
        reset(id = "smoothing")
        reset(id = "date")
        
        sendSweetAlert(title = "Subset Error",
                       session = getDefaultReactiveDomain(),
                       type = "error",
                       text = paste("Rolling median and baseline range cannot",
                                    "exceed selected date range. Restoring defaults."))
        
      }
      
      
      dt_vals <- vals_sum %>% 
        essence_datatable() %>%
        filter(essence_alert != "blue") %>% 
        select(Date,
               
               Validity,
               Type = Anomaly_Type,
               Description = Anomaly_Description)
      
      #Creating new variables for plot
      vals_sum$red <- ifelse(vals_sum$essence_alert == "red", vals_sum$Validity, NA)
      
      loaded_data$vals_ts_ewma <- sum(vals_sum$red)
      

      
      output$validity_ts <- renderEcharts4r({
        
        suppressWarnings(
          vals_sum %>% 
            e_chart(Date) %>% 
            e_line(Validity) %>% 
            e_y_axis(formatter = e_axis_formatter("percent")) %>% 
            e_title("Data Validity by Visit") %>% 
            e_effect_scatter(red, name = "Alert") 
          
          
          
        )
        
      })
      
      
      
      
      output$validity_dt <- DT::renderDataTable({
        
        if (length(loaded_data$vals_ts_ewma) == 0){
          
          empty_dt <- data.frame(Undetected = 'No anomalies were detected with this stratification.')
          
          datatable(data = empty_dt, options = list(dom = 't'))
          
        } else {
          poc_dt(dt_vals)
        }
        
      })
      
    }
    
    ############################## No anomaly detection ##############################
    if (input$method == "None"){
      
      ts_prep <- loaded_data$validity %>% 
        
        rename(Grouping = input$stratification) %>%                   # Reassign y-axis variable here
        
        group_by(Date) %>% 
        summarize(Validity = sum(Valid,
                                 na.rm = TRUE) / sum(Total, 
                                                     na.rm = TRUE)) %>% 
        ungroup() %>%
        
        arrange(desc(Date) 
        )
      
      output$validity_ts <- renderEcharts4r({
        
        suppressWarnings(
          ts_prep %>% 
            e_chart(Date) %>% 
            e_line(Validity) %>% 
            e_y_axis(formatter = e_axis_formatter("percent")) %>% 
            e_title("Data Validity by Visit") 
        )
        
        
      })
      
      output$validity_dt <- DT::renderDataTable({
        
        empty_dt <- data.frame(Disabled = 'Select a detection method in the "Detection Options" tab.')
        
        datatable(data = empty_dt, options = list(dom = 't'))
        
        
        
      })
      
      
    }
    
    ############################## Trend ##############################
    if (input$method == "Trend"){
      
      trend_prep <- data.frame(loaded_data$validity) %>% 
        
        rename(Grouping = input$stratification) %>%  
        
        rename(Facet = input$facet) %>% 
        
        #group_by(Date, Grouping, Facet) %>% 
        
        group_by(Date) %>% 
        
        summarize(Validity = sum(Valid,
                                 na.rm = TRUE) / sum(Total, 
                                                     na.rm = TRUE)) %>% 
        
        ungroup() %>% 
        mutate(Date = as.Date(Date))
      
      #view(trend_prep)
      
      trend_detection <- detect_alerts(df = data.frame(ewma_prep), date = "Date", 
                                       value = "Validity", 
                                       method = "trend")
      
      #view(trend_detection)
      df <- merge(trend_detection, trend_prep)
      
      #view(df)
      df$Stable <- ifelse(df$Trajectory == "Stable", df$Validity, NA)
      
      df$Increasing <- ifelse(df$Trajectory == "Increasing", df$Validity, NA)
      
      df$Decreasing <- ifelse(df$Trajectory == "Decreasing", df$Validity, NA)
      
      trend_dt_inc <- subset(df, (!is.na(Increasing)))
      
      trend_dt_dec <- subset(df, (!is.na(Decreasing)))
      
      trend_dt <- rbind(trend_dt_inc, trend_dt_dec)
     
      trend_dt <- trend_dt[, c("Date", "Validity", "Trajectory")]
        
      output$validity_ts <- renderEcharts4r({
        
        suppressWarnings(
          e_chart(df, Date) %>% 
        e_line(Validity, color = "blue") %>% 
        e_line(Decreasing, color = "red") %>% 
        e_line(Increasing, color = "green")
        )
      })
      
      output$validity_dt <- DT::renderDataTable({
        
        if (nrow(trend_dt) == 0){
          
          empty_dt <- data.frame(Undetected = 'No anomalies were detected with this stratification.')
          
          datatable(data = empty_dt, options = list(dom = 't'))
          
        } else {
          poc_dt(trend_dt)
        }
        
      })
    }
    

    
  })
  
  #Registering logout
  #log_logout(user_connection)
}



# 7. App ----

shinyApp(ui = ui, 
         server = server)
