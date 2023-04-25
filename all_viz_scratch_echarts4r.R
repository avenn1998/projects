
# i. SETUP ----


    #' R version 4.1.2 (2021-11-01)
    #' RStudio version 2021.9.1.372.1
    #' Windows 10


library(DBI)
library(odbc)
library(plotly)
library(scales)
library(lubridate)
library(tidyverse)
library(data.table)
library(echarts4r)

source("query_functions.R")

skey <- c(4)                 # Supports national view (2+ skeys)

start <- Sys.Date() - 122
end   <- Sys.Date()

keeps_comp <- c("Facility_Type_Code",
                "Sending_Facility_ID",
                "Sending_Facility_ID_Source",
                "Treating_Facility_ID",
                "Visit_ID",
                "C_FacType_Patient_Class",
                "Patient_Class_Code",
                "Admit_Date_Time",
                "Discharge_Disposition",
                "Discharge_Date_Time",
                "First_Patient_ID",
                "Medical_Record_Number",
                "Recorded_Date_Time",
                "Admit_Reason_Code",
                "Admit_Reason_Description",
                "C_Chief_Complaint",
                "Chief_Complaint_Code",
                "Chief_Complaint_Text",
                "Diagnosis_Code",
                "Diagnosis_Description",
                "Diagnosis_Type",
                "C_Patient_Age",
                "Administrative_Sex",
                "Age_Reported",
                "Age_Units_Reported",
                "C_Patient_Age_Units",
                "C_Patient_Age_Years",
                "C_Patient_County",
                "Ethnicity_Code",
                "Ethnicity_Description",
                "Patient_City",
                "Patient_Country",
                "Patient_State",
                "Patient_Zip",
                "Race_Code",
                "Race_Description",
                "Trigger_Event",
                "Processing_ID",
                "Version_ID",
                "Procedure_Combo",
                "Procedure_Code",
                "Travel_History",
                "Triage_Notes",
                "Height",
                "Weight",
                "Smoking_Status_Code",
                "Insurance_Coverage",
                "Combined_CCDD")            # Selected completeness elements

keeps_vals <- c("C_FacType_Patient_Class",
                "Patient_Class_Code",
                "Discharge_Disposition",
                "C_Chief_Complaint",
                "Diagnosis_Code",
                "Diagnosis_Type",
                "C_Patient_Age",
                "Administrative_Sex",
                "Age_Reported",
                "Age_Units_Reported",
                "C_Patient_Age_Units",
                "C_Patient_Age_Years",
                "Ethnicity_Code",
                "Patient_Zip",
                "Race_Code",
                "Trigger_Event",
                "Height", 
                "Weight", 
                "Insurance_Coverage", 
                "Smoking_Status_Code", 
                "Admit_Date_Time")            # Selected validity elements



# I. HOME ----

    # __ A. Raw Count Overview ----

home_plot <- home_table() %>% 
  filter(Category == "Fact_Processed") %>% 
  group_by(Date) %>% 
  summarize(Raw_Count = sum(Total_Records, na.rm = TRUE)) %>% 
  ungroup()

plot_ly(data = home_plot,
        x = ~Date,
        y = ~Raw_Count,
        type = "bar",
        name = "Last 120 Days",
        text = ~comma(Raw_Count, accuracy = 1),
        hovertemplate = paste("<b>Date:</b> %{x}<br>",
                              "<b>Count:</b> %{text}")) %>%
  layout(title = "Raw Message Count Overview",
         xaxis = list(rangeslider = list(type = "date"),
                      title = "PLACEHOLDER"),
         
         yaxis = list(title = "Messages"))

home_plot %>% 
  e_charts(Date) %>% 
  e_bar(Raw_Count) %>% 
  e_title("Raw Message Count Overview") %>% 
  e_y_axis()

# II. DATA FLOW ----



    # __ A. Data Flow Overview ----
    

date_seq <- data.frame(Date = seq.Date(from = start, 
                                       to = end, 
                                       by = "day"))

df_all <- date_seq %>%
  
  left_join(operations_summary(skey = skey) %>%
              group_by(Date) %>%
              summarize(Processed = sum(Processed_Message_Count, na.rm = TRUE)) %>%
              ungroup(),
            by = "Date") %>%
  
  left_join(filters(skey = skey) %>%
              group_by(Date) %>% 
              summarize(Filtered = sum(Filtered_Reason_Count, na.rm = TRUE)) %>%
              ungroup(),
            by = "Date") %>%
  
  left_join(exceptions(skey = skey) %>%
              group_by(Date) %>%
              summarize(Exceptions = sum(Exceptions_Count, na.rm = TRUE)) %>%
              ungroup(),
            by = "Date") %>%
  
  mutate(Processed  = ifelse(is.na(Processed ), 0, Processed ),
         Exceptions = ifelse(is.na(Exceptions), 0, Exceptions),
         Filtered   = ifelse(is.na(Filtered  ), 0, Filtered  )) %>%
  
  rename(Arrived_Date = Date)

plot_ly(data = df_all, 
        x = ~Arrived_Date, 
        y = ~Processed, 
        type = "bar", 
        name = "Processed",
        text = ~comma(Processed, accuracy = 1),
        hovertemplate = paste("<b> Date:</b> %{x}<br>",
                              "<b>Count:</b> %{y:,.0f}")) %>%
  
  add_trace(y = ~Filtered, 
            name = "Filtered", 
            text = ~comma(Filtered, accuracy = 1)) %>%
  
  add_trace(y = ~Exceptions, 
            name = "Exceptions", 
            text = ~comma(Exceptions, accuracy = 1)) %>%
  
  layout(yaxis = list(title = "Message Count"),
         colorway = c("DarkGreen", "Gold", "Firebrick"),
         barmode = "stack",
         showlegend = T,
         title = "Summary of Processed, Filtered, and Exceptioned Messages",
         xaxis = list(type = "date",
                      tickformat = "%d %b. %Y"),
         hoverlabel = "left")

df_all %>% 
  e_charts(Arrived_Date) %>% 
  e_bar(Processed, stack = "stack") %>% 
  e_bar(Filtered, stack = "stack") %>% 
  e_bar(Exceptions, stack = "stack")

    # __ B. Visit Comparison ----
    

operations_summary(skey = skey) %>% 
  left_join(site_info())

df_all <- date_seq %>%
  
  left_join(essence_summary(skey = skey) %>%
              group_by(Date) %>%
              summarize(Essence = sum(Essence_Visit_Count, na.rm = TRUE)) %>%
              ungroup(),
            by = "Date") %>%
  
  left_join(processed_summary(skey = skey) %>%
              group_by(Date) %>% 
              summarize(Processed = sum(Visit_Count, na.rm = TRUE)) %>% 
              ungroup(),
            by = "Date") %>%
  
  left_join(operations_summary(skey = skey) %>%
              group_by(Date) %>% 
              summarize(Messages = sum(Processed_Message_Count, na.rm = TRUE)) %>%
              ungroup(),
            by = "Date") %>%
  
  mutate(Essence   = ifelse(is.na(Essence),   0, Essence),
         Processed = ifelse(is.na(Processed), 0, Processed),
         Messages  = ifelse(is.na(Messages),  0, Messages)) %>%
  
  arrange(desc(Date))

plot_ly(data = df_all, 
        x = ~Date, 
        y = ~Messages, 
        type = "bar",
        name = "Processed<br>Messages", 
        text = ~comma(Messages, accuracy = 1),
        hovertemplate = paste("<b> Date:</b> %{x}<br>",
                              "<b>Count:</b> %{y:,.0f}")) %>%
  
  add_trace(y = ~Essence, 
            name = "ESSENCE<br>Visits", 
            type = "scatter",
            marker = list(size = 7,
                          color = "orange",
                          line = list(width = 5)),
            text = ~comma(Essence, accuracy = 1), 
            mode = "lines+markers") %>%
  
  add_trace(y = ~Processed, 
            name = "Processed<br>Visits", 
            type = "scatter",
            marker = list(size = 7,
                          color = "green",
                          line = list(
                            width = 5)),
            text = ~comma(Processed, accuracy = 1), 
            mode = "lines+markers") %>%
  
  layout(yaxis = list(title = "Count (Message & Visit)"), 
         barmode = "stack",
         title = "Processed Message Count",
         xaxis = list(type = "date",
                      tickformat = "%d %b. %Y"),
         hoverlabel = "left")

df_all %>% 
  e_charts(Date) %>% 
  e_bar(Messages) %>% 
  e_line(Essence) %>% 
  e_line(Processed)

    # __ C. Filter Reasons ----


filters(skey = skey, 
        from_rpt = TRUE) %>% 
  
  select(Date, 
         Description, 
         Filtered_Reason_Count) %>%
  
  mutate(Description = strtrim(x = Description, 
                               width = 10),
         Description = paste0(Description, "...")) %>% 
  
  plot_ly(x = ~Date, 
        y = ~Filtered_Reason_Count, 
        type = "bar",
        name = ~Description,
        color = ~Description, 
        text = ~comma(Filtered_Reason_Count, accuracy = 1),
        hovertemplate = paste("<b> Date:</b> %{x}<br>",
                              "<b>Filtered_Reason_Count:</b> %{y:,.0f}"),
        hoverlabel = list(align = "left")) %>%
  
  layout(title = "Filter Reasons Over Time",
         barmode = "stack",
         showlegend = TRUE,
         xaxis = list(title = "",
                      type = "date",
                      tickformat = "%d %b. %Y"),
         hoverlabel = "left")
  
filters(skey = skey, 
        from_rpt = TRUE) %>% 
  
  select(Date, 
         Description, 
         Filtered_Reason_Count) %>%
  
  mutate(Description = strtrim(x = Description, 
                               width = 10),
         Description = paste0(Description, "...")) %>% 
  
group_by(Description) %>% 
  e_charts(Date) %>% 
  e_bar(Filtered_Reason_Count)


    # __ D. Exception Reasons ----


exceptions(skey = skey, 
           from_rpt = TRUE) %>% 
  
  select(Date, 
         Exception_Description, 
         Exceptions_Count) %>% 
  
  mutate(Exception_Description = strtrim(x = Exception_Description, 
                                         width = 10),
         Exception_Description = paste0(Exception_Description, "...")) %>% 
  
  plot_ly(x = ~Date, 
          y = ~Exceptions_Count, 
          type = "bar",
          color = ~Exception_Description, 
          text = ~comma(Exceptions_Count, accuracy = 1),
          hovertemplate = paste("<b> Date:</b> %{x}<br>",
                                "<b>Exceptions_Count:</b> %{y:,.0f}"),
          hoverlabel = list(align = "left"))  %>%
  
  layout(title = "Exception Reasons Over Time",
         showlegend = TRUE,
         barmode = "stack",
         xaxis = list(
           title = "",
           type = "date",
           tickformat = "%d %b. %Y"))


exceptions(skey = skey, 
           from_rpt = TRUE) %>% 
  
  select(Date, 
         Exception_Description, 
         Exceptions_Count) %>% 
  
  mutate(Exception_Description = strtrim(x = Exception_Description, 
                                         width = 10),
         Exception_Description = paste0(Exception_Description, "...")) %>% 
  group_by(Exception_Description) %>% 
e_charts(Date) %>% 
  e_bar(Exceptions_Count)

# III. TIMELINESS ---- 



    # __ A. Overall Timeliness ----
    

timeliness_summary(skey = skey) %>%
  
  rename(timeliness = Average_Timeliness_in_Hours) %>%
  mutate(category = as.character(NA),
         category = ifelse(timeliness <= 24, "Early", category),
         category = ifelse(timeliness > 24 & timeliness <= 48, "Normal", category),
         category = ifelse(timeliness > 48, "Late", category),
         category = as.character(category)) %>%
  bind_rows(data.frame(Facility_Name = rep(x = "Placeholder", times = 3),
                       timeliness = c(20, 40, 60),
                       category = c("Early", "Normal", "Late"))) %>%
  group_by(Facility_Name, category) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  spread(key = category,
         value = n) %>%
  filter(Facility_Name != "Placeholder") %>%
  left_join(timeliness_summary(skey = skey) %>%
              group_by(Facility_Name) %>%           # Change to different grouping var here
              summarize(Total = n()) %>%
              ungroup(), 
            by = "Facility_Name") %>%
  rename(Entity = Facility_Name) %>% 
  mutate(Early = Early / Total,
         Normal = Normal / Total,
         Late = Late / Total) %>% 
  
  plot_ly(x = ~Late,
          y = ~reorder(Entity, Late),
          type = "bar",
          orientation = "h",
          name = "Greater Than 48",
          text = ~percent(Late, accuracy = 0.01),
          marker = list(color = "firebrick",
                        line = list(color = "rgb(248, 248, 249)")),
          hovertemplate = paste("<b>Proportion:</b> %{x}")) %>%
  
  add_trace(x = ~Normal,
            name = "Between 24 and 48",
            text = ~percent(Normal, accuracy = 0.01),
            marker = list(color = "goldenrod")) %>%
  
  add_trace(x = ~Early,
            name = "Less Than 24",
            text = ~percent(Early, accuracy = 0.01),
            marker = list(color = "green")) %>%
  
  layout(xaxis = list(title = "Percent of Visits Received within Timeframe",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      zeroline = FALSE,
                      domain = c(0.15, 1),
                      tickvals = seq(0, 1, 0.2),
                      tickformat = ".1%"),
         yaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      zeroline = FALSE),
         barmode = "stack",
         title = "Overall Timeliness")

timeliness_summary(skey = skey) %>%
  
  rename(timeliness = Average_Timeliness_in_Hours) %>%
  mutate(category = as.character(NA),
         category = ifelse(timeliness <= 24, "Early", category),
         category = ifelse(timeliness > 24 & timeliness <= 48, "Normal", category),
         category = ifelse(timeliness > 48, "Late", category),
         category = as.character(category)) %>%
  bind_rows(data.frame(Facility_Name = rep(x = "Placeholder", times = 3),
                       timeliness = c(20, 40, 60),
                       category = c("Early", "Normal", "Late"))) %>%
  group_by(Facility_Name, category) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  spread(key = category,
         value = n) %>%
  filter(Facility_Name != "Placeholder") %>%
  left_join(timeliness_summary(skey = skey) %>%
              group_by(Facility_Name) %>%           # Change to different grouping var here
              summarize(Total = n()) %>%
              ungroup(), 
            by = "Facility_Name") %>%
  rename(Entity = Facility_Name) %>% 
  mutate(Early = Early / Total,
         Normal = Normal / Total,
         Late = Late / Total) %>% 
e_charts(Entity) %>% 
  e_bar(Early, stack = "stack") %>% 
  e_bar(Normal, stack = "stack") %>% 
  e_bar(Late, stack = "stack") %>% 
  e_title("Percent of Visits Received within Timeframe") %>% 
  e_flip_coords()
    

# __ B. Timeliness Over Time ----


timeliness_summary(skey = skey) %>% 
  
  rename(Entity = Facility_Name) %>%          # Specify grouping variable here
  
  mutate(C_Visit_Date = floor_date(x = Date, 
                                   unit = "week", 
                                   week_start = 1)) %>%
  group_by(C_Visit_Date, Entity) %>%
  summarize(Average_Timeliness = mean(Average_Timeliness_in_Hours, 
                                      na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(C_Visit_Date, Entity) %>% 
  plot_ly(x = ~C_Visit_Date,
          y = ~Average_Timeliness,
          type = "scatter",
          mode = "lines+markers",
          name = ~Entity,
          text = ~round(Average_Timeliness, digits = 0.01),
          textposition = "auto",
          color = ~Entity,
          hovertemplate = paste("<b> Date:</b> %{x}<br>",
                                "<b>Timeliness:</b> %{y:,.0f} hrs")) %>%
  
  layout(title = "Time from Visit to First Message Received",
         xaxis = list(title = "C_Visit_Date (MMWR Max Date in Week)",
                      rangemode = "tozero",
                      type = "date",
                      tickformat = "%d %b. %Y"),
         yaxis = list(title = "Average Hours Delayed"))


timeliness_summary(skey = skey) %>% 
  
  rename(Entity = Facility_Name) %>%          # Specify grouping variable here
  
  mutate(C_Visit_Date = floor_date(x = Date, 
                                   unit = "week", 
                                   week_start = 1)) %>%
  group_by(C_Visit_Date, Entity) %>%
  summarize(Average_Timeliness = mean(Average_Timeliness_in_Hours, 
                                      na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(C_Visit_Date, Entity) %>% 
  group_by(Entity) %>% 
e_charts(C_Visit_Date) %>% 
    e_line(Average_Timeliness) %>% 
  e_legend(type = "scroll") %>% 
  e_title("Time from Visit to First Message Received")

# IV. COMPLETENESS ----



    # __ A. Overall Completeness ----



completeness_summary(skey = skey) %>%
  as.data.table() %>%
  .[ , lapply(.SD, sum, na.rm = TRUE), 
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
  melt.data.table(measure.vars = patterns('^Completeness',
                                          '^InCompleteness',
                                          '^Total'), 
                  variable.name = 'Element', 
                  value.name = c('Complete',
                                 'Incomplete',
                                 'Total')) %>%
  .[, Element := factor(Element, 
                        labels = unique(gsub('.*_Count_','', 
                                             names(completeness_summary(skey = skey))[grepl('.*_Count_', 
                                             names(completeness_summary(skey = skey)))])))] %>%
  .[Element %in% keeps_comp] %>%
  .[order(Element, Week_Of_Year)] %>% 
  
  rename(Grouping = Facility_Name) %>%                   # Reassign y-axis variable here
  
  group_by(Grouping) %>% 
  summarize(Completeness = sum(Complete, 
                               na.rm = TRUE) / sum(Total, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(-Completeness) %>% 
  mutate(Completeness_Perc = percent(x = Completeness, accuracy = 0.01)) %>% 
  
  plot_ly(x = ~Completeness, 
          y = ~reorder(Grouping, Completeness), 
          name = "Site-Wide", 
          marker = list(color = toRGB("dodgerblue", alpha = 0.75)),
          text = ~Completeness_Perc,
          hovertemplate = paste("<b>%{y}</b><br>", 
                                "<b>Completeness:</b> %{x:.0%}<br>")) %>%
  layout(title = "Data Completeness by Visit",
         xaxis = list(title = "Percent Complete (by Visit)",
                      autotick = FALSE,
                      tickvals = seq(0, 1, 0.2),
                      tickmode = "array",
                      tickformat = "0.1%"))

    
completeness_summary(skey = skey) %>%
  as.data.table() %>%
  .[ , lapply(.SD, sum, na.rm = TRUE), 
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
  melt.data.table(measure.vars = patterns('^Completeness',
                                          '^InCompleteness',
                                          '^Total'), 
                  variable.name = 'Element', 
                  value.name = c('Complete',
                                 'Incomplete',
                                 'Total')) %>%
  .[, Element := factor(Element, 
                        labels = unique(gsub('.*_Count_','', 
                                             names(completeness_summary(skey = skey))[grepl('.*_Count_', 
                                                                                            names(completeness_summary(skey = skey)))])))] %>%
  .[Element %in% keeps_comp] %>%
  .[order(Element, Week_Of_Year)] %>% 
  
  rename(Grouping = Facility_Name) %>%                   # Reassign y-axis variable here
  
  group_by(Grouping) %>% 
  summarize(Completeness = sum(Complete, 
                               na.rm = TRUE) / sum(Total, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(-Completeness) %>% 
  mutate(Completeness_Perc = percent(x = Completeness, accuracy = 0.01)) %>% 
  
e_charts(Grouping) %>% 
  e_bar(Completeness) %>% 
  e_title("Data Completeness by Visit") %>% 
  e_flip_coords()
    
# __ B. Completeness Over Time ----



completeness_summary(skey = skey) %>%
  as.data.table() %>%
  .[ , lapply(.SD, sum, na.rm = TRUE), 
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
  melt.data.table(measure.vars = patterns('^Completeness',
                                          '^InCompleteness',
                                          '^Total'), 
                  variable.name = 'Element', 
                  value.name = c('Complete',
                                 'Incomplete',
                                 'Total')) %>%
  .[, Element := factor(Element, 
                        labels = unique(gsub('.*_Count_','', 
                                             names(completeness_summary(skey = skey))[grepl('.*_Count_', 
                                             names(completeness_summary(skey = skey)))])))] %>%
  .[Element %in% keeps_comp] %>%
  .[order(Element, Week_Of_Year)] %>% 
  
  rename(Grouping = Feed_Name) %>%                   # Reassign y-axis variable here
  
  mutate(Week_Start = floor_date(x = Date, 
                                 unit = "week", 
                                 week_start = 7)) %>% 
  group_by(Week_Start, 
           Grouping) %>% 
  summarize(Completeness = sum(Complete,
                               na.rm = TRUE) / sum(Total, 
                                                   na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Completeness_Perc = percent(x = Completeness, accuracy = 0.01)) %>% 
  
  plot_ly(x = ~Week_Start,
          y = ~Completeness,
          text = ~Completeness_Perc,
          color = ~Grouping,
          colors = "Paired", 
          type = "scatter",
          mode = "lines+markers",
          name = ~Grouping,
          hovertemplate = paste("<b> Date:</b> %{x}<br>",
                                "<b>Completeness:</b> %{y:.0%}")) %>%
  layout(title = paste("Data Completeness Over Time"),
         yaxis = list(title = "Percent Complete (%)",
                      rangemode = "tozero",
                      tickformat = ".1%"),
         xaxis = list(title = "Arrived_Date (MMWR Week)",
                      type = "date",
                      tickformat = "%d %b. %Y"))

completeness_summary(skey = skey) %>%
  as.data.table() %>%
  .[ , lapply(.SD, sum, na.rm = TRUE), 
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
  melt.data.table(measure.vars = patterns('^Completeness',
                                          '^InCompleteness',
                                          '^Total'), 
                  variable.name = 'Element', 
                  value.name = c('Complete',
                                 'Incomplete',
                                 'Total')) %>%
  .[, Element := factor(Element, 
                        labels = unique(gsub('.*_Count_','', 
                                             names(completeness_summary(skey = skey))[grepl('.*_Count_', 
                                                                                            names(completeness_summary(skey = skey)))])))] %>%
  .[Element %in% keeps_comp] %>%
  .[order(Element, Week_Of_Year)] %>% 
  
  rename(Grouping = Feed_Name) %>%                   # Reassign y-axis variable here
  
  mutate(Week_Start = floor_date(x = Date, 
                                 unit = "week", 
                                 week_start = 7)) %>% 
  group_by(Week_Start, 
           Grouping) %>% 
  summarize(Completeness = sum(Complete,
                               na.rm = TRUE) / sum(Total, 
                                                   na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Completeness_Perc = percent(x = Completeness, accuracy = 0.01)) %>% 
  group_by(Grouping) %>% 
e_chart(Week_Start) %>% 
  e_line(Completeness) %>% 
  e_title("Data Completeness Over Time") %>% 
  e_y_axis(formatter = e_axis_formatter("percent")) %>% 
  e_legend(type = "scroll")

# V. VALIDITY ----



    # __ A. Overall Validity ----
    

validity_summary(skey = skey) %>%
  
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
  .[order(Element, Week_Of_Year)] %>% 
  
  rename(Grouping = Vendor_Name) %>%                   # Reassign y-axis variable here
  
  group_by(Grouping) %>% 
  summarize(Validity = sum(Valid, na.rm = TRUE) / sum(Total, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(-Validity) %>% 
  mutate(Validity_Perc = percent(x = Validity, accuracy = 0.01)) %>% 
  
  plot_ly(x = ~Validity, 
          y = ~reorder(Grouping, Validity), 
          name = "Site-Wide", 
          marker = list(color = toRGB("dodgerblue", alpha = 0.75)),
          text = ~Validity_Perc,
          hovertemplate = paste("<b>%{y}</b><br>",
                                "<b>Validity:</b> %{x:.0%}<br>")) %>%
  layout(title = "Data Validity by Visit",
         yaxis = list(title = "Grouping Variable"),
         xaxis = list(title = "Percent Valid (by Visit)",
                      autotick = FALSE,
                      tickvals = seq(0, 1, 0.2),
                      tickmode = "array",
                      tickformat = ".1%"))

validity_summary(skey = skey) %>%
  
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
  .[order(Element, Week_Of_Year)] %>% 
  
  rename(Grouping = Vendor_Name) %>%                   # Reassign y-axis variable here
  
  group_by(Grouping) %>% 
  summarize(Validity = sum(Valid, na.rm = TRUE) / sum(Total, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(-Validity) %>% 
  mutate(Validity_Perc = percent(x = Validity, accuracy = 0.01)) %>% 
  
e_chart(Grouping) %>% 
  e_bar(Validity) %>% 
  e_y_axis(formatter = e_axis_formatter("percent")) %>% 
  e_flip_coords() %>% 
  e_title("Data Validity by Visit") 
  

    # __ B. Overall Invalidity ----
    

validity_summary(skey = skey) %>%
  
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
  .[order(Element, Week_Of_Year)] %>% 
  
  rename(Grouping = Vendor_Name) %>%                   # Reassign y-axis variable here
  
  group_by(Grouping) %>% 
  summarize(Invalidity = sum(Invalid, na.rm = TRUE) / sum(Total, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(-Invalidity) %>% 
  mutate(Validity_Perc = percent(x = Invalidity, accuracy = 0.01)) %>% 
  
  plot_ly(x = ~Invalidity, 
          y = ~reorder(Grouping, Invalidity), 
          name = "Site-Wide", 
          marker = list(color = toRGB("dodgerblue", alpha = 0.75)),
          text = ~Validity_Perc,
          hovertemplate = paste("<b>%{y}</b><br>",
                                "<b>Invalidity:</b> %{x:.0%}<br>")) %>%
  layout(title = "Data Invalidity by Visit",
         yaxis = list(title = "Grouping Variable"),
         xaxis = list(title = "Percent Valid (by Visit)",
                      autotick = FALSE,
                      tickvals = seq(0, 1, 0.2),
                      tickmode = "array",
                      tickformat = ".1%"))


validity_summary(skey = skey) %>%
  
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
  .[order(Element, Week_Of_Year)] %>% 
  
  rename(Grouping = Vendor_Name) %>%                   # Reassign y-axis variable here
  
  group_by(Grouping) %>% 
  summarize(Invalidity = sum(Invalid, na.rm = TRUE) / sum(Total, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(-Invalidity) %>% 
  mutate(Validity_Perc = percent(x = Invalidity, accuracy = 0.01)) %>% 
  
e_chart(Grouping) %>% 
  e_bar(Invalidity) %>% 
  e_y_axis(formatter = e_axis_formatter("percent")) %>% 
  e_flip_coords() %>% 
  e_title("Data Validity by Visit") 



    # __ C. Valid, Invalid, Missing ----
    

validity_summary(skey = skey) %>%
  
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
  .[order(Element, Week_Of_Year)] %>% 
  
  rename(Grouping = Vendor_Name) %>%                   # Reassign y-axis variable here
  
  group_by(Grouping) %>% 
  summarize(Validity   = sum(Valid,   na.rm = TRUE) / sum(Total, na.rm = TRUE),
            Invalidity = sum(Invalid, na.rm = TRUE) / sum(Total, na.rm = TRUE),
            Missing    = sum(Missing, na.rm = TRUE) / sum(Total, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(-Validity) %>% 
  mutate(Validity_Perc   = percent(x = Validity,   accuracy = 1),
         Invalidity_Perc = percent(x = Invalidity, accuracy = 1),
         Missing_Perc    = percent(x = Missing,    accuracy = 1)) %>% 
  
  plot_ly(x = ~Validity,
          y = ~reorder(Grouping, Validity),
          type = "bar",
          orientation = "h",
          name = "Valid",
          text = ~Validity_Perc,
          marker = list(color = "#00BA38"),
          hovertemplate = paste("<b>%{y}</b><br>",
                                "<b>Proportion:</b> %{x:.0%}")) %>%
  
  add_trace(x = ~Invalidity,
            name = "Invalid",
            text = ~Invalidity_Perc,
            marker = list(color = "#F8766D"),
            hovertemplate = paste(paste0("<b>%{y}</b><br>"),
                                  "<b>Proportion:</b> %{x:.0%}")) %>%
  
  add_trace(x = ~Missing,
            name = "Missing",
            text = ~Missing_Perc,
            marker = list(color = "#D3D3D3"),
            hovertemplate = paste(paste0("<b>%{y}</b><br>"),
                                  "<b>Proportion:</b> %{x:.0%}")) %>%
  
  layout(xaxis = list(title = paste("Valid/Invalid/Missing Data (%)"),
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      zeroline = FALSE,
                      domain = c(0.15, 1),
                      tickvals = seq(0, 1, 0.2),
                      tickmode = "array",
                      tickformat = ".1%"),
         yaxis = list(title = "Grouping Variable",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      zeroline = FALSE),
         barmode = "stack",
         title = paste("Proportion of Valid, Invalid, & Missing Data"))

validity.df <- validity_summary(skey = skey) %>%
  
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
  .[order(Element, Week_Of_Year)] %>% 
  
  rename(Grouping = Vendor_Name) %>%                   # Reassign y-axis variable here
  
  group_by(Grouping) %>% 
  summarize(Validity   = sum(Valid,   na.rm = TRUE) / sum(Total, na.rm = TRUE),
            Invalidity = sum(Invalid, na.rm = TRUE) / sum(Total, na.rm = TRUE),
            Missing    = sum(Missing, na.rm = TRUE) / sum(Total, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(-Validity) %>% 
  mutate(Validity_Perc   = percent(x = Validity,   accuracy = 1),
         Invalidity_Perc = percent(x = Invalidity, accuracy = 1),
         Missing_Perc    = percent(x = Missing,    accuracy = 1)) 

validity.plot <- validity.df %>% 
  e_chart(Grouping) %>% 
  e_bar(Validity, stack = "stack") %>% 
  e_bar(Invalidity, stack = "stack") %>% 
  e_bar(Missing, stack = "stack") %>% 
  e_title("Proportion of Valid, Invalid, & Missing Data") %>% 
  e_y_axis(formatter = e_axis_formatter("percent")) %>% 
  e_flip_coords()

    # __ D. Validity Over Time ----


validity_summary(skey = skey) %>%
  
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
  .[order(Element, Week_Of_Year)] %>% 
  
  rename(Grouping = Parent_Organization) %>%                   # Reassign y-axis variable here
  
  mutate(Week_Start = floor_date(x = Date, 
                                 unit = "week", 
                                 week_start = 7)) %>%
  
  group_by(Week_Start, Grouping) %>% 
  
    summarize(Validity = sum(Valid,
                             na.rm = TRUE) / sum(Total, 
                                                 na.rm = TRUE)) %>% 
  
  ungroup() %>% 
  
  mutate(Validity_Perc = percent(x = Validity, 
                                 accuracy = 0.01)) %>% 
  
  plot_ly(x = ~Week_Start,
          y = ~Validity,
          text = ~Validity_Perc,
          color = ~Grouping,
          colors = "Paired", 
          type = "scatter",
          mode = "lines+markers",
          name = ~Grouping,
          hovertemplate = paste("<b> Date:</b> %{x}<br>",
                                "<b>Validity:</b> %{y:.0%}")) %>%
  layout(title = paste("Data Validity Over Time"),
         yaxis = list(title = "Percent Valid (%)",
                      rangemode = "tozero",
                      tickformat = "%"),
         xaxis = list(title = "Arrived_Date (MMWR Week)",
                      type = "date",
                      tickformat = "%d %b. %Y"))


validity_summary(skey = skey) %>%
  
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
  .[order(Element, Week_Of_Year)] %>% 
  
  rename(Grouping = Parent_Organization) %>%                   # Reassign y-axis variable here
  
  mutate(Week_Start = floor_date(x = Date, 
                                 unit = "week", 
                                 week_start = 7)) %>%
  
  group_by(Week_Start, Grouping) %>% 
  
  summarize(Validity = sum(Valid,
                           na.rm = TRUE) / sum(Total, 
                                               na.rm = TRUE)) %>% 
  
  ungroup() %>% 
  
  mutate(Validity_Perc = percent(x = Validity, 
                                 accuracy = 0.01)) %>% 
  group_by(Grouping) %>% 

e_charts(Week_Start) %>% 
  e_line(Validity) %>% 
  e_title("Data Validity Over Time") %>% 
  e_y_axis(formatter = e_axis_formatter("percent")) %>% 
  e_legend(type = "scroll")


##########################################################################
# Visit Count Banner
##########################################################################

#Create dataframe
visit_count <-31274

total_count <- 124982

not_selected <- total_count - visit_count
  
df <- as.data.frame(rbind(visit_count, total_count, not_selected))

df$category <- c("Selected", "Total", "Not selected")

df$percent <- df$V1 / sum(df$V1)

#Drop the total category
df <- subset(df, category != "Total")

#Create chart
df %>% 
  e_charts(category) %>% 
  e_pie(V1, radius = c("30%", "70%")) %>% 
  e_title(paste0("Total Visit Count Selected: ", visit_count, " (", round(100 * visit_count/total_count,1), "%)"))


#Data Flow

date_seq <- data.frame(Date = seq.Date(from = start, 
                                       to = end, 
                                       by = "day"))

df_all <- date_seq %>%
  
  left_join(operations_summary(skey = skey) %>%
              group_by(Date) %>%
              summarize(Processed = sum(Processed_Message_Count, na.rm = TRUE)) %>%
              ungroup(),
            by = "Date") %>%
  
  left_join(filters(skey = skey) %>%
              group_by(Date) %>% 
              summarize(Filtered = sum(Filtered_Reason_Count, na.rm = TRUE)) %>%
              ungroup(),
            by = "Date") %>%
  
  left_join(exceptions(skey = skey) %>%
              group_by(Date) %>%
              summarize(Exceptions = sum(Exceptions_Count, na.rm = TRUE)) %>%
              ungroup(),
            by = "Date") %>%
  
  mutate(Processed  = ifelse(is.na(Processed ), 0, Processed ),
         Exceptions = ifelse(is.na(Exceptions), 0, Exceptions),
         Filtered   = ifelse(is.na(Filtered  ), 0, Filtered  )) %>%
  
  rename(Arrived_Date = Date)


#Timeliness
timeliness_df <- timeliness_summary(skey = skey) %>%
  
  rename(timeliness = Average_Timeliness_in_Hours) %>%
  mutate(category = as.character(NA),
         category = ifelse(timeliness <= 24, "Early", category),
         category = ifelse(timeliness > 24 & timeliness <= 48, "Normal", category),
         category = ifelse(timeliness > 48, "Late", category),
         category = as.character(category)) %>%
  bind_rows(data.frame(Facility_Name = rep(x = "Placeholder", times = 3),
                       timeliness = c(20, 40, 60),
                       category = c("Early", "Normal", "Late"))) %>%
  group_by(Facility_Name, category) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  spread(key = category,
         value = n) %>%
  filter(Facility_Name != "Placeholder") %>%
  left_join(timeliness_summary(skey = skey) %>%
              group_by(Facility_Name) %>%           # Change to different grouping var here
              summarize(Total = n()) %>%
              ungroup(), 
            by = "Facility_Name") %>%
  rename(Entity = Facility_Name) %>% 
  mutate(Early = Early / Total,
         Normal = Normal / Total,
         Late = Late / Total)


#Completeness

completeness_df_total <- completeness_summary(skey = skey) %>%
  as.data.table() %>%
  .[ , lapply(.SD, sum, na.rm = TRUE), 
     by = .(Date, 
            Site_Skey, 
            Site_ID, 
            Facility_Name, 
            Facility_Type, 
            Week_Of_Year, 
            Parent_Organization, 
            C_BioSense_Facility_ID, 
            Feed_Name, 
            Vendor_Name)]%>%
  melt.data.table(measure.vars = patterns('^Completeness',
                                          '^InCompleteness',
                                          '^Total'), 
                  variable.name = 'Element', 
                  value.name = c('Complete',
                                 'Incomplete',
                                 'Total')) %>%
  .[, Element := factor(Element, 
                        labels = unique(gsub('.*_Count_','', 
                                             names(completeness_summary(skey = skey))[grepl('.*_Count_', 
                                                                                            names(completeness_summary(skey = skey)))])))] %>%
  .[Element %in% keeps_comp] %>%
  .[order(Element, Week_Of_Year)] 


completeness_df <- completeness_summary(skey = skey) %>%
  as.data.table() %>%
  .[ , lapply(.SD, sum, na.rm = TRUE), 
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
  melt.data.table(measure.vars = patterns('^Completeness',
                                          '^InCompleteness',
                                          '^Total'), 
                  variable.name = 'Element', 
                  value.name = c('Complete',
                                 'Incomplete',
                                 'Total')) %>%
  .[, Element := factor(Element, 
                        labels = unique(gsub('.*_Count_','', 
                                             names(completeness_summary(skey = skey))[grepl('.*_Count_', 
                                                                                            names(completeness_summary(skey = skey)))])))] %>%
  .[Element %in% keeps_comp] %>%
  .[order(Element, Week_Of_Year)] %>% 
  
  rename(Grouping = Facility_Name) %>%                   # Reassign y-axis variable here
  
  group_by(Grouping) %>%
  
  summarize(Completeness = sum(Complete, 
                               na.rm = TRUE) / sum(Total, na.rm = TRUE),
            n = n()) %>% 
  ungroup() %>% 
  arrange(-Completeness) %>% 
  mutate(Completeness_Perc = percent(x = Completeness, accuracy = 0.01))

sum(completeness_df$Total_Count_Facility_Type_Code)
sum(timeliness_df$Total)
sum(df_all$Processed) + sum(df_all$Exceptions) + sum(df_all$Filtered) 



#Practicing RMD
render_report = function(validity.df, validity.plot) {
  rmarkdown::render(
    "DQDB Graph Pop-out Reporting.rmd", params = list(
      data = data(),
      plot = plot
    ),
    output_file = "Report Plot.pdf"
  )
}


#echarts scatterplot for Natasha
#making a random variable of time components
rand.time <- function(size) {
  hourselect <- sample(1:24,size,replace=TRUE)
  minselect <- sample(0:59,size,replace=TRUE)
  paste(hourselect,":",minselect,sep="")
}

home_plot$time <- rand.time(size = nrow(home_plot))

home_plot$time <- as.POSIXlt(home_plot$time, format = "h:m:s")

home_plot %>% 
  e_charts(Date) %>% 
  e_scatter(Raw_Count)
