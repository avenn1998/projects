
# SETUP & DATA ----

library(DBI)
library(odbc)
library(dplyr)
library(plotly)
library(scales)

cubes_hitter <- function(query){
  
  con <- dbConnect(odbc::odbc(),
                   Driver = "FreeTDS",
                   Server = "datacubes.biosense.wan",
                   Port = 1433,
                   UID = "dashboards",
                   PWD = "#d7yit6Wp+mT")
  
  table <- dbGetQuery(con, query)
  
  return(table)
  
}



# QUERIES ----

skey <- 4 # Arizona's Skey - find more Skey values with: SELECT * FROM [BIOSENSE_DW_DB].[dbo].[Dim_Site]

qry <- paste0("SELECT * FROM [BIOSENSE_DW_DB].[dbo].[Rpt_Table_Essence_Visit_Count]")

essence <- cubes_hitter(qry)

qry <- paste0("SELECT Parent_Organization,
                      Site_ID,
                      Facility_Type,
                      C_BioSense_Facility_ID,
                      Feed_Name,
                      Vendor_Name,
                      Facility_Name,
                      Facility_Status,
                      Record_Status
               FROM [Master_Profile].[dbo].[Facility_Master]
               WHERE Primary_Facility = 'Y'")

dim_all <- cubes_hitter(qry)

qry <- paste0("SELECT * FROM [BIOSENSE_DW_DB].[dbo].[RPT_Table_Processed_Visit_Count] ",
              "WHERE Site_Skey = '", skey, "'")

proc <- cubes_hitter(qry) %>% 
  left_join(dim_all, 
            by = c("Site_ID", 
                   "Facility_Name", 
                   "Feed_Name"))

qry <- paste0("SELECT * ",
              "FROM [BIOSENSE_DW_DB].[dbo].[RPT_Table_Operations] ",
              "WHERE Site_Skey = ", 4," and (Date) >= GETDATE() - 122")

dataflow_all <- cubes_hitter(qry)

dataflow <- merge(x = dim_all, 
                  y = dataflow_all, 
                  by = c("Facility_Name", "Site_ID"))



# PLOT ----

seq_date <- data.frame(Date = seq.Date(from = start, 
                                       to = end, 
                                       by = "day"))

df_all2 <- seq_date %>%
  
  left_join(essence %>%
              group_by(Date) %>%
              summarize(Essence = sum(Essence_Visit_Count, na.rm = TRUE)) %>%
              ungroup(),
            by = "Date") %>%
  
  left_join(proc %>%
              group_by(Date) %>% 
              summarize(Processed = sum(Visit_Count, na.rm = TRUE)) %>% 
              ungroup(),
            by = "Date") %>%
  
  left_join(dataflow %>%
              group_by(Date) %>% 
              summarize(Messages = sum(Processed_Message_Count, na.rm = TRUE)) %>%
              ungroup(),
            by = "Date") %>%
  
  mutate(Essence   = ifelse(is.na(Essence),   0, Essence),
         Processed = ifelse(is.na(Processed), 0, Processed),
         Messages  = ifelse(is.na(Messages),  0, Messages)) %>%
  
  arrange(desc(Date))

p <- plot_ly(data = df_all2, 
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
         title = "Processed Message Count with Processed Visit and ESSENCE Visit Count Overlay",
         xaxis = list(type = "date",
                      tickformat = "%d %b. %Y"),
         hoverlabel = "left")

p