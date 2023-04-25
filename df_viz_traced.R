
# SETUP & DATA ----

library(DBI)
library(odbc)
library(dplyr)
library(plotly)
library(scales)

c


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
