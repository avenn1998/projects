library(echarts4r)
library(DBI)
library(odbc)
library(data.table)
library(magrittr)
library(MMWRweek)

con <- dbConnect(odbc::odbc(), Driver = "FreeTDS", Server = 'datacubes.biosense.wan', Port = 1433, UID = 'dashboards', PWD = '#d7yit6Wp+mT')

setwd("/opt/sas/shared/homes/avennoa01/Trend Classification")
#load data elements -- or explicitly write out elements (line 21)
data.elements <- read.csv('data_element_properties (1).csv', stringsAsFactors = FALSE) %>% as.data.table() %>% .[Is_Visible == TRUE]

#set Site_ID
sites <- c('857')

#set date between where clause
date <- " '2022-03-01' AND '2022-05-31' "

#filter to completeness elements only
ele <- data.elements[Is_Completeness == 1, Data_Element]
# ele <- c('Diagnosis_Code','Diagnosis_Description')

#prepare column names for sql (completeness and total only; omit incompleteness)
col <- unlist(lapply(ele, function(i) paste0(c('Completeness_Count_','Total_Count_'), i)))

#sql query from wide to long
qry <- paste0("
              SELECT Date, Site_ID, Facility_Name, Element, Completeness_Count, Total_Count
                FROM (
                  SELECT Date, Site_ID, Facility_Name, ", paste0("SUM(", paste(paste0(col, ") as ", col), collapse = ', SUM(')), "
                  FROM (
                      SELECT * from BIOSENSE_DW_DB.dbo.RPT_Table_Completeness with (NOLOCK) WHERE Date BETWEEN ",date," AND Site_ID in ('",paste(sites, collapse = "', '"),"')
                    ) b GROUP BY Date, Site_ID, Facility_Name
                ) t 
                CROSS APPLY (",
              paste(unlist(lapply(ele, function(i){paste0("select '", i, "' as element, ",
                                                          paste0("Completeness_Count_",i), " as Completeness_Count, ",
                                                          paste0("Total_Count_",i), " as Total_Count")})), collapse = " UNION ALL "),
              ") c
  ")


#submit query
tbl <- dbGetQuery(con, qry) %>% as.data.table()

#plot by Element
tbl %>%
  .[, .(count = sum(Completeness_Count), count_all = sum(Total_Count)), by = .(Date, Element)] %>%
  .[, Date_MMWR := MMWRweek::MMWRweek2Date(MMWRweek::MMWRweek(Date)$MMWRyear, MMWRweek::MMWRweek(Date)$MMWRweek, 1)] %>%
  .[, .(pct = sum(count)/sum(count_all)), by = .(Date_MMWR, Element)] %>%
  .[order(Date_MMWR)] %>%
  group_by(Element) %>%
  e_charts(Date_MMWR) %>% 
  e_line(pct) %>% 
  e_y_axis(
    min = 0,
    max = 1,
    formatter = e_axis_formatter("percent", digits = 0)
  ) %>%
  e_grid(top = 40, bottom = 150, left = 60, right = 20) %>%
  e_legend(type = 'plain', bottom = 10)
