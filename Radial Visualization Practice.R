#Alyssa Venn
#5/5/2022
#Create a pie chart with multiple layers for the Completeness data

#Libraries
library(echarts4r)
library(data.tree)
library(ggplot2)
library(ecodist)
library(plotly)
library(sunburstR)
library(treemapify)
library(ggplotify)

#Functions from global.R

`%!in%` <- Negate(`%in%`)

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

get_skey <- function(x, from_name = TRUE){
  
  if (from_name == TRUE){
    
    index <- which(site_info()$Site_Short_Name %in% x)
    
    output <- site_info()$Site_Skey[index]
    
  }
  
  if (from_name == FALSE){
    
    index <- which(site_info()$Site_ID %in% x)
    
    output <- site_info()$Site_Skey[index]
    
  }
  
  return(output)
  
}

completeness_summary <- function(skey, db = "BIOSENSE_DW_DB"){
  
  df <- summary_query(db = db, 
                      skey = skey, 
                      table = "RPT_Table_Completeness")
  
  merge(x = df, 
        y = dim_all())
  
}



#Data import

#AL
skey <- get_skey(x = 'AK')

comp_all <- completeness_summary(skey = skey) %>%
  as.data.table() %>%
  .[ , lapply(.SD, sum, na.rm = TRUE),
     by = .(Date,
            Site_Skey,
            Site_ID,
            Facility_Name,
            Lat, 
            Lon,
            Facility_City,
            Facility_Zip,
            Facility_County,
            State_FIPS_Code,
            County_FIPS_Code,
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
  .[order(Element, Week_Of_Year)]

comp_all <- comp_all %>%
  
  mutate(Date = ymd(Date)) %>% 
  
  filter(Date >= "2022-02-01",
         Date <= "2022-02-28")


#Create a variable that is 1 for each observation, which is then summed up
#in the next step. This allows us to simply see what proportion of the larger
#variables the larger variables make up, basically.
comp_all$V2 <- 1

#The ideal order is site > feed > vendor > parent > facility > element
#Aggregate data by these variables. Right now, the dataframe is SO big,
#it takes several minutes to create the chart.
comp_all_agg <- aggregate(V2 ~ Feed_Name + Vendor_Name + 
                          Facility_Name + Element, data = comp_all, FUN = sum, na.rm = TRUE)

# comp_all_agg2 <- aggregate(Incomplete ~ Feed_Name + Vendor_Name + 
#                             Facility_Name + Element, data = comp_all, FUN = sum, na.rm = TRUE)
# 
# comp_all_agg <- merge(comp_all_agg, comp_all_agg2)
#sunburstR method
#Element and Facility are the highest-priority layers
# comp_all_sunburst <- comp_all %>% 
#   select(Feed_Name, Vendor_Name, Facility_Name, Element, Complete) %>% 
#   mutate(across(everything(), gsub, pattern = "-", replacement = " "))%>% 
#   mutate(
#     path = paste(Facility_Name, Element,
#                  sep = "-")
#     ) 
  #slice(2:100)
  
comp_all_sunburst <- comp_all_agg %>% 
  mutate(across(everything(), gsub, pattern = "-", replacement = " "))%>% 
  mutate(
    path = paste(Feed_Name, Vendor_Name, Facility_Name,
                 sep = "-")
  ) 
#Complete got converted to a character for some reason

comp_all_sunburst$V2 <- as.numeric(comp_all_sunburst$V2)

sunburst(data = data.frame(xtabs(V2~path, comp_all_sunburst)))


#ggplot method

sunburst_1 = ggplot(comp_all_sunburst, aes(y = Complete, x = '')) +
  geom_bar(aes(x = 0.25, fill = Element), 
           stat = 'identity', width = 0.25) +
  geom_bar(aes(x = 0, fill = Facility_Name),
           stat = 'identity', width = 0.5) +
  coord_polar('y') +
  theme(legend.position = "none")

sunburst_1

#plotly method
comp_all_sunburst_plotly <- comp_all_agg %>% 
  mutate(across(everything(), gsub, pattern = "-", replacement = " "))%>% 
  mutate(
    ids = paste(Facility_Name, Element,
                 sep = "-")) %>% 
  mutate(
     parents = paste(Facility_Name,
                 sep = "-")
  ) #%>%
  #slice(1:200)

comp_all_sunburst_plotly$V2 <- as.numeric(comp_all_sunburst_plotly$V2)

sunburst_plotly <- plot_ly(comp_all_sunburst_plotly, ids = ~ids, labels = ~Element,
                           parents = ~parents,
                           values = ~V2, type = 'sunburst', branchvalues = 'total')

sunburst_plotly

#Try making a tree map plot instead

treeMapCoordinates <- treemapify(comp_all_sunburst_plotly,
                                 area = "Complete",
                                 fill = "parents",
                                 label = "ids",
                                 group = "parents")

treeMapPlot <- ggplot(treeMapCoordinates) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_brewer(palette = "Dark2")

print(treeMapPlot)


#Plotly attempt 2
comp_all_agg <- aggregate(Complete ~ Feed_Name + Vendor_Name + 
                            Facility_Name, data = comp_all, FUN = sum, na.rm = TRUE)

comp_all_agg2 <- aggregate(Incomplete ~ Feed_Name + Vendor_Name + 
                             Facility_Name, data = comp_all, FUN = sum, na.rm = TRUE)

comp_all_agg <- merge(comp_all_agg, comp_all_agg2)

comp_all_sunburst_plotly <- comp_all_agg %>% 
  mutate(across(everything(), gsub, pattern = "-", replacement = " "))%>% 
  mutate(
    ids = paste(Feed_Name, Vendor_Name,
                sep = "-")) %>% 
  mutate(
    parents = paste(Feed_Name,
                    sep = "-")
  )

as.sunburstDF <- function(DF, value_column = NULL, add_root = FALSE){
  require(data.table)
  
  colNamesDF <- names(DF)
  
  if(is.data.table(DF)){
    DT <- copy(DF)
  } else {
    DT <- data.table(DF, stringsAsFactors = FALSE)
  }
  
  if(add_root){
    DT[, root := "Total"]  
  }
  
  colNamesDT <- names(DT)
  hierarchy_columns <- setdiff(colNamesDT, value_column)
  DT[, (hierarchy_columns) := lapply(.SD, as.factor), .SDcols = hierarchy_columns]
  
  if(is.null(value_column) && add_root){
    setcolorder(DT, c("root", colNamesDF))
  } else if(!is.null(value_column) && !add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c(setdiff(colNamesDF, value_column), "values"))
  } else if(!is.null(value_column) && add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c("root", setdiff(colNamesDF, value_column), "values"))
  }
  
  hierarchyList <- list()
  
  for(i in seq_along(hierarchy_columns)){
    current_columns <- colNamesDT[1:i]
    if(is.null(value_column)){
      currentDT <- unique(DT[, ..current_columns][, values := .N, by = current_columns], by = current_columns)
    } else {
      currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=current_columns, .SDcols = "values"]
    }
    setnames(currentDT, length(current_columns), "labels")
    hierarchyList[[i]] <- currentDT
  }
  
  hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
  
  parent_columns <- setdiff(names(hierarchyDT), c("labels", "values", value_column))
  hierarchyDT[, parents := apply(.SD, 1, function(x){fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parent_columns]
  hierarchyDT[, ids := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
  hierarchyDT[, c(parent_columns) := NULL]
  return(hierarchyDT)
}

comp_all_sunburst_plotly$Complete <- as.numeric(comp_all_sunburst_plotly$Complete)

sunburstDF <- as.sunburstDF(comp_all_sunburst_plotly, value_column = "Complete", add_root = TRUE)

sunburstDF <- sunburstDF %>% 
  mutate(
    ids = paste(parents, labels,
                    sep = "-")
  ) %>% 
  slice(1:10)

sunburstDF <- na.omit(sunburstDF)
plot_ly(data = sunburstDF, ids = ~ids, labels= ~labels, 
        parents = ~parents, type='sunburst', branchvalues = 'total')



#Testing something for the shiny app

test <- "AKehealth Unknown Kanakanak Hospital"


comp_all_sunburst %>% 
  mutate(path2 = gsub("-", " ", path)) %>% 
  dplyr::filter(test %in% path2)
