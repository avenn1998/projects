#For ticket 1122: Develop DT hovertext
#set up the hovertext JS logic for any elements that have hovertext in our wireframes/mockups

#Libraries
library(data.table)

#Dummy rule generator from Jamison

set.seed(998)
n_rules <- sample(x = 0:30, size = 1, replace = FALSE)

url <- paste0("https://gist.githubusercontent.com/jaidevd/23aef12e9bf56c618c41",
              "/raw/c05e98672b8d52fa0cb94aad80f75eb78342e5d4/books.csv")

titles <- readr::read_csv(file = url,
                          col_select = "Title",
                          col_types = "c",
                          trim_ws = TRUE)$Title

url <- "https://raw.githubusercontent.com/akhiltak/inspirational-quotes/master/Quotes.csv"

descriptions <- readr::read_delim(file = url,
                                  delim = ";",
                                  col_select = 1)$QUOTE[sample(x = 1:75000,
                                                               size = n_rules,
                                                               replace = FALSE)]


recipients <- NA

for (i in 1:n_rules){
  
  sublist <- NA
  
  for (j in 1:sample(x = 1:10, size = 1)){
    
    sublist[j] <- paste0(paste0(sample(x = letters,
                                       size = 3,
                                       replace = FALSE),
                                collapse = ""),
                         sample(x = 0:9, size = 1), "@cdc.gov")
    
  }
  
  recipients[i] <- paste0(sublist, collapse = "; ")
  
}

sched_days <- NA

for (i in 1:n_rules){
  
  sched_days[i] <- paste0(paste0(sort(sample(x = factor(x = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                                                        levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                                                        ordered = TRUE),
                                             size = sample(x = 1:7,
                                                           size = 1,
                                                           prob = (1:7)^-0.95),
                                             replace = FALSE)), collapse = "; "), " at ",
                          paste0(sample(x = 1:12, size = 1), ":",
                                 sample(x = c("00", "15", "30", "45"), size = 1), " ",
                                 sample (x = c("AM", "PM"), size = 1)))
  
}

metrics <- sample(x = c("Data Flow", "Timeliness", "Completeness", "Validity"),
                  size = n_rules,
                  replace = TRUE)

submetrics <- NA

for (i in seq_along(metrics)){
  
  if (metrics[i] == "Data Flow"){
    
    submetrics[i] <- sample(x = c("Processed",
                                  "Filtered",
                                  "Excepted"), size = 1)
    
  }
  
  if (metrics[i] == "Timeliness"){
    
    submetrics[i] <- sample(x = c("Less Than 24 Hours",
                                  "Between 24 and 48 Hours",
                                  "Greater Than 48 Hours"), size = 1)
    
  }
  
  if (metrics[i] == "Completeness" | metrics[i] == "Validity"){
    
    submetrics[i] <- "Average"
    
  }
  
}

dummy_rules <- tibble("name" = sample(x = titles, size = n_rules, replace = FALSE),
                      "metric" = metrics,
                      "submetric" = submetrics,
                      "grouping" = sample(x = c("None", "Facility", "Feed", "Vendor", "Parent", "Site", "Type"), size = n_rules, replace = TRUE),
                      "status" = sample(x = c("Active", "Inactive"), size = n_rules, replace = TRUE, prob = c(0.85, 0.15)),
                      "severity" = sample(x = c("Low", "Medium", "High"), size = n_rules, replace = TRUE),
                      "description" = descriptions,
                      
                      "method" = sample(x = c("CUSUM", "EWMA", "Percent Change", "Value Limit", "Summary Statistic"), size = n_rules, replace = TRUE),
                      "width" = sample(x = c(1, 3, 5, 7, 14, 21, 28), size = n_rules, replace = TRUE),
                      "guardband" = round(x = width / sample(x = seq_along(width), size = n_rules, replace = TRUE, prob = seq_along(width)^0.01), digits = 0),
                      "threshold" = sample(x = 1:50000, size = n_rules, replace = TRUE),
                      "min_anomalies" = sample(x = 1:100, size = n_rules, replace = TRUE),
                      "exclude_weekends" = sample(x = c(TRUE, FALSE), size = n_rules, replace = TRUE),
                      "min_consecutive" = round(x = width / sample(x = seq_along(width), size = n_rules, replace = TRUE), digits = 0),
                      "standardize" = sample(x = c(TRUE, FALSE), size = n_rules, replace = TRUE, prob = c(0.2, 0.8)),
                      "direction" = sample(x = c("Below", "Above"), size = n_rules, replace = TRUE),
                      "sum_stat" = sample(x = c("Mean", "Median", "Mode", "1st Quartile", "3rd Quartile"), size = n_rules, replace = TRUE),
                      "multiplier" = 1 + sample(x = seq(from = -0.5, to = 2, by = 0.05), size = n_rules, replace = TRUE, prob = rev(seq(from = -0.5, to = 2, by = 0.05)^2)),
                      "roll_stat" = sample(x = c(TRUE, FALSE), size = n_rules, replace = TRUE),
                      
                      "n_method" = sample(x = c("None", "Scheduled", "Condition-Based"), size = n_rules, replace = TRUE),
                      "n_schedule" = sched_days,
                      "n_recipients" = recipients,
                      "n_timeframe" = sample(x = c(1, 3, 5, 7, 14, 21, 28), size = n_rules, replace = TRUE),
                      "n_min_alerts" = sample(x = 1:100, size = n_rules, replace = TRUE),
                      "n_max_notices" = sample(x = 1:20, size = n_rules, replace = TRUE))

for (i in 1:nrow(dummy_rules)){
  
  if (dummy_rules$method[i] != "Summary Statistic"){
    
    dummy_rules$sum_stat[i] <- NA
    dummy_rules$roll_stat[i] <- NA
    dummy_rules$multiplier[i] <- NA
    
  }
  
  if (dummy_rules$n_method[i] != "Scheduled"){
    
    dummy_rules$n_schedule[i] <- NA
    
  }
  
  if (dummy_rules$standardize[i] == TRUE){
    
    dummy_rules$threshold[i] <- round(x = dummy_rules$threshold[i]^-0.1, digits = 2)
    
  }
  
  if (dummy_rules$method[i] == "CUSUM"){
    
    dummy_rules$threshold[i] <- sample(x = seq(from = 0.5, to = 3.5, by = 0.5), size = 1)
    
  }
  
}


#Data table
datatable(dummy_rules, rownames = F,
          callback = JS("
                              table.on('mouseenter', 'tbody td', function() {
                                var column = $(this).index();
                                var row = $(this).parent().index();

                                var dataFromOtherTable = $('#tableWithHoverData').find('tbody tr').eq(row).find('td').eq(column).text();

                                this.setAttribute('title', dataFromOtherTable);
                              });

                              return table;
                              ")
)
