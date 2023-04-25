library(DT)
library(rjson)
iris$Sepal.Width <- 'Verrrrrrrrry Looooooooooooooong Commmmmmment'

iris_upd <- cbind(' ' = '<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_open.png\"/>', iris)

#Reading in json rules from dqdb_alerts/rules/public
rules <- list.files("/opt/sas/shared/homes/dwirtzoa01/bitbucket/data-quality-dashboard-redesign/dqdb_alerts/rules/public/",
                    pattern = "*.json", full.names = TRUE)
result <- data.frame()

for (i in rules) {
  rule <- fromJSON(file = i)
}

#The div from the DQDB
display_rule_details <- function(file){
  rule <- fromJSON(file)
  
  .pull <- function(ls){
    n <- names(ls)
    r <- c()
    for(i in n){
      if(is.list(ls[[i]][[1]])){
        n2 <- names(ls[[i]])
        r <- c(r, paste0('&nbsp;&nbsp;&nbsp;',i,': '))
        for(j in n2){
          r <- c(r, paste0('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;',j,': ',paste(ls[[i]][[j]], collapse = ', ')))
        }
      }else{
        r <- c(r, paste0('&nbsp;&nbsp;&nbsp;',i,': ',ls[[i]][[1]]))
      }
    }
    return(r)
  }
  
  sprintf('<h3>Rule Details</h3> <br><b>Name:</b> %s<br><b>Created By:</b> %s<br><b>Create DateTime:</b> %s<br><b>Data Quality Metric:</b> %s<br><b>Configurations:</b><br>%s<br><b>Method:</b> %s<br><b>Configurations:</b><br>%s',
          rule$name[[1]],
          rule$create_by[[1]],
          rule$create_date_time[[1]],
          rule$dq.metric[[1]],
          paste(.pull(rule$dq.metric.config), collapse = '<br>'),
          rule$detector[[1]],
          paste(.pull(rule$detector.config), collapse = '<br>'))
  
}

display_rule_details("/opt/sas/shared/homes/dwirtzoa01/bitbucket/data-quality-dashboard-redesign/dqdb_alerts/rules/public//3e2cab17a035e550bd672cc46c2adb48.json")


#How it is used
HTML(display_rule_details(file.path(gl.alert.dir.path,'rules',
  ifelse(rules_data()$`Rule Type`[input$dt_rules_rows_selected]=='private',
  session$userData$rv$sites$Site_ID[session$userData$rv$sites$Site_Name == 
  session$userData$rv$user.profile$Selected_Site],'public'),
  paste0(rules_data()$`Rule Name`[input$dt_rules_rows_selected],'.json'))))


#Datatable test
datatable(
  as.data.frame(rule),
  options = list(
    columnDefs = list(
      list(visible = FALSE, targets = c(0)),
      list(orderable = FALSE, className = 'details-control', targets = 1),
      list(
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 6 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
          "}")
      )
    )
  ),
  callback = HTML(display_rule_details("/opt/sas/shared/homes/dwirtzoa01/bitbucket/data-quality-dashboard-redesign/dqdb_alerts/rules/public//3e2cab17a035e550bd672cc46c2adb48.json")
))
  


#Datatable

datatable(
  iris_upd, 
  escape = -2,
  options = list(
    columnDefs = list(
      list(visible = FALSE, targets = c(0)),
      list(orderable = FALSE, className = 'details-control', targets = 1),
      list(
        targets = 3,
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 6 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
          "}")
      )
    )
  ),
  callback = JS("
                  table.column(1).nodes().to$().css({cursor: 'pointer'});
                  var format = function(d) {
                  return'<p>' + 'Insert div here' + '</p>';
                  };
                  table.on('click', 'td.details-control', function() {
                  var td = $(this), row = table.row(td.closest('tr'));
                  if (row.child.isShown()) {
                  row.child.hide();
                  td.html('<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_open.png\"/>');
                  } else {
                  row.child(format(row.data())).show();
                  td.html('<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_close.png\"/>');
                  }
                  });"
  ))






