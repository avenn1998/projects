library("RSQLite")

## connect to db
con <- dbConnect(drv=RSQLite::SQLite(), dbname="~/bitbucket/dqad-poc/shiny/shinylogs.sqlite")

## list all tables
tables <- dbListTables(con)

## exclude sqlite_sequence (contains table information)
#tables <- tables[tables != "sqlite_sequence"]


#Saving as dataframes
inputs2 <- dbGetQuery(conn=con, statement="SELECT * FROM 'inputs'")

outputs2 <- dbGetQuery(conn=con, statement="SELECT * FROM 'outputs'")

session2 <- dbGetQuery(conn=con, statement="SELECT * FROM 'session'")


#Try part 2
con <- dbConnect(drv=RSQLite::SQLite(), 
                 dbname = "~/Validity Grid Card Alerts Page/ValidityAlert/user_stats.sqlite")

## list all tables
tables <- dbListTables(con)

#Saving as dataframes
session <- dbGetQuery(conn=con, statement="SELECT * FROM 'session_details'")

user_log <- dbGetQuery(conn=con, statement="SELECT * FROM 'user_log'")



#Testing JSON
result <- fromJSON(file = "~/Validity Grid Card Alerts Page/ValidityAlert/user_logs.json")

print(result)


#Try part 2
con <- dbConnect(drv=RSQLite::SQLite(), 
                 dbname = "~/Validity Grid Card Alerts Page/ValidityAlert/user_login_tracking.sqlite")

## list all tables
tables <- dbListTables(con)

#Saving as dataframes
login <- dbGetQuery(conn=con, statement="SELECT * FROM 'user_login_tracking.sqlite'")
