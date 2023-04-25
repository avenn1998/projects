library(data.table)
library(magrittr)
library(shinylogs)
library(rjson)
library(hms)

#dependencies

#bit64
#nanotime

# time <- Sys.time()

`%!in%` <- Negate(`%in%`)

fmt_time <- function(time = NULL){
  if (is.null(time)) 
    time <- Sys.time()
  format(time, format = "%Y-%m-%dT%H:%M:%S%z")
}

hms <- function(secs){
  h.int <- floor(secs/3600)
  h <- sprintf('%02d', h.int)
  m.int <- floor((secs - (h.int * 3600))/60)
  m <- sprintf('%02d', m.int)
  s.int <- floor(secs - (h.int * 3600) - (m.int * 60))
  s <- sprintf('%02d', s.int)
  return(paste(c(h,m,s),collapse = ':'))
}

# timestamp <- format(bit64::as.integer64(nanotime::nanotime(Sys.time())), scientific = FALSE)

auditR_init <- function(use_timeout = TRUE, timeout = 5, session = getDefaultReactiveDomain()){
  
  app_name <- basename(getwd())
  user <- Sys.info()[["user"]] #Sys.getenv("SHINYPROXY_USERNAME
  timestamp <- Sys.time()
  session_id <- digest::digest(timestamp)
  
  init_log <- list(app = app_name, user = user, session_id = session_id, server_connected = fmt_time(timestamp))
  
  session$userData$.auditR <- init_log
  
  # .auditR <- list2env(init_log, envir = .GlobalEnv)
  # ls(envir = .auditR)
  # storage_mode$appname <- app_name
  # storage_mode$timestamp <- format(as.integer64(nanotime(timestamp)), 
  #                                  scientific = FALSE)
  # init_log$sessionid <- digest::digest(timestamp)
  
  write(jsonlite::toJSON(init_log), paste0('./',session_id,'.json'))
  
  print(init_log)
  # observe({
  #   print(jsonlite::toJSON(session$input$bins))
  # })
  
  if(isTRUE(use_timeout)){
    timeoutSeconds <- timeout

    inactivity <- sprintf("function idleTimer() {
    var t = setTimeout(logout, %s);
    window.onmousemove = resetTimer; // catches mouse movements
    window.onmousedown = resetTimer; // catches mouse movements
    window.onclick = resetTimer;     // catches mouse clicks
    window.onscroll = resetTimer;    // catches scrolling
    window.onkeypress = resetTimer;  // catches keyboard actions
    
    function logout() {
    Shiny.setInputValue('.auditR_timeOut', '%ss')
    }

    function resetTimer() {
    clearTimeout(t);
    t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
    }
    }
    idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)

    #insert UI
    insertUI(selector = 'body', where = 'afterBegin', ui = singleton(tags$head(tags$script(inactivity))), immediate = TRUE, session = session)
    
    observeEvent(session$input$.auditR_timeOut, {
      print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
      showModal(modalDialog(
        title = "Timeout",
        paste("Session timeout due to", session$input$.auditR_timeOut, "of inactivity -", Sys.time()),
        footer = NULL
      ))
      session$close()
    })
  }

  
  onSessionEnded(fun = function() {
    init_log$server_disconnected <- fmt_time(Sys.time())
    # logs <- c(isolate(session$input$.shinylogs_input), isolate(session$input$.shinylogs_error), 
    #           isolate(session$input$.shinylogs_output))
    # browser_data <- isolate(session$input$.shinylogs_browserData)
    # if (!is.null(browser_data)) {
    #     browser_data <- as.data.frame(browser_data)
    #     logs$session <- cbind(init_log, browser_data)
    # }
    # else {
    #     logs$session <- init_log
    # }
    # if (isTRUE(!user %in% exclude_users)) {
    #     write_logs(storage_mode, logs)
    # }
    write(jsonlite::toJSON(init_log), paste0('./',session_id,'.json'))
    stopApp()
  }, session = session)
  
}

#Taking the function from shiny.stats and altering it

auditR_log_input <- function (input, input_ids, matching_values = NULL, 
                              input_type = "text", app = NULL) 
{  
  connection <- odbc::dbConnect(RSQLite::SQLite(), dbname = "user_stats_2.sqlite")
  
  user_connection <- initialize_connection(connection, username = Sys.info()[["user"]] )
  
  for(i in input_ids){
    
  shiny::observeEvent(input[[i]], {
    input_value <- input[[i]]
    if (is.logical(input_value)) {
      input_value <- as.character(input_value)
    }
    if (!is.null(input_value)) {
      n_values <- length(input_value)
      if (!is.null(matching_values) && input_type == "json") {
        input_value <- parse_val(input_value)
      }
      if (is.null(matching_values) | (!is.null(matching_values) && 
                                      input_value %in% matching_values)) {
        db <- user_connection$db_connection
        persist_log <- function(input_value, i) {
          res <- odbc::dbSendQuery(conn = db, DBI::sqlInterpolate(conn = db, 
                                                                  "INSERT INTO user_log\n              VALUES (?time, ?session, ?username, ?action, ?id, ?value, ?app_name)", 
                                                                  time = as.character(Sys.time()), session = user_connection$session_id, 
                                                                  username = user_connection$username, 
                                                                  action = "input", id = i, value = input_value,
                                                                  app_name = app))
          odbc::dbClearResult(res)
          
          
        }
        if (n_values > 1) {
          input_ids2 <- sprintf("%s_%s", i, 1:n_values)
          purrr::walk2(input_value, input_ids2, persist_log)
        }
        else {
          persist_log(input_value, i)
        }
      }
    }
  } , priority = -1, ignoreInit = TRUE) }
  
    
  
}


# Writes to SQL

auditR_log_login <- function (app_name = NULL) 
{
  connection <- odbc::dbConnect(RSQLite::SQLite(), dbname = "user_login_tracking.sqlite")
  
  user_connection <- initialize_connection(connection, username = Sys.info()[["user"]] )
  
  log_custom_action(user_connection, "user_login_tracking.sqlite", values = list(session = user_connection$session_id, 
                                                                    username = user_connection$username, action = "login",
                                                                    app = app_name))
}

#Writes to text file
auditR_log_login_txt <- function (app_name = NULL) {
  
  connection <- odbc::dbConnect(RSQLite::SQLite(), dbname = "user_login_tracking.sqlite")
  
  user_connection <- initialize_connection(connection, username = Sys.info()[["user"]] )
  
  df <- data.frame(session = user_connection$session_id, 
             username = user_connection$username, action = "login",
             app = app_name)
  
  write.table(df, file = "user_login_tracking.txt", sep = ",")
}

#Input with text file
auditR_log_input_txt <- function (input, input_ids, matching_values = NULL, 
                              input_type = "text", app = NULL) 
{  
  connection <- odbc::dbConnect(RSQLite::SQLite(), dbname = "user_stats_2.sqlite")
  
  user_connection <- initialize_connection(connection, username = Sys.info()[["user"]] )
  
  res <- data.frame(time = character(), session = character(), 
                    username = character(), 
                    action = character(), id = character(), value = character(),
                    app_name = character())
  
  #print(res)
  
  for(i in input_ids){
    
    shiny::observeEvent(input[[i]], {
      input_value <- input[[i]]
      if (is.logical(input_value)) {
        input_value <- as.character(input_value)
      }
      if (!is.null(input_value)) {
        n_values <- length(input_value)
        if (!is.null(matching_values) && input_type == "json") {
          input_value <- parse_val(input_value)
        }
        if (is.null(matching_values) | (!is.null(matching_values) && 
                                        input_value %in% matching_values)) {
          db <- user_connection$db_connection
          persist_log <- function(input_value, i) {
            test <- rbind(res, c(as.character(Sys.time()), user_connection$session_id, 
                                 user_connection$username, 
                                 "input", i, input_value,
                                 app))
            
            
            return(test)
          }
          
        }
        
        if (n_values > 1) {
          input_ids2 <- sprintf("%s_%s", i, 1:n_values)
          test2 <- purrr::walk2(input_value, input_ids2, persist_log)
        }
        else {
          test2 <- persist_log(input_value, i)
        }
      }
      
      #print(test)
      
      print(test2)
      #res <- rbind(res, test)
      
      #print(res)
      
      write.table(test2, file = "user_input_tracking.txt", sep = ",")} , 
      
      priority = -1, ignoreInit = TRUE) }
  
  
  
}
js <- "
// execute the code after the shiny session has started
$(document).on('shiny:sessioninitialized', function(event) {
  // browser detection from https://stackoverflow.com/a/5918791/8099834
  navigator.sayswho= (function(){
    var ua= navigator.userAgent, tem, 
    M= ua.match(/(opera|chrome|safari|firefox|msie|trident(?=\\/))\\/?\\s*(\\d+)/i) || [];
    if(/trident/i.test(M[1])){
        tem=  /\\brv[ :]+(\\d+)/g.exec(ua) || [];
        return 'IE '+(tem[1] || '');
    }
    if(M[1]=== 'Chrome'){
        tem= ua.match(/\\b(OPR|Edge)\\/(\\d+)/);
        if(tem!= null) return tem.slice(1).join(' ').replace('OPR', 'Opera');
    }
    M= M[2]? [M[1], M[2]]: [navigator.appName, navigator.appVersion, '-?'];
    if((tem= ua.match(/version\\/(\\d+)/i))!= null) M.splice(1, 1, tem[1]);
    return M.join(' ');
  })(); 
  // pass browser info from JS to R
  Shiny.onInputChange('myBrowser', navigator.sayswho); 
});
"


auditR_log_API <- function (input, input_ids = NULL,
                            input_type = "text", 
                            session = session,
                            extra = FALSE) {
  
  #session$token is an existing session ID, rather than needing to create my own
  start_time <- Sys.time
  
  app <- isolate({
    basename(session$url_pathname)
  })
  
  
  #wrap session in "shiny::isolate" to get around "reactive" requirement

  shiny::onStop(function() {
    
    stop_time <- Sys.time()
    
    session_length <- difftime(start_time, stop_time, units = "mins")
    
    
  
  })
  
  if (extra == TRUE){
    for(i in input_ids){
      
      shiny::observeEvent(input[[i]], {
        body <- list(user = Sys.info()[["user"]], 
                     app_name = app,
                     datetime = Sys.time(),
                     action = i, 
                     action_value = input[[i]],
                     session_id = user_connection$session_id
                     )
        
        print(i)
        print(input[[i]])
        POST('https://biosense-beta-api.syndromicsurveillance.org/log', body = body, encode = 'json')
      
        
        
        })
    }
  }
  
}


#Code to capture screen resolution in DQDB

mod_window_properties_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(tags$script(paste0("
        var dimension = [0, 0];
        $(document).on('shiny:connected', function(e) {
            dimension[0] = window.innerWidth;
            dimension[1] = window.innerHeight;
            dimension[2] = screen.width;
            dimension[3] = screen.height;
            Shiny.setInputValue('", ns("dimension"), "', dimension);
        });
        $(window).resize(function(e) {
            dimension[0] = window.innerWidth;
            dimension[1] = window.innerHeight;
            dimension[2] = screen.width;
            dimension[3] = screen.height;
            Shiny.setInputValue('", ns("dimension"), "', dimension);
        });
    ")))
  )
}
mod_window_properties_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      
      #set properties to session data
      observe({
        session$userData$rv$window.properties <- list(innerWidth = input$dimension[1], 
                                                      innerHeight = input$dimension[2],
                                                      screenWidth = input$dimension[3],
                                                      screenHeight = input$dimension[4])
      })
      
    }
  )
}

#Init rework

auditR_init <- function(session = getDefaultReactiveDomain(), extra = FALSE, appname = NULL){
 
  #To do: we need a condition to detect shiny proxy deployment
  
  user_id <- ifelse(Sys.getenv("SHINYPROXY_USERNAME") != "", Sys.getenv("SHINYPROXY_USERNAME"), Sys.getenv("USER"))
  
  appname <- ifelse(!is.null(appname), appname, basename(session$clientData$url_pathname))
  
  init_log <- list(session_id = session$token, user_id = user_id, 
                   app_name = appname, login_datetime = Sys.time(),
                   logout_datetime = NULL, session_length = NULL, activity = list())
  
  session$userData$.auditR <- init_log
  
  #Go through W3 schools, look at type of window info captured, keep list of potentially
  #useful data. Screenrez and client are especially important
  
  #print(init_log)
  
  # if (extra = TRUE){
  #   for(i in input_ids){
  #   
  #    shiny::observeEvent(input[[i]], {
  #       input_value <- input[[i]]
  #       
  #       event_activity <- list(value = input_value, 
  #                              datetime = Sys.time(),
  #                              description = i)
  #       
  #       session$userData$.auditR$
  #       
  #       
  #       
  #    })
  #   }
  # }
  
  shiny::onStop(function() {
   session$userData$.auditR$logout_datetime <- Sys.time()
    
   session$userData$.auditR$session_length <- hms(as.integer(difftime(session$userData$.auditR$logout_datetime, 
                                                                      session$userData$.auditR$login_datetime, 
                                                                      units = 'secs')))
   
   #print(session$userData$.auditR)
   
   #To do: POST request to API backend
   
   POST('https://biosense-beta-api.syndromicsurveillance.org/log', body = session$userData$.auditR, encode = 'json')
   
    stopApp()
  }, session = session)
  
}

#Old code

# login <- list(user = Sys.info()[["user"]], 
#              app_name = basename(session$clientData$url_hostname),
#              datetime = Sys.time(),
#              action = 'login',
#              action_value = NULL,
#              session_id = user_connection$session_id)

#POST('https://biosense-beta-api.syndromicsurveillance.org/log', body = login, encode = 'json')


# logout <- list(user = Sys.info()[["user"]],
# app_name = basename(session$clientData$url_hostname),
# datetime = Sys.time(),
# action = 'logout',
# action_value = NULL,
# session_id = user_connection$session_id)

#POST('https://biosense-beta-api.syndromicsurveillance.org/log', body = logout, encode = 'json')

auditR_screenInfo <- function() {
  
  #Screen res
  jscode <- 'Shiny.setInputValue("GetScreenWidth", screen.width);
            Shiny.setInputValue("GetScreenHeight", screen.height);'
  
  
  insertUI(selector = 'body', where = 'afterBegin', 
           ui = tags$script(jscode), 
           immediate = TRUE)
  
  
  observe({
    
    print(input$GetScreenWidth)
    
    print(input$GetScreenHeight)
  })
  
  #Browser
  jsbrowser <- 'function fnBrowserDetect(){
                 
         let userAgent = navigator.userAgent;
         let browserName;
         
         if(userAgent.match(/chrome|chromium|crios/i)){
             browserName = "chrome";
           }else if(userAgent.match(/firefox|fxios/i)){
             browserName = "firefox";
           }  else if(userAgent.match(/safari/i)){
             browserName = "safari";
           }else if(userAgent.match(/opr/i)){
             browserName = "opera";
           } else if(userAgent.match(/edg/i)){
             browserName = "edge";
           }else{
             browserName="No browser detection";
           }
         
           Shiny.setInputValue("GetBrowser", browserName);        
  };
  fnBrowserDetect()
   '
  
  insertUI(selector = 'body', where = 'afterBegin', 
           ui = tags$script(jsbrowser), 
           immediate = TRUE)
  
  observe({
    
    print(input$GetBrowser)
    
  })
}
