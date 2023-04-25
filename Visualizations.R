#Alyssa Venn
#3/23/2022
#Creating functions for the visualization that will be used in the DQDB.

#Library
library(plotly)
library(echarts4r)

#Data


#Function for gauge chart
#Val1 is whatever value we want for the gauge, val2 is for the comparison (that it
#looks like we have in the ppt?). Best way to do this would just be to have
#some sort of formula either before the function or within it, for the percent of
#the data selected. Like create an object for each value and then pass it to
#the function.
gauge_vis <- function(val1, val2){
  fig <- plot_ly(
    value = val1,
    title = list(text = "Percent Data Selected"),
    type = "indicator",
    mode = "gauge+number",
    gauge = list(
      axis = list(range = list(NULL, 100)),
      bar = list(color = "royalblue"),
      steps = list(
        list(range = c(0, val2), color = "cyan")
      )
    )
  )
  
  fig <- fig %>% 
    layout(margin = list(l = 20, r = 30))
  
  return(fig)
}


#Function for 100% stacked bar chart (horizontal)
horiz_stacked_bar_chart <- function(df, xval){

  fig <- plot_ly(df, x = ~Valid, y = ~xval, type = 'bar',  name = "Valid", orientation = 'h',
                 marker = list(color = "#BBF1C4"))
  
  fig <- fig %>% add_trace(x = ~Invalid, name = "Invalid",
                           marker = list(color = "#FDDD5C"))
  
  fig <- fig %>% add_trace(x = ~Unknown, name = "Unknown",
                           marker = list(color = "#FF7974"))
  
  fig <- fig %>% layout(barmode = 'stack', 
                        title = "Messages")
  
  return(fig)
}

#Testing
#gauge
gauge_vis(90, 95)

#100% stacked
type <- c("Selected", "All")

Valid <- c(90, 80)

Invalid <- c(10, 10)

Unknown <- c(0, 10)

df <- data.frame(type, Valid, Invalid, Unknown)

horiz_stacked_bar_chart(df, type)


#Trying echarts4r
#Gauge
e_charts() |>
  e_gauge(90, "PERCENT") |>
  e_title("Percent Data Selected")

#Bar
df |> 
  e_charts(type) |>
  e_bar(Valid, stack = "stack") |>
  e_bar(Invalid, stack = "stack") |>
  e_bar(Unknown, stack = "stack") |>
  e_x_axis(type = "category") |>
  e_flip_coords()

