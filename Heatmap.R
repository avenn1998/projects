
library(echarty)

df <- read.csv("~/Portfolio/Pattern Recognition Alert/daily-minimum-temperatures-in-me.csv")

# Generate a random permutation of the hours
df$hours <- sample(0:23, nrow(df), replace = TRUE)

df$day_of_week <- lubridate::wday(
  mdy(df$Date),
  label = TRUE
)

df$hours <- as.numeric(df$hours)
df$Daily.minimum.temperatures <- as.numeric(df$Daily.minimum.temperatures)
df$day_of_week <- as.character(df$day_of_week)

df <- df |>
  dplyr::group_by(day_of_week, hours) |> 
  dplyr::summarise(Daily.minimum.temperatures = sum(Daily.minimum.temperatures)) |> 
  dplyr::ungroup()

df |> 
  e_charts(day_of_week, reorder = FALSE) |> 
  e_heatmap(hours, Daily.minimum.temperatures) |> 
  e_visual_map(Daily.minimum.temperatures) |> 
  e_title("Heatmap of Daily Minimum Temperature by Hour of Day and Day of Week")


#RMD
heatmap_df <- df %>% group_by(hours, day_of_week) %>% 
  summarise(min_temp = sum(as.numeric(Daily.minimum.temperatures))) %>% 
  dplyr::ungroup() %>% 
  arrange(hours, day_of_week)


heatmap_df <- heatmap_df %>% 
  arrange(day_of_week, hours)



#echarts
heatmap_df$day_of_week <- as.character(heatmap_df$day_of_week)

heatmap_df |> 
  distinct(hours, day_of_week, .keep_all=TRUE) |>
  ec.init(
    
    title= list(text= "Heatmap of Daily Minimum Temperature by Day and Hour"),
    
    series= list(list(type= 'heatmap')),
    
    xAxis= list(type= 'category', data= c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')),
    
    yAxis= list(min= 0, max = 23, name= 'hours', interval= 1),
    
    visualMap= list(calculable= TRUE, min= min(na.omit(heatmap_df$min_temp)), max = max(na.omit(heatmap_df$min_temp)),
                    
                    orient= 'horizontal', bottom= 'bottom', left='middle', align= 'right', 
                    
                    inRange= list(color= c('yellow','red'))),
    
    tooltip= list(trigger= "item"))


heatmap_df |> 
  distinct(hours, day_of_week, .keep_all=TRUE) |>
  ec.init(
    title = list(text = "Heatmap of Daily Minimum Temperature by Day and Hour"),
    series = list(list(type = "heatmap")),
    xAxis = list(min = -1, max = 24, name = "hours", interval = 1),
    yAxis = list(type = "category", data = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
    visualMap = list(calculable = TRUE, min = min(na.omit(heatmap_df$min_temp)), max = max(na.omit(heatmap_df$min_temp)),
                     orient = "horizontal", bottom = "bottom", left = "middle", align = "right", 
                     inRange = list(color = c("yellow", "red"))),
    tooltip = list(trigger = "item"))


save(heatmap_df, file = "~/Portfolio/portfolio/heatmap_df.rda")

p <- ggplot(heatmap_df, aes(day_of_week, hours, fill= min_temp)) + 
     geom_tile() + xlab("Day of the Week") + ylab("Hour of the Day") + 
     ggtitle("Heatmap of Daily Minimum Temperature by Day and Hour",
             subtitle = "24 Hour Clock") +
   scale_fill_gradient(low = "yellow",
                       high = "red",
                       guide = "colorbar")
   
   ggplotly(p, tooltip = "all")
