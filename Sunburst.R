data("HairEyeColor")

HairEyeColor <- as.data.frame(HairEyeColor)


#plotly method

fig <- HairEyeColor %>% 
  relocate(Eye) %>% 
  relocate(Sex) %>% 
  rename(n = Freq) %>% 
  count_to_sunburst()

fig %>% layout(title = 'Interactive Sunburst Chart of Sex, Hair Color, and Eye Color', plot_bgcolor = "#e5ecf6")
