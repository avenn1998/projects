#Testing time series pattern recognition methods on a random dataset from Kaggle
#Dataset was downloaded from 
#https://www.kaggle.com/shenba/time-series-datasets?select=sales-of-shampoo-over-a-three-ye.csv
#Script created 12/2/2021
#Alyssa Venn

#Libraries
#library(Rnssp)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(qcc)
library(roll)
library(zoo)
library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)
library(tsfgrnn)
library(forecast)
library(fable)

#Data import
#Data has 2 variables: Date and Daily.minimum.temperatures, which I will rename as Temp
#Data is already grouped by date but normally that would have to be done
df <- read.csv("~/Portfolio/Pattern Recognition Alert/daily-minimum-temperatures-in-me.csv")

names(df) <- c("Date", "Temp")

#--------------------------------------------------------------------
#Methodology from the Rnssp package
#--------------------------------------------------------------------
#Has 2 methods: alert_ewma and alert_mar

#Date needs to be properly formatted
df$Date <- mdy(df$Date)

#So does temp
df$Temp <- as.numeric(df$Temp)

#Remove NA values
df <- na.omit(df)

#Using alert_ewma method
df_ewma <- alert_ewma(df, t=Date, y=Temp)


#Visualize
df_ewma %>%
  ggplot(aes(x = t, y = y)) +
  geom_line(color = "blue") +
  geom_point(data = subset(df_ewma, alert == "red"), color = "red") +
  geom_point(data = subset(df_ewma, alert == "yellow"), color = "yellow") +
  theme_bw() +
  labs(x = "Date",
       y = "Temperature")

#Using alert_mar method
df_mar <- alert_mar(df, t=Date, y =Temp)

#Visualize
df_mar %>%
  ggplot(aes(x = t, y = y)) +
  geom_line(color = "blue") +
  geom_point(data = subset(df_mar, alert == "red"), color = "red") +
  geom_point(data = subset(df_mar, alert == "yellow"), color = "yellow") +
  theme_bw() +
  labs(x = "Date",
       y = "Temperature")

#For both, blue = fine, yellow = p-value is >= 0.01 and < 0.05, red is < 0.01


#--------------------------------------------------------------------
#CUSUM method
#Jamison recommended manually creating the cusum formula rather than
#relying on the packages within R, so I will be trying that using
#the "roll" package
#--------------------------------------------------------------------

#Computing the rolling average
temp_mean <- roll_mean(df$Temp, width = 30)

#Computing the rolling Std Dev
temp_SD <- roll_sd(df$Temp, width = 30)

#Combining the rolling average with the original dataset
df_cusum <- cbind(df, temp_mean)

df_cusum <- cbind(df_cusum, temp_SD)

#Online it talks about 3 stddev as being a good cutoff, so I'll use that for testing
df_cusum$LB <- df_cusum$temp_mean - (df_cusum$temp_SD * 3)

df_cusum$UB <- df_cusum$temp_mean + (df_cusum$temp_SD * 3)

#OOB stands for out of bounds
df_cusum$OOB <- ifelse(df_cusum$Temp > df_cusum$UB | df_cusum$Temp < df_cusum$LB, 1, 0)

table(df_cusum$OOB)

#Visualize
df_cusum %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y = Temp), color = "blue") +
  geom_line(aes(y = temp_mean), color = "black", linetype = "twodash") +
  geom_line(aes(y = UB), color = "darkred", linetype = "dotted") +
  geom_line(aes(y = LB), color = "darkred", linetype = "dotted") +
  geom_point(aes(x = Date, y = Temp), data = subset(df_cusum, OOB == 1), color = "red")

#Shrinking the dataset so it can be seen better
df_cusum_small <- subset(df_cusum, Date > "1990-01-01")

save(df_cusum_small, file = "~/Portfolio/portfolio/df_cusum_small.rda")

#Visualize
df_cusum_small %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y = Temp), color = "blue") +
  geom_line(aes(y = temp_mean), color = "black", linetype = "twodash") +
  geom_line(aes(y = UB), color = "darkred", linetype = "dotted") +
  geom_line(aes(y = LB), color = "darkred", linetype = "dotted") +
  geom_point(aes(x = Date, y = Temp), data = subset(df_cusum_small, OOB == 1), color = "red") +
  ggtitle("Anomaly Detection Based on Rolling Average") 
  

#Okay that works! That's just a basic "if outside of the lines" thing, but what if I want different patterns?
#Maybe one color for positive, one color for negative, and one color of consistently negative?
#That won't work well with how it's set up now, they're so rare, so I'll reduce the LB and UB differences to 1
df_cusum$LB <- df_cusum$temp_mean - (df_cusum$temp_SD * 2)

df_cusum$UB <- df_cusum$temp_mean + (df_cusum$temp_SD * 2)

#Yellow = a short negative change, green = a positive change, red = negative change for 4 or more days
df_cusum$OOBpos <- ifelse(df_cusum$Temp > df_cusum$UB, 1, 0)

df_cusum$OOBneg <- ifelse(df_cusum$Temp < df_cusum$LB, 1, 0)

df_cusum$OOBcont <- rollapply(df_cusum$OOBneg, width = 5, FUN = sum, fill = NA)

#Where OOBcont is >= 4, it means that the previous 3 days (4 total) were also negative
df_cusum$OOBdesc <- ifelse(df_cusum$OOBcont >= 4, "red", 
                           ifelse(df_cusum$OOBneg == 1, "yellow", 
                                  ifelse(df_cusum$OOBpos == 1, "green", NA)))

#Visualize
df_cusum %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y = Temp), color = "blue") +
  geom_line(aes(y = temp_mean), color = "black", linetype = "twodash") +
  geom_line(aes(y = UB), color = "darkred", linetype = "dotted") +
  geom_line(aes(y = LB), color = "darkred", linetype = "dotted") +
  geom_point(aes(x = Date, y = Temp), data = subset(df_cusum, OOBdesc == "red"), color = "red") +
  geom_point(aes(x = Date, y = Temp), data = subset(df_cusum, OOBdesc == "yellow"), color = "yellow") +
  geom_point(aes(x = Date, y = Temp), data = subset(df_cusum, OOBdesc == "green"), color = "green") 

#--------------------------------------------------------------------
#Anomalize
#--------------------------------------------------------------------

library(anomalize)

#To use this, data must be a tibble of only the X and Y variables
df <- as_tibble(df)

#Frequency and trend can be adjusted manually. As can alpha (by default 0.05) and max_anoms (0.2)
df_anomalized <- df %>%
  time_decompose(Temp, 
                 frequency = "auto", 
                 trend = "auto",
                 merge = TRUE) %>%
  anomalize(remainder, 
            alpha = 0.05,
            max_anoms = 0.2) %>%
  time_recompose()


df_anomalized %>% plot_anomalies(ncol = 3, alpha_dots = 0.75)

#Testing different parameters
#This should be stricter
df_anomalized <- df %>%
  time_decompose(Temp, 
                 frequency = "auto", 
                 trend = "auto", 
                 merge = TRUE) %>%
  anomalize(remainder, 
            alpha = 0.1,
            max_anoms = 0.2,) %>%
  time_recompose()


df_anomalized %>% plot_anomalies(ncol = 3, alpha_dots = 0.75)
