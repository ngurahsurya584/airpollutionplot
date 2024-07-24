library(openair)
library(dplyr)
library(lubridate)

df <-read.csv("data.csv", header=TRUE)
colnames(df)[1] <- "date"
colnames(df)[3] <- "wd"
colnames(df)[4] <- "ws"

df$date <- as.POSIXct(df$date)

#Time plot
timePlot(df, pollutant = c("BC", "PM2.5"), y.relation = "free")

#If we want to fraw time plot of any month then use this code 
data(df)
timePlot(selectByDate(df, year = 2021, month = "aug"),
         pollutant = c("BC", "BCff", "BCbb", "PM2.5", "ws"),
         y.relation = "free")
timePlot(head(df), pollutant = c("BC", "BCff", "BCbb", "PM2.5", "ws"),
         windflow = list(scale = 0.1, lwd = 2, col = "pink"),
         lwd = 3, group = FALSE,
         ylab = "concentration (ug/m3)")

#Time variation
timeVariation(df, pollutant = "BC", ylab = "BCff (ng/m3)")
timeVariation(df, pollutant = "BC")
timeVariation(df, pollutant = c("BC", "BCff", "BCbb"), ylab = "PM2.5 (ng/m3)")
timeVariation(df, pollutant = "BCff", statistic = "mean",
              col = "firebrick")
myOutput <- timeVariation(df, pollutant = c("BC", "BCff", "BCbb"), ylab = "PM2.5 (ng/m3)", local.tz = "Asia/Taipei", 
                          statistic = "mean")
day.hour <- myOutput$data$day.hour
head(day.hour)
hour <- myOutput$data$hour 
head(hour)
plot(myOutput, subset = "hour", ylab = "PM2.5 (ng/m3)")
month <- myOutput$data$month
head(month)
plot(myOutput, subset = "month")