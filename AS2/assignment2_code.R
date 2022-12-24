library(lubridate)
library(dplyr)
library(chron)
library(TTR)
library(zoo)
library(forecast)

data <- read.csv("Group_Assignment_2_Dataset.txt", header = TRUE)
data <- na.omit(data)

days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
data$Weekday_Num <- wday(dmy(data$Date))
data$Weekday_Num <- data$Weekday_Num - 1
data$Weekday_Num[data$Weekday_Num == 0] <- 7
data$Weekday <- days[data$Weekday_Num]

data$Date <- as.Date(data$Date, "%d/%m/%Y")
data$Cut <- cut(data$Date, breaks="weeks", start.on.monday = TRUE)

incomplete <- data %>%
  group_by(Cut) %>%
  summarise(count = n_distinct(Weekday)) %>%
  filter(count < 7) %>%
  select(Cut)

data <- data %>%
  filter(!(Cut %in% incomplete$Cut)) %>%
  arrange(Cut, Date, Time)

uni_cuts <- unique(data[c("Cut")])
uni_cuts$Week <- 1:nrow(uni_cuts)
data <- merge(data, uni_cuts)
data <- data %>%
  group_by(Week) %>%
  mutate(SMA = SMA(Global_intensity, n=7))

arb_date_time <- function(data){
  for(i in 1:7){
    data$Datetime[data$Weekday_Num == i] <- as.POSIXct(paste(toString(Sys.Date() + i - 1), data$Time), format="%Y-%m-%d %H:%M:%S")
  }
  return(data)
}

average_smoothened_week <- data %>%
  select(Weekday, Weekday_Num, Time, Global_intensity) %>%
  group_by(Weekday, Weekday_Num, Time) %>%
  summarise(Avg_Global_intensity = mean(Global_intensity)) %>%
  arrange(Weekday_Num, Time)


average_smoothened_week <- arb_date_time(average_smoothened_week)
data <- merge(data, average_smoothened_week, by=c("Weekday","Time")) %>%
  arrange(Week, Date, Time)
data <- na.omit(data)
data$Squared_error <- (data$SMA - data$Avg_Global_intensity)^2


summary <- data %>% 
  group_by(Week) %>%
  summarise(Sum_squared_error = sum(Squared_error), n = n()) %>%
  arrange(Sum_squared_error)

least_anamolous <- data %>%
  filter(Week == 29) %>%
  arrange(Datetime)

most_anamolous <- data %>%
  filter(Week == 51) %>%
  arrange(Datetime)

start = as.numeric(average_smoothened_week$Datetime)[1]
end = as.numeric(average_smoothened_week$Datetime)[nrow(average_smoothened_week)]

plot.new
plot(average_smoothened_week$Datetime, average_smoothened_week$Avg_Global_intensity, type="l", xaxt='n', ylab = "Global Intensity", xlab = "Day of the Week", main="Smoothened Global Intensity Over a Week")
points(most_anamolous$Datetime, most_anamolous$SMA, col='red', cex=0.15)
points(least_anamolous$Datetime, least_anamolous$SMA, col='green', cex=0.15)
axis(1, at=seq(start,end, by=(end-start)/7), labels=c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun",""))
