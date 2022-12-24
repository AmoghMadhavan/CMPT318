############################################################
# CMPT 318| Fall 2022 | 
# Assignment 1
# Amogh
# 08 OCT 2022 
############################################################

#### PREAMBLE : ## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all

gc()

set.seed(42) # Set a seed to ensure repeatable random samples

# libraries
require(data.table)
require(pastecs)
require(stargazer)
library(corrplot)

#LOAD DATA  
dt <- fread("/Users/amoghmadhavan/Desktop/CMPT_318/AS1/Group_Assignment_1_Dataset.txt")

View(dt)

weeker = as.POSIXlt(dt$Date, format = "%d/%m/%Y")
 
dt <- cbind(dt, weeker)


the_weeks = strftime(dt$weeker, format = "%W")

dt <- cbind(dt, the_weeks)


new_dt = dt[(dt$the_weeks == 11),]

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Active power
exp(mean(log(new_dt$Global_active_power)))

mean(new_dt$Global_active_power)

median(new_dt$Global_active_power)

getmode(new_dt$Global_active_power)

sd(new_dt$Global_active_power)

#reactive_power
exp(mean(log(new_dt$Global_reactive_power)))

mean(new_dt$Global_reactive_power)

median(new_dt$Global_reactive_power)

getmode(new_dt$Global_reactive_power)

sd(new_dt$Global_reactive_power)

#voltage
exp(mean(log(new_dt$Voltage)))

mean(new_dt$Voltage)

median(new_dt$Voltage)

getmode(new_dt$Voltage)

sd(new_dt$Voltage)

weekdaydata <- new_dt[(new_dt$weeker < "2007-03-16"),]
weenenddata <- new_dt[(new_dt$weeker > "2007-03-15"),]

#data splits for week day
weekdaydatadaytime <- weekdaydata[(weekdaydata$Time > "07:59:00" & weekdaydata$Time < "18:00:00")]
weekdaydatadaytimeearlymorning <- weekdaydata[(weekdaydata$Time < "08:00:00")]
weekdaydatalater <- weekdaydata[(weekdaydata$Time > "17:59:00")]
weekdaynight <- rbind(weekdaydatadaytimeearlymorning, weekdaydatalater)

#data splits for week end
weekenddatadaytime <- weenenddata[(weenenddata$Time > "07:59:00" & weenenddata$Time < "18:00:00")]
weekenddatadaytimeearlymorning <- weenenddata[(weenenddata$Time < "08:00:00")]
weekenddatalater <- weenenddata[(weenenddata$Time > "17:59:00")]
weekendnight <- rbind(weekenddatadaytimeearlymorning, weekenddatalater)

#max and min for week day day and night for GAP
max(weekdaydatadaytime$Global_active_power)
max(weekdaynight$Global_active_power)
min(weekdaydatadaytime$Global_active_power)
min(weekdaynight$Global_active_power)

#max and min for week day day and night for GRP
max(weekdaydatadaytime$Global_reactive_power)
max(weekdaynight$Global_reactive_power)
min(weekdaydatadaytime$Global_reactive_power)
min(weekdaynight$Global_reactive_power)

#max and min for week end day and night for GAP
max(weekenddatadaytime$Global_active_power)
max(weekendnight$Global_active_power)
min(weekenddatadaytime$Global_active_power)
min(weekendnight$Global_active_power)

#max and min for week end day and night for GRP
max(weekenddatadaytime$Global_reactive_power)
max(weekendnight$Global_reactive_power)
min(weekenddatadaytime$Global_reactive_power)
min(weekendnight$Global_reactive_power)


corelation <- cor(new_dt[,c('Global_active_power', 'Global_reactive_power', 'Voltage', 'Global_intensity', 'Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3')], method = c("pearson"))

corrplot(corelation, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 90)


#data sorted by time so all the 7:00 pm for all weekday together for example.
sorted_data_for_week_day <- weekdaydatadaytime[order(Time),]
sorted_data_for_week_night <- weekdaynight[order(Time),]

sorted_data_for_weekend_day <- weekenddatadaytime[order(Time),]
sorted_data_for_weekend_night <- weekendnight[order(Time),]

library(dplyr)

#averages per time data point for Global intensity
averages_gI_weekday_daytime <- group_by(sorted_data_for_week_day, Time) %>% summarize(m = mean(Global_intensity))
averages_gI_weekday_nightime <- group_by(sorted_data_for_week_night, Time) %>% summarize(m = mean(Global_intensity))
averages_gI_weekend_daytime <- group_by(sorted_data_for_weekend_day, Time) %>% summarize(m = mean(Global_intensity))
averages_gI_weekend_nighttime <- group_by(sorted_data_for_weekend_night, Time) %>% summarize(m = mean(Global_intensity))

new_TIME <- as.POSIXlt(averages_gI_weekday_daytime$Time, format = "%H:%M:%S")
new_TIME_two <- as.POSIXct(averages_gI_weekday_nightime$Time, format = "%H:%M:%S")
new_TIME_three <- as.POSIXct(averages_gI_weekend_daytime$Time, format = "%H:%M:%S")
new_TIME_four <- as.POSIXct(averages_gI_weekend_nighttime$Time, format = "%H:%M:%S")

averages_gI_weekday_daytime<- cbind(averages_gI_weekday_daytime, new_TIME)
averages_gI_weekday_nightime<- cbind(averages_gI_weekday_nightime, new_TIME_two)
averages_gI_weekend_daytime<- cbind(averages_gI_weekend_daytime, new_TIME_three)
averages_gI_weekend_nighttime<- cbind(averages_gI_weekend_nighttime, new_TIME_four)

TIME <- strftime(new_TIME, format = "%H.%M")
TIME_two <- strftime(new_TIME_two, format = "%H.%M")
TIME_three <- strftime(new_TIME_three, format = "%H.%M")
TIME_four <- strftime(new_TIME_four, format = "%H.%M")

averages_gI_weekday_daytime<- cbind(averages_gI_weekday_daytime, TIME)
averages_gI_weekday_nightime<- cbind(averages_gI_weekday_nightime, TIME_two)
averages_gI_weekend_daytime<- cbind(averages_gI_weekend_daytime, TIME_three)
averages_gI_weekend_nighttime<- cbind(averages_gI_weekend_nighttime, TIME_four)

averages_gI_weekday_daytime$TIME <- as.numeric(averages_gI_weekday_daytime$TIME)
averages_gI_weekday_nightime$TIME_two <- as.numeric(averages_gI_weekday_nightime$TIME_two)
averages_gI_weekend_daytime$TIME_three <- as.numeric(averages_gI_weekend_daytime$TIME_three)
averages_gI_weekend_nighttime$TIME_four <- as.numeric(averages_gI_weekend_nighttime$TIME_four)


averages_gI_weekday_daytime$LIN  <- predict(lm(m ~ TIME, averages_gI_weekday_daytime))
averages_gI_weekday_nightime$LIN <- predict(lm(m ~ TIME_two, averages_gI_weekday_nightime))
averages_gI_weekend_daytime$LIN  <- predict(lm(m ~ TIME_three, averages_gI_weekend_daytime))
averages_gI_weekend_nighttime$LIN<- predict(lm(m ~ TIME_four, averages_gI_weekend_nighttime))

averages_gI_weekday_daytime$POLY <- predict(lm(m ~ (poly(averages_gI_weekday_daytime$TIME, 3, raw=FALSE)), averages_gI_weekday_daytime))
averages_gI_weekday_nightime$POLY <- predict(lm(m ~ (poly(averages_gI_weekday_nightime$TIME_two, 3, raw=FALSE)), averages_gI_weekday_nightime))
averages_gI_weekend_daytime$POLY <- predict(lm(m ~ (poly(averages_gI_weekend_daytime$TIME_three, 3, raw=FALSE)), averages_gI_weekend_daytime)) 
averages_gI_weekend_nighttime$POLY <- predict(lm(m ~ (poly(averages_gI_weekend_nighttime$TIME_four, 3, raw=FALSE)), averages_gI_weekend_nighttime)) 


library(ggplot2)

ggplot() +
  geom_point(data = averages_gI_weekday_daytime, aes(TIME, m), size = 0.75) +
  geom_point(data = averages_gI_weekday_nightime, aes(TIME_two, m), size = 0.75) +
  geom_point(aes(x = TIME, y = LIN), data = averages_gI_weekday_daytime, color = "red") + 
  geom_point(aes(x = TIME_two, y = LIN), data = averages_gI_weekday_nightime, color = "red") + 
  geom_point(aes(x = TIME, y = POLY), data = averages_gI_weekday_daytime, color = "blue") + 
  geom_point(aes(x = TIME_two, y = POLY), data = averages_gI_weekday_nightime, color = "blue") + ggtitle("Global_intensity Behavior Weekday") +
  xlab("Time") + ylab("Average GI")

ggplot() +
  geom_point(data = averages_gI_weekend_daytime, aes(TIME_three, m), size = 0.75) +
  geom_point(data = averages_gI_weekend_nighttime, aes(TIME_four, m), size = 0.75) +
  geom_point(aes(x = TIME_three, y = POLY), data = averages_gI_weekend_daytime, color = "blue")+ 
  geom_point(aes(x = TIME_four, y = POLY), data = averages_gI_weekend_nighttime, color = "blue") + 
  geom_point(aes(x = TIME_three, y = LIN), data = averages_gI_weekend_daytime, color = "red") + 
  geom_point(aes(x = TIME_four, y = LIN), data = averages_gI_weekend_nighttime, color = "red") + ggtitle("Global_intensity Behavior Weekend") +
  xlab("Time") + ylab("Average GI")







