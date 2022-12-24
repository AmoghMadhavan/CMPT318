############################################################
# CMPT 318 | Fall 2022 | 
# Assignment 3
# Group 11
# Amogh Madhavan, Mike Nguyen, Arman Elgudzhyan
# 12 NOV 2022 
############################################################

#### PREAMBLE : ## Clearing mem buffers ####
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE)) # Clear all

# Libraries
library(lubridate)
library(dplyr)
library(chron)
library(TTR)
library(depmixS4)
library(ggplot2)
library(scales)
require(data.table)

# Load data
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
df <- read.table("Group_Assignment_2_Dataset.txt", header = TRUE, sep = ",")

# English labels indexed
days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

# Map numeric value to our labels
df$Weekday <- days[wday(dmy(df$Date))]

# Change hour to an integer value
df$Hour <- as.numeric(sapply(strsplit(as.character(df$Time), ":"), "[", 1))

# Scaled value of global active power
df$Scaled_Value <- scale(df$Global_active_power)

# Get data from Fridays
Friday <- df[df$Weekday == "Friday", ]
Friday$TimeOfDay <- as.POSIXct(Friday$Time, format = "%H:%M:%S")

# Plot all data by hour
ggplot() + geom_point(data = Friday, aes(TimeOfDay, Scaled_Value), size = 0.5, color = "#0072B2") +
  scale_x_datetime(breaks = "2 hours", labels = date_format(format = "%H:%M", tz = ""))


# Filter data from between 5PM to 8PM
Friday5to8 <- Friday[Friday$Hour >= 17 & Friday$Hour <= 20,]
# Plot only from 5PM to 8PM
ggplot() + geom_point(data = Friday5to8, aes(TimeOfDay, Scaled_Value), size = 0.75, color = "#0072B2") +
  scale_x_datetime(breaks = breaks_width("30 min"), labels = date_format(format = "%H:%M", tz = ""))


# Each time frame has 180 observations
Friday5to8 %>%
  group_by(Date) %>%
  tally()

# Sort dataframe by datetime
Friday5to8$TimeOfDay <- as.POSIXct(paste(Friday5to8$Date, Friday5to8$Time), format = "%d/%m/%Y %H:%M:%S")
Friday5to8 <- arrange(Friday5to8, TimeOfDay)


# States that will be explored to train the HMM
states <- c(3:16);
ln <- vector();
bIC <- vector();

for (i in states)
{
  model <- depmix(response = Global_active_power ~ 1,
                  data = Friday5to8,
                  nstates = i,
                  ntimes = rep(240, 52));
  fitModel <- fit(model);
  ln <- append(ln, logLik(fitModel))
  bIC <- append(bIC, BIC(fitModel));
}

# Graph which shows the log likelihood as the number of states explored increases
Likelihood_Graph <- data.frame(states, ln, bIC);
ggplot(data = Likelihood_Graph) +
  geom_line(aes(states, ln, color = "Log Likelihood")) +
  geom_line(aes(states, bIC, color = "BIC"))
