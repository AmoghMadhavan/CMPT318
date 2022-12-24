## ------------------ ----------------------------------------------------------------------------------------------------
library(dplyr)
library(lubridate)
library(ggfortify)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(depmixS4)
#### PREAMBLE : ## Clearing mem buffers ####
cat("0\14") # Clear Console
rm(list = ls(all.names = TRUE)) # clear all
gc()


## ----------------------------------------------------------------------------------------------------------------------
# Load data
getwd()
setwd("/Users/amoghmadhavan/Desktop/CMPT_318/TermProject")
data <- read.table("TermProjectData.txt", header = TRUE, sep = ",")

# Delete obs with na values
# May look later to see if these are systematic or not
data <- na.omit(data)

# Convert each date to a day of the week for easy partitioning later
days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
data$Weekday = days[wday(dmy(data$Date))]


# Scale each response variable
data$Global_active_power <- (data$Global_active_power - mean(data$Global_active_power))/sd(data$Global_active_power)
data$Global_reactive_power <- (data$Global_reactive_power - mean(data$Global_reactive_power))/sd(data$Global_reactive_power)
data$Voltage <- (data$Voltage - mean(data$Voltage))/sd(data$Voltage)
data$Global_intensity <- (data$Global_intensity - mean(data$Global_intensity))/sd(data$Global_intensity)
data$Sub_metering_1 <- (data$Sub_metering_1 - mean(data$Sub_metering_1))/sd(data$Sub_metering_1)
data$Sub_metering_2 <- (data$Sub_metering_2 - mean(data$Sub_metering_2))/sd(data$Sub_metering_2)
data$Sub_metering_3 <- (data$Sub_metering_3 - mean(data$Sub_metering_3))/sd(data$Sub_metering_3)


## ----------------------------------------------------------------------------------------------------------------------
# Calculate principal components
res.pca <- prcomp(data[3:9], scale=FALSE)

# Plot the data on pc1 and pc2

tmp_df <- cor(data[3:9])
corrplot(cor(data[3:9]), method = 'color', is.corr=FALSE)


## ----------------------------------------------------------------------------------------------------------------------
eig <- get_eig(res.pca)

fviz_pca_var(res.pca, col.var='black')

var <- get_pca_var(res.pca)

corrplot(var$cos2, is.corr=FALSE)
#res.pca.eigen <- eigen(cov(res.pca))
res.pca.ve <- eig[,1]/sum(eig[,1])
plot(res.pca.ve,
     xlab = 'Principal Component',
     ylab='Proportion of Variance Explained',
     type = 'b',
     main = 'Scree Plot')


## ----------------------------------------------------------------------------------------------------------------------
data_summary <- data %>%
  group_by(Weekday, Time) %>%
  summarise(mean_global_intensity = mean(Global_intensity), mean_sub_metering_3 = mean(Sub_metering_3), mean_global_reactive_power = mean(Global_reactive_power))
data_summary <- data_summary %>% 
  arrange(factor(Weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), Time)

# Plots of average values over the week
par(mfrow=c(3,1))
plot(1:10080, data_summary$mean_global_intensity, type='l', xaxt='n', main="Global Intensity", xlab='', ylab='')
abline(v=seq(0,10080, by=(10080)/7), col='blue')
axis(1, at=seq(0,10080, by=(10080)/7), labels=c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun",""))
plot(1:10080, data_summary$mean_sub_metering_3, type='l', xaxt='n', main="Sub Metering 3", xlab='', ylab='')
abline(v=seq(0,10080, by=(10080)/7), col='blue')
axis(1, at=seq(0,10080, by=(10080)/7), labels=c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun",""))
plot(1:10080, data_summary$mean_global_reactive_power, type='l', xaxt='n', main="Global Reactive Power", xlab='', ylab='')
abline(v=seq(0,10080, by=(10080)/7), col='blue')
axis(1, at=seq(0,10080, by=(10080)/7), labels=c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun",""))
plot(1:10,1:10)

saturday_summary <- data_summary %>%
  filter(Weekday == 'Saturday')

#Plots of average values over Saturday
par(mfrow=c(3,1))
plot(1:1440, saturday_summary$mean_global_intensity, type='l', xaxt='n', main="Global Intensity", xlab='', ylab='')
axis(1, at=seq(0,1440, by=(240)), labels=c('00:00','04:00','08:00','12:00','16:00','20:00','23:59'))
plot(1:1440, saturday_summary$mean_sub_metering_3, type='l', xaxt='n', main="Sub Metering 3", xlab='', ylab='')
axis(1, at=seq(0,1440, by=(240)), labels=c('00:00','04:00','08:00','12:00','16:00','20:00','23:59'))
plot(1:1440, saturday_summary$mean_global_reactive_power, type='l', xaxt='n', main="Global Reactive Power", xlab='', ylab='')
axis(1, at=seq(0,1440, by=(240)), labels=c('00:00','04:00','08:00','12:00','16:00','20:00','23:59'))
plot(1:10,1:10)

time_window <- data[data$Time >= "18:00:00" & data$Time <= "23:59:00" & data$Weekday == 'Saturday',]
time_window <- time_window %>%
  arrange(Date, Time)

counts <- time_window %>%
  group_by(Date) %>%
  count()



# Train hmm using time_window data already split.

filtered_data <- subset(time_window, select=c("Global_intensity", "Global_reactive_power", "Sub_metering_3"))
train_data <- filtered_data[1:46800,]
test_data <- filtered_data[46801:54960,]

nrow(test_data[1])

state_vals <- c(4,6,8,10,12,14,16,18,20,22,24)
log_likelihood <- rep(0, times = length(state_vals))
test_log_likelihood <- rep(0, times = length(state_vals))
BICs <- rep(0, times = length(state_vals))

# Optimal model states we've chosen
chosen_states = 14

# Install.packages("depmixS4")
library(depmixS4)

for (val in state_vals) {
  ntimes195 <- rep(240, times = 195)
  model <- depmix(response = list(train_data$Global_intensity ~ 1, train_data$Global_reactive_power ~ 1, train_data$Sub_metering_3 ~ 1), data = train_data, nstates = val, ntimes = ntimes195, family = list(gaussian(), gaussian(), gaussian()))
  fit_model <- fit(model)
  #extract log-likelihood and BIC
  log_likelihood[(val - 2)/2] <- logLik(fit_model)
  BICs[(val - 2)/2] <- BIC(fit_model)
  
  summary(fit_model)
  
  # Testing on training data
  ntimes34 <- rep(240, times = 34)
  test_model <- depmix(response = list(test_data$Global_intensity ~ 1, test_data$Global_reactive_power ~ 1, test_data$Sub_metering_3 ~ 1), data = test_data, nstates = val, ntimes = ntimes34, family = list(gaussian(), gaussian(), gaussian()))
  test_model <- setpars(test_model,getpars(fit_model))
  fb_test <- forwardbackward(test_model)
  test_log_likelihood[(val - 2)/2] <- fb_test$logLike
  
  # Chosen model
  #if (val == chosen_states) {
   #selected_model_params <- getpars(fit_model)
  #}
}
print(log_likelihood)
print(test_log_likelihood)
print(BICs)
scaled_log_likelihood <- scale(log_likelihood)
scaled_BICs <- scale(BICs)
scaled_test_log_likelihood <- scale(test_log_likelihood)
print(scaled_log_likelihood)
print(scaled_test_log_likelihood)
print(scaled_BICs)

plot(state_vals, scaled_log_likelihood, type="b", col="blue", lwd=2, xlab="nstates", ylab="Log-Likelihood, Test Log-Likelihood, and BIC")
lines(state_vals, scaled_BICs, type="b", col="red", lwd=2)
lines(state_vals, scaled_test_log_likelihood, type="b", col="green", lwd=2)
legend(3,1,c("Log-Likelihood","BIC", "Test Log-Likelihoods"), lwd=c(2,2), col=c("blue","red","green"), y.intersp=1, xjust=-1, yjust = 3.5)

## ----------------------------------------------------------------------------------------------------------------------

# Load data with anomalies
dataset_1 <- read.table("DataWithAnomalies1.txt", header = TRUE, sep = ",")
dataset_2 <- read.table("DataWithAnomalies2.txt", header = TRUE, sep = ",")
dataset_3 <- read.table("DataWithAnomalies3.txt", header = TRUE, sep = ",")

data_list = list(dataset_1, dataset_2, dataset_3)


for (i in 1:length(data_list)) {
  cur_data = na.omit(data_list[[i]])
  
  cur_data$Weekday = days[wday(dmy(cur_data$Date))]
  
  # Scale each response variable
  cur_data$Global_active_power <- (cur_data$Global_active_power - mean(cur_data$Global_active_power))/sd(cur_data$Global_active_power)
  cur_data$Global_reactive_power <- (cur_data$Global_reactive_power - mean(cur_data$Global_reactive_power))/sd(cur_data$Global_reactive_power)
  cur_data$Voltage <- (cur_data$Voltage - mean(cur_data$Voltage))/sd(cur_data$Voltage)
  cur_data$Global_intensity <- (cur_data$Global_intensity - mean(cur_data$Global_intensity))/sd(cur_data$Global_intensity)
  cur_data$Sub_metering_1 <- (cur_data$Sub_metering_1 - mean(cur_data$Sub_metering_1))/sd(cur_data$Sub_metering_1)
  cur_data$Sub_metering_2 <- (cur_data$Sub_metering_2 - mean(cur_data$Sub_metering_2))/sd(cur_data$Sub_metering_2)
  cur_data$Sub_metering_3 <- (cur_data$Sub_metering_3 - mean(cur_data$Sub_metering_3))/sd(cur_data$Sub_metering_3)
  
  # Separate into time windows
  cur_data_time_window <- cur_data[cur_data$Time >= "18:00:00" & cur_data$Time <= "23:59:00" & cur_data$Weekday == 'Saturday',]
  cur_data_time_window <- cur_data_time_window %>%
    arrange(Date, Time)
  
  filtered_cur_data <- subset(cur_data_time_window, select=c("Global_intensity", "Global_active_power", "Sub_metering_3"))
  filtered_cur_data <- filtered_cur_data[1:54960,]
  
  if (i == 1) {
    filtered_dataset_1 = filtered_cur_data
  } else if (i == 2) {
    filtered_dataset_2 = filtered_cur_data
  } else {
    filtered_dataset_3 = filtered_cur_data
  }
}

ntimes229 <- rep(240, times = 229)

# Models
data_1_model <- depmix(response = list(filtered_dataset_1$Global_intensity ~ 1, filtered_dataset_1$Global_active_power ~ 1, filtered_dataset_1$Sub_metering_3 ~ 1), data = filtered_dataset_1, nstates = chosen_states, ntimes = ntimes229, family = list(gaussian(), gaussian(), gaussian()))
data_1_model <- setpars(data_1_model, selected_model_params)
fb_test_1 <- forwardbackward(data_1_model)
data_1_log_likelihood <- fb_test_1$logLike

data_2_model <- depmix(response = list(filtered_dataset_2$Global_intensity ~ 1, filtered_dataset_2$Global_active_power ~ 1, filtered_dataset_2$Sub_metering_3 ~ 1), data = filtered_dataset_2, nstates = chosen_states, ntimes = ntimes229, family = list(gaussian(), gaussian(), gaussian()))
data_2_model <- setpars(data_2_model, selected_model_params)
fb_test_2 <- forwardbackward(data_2_model)
data_2_log_likelihood <- fb_test_2$logLike

data_3_model <- depmix(response = list(filtered_dataset_3$Global_intensity ~ 1, filtered_dataset_3$Global_active_power ~ 1, filtered_dataset_3$Sub_metering_3 ~ 1), data = filtered_dataset_3, nstates = chosen_states, ntimes = ntimes229, family = list(gaussian(), gaussian(), gaussian()))
data_3_model <- setpars(data_3_model, selected_model_params)
fb_test_3 <- forwardbackward(data_3_model)
data_3_log_likelihood <- fb_test_3$logLike

# Show data
print(data_1_log_likelihood)
print(data_2_log_likelihood)
print(data_3_log_likelihood)

