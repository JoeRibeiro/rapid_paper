# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# Define the directory containing the files
data_directory <- "C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/rapid_paper/data/hitsmisses"

# Get the list of all CSV files in the directory
file_list <- list.files(path = data_directory, pattern = "*.csv", full.names = TRUE)

# Initialize an empty dataframe
combined_data <- data.frame()

# Loop through each file and append the data
for (file in file_list) {
  temp_data <- read.csv(file)
  combined_data <- rbind(combined_data, temp_data)
}

# Combine 'Tenbin' and 'Minute' to create a time in the 24-hour format
combined_data$Time <- sprintf("%04d", combined_data$Tenbin + combined_data$Minute)

# Convert the combined 'Date' and 'Time' columns to a proper datetime format
combined_data$Datetime <- ymd_hm(paste(combined_data$Date, combined_data$Time))


# Plot the raw data for Hits and Misses without a log scale on the y-axis
ggplot(combined_data, aes(x = Datetime)) +
  geom_line(aes(y = Hits, color = "Hits")) +
  geom_line(aes(y = Misses, color = "Misses")) +
  labs(title = "Raw Data: Time Series of Hits and Misses",
       x = "Datetime",
       y = "Count (Log Scale)") +
  scale_color_manual(values = c("Hits" = "blue", "Misses" = "red")) +
  theme_minimal()


# Plot the raw data for Hits and Misses with a log scale on the y-axis
ggplot(combined_data, aes(x = Datetime)) +
  geom_line(aes(y = Hits, color = "Hits")) +
  geom_line(aes(y = Misses, color = "Misses")) +
  scale_y_log10() +
  labs(title = "Raw Data: Time Series of Hits and Misses (Log Scale)",
       x = "Datetime",
       y = "Count (Log Scale)") +
  scale_color_manual(values = c("Hits" = "blue", "Misses" = "red")) +
  theme_minimal()




