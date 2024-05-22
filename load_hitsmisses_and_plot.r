library(jsonlite)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)

log_directory <- "C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/rapid_paper/data/jetsonlog"
files <- list.files(log_directory, full.names = TRUE)
df_list <- list()

for (file in files) {
  content <- readLines(file, warn = FALSE)
  content <- paste(content, collapse = "\n")
  json_strings <- str_extract_all(content, "\\{[^\\}]*\\}")[[1]]
  json_list <- lapply(json_strings, fromJSON)
  if (length(json_list) > 0) {
    df <- bind_rows(json_list)
    df_list <- append(df_list, list(df))
  }
}

jetson_data_sent <- bind_rows(df_list)
jetson_data_sent$Datetime <- lubridate::as_datetime(jetson_data_sent$timestamp)
jetson_data_sent$rounded_datetime <- round_date(jetson_data_sent$Datetime, unit = "minute")
jetson_data_sent$rounded_datetime_5 <- floor_date(jetson_data_sent$Datetime, unit = "5 minutes")  # Round to nearest 5 minutes
jetson_data_sent$total_particles_jetson <- jetson_data_sent$copepodCount + jetson_data_sent$nonCopepodCount + jetson_data_sent$detritusCount

hits_directory <- "C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/rapid_paper/data/hitsmisses"
file_list <- list.files(path = hits_directory, pattern = "*.csv", full.names = TRUE)
imager_hits_misses <- data.frame()

for (file in file_list) {
  temp_data <- read.csv(file)
  imager_hits_misses <- rbind(imager_hits_misses, temp_data)
}

imager_hits_misses$Time <- sprintf("%04d", imager_hits_misses$Tenbin + imager_hits_misses$Minute)
imager_hits_misses$Datetime <- ymd_hm(paste(imager_hits_misses$Date, imager_hits_misses$Time))
imager_hits_misses$rounded_datetime <- round_date(imager_hits_misses$Datetime, unit = "minute")
imager_hits_misses$rounded_datetime_5 <- floor_date(imager_hits_misses$Datetime, unit = "5 minutes")  # Round to nearest 5 minutes
imager_hits_misses$total_particles_imager <- imager_hits_misses$Hits + imager_hits_misses$Misses

# Merge the two dataframes on rounded_datetime
merged_data <- left_join(jetson_data_sent, imager_hits_misses, by = "rounded_datetime_5")


# First look at data

ggplot(imager_hits_misses, aes(x = Datetime)) +
  geom_line(aes(y = Hits, color = "Hits")) +
  geom_line(aes(y = Misses, color = "Misses")) +
  labs(title = "Raw Data: Time Series of Hits and Misses",
       x = "Datetime",
       y = "Count") +
  scale_color_manual(values = c("Hits" = "blue", "Misses" = "red")) +
  theme_minimal()


# It seems log scale is needed

ggplot() +
  geom_line(data = imager_hits_misses, aes(x = Datetime, y = Hits, color = "Hits")) +
  geom_line(data = imager_hits_misses, aes(x = Datetime, y = total_particles_imager, color = "Total particles imager")) +
  geom_line(data = jetson_data_sent, aes(x = Datetime, y = total_particles_jetson, color = "Total particles jetson")) +
  scale_y_log10() +
  labs(title = "Combined Time Series",
       x = "Datetime",
       y = "Count (Log Scale)",
       color = "Legend") +
  scale_color_manual(values = c("Hits" = "blue", "Total particles imager" = "red", "Total particles jetson" = "green")) +
  theme_minimal()


# Why do these look so weird?
ggplot(merged_data, aes(x = total_particles_jetson, y = Hits)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  labs(title = "Scatter Plot of Hits vs Total Particles (Jetson)",
       x = "Total Particles (Jetson)",
       y = "Hits")

ggplot(merged_data, aes(x = total_particles_imager, y = Hits)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  labs(title = "Scatter Plot of Hits vs Total Particles (Jetson)",
       x = "total_particles_imager",
       y = "Hits")
