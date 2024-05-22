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
jetson_data_sent$Datetime = lubridate::as_datetime(jetson_data_sent$timestamp)
jetson_data_sent$rounded_datetime <- round_date(jetson_data_sent$Datetime, unit = "minute")
jetson_data_sent$total_particles_jetson = jetson_data_sent$copepodCount + jetson_data_sent$nonCopepodCount + jetson_data_sent$detritusCount



hits_directory <- "C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/rapid_paper/data/hitsmisses"
file_list <- list.files(path = hits_directory, pattern = "*.csv", full.names = TRUE)
imager_hits_misses <- data.frame()

for (file in file_list) {
  temp_data <- read.csv(file)
  imager_hits_misses <- rbind(imager_hits_misses, temp_data)
}

imager_hits_misses$Time <- sprintf("%04d", imager_hits_misses$Tenbin + imager_hits_misses$Minute)
imager_hits_misses$Datetime <- ymd_hm(paste(imager_hits_misses$Date, imager_hits_misses$Time))
imager_hits_misses$total_particles_imager = imager_hits_misses$Hits + imager_hits_misses$Misses

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
