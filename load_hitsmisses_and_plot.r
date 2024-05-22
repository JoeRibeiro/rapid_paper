library(jsonlite)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(fs)  # For directory creation

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

imager_hits_misses$Time <- sprintf("%04d", imager_hits_misses$Tenbin + 100 + imager_hits_misses$Minute) # The + 100 corrects the data from UTC to BST, which the jetson data are in
imager_hits_misses$Datetime <- ymd_hm(paste(imager_hits_misses$Date, imager_hits_misses$Time))
imager_hits_misses$rounded_datetime <- round_date(imager_hits_misses$Datetime, unit = "minute")
imager_hits_misses$rounded_datetime_5 <- floor_date(imager_hits_misses$Datetime, unit = "5 minutes")  # Round to nearest 5 minutes
imager_hits_misses$total_particles_imager <- imager_hits_misses$Hits + imager_hits_misses$Misses


# Aggregate jetson_data_sent and imager_hits_misses by rounded_datetime
agg_jetson <- jetson_data_sent %>%
  group_by(rounded_datetime_5) %>%
  summarise(total_particles_jetson = sum(total_particles_jetson))

agg_imager <- imager_hits_misses %>%
  group_by(rounded_datetime_5) %>%
  summarise(total_particles_imager = sum(total_particles_imager),
            Hits = sum(Hits))

# Merge the aggregated dataframes on rounded_datetime
merged_data <- left_join(agg_jetson, agg_imager, by = "rounded_datetime_5")

# Directory creation
figures_directory <- "C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/rapid_paper/figures"
dir_create(figures_directory)

# First look at data
plot1 <- ggplot(imager_hits_misses, aes(x = Datetime)) +
  geom_line(aes(y = Hits, color = "Hits")) +
  geom_line(aes(y = Misses, color = "Misses")) +
  labs(title = "Raw Data: Time Series of Hits and Misses",
       x = "Datetime",
       y = "Count") +
  scale_color_manual(values = c("Hits" = "blue", "Misses" = "red")) +
  theme_minimal()

ggsave(file.path(figures_directory, "hits_misses_raw_data.png"), plot1, width = 10, height = 8, dpi = 500,bg = "white")

# It seems log scale is needed
plot2 <- ggplot() +
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
ggsave(file.path(figures_directory, "combined_time_series.png"), plot2, width = 10, height = 8, dpi = 500,bg = "white")

# Plot scatter graph of Hits against total_particles_jetson 
# Iterate over y-axis limits
xylimits <- c(max(merged_data$Hits, na.rm = TRUE), 
              max(merged_data$Hits, na.rm = TRUE)/2, 
              max(merged_data$Hits, na.rm = TRUE)/4,
              max(merged_data$Hits, na.rm = TRUE)/10)
for (limit in xylimits) {
  file_name <- paste0("scatter_jetson_", format(limit, scientific = FALSE), ".png")
  plot <- ggplot(merged_data, aes(x = Hits , y = total_particles_jetson)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(title = "Scatter Plot of Hits vs Total Particles (Jetson)",
         x = "Photographed Particles (Imager)",
         y = "Classified Particles (Jetson)") +
    xlim(0.0000001, limit) +
    ylim(0.0000001, limit)
  ggsave(file.path(figures_directory, file_name), plot, width = 10, height = 8, dpi = 500, bg = "white")
}



y_limits_imager <- c(max(imager_hits_misses$total_particles_imager, na.rm = TRUE),
                     max(imager_hits_misses$total_particles_imager, na.rm = TRUE) / 2,
                     max(imager_hits_misses$total_particles_imager, na.rm = TRUE) / 4,
                     max(imager_hits_misses$total_particles_imager, na.rm = TRUE) / 10,
                     max(imager_hits_misses$total_particles_imager, na.rm = TRUE) / 20)
for (limit in y_limits_imager) {
  # Reflect the y-axis option in the file name saved
  file_name_imager <- paste0("scatter_imager_", format(limit, scientific = FALSE), ".png")
  # Plot scatter graph of Hits against total_particles_imager
  plot_imager <- ggplot(imager_hits_misses, aes(x = total_particles_imager, y = Hits)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(title = "Scatter Plot of Hits (Imager) vs Total Particles (Imager)",
         x = "Total Particles (Imager)",
         y = "Photographed Particles (Imager)") +
    xlim(c(0.0000001, limit)) +
    ylim(c(0.0000001, limit))
  ggsave(file.path(figures_directory, file_name_imager), plot_imager, width = 10, height = 8, dpi = 500, bg = "white")
}




plot3 <- ggplot(merged_data, aes(x= log10(Hits) , y = log10(total_particles_jetson))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Scatter Plot of Hits vs Total Particles (Jetson)",
       x =  "log(Photographed Particles (Imager))",
       y ="log(Classified Particles (Jetson))")+
  xlim(2,max(log10(merged_data$Hits)+1,na.rm = T))+
  ylim(2,max(log10(merged_data$Hits)+1,na.rm = T))

ggsave(file.path(figures_directory, "scatter_jetson.png"), plot3, width = 10, height = 8, dpi = 500,bg = "white")

# Plot scatter graph of Hits against total_particles_imager
plot4 <- ggplot(imager_hits_misses, aes(x= log10(total_particles_imager) , y = log10(Hits))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Scatter Plot of Hits (imager) vs Total Particles (imager)",
       x = "Total Particles (Imager)",
       y = "Photographed Particles (Imager)")+
  xlim(2,max(log10(imager_hits_misses$total_particles_imager)+1,na.rm = T))+
  ylim(2,max(log10(imager_hits_misses$total_particles_imager)+1,na.rm = T))

ggsave(file.path(figures_directory, "scatter_imager.png"), plot4, width = 10, height = 8, dpi = 500,bg = "white")

