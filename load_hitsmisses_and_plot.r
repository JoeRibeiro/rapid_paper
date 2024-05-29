library(jsonlite)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(fs) 
library(tidyverse)

# Stations
ship_log <- read_csv("C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/rapid_paper/data/stations/CEND0824_start_end_times_and_positions.csv")
ship_log <- ship_log %>%  mutate(TimeStart = dmy_hm(TimeStart), TimeEnd = dmy_hm(TimeEnd))

# Jetson sent
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
jetson_data_sent$total_particles_jetson_sent <- jetson_data_sent$copepodCount + jetson_data_sent$nonCopepodCount + jetson_data_sent$detritusCount


# Dashboard received:
data <- read.csv("C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/rapid_paper/data/dashboard/latest_survey.csv")
dashboard <- data %>%  pivot_wider(names_from = AttributeName, values_from = Value)
dashboard$Datetime <- lubridate::as_datetime(dashboard$Timestamp)
dashboard$rounded_datetime <- round_date(dashboard$Datetime, unit = "minute")
dashboard$rounded_datetime_5 <- floor_date(dashboard$Datetime, unit = "5 minutes")  # Round to nearest 5 minutes
dashboard$total_particles_jetson_sent <- dashboard$copepodCount + dashboard$nonCopepodCount + dashboard$detritusCount
merged_sent_and_received=left_join(jetson_data_sent,dashboard, by = "rounded_datetime")
# Verify that the dashboard contains the data marked as sent in the logfile... yes (difference = 0)
stopifnot(max(merged_sent_and_received$total_particles_jetson_sent.x-merged_sent_and_received$total_particles_jetson_sent.y)==0)

# Jetson seen
jetson_data_seen <- data.frame(
  datetime = as.POSIXct(character()),
  Copepod = numeric(),
  Noncopepod = numeric(),
  Detritus = numeric(),
  objectlength = numeric(),
  thresholdArea_otsu = numeric(),
  thresholdArea_standard = numeric(),
  equispherdiameter_otsu = numeric(),
  equispherdiameter_standard = numeric()
)

log_files <- list.files(path = log_directory, pattern = "*.log", full.names = TRUE)

# Process each log file
for (file in log_files) {
  # Read lines from the log file
  lines <- readLines(file)
  
  # Extract relevant information from each line
  for (line in lines) {
    if (grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2},\\d{3} - INFO - (Copepod|objectlength)", line)) {
      # Extract datetime
      datetime <- as.POSIXct(strptime(substr(line, 1, 19), "%Y-%m-%d %H:%M:%S"))
      
      # Extract variables
      copepod <- as.numeric(sub('.*Copepod : ([0-9]+).*', '\\1', line))
      noncopepod <- as.numeric(sub('.*Noncopepod : ([0-9]+).*', '\\1', line))
      detritus <- as.numeric(sub('.*Detritus : ([0-9]+).*', '\\1', line))
      objectlength <- as.numeric(sub('.*objectlength : ([0-9]+).*', '\\1', line))
      thresholdArea_otsu <- as.numeric(sub('.*thresholdArea_otsu : ([0-9.]+).*', '\\1', line))
      thresholdArea_standard <- as.numeric(sub('.*thresholdArea_standard : ([0-9.]+).*', '\\1', line))
      equispherdiameter_otsu <- as.numeric(sub('.*equispherdiameter_otsu : ([0-9.]+).*', '\\1', line))
      equispherdiameter_standard <- as.numeric(sub('.*equispherdiameter_standard : ([0-9.]+).*', '\\1', line))
      
      # Add extracted information to the data frame
      jetson_data_seen <- jetson_data_seen %>%
        bind_rows(data.frame(
          datetime = datetime,
          Copepod = copepod,
          Noncopepod = noncopepod,
          Detritus = detritus,
          objectlength = objectlength,
          thresholdArea_otsu = thresholdArea_otsu,
          thresholdArea_standard = thresholdArea_standard,
          equispherdiameter_otsu = equispherdiameter_otsu,
          equispherdiameter_standard = equispherdiameter_standard
        ))
    }
  }
}

jetson_data_seen=jetson_data_seen[!is.na(jetson_data_seen$datetime),]

jetson_data_seen <- jetson_data_seen %>%
 group_by(datetime) %>%
 summarise_all(funs(max(., na.rm = TRUE))) %>%
 ungroup()

#return nas from -inf back to na
#jetson_data_seen[] <- lapply(jetson_data_seen, function(x) ifelse(is.infinite(x) & x == -Inf, NA, x))
jetson_data_seen <- jetson_data_seen %>%  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.) & . == -Inf, NA, .)))

jetson_data_seen$Datetime <- jetson_data_seen$datetime + lubridate::hours(1)
jetson_data_seen$datetime <- NULL
jetson_data_seen$rounded_datetime <- round_date(jetson_data_seen$Datetime, unit = "minute")
jetson_data_seen$rounded_datetime_5 <- floor_date(jetson_data_seen$Datetime, unit = "5 minutes")  # Round to nearest 5 minutes
jetson_data_seen$total_particles_jetson <- jetson_data_seen$Copepod + jetson_data_seen$Noncopepod + jetson_data_seen$Detritus


# Imager
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
agg_jetson_seen <- jetson_data_seen %>%
  group_by(rounded_datetime_5) %>%
  summarise(total_particles_jetson = sum(total_particles_jetson, na.rm = TRUE))

agg_jetson_sent <- jetson_data_sent %>%
  group_by(rounded_datetime_5) %>%
  summarise(total_particles_jetson_sent = sum(total_particles_jetson_sent, na.rm = TRUE))

agg_imager <- imager_hits_misses %>%
  group_by(rounded_datetime_5) %>%
  summarise(total_particles_imager = sum(total_particles_imager, na.rm = TRUE),
            Hits = sum(Hits))

# Merge the aggregated dataframes on rounded_datetime
merged_data <- left_join(agg_jetson_sent, agg_imager, by = "rounded_datetime_5")
merged_data <- left_join(merged_data, agg_jetson_seen, by = "rounded_datetime_5")

# Directory creation
figures_directory <- "C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/rapid_paper/figures"
dir_create(figures_directory)

# A look at the misses during times of sampling versus travel: Function to check if a datetime is within any of the grey bands
is_within_a_sampling_period <- function(datetime, periods) {  any(sapply(1:nrow(periods), function(i) datetime >= periods$TimeStart[i] & datetime <= periods$TimeEnd[i])) } 
imager_hits_misses <- imager_hits_misses %>%  mutate( within_a_sampling_period = sapply(Datetime, is_within_a_sampling_period, periods = ship_log)  )
misses_comparison_dataframe <- imager_hits_misses %>%  mutate(period = ifelse(within_a_sampling_period, "Within Sampling Period", "Outside Sampling Period")) %>%  select(Datetime, Misses, period)

# Plot boxplots or similar
library(ggridges)
ridgeline_plot <- ggplot(misses_comparison_dataframe, aes(x = Misses, y = period, fill = period)) +
  geom_density_ridges(alpha = 0.5, scale = 0.99, bandwidth = 10) +
  labs(    title = "Ridgeline Plot of Misses within versus outside sampling period",
    x = "Misses",
    y = "Period"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Within" = "grey", "Outside" = "white"))
ggsave(file.path(figures_directory, "misses_ridgeline_plot.png"), ridgeline_plot, width = 10, height = 8, dpi = 500, bg = "white")

# Violin plot
violin_plot <- ggplot(misses_comparison_dataframe, aes(x = period, y = Misses, fill = period)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.3) +
  labs(
    title = "Misses During and Outside Grey Bands",
    x = "Period",
    y = "Misses"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Within Grey Band" = "grey", "Outside Grey Band" = "white"))
ggsave(file.path(figures_directory, "misses_violin_plot.png"), violin_plot, width = 10, height = 8, dpi = 500, bg = "white")




# First look at data
plot1 <- ggplot() +
  geom_rect(data= ship_log, aes(xmin = TimeStart, xmax = TimeEnd, ymin = 0, ymax = max(imager_hits_misses$Misses,na.rm=T)), fill = "grey")+
  geom_line(data=imager_hits_misses, aes(x = Datetime, y = Hits, color = "Hits")) +
  geom_line(data=imager_hits_misses, aes(x = Datetime, y = Misses, color = "Misses")) +
  labs(title = "Raw Data: Time Series of Hits and Misses. Grey shading indicates duration of sampling station.",
       x = "Datetime",
       y = "Count") +
  scale_color_manual(values = c("Hits" = "blue", "Misses" = "red")) +
  theme_minimal()+
  xlim(min(imager_hits_misses$Datetime,na.rm=T),max(imager_hits_misses$Datetime,na.rm=T))


ggsave(file.path(figures_directory, "hits_misses_raw_data.png"), plot1, width = 10, height = 8, dpi = 500,bg = "white")

# It seems log scale is needed. 
plot2 <- ggplot() +
  geom_rect(data= ship_log, aes(xmin = TimeStart, xmax = TimeEnd, ymin = 0, ymax = max(imager_hits_misses$Misses,na.rm=T)), fill = "grey")+
  geom_line(data = imager_hits_misses, aes(x = Datetime, y = Hits, color = "Hits")) +
  geom_line(data = imager_hits_misses, aes(x = Datetime, y = total_particles_imager, color = "Total particles imager")) +
  geom_line(data = jetson_data_seen, aes(x = Datetime, y = total_particles_jetson, color = "Total particles jetson")) +
  scale_y_log10() +
  labs(title = "Combined Time Series",
       x = "Datetime",
       y = "Count (Log Scale)",
       color = "Legend") +
  scale_color_manual(values = c("Hits" = "blue", "Total particles imager" = "red", "Total particles jetson" = "green")) +
  theme_minimal()+
  xlim(min(imager_hits_misses$Datetime,na.rm=T),max(imager_hits_misses$Datetime,na.rm=T))

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



plotfig <- ggplot(merged_data, aes(x= total_particles_jetson , y = total_particles_jetson_sent)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Scatter Plot of transmission losses",
       x =  "Total particles processed by jetson",
       y ="Total particles succesfully sent by jetson")

ggsave(file.path(figures_directory, "scatter_jetson_jetson.png"), plotfig, width = 10, height = 8, dpi = 500,bg = "white")




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
  labs(title = "Log-log scatter Plot of Hits (imager) vs Total Particles (imager)",
       x = "Total Particles (Imager)",
       y = "Photographed Particles (Imager)")+
  xlim(2,max(log10(imager_hits_misses$total_particles_imager)+1,na.rm = T))+
  ylim(2,max(log10(imager_hits_misses$total_particles_imager)+1,na.rm = T))

ggsave(file.path(figures_directory, "scatter_imager.png"), plot4, width = 10, height = 8, dpi = 500,bg = "white")






# Determining a scaling factor: A correction must be made because the jetson does not log 'on the minute'.
# Measurements are on an inconsistent frequency of approximately 70 seconds. I need to resample the data so that the time between amount of data within any given minute is the best available estimate in that minute.
# The first step is that where diff(jetson_data_seen$Datetime) is between 60 and 100 (calculate what % of the data this is), we can flag these as continuous uninterrupted data.
# Next we must take these data where continuousuniterrupted=T, and set the measurementstarttime to be that of the last available measurement taken in the timeseries, which we know was in the last 60-100 seconds.
# Then set a column called measurementendtime to be 60 seconds after that measurementstarttime.
# We then iterate through the minutes within our data time period. For any record where the span of measurementendtime to measurementstarttime falls in a minute, the fraction of the counts is split within a given minute based on the fraction of time that overlaps with that minute. These are summed to get the count estimate for that minute.

jetson_data_seen <- jetson_data_seen %>%  arrange(Datetime) %>%  mutate(Datetime = as.POSIXct(Datetime),         diff_time = c(NA, diff(Datetime)), continuous_uninterrupted = diff_time >= 60 & diff_time <= 100)

# Set measurementstarttime and measurementendtime
jetson_data_seen <- jetson_data_seen %>%  mutate(measurementstarttime = if_else(continuous_uninterrupted, lag(Datetime), NA_POSIXct_), measurementendtime = if_else(continuous_uninterrupted, lag(Datetime) + 60, NA_POSIXct_))

start_time <- floor_date(min(jetson_data_seen$Datetime, na.rm = TRUE), unit = "minute")
end_time <- ceiling_date(max(jetson_data_seen$Datetime, na.rm = TRUE), unit = "minute")
minute_intervals <- seq(start_time, end_time, by = 60)
minute_counts <- numeric(length(minute_intervals))

for (j in seq_along(minute_intervals)) {
  minute <- minute_intervals[j]
  
  overlapping_records <- jetson_data_seen %>%
    filter(!is.na(measurementstarttime) & 
             measurementstarttime <= minute + minutes(1) & 
                                                  measurementendtime >= minute)
  
  
  count_sum <- 0  # Initialize count sum for the current minute interval
  if(nrow(overlapping_records) > 0) {
    for (i in 1:nrow(overlapping_records)) {
      record <- overlapping_records[i, ]
      overlap_start <- max(minute, record$measurementstarttime)
      overlap_end <- min(minute + minutes(1), record$measurementendtime)
      overlap_duration <- as.numeric(difftime(overlap_end, overlap_start, units = "secs"))
      overlap_fraction <- overlap_duration / 60
      count_sum <- count_sum + record$total_particles_jetson * overlap_fraction
    }
  }
  
  minute_counts[j] <- count_sum
}

# Create a data frame with the minute intervals and corresponding estimated counts
jetson_data_seen_resampled <- data.frame(  minute_interval = minute_intervals,  estimated_counts = minute_counts )


ggplot() +
  geom_line(data = jetson_data_seen, aes(x = Datetime, y = total_particles_jetson), color = "blue", alpha = 0.5) +
  geom_line(data = jetson_data_seen_resampled, aes(x = minute_interval, y = estimated_counts), color = "red") +
  labs(title = "Original vs Resampled Data",
       x = "Datetime",
       y = "Particle Counts",
       color = "Data Type") +
  scale_color_manual(values = c("Original Data" = "blue", "Resampled Data" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "Data Type", override.aes = list(alpha = 1)))


# Calculate the lost counts percentage
print(paste("Lost counts:", (sum(jetson_data_seen$total_particles_jetson, na.rm = TRUE)-sum(resampled_data$estimated_counts, na.rm = TRUE))/sum(resampled_data$estimated_counts, na.rm = TRUE) * 100,"%"))
continuous_counts <- sum(jetson_data_seen$total_particles_jetson[jetson_data_seen$continuous_uninterrupted], na.rm = TRUE)
total_counts <- sum(jetson_data_seen$total_particles_jetson, na.rm = TRUE)
print(paste("Percentage of continuous uninterrupted data by counts:", continuous_counts / total_counts * 100, "%"))

