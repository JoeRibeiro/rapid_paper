library(jsonlite)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(fs) 
library(tidyverse)

# Directory creation
figures_directory <- "C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/rapid_paper/figures"
statements_directory <- "C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/rapid_paper/statements"
dir_create(figures_directory)
dir_create(statements_directory)

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
dashboard <- dashboard %>% filter(Datetime < as.POSIXct("2024-05-15 00:00:00") | Datetime >= as.POSIXct("2024-05-16 00:00:00")) # emulated data in port.
merged_sent_and_received=left_join(jetson_data_sent,dashboard, by = "rounded_datetime")
# Verify that the dashboard contains the data marked as sent in the logfile... yes (difference = 0)
stopifnot(max(merged_sent_and_received$total_particles_jetson_sent.x-merged_sent_and_received$total_particles_jetson_sent.y,na.rm=T)==0)

# Create a stacked bar plot of proportions
dashboard_long <- dashboard %>%  select(Datetime, copepodCount, nonCopepodCount, detritusCount) %>% pivot_longer(cols = c(copepodCount, nonCopepodCount, detritusCount),   names_to = "Category",  values_to = "Count") %>%  group_by(Datetime) %>% mutate(TotalCount = sum(Count),   Proportion = Count / TotalCount)
dashboard_long$Category <- factor(dashboard_long$Category, levels = c("nonCopepodCount", "detritusCount","copepodCount"))
dashboarddata=ggplot(dashboard_long, aes(x = Datetime, y = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Dashboard Proportions Over Time",       x = "Datetime",       y = "Proportion",       fill = "Category") +
  theme_minimal()
ggsave(file.path(figures_directory, "dashboard_proportion_plot.png"), dashboarddata, width = 10, height = 4, dpi = 500, bg = "white")

dashboarddata2=ggplot(dashboard_long, aes(x = Datetime, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Dashboard Counts Over Time",       x = "Datetime",       y = "Count",       fill = "Category") +
  theme_minimal()
ggsave(file.path(figures_directory, "dashboard_data_plot.png"), dashboarddata2, width = 10, height = 4, dpi = 500, bg = "white")


dashboard_long$Category <- factor(dashboard_long$Category, 
                                  levels = c( "nonCopepodCount", "detritusCount","copepodCount"),
                                  labels = c( "Non-Copepod Count", "Detritus Count","Copepod Count"))

dashboarddata3=ggplot(dashboard_long, aes(x = Category, y = Proportion, fill = Category)) +
  geom_boxplot() +
  labs(title = "Summary Statistics of Proportions by Category",       x = "Category",       y = "Proportion of count") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave(file.path(figures_directory, "dashboard_data_box.png"), dashboarddata3, width = 10, height = 8, dpi = 500, bg = "white")



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
jetson_data_seen$`Particles classified` <- jetson_data_seen$Copepod + jetson_data_seen$Noncopepod + jetson_data_seen$Detritus


# Imager hits misses rate
hits_directory <- "C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/rapid_paper/data/hitsmisses"
file_list <- list.files(path = hits_directory, pattern = "*.csv", full.names = TRUE)
imager_hits_misses <- data.frame()

for (file in file_list) {
  temp_data <- read.csv(file)
  imager_hits_misses <- rbind(imager_hits_misses, temp_data)
}
imager_hits_misses$`Particles photographed` = imager_hits_misses$Hits
imager_hits_misses$Hits = NULL
imager_hits_misses$`Particles not photographed (missed)` = imager_hits_misses$Misses
imager_hits_misses$Misses = NULL
imager_hits_misses$Time <- sprintf("%04d", imager_hits_misses$Tenbin + 100 + imager_hits_misses$Minute) # The + 100 corrects the data from UTC to BST, which the jetson data are in
imager_hits_misses$Datetime <- ymd_hm(paste(imager_hits_misses$Date, imager_hits_misses$Time))
imager_hits_misses$rounded_datetime <- round_date(imager_hits_misses$Datetime, unit = "minute")
imager_hits_misses$rounded_datetime_5 <- floor_date(imager_hits_misses$Datetime, unit = "5 minutes")  # Round to nearest 5 minutes
imager_hits_misses$`Particles total` <- imager_hits_misses$`Particles photographed` + imager_hits_misses$`Particles not photographed (missed)`


# Imager seen
classified_blob_directory <- "C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/rapid_paper/data/azureblobsums"
file_list <- list.files(path = classified_blob_directory, pattern = "*.csv", full.names = TRUE)
imager_hits_misses <- data.frame()

for (file in file_list) {
  temp_data <- read.csv(file)
  imager_seen <- rbind(imager_hits_misses, temp_data)
}

imager_seen$Date = "2024-05-19"
imager_seen$Time <- sprintf("%04d", imager_seen$Bin.Name+ 100)
imager_seen$Datetime <- ymd_hm(paste(imager_seen$Date, imager_seen$Time))
imager_seen=dplyr::rename(imager_seen,`Copepod Count`=copepod)
imager_seen=dplyr::rename(imager_seen,`Non-Copepod Count`=noncopepod)
imager_seen=dplyr::rename(imager_seen,`Detritus Count`=detritus)
imager_seen$`Particles total` <- imager_seen$`Copepod Count` + imager_seen$`Non-Copepod Count` + imager_seen$`Detritus Count`


# Imager seen: Create a stacked bar plot of proportions
imager_seen_long <- imager_seen %>%  select(Datetime, `Copepod Count`, `Non-Copepod Count`, `Detritus Count`) %>% pivot_longer(cols = c(`Copepod Count`, `Non-Copepod Count`, `Detritus Count`),   names_to = "Category",  values_to = "Count") %>%  group_by(Datetime) %>% mutate(TotalCount = sum(Count),   Proportion = Count / TotalCount)
imager_seen_long$Category <- factor(imager_seen_long$Category, levels = c("Non-Copepod Count", "Detritus Count","Copepod Count"))
imager_seen_data=ggplot(imager_seen_long, aes(x = Datetime, y = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Imager Proportions Over Time",       x = "Datetime",       y = "Proportion",       fill = "Category") +
  theme_minimal() + scale_x_datetime(limits = as.POSIXct(c("2024-05-19 00:00:00", "2024-05-19 23:59:59")),date_breaks = "2 hours", date_labels = "%H:%M")
ggsave(file.path(figures_directory, "Imager_proportion_plot_19th_may.png"), imager_seen_data, width = 10, height = 4, dpi = 500, bg = "white")

imager_seen_data2=ggplot(imager_seen_long, aes(x = Datetime, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Imager Counts Over Time",       x = "Datetime",       y = "Count",       fill = "Category") +
  theme_minimal() + scale_x_datetime(limits = as.POSIXct(c("2024-05-19 00:00:00", "2024-05-19 23:59:59")),date_breaks = "2 hours", date_labels = "%H:%M")
ggsave(file.path(figures_directory, "Imager_data_plot_19th_may.png"), imager_seen_data2, width = 10, height = 4, dpi = 500, bg = "white")



# Aggregate jetson_data_sent and imager_hits_misses by rounded_datetime
agg_jetson_seen <- jetson_data_seen %>%
  group_by(rounded_datetime_5) %>%
  summarise(`Particles classified` = sum(`Particles classified`, na.rm = TRUE))

agg_jetson_sent <- jetson_data_sent %>%
  group_by(rounded_datetime_5) %>%
  summarise(total_particles_jetson_sent = sum(total_particles_jetson_sent, na.rm = TRUE))

agg_imager <- imager_hits_misses %>%
  group_by(rounded_datetime_5) %>%
  summarise(`Particles total` = sum(`Particles total`, na.rm = TRUE),
            `Particles photographed` = sum(`Particles photographed`))

# Merge the aggregated dataframes on rounded_datetime
merged_data <- left_join(agg_jetson_sent, agg_imager, by = "rounded_datetime_5")
merged_data <- left_join(merged_data, agg_jetson_seen, by = "rounded_datetime_5")
# 5 minutes rate to 1 minute
merged_data$total_particles_jetson_sent = merged_data$total_particles_jetson_sent/5
merged_data$`Particles total` = merged_data$`Particles total`/5
merged_data$`Particles photographed` = merged_data$`Particles photographed`/5
merged_data$`Particles classified` = merged_data$`Particles classified`/5


# A look at the `Particles not photographed (missed)` during times of sampling versus travel: Function to check if a datetime is within any of the grey bands
is_within_a_sampling_period <- function(datetime, periods) {  any(sapply(1:nrow(periods), function(i) datetime >= periods$TimeStart[i] & datetime <= periods$TimeEnd[i])) } 
imager_hits_misses <- imager_hits_misses %>%  mutate( within_a_sampling_period = sapply(Datetime, is_within_a_sampling_period, periods = ship_log)  )
misses_comparison_dataframe <- imager_hits_misses %>%  mutate(period = ifelse(within_a_sampling_period, "Within Sampling Period", "Outside Sampling Period")) %>%  select(Datetime, `Particles not photographed (missed)`, period)

# Violin plot
violin_plot <- ggplot(misses_comparison_dataframe, aes(x = period, y = `Particles not photographed (missed)`, fill = period)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.3) +
  labs(
    title = "`Particles not photographed (missed)` During and Outside Grey Bands",
    x = "Period",
    y = "Rate missed per minute"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Within Grey Band" = "grey", "Outside Grey Band" = "white"))
ggsave(file.path(figures_directory, "misses_violin_plot.png"), violin_plot, width = 10, height = 8, dpi = 500, bg = "white")




# First look at data
plot1 <- ggplot() +
  geom_rect(data= ship_log, aes(xmin = TimeStart, xmax = TimeEnd, ymin = 0, ymax = max(imager_hits_misses$`Particles not photographed (missed)`,na.rm=T)), fill = "grey")+
  geom_line(data=imager_hits_misses, aes(x = Datetime, y = `Particles photographed`, color = "`Particles photographed`")) +
  geom_line(data=imager_hits_misses, aes(x = Datetime, y = `Particles not photographed (missed)`, color = "`Particles not photographed (missed)`")) +
  labs(title = "Raw Data: Time Series of `Particles photographed` and `Particles not photographed (missed)`. Grey shading indicates duration of sampling station.",
       x = "Datetime",
       y = "Count (per minute)") +
  scale_color_manual(values = c("`Particles photographed`" = "blue", "`Particles not photographed (missed)`" = "red")) +
  theme_minimal()+
  xlim(min(imager_hits_misses$Datetime,na.rm=T),max(imager_hits_misses$Datetime,na.rm=T))


ggsave(file.path(figures_directory, "hits_misses_raw_data.png"), plot1, width = 10, height = 8, dpi = 500,bg = "white")

# It seems log scale is needed. 
jetson_data_seen <- jetson_data_seen %>%
  arrange(Datetime) %>%
  mutate(Gap = c(NA, diff(Datetime))) %>%
  mutate(`Particles classified gap` = ifelse(Gap > minutes(5), NA, `Particles classified`))
plot2 <- ggplot() +
  geom_rect(data = ship_log, aes(xmin = TimeStart, xmax = TimeEnd, ymin = min(imager_hits_misses$`Particles not photographed (missed)`, na.rm = TRUE), ymax = max(imager_hits_misses$`Particles not photographed (missed)`, na.rm = TRUE)), fill = "grey") +
  geom_line(data = imager_hits_misses, aes(x = Datetime, y = `Particles total`, color = "Total particles imager")) +
  geom_line(data = imager_hits_misses, aes(x = Datetime, y = `Particles photographed`, color = "`Particles photographed`")) +
  geom_line(data = jetson_data_seen, aes(x = Datetime, y = `Particles classified gap`, color = "Total particles jetson")) +
  labs(x = NULL,
       y = "Particle count (Per minute)",
       color = "Legend") +
  scale_color_manual(values = c("`Particles photographed`" = "blue", "Total particles imager" = "red", "Total particles jetson" = "green")) +
  theme_minimal() +
#  xlim(min(imager_hits_misses$Datetime, na.rm = TRUE), max(imager_hits_misses$Datetime, na.rm = TRUE))
  xlim(min(imager_hits_misses$Datetime,na.rm=T),"2024-05-18 16:00:00 UTC")+
 # ylim+
  scale_y_log10(limit = c(100,10000000)) 

ggsave(file.path(figures_directory, "combined_time_series.png"), plot2, width = 10, height = 4, dpi = 500,bg = "white")


limit = max(merged_data$`Particles photographed`, na.rm = TRUE)
  file_name <- paste0("scatter_jetson.png")
  plot <- ggplot(merged_data, aes(x = `Particles photographed` , y = `Particles classified`)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(title = "Scatter Plot of photographed particles vs number particles classified (per minute)",
         x = "Photographed Particles (Imager)",
         y = "Classified Particles (Jetson)") +
    xlim(0.0000001, limit) +
    ylim(0.0000001, limit)
  ggsave(file.path(figures_directory, file_name), plot, width = 10, height = 8, dpi = 500, bg = "white")





plotfig <- ggplot(merged_data, aes(x= `Particles classified` , y = total_particles_jetson_sent)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Scatter Plot of transmission losses",
       x =  "Total particles classified (per minute)",
       y ="Total particles transmitted to dashboard database (per minute)")

ggsave(file.path(figures_directory, "scatter_jetson_dashboard.png"), plotfig, width = 10, height = 8, dpi = 500,bg = "white")
# There is one instance where the summary packet is computed within the second 2024-05-22 10:49:59 yet the attempted data transmission occurs in the second 10:50:00. This causes the data point to fall to the left of the line for this summary packet.



plot3 <- ggplot(merged_data, aes(x= log10(`Particles photographed`) , y = log10(`Particles classified`))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Scatter Plot of particles photographed vs particles classified (per minute)",
       x =  "log(Photographed Particles (per minute))",
       y ="log(Classified Particles (per minute))")+
  xlim(2,max(log10(merged_data$`Particles photographed`)+1,na.rm = T))+
  ylim(2,max(log10(merged_data$`Particles photographed`)+1,na.rm = T))

ggsave(file.path(figures_directory, "scatter_jetson.png"), plot3, width = 10, height = 8, dpi = 500,bg = "white")


plot4 <- ggplot(imager_hits_misses, aes(x = log10(`Particles total`), y = log10(`Particles photographed`))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Log-log scatter Plot of particles photographed vs total particles (per minute)",
       x = "Total Particles (Imager)",
       y = "Photographed Particles (Imager)") +
  coord_cartesian(xlim = c(1.5, max(log10(imager_hits_misses$`Particles total`) + 1, na.rm = TRUE)), ylim= c(1.5, max(log10(imager_hits_misses$`Particles total`) + 1, na.rm = TRUE)),# This focuses the x-axis on the range of interest
                  clip = 'off') +
  geom_text(x = 1.6, y = log10(100), label = "100") +
  geom_text(x = 1.8, y = log10(10000), label = "10000") +
  geom_text(x = 1.9, y = log10(1000000), label = "1000000") +
  geom_text(y = 1.5, x = log10(100), label = "100") +
  geom_text(y = 1.5, x = log10(10000), label = "10000") +
  geom_text(y = 1.5, x = log10(1000000), label = "1000000") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
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
      count_sum <- count_sum + record$`Particles classified` * overlap_fraction
    }
  }
  
  minute_counts[j] <- count_sum
}

# Create a data frame with the minute intervals and corresponding estimated counts
jetson_data_seen_resampled <- data.frame(  Datetime = minute_intervals,  estimated_counts = minute_counts )
jetson_data_seen_resampled=jetson_data_seen_resampled[jetson_data_seen_resampled$estimated_counts>0,] # The only situation where the jetson is counting zero particles in a minute, we can assume it is not operating (switched off)

ggplot() +
  geom_point(data = jetson_data_seen, aes(x = Datetime, y = `Particles classified`), color = "blue", alpha = 0.1, size=0.05) +
  geom_point(data = jetson_data_seen_resampled, aes(x = Datetime, y = estimated_counts), color = "red", alpha = 0.1, size=0.05) +
  geom_line(data = jetson_data_seen, aes(x = Datetime, y = `Particles classified`), color = "blue", alpha = 0.1, size=0.05) +
  geom_line(data = jetson_data_seen_resampled, aes(x = Datetime, y = estimated_counts), color = "red", alpha = 0.1, size=0.05) +
  labs(title = "Original vs Resampled Data",
       x = "Datetime",
       y = "Particle Counts (per minute)",
       color = "Data Type") +
  scale_color_manual(values = c("Original Data" = "blue", "Resampled Data" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "Data Type", override.aes = list(alpha = 1)))


# Calculate the lost counts percentage
print(paste("Lost counts:", (sum(jetson_data_seen$`Particles classified`, na.rm = TRUE)-sum(jetson_data_seen_resampled$estimated_counts, na.rm = TRUE))/sum(jetson_data_seen_resampled$estimated_counts, na.rm = TRUE) * 100,"%"))
continuous_counts <- sum(jetson_data_seen$`Particles classified`[jetson_data_seen$continuous_uninterrupted], na.rm = TRUE)
total_counts <- sum(jetson_data_seen$`Particles classified`, na.rm = TRUE)
print(paste("Percentage of continuous uninterrupted data by counts:", continuous_counts / total_counts * 100, "%"))

# Merge on datetime
merged_seen_and_PI=left_join(jetson_data_seen_resampled,imager_hits_misses, by = "Datetime")
merged_seen_and_PI$jetson_sampling_percent = merged_seen_and_PI$estimated_counts / merged_seen_and_PI$`Particles photographed` * 100
merged_seen_and_PI$overall_sampling_percent = merged_seen_and_PI$estimated_counts / merged_seen_and_PI$`Particles total` * 100

ss_plt <- ggplot(merged_seen_and_PI, aes(x = `Particles photographed`, y = jetson_sampling_percent)) +
  geom_point() +
  labs(x = "`Particles photographed`", y = "Percent of photos classified within 1 minute window")
ggsave(file.path(figures_directory, "jetson_subsampling_plot1.png"), ss_plt, width = 10, height = 8, dpi = 500, bg = "white")

# Overall sampling percent seems fairly meaningless
ss_plt <- ggplot()+
geom_point(data = merged_seen_and_PI, aes(x = Datetime, y = jetson_sampling_percent), color = "blue", alpha = 0.2)+
  labs(x = "`Particles photographed`", y = "Percent of photos classified  within 1 minute window")
ggsave(file.path(figures_directory, "jetson_subsampling_plot2.png"), ss_plt, width = 10, height = 8, dpi = 500, bg = "white")



violin_plot <- ggplot(merged_seen_and_PI, aes(x = NA, y = jetson_sampling_percent)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.3) +
  labs(
    title = "Percent of photos classified",
    x = NA,
    y = "% sampled"
  ) +
  theme_minimal() 
ggsave(file.path(figures_directory, "jetson_subsampling_violin_plot.png"), violin_plot, width = 10, height = 8, dpi = 500, bg = "white")

totclass = sum(jetson_data_seen$`Particles classified`,na.rm=T)
totsent = sum(jetson_data_sent$total_particles_jetson_sent,na.rm=T)
statement1=paste0(totclass," particles were classified and statistics for ", totsent," of these were successfully sent to the dashboard, a transmission success rate of ",round(totsent/totclass*100,2)," %")

pclassasdetritus = round(sum(dashboard$detritusCount)/sum(dashboard$total_particles_jetson_sent)*100,1)
pclassascopepod = round(sum(dashboard$copepodCount)/sum(dashboard$total_particles_jetson_sent)*100,1)
pclassasnoncopepod = round(sum(dashboard$nonCopepodCount)/sum(dashboard$total_particles_jetson_sent)*100,1)

statement2=paste0("The majority of particles were classified as detritus (", pclassasdetritus, "%)  followed by copepods (",pclassascopepod,"%) and non-copepods (",pclassasnoncopepod,"%), in line with expectations")


timestamps <- as.POSIXct(dashboard$Timestamp, format="%Y-%m-%d %H:%M:%OS")
time_diffs <- diff(timestamps)
interrupted_periods <- time_diffs[time_diffs > 2]
total_interrupted_time <- sum(interrupted_periods)
total_time_span <- as.numeric(difftime(max(timestamps), min(timestamps), units = "mins"))
cat("Total interrupted time (seconds):", total_interrupted_time, "\n")
cat("Total time span (seconds):", total_time_span, "\n")
cat("Percentage of interrupted time:", total_interrupted_time / total_time_span * 100, "\n")

statement3=paste0("Over the 7 days and 5 hours duration, ",round(as.numeric(total_interrupted_time)/60)," hours were identified as 'downtime' where the dashboard did not receive any data for over 2 minutes. This is equal to a downtime of ", round(total_interrupted_time / total_time_span * 100),"%")

write.csv(c(statement1,statement2,statement3),file.path(statements_directory, "numbers_transmitted.csv"))



# Map
get_dominant_class <- function(row) {counts <- row[c("copepodCount", "nonCopepodCount", "detritusCount")]
  class_names <- c("copepod", "nonCopepod", "detritus")
  dominant_class <- class_names[which.max(counts)]
  return(dominant_class)}
dashboard$`Dominant class` <- apply(dashboard, 1, get_dominant_class)

dashboard_filtered <- dashboard %>%
  arrange(Datetime) %>%
  mutate(day = as.numeric(format(Datetime, "%d")),
         day_change = day != lag(day, default = first(day)))

vmin <- quantile(dashboard_filtered$copepodCount, 0.05)
vmax <- quantile(dashboard_filtered$copepodCount, 0.95)

# Extract locations where the day changes
day_change_locations <- dashboard_filtered %>% filter(day_change)

world <- map_data('world')
map <- ggplot(world, aes(long, lat)) +  
  geom_map(map = world, aes(map_id = region), fill = 'darkgreen', color = "black") +
  coord_quickmap() +
  geom_point(data=dashboard_filtered, aes(x = Longitude, y = Latitude, color = copepodCount), size = 2, alpha = 0.7, stroke = 1, shape = 21, fill = 'black') +
  scale_color_viridis_c(option = 'viridis', limits = c(vmin, vmax)) +
  labs(color = 'Copepod count', x = 'Longitude', y = 'Latitude') +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey", size = 0.5),
        panel.grid.minor = element_blank()) +
  xlim(min(dashboard_filtered$Longitude) - 2, max(dashboard_filtered$Longitude) + 2) +
  ylim(min(dashboard_filtered$Latitude) - 2, max(dashboard_filtered$Latitude) + 2) +
  geom_text(data=day_change_locations, aes(x = Longitude, y = Latitude, label = day), color = "red", size = 5, vjust = -1)

ggsave(file.path(figures_directory, "mapplotcopepod.png"), map, width = 10, height = 8, dpi = 500, bg = "white")

map <- map +
  xlim(-1.5, 0) +
  ylim(54.2, 55.7)

ggsave(file.path(figures_directory, "mapplotcopepod2.png"), map, width = 10, height = 8, dpi = 500, bg = "white")



mapclass <- ggplot(world, aes(long, lat)) +  
  geom_map(map = world, aes(map_id = region), fill = 'darkgreen', color = "black") +
  coord_quickmap() +
  geom_point(data=dashboard, aes(x = Longitude, y = Latitude, color = `Dominant class`), size = 2, alpha=0.1) +
  labs(x = 'Longitude', y = 'Latitude') +
  theme_minimal()+
  theme(panel.grid.major = element_line(color = "grey", size = 0.5),
        panel.grid.minor = element_blank()) +
  xlim(min(dashboard_filtered$Longitude) - 2, max(dashboard_filtered$Longitude) + 2) +
  ylim(min(dashboard_filtered$Latitude) - 2, max(dashboard_filtered$Latitude) + 2) +
  geom_text(data=day_change_locations, aes(x = Longitude, y = Latitude, label = day), color = "red", size = 5, vjust = -1)

ggsave(file.path(figures_directory, "mapplotclass.png"), mapclass, width = 10, height = 8, dpi = 500, bg = "white")

mapclass <- mapclass +
  xlim(-1.5, 0) +
  ylim(54.2, 55.7)

ggsave(file.path(figures_directory, "mapplotclass2.png"), mapclass, width = 10, height = 8, dpi = 500, bg = "white")



# Create a stacked bar plot of proportions for 19th May only 
dashboarddata=ggplot(dashboard_long[as.Date(dashboard_long$Datetime) == "2024-05-19", ], aes(x = Datetime, y = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Dashboard Proportions Over Time",       x = "Datetime",       y = "Proportion",       fill = "Category") +
  theme_minimal() + scale_x_datetime(limits = as.POSIXct(c("2024-05-19 00:00:00", "2024-05-19 23:59:59")),date_breaks = "2 hours", date_labels = "%H:%M")
ggsave(file.path(figures_directory, "dashboard_proportion_plot_19th_May.png"), dashboarddata, width = 10, height = 4, dpi = 500, bg = "white")

dashboarddata2=ggplot(dashboard_long[as.Date(dashboard_long$Datetime) == "2024-05-19", ], aes(x = Datetime, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Dashboard Counts Over Time",       x = "Datetime",       y = "Count",       fill = "Category") +
  theme_minimal() + scale_x_datetime(limits = as.POSIXct(c("2024-05-19 00:00:00", "2024-05-19 23:59:59")),date_breaks = "2 hours", date_labels = "%H:%M")
ggsave(file.path(figures_directory, "dashboard_data_plot_19th_May.png"), dashboarddata2, width = 10, height = 4, dpi = 500, bg = "white")
