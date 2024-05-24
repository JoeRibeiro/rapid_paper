# Load required libraries
library(dplyr)

# Initialize an empty data frame to store the extracted information
df <- data.frame(
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

# List all log files in the directory
log_files <- list.files(path = "C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/rapid_paper/data/jetsonlog", pattern = "*.log", full.names = TRUE)

# Process each log file
for (file in log_files) {
  # Read lines from the log file
  lines <- readLines(file)
  
  # Extract relevant information from each line
  for (line in lines) {
    if (grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2},\\d{3} - INFO - (Copepod|objectlength)", line)) {
      # Extract datetime
      datetime <- as.POSIXct(strptime(substr(line, 1, 23), "%Y-%m-%d %H:%M:%S,%OS"))
      
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
      df <- df %>%
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

data=df

# Combine lines
library(dplyr)
combined_data <- data %>%
  group_by(datetime) %>%
  summarise_all(funs(max(., na.rm = TRUE))) %>%
  ungroup()

