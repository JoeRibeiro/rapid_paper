# Load necessary libraries
library(jsonlite)
library(dplyr)
library(stringr)

# Specify the directory containing the log files
log_directory <- "C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/rapid_paper/data/jetsonlog"

# List all files in the directory
files <- list.files(log_directory, full.names = TRUE)

# Initialize an empty list to store data frames
df_list <- list()

# Loop through each file
for (file in files) {
  # Read the entire file content
  content <- readLines(file, warn = FALSE)
  
  # Collapse content into a single string
  content <- paste(content, collapse = "\n")
  
  # Extract JSON strings using regex to find blocks between braces
  json_strings <- str_extract_all(content, "\\{[^\\}]*\\}")[[1]]
  
  # Parse each JSON string and store it in a list
  json_list <- lapply(json_strings, fromJSON)
  
  # Combine all JSON objects into a data frame
  if (length(json_list) > 0) {
    df <- bind_rows(json_list)
    df_list <- append(df_list, list(df))
  }
}

# Combine all data frames into one
final_df <- bind_rows(df_list)

# Print the resulting data frame
print(final_df)
