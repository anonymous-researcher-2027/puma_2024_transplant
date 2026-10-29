# Load required library
library(dplyr)

# Define file paths
input_file <- "D:\\PUMA-motus\\1.filtered_motus_data_puma_feb_11_2025.csv" # the manually filtered version of the csv created in step 1
output_file <- "D:\\PUMA-motus\\Checking_for_false_positives.csv"

# Read the CSV file
motus_data <- read.csv(input_file, stringsAsFactors = FALSE)

# Filter out rows where tagDeployID is NA
motus_data <- motus_data %>%
  filter(!is.na(tagDeployID))

# Process data to extract first and last detection before switching towers
filtered_data <- motus_data %>%
  arrange(tagDeployID, detectyear, detectdoy, detecttime) %>%  # Ensure chronological order
  group_by(tagDeployID) %>%
  mutate(tower_change = recvDeployName != lag(recvDeployName, default = first(recvDeployName))) %>%  # Identify changes
  group_by(tagDeployID, recvDeployName, cumsum(tower_change)) %>%  # Group consecutive detections at the same tower
  summarise(
    first_detect = first(detectdoy),
    first_time = first(detecttime),
    first_year = first(detectyear),
    first_lat = first(recvDeployLat),
    first_lon = first(recvDeployLon),
    last_detect = last(detectdoy),
    last_time = last(detecttime),
    last_year = last(detectyear),
    last_lat = last(recvDeployLat),
    last_lon = last(recvDeployLon),
    motusTagID = first(motusTagID),
    .groups = "drop"
  ) %>%
  arrange(tagDeployID, first_year, first_detect, first_time)  # Ensure final sorting is correct

# Write the output to CSV
write.csv(filtered_data, output_file, row.names = FALSE)

cat("Processed data saved to:", output_file, "\n")












#Subsetting the entire dataset by individual and making subset files
# Load necessary package
library(dplyr)

# Define file paths
input_file <- "D:\\PUMA-motus\\csv4.csv"
output_folder <- "D:\\PUMA-motus\\Individual_Motus_Data"

# Read the CSV file
data <- read.csv(input_file, stringsAsFactors = FALSE)

# Remove exact duplicate rows (keeping one instance of each)
data_unique <- data %>% distinct()

# Split data by 'tagDeployID'
split_data <- split(data_unique, data_unique$tagDeployID)

# Save each subset to an individual CSV file
for (tag_id in names(split_data)) {
  output_file <- file.path(output_folder, paste0(tag_id, ".csv"))
  write.csv(split_data[[tag_id]], output_file, row.names = FALSE)
}

# Print completion message
cat("Processing complete. Individual files saved in:", output_folder, "\n")










#Listing all nest-cavity values which appear
# Load necessary library
library(data.table)

# Read the CSV file
file_path <- "D:/PUMA-motus/csv4.csv"
data <- fread(file_path, select = c("Site.Cavity", "Nest_Origin"))

# Get unique values from both columns
unique_values <- unique(c(data$Site.Cavity, data$Nest_Origin))

# Sort alphabetically
sorted_values <- sort(unique_values)

# Print to console
print(sorted_values)










#Removing irrelevant columns from our subsetted files
# Load necessary package
library(dplyr)

# Define folder path where subsetted files are stored
input_folder <- "D:\\PUMA-motus\\Individual_Motus_Data"

# List all CSV files in the folder
file_list <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)

# Define columns to remove
columns_to_remove <- c(
  "motusTagID", "ambigID", "port", "runLen", "motusFilter", "bootnum", "tagProjID", 
  "mfgID", "tagLifespan", "pulseLen", "markerNumber", "tagDeployStart", "tagDeployEnd", 
  "tagDepAlt", "tagDepComments", "tagDeployTest", "fullID", "deviceID", "recvDeployID", 
  "recvDeployAlt", "recv", "recvSiteName", "isRecvMobile", "recvProjID", "recvUtcOffset", 
  "antType", "antBearing", "antHeight", "speciesEN", "speciesFR", "speciesSci", 
  "speciesGroup", "tagProjName", "recvProjName", "probability", "Federal_.", "Bander", 
  "Weight", "Tarsus", "Bill_length", "Bill_width", "Bill_depth", "Wing_chord", "Tail", 
  "Kipps", "Distal", "Tag_type", "Tag_ID", "Method", "motusTagID.y", "Notes"
)

# Process each file
for (file in file_list) {
  # Read the CSV file
  data <- read.csv(file, stringsAsFactors = FALSE)
  
  # Remove specified columns if they exist in the dataset
  data_filtered <- select(data, -one_of(columns_to_remove))
  
  # Overwrite the original file with the modified data
  write.csv(data_filtered, file, row.names = FALSE)
}

# Print completion message
cat("Processing complete. Columns removed from all subsetted files.\n")












#Now i want to pull out the DOY and date at which the final home tower detection
# occurs for each individual prior to their first 24+ hr lapse in detections at
# their home colony. Basically, I want to know the DOY and Age of each individua's
# first real departure from their home colony. 
library(dplyr)

# Define file paths
input_folder <- "D:/PUMA-motus/Individual_Motus_Data"
output_file <- "D:/PUMA-motus/First_24hr_colony_lapse.csv"

# Get list of all CSV files in the input folder
file_list <- list.files(input_folder, pattern = "*.csv", full.names = TRUE)

# Initialize an empty list to store results
results <- list()

# Loop through each file
for (file in file_list) {
  # Read CSV file
  df <- read.csv(file)
  
  # Rename tagDeployID column if needed
  if ("tagDeployID.x" %in% names(df)) {
    df <- df %>% rename(tagDeployID = tagDeployID.x)
  }
  
  # Ensure relevant columns exist
  if (!all(c("tagDeployID", "Home_Tower", "recvDeployName", "detectdoy", "Detection_Age") %in% names(df))) {
    next
  }
  
  # Extract the individual's tagDeployID (from the file name or column)
  tagDeployID <- unique(df$tagDeployID)[1]
  
  # Identify the individual's Home_Tower
  home_tower <- unique(df$Home_Tower)[1]
  
  # Filter data for detections at the Home_Tower
  home_detections <- df %>%
    filter(recvDeployName == home_tower) %>%
    arrange(detectdoy)
  
  # Identify the first occurrence of a lapse of 2 or more days
  if (nrow(home_detections) > 0) {
    lapse_indices <- which(diff(home_detections$detectdoy) > 1)
    
    if (length(lapse_indices) > 0) {
      # Get the last detection before the first lapse
      last_detect_index <- lapse_indices[1]
      last_detect_row <- home_detections[last_detect_index, ]
    } else {
      # If no lapse exists, take the last recorded detection
      last_detect_row <- home_detections[nrow(home_detections), ]
    }
    
    # Store the result
    results[[length(results) + 1]] <- data.frame(
      tagDeployID = tagDeployID,
      detectdoy = last_detect_row$detectdoy,
      Detection_Age = last_detect_row$Detection_Age
    )
  }
}

# Combine results and save to CSV
if (length(results) > 0) {
  final_results <- bind_rows(results)
  write.csv(final_results, output_file, row.names = FALSE)
}