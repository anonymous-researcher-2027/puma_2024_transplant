# Load required library
library(dplyr)

# Define file paths
input_file <- "D:\\PUMA-motus\\1.filtered_motus_data_puma.csv" # the manually filtered version of the csv created in step 1
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
