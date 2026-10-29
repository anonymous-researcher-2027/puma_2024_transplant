# Load necessary library
library(dplyr)
library(lubridate)

# File paths
csv1_path <- "D:/PUMA-motus/1.filtered_motus_data_puma_feb_11_2025 - Copy.csv"
csv2_path <- "D:/PUMA-motus/PUMA_Nestling_Tag_SUMMARY_ALL_2024.csv"  # subsetted by individual, from step 2
# Read CSV files
csv1 <- read.csv(csv1_path, stringsAsFactors = FALSE)
csv2 <- read.csv(csv2_path, stringsAsFactors = FALSE)

# Step 1: Filter out rows in csv1 where "tagDeployID" is NA
csv1 <- csv1 %>% filter(!is.na(tagDeployID))

# Step 2: Filter out rows in csv1 where "speciesID" is NA
csv1 <- csv1 %>% filter(!is.na(speciesID))

# Step 3: Filter out rows in csv1 where "tagDeployTest" is 1 or NA
csv1 <- csv1 %>% filter(!tagDeployTest %in% c(1, NA))

# Step 4: Filter out rows in csv1 where "recvDeployName" is "Kennekuk 3"
csv1 <- csv1 %>% filter(recvDeployName != "Kennekuk 3")

# Step 5: Filter csv1 to retain only rows where "tagDeployID" exists in csv2
csv3 <- csv1 %>% filter(tagDeployID %in% csv2$tagDeployID)

# Step 6: Identify and print tagDeployIDs in csv2 that are missing from csv1
missing_tagDeployIDs <- setdiff(csv2$tagDeployID, csv1$tagDeployID)
if (length(missing_tagDeployIDs) > 0) {
  print("These tagDeployID values exist in csv2 but not in csv1:")
  print(missing_tagDeployIDs)
}

# Step 7: Merge csv1 (now filtered) with csv2 based on "tagDeployID"
csv3 <- csv3 %>%
  left_join(csv2, by = "tagDeployID") %>%
  mutate(
    Deploy_Date = as.Date(Date, format = "%m/%d/%Y"),
    Deploy_DOY = yday(Deploy_Date),
    Deploy_Age = Age,
    Detection_Age = Deploy_Age + abs(as.numeric(detectdoy) - Deploy_DOY)
  ) %>%
  select(-Date, -Age)

# Step 8: Rename and ensure necessary columns are treated as strings
csv3 <- csv3 %>%
  mutate(
    Nestling_. = as.character(Nestling_.),
    Home_Tower = as.character(Home_Tower),
    Sex = as.character(Sex),
    T.S.C. = as.character(T.S.C.),
    Nest_Origin = as.character(Nest_Origin),
    Federal_. = as.character(Federal_.),
    Color_. = as.character(Color_.)
  )

# Step 9: Write csv3 to a file
write.csv(csv3, "D:/PUMA-motus/csv3.csv", row.names = FALSE)






#read csv3
csv3_path <- "D:/PUMA-motus/csv3.csv"
csv3 <- read.csv(csv3_path, stringsAsFactors = FALSE)

# Step 12: Remove specified columns from csv3
csv3 <- csv3 %>%
  select(-c(
    hitID, runID, batchID, ts, tsCorrected, sig, sigsd, noise, freq, freqsd, slop, burstSlop, 
    done, nodeNum, tagType, codeSet, mfg, tagModel, nomFreq, tagBI, speciesID, markerType
  ))

# Step 13: Filter out detections that occur before the individual's Deploy_Date
csv3 <- csv3 %>%
  mutate(
    detection_date = as.Date(paste(detectyear, detectdoy), format = "%Y %j")
  ) %>%
  filter(detection_date >= Deploy_Date) %>%
  select(-detection_date)  # Remove temporary column

# Save the updated csv3
write.csv(csv3, "D:/PUMA-motus/csv3-refined.csv", row.names = FALSE)















# Step 10: Create csv4 by filtering out rows where "T.S.C." is "UNKNOWN"
#csv4 <- csv3 %>% filter(T.S.C. != "UNKNOWN")

# Step 11: Write csv4 to a file
#write.csv(csv4, "D:/PUMA-motus/csv4.csv", row.names = FALSE)








#removing remaining pre-deployment detections

# Load necessary library
library(dplyr)
library(lubridate)

#file path
csv4_path <- "D:/PUMA-motus/csv4.csv"  # Replace with actual csv2 file path

# Read CSV files
csv4 <- read.csv(csv4_path, stringsAsFactors = FALSE)

# Step 14: Recalculate Detection_Age properly
csv4.5 <- csv4 %>%
  mutate(
    detection_date = as.Date(paste(detectyear, detectdoy), format = "%Y %j"),
    days_since_deploy = as.numeric(detection_date - Deploy_Date),
    Detection_Age = Deploy_Age + days_since_deploy
  ) %>%
  select(-detection_date, -days_since_deploy)  # Remove temporary columns

# Step 15: Filter out detections where Detection_Age is less than Deploy_Age
csv4.5 <- csv4.5 %>%
  filter(Detection_Age >= Deploy_Age)

# Save the new csv5 file
write.csv(csv5, "D:/PUMA-motus/csv4-filtered.csv", row.names = FALSE)