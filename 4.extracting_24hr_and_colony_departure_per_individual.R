# THIS CODE FILTERS OUT THE MOTUS DATA SUMMARY INFO (AFTER HAVING FILTERED IT MANUALLY)
# TO SPECIFY, I AM EXTRACTING FIRST 24+ HR GAP AWAY FROM HOME TOWER (DOY AND TIME) PER INDIVIDUAL
# AND ALSO EXRTRACTING LAST HOME TOWER DETECTION (DOY AND TIME) PER INDIVIDUAL


# Folder with individual Motus CSVs
folder <- "E:/Desktop_Backup/Work/2025/Motus_download_filtering/fall_2024_individual_motus"

# Output summary file path (NEW)
out_path <- "E:/Desktop_Backup/Work/2025/Motus_download_filtering/new-summary-info/motus_summary_new.csv"

# File with deploy_doy and min_fledge_doy info
info_path <- "E:/Desktop_Backup/Work/2025/Motus_download_filtering/new-summary-info/motus_24hr_gap_and_departure_by_tag_with_sibship.csv"

files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
length(files)

##
first_non_na <- function(x) {
  if (is.null(x)) return(NA)
  x <- x[!is.na(x)]
  x <- x[x != ""]
  if (length(x) == 0) NA else x[1]
}

std_str <- function(x) {
  # standardize strings for comparison: trim + lowercase
  x <- as.character(x)
  x <- trimws(x)
  tolower(x)
}

clean_band_digits <- function(x) {
  # keep only digits (removes dashes, spaces, letters, etc.)
  gsub("[^0-9]", "", as.character(x))
}

## LOAD INFO FILE (min_fledge_doy, deploy_doy)
info_df <- tryCatch(
  read.csv(info_path, stringsAsFactors = FALSE),
  error = function(e) {
    stop("Could not read info file: ", info_path, "\nError: ", e$message)
  }
)

# Expecting column names:
# fed_band, deploy_age, hatch_doy, deploy_doy, X24_hr_doy, X24_hr_time_utc,
# departure_doy, departure_time_utc, min_fledge_doy, dep_age, fate, notes

if (!all(c("fed_band", "deploy_doy", "min_fledge_doy") %in% names(info_df))) {
  stop("Info file does not contain required columns: fed_band, deploy_doy, min_fledge_doy")
}

info_df$fed_clean <- clean_band_digits(info_df$fed_band)

##
all_rows <- vector("list", length(files))

for (i in seq_along(files)) {
  f <- files[i]
  cat("Processing", i, "of", length(files), ":", basename(f), "\n")
  
  df <- tryCatch(
    read.csv(f, stringsAsFactors = FALSE),
    error = function(e) {
      warning("Could not read file: ", f, " - skipping.")
      return(NULL)
    }
  )
  if (is.null(df)) next
  

  ## Extract ID / static info
  federal     <- if ("Federal_."    %in% names(df)) first_non_na(df$Federal_.)    else NA
  color       <- if ("Color_."      %in% names(df)) first_non_na(df$Color_.)      else NA
  tag_id      <- if ("Tag_ID"       %in% names(df)) first_non_na(df$Tag_ID)       else NA
  motus_x     <- if ("motusTagID.x" %in% names(df)) first_non_na(df$motusTagID.x) else NA
  motus_y     <- if ("motusTagID.y" %in% names(df)) first_non_na(df$motusTagID.y) else NA
  tagDeployID <- if ("tagDeployID"  %in% names(df)) first_non_na(df$tagDeployID)  else NA
  home_raw    <- if ("Home_Tower"   %in% names(df)) first_non_na(df$Home_Tower)   else NA
  
  # Look up min_fledge_doy and deploy_doy for this individual (via Federal band)
  min_fledge_doy <- NA_integer_
  deploy_doy_val <- NA_integer_
  if (!is.na(federal)) {
    band_clean <- clean_band_digits(federal)
    if (nzchar(band_clean)) {
      idx_match <- which(info_df$fed_clean == band_clean)
      if (length(idx_match) > 0) {
        idx_use <- idx_match[1]
        min_fledge_doy <- suppressWarnings(as.integer(info_df$min_fledge_doy[idx_use]))
        deploy_doy_val <- suppressWarnings(as.integer(info_df$deploy_doy[idx_use]))
      }
    }
  }
  
  hr24_doy   <- NA
  hr24_time  <- NA
  dep_doy    <- NA
  dep_time   <- NA
  
  have_cols <- all(c("detectyear", "detectdoy", "detecttime") %in% names(df))
  
  if (!is.na(home_raw) && ("recvDeployName" %in% names(df)) && have_cols) {
    
    ## Standardize tower names
    home_std <- std_str(home_raw)
    recv_std <- std_str(df$recvDeployName)
    
    at_home <- !is.na(recv_std) & recv_std == home_std
    
    ## Keep only rows with valid datetime AND (later) subset to home
    year <- suppressWarnings(as.integer(df$detectyear))
    doy  <- suppressWarnings(as.integer(df$detectdoy))
    tstr <- as.character(df$detecttime)
    
    date <- as.Date(sprintf("%d %03d", year, doy), format = "%Y %j")
    dt   <- as.POSIXct(paste(date, tstr), tz = "UTC")
    
    keep_dt <- !is.na(dt)
    df2 <- df[keep_dt, , drop = FALSE]
    dt  <- dt[keep_dt]
    
    if (length(dt) > 0) {
      # order by time
      ord <- order(dt)
      df2 <- df2[ord, , drop = FALSE]
      dt  <- dt[ord]
      
      # recompute at_home in the ordered frame
      recv_std2 <- std_str(df2$recvDeployName)
      at_home2  <- !is.na(recv_std2) & recv_std2 == home_std
      
      if (any(at_home2)) {
        home_idx   <- which(at_home2)
        home_times <- dt[home_idx]
        

        ## Subset to home detections only, with datetime
        sub    <- df2[home_idx, , drop = FALSE]
        t_home <- home_times
        
        # order within home detections just in case (should already be in order)
        ord_h  <- order(t_home)
        sub    <- sub[ord_h, , drop = FALSE]
        t_home <- t_home[ord_h]
        
        n <- length(t_home)
        

        ## Departure: last home-tower detection ever
        dep_doy  <- sub$detectdoy[n]
        dep_time <- sub$detecttime[n]
        
        ## First time away from HOME for >= 24h
        ##
        ## New logic:
        ##  - Compute all gaps_hrs (time until next home detection; last one = Inf).
        ##  - Consider only indices with gap >= 24h.
        ##  - Among those, choose:
        ##      1. First with detectdoy >= min_fledge_doy (if available).
        ##      2. Else first with detectdoy >= deploy_doy.
        ##      3. Else first 24h gap overall.
        ##  - If no 24h gaps at all, fall back to last home detection.
        ## -----------------------------
        
        # DOY at each home detection (ensure integer)
        home_doy <- suppressWarnings(as.integer(sub$detectdoy))
        
        # Gaps in hours between successive home detections; last one is Inf
        if (n == 1) {
          gaps_hrs <- Inf
        } else {
          gaps <- diff(t_home)
          gaps_hrs <- c(as.numeric(gaps, units = "hours"), Inf)
        }
        
        # Indices of home detections that start a >= 24h gap
        idx_24 <- which(gaps_hrs >= 24)
        
        if (length(idx_24) > 0) {
          chosen_idx <- NA_integer_
          
          # 1) First gap with DOY >= min_fledge_doy (if we have it)
          if (!is.na(min_fledge_doy)) {
            cand1 <- idx_24[home_doy[idx_24] >= min_fledge_doy]
            if (length(cand1) > 0) {
              chosen_idx <- cand1[1]
            }
          }
          
          # 2) If none, first gap with DOY >= deploy_doy
          if (is.na(chosen_idx) && !is.na(deploy_doy_val)) {
            cand2 <- idx_24[home_doy[idx_24] >= deploy_doy_val]
            if (length(cand2) > 0) {
              chosen_idx <- cand2[1]
            }
          }
          
          # 3) If still none, just use earliest 24h gap
          if (is.na(chosen_idx)) {
            chosen_idx <- idx_24[1]
          }
          
          first_k <- chosen_idx
        } else {
          # No 24h gaps at all -> fall back to last home detection
          first_k <- n
        }
        
        hr24_doy  <- sub$detectdoy[first_k]
        hr24_time <- sub$detecttime[first_k]
      }
    }
  }
  

  ## Store result for this individual
  all_rows[[i]] <- data.frame(
    Federal_.      = federal,
    Color_.        = color,
    Tag_ID         = tag_id,
    motusTagID.x   = motus_x,
    motusTagID.y   = motus_y,
    tagDeployID    = tagDeployID,
    Home_Tower     = home_raw,
    `24_hr_doy`    = hr24_doy,
    `24_hr_time`   = hr24_time,
    departure_doy  = dep_doy,
    departure_time = dep_time,
    stringsAsFactors = FALSE
  )
}

## =====================================================
## COMBINE & SAVE
## =====================================================

result <- do.call(rbind, all_rows)

head(result)

write.csv(result, out_path, row.names = FALSE)

cat("\nWrote summary file to:\n", out_path, "\n")


## NOW REMOVE DASHES IN FEDERAL BAND CODES IN OUTPUT
# Read the file back
df_out <- read.csv(out_path, stringsAsFactors = FALSE)

# Remove dashes from Federal_. while preserving all other characters
if ("Federal_." %in% names(df_out)) {
  df_out$Federal_. <- gsub("-", "", df_out$Federal_.)
}

# Overwrite the output file
write.csv(df_out, out_path, row.names = FALSE)

cat("Dashes successfully removed from Federal_. and file overwritten:\n", out_path, "\n")

