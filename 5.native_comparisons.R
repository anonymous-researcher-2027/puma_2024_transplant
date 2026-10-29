### Comparing native == "Y" for fledge_doy, X24_hr_doy, departure_doy,  
### between diff bio_pop values.
### Only for individuals with fate == "lived"

# Read data
dat <- read.csv(
  "C:/Users/tiger/Desktop/puma/2024 transplant/motus_summary_21jan2026.csv",
  stringsAsFactors = FALSE
)

# Filter for fate == "lived" and native == "Y"
# No need to filter out blank bio_pop values since those would have native=U
dat_native_lived <- subset(dat, fate == "lived" & native == "Y")

# Set bio_pop as factor
dat_native_lived$bio_pop <- factor(dat_native_lived$bio_pop)

cat("bio_pop values included:\n")
print(levels(dat_native_lived$bio_pop))

cat("\n rows after filtering (fate == 'lived' & native == 'Y'):",
    nrow(dat_native_lived), "\n\n")

# Run and report t-test and group summaries
compare_var <- function(var_name) {
  cat("------------------------------------\n")
  cat("Variable:", var_name, "\n\n")
  
  x   <- dat_native_lived[[var_name]]
  grp <- dat_native_lived$bio_pop
  
  # Group summaries
  cat("Group means (na.rm = TRUE):\n")
  print(tapply(x, grp, mean, na.rm = TRUE))
  cat("\nGroup SDs:\n")
  print(tapply(x, grp, sd, na.rm = TRUE))
  cat("\nGroup ns:\n")
  print(tapply(x, grp, function(z) sum(!is.na(z))))
  cat("\n")
  
  # Two-sample t-test between bio_pop values
  cat("Two-sample t-test by bio_pop:\n")
  print(t.test(x ~ grp))
  cat("\n")
}

# Run comparisons
compare_var("X24_hr_doy")
compare_var("departure_doy")
compare_var("fledge_doy")




# Make three-panel t-test-based plots for timing 
plot_t_panel <- function(data, response, main_title) {
  d <- data[!is.na(data[[response]]) & !is.na(data$bio_pop), ]
  y <- d[[response]]
  g <- droplevels(d$bio_pop)
  
  tt    <- t.test(y ~ g)
  pval  <- tt$p.value
  p_txt <- if (pval < 0.001) "p < 0.001" else sprintf("p = %.3f", pval)
  
  means <- tapply(y, g, mean, na.rm = TRUE)
  ns    <- tapply(y, g, function(z) sum(!is.na(z)))
  
  boxplot(y ~ g,
          xlab = "Biological Origin",
          ylab = "Day of Year",
          main = main_title,
          border = "grey40",
          col = "grey90",
          outline = FALSE)
  
  stripchart(y ~ g, vertical = TRUE, method = "jitter",
             pch = 16, col = "black", add = TRUE)
  
  for (i in seq_along(means)) {
    segments(i, means[i], i, means[i], col = "blue", lwd = 3)
  }
  
  usr   <- par("usr")
  xmid  <- mean(usr[1:2])
  xleft <- usr[1] + 0.05 * (usr[2] - usr[1])
  yrng  <- usr[4] - usr[3]
  
  text(xmid, usr[4] - 0.05 * yrng, p_txt)
  text(xleft, usr[3] + 0.10 * yrng, paste0(names(ns)[1], ": n = ", ns[1]), adj = c(0, 0.5))
  text(xleft, usr[3] + 0.04 * yrng, paste0(names(ns)[2], ": n = ", ns[2]), adj = c(0, 0.5))
}

# Three-panel figure for native == "Y" individuals
par(mfrow = c(1, 3), mar = c(5, 4, 4, 2) + 0.1)

plot_t_panel(dat_native_lived, "fledge_doy",
             "a. Fledging Date")

plot_t_panel(dat_native_lived, "X24_hr_doy",
             "b. First 24-hr Colony Absence Date")

plot_t_panel(dat_native_lived, "departure_doy",
             "c. Final Colony Departure Date")



















### Comparing native == "Y" for fledge_doy, X24_hr_doy, departure_doy,  
### between diff bio_pop values.
### Only for individuals with fate == "lived"

# Read data
dat <- read.csv(
  "C:/Users/tiger/Desktop/puma/2024 transplant/motus_summary_21jan2026.csv",
  stringsAsFactors = FALSE
)

# Filter for fate == "lived" and native == "Y"
# No need to filter out blank bio_pop values since those would have native=U
dat_native_lived <- subset(dat, fate == "lived" & native == "Y")

# Set bio_pop as factor
dat_native_lived$bio_pop <- factor(dat_native_lived$bio_pop)

cat("bio_pop values included:\n")
print(levels(dat_native_lived$bio_pop))

cat("\n rows after filtering (fate == 'lived' & native == 'Y'):",
    nrow(dat_native_lived), "\n\n")

# Run and report t-test and group summaries
compare_var <- function(var_name) {
  cat("------------------------------------\n")
  cat("Variable:", var_name, "\n\n")
  
  x   <- dat_native_lived[[var_name]]
  grp <- dat_native_lived$bio_pop
  
  # Group summaries
  cat("Group means (na.rm = TRUE):\n")
  print(tapply(x, grp, mean, na.rm = TRUE))
  cat("\nGroup SDs:\n")
  print(tapply(x, grp, sd, na.rm = TRUE))
  cat("\nGroup ns:\n")
  print(tapply(x, grp, function(z) sum(!is.na(z))))
  cat("\n")
  
  # Two-sample t-test between bio_pop values
  cat("Two-sample t-test by bio_pop:\n")
  print(t.test(x ~ grp))
  cat("\n")
}

# Run comparisons
compare_var("X24_hr_doy")
compare_var("departure_doy")
compare_var("fledge_doy")

# ---- Min/Max DOY for each timing metric (native == "Y", fate == "lived") ----

timing_vars <- c("fledge_doy", "X24_hr_doy", "departure_doy")

cat("\n============================\n")
cat("Min/Max DOY for timing metrics\n")
cat("(native == 'Y' & fate == 'lived')\n")
cat("============================\n\n")

for (v in timing_vars) {
  x <- dat_native_lived[[v]]
  n_non_na <- sum(!is.na(x))
  
  cat("Variable:", v, "\n")
  cat("  n (non-NA):", n_non_na, "\n")
  
  if (n_non_na == 0) {
    cat("  min:", NA, "\n")
    cat("  max:", NA, "\n\n")
  } else {
    cat("  min:", min(x, na.rm = TRUE), "\n")
    cat("  max:", max(x, na.rm = TRUE), "\n\n")
  }
}

# Make three-panel t-test-based plots for timing 
plot_t_panel <- function(data, response, main_title) {
  d <- data[!is.na(data[[response]]) & !is.na(data$bio_pop), ]
  y <- d[[response]]
  g <- droplevels(d$bio_pop)
  
  tt    <- t.test(y ~ g)
  pval  <- tt$p.value
  p_txt <- if (pval < 0.001) "p < 0.001" else sprintf("p = %.3f", pval)
  
  means <- tapply(y, g, mean, na.rm = TRUE)
  ns    <- tapply(y, g, function(z) sum(!is.na(z)))
  
  boxplot(y ~ g,
          xlab = "Biological Origin",
          ylab = "Day of Year",
          main = main_title,
          border = "grey40",
          col = "grey90",
          outline = FALSE,
          ylim = c(200, 220))
  
  stripchart(y ~ g, vertical = TRUE, method = "jitter",
             pch = 16, col = "black", add = TRUE)
  
  for (i in seq_along(means)) {
    segments(i, means[i], i, means[i], col = "blue", lwd = 3)
  }
  
  usr   <- par("usr")
  xmid  <- mean(usr[1:2])
  xleft <- usr[1] + 0.05 * (usr[2] - usr[1])
  yrng  <- usr[4] - usr[3]
  
  text(xmid, usr[4] - 0.05 * yrng, p_txt)
  text(xleft, usr[3] + 0.10 * yrng, paste0(names(ns)[1], ": n = ", ns[1]), adj = c(0, 0.5))
  text(xleft, usr[3] + 0.04 * yrng, paste0(names(ns)[2], ": n = ", ns[2]), adj = c(0, 0.5))
}

# Three-panel figure for native == "Y" individuals
par(mfrow = c(1, 3), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 3, 0))

plot_t_panel(dat_native_lived, "fledge_doy",
             "a. Fledging Date")

plot_t_panel(dat_native_lived, "X24_hr_doy",
             "b. First 24-hr Colony Absence Date")

plot_t_panel(dat_native_lived, "departure_doy",
             "c. Final Colony Departure Date")

#mtext("Baseline Population Differences", outer = TRUE, cex = 1.5, line = 1)

