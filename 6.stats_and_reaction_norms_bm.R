### First, to produce stats for the modeled data Chisq, pvalues, etc.
### Each timing metric has its own best model 
### Best models were determined from model_dredging.R
### THIS VERSION ONLY USED THE TOP MODEL. I have since hashtagged all this out. 
### Please see the code which uses a model-averaging approach in the file:
### 'model-averaging_stats.R'

# Library
library(lme4)
library(lmerTest)  # p-values in summary()
library(car)       # Anova(..., test.statistic="Chisq")

# Read data
dat <- read.csv("C:/Users/tiger/Desktop/puma/2024 transplant/motus_summary_21jan2026.csv",
                stringsAsFactors = FALSE)

# Filter for fate == "lived" & remove MBY12 bc it is missing bio_pop
dat <- subset(dat, fate == "lived" & color_band != "MBY12")

# setting factors
dat$raised_nest <- factor(dat$raised_nest)
dat$bio_pop     <- factor(dat$bio_pop)
dat$raised_pop  <- factor(dat$raised_pop)
dat$sex         <- factor(dat$sex, levels = c("F","M","U"))
dat$pair_age    <- factor(dat$pair_age)
dat$site        <- factor(dat$site)
dat$eggs <- as.numeric(dat$eggs)
dat$sibs_hatched <- as.numeric(dat$sibs_hatched)
dat$sibs_fledged <- as.numeric(dat$sibs_fledged)
dat$hatch_doy <- as.numeric(dat$hatch_doy)

# For Type-III tests
options(contrasts = c("contr.sum", "contr.poly"))
timing_vars <- c("fledge_doy", "X24_hr_doy", "departure_doy")
fit_one <- function(resp, dat) {
  # Defining model for each timing metric
  fml <- switch(
    resp,
    "fledge_doy" = fledge_doy ~ bio_pop * raised_pop + eggs + sibs_hatched + sibs_fledged + hatch_doy + (1 | raised_nest),
    "X24_hr_doy" = X24_hr_doy ~ bio_pop * raised_pop + site + hatch_doy + (1 | raised_nest),
    "departure_doy" = departure_doy ~ bio_pop * raised_pop + site + hatch_doy + (1 | raised_nest),
    stop("Unknown response: ", resp)
  )
  
  mod <- lmer(fml, data = dat, REML = FALSE)
  
  cat("\n==============================\n")
  cat("Response:", resp, "\n")
  cat("==============================\n")
  
  # Coefficient table with p-values (Satterthwaite)
  print(summary(mod))
  
  # Chi-square / df / p-value table for fixed effects Type-III Wald tests
  a3 <- car::Anova(mod, type = 3, test.statistic = "Chisq")
  print(a3)
  
  # Return a clean data.frame for table
  tab <- as.data.frame(a3)
  tab$term <- rownames(tab)
  rownames(tab) <- NULL
  tab$response <- resp
  
  list(model = mod, anova_chisq = tab)
}

fits <- lapply(timing_vars, fit_one, dat = dat)

# Combined table all 3 timing metrics:
anova_table_all <- do.call(rbind, lapply(fits, `[[`, "anova_chisq"))

# (X^2, df, p-value) + identifiers
anova_table_all <- anova_table_all[, c("response", "term", "Chisq", "Df", "Pr(>Chisq)")]

cat("\n\n===== Combined Chi-square table (all responses) =====\n")
print(anova_table_all, row.names = FALSE)





















## ============================================================
## Libraries
## ============================================================

library(lme4)      # for lmer
library(car)       # for car::Anova
library(visreg)    # for visreg plots
library(dplyr)     # for count()
library(knitr)     # for kable() tables

## ============================================================
## Read data
## ============================================================

dat <- read.csv(
  "C:\\Users\\tiger\\Desktop\\puma\\2024 transplant\\motus_summary_21jan2026.csv",
  stringsAsFactors = FALSE
)

cat("Rows in raw data:", nrow(dat), "\n")

## ============================================================
## Filter to fate == 'lived'; remove MBY12
## ============================================================

dat <- subset(dat, fate == "lived" & color_band != "MBY12")
cat("Rows after fate == 'lived':", nrow(dat), "\n")

## ============================================================
## Treatment recode
## C,S -> 'native'; T -> 'non-native'
## ============================================================

dat$treatment <- gsub("C", "native",     dat$treatment)
dat$treatment <- gsub("S", "native",     dat$treatment)
dat$treatment <- gsub("T", "non-native", dat$treatment)

counts_df <- dat %>%
  count(treatment, raised_pop, name = "count")
cat("\nCounts by treatment and raised_pop (fate == 'lived'):\n")
print(counts_df)

## ============================================================
## Clean sex: blank / NA -> "U", then factor later
## ============================================================
#dat$sex[dat$sex == ""] <- NA
#dat$sex[is.na(dat$sex)] <- "U"

## ============================================================
## variable coding
## ============================================================

## Factors
dat$bio_pop      <- as.factor(dat$bio_pop)
dat$raised_pop   <- factor(dat$raised_pop, levels = c("MB","MN"))  
dat$sex          <- as.factor(dat$sex)
dat$eggs         <- as.numeric(dat$eggs)
dat$sibs_hatched <- as.numeric(dat$sibs_hatched)
dat$sibs_fledged <- as.numeric(dat$sibs_fledged)
dat$raised_nest <- factor(dat$raised_nest)
dat$pair_age    <- factor(dat$pair_age)
dat$site        <- factor(dat$site)
dat$hatch_doy <- as.numeric(dat$hatch_doy)
dat$fledge_doy    <- as.numeric(dat$fledge_doy)
dat$X24_hr_doy    <- as.numeric(dat$X24_hr_doy)
dat$departure_doy <- as.numeric(dat$departure_doy)

cat("\nNon-NA fledge_doy:",    sum(!is.na(dat$fledge_doy)),    "\n")
cat("Non-NA X24_hr_doy:",     sum(!is.na(dat$X24_hr_doy)),     "\n")
cat("Non-NA departure_doy:",  sum(!is.na(dat$departure_doy)),  "\n\n")

## Color scheme for populations
col_pop <- c(MB = "lightblue", MN = "orange")

## ============================================================
## Helper: reaction norm summaries (means, SD, SE, CI) by bio_pop x raised_pop
## ============================================================

reaction_summary <- function(data, response, response_label) {
  dsub <- data[!is.na(data[[response]]), ]
  
  cells <- with(
    dsub,
    split(dsub[[response]], list(bio_pop, raised_pop), drop = TRUE)
  )
  
  cell_names <- names(cells)
  n  <- sapply(cells, function(z) sum(!is.na(z)))
  mu <- sapply(cells, function(z) mean(z, na.rm = TRUE))
  sd <- sapply(cells, function(z) sd(z,   na.rm = TRUE))
  se <- sd / sqrt(n)
  ci_half <- qt(0.975, df = n - 1) * se
  lower <- mu - ci_half
  upper <- mu + ci_half
  
  bp <- sub("\\..*$", "", cell_names)
  rp <- sub("^.*\\.", "", cell_names)
  
  out <- data.frame(
    response    = response_label,
    bio_pop     = bp,
    raised_pop  = rp,
    mean        = mu,
    sd          = sd,
    n           = n,
    se          = se,
    ci_lower    = lower,
    ci_upper    = upper,
    row.names   = NULL,
    stringsAsFactors = FALSE
  )
  
  out[order(out$bio_pop, out$raised_pop), ]
}

## ============================================================
## Reaction norm: build tables for the 3 DOY timing metrics
## ============================================================

tab_fledge_doy_react <- reaction_summary(
  dat, "fledge_doy",
  "Fledge Date (DOY)"
)

tab_X24_doy_react <- reaction_summary(
  dat, "X24_hr_doy",
  "First 24-hr Colony Absence Date (DOY)"
)

tab_depart_doy_react <- reaction_summary(
  dat, "departure_doy",
  "Final Colony Departure Date (DOY)"
)

cat("Reaction norm summary: fledge_doy\n")
kable(tab_fledge_doy_react, digits = 2)

cat("\nReaction norm summary: X24_hr_doy\n")
kable(tab_X24_doy_react, digits = 2)

cat("\nReaction norm summary: departure_doy\n")
kable(tab_depart_doy_react, digits = 2)

## Total n for each response
n_fledge_doy <- sum(!is.na(dat$fledge_doy))
n_X24_doy    <- sum(!is.na(dat$X24_hr_doy))
n_depart_doy <- sum(!is.na(dat$departure_doy))

## ============================================================
## Reaction norm plotting function
## (fixed y-axis 200-220, all individual points, NO p-values)
## ============================================================

plot_reaction_norm <- function(tab, data, response_var, n_total, main_title) {
  tab <- tab[order(tab$bio_pop, tab$raised_pop), ]
  
  x_vals <- c(1, 2)  # MB, MN for raised_pop
  names(x_vals) <- c("MB", "MN")
  
  plot(NA,
       xlim = c(0.5, 2.5),
       ylim = c(200, 220),
       xaxt = "n",
       xlab = "Rearing Population",
       ylab = "Day of Year",
       main = main_title)
  axis(1, at = c(1, 2), labels = c("MB", "MN"))
  
  ## Individual points (jittered), colored by bio_pop
  dsub <- data[!is.na(data[[response_var]]), ]
  x_jitter <- x_vals[as.character(dsub$raised_pop)] +
    runif(nrow(dsub), min = -0.05, max = 0.05)
  points(
    x_jitter,
    dsub[[response_var]],
    col = col_pop[as.character(dsub$bio_pop)],
    pch = 16,
    cex = 0.7
  )
  
  ## For each bio_pop, draw line + CI bars with horizontal caps
  for (bp in c("MB", "MN")) {
    sub_bp <- tab[tab$bio_pop == bp, ]
    xs   <- x_vals[sub_bp$raised_pop]
    ys   <- sub_bp$mean
    lows <- sub_bp$ci_lower
    ups  <- sub_bp$ci_upper
    
    for (i in seq_along(xs)) {
      segments(xs[i], lows[i], xs[i], ups[i],
               col = col_pop[bp], lwd = 1.5)
      cap_width <- 0.08
      segments(xs[i] - cap_width, lows[i],
               xs[i] + cap_width, lows[i],
               col = col_pop[bp], lwd = 1.5)
      segments(xs[i] - cap_width, ups[i],
               xs[i] + cap_width, ups[i],
               col = col_pop[bp], lwd = 1.5)
    }
    lines(xs, ys, col = col_pop[bp], lwd = 2)
    points(xs, ys, col = col_pop[bp], pch = 16, cex = 1.3)
  }
  
  legend("topright",
         legend = c("MB origin", "MN origin"),
         col    = col_pop,
         pch    = 16,
         lwd    = 2,
         title  = "Biological Origin",
         bty    = "n")
  
  usr    <- par("usr")
  y_bot  <- usr[3]
  x_left <- usr[1] + 0.05 * (usr[2] - usr[1])
  
  text(x_left, y_bot + 0.05*(usr[4]-usr[3]),
       labels = paste0("n = ", n_total),
       cex = 0.9, adj = c(0, 0.5))
}

## ============================================================
## Make reaction norm figure (three panels)
## ============================================================

par(mfrow = c(1, 3), mar = c(5, 4, 4, 2) + 0.1)

## Panel a: fledge_doy
plot_reaction_norm(
  tab          = tab_fledge_doy_react,
  data         = dat,
  response_var = "fledge_doy",
  n_total      = n_fledge_doy,
  main_title   = "a. Fledge Date"
)

## Panel b: X24_hr_doy
plot_reaction_norm(
  tab          = tab_X24_doy_react,
  data         = dat,
  response_var = "X24_hr_doy",
  n_total      = n_X24_doy,
  main_title   = "b. First 24-hr Colony Absence Date"
)

## Panel c: departure_doy
plot_reaction_norm(
  tab          = tab_depart_doy_react,
  data         = dat,
  response_var = "departure_doy",
  n_total      = n_depart_doy,
  main_title   = "c. Final Colony Departure Date"
)












###Unmodeled raw value stats 2-way ANOVA & F tests
## This is for what is currently Table S5 (as of Feb 27 2026)
## Raw (unmodeled) test to go with reaction norms
## Two-way ANOVA via lm: response ~ bio_pop * raised_pop
## F-tests (Type III) for main effects and interaction

# Use sum-to-zero contrasts so Type III tests are meaningful
options(contrasts = c("contr.sum", "contr.poly"))

raw_rn_test <- function(data, response_var, response_label) {
  dsub <- data[!is.na(data[[response_var]]), c("bio_pop", "raised_pop", response_var)]
  names(dsub)[3] <- "y"
  
  # Make sure factors are factors
  dsub$bio_pop    <- factor(dsub$bio_pop)
  dsub$raised_pop <- factor(dsub$raised_pop)
  
  cat("\n==============================\n")
  cat("RAW reaction-norm test:", response_label, "\n")
  cat("Model: y ~ bio_pop * raised_pop (lm; Type III F-tests)\n")
  cat("n =", nrow(dsub), "\n")
  cat("==============================\n")
  
  fit <- lm(y ~ bio_pop * raised_pop, data = dsub)
  
  # Type III F-tests for terms (main effects + interaction)
  a3F <- car::Anova(fit, type = 3)  # F-tests by default for lm
  print(a3F)
  
  invisible(list(model = fit, anova_F = as.data.frame(a3F)))
}

raw_fledge  <- raw_rn_test(dat, "fledge_doy",    "Fledge Date (DOY)")
raw_X24     <- raw_rn_test(dat, "X24_hr_doy",    "First 24-hr Colony Absence Date (DOY)")
raw_depart  <- raw_rn_test(dat, "departure_doy", "Final Colony Departure Date (DOY)")





























## ============================================================
## Libraries
## ============================================================

library(lme4)
library(visreg)

## ============================================================
## Read + filter data
## ============================================================

dat <- read.csv(
  "C:\\Users\\tiger\\Desktop\\puma\\2024 transplant\\motus_summary_21jan2026.csv",
  stringsAsFactors = FALSE
)

dat <- subset(dat, fate == "lived")
dat$sex[dat$sex == ""] <- NA
dat$sex[is.na(dat$sex)] <- "U"
dat <- subset(dat, color_band != "MBY12")

## ============================================================
## Variable coding
## ============================================================

dat$bio_pop       <- as.factor(dat$bio_pop)
dat$raised_pop    <- factor(dat$raised_pop, levels = c("MB", "MN"))
dat$sex           <- as.factor(dat$sex)
dat$eggs          <- factor(dat$eggs)
dat$sibs_hatched  <- factor(dat$sibs_hatched)
dat$sibs_fledged  <- factor(dat$sibs_fledged)
dat$raised_nest   <- factor(dat$raised_nest)
dat$pair_age      <- factor(dat$pair_age)
dat$site          <- factor(dat$site)

dat$hatch_doy     <- as.numeric(dat$hatch_doy)
dat$fledge_doy    <- as.numeric(dat$fledge_doy)
dat$X24_hr_doy    <- as.numeric(dat$X24_hr_doy)
dat$departure_doy <- as.numeric(dat$departure_doy)

## ============================================================
## Fit the 3 models
## ============================================================

m_fledge <- lmer(
  fledge_doy ~ bio_pop * raised_pop + eggs +
    sibs_hatched + sibs_fledged + hatch_doy +
    (1 | nest_id),
  data = dat,
  REML = FALSE
)

m_x24 <- lmer(
  X24_hr_doy ~ bio_pop * raised_pop + site +
    hatch_doy + (1 | nest_id),
  data = dat,
  REML = FALSE
)

m_depart <- lmer(
  departure_doy ~ bio_pop * raised_pop + site +
    hatch_doy + (1 | nest_id),
  data = dat,
  REML = FALSE
)

## ============================================================
## Helper for visreg panel with n
## ============================================================

plot_visreg_with_n <- function(mod, xvar, main_title,
                               xlab = "Rearing Population",
                               ylab = "Day of Year",
                               partial = TRUE,
                               point_cex = 1.15) {
  
  visreg(
    mod, xvar,
    partial = partial,
    xlab    = xlab,
    ylab    = ylab,
    main    = main_title,
    cex     = point_cex
  )
  
  usr    <- par("usr")
  x_left <- usr[1] + 0.05 * (usr[2] - usr[1])
  y_bot  <- usr[3] + 0.05 * (usr[4] - usr[3])
  
  text(
    x = x_left,
    y = y_bot,
    labels = paste0("n = ", nobs(mod)),
    cex = 0.9,
    adj = c(0, 0)
  )
}

## ============================================================
## Single 3-panel figure: raised_pop for all timing metrics
## ============================================================

par(mfrow = c(1, 3), mar = c(5, 4, 4, 2) + 0.1)

plot_visreg_with_n(
  mod        = m_fledge,
  xvar       = "raised_pop",
  main_title = "a. Fledge Date"
)

plot_visreg_with_n(
  mod        = m_x24,
  xvar       = "raised_pop",
  main_title = "b. First 24-hr Colony Absence Date"
)

plot_visreg_with_n(
  mod        = m_depart,
  xvar       = "raised_pop",
  main_title = "c. Final Colony Departure Date"
)

## Optional: reset plotting layout afterward
par(mfrow = c(1, 1))

