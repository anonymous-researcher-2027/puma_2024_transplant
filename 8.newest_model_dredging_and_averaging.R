# Model dredging for all 3 timing metrics using DOY not using age
# Note: Always included in every model: bio_pop * raised_pop, hatch_doy
# Random effect: (1 | raised_nest)
# One dredge per timing metric:
# 1. all retained individuals

# Load packages
library(lme4)
library(MuMIn)

# Load data and filter for fate + filter out MBY12 (it is missing bio_pop)
dat <- read.csv(
  "C:\\Users\\tiger\\Desktop\\puma\\2024 transplant\\motus_summary_21jan2026.csv",
  stringsAsFactors = FALSE
)
dat <- subset(dat, fate == "lived" & color_band != "MBY12")

# Set classes
dat$raised_nest   <- factor(dat$raised_nest)
dat$bio_pop       <- factor(dat$bio_pop)
dat$raised_pop    <- factor(dat$raised_pop)
dat$sex           <- factor(dat$sex, levels = c("F", "M", "U"))
dat$pair_age      <- factor(dat$pair_age)
#dat$eggs          <- as.numeric(dat$eggs) #removed for kevin
dat$sibs_hatched  <- as.numeric(dat$sibs_hatched)
#dat$sibs_fledged  <- as.numeric(dat$sibs_fledged) #removed for kevin
dat$hatch_doy     <- as.numeric(dat$hatch_doy)
#dat$deploy_doy    <- as.numeric(dat$deploy_doy) #removed for kevin
#dat$site      <- factor(dat$site)

# Dredging
run_dredge_for_metric <- function(metric, dat_in) {
  
  cat("\n============================================================\n")
  cat("TIMING METRIC:", metric, "\n")
  cat("============================================================\n")
  
  d <- dat_in[!is.na(dat_in[[metric]]) & is.finite(dat_in[[metric]]), , drop = FALSE]
  cat("Rows available:", nrow(d), "\n")
  if (nrow(d) < 5) return(invisible(NULL))
  
  global <- lmer(
    as.formula(paste0(
      metric, " ~ bio_pop * raised_pop + hatch_doy + ",
      "sex + pair_age + sibs_hatched + site +",
      "(1|raised_nest)" # removed deploy_doy # turned site into a random effect
    )),
    data = d,
    na.action = na.fail,
    REML = FALSE
  )
  
  cat("\n--- DREDGE: all retained individuals ---\n")
  dd1 <- dredge(
    global,
    fixed = c("bio_pop:raised_pop", "hatch_doy")
  )
  
  cat("Total models tested:", nrow(dd1), "\n")
  
  print(head(dd1, 20))
  cat("\nTop models (delta <= 2):\n")
  print(subset(dd1, delta <= 2))
  cat("\nBest model summary:\n")
  print(summary(get.models(dd1, 1)[[1]]))
  
  invisible(NULL)
}

# Run for each timing metric
timing_metrics <- c("fledge_doy", "X24_hr_doy", "departure_doy")

for (m in timing_metrics) {
  run_dredge_for_metric(m, dat)
}


















############################################################
### Model averaging / single-model handling for 3 timing metrics
### Create publication-style table + export to Word
### NOTE: site dummy rows are removed ONLY from the publication table
###       but retained in the raw coefficient table
############################################################

library(lme4)
library(MuMIn)
library(dplyr)
library(stringr)
library(tibble)
library(flextable)
library(officer)

############################################################
### Read data
############################################################

dat <- read.csv(
  "C:/Users/tiger/Desktop/puma/2024 transplant/motus_summary_21jan2026.csv",
  stringsAsFactors = FALSE
)

# Filter
dat <- subset(dat, fate == "lived" & color_band != "MBY12")

# Set classes
dat$raised_nest   <- factor(dat$raised_nest)
dat$bio_pop       <- factor(dat$bio_pop)
dat$raised_pop    <- factor(dat$raised_pop)
dat$sex           <- factor(dat$sex, levels = c("F", "M", "U"))
dat$pair_age      <- factor(dat$pair_age)
#dat$eggs          <- as.numeric(dat$eggs)
dat$sibs_hatched  <- as.numeric(dat$sibs_hatched)
#dat$sibs_fledged  <- as.numeric(dat$sibs_fledged)
dat$hatch_doy     <- as.numeric(dat$hatch_doy)
#dat$deploy_doy    <- as.numeric(dat$deploy_doy)
dat$site      <- factor(dat$site)

############################################################
### MANUALLY ENTER SUPPORTED MODELS HERE
############################################################

model_formulas <- list(
  
  fledge_doy = list(
    fledge_doy ~ bio_pop * raised_pop + hatch_doy + sibs_hatched + (1 | raised_nest),
    fledge_doy ~ bio_pop * raised_pop + hatch_doy + (1 | raised_nest)
  ),
  
  X24_hr_doy = list(
    X24_hr_doy ~ bio_pop * raised_pop + hatch_doy + site + (1 | raised_nest),
  ),
  
  departure_doy = list(
    departure_doy ~ bio_pop * raised_pop + hatch_doy + site + (1 | raised_nest),
    departure_doy ~ bio_pop * raised_pop + hatch_doy + (1 | raised_nest)
    
  )
)

############################################################
### Labels for table sections
############################################################

section_labels <- c(
  fledge_doy    = "(a) fledge date",
  X24_hr_doy    = "(b) first 24-hr colony absence date",
  departure_doy = "(c) final colony departure date"
)

############################################################
### Make terms prettier for publication table
############################################################

pretty_term <- function(x) {
  x <- gsub("\\(Intercept\\)", "(intercept)", x)
  x <- gsub("bio_pop", "biological population", x)
  x <- gsub("raised_pop", "rearing population", x)
  x <- gsub("hatch_doy", "hatch date", x)
  x <- gsub("sibs_hatched", "siblings hatched", x)
  x <- gsub("sibs_fledged", "siblings fledged", x)
  x <- gsub("eggs", "eggs", x)
  x <- gsub(":", " × ", x)
  x
}

############################################################
### Helper to format numbers for publication table
############################################################

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "", format(round(x, digits), nsmall = digits, trim = TRUE))
}

############################################################
### Helper: terms to omit from publication table only
############################################################

omit_from_publication_table <- function(term_vec) {
  grepl("^site[0-9]+$", term_vec)
}

############################################################
### Extract fixed-effect table
############################################################

extract_fixed_table <- function(model_or_avg, resp, mode = c("single", "averaged")) {
  
  mode <- match.arg(mode)
  
  if (mode == "single") {
    s <- summary(model_or_avg)
    
    coef_tab <- as.data.frame(s$coefficients) %>%
      rownames_to_column("term") %>%
      dplyr::select(term, Estimate, `Std. Error`) %>%
      dplyr::rename(SE = `Std. Error`)
    
    ci_tab <- suppressMessages(
      as.data.frame(confint(model_or_avg, method = "Wald"))
    ) %>%
      rownames_to_column("term")
    names(ci_tab)[2:3] <- c("lower_CI", "upper_CI")
    
    out <- left_join(coef_tab, ci_tab, by = "term")
  }
  
  if (mode == "averaged") {
    s <- summary(model_or_avg)
    
    coef_tab <- as.data.frame(s$coefmat.full) %>%
      rownames_to_column("term") %>%
      dplyr::select(term, Estimate, `Adjusted SE`) %>%
      dplyr::rename(SE = `Adjusted SE`)
    
    ci_tab <- as.data.frame(confint(model_or_avg, full = TRUE)) %>%
      rownames_to_column("term")
    names(ci_tab)[2:3] <- c("lower_CI", "upper_CI")
    
    out <- left_join(coef_tab, ci_tab, by = "term")
  }
  
  out <- out %>%
    mutate(response = resp) %>%
    dplyr::select(response, term, Estimate, SE, lower_CI, upper_CI)
  
  out
}

############################################################
### Fit models for one response
############################################################

fit_avg_models <- function(resp, dat, formulas) {
  
  cat("\n=====================================================\n")
  cat("Response:", resp, "\n")
  cat("=====================================================\n")
  
  d <- dat[!is.na(dat[[resp]]) & is.finite(dat[[resp]]), , drop = FALSE]
  cat("Rows available:", nrow(d), "\n")
  
  fits <- lapply(formulas, function(fml) {
    lmer(fml, data = d, REML = FALSE, na.action = na.fail)
  })
  names(fits) <- paste0("m", seq_along(fits))
  
  ms <- model.sel(fits)
  print(ms)
  
  ##########################################################
  # CASE 1: only one supported model
  ##########################################################
  if (length(fits) == 1) {
    
    cat("\n--- Only one model supplied: using single-model output ---\n")
    
    mod <- fits[[1]]
    print(summary(mod))
    
    coef_tab <- extract_fixed_table(mod, resp, mode = "single")
    
    return(list(
      fits = fits,
      model_selection = ms,
      averaged_model = NULL,
      single_model = mod,
      coef_table = coef_tab,
      n_rows = nrow(d),
      mode = "single"
    ))
  }
  
  ##########################################################
  # CASE 2: multiple supported models
  ##########################################################
  
  avg_mod <- model.avg(fits)
  
  cat("\n--- Model-averaged summary ---\n")
  print(summary(avg_mod))
  
  coef_tab <- extract_fixed_table(avg_mod, resp, mode = "averaged")
  
  list(
    fits = fits,
    model_selection = ms,
    averaged_model = avg_mod,
    single_model = NULL,
    coef_table = coef_tab,
    n_rows = nrow(d),
    mode = "averaged"
  )
}

############################################################
### Run all 3 responses
############################################################

results <- lapply(names(model_formulas), function(resp) {
  fit_avg_models(resp, dat, model_formulas[[resp]])
})
names(results) <- names(model_formulas)

############################################################
### Build publication-style section table
############################################################

make_section_table <- function(resp, result_obj, section_label) {
  
  tab <- result_obj$coef_table %>%
    filter(!omit_from_publication_table(term)) %>%
    mutate(
      `predictor variables` = pretty_term(term),
      `parameter estimates` = fmt_num(Estimate, 3),
      `standard errors`     = fmt_num(SE, 3),
      `lower CI`            = fmt_num(lower_CI, 3),
      `Upper CI`            = fmt_num(upper_CI, 3)
    ) %>%
    dplyr::select(
      `predictor variables`,
      `parameter estimates`,
      `standard errors`,
      `lower CI`,
      `Upper CI`
    )
  
  header_row <- data.frame(
    `predictor variables` = paste0(section_label, " (n = ", result_obj$n_rows, ")"),
    `parameter estimates` = "",
    `standard errors` = "",
    `lower CI` = "",
    `Upper CI` = "",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  tab <- as.data.frame(tab, stringsAsFactors = FALSE)
  
  bind_rows(header_row, tab)
}

paper_table <- bind_rows(
  make_section_table("fledge_doy",    results[["fledge_doy"]],    section_labels["fledge_doy"]),
  make_section_table("X24_hr_doy",    results[["X24_hr_doy"]],    section_labels["X24_hr_doy"]),
  make_section_table("departure_doy", results[["departure_doy"]], section_labels["departure_doy"])
)

############################################################
### Print clean publication table in R
############################################################

cat("\n\n===== Publication-style table =====\n")
print(paper_table, row.names = FALSE)

############################################################
### Save publication table as CSV
############################################################

write.csv(
  paper_table,
  "C:/Users/tiger/Desktop/puma/2024 transplant/model_results_publication_table.csv",
  row.names = FALSE,
  na = ""
)

############################################################
### Save publication table directly to Word
############################################################

ft <- flextable(paper_table)

section_rows <- which(
  grepl("^\\([a-z]\\)", paper_table$`predictor variables`)
)

ft <- bold(ft, i = section_rows, bold = TRUE, part = "body")
ft <- autofit(ft)
ft <- align(ft, align = "left", part = "all")
ft <- valign(ft, valign = "center", part = "all")

doc <- read_docx()
doc <- body_add_par(doc, "Model results table", style = "heading 1")
doc <- body_add_flextable(doc, ft)

print(
  doc,
  target = "C:/Users/tiger/Desktop/puma/2024 transplant/model_results_publication_table.docx"
)

############################################################
### Save raw coefficient table too
### This still includes site rows
############################################################

coef_table_all <- bind_rows(lapply(results, function(x) x$coef_table))

write.csv(
  coef_table_all,
  "C:/Users/tiger/Desktop/puma/2024 transplant/model_results_raw_coefficients.csv",
  row.names = FALSE,
  na = ""
)