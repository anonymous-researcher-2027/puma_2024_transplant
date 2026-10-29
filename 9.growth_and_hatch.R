### STATS FOR GROWTH RATE MODEL
library(lme4)
library(lmerTest)
library(car)

dat <- read.csv(
  "growth_with_rates.csv",
  stringsAsFactors = FALSE
)

dat <- subset(dat, eggs != "U" & hatch_doy != "U")

dat$raised_nest   <- factor(dat$raised_nest)
dat$pair_age      <- factor(dat$pair_age)
dat$site          <- factor(dat$site)
dat$sex           <- factor(dat$sex)
dat$eggs          <- as.numeric(dat$eggs)
dat$sibs_hatched  <- as.numeric(dat$sibs_hatched)
dat$sibs_fledged  <- as.numeric(dat$sibs_fledged)
dat$hatch_doy     <- as.numeric(dat$hatch_doy)
dat$treatment     <- factor(dat$treatment)
dat$w_t_gain      <- as.numeric(dat$w_t_gain)

options(contrasts = c("contr.sum", "contr.poly"))

timing_vars <- c("w_t_gain")

fit_one <- function(resp, dat) {
  fml <- switch(
    resp,
    "w_t_gain" = w_t_gain ~ treatment * site + hatch_doy + sex + pair_age +
      eggs + sibs_hatched + sibs_fledged + (1 | raised_nest),
    stop("Unknown response: ", resp)
  )
  
  mod <- lmer(fml, data = dat, REML = FALSE)
  
  cat("\n==============================\n")
  cat("Response:", resp, "\n")
  cat("==============================\n")
  
  print(summary(mod))
  
  a3 <- car::Anova(mod, type = 3, test.statistic = "Chisq")
  print(a3)
  
  tab <- as.data.frame(a3)
  tab$term <- rownames(tab)
  rownames(tab) <- NULL
  tab$response <- resp
  
  list(model = mod, anova_chisq = tab)
}

fits <- lapply(timing_vars, fit_one, dat = dat)

anova_table_all <- do.call(rbind, lapply(fits, `[[`, "anova_chisq"))
anova_table_all <- anova_table_all[, c("response", "term", "Chisq", "Df", "Pr(>Chisq)")]

cat("\n\n===== Combined Chi-square table (all responses) =====\n")
print(anova_table_all, row.names = FALSE)






### STATS FOR HATCHING SUCCESS MODEL
### BINOMIAL GLM
library(car)

dat <- read.csv(
  "nest_success_1.csv",
  stringsAsFactors = FALSE
)

dat$pair_age      <- factor(dat$pair_age)
dat$site          <- factor(dat$site)
dat$eggs          <- as.numeric(dat$eggs)
dat$eggs_hatch    <- as.numeric(dat$eggs_hatch)
dat$percent_hatch <- as.numeric(dat$percent_hatch)
dat$estimate_inc  <- as.numeric(dat$estimate_inc)
dat$type          <- factor(dat$type, levels = c("Experimental", "Control"))

dat <- subset(
  dat,
  !is.na(eggs) &
    !is.na(eggs_hatch) &
    !is.na(type) &
    !is.na(site) &
    !is.na(estimate_inc) &
    !is.na(pair_age) &
    eggs > 0 &
    eggs_hatch >= 0 &
    eggs_hatch <= eggs
)

options(contrasts = c("contr.sum", "contr.poly"))

timing_vars <- c("hatching_success")

fit_one <- function(resp, dat) {
  fml <- switch(
    resp,
    "hatching_success" = cbind(eggs_hatch, eggs - eggs_hatch) ~ type * site + eggs + pair_age,
    stop("Unknown response: ", resp)
  )
  
  mod <- glm(fml, data = dat, family = binomial)
  
  cat("\n==============================\n")
  cat("Response:", resp, "\n")
  cat("==============================\n")
  
  print(summary(mod))
  
  a3 <- car::Anova(mod, type = 3, test.statistic = "Wald")
  print(a3)
  
  tab <- as.data.frame(a3)
  tab$term <- rownames(tab)
  rownames(tab) <- NULL
  tab$response <- resp
  
  list(model = mod, anova_chisq = tab)
}

fits <- lapply(timing_vars, fit_one, dat = dat)

anova_table_all <- do.call(rbind, lapply(fits, `[[`, "anova_chisq"))
anova_table_all <- anova_table_all[, c("response", "term", "Chisq", "Df", "Pr(>Chisq)")]

cat("\n\n===== Combined Wald Chi-square table (all responses) =====\n")
print(anova_table_all, row.names = FALSE)







########################

library(car)

# Ensure correct types
dat$pair_age      <- factor(dat$pair_age)
dat$site          <- factor(dat$site)
dat$type          <- factor(dat$type, levels = c("Experimental", "Control"))
dat$eggs          <- as.numeric(dat$eggs)
dat$eggs_hatch    <- as.numeric(dat$eggs_hatch)
dat$percent_hatch <- as.numeric(dat$percent_hatch)
dat$estimate_inc  <- as.numeric(dat$estimate_inc)

options(contrasts = c("contr.sum", "contr.poly"))

# fit model
mod_hatch <- glm(
  cbind(eggs_hatch, eggs - eggs_hatch) ~ type * site + eggs + pair_age,
  data = dat,
  family = binomial
)

# Wald Chi-square Type III table
a3 <- car::Anova(mod_hatch, type = 3, test.statistic = "Wald")

# format to match w_t_gain table
tab <- as.data.frame(a3)
tab$term <- rownames(tab)
rownames(tab) <- NULL
tab$response <- "hatching_success"

tab <- tab[, c("response", "term", "Chisq", "Df", "Pr(>Chisq)")]

print(tab, row.names = FALSE)






## Visreg plot code for multipanel fig
library(lme4)
library(visreg)

# read csv
growth_dat <- read.csv(
  "growth_with_rates.csv",
  stringsAsFactors = FALSE
)

growth_dat <- subset(growth_dat, eggs != "U" & hatch_doy != "U")

growth_dat$raised_nest   <- factor(growth_dat$raised_nest)
growth_dat$pair_age      <- factor(growth_dat$pair_age)
growth_dat$site          <- factor(growth_dat$site)
growth_dat$sex           <- factor(growth_dat$sex)
growth_dat$eggs          <- as.numeric(growth_dat$eggs)
growth_dat$sibs_hatched  <- as.numeric(growth_dat$sibs_hatched)
growth_dat$sibs_fledged  <- as.numeric(growth_dat$sibs_fledged)
growth_dat$hatch_doy     <- as.numeric(growth_dat$hatch_doy)
growth_dat$treatment     <- factor(growth_dat$treatment)
growth_dat$w_t_gain      <- as.numeric(growth_dat$w_t_gain)

m_w_t_gain <- lmer(
  w_t_gain ~ treatment * site + hatch_doy +
    sex + pair_age + eggs + sibs_hatched + sibs_fledged +
    (1 | raised_nest),
  data = growth_dat,
  REML = FALSE
)


# hatching success data + model
hatch_dat <- read.csv(
  "nest_success_1.csv",
  stringsAsFactors = FALSE
)

hatch_dat$pair_age      <- factor(hatch_dat$pair_age)
hatch_dat$site          <- factor(hatch_dat$site)
hatch_dat$eggs          <- as.numeric(hatch_dat$eggs)
hatch_dat$eggs_hatch    <- as.numeric(hatch_dat$eggs_hatch)
hatch_dat$percent_hatch <- as.numeric(hatch_dat$percent_hatch)
hatch_dat$estimate_inc  <- as.numeric(hatch_dat$estimate_inc)
hatch_dat$type          <- factor(hatch_dat$type, levels = c("Experimental", "Control"))

hatch_dat <- subset(
  hatch_dat,
  !is.na(eggs) &
    !is.na(eggs_hatch) &
    !is.na(type) &
    !is.na(site) &
    !is.na(estimate_inc) &
    !is.na(pair_age) &
    eggs > 0 &
    eggs_hatch >= 0 &
    eggs_hatch <= eggs
)

m_percent_hatch <- glm(
  cbind(eggs_hatch, eggs - eggs_hatch) ~ type * site + eggs + pair_age,
  data = hatch_dat,
  family = binomial
)

# helper for visreg panels
plot_visreg_with_n <- function(mod, xvar, main_title,
                               xlab,
                               ylab,
                               partial = TRUE,
                               point_cex = 1.15,
                               scale = "response") {
  visreg(
    mod, xvar,
    partial = partial,
    scale   = scale,
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


# 2-panel fig
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)


plot_visreg_with_n(
  mod        = m_percent_hatch,
  xvar       = "type",
  main_title = "a. Hatching Success",
  xlab       = "Treatment",
  ylab       = "Proportion of Eggs Hatched",
  scale      = "response"
)

plot_visreg_with_n(
  mod        = m_w_t_gain,
  xvar       = "treatment",
  main_title = "b. Growth Rate",
  xlab       = "Treatment",
  ylab       = "Growth Rate (weight/tarsus)"
)

