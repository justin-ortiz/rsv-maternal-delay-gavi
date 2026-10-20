# Functions for the published maternal RSV vaccination model (Mahmud et
# al., BMC Medicine 2023;21:138). We rebuilt the model from the paper and
# its supplement. We did not use the UNIVAC software.
#
# This file is the same in rsv-maternal-delay-gavi and
# rsv-asthma-maternal-lmic. If you change it, change both copies.


# Values from the published model. We hold them as published, so that
# any difference between our results and the paper comes from our inputs
# and not from the model.

efficacy_severe    <- 0.694   # severe disease, 0-180 days (trial report)
efficacy_severe_lo <- 0.443   # trial report, 97.58% confidence interval
efficacy_severe_hi <- 0.841
efficacy_ci_level  <- 0.9758
efficacy_nonsevere <- 0.513   # non-severe disease, 0-180 days (trial report)

# Share of all RSV events before age 5 that happen by 6 months and by 12
# months of age, from the age distributions fitted in the published
# model. Protection lasts 6 months and then stops.
severe_by_6mo     <- 0.5953
severe_by_12mo    <- 0.7721
nonsevere_by_6mo  <- 0.3790
nonsevere_by_12mo <- 0.6585

# Disability weights and durations for the acute illness.
weight_severe    <- 0.133
days_severe      <- 10
weight_nonsevere <- 0.051
days_nonsevere   <- 5


# RSV outcomes prevented in one birth cohort, one row per country.
#
# person_years   person-years lived from birth to age 5
# coverage       share of pregnant women vaccinated, 0 to 1
# rates          data frame with columns severe, nonsevere, admissions,
#                deaths, each per 100,000 children under 5 per year
# life_exp_disc  discounted life expectancy at birth, for years of life lost
# efficacy       efficacy against severe disease. Deaths and admissions
#                use the same efficacy, as in the published model.
mahmud_outcomes <- function(person_years, coverage, rates, life_exp_disc,
                            efficacy = efficacy_severe) {

  # RSV events in the first 5 years of life without vaccination.
  severe     <- person_years * rates$severe     / 1e5
  nonsevere  <- person_years * rates$nonsevere  / 1e5
  admissions <- person_years * rates$admissions / 1e5
  deaths     <- person_years * rates$deaths     / 1e5

  # Events prevented are the events that fall in the first 6 months,
  # times coverage, times efficacy. Admissions and deaths follow the age
  # distribution of severe disease.
  severe_prev     <- severe     * severe_by_6mo    * coverage * efficacy
  nonsevere_prev  <- nonsevere  * nonsevere_by_6mo * coverage * efficacy_nonsevere
  admissions_prev <- admissions * severe_by_6mo    * coverage * efficacy
  deaths_prev     <- deaths     * severe_by_6mo    * coverage * efficacy

  # DALYs prevented. Years of life lost dominate. The acute illness adds
  # a small amount of years lived with disability.
  dalys_prev <- deaths_prev * life_exp_disc +
    severe_prev    * weight_severe    * days_severe    / 365 +
    nonsevere_prev * weight_nonsevere * days_nonsevere / 365

  # Episodes in the first year of life without vaccination. The asthma
  # analysis uses these as its exposure. The delay analysis does not.
  data.frame(severe_prevented     = severe_prev,
             nonsevere_prevented  = nonsevere_prev,
             cases_prevented      = severe_prev + nonsevere_prev,
             admissions_prevented = admissions_prev,
             deaths_prevented     = deaths_prev,
             dalys_prevented      = dalys_prev,
             severe_first_year    = severe    * severe_by_12mo,
             nonsevere_first_year = nonsevere * nonsevere_by_12mo)
}


# One draw of the RSV rates.
#
# The published model gives one set of rates for each World Bank income
# group, so we draw once per group and every country in the group gets
# that draw. Severe cases, admissions, and deaths share one draw, because
# the published model ties them to severe disease. Non-severe cases get
# their own. Each rate is lognormal, with its spread taken from the
# published 95% bounds.
#
# income_group  one value per country
# rates         point estimates per country (columns as in mahmud_outcomes)
# bounds        one row per income group, from mahmud_bounds.csv
draw_rates <- function(income_group, rates, bounds) {
  sd_log <- function(lo, hi) (log(hi) - log(lo)) / (2 * 1.96)

  z_severe    <- rnorm(nrow(bounds))
  z_nonsevere <- rnorm(nrow(bounds))
  k <- match(income_group, bounds$income_group)

  data.frame(
    severe     = rates$severe     * exp(sd_log(bounds$sev_lo,  bounds$sev_hi)[k]  * z_severe[k]),
    nonsevere  = rates$nonsevere  * exp(sd_log(bounds$non_lo,  bounds$non_hi)[k]  * z_nonsevere[k]),
    admissions = rates$admissions * exp(sd_log(bounds$hosp_lo, bounds$hosp_hi)[k] * z_severe[k]),
    deaths     = rates$deaths     * exp(sd_log(bounds$dth_lo,  bounds$dth_hi)[k]  * z_severe[k]))
}


# One draw of efficacy against severe disease.
#
# We draw on the logit scale, centred on 69.4%. The trial interval is
# not symmetric on that scale, so the spread below the centre is set
# from the lower limit and the spread above it from the upper limit.
# The draws then match the point estimate and both limits of the 97.58%
# interval.
draw_efficacy <- function() {
  logit <- function(p) log(p / (1 - p))
  z     <- qnorm(1 - (1 - efficacy_ci_level) / 2)
  sd_lo <- (logit(efficacy_severe) - logit(efficacy_severe_lo)) / z
  sd_hi <- (logit(efficacy_severe_hi) - logit(efficacy_severe)) / z
  e     <- rnorm(1)
  x     <- logit(efficacy_severe) + ifelse(e < 0, sd_lo, sd_hi) * e
  1 / (1 + exp(-x))
}


# 95% uncertainty interval.
interval <- function(x) {
  c(lower = unname(quantile(x, 0.025)),
    upper = unname(quantile(x, 0.975)))
}
