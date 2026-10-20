# 95% uncertainty intervals for deaths and admissions prevented, and the
# percentage of deaths before age 6 months that vaccination prevents.
#
# Run after run_analysis.R, which this file sources for the country
# inputs and scenario files. It takes about two minutes for 13 scenarios.
#
# Each draw changes the RSV rates (one draw per income group) and
# efficacy against severe disease. The same 10,000 draws are applied to
# every scenario, so the difference between a scenario and the reference
# is calculated draw by draw and has its own interval. Intervals are the
# 2.5th and 97.5th percentiles of the draws.

source("run_analysis.R")

bounds <- read.csv(bounds_file)
point  <- data.frame(severe     = countries$sev_mid,
                     nonsevere  = countries$non_mid,
                     admissions = countries$hosp_mid,
                     deaths     = countries$dth_mid)

# Draw all inputs first, so every scenario sees the same draws.
set.seed(seed)
rate_draws     <- vector("list", n_draws)
efficacy_draws <- numeric(n_draws)
for (i in 1:n_draws) {
  rate_draws[[i]]   <- draw_rates(countries$income_group, point, bounds)
  efficacy_draws[i] <- draw_efficacy()
}

# Deaths and admissions prevented for every draw (rows) and scenario
# (columns).
deaths     <- matrix(NA, n_draws, length(demand), dimnames = list(NULL, names(demand)))
admissions <- deaths
for (s in seq_along(demand)) {
  for (i in 1:n_draws) {
    r <- scenario_outcomes(demand[[s]], rate_draws[[i]], efficacy_draws[i])
    deaths[i, s]     <- r["deaths"]
    admissions[i, s] <- r["admissions"]
  }
}

# Intervals for each scenario, and for its difference from the reference.
psa <- data.frame(scenario = names(demand),
                  t(apply(deaths, 2, interval)),
                  t(apply(deaths - deaths[, 1], 2, interval)),
                  t(apply(admissions, 2, interval)),
                  t(apply(admissions - admissions[, 1], 2, interval)),
                  row.names = NULL)
names(psa) <- c("scenario",
                "deaths_lower", "deaths_upper",
                "deaths_diff_lower", "deaths_diff_upper",
                "admissions_lower", "admissions_upper",
                "admissions_diff_lower", "admissions_diff_upper")

print(data.frame(scenario    = psa$scenario,
                 deaths      = paste0(round(psa$deaths_lower), "-", round(psa$deaths_upper)),
                 difference  = paste0(round(psa$deaths_diff_lower), " to ", round(psa$deaths_diff_upper))),
      row.names = FALSE)

write.csv(psa, "results_uncertainty.csv", row.names = FALSE)


# Deaths before age 6 months.
#
# Deaths before 6 months with no vaccination = person-years from birth to
# age 5 x RSV death rate x the share of deaths before age 5 that happen
# by 6 months (severe_by_6mo in mahmud_model.R). This is the same
# quantity the model multiplies by coverage and efficacy, so deaths
# prevented can be stated as a percentage of it. It counts every infant
# born 2028-2037 in the 56 countries, whether or not the country has
# introduced the vaccine. Births are the same in every scenario file,
# so we take them from the reference.
deaths_before_6mo <- function(demand, death_rates) {
  k  <- match(demand$iso3, countries$iso3)
  py <- life_table$PY_per_birth[match(paste(demand$iso3, demand$year),
                                      paste(life_table$iso3, life_table$year))]
  sum(demand$births * py * death_rates[k] / 1e5 * severe_by_6mo)
}

no_vaccine_point <- deaths_before_6mo(demand[[1]], point$deaths)
no_vaccine_draws <- sapply(rate_draws, function(r) deaths_before_6mo(demand[[1]], r$deaths))

# Percentage prevented, for each draw and scenario. Deaths prevented use
# the same draws as above.
pct_draws <- 100 * deaths / no_vaccine_draws

first_6mo <- data.frame(scenario         = names(demand),
                        no_vaccine       = no_vaccine_point,
                        no_vaccine_lower = interval(no_vaccine_draws)["lower"],
                        no_vaccine_upper = interval(no_vaccine_draws)["upper"],
                        pct_prevented    = 100 * results$deaths / no_vaccine_point,
                        t(apply(pct_draws, 2, interval)),
                        row.names = NULL)
names(first_6mo)[6:7] <- c("pct_lower", "pct_upper")

print(data.frame(scenario  = first_6mo$scenario,
                 pct       = paste0(round(first_6mo$pct_prevented), " (",
                                    round(first_6mo$pct_lower), "-",
                                    round(first_6mo$pct_upper), ")")),
      row.names = FALSE)

write.csv(first_6mo, "results_first_6_months.csv", row.names = FALSE)
