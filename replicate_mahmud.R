# Checks that our rebuild of the published model reproduces its results.
#
# We apply mahmud_model.R to the published model's own inputs: its RSV
# rates by income group, its maternal vaccine coverage for each country,
# and a 2025 birth cohort that we calculated from UN World Population Prospects 2019. We
# then compare the totals with the published totals for maternal
# vaccination, which are the no-intervention results minus the maternal
# vaccine results in the published supplement (Additional file 1, Table
# S12).
#
# Run the whole file. It prints two comparisons and writes two files.
#   results_replication.csv           outcomes prevented (used for Table S4)
#   results_replication_baseline.csv  outcomes with no vaccination

source("mahmud_model.R")

inputs <- read.csv("mahmud_inputs.csv")
cohort <- read.csv("mahmud_cohort_2025.csv")
d <- merge(inputs, cohort, by = "iso3")

rates <- data.frame(severe     = d$sev_mid,
                    nonsevere  = d$non_mid,
                    admissions = d$hosp_mid,
                    deaths     = d$dth_mid)

out <- mahmud_outcomes(person_years  = d$PY_0to5,
                       coverage      = d$maternal_coverage_pct / 100,
                       rates         = rates,
                       life_exp_disc = d$e0_discounted)

# Published totals, Table S12: no intervention minus maternal vaccine.
published <- c(cases      = 31882631 - 27723308,
               admissions = 3468109  - 2664673,
               deaths     = 108904   - 87358,
               dalys      = 3116915  - 2491362)

ours <- c(cases      = sum(out$cases_prevented),
          admissions = sum(out$admissions_prevented),
          deaths     = sum(out$deaths_prevented),
          dalys      = sum(out$dalys_prevented))

result <- data.frame(outcome    = names(published),
                     published  = unname(published),
                     ours       = round(unname(ours)),
                     difference = round(100 * (unname(ours) / unname(published) - 1), 1))
names(result)[4] <- "difference_pct"

cat("\nCountries:", nrow(d), "\n\n")
print(result, row.names = FALSE)
write.csv(result, "results_replication.csv", row.names = FALSE)


# Second check, added 31 October 2026: outcomes with no vaccination.
#
# The check above compares outcomes prevented. A close match there can
# hide two differences that cancel. This check compares the published
# totals with no vaccination (Table S12, Baseline rows, UNIVAC column),
# which depend only on person-years, the RSV rates, and how years of life
# lost are calculated, not on coverage or efficacy.
#
# These are written to a separate file so that Table S4 (tables.R) does
# not change.

severe     <- sum(d$PY_0to5 * d$sev_mid  / 1e5)
nonsevere  <- sum(d$PY_0to5 * d$non_mid  / 1e5)
admissions <- sum(d$PY_0to5 * d$hosp_mid / 1e5)
deaths     <- d$PY_0to5 * d$dth_mid / 1e5   # one value per country

# DALYs are calculated the same way as in mahmud_outcomes(): deaths times
# discounted life expectancy at birth, plus years lived with disability
# for the acute illness, each discounted to birth for the age at which the
# event happens. With no vaccination, events happen at any age up to 5, so
# we use the discount for all ages under 5 (version 1.1).
dalys <- sum(deaths * d$e0_discounted) * timing_severe_5y +
  severe    * weight_severe    * days_severe    / 365 * timing_severe_5y +
  nonsevere * weight_nonsevere * days_nonsevere / 365 * timing_nonsevere_5y

published_base <- c(cases      = 31882631,
                    admissions = 3468109,
                    deaths     = 108904,
                    dalys      = 3116915)

ours_base <- c(cases      = severe + nonsevere,
               admissions = admissions,
               deaths     = sum(deaths),
               dalys      = dalys)

baseline <- data.frame(outcome   = names(published_base),
                       published = unname(published_base),
                       ours      = round(unname(ours_base)),
                       difference = round(100 * (unname(ours_base) / unname(published_base) - 1), 1))
names(baseline)[4] <- "difference_pct"

cat("\nNo vaccination, compared with Table S12 Baseline rows\n\n")
print(baseline, row.names = FALSE)
write.csv(baseline, "results_replication_baseline.csv", row.names = FALSE)
