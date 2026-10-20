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
# Run the whole file. It prints the comparison and writes
# results_replication.csv.

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
