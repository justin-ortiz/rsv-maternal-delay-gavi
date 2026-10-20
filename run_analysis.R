# Infant RSV outcomes prevented under each scenario, at point estimates.
#
# Run order: replicate_mahmud.R once, then this file, then run_psa.R.
# This file reads every scenario file in the scenario folder, applies
# the published model to each country and birth year, and writes
# results_scenarios.csv. It takes a few seconds.

source("mahmud_model.R")
source("read_scenario.R")
source("assumptions.R")


# Country inputs: the 56 countries eligible for Gavi support, with the
# published model's rates and discounted life expectancy.
eligible  <- read.csv(eligible_file)
inputs    <- read.csv(inputs_file)
cohort    <- read.csv(cohort_file)
countries <- merge(inputs, cohort, by = "iso3")
countries <- countries[match(eligible$iso3, countries$iso3), ]

if (anyNA(countries$iso3)) stop("A Gavi-eligible country is missing from ", inputs_file)
if (!setequal(countries$iso3, inputs$iso3[inputs$gavi_eligible == 1]))
  stop("The eligibility list and the gavi_eligible column in ", inputs_file, " disagree")

# Person-years per birth for each country and birth year (WPP 2024).
life_table <- read.csv(life_table_file)
life_table$PY_per_birth <- (life_table$L_age0 + life_table$L_age1to4) / 1e5

if (nrow(life_table) != length(countries$iso3) * length(years) ||
    !all(countries$iso3 %in% life_table$iso3))
  stop(life_table_file, " should have one row for each country and birth year")


# Outcomes prevented for one scenario. rates holds one row per country,
# in the same order as countries. With no rates given, the published
# point estimates are used.
scenario_outcomes <- function(demand, rates = NULL, efficacy = efficacy_severe) {
  if (is.null(rates))
    rates <- data.frame(severe     = countries$sev_mid,
                        nonsevere  = countries$non_mid,
                        admissions = countries$hosp_mid,
                        deaths     = countries$dth_mid)

  # Line up each country-year of the scenario with its country inputs,
  # and with person-years per birth for that country and birth year.
  k  <- match(demand$iso3, countries$iso3)
  py <- life_table$PY_per_birth[match(paste(demand$iso3, demand$year),
                                      paste(life_table$iso3, life_table$year))]

  out <- mahmud_outcomes(person_years  = demand$births * py,
                         coverage      = demand$coverage,
                         rates         = rates[k, ],
                         life_exp_disc = countries$e0_discounted[k],
                         efficacy      = efficacy)

  c(women      = sum(demand$women),
    doses      = sum(demand$doses),
    cases      = sum(out$cases_prevented),
    admissions = sum(out$admissions_prevented),
    deaths     = sum(out$deaths_prevented))
}


# Read every scenario file. The files sort by their number, so the first
# one is the reference.
files <- sort(list.files(scenario_folder, pattern = "\\.csv$", full.names = TRUE))
if (length(files) == 0) stop("No scenario files in the folder ", scenario_folder)

demand <- lapply(files, read_scenario, countries = countries$iso3, years = years)
names(demand) <- sub("\\.csv$", "", basename(files))


# Point results for every scenario, and the difference in deaths
# prevented from the reference scenario.
results <- data.frame(scenario = names(demand),
                      t(sapply(demand, scenario_outcomes)),
                      row.names = NULL)
results$deaths_vs_reference <- results$deaths - results$deaths[1]

print(data.frame(scenario   = results$scenario,
                 women_m    = round(results$women / 1e6, 1),
                 doses_m    = round(results$doses / 1e6, 1),
                 deaths     = round(results$deaths),
                 difference = round(results$deaths_vs_reference)),
      row.names = FALSE)

write.csv(results, "results_scenarios.csv", row.names = FALSE)
