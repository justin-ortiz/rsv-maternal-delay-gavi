# Deaths prevented across 1,000 constrained random orders of countries.
#
# Run after run_analysis.R, which this file sources. It takes a few
# minutes.
#
# Gavi's investment case gives the number of new introductions in each
# year and start years for Ethiopia, Pakistan, and the DRC. It does not
# say which countries fill the other years, so we assigned them. In the
# primary analysis, the six other readiness network countries take the
# earliest open years, and all other countries follow. Within each group
# countries are ordered by their mean introduction interval for
# pneumococcal conjugate, rotavirus, and HPV vaccines.
#
# Here we keep the yearly counts, the three named start years, and the
# two groups, and shuffle countries at random within each group. We
# apply every order to the baseline, delay, and slower scale-up
# schedules, with RSV rates and efficacy at their point estimates.
#
# Inputs, both saved as CSV from the same version of the workbook:
#   schedule.csv               the Schedule sheet
#   scenarios/01_Baseline.csv  the Demand sheet for the baseline
# Writes results_random_orders.csv (summary) and
# results_random_orders_deaths.csv (deaths prevented for every order).

source("run_analysis.R")

n_orders <- 1000
schedule_file <- "schedule.csv"
baseline_file <- file.path(scenario_folder, "01_Baseline.csv")


# Country information from the Schedule sheet, read by column name.
sched <- read.csv(schedule_file, check.names = FALSE, stringsAsFactors = FALSE)
names(sched)[1] <- "iso3"
ctry <- sched[sched$iso3 %in% countries$iso3, ]
ctry <- ctry[match(countries$iso3, ctry$iso3), ]

network   <- ctry$iso3[ctry[["readiness network (1 = member)"]] %in% "1"]
three_yr  <- ctry$iso3[ctry[["3-year uptake (1 = yes)"]] %in% "1"]
named_yr  <- as.numeric(ctry[["start year named by Gavi"]])
named     <- setNames(named_yr[!is.na(named_yr)], ctry$iso3[!is.na(named_yr)])
mean_lag  <- setNames(as.numeric(ctry[["mean lag, 3 vaccines"]]), ctry$iso3)

# New introductions per year, from the table lower on the Schedule sheet
# (first column the year, second column the number).
yr     <- suppressWarnings(as.numeric(sched$iso3))
counts <- setNames(as.numeric(sched[[2]][!is.na(yr) & yr <= 2040]), yr[!is.na(yr) & yr <= 2040])

stopifnot(length(network) == 8, length(named) == 3, length(three_yr) == 3,
          sum(counts) == 37)

# The open years, one per introduction, after removing the years Gavi
# gives to the named countries.
open_years <- NULL
for (y in names(counts))
  open_years <- c(open_years, rep(as.numeric(y), counts[[y]] - sum(named == as.numeric(y))))

# Start year for every country, given an order. Countries past the last
# open year do not start by 2040, and we code them as 2041.
start_years <- function(order) {
  s <- setNames(rep(2041, nrow(countries)), countries$iso3)
  s[order[seq_along(open_years)]] <- open_years
  s[names(named)] <- named
  s
}


# Demand for any set of start years. Share vaccinated, births, and the
# pregnancy factor come from the baseline file. Only the start year and
# the rise to full coverage change between orders and schedules.
#   Baseline uptake: 80% of full coverage in the first year and full
#   coverage after, except one third, two thirds, then full in the three
#   countries Gavi gives 3 years.
#   Slower scale-up: equal steps over 5 years in those three countries
#   and over 3 years in all others.
base <- read.csv(baseline_file, stringsAsFactors = FALSE)
names(base)[1] <- "iso3"
base <- base[match(countries$iso3, base$iso3), ]
num  <- function(v) as.numeric(gsub(",", "", v))
is_three <- countries$iso3 %in% three_yr

deaths_for <- function(start, slower = FALSE) {
  demand <- NULL
  for (y in years) {
    t <- y - start + 1   # year of the program, 1 = first year
    if (!slower) {
      ramp   <- ifelse(is_three, 3, 2)
      uptake <- ifelse(t < 1, 0, ifelse(is_three, pmin(1, t / 3), ifelse(t == 1, 0.8, 1)))
    } else {
      ramp   <- ifelse(is_three, 5, 3)
      uptake <- ifelse(t < 1, 0, pmin(1, t / ramp))
    }
    # After full coverage, coverage rises each year as in the investment
    # case: 3 percentage points a year below 70%, 1 point a year from 70%
    # to 90%, and never above 90%. This is the same rule as the scale-up
    # columns of the Demand sheet.
    full  <- num(base$share_vaccinated) * num(base$share_of_year)
    step  <- ifelse(full < 0.70, 0.03, 0.01)
    after <- t - ramp   # years since the first year at full coverage
    uptake <- ifelse(after > 0 & full < 0.90,
                     pmin(0.90, full + step * after) / full, uptake)
    coverage <- full * uptake
    births   <- num(base[[paste0("births_", y)]])
    demand <- rbind(demand, data.frame(iso3 = countries$iso3, year = y,
                                       births = births, coverage = coverage,
                                       women = births * num(base$pregnancy_factor) * coverage,
                                       doses = 0))
  }
  scenario_outcomes(demand)["deaths"]
}


# The primary analysis order, and a check that the rebuild matches the
# workbook's baseline and slower scale-up files.
rest   <- setdiff(countries$iso3, names(named))
in_net <- rest %in% network
primary <- start_years(c(rest[in_net][order(mean_lag[rest[in_net]])],
                         rest[!in_net][order(mean_lag[rest[!in_net]])]))

check_base <- deaths_for(primary)
check_slow <- deaths_for(primary, slower = TRUE)
cat("Baseline rebuilt here:", round(check_base), " workbook:", round(results$deaths[1]), "\n")
cat("Slower scale-up rebuilt here:", round(check_slow), " workbook:",
    round(results$deaths[results$scenario == "07_Slower_scale_up"]), "\n")
stopifnot(abs(check_base - results$deaths[1]) < 1)


# Schedules: how many years every start year moves, and whether scale-up
# is slower. Every order is applied to every schedule.
schedules <- data.frame(schedule = c("Baseline", paste0(1:5, "-year delay"),
                                     "Slower coverage scale-up, baseline dates",
                                     "Slower coverage scale-up, 1-year delay",
                                     "Slower coverage scale-up, 2-year delay"),
                        shift  = c(0:5, 0, 1, 2),
                        slower = c(rep(FALSE, 6), TRUE, TRUE, TRUE))

set.seed(seed)
deaths <- matrix(NA, n_orders, nrow(schedules), dimnames = list(NULL, schedules$schedule))
for (i in 1:n_orders) {
  s <- start_years(c(sample(rest[in_net]), sample(rest[!in_net])))
  for (j in 1:nrow(schedules))
    deaths[i, j] <- deaths_for(s + schedules$shift[j], schedules$slower[j])
}

primary_deaths <- numeric(nrow(schedules))
for (j in 1:nrow(schedules))
  primary_deaths[j] <- deaths_for(primary + schedules$shift[j], schedules$slower[j])


# Summary. The percentage change compares each schedule with the same
# order on time.
mid <- function(x) c(median = unname(median(x)), interval(x))
pct <- 100 * (deaths - deaths[, 1]) / deaths[, 1]

summary <- data.frame(schedule        = schedules$schedule,
                      deaths_primary  = primary_deaths,
                      t(apply(deaths, 2, mid)),
                      pct_primary     = 100 * (primary_deaths - primary_deaths[1]) / primary_deaths[1],
                      t(apply(pct, 2, mid)),
                      row.names = NULL)
names(summary) <- c("schedule", "deaths_primary", "deaths_median", "deaths_lower", "deaths_upper",
                    "pct_change_primary", "pct_change_median", "pct_change_lower", "pct_change_upper")

print(data.frame(schedule = summary$schedule,
                 primary  = round(summary$deaths_primary),
                 orders   = paste0(round(summary$deaths_median), " (", round(summary$deaths_lower),
                                   "-", round(summary$deaths_upper), ")"),
                 pct      = paste0(round(summary$pct_change_median), " (", round(summary$pct_change_lower),
                                   " to ", round(summary$pct_change_upper), ")")),
      row.names = FALSE)

write.csv(summary, "results_random_orders.csv", row.names = FALSE)
write.csv(data.frame(order = 1:n_orders, deaths), "results_random_orders_deaths.csv", row.names = FALSE)
