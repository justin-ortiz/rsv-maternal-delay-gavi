# Table 1 and Supplement Tables S1-S4, built from the results files so
# that no number in a table is typed by hand.
#
# Run after run_analysis.R, run_psa.R, random_orders.R, and
# replicate_mahmud.R. It takes a second.
#
# Each table is written as a tab-delimited file (table_1.tsv,
# table_S1.tsv, and so on). To put one into Word, paste it, select it,
# and use Insert > Table > Convert Text to Table. Titles, footnotes, and
# abbreviations are in the manuscript, not here.
#
# Rounding follows the manuscript. Deaths are rounded to the nearest
# 100, hospitalizations to the nearest 1,000 (shown in thousands), women
# vaccinated to whole millions, and percentages to whole numbers.
# Differences are calculated before rounding.

source("mahmud_model.R")
source("assumptions.R")

points   <- read.csv("results_scenarios.csv")
ui       <- read.csv("results_uncertainty.csv")
first6   <- read.csv("results_first_6_months.csv")
orders   <- read.csv("results_random_orders.csv")
validate <- read.csv("results_replication.csv")
bounds   <- read.csv(bounds_file)


# Formatting. Negative numbers get a true minus sign and ranges an en
# dash, as in the manuscript. Halves round away from zero, so 17.5
# becomes 18 (R on its own would round it to 18 but 16.5 to 16).
minus <- "−"
dash  <- "–"
half_up <- function(x, digits = 0) sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
num <- function(x, digits = 0)
  sub("^-", minus, format(half_up(x, digits), big.mark = ",", trim = TRUE, scientific = FALSE))
signed <- function(x, digits = 0) ifelse(half_up(x, digits) > 0, paste0("+", num(x, digits)), num(x, digits))
with_ui     <- function(x, lo, hi, digits = 0) paste0(num(x, digits), " (", num(lo, digits), dash, num(hi, digits), ")")
change_ui   <- function(x, lo, hi, digits = 0) paste0(signed(x, digits), " (", signed(lo, digits), " to ", signed(hi, digits), ")")

# Writes one table as tab-delimited text in UTF-8, so the minus signs
# and en dashes survive on any computer, and shows it on screen.
write_table <- function(df, file) {
  lines <- c(paste(names(df), collapse = "\t"),
             apply(df, 1, paste, collapse = "\t"))
  writeLines(enc2utf8(lines), file, useBytes = TRUE)
  cat(lines, "", sep = "\n")
}


# Countries started by 2030 and by 2037: countries with any coverage in
# that year, read from each scenario file. We keep only the 56 country
# rows, so the year row and the total row are left out.
eligible <- read.csv(eligible_file)$iso3
started <- function(scenario) {
  x <- read.csv(file.path(scenario_folder, paste0(scenario, ".csv")), stringsAsFactors = FALSE)
  names(x)[1] <- "iso3"
  x <- x[x$iso3 %in% eligible, ]
  n <- function(y) sum(as.numeric(gsub(",", "", x[[paste0("scaleup_", y)]])) > 0)
  paste(n(2030), "/", n(2037))
}

# One row per scenario with every result we need, in the order of the
# scenario files.
r <- merge(merge(points, ui, by = "scenario"), first6, by = "scenario")
r <- r[order(r$scenario), ]
ref <- r$scenario == r$scenario[1]


# ---- Table 1 ---------------------------------------------------------
# Baseline, five delays, and slower scale-up on baseline dates and 1
# year later. The percentage is the change in deaths prevented as a
# share of deaths prevented on the baseline schedule.

t1 <- r[match(c("01_Baseline", "02_One_year_later", "03_Two_years_later",
                "04_Three_years_later", "05_Four_years_later", "06_Five_years_later",
                "07_Slower_scale_up", "08_Slower_scale_up_one_year_later"), r$scenario), ]
is_ref <- t1$scenario == "01_Baseline"

table_1 <- data.frame(
  "Schedule" = c("Baseline", "1 year later", "2 years later", "3 years later",
                 "4 years later", "5 years later",
                 "Slower coverage scale-up, baseline dates",
                 "Slower coverage scale-up, 1 year later"),
  "Countries started by 2030 / 2037" = sapply(t1$scenario, started),
  "Deaths prevented (95% UI)" = with_ui(t1$deaths, t1$deaths_lower, t1$deaths_upper, -2),
  "Change from baseline, deaths (95% UI)" =
    ifelse(is_ref, "Reference",
           change_ui(t1$deaths_vs_reference, t1$deaths_diff_lower, t1$deaths_diff_upper, -2)),
  "Change from baseline, %" =
    ifelse(is_ref, "Reference", num(100 * t1$deaths_vs_reference / t1$deaths[is_ref])),
  check.names = FALSE, row.names = NULL)

write_table(table_1, "table_1.tsv")


# ---- Table S1 --------------------------------------------------------
# Scenarios 01 to 13, the scenarios in the manuscript's Table S1.

s1 <- r[r$scenario < "14", ]
labels <- c("01_Baseline"                        = "Baseline",
            "02_One_year_later"                  = "1 year later",
            "03_Two_years_later"                 = "2 years later",
            "04_Three_years_later"               = "3 years later",
            "05_Four_years_later"                = "4 years later",
            "06_Five_years_later"                = "5 years later",
            "07_Slower_scale_up"                 = "Slower coverage scale-up, baseline dates",
            "08_Slower_scale_up_one_year_later"  = "Slower coverage scale-up, 1 year later",
            "09_Slower_scale_up_two_years_later" = "Slower coverage scale-up, 2 years later",
            "10_RSV_follows_PCV"                 = "RSV follows the pattern of PCV",
            "11_RSV_follows_rotavirus"           = "RSV follows the pattern of rotavirus vaccine",
            "12_RSV_follows_HPV"                 = "RSV follows the pattern of HPV vaccine",
            "13_Historic_order_only"             = "Countries ordered by introduction history only")
is_ref <- s1$scenario == "01_Baseline"

table_S1 <- data.frame(
  "Scenario" = unname(labels[s1$scenario]),
  "Countries started by 2030 / 2037" = sapply(s1$scenario, started),
  "Women vaccinated, millions" = num(s1$women / 1e6),
  "Hospitalizations prevented, thousands (95% UI)" =
    with_ui(s1$admissions / 1e3, s1$admissions_lower / 1e3, s1$admissions_upper / 1e3),
  "Change from baseline, hospitalizations, thousands (95% UI)" =
    ifelse(is_ref, "Reference",
           change_ui((s1$admissions - s1$admissions[is_ref]) / 1e3,
                     s1$admissions_diff_lower / 1e3, s1$admissions_diff_upper / 1e3)),
  "Deaths prevented (95% UI)" = with_ui(s1$deaths, s1$deaths_lower, s1$deaths_upper, -2),
  "Change from baseline, deaths (95% UI)" =
    ifelse(is_ref, "Reference",
           change_ui(s1$deaths_vs_reference, s1$deaths_diff_lower, s1$deaths_diff_upper, -2)),
  "Deaths before age 6 months prevented, % (95% UI)" =
    with_ui(s1$pct_prevented, s1$pct_lower, s1$pct_upper),
  check.names = FALSE, row.names = NULL)

write_table(table_S1, "table_S1.tsv")

# The denominator in footnote b of Table S1.
cat("Deaths before age 6 months without vaccination:",
    with_ui(first6$no_vaccine[1], first6$no_vaccine_lower[1], first6$no_vaccine_upper[1], -2), "\n\n")


# ---- Table S2 --------------------------------------------------------
# Deaths prevented across 1,000 constrained random orders of countries.

is_ref <- orders$schedule == "Baseline"
pct <- function(x) ifelse(is_ref, "Reference", num(x))

table_S2 <- data.frame(
  "Scenario" = c("Baseline", "1 year later", "2 years later", "3 years later",
                 "4 years later", "5 years later",
                 "Slower coverage scale-up, baseline dates",
                 "Slower coverage scale-up, 1 year later"),
  "Deaths prevented, primary analysis" = num(orders$deaths_primary, -2),
  "Deaths prevented, constrained random orders, median (95% range)" =
    with_ui(orders$deaths_median, orders$deaths_lower, orders$deaths_upper, -2),
  "Change from baseline, deaths, %, primary analysis" = pct(orders$pct_change_primary),
  "Change from baseline, deaths, %, constrained random orders, median (95% range)" =
    ifelse(is_ref, "Reference",
           paste0(num(orders$pct_change_median), " (", num(orders$pct_change_lower),
                  " to ", num(orders$pct_change_upper), ")")),
  check.names = FALSE, row.names = NULL)

write_table(table_S2, "table_S2.tsv")


# ---- Table S3 --------------------------------------------------------
# Impact model parameters, taken from mahmud_bounds.csv and
# mahmud_model.R, so the table shows the values the code applies.

b <- bounds[match(c("LI", "LMI", "UMI"), bounds$income_group), ]
rate <- function(mid, lo, hi, digits = 0) with_ui(mid, lo, hi, digits)
same <- function(x) rep(x, 3)
eff  <- paste0(num(100 * efficacy_severe, 1), "% (", num(100 * efficacy_severe_lo, 1),
               dash, num(100 * efficacy_severe_hi, 1), "%)")

table_S3 <- data.frame(
  "Parameter" = c("Severe RSV disease, per 100,000 children <5 years per year",
                  "Non-severe RSV disease, per 100,000",
                  "RSV hospitalizations, per 100,000",
                  "RSV deaths, per 100,000",
                  "Share of under-5 severe events before age 6 months",
                  "Share of under-5 non-severe events before age 6 months",
                  "Efficacy against severe disease",
                  "Efficacy against non-severe disease"),
  rbind(rate(b$sev_mid,  b$sev_lo,  b$sev_hi),
        rate(b$non_mid,  b$non_lo,  b$non_hi),
        rate(b$hosp_mid, b$hosp_lo, b$hosp_hi),
        rate(b$dth_mid,  b$dth_lo,  b$dth_hi, 2),
        same(sprintf("%.4f", severe_by_6mo)),
        same(sprintf("%.4f", nonsevere_by_6mo)),
        same(eff),
        same(paste0(num(100 * efficacy_nonsevere, 1), "%"))),
  check.names = FALSE, row.names = NULL)
names(table_S3)[2:4] <- c("Low income", "Lower-middle income", "Upper-middle income")

write_table(table_S3, "table_S3.tsv")


# ---- Table S4 --------------------------------------------------------
# Our reconstruction of the published model against its published
# totals (from replicate_mahmud.R).

table_S4 <- data.frame(
  "Outcome prevented" = c("RSV cases", "RSV hospitalizations", "RSV deaths",
                          "Disability-adjusted life-years"),
  "Published" = num(validate$published),
  "Our reconstruction" = num(validate$ours),
  "Difference, %" = num(validate$difference_pct, 1),
  check.names = FALSE, row.names = NULL)

write_table(table_S4, "table_S4.tsv")
