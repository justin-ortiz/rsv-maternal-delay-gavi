# Assumptions and settings. Sourced by run_analysis.R and run_psa.R.
#
# The demand workbook holds every demand input (births, coverage,
# introduction years, wastage, buffer). This file holds only what the
# impact calculation needs on top of the workbook. The values of the
# published model are in mahmud_model.R and are not changed here.


# Scenario files. Save the Demand sheet of the workbook as "CSV (Comma
# delimited)", one file per scenario, into this folder. Number the files
# 01_, 02_, and so on. The first file is the reference that every other
# scenario is compared with.
scenario_folder <- "scenarios"

# Years of birth counted in the analysis.
years <- 2028:2037

# The 56 countries eligible for Gavi support on 1 July 2026, in the
# order of Gavi's list (by phase, then by name). The random country
# orders in random_orders.R start from this order, so keep it as is.
eligible_file <- "gavi_eligible_2026.csv"

# Inputs from the published model, one row per country.
#   mahmud_inputs.csv       income group and RSV rates per 100,000
#   mahmud_cohort_2025.csv  discounted life expectancy at birth for the
#                           2025 birth cohort (UN WPP 2019). Used only
#                           for DALYs, which this paper does not report.
inputs_file <- "mahmud_inputs.csv"
cohort_file <- "mahmud_cohort_2025.csv"
bounds_file <- "mahmud_bounds.csv"

# Person-years lived from birth to age 5, per birth, for each country
# and birth year. We took Lx for age 0 and for ages 1-4 from the UN
# World Population Prospects 2024 abridged life tables (medium variant,
# both sexes, file WPP2024_Life_Table_Abridged_Medium_2024-2100.csv).
# Lx is person-years lived in the age group per 100,000 births, so
# person-years per birth = (Lx age 0 + Lx ages 1-4) / 100,000.
# These are the same projections as the births in the workbook.
life_table_file <- "wpp2024_life_table_2028-2037.csv"

# Probabilistic analysis. The same draws are applied to every scenario,
# so differences between scenarios can be subtracted draw by draw.
n_draws <- 10000
seed    <- 20260904
