# The cost of delay: maternal RSV vaccination in Gavi-eligible countries

R code for the infant RSV deaths and hospitalizations prevented by
maternal RSV vaccination in the 56 countries eligible for Gavi support,
for infants born 2028–2037, when introduction starts late or coverage
scales up slowly.

    mahmud_model.R                 the published acute RSV model, as functions
    assumptions.R                  settings and file names
    read_scenario.R                reads one scenario saved from the demand workbook
    run_analysis.R                 point results for every scenario
    run_psa.R                      95% uncertainty intervals, and deaths before 6 months
    random_orders.R                1,000 constrained random orders of countries
    replicate_mahmud.R             checks the model against the published results
    tables.R                       Table 1 and Supplement Tables S1–S4
    figure1.R                      Figure 1, panels A and B
    build_wpp2024_life_table.R     rebuilds the person-years input from the UN file
    00b_ingest_wpp2019.R           rebuilds the published model's 2025 cohort file

## Data

Women vaccinated, doses required, births, and coverage come from our
demand workbook, an Excel file. Each scenario is the workbook's Demand
sheet saved as a CSV file in the `scenarios` folder, and `schedule.csv`
is its Schedule sheet. The R code changes none of these values.

    scenarios/01_Baseline.csv ... 13_Historic_order_only.csv
    schedule.csv
    gavi_eligible_2026.csv          the 56 countries on Gavi's list of 1 July 2026

Inputs from the published model (Mahmud et al., BMC Medicine 2023):

    mahmud_inputs.csv               income group, maternal coverage, and RSV rates per 100,000, by country
    mahmud_bounds.csv               the uncertainty bounds of the RSV rates, by income group

Person-years lived from birth to age 5:

    wpp2024_life_table_2028-2037.csv   Lx at age 0 and ages 1–4, UN World Population
                                       Prospects 2024, for the 2028–2037 analysis
    mahmud_cohort_2025.csv             the 2025 cohort from World Population Prospects 2019,
                                       used only to check the model against the published results

The two build scripts rebuild these files from the UN downloads. You do
not need to run them to reproduce the results. Each script says at the
top where to download its source.

## To run

1. Download this repository as a zip file from the green Code button
   and unzip it.
2. In RStudio, set the working folder with Session, Set Working
   Directory, To Source File Location.
3. Run `replicate_mahmud.R`. It prints our results next to the published
   results and writes `results_replication.csv`.
4. Run `run_analysis.R`. It prints deaths prevented for every scenario
   and writes `results_scenarios.csv`. It takes a few seconds.
5. Run `run_psa.R`. It writes `results_uncertainty.csv` and
   `results_first_6_months.csv`. It takes about two minutes, because it
   applies 10,000 draws to every scenario.
6. Run `random_orders.R`. It writes `results_random_orders.csv` and
   `results_random_orders_deaths.csv`. It takes a few minutes.
7. Run `tables.R` and then `figure1.R`. They read the result files and
   write the tables as tab-delimited text and the figure panels as PNG
   and TIFF files.

A new run overwrites the result files. The files in this repository are
the results we report.

## Method

We rebuilt the published model from the paper and its supplement. We
did not use the UNIVAC software. `replicate_mahmud.R` applies our
rebuild to the published model's RSV rates and maternal coverage, with births and person-years for the 2025 birth cohort that we calculated from World Population Prospects 2019,
and compares the totals with the published totals (Additional file 1,
Table S12). `mahmud_model.R` is the same file in this repository and in
the asthma analysis, so the two analyses share one acute model.

For each scenario, women vaccinated and coverage come from the demand
workbook. Person-years lived to age 5 are births times person-years per
birth for that country and birth year. The published model then gives
deaths and hospitalizations prevented in each country and year.

The 10,000 draws change the RSV rates, with one draw per income group,
and efficacy against severe disease. The same draws are applied to
every scenario, so the difference between a scenario and the baseline
is calculated draw by draw and has its own interval. The seed is fixed
in `assumptions.R`.

`random_orders.R` keeps Gavi's yearly counts of new introductions and
the three start years the investment case gives, and shuffles the other
countries within two groups. Rates and efficacy are held at their point
estimates, so the spread across orders comes only from country order.

Source model: Mahmud S, Baral R, Sanderson C, et al. Cost-effectiveness
of pharmaceutical strategies to prevent respiratory syncytial virus
disease in young children: a decision-support model for use in
low-income and middle-income countries. BMC Med. 2023;21:138.
