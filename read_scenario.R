# Reads one scenario saved from the Demand sheet of the workbook.
#
# Returns one row per country and birth year with live births, coverage
# of live births, women vaccinated, and doses required. Every number
# comes from the workbook. Nothing here changes a demand input.
#
# Coverage of live births = share vaccinated at full coverage x scale-up
# in that year x share of the year with vaccination.
# Women vaccinated = live births x pregnancy factor x coverage. The
# pregnancy factor adds stillbirths to live births.
read_scenario <- function(file, countries, years) {

  # Excel can add a hidden character to the first column name, so we
  # rename it.
  x <- read.csv(file, stringsAsFactors = FALSE)
  names(x)[1] <- "iso3"
  x <- x[x$iso3 %in% countries, ]

  if (nrow(x) != length(countries))
    stop(file, ": expected ", length(countries), " countries, found ", nrow(x))

  # Excel saves numbers as they are displayed. Remove any thousands
  # separators before reading them as numbers.
  num <- function(v) as.numeric(gsub(",", "", v))

  out <- NULL
  for (y in years) {
    births   <- num(x[[paste0("births_", y)]])
    coverage <- num(x$share_vaccinated) * num(x[[paste0("scaleup_", y)]]) *
                num(x$share_of_year)
    out <- rbind(out, data.frame(
      iso3     = x$iso3,
      year     = y,
      births   = births,
      coverage = coverage,
      women    = births * num(x$pregnancy_factor) * coverage,
      doses    = num(x[[paste0("doses_", y)]])))
  }

  if (anyNA(out)) stop(file, ": a value used by the analysis is missing or not a number")
  out
}
