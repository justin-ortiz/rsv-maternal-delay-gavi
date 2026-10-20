# Builds wpp2024_life_table_2028-2037.csv from the UN life table file.
#
# You only need to run this if you want to rebuild that file from the UN
# download. The analysis itself reads the small file this script writes.
#
# Download (UN World Population Prospects 2024):
#   population.un.org/wpp > Downloads > Standard Projections > CSV format >
#   Life Tables > 2024-2100, medium. The file is
#   WPP2024_Life_Table_Abridged_Medium_2024-2100.csv (about 511 MB). The
#   site offers it as a .gz file, and some browsers unpack it on download.
#   This script reads either one. Put it in this folder.
#
# For each of the 56 countries and each birth year from 2028 to 2037, we
# keep two numbers from the both-sexes life table (Sex = Total):
#   L_age0     Lx for age 0 (person-years lived from birth to age 1, per
#              100,000 births)
#   L_age1to4  Lx for ages 1-4 (person-years lived from age 1 to age 5, per
#              100,000 births)
# run_analysis.R adds the two and divides by 100,000 to get person-years
# lived from birth to age 5 per birth.
#
# To check one value by hand, open the UN file in Excel with Data > From
# Text/CSV > Transform Data (it has too many rows to open directly), and
# filter ISO3_code, Time, Sex = Total, and AgeGrp. Kenya 2028 should give
# 97591.57 for age 0 and 386450.617 for ages 1-4.
#
# It takes a minute or two, because the UN file is large.

un_file <- "WPP2024_Life_Table_Abridged_Medium_2024-2100.csv"
if (!file.exists(un_file)) un_file <- paste0(un_file, ".gz")
if (!file.exists(un_file)) stop("Put the UN life table file in this folder first")

eligible <- read.csv("gavi_eligible_2026.csv")
years    <- 2028:2037

# Read only the five columns we need. Setting the others to "NULL" tells
# R to skip them, which saves a lot of memory.
header <- names(read.csv(un_file, nrows = 1, check.names = FALSE))
header[1] <- sub("^﻿", "", header[1])   # Excel-style marker on the first name
keep <- c("ISO3_code", "Time", "Sex", "AgeGrp", "Lx")
classes <- ifelse(header %in% keep, "character", "NULL")
un <- read.csv(un_file, colClasses = classes, check.names = FALSE)
names(un) <- header[header %in% keep]

un <- un[un$Sex == "Total" & un$ISO3_code %in% eligible$iso3 &
         un$Time %in% years & un$AgeGrp %in% c("0", "1-4"), ]

# One row per country and year, in the order of the eligibility list.
out <- NULL
for (iso in eligible$iso3) {
  for (y in years) {
    x <- un[un$ISO3_code == iso & un$Time == y, ]
    out <- rbind(out, data.frame(
      iso3      = iso,
      country   = eligible$country[eligible$iso3 == iso],
      year      = y,
      L_age0    = as.numeric(x$Lx[x$AgeGrp == "0"]),
      L_age1to4 = as.numeric(x$Lx[x$AgeGrp == "1-4"])))
  }
}

if (nrow(out) != nrow(eligible) * length(years) || anyNA(out))
  stop("A country or year is missing from the UN file")

write.csv(out, "wpp2024_life_table_2028-2037.csv", row.names = FALSE, quote = FALSE)
cat("Wrote", nrow(out), "rows. Kenya 2028:",
    out$L_age0[out$iso3 == "KEN" & out$year == 2028], "and",
    out$L_age1to4[out$iso3 == "KEN" & out$year == 2028], "\n")
