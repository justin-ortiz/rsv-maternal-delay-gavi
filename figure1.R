# Figure 1, panels A and B, saved as separate images with no titles or
# panel letters (those go in the manuscript).
#
# Run after random_orders.R. This file sources run_analysis.R for the
# scenario files and country inputs, and reads
# results_random_orders.csv and results_random_orders_deaths.csv. It
# takes a few seconds.
#
# Panel A: cumulative infant RSV deaths prevented by birth year, for
#   every schedule in Table 1, from scenario files 01 to 08.
# Panel B: change in deaths prevented from baseline, as a percentage,
#   for each schedule. A vertical mark shows the primary analysis, and
#   points show each of the 1,000 constrained random country orders.
#
# Writes figure1a.png, figure1a.tiff, figure1b.png, figure1b.tiff (300
# dpi), and figure1_series.csv, the numbers plotted in panel A.

source("run_analysis.R")

# The eight schedules in Table 1, in the same order as the columns of
# results_random_orders_deaths.csv.
schedules <- data.frame(
  file   = c("01_Baseline", "02_One_year_later", "03_Two_years_later",
             "04_Three_years_later", "05_Four_years_later", "06_Five_years_later",
             "07_Slower_scale_up", "08_Slower_scale_up_one_year_later"),
  label  = c("Baseline", "1 year later", "2 years later", "3 years later",
             "4 years later", "5 years later",
             "Slower scale-up, baseline dates", "Slower scale-up, 1 year later"),
  delay  = c(0:5, 0, 1),
  slower = c(rep(FALSE, 6), TRUE, TRUE))


# ---- Numbers for panel A ----------------------------------------------
# Deaths prevented among infants born in each year, added up year by
# year. The last row equals deaths prevented in results_scenarios.csv.
cumulative <- matrix(NA, length(years), nrow(schedules),
                     dimnames = list(years, schedules$label))
for (j in 1:nrow(schedules)) {
  d <- demand[[schedules$file[j]]]
  by_year <- sapply(years, function(y) scenario_outcomes(d[d$year == y, ])["deaths"])
  cumulative[, j] <- cumsum(by_year)
}
stopifnot(abs(cumulative[length(years), ] -
              results$deaths[match(schedules$file, results$scenario)]) < 1e-6)

write.csv(data.frame(schedule   = rep(schedules$label, each = length(years)),
                     year       = years,
                     cumulative = as.vector(cumulative)),
          "figure1_series.csv", row.names = FALSE)


# ---- Numbers for panel B ----------------------------------------------
# Percentage change from baseline for every random order (the baseline
# column is always 0, so we drop it), and for the primary analysis.
orders  <- read.csv("results_random_orders.csv")
by_order <- as.matrix(read.csv("results_random_orders_deaths.csv")[, -1])
pct_orders  <- 100 * (by_order[, -1] - by_order[, 1]) / by_order[, 1]
pct_primary <- orders$pct_change_primary[-1]
pct_lower   <- orders$pct_change_lower[-1]
pct_upper   <- orders$pct_change_upper[-1]


# ---- Look -------------------------------------------------------------
# One blue, darker for the baseline and lighter with each year of
# delay. Slower scale-up is dashed, in the color of its start dates.
blues <- c("#08306b", "#2171b5", "#4292c6", "#6baed6", "#9ecae1", "#c6dbef")
col   <- blues[schedules$delay + 1]
lty   <- ifelse(schedules$slower, 2, 1)
comma <- function(x) format(x, big.mark = ",", trim = TRUE)
minus <- function(x) gsub("-", "\u2212", x)


# ---- Panel A ------------------------------------------------------------
draw_a <- function() {
  top <- ceiling(max(cumulative) / 10000) * 10000
  par(mar = c(4, 4.8, 0.8, 0.8), family = "sans", las = 1, mgp = c(3.4, 0.7, 0))
  plot(NA, xlim = range(years), ylim = c(0, top), xlab = "",
       ylab = "Cumulative deaths prevented", axes = FALSE, xaxs = "i", yaxs = "i")
  abline(h = seq(10000, top, 10000), col = "grey92", lwd = 0.8)
  axis(1, at = years, cex.axis = 0.8, col = "grey40")
  axis(2, at = seq(0, top, 10000), labels = comma(seq(0, top, 10000)),
       cex.axis = 0.8, col = "grey40")
  mtext("Birth year", side = 1, line = 2.5, cex = 0.95)
  for (j in nrow(schedules):1)
    lines(years, cumulative[, j], col = col[j], lwd = 2.2, lty = lty[j])
  totals <- comma(round(cumulative[length(years), ], -2))
  legend("topleft", legend = paste0(schedules$label, " (", totals, ")"),
         col = col, lty = lty, lwd = 2.2, bty = "n", cex = 0.72,
         seg.len = 2.4, y.intersp = 1.2, inset = c(0.01, 0.01))
}


# ---- Panel B ------------------------------------------------------------
# Rows from the top: the five delays, a gap, then the two slower
# scale-up schedules. Points are spread up and down a little so they
# can be seen. The spread is random but fixed by set.seed, and it does
# not change any result.
draw_b <- function() {
  rows <- c(7.4, 6.4, 5.4, 4.4, 3.4, 1.8, 0.8)
  par(mar = c(4, 12.6, 0.8, 7.6), family = "sans", las = 1, mgp = c(2.6, 0.7, 0))
  plot(NA, xlim = c(-80, 0), ylim = c(0.2, 9.7), axes = FALSE, xlab = "", ylab = "", xaxs = "i")
  abline(v = seq(-70, -10, 10), col = "grey92", lwd = 0.8)
  abline(v = 0, col = "grey55", lwd = 1)
  axis(1, at = seq(-80, 0, 10), labels = minus(seq(-80, 0, 10)), cex.axis = 0.8, col = "grey40")
  mtext("Change in deaths prevented from baseline, %", side = 1, line = 2.5, cex = 0.95)
  axis(2, at = rows, labels = schedules$label[-1], tick = FALSE, cex.axis = 0.8, line = -0.6)

  set.seed(1)
  for (j in seq_along(rows)) {
    points(pct_orders[, j], rows[j] + runif(nrow(pct_orders), -0.22, 0.22),
           pch = 16, cex = 0.32, col = adjustcolor(blues[3], 0.35))
    segments(pct_primary[j], rows[j] - 0.32, pct_primary[j], rows[j] + 0.32,
             lwd = 3, col = blues[1])
    # Right-hand column: the primary analysis, then the 2.5th to 97.5th
    # percentiles across random orders.
    par(xpd = NA)
    text(3, rows[j], minus(paste0(round(pct_primary[j]), " (", round(pct_lower[j]),
                                  " to ", round(pct_upper[j]), ")")),
         adj = c(0, 0.5), cex = 0.75, col = "grey15")
    par(xpd = FALSE)
  }

  # Key, top left.
  segments(-78.5, 9.25, -78.5, 9.65, lwd = 3, col = blues[1])
  text(-76.5, 9.45, "Primary analysis", adj = c(0, 0.5), cex = 0.7, col = "grey15")
  points(c(-79.1, -78.5, -77.9), c(8.65, 8.75, 8.67), pch = 16, cex = 0.45,
         col = adjustcolor(blues[3], 0.6))
  text(-76.5, 8.7, "Each of 1,000 random country orders", adj = c(0, 0.5), cex = 0.7, col = "grey15")
}


# ---- Save ---------------------------------------------------------------
png("figure1a.png", width = 6.5, height = 4.5, units = "in", res = 300); draw_a(); dev.off()
tiff("figure1a.tiff", width = 6.5, height = 4.5, units = "in", res = 300, compression = "lzw"); draw_a(); dev.off()
png("figure1b.png", width = 6.5, height = 3.6, units = "in", res = 300); draw_b(); dev.off()
tiff("figure1b.tiff", width = 6.5, height = 3.6, units = "in", res = 300, compression = "lzw"); draw_b(); dev.off()
