## =============================================================================
## Ingest: UN World Population Prospects 2019 -> analysis-ready demographic inputs
##
## Source: United Nations, Department of Economic and Social Affairs, Population
##   Division (2019). World Population Prospects 2019. Distributed as the R data
##   package `wpp2019` (author: UN Population Division; maintainer: H. Sevcikova),
##   CRAN, version 1.1-1, published 2020-01-31, DOI 10.32614/CRAN.package.wpp2019.
##   Source repository: https://github.com/PPgp/wpp2019
##   The package ships every table as plain tab-delimited text under data/, so
##   this script needs only base R.
##
## Why WPP 2019 and not 2024: the source acute-RSV model (Mahmud et al., BMC
##   Medicine 2023, UNIVAC) used the 2019 revision. Reproducing its baseline
##   requires the same demographic inputs.
##
## Outputs (one row per country):
##   births_2025        live births in calendar 2025 = sum over 5-year maternal
##                      age groups of TFR(2025-30) x %ASFR(2025-30)/100/5 x
##                      female population(1 July 2025)
##   PY_per_birth       person-years lived from birth to age 5 per live birth,
##                      = (1L0 + 4L1)/l0 from an abridged life table built on
##                      WPP 2019 mx(2025-30); both sexes weighted by the sex
##                      ratio at birth; a0 by Andreev-Kingkade, a1 = 1.5
##   PY_0to5            births_2025 x PY_per_birth  (UNIVAC's "PY")
##   e0                 life expectancy at birth, 2025-30, both sexes
##   u5mr_per1000       under-five mortality implied by the same life table
##
## Usage:  Rscript ingest_wpp2019.R <path-to-wpp2019-master/data> <country-list.csv> <out.csv>
##   country-list.csv must have columns iso3, country (names as in UN M49 list
##   or one of the aliases below).
## =============================================================================

args   <- commandArgs(trailingOnly = TRUE)
D      <- if (length(args) >= 1) args[1] else "wpp2019-master/data"
clist  <- if (length(args) >= 2) args[2] else "inputs_core.csv"
outf   <- if (length(args) >= 3) args[3] else "inputs_wpp2019_2025cohort.csv"

PERIOD <- "2025-2030"   # mortality / fertility period for the 2025 birth cohort
POPYR  <- "2025"        # 1 July population used for births
DISC   <- 0.03          # for discounted life expectancy, used in YLL

rd <- function(f) read.delim(file.path(D, f), check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
mxF <- rd("mxF.txt"); mxM <- rd("mxM.txt"); sr <- rd("sexRatio.txt")
e0F <- rd("e0Fproj.txt"); e0M <- rd("e0Mproj.txt")
asfr <- rd("percentASFR.txt"); tfr <- rd("tfrprojMed.txt"); popF <- rd("popFprojMed.txt")
loc <- rd("UNlocations.txt")

## ---- Country matching on normalised names ----------------------------------
norm <- function(s) {
  s <- enc2utf8(s)
  s <- chartr("\u00e0\u00e1\u00e2\u00e3\u00e4\u00e7\u00e8\u00e9\u00ea\u00eb\u00ed\u00ee\u00f1\u00f3\u00f4\u00f6\u00fa\u00fc",
              "aaaaaceeeeiinooouu", s)          # strip accents without locale dependence
  s <- gsub("[^a-zA-Z0-9 ,'().-]", "", s); s <- tolower(s)
  s <- gsub("dem\\. people's", "democratic people's", s)
  s <- gsub("\\(fed\\. states of\\)", "(federated states of)", s)
  s <- gsub("vietnam", "viet nam", s)
  trimws(gsub("\\s+", " ", s))
}
alias <- c("bolivia" = "bolivia (plurinational state of)",
           "kyrgyz republic" = "kyrgyzstan",
           "lao pdr" = "lao people's democratic republic",
           "moldova" = "republic of moldova",
           "tanzania" = "united republic of tanzania",
           "yemen, rep." = "yemen",
           "micronesia" = "micronesia (federated states of)")
countries <- read.csv(clist, stringsAsFactors = FALSE, encoding = "UTF-8")
key <- norm(countries$country); key <- ifelse(key %in% names(alias), alias[key], key)
loc$key <- norm(loc$name)
countries$country_code <- loc$country_code[match(key, loc$key)]
if (any(is.na(countries$country_code)))
  stop("no UN location for: ", paste(countries$country[is.na(countries$country_code)], collapse = ", "))
cc <- countries$country_code

## ---- Abridged life table, ages 0 and 1-4 -------------------------------------
lt5 <- function(mx, sex) {
  m0 <- mx[mx$age == 0, ]; m0 <- setNames(m0[[PERIOD]], m0$country_code)[as.character(cc)]
  m1 <- mx[mx$age == 1, ]; m1 <- setNames(m1[[PERIOD]], m1$country_code)[as.character(cc)]
  a0 <- if (sex == "M") ifelse(m0 < 0.107, 0.053 + 2.800 * m0, 0.330)
        else            ifelse(m0 < 0.107, 0.050 + 3.000 * m0, 0.350)
  q0 <- m0 / (1 + (1 - a0) * m0)
  q1 <- 4 * m1 / (1 + (4 - 1.5) * m1)
  l1 <- 1 - q0; l5 <- l1 * (1 - q1)
  data.frame(L0 = l1 + a0 * q0, L1_4 = 4 * l5 + 1.5 * (l1 - l5), l5 = l5)
}
F <- lt5(mxF, "F"); M <- lt5(mxM, "M")
srb <- setNames(sr[[PERIOD]], sr$country_code)[as.character(cc)]
pm  <- srb / (1 + srb)                                  # proportion male at birth
L0 <- pm * M$L0 + (1 - pm) * F$L0
L14 <- pm * M$L1_4 + (1 - pm) * F$L1_4
l5 <- pm * M$l5 + (1 - pm) * F$l5
e0 <- pm * setNames(e0M[[PERIOD]], e0M$country_code)[as.character(cc)] +
     (1 - pm) * setNames(e0F[[PERIOD]], e0F$country_code)[as.character(cc)]

## ---- Births in 2025 from the fertility schedule -----------------------------
ages <- c("15-19","20-24","25-29","30-34","35-39","40-44","45-49")
births <- sapply(cc, function(c) {
  T  <- tfr[[PERIOD]][tfr$country_code == c]
  pa <- asfr[asfr$country_code == c & asfr$age %in% ages, ]
  pa <- pa[[PERIOD]][match(ages, pa$age)] / 100
  pf <- popF[popF$country_code == c & popF$age %in% ages, ]
  pf <- pf[[POPYR]][match(ages, pf$age)] * 1000
  sum(T * pa / 5 * pf)
})

out <- data.frame(iso3 = countries$iso3, country = countries$country,
                  un_code = cc, births_2025 = births,
                  PY_per_birth = L0 + L14, PY_0to5 = births * (L0 + L14),
                  e0 = e0, e0_discounted = (1 - (1 + DISC)^(-e0)) / DISC,
                  u5mr_per1000 = 1000 * (1 - l5))
write.csv(out, outf, row.names = FALSE)
cat("wrote", nrow(out), "countries to", outf, "\n")
cat("PY per birth: median", round(median(out$PY_per_birth), 3),
    "| e0 median", round(median(out$e0), 1), "\n")
