suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(countrycode)
})

# -----------------------------------
# Parameters
# -----------------------------------
ext_window_start <- 1960L
ext_window_end   <- 2023L

# -----------------------------------
# Helpers
# -----------------------------------
cap_pct <- function(x) pmin(pmax(x, 0), 100)

# geometric annual average growth (fraction):
g_annual <- function(x1, x2, T_years) {
  ifelse(is.na(x1) | is.na(x2) | x1 <= 0 | x2 <= 0, NA_real_,
         (x2/x1)^(1/T_years) - 1)
}

mean_window <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

summarise_window <- function(dat, y1, y2){
  dat %>%
    dplyr::filter(year >= y1, year <= y2) %>%
    dplyr::arrange(iso3c, country, year) %>%   # ensure time order
    dplyr::group_by(iso3c, country) %>%
    dplyr::summarise(
      # First/last non-NA y_per_adult within the window
      yA_y1 = dplyr::first(y_per_adult, default = NA_real_),
      yA_y2 = dplyr::last( y_per_adult[!is.na(y_per_adult)],  default = NA_real_),
      
      # GDP: first/last non-NA endpoints + actual year span
      gy = {
        v0 <- dplyr::first(rgdpna[!is.na(rgdpna)], default = NA_real_)
        v1 <- dplyr::last( rgdpna[!is.na(rgdpna)],  default = NA_real_)
        t0 <- dplyr::first(year[!is.na(rgdpna)],    default = NA_integer_)
        t1 <- dplyr::last( year[!is.na(rgdpna)],    default = NA_integer_)
        span_g <- if (is.na(t0) | is.na(t1)) NA_integer_ else (t1 - t0)
        if (is.na(v0) | is.na(v1) | is.na(span_g) | span_g <= 0) NA_real_
        else g_annual(v0, v1, span_g)
      },
      
      # Population (15–64): first/last non-NA endpoints + actual year span
      n = {
        v0 <- dplyr::first(pop1564[!is.na(pop1564)], default = NA_real_)
        v1 <- dplyr::last( pop1564[!is.na(pop1564)],  default = NA_real_)
        t0 <- dplyr::first(year[!is.na(pop1564)],     default = NA_integer_)
        t1 <- dplyr::last( year[!is.na(pop1564)],     default = NA_integer_)
        span_n <- if (is.na(t0) | is.na(t1)) NA_integer_ else (t1 - t0)
        if (is.na(v0) | is.na(v1) | is.na(span_n) | span_n <= 0) NA_real_
        else g_annual(v0, v1, span_n)
      },
      
      IY     = mean(csh_i,  na.rm = TRUE),
      SCHOOL = mean(SCHOOL, na.rm = TRUE),
      .groups = "drop"
    )
}



# -----------------------------------
# MRW ISO set (filter universe)
# -----------------------------------
mrw_iso3c <- c(
  "DZA","AGO","BEN","BWA","BFA","BDI","CMR","CAF","TCD","COG","EGY","ETH","GAB","GMB","GHA","GIN",
  "CIV","KEN","LSO","LBR","MDG","MWI","MLI","MRT","MUS","MAR","MOZ","NER","NGA","RWA","SEN","SLE",
  "SOM","ZAF","SDN","SWZ","TZA","TGO","TUN","UGA","COD","ZMB","ZWE",
  "AFG","BHR","BGD","MMR","HKG","IND","IRN","IRQ","ISR","JPN","JOR","KOR","KWT","MYS","NPL","OMN",
  "PAK","PHL","SAU","SGP","LKA","SYR","TWN","THA","ARE","YEM",
  "AUT","BEL","CYP","DNK","FIN","FRA","DEU","GRC","ISL","IRL","ITA","LUX","MLT","NLD","NOR","PRT",
  "ESP","SWE","CHE","TUR","GBR",
  "BRB","CAN","CRI","DOM","SLV","GTM","HTI","HND","JAM","MEX","NIC","PAN","TTO","USA",
  "ARG","BOL","BRA","CHL","COL","ECU","GUY","PRY","PER","SUR","URY","VEN",
  "AUS","FJI","IDN","NZL","PNG"
)

# Legacy-name overrides (for mrw.csv if it lacks iso3c)
name_overrides <- c(
  "Congo,"="COG","Ivory Coast"="CIV","Korea, South"="KOR","Germany, W."="DEU",
  "U.A.E."="ARE","Dominican Rep."="DOM","Trinidad and Tobago"="TTO","Surinam"="SUR",
  "Taiwan"="TWN","Zaire"="COD","Hong Kong"="HKG","Papua New Guinea"="PNG",
  "Gambia, The"="GMB","Central Afr. Rep."="CAF","S. Africa"="ZAF","Swaziland"="SWZ"
)

# -----------------------------------
# Load PWT 11
# -----------------------------------
stopifnot(file.exists("data/pwt110.xlsx"))
pwt <- read_excel("data/pwt110.xlsx", sheet = "Data")

pwt_core <- pwt %>%
  transmute(
    countrycode, country, year,
    rgdpna, emp, pop, csh_i, delta, rtfpna, labsh, hc, pl_i, csh_x, csh_m, csh_g
  )

# -----------------------------------
# Load WDI 15–64
# -----------------------------------
stopifnot(file.exists("data/POP.csv"))
wdi_raw <- read_csv("data/POP.csv", skip = 4)

wdi_1564 <- wdi_raw %>%
  select(`Country Code`, starts_with("19"), starts_with("20")) %>%
  rename(iso3c = `Country Code`) %>%
  pivot_longer(-iso3c, names_to = "year", values_to = "pop1564") %>%
  mutate(year = suppressWarnings(as.integer(year))) %>%
  filter(!is.na(year))

# -----------------------------------
# Load enrolment (GER.2T3 + SAP.2T3) -> SCHOOL
# -----------------------------------
stopifnot(file.exists("data/data.csv"))
enr_raw <- read_csv("data/data.csv")

enr_wide <- enr_raw %>%
  mutate(year = as.integer(year)) %>%
  select(indicatorId, geoUnit, year, value) %>%
  distinct() %>%
  pivot_wider(names_from = indicatorId, values_from = value) %>%
  rename(iso3c = geoUnit,
         ger_sec = `GER.2T3`,
         pop_sec_off = `SAP.2T3`) %>%
  mutate(ger_sec = cap_pct(ger_sec))

enr <- enr_wide %>%
  left_join(wdi_1564, by = c("iso3c","year")) %>%
  mutate(share_off = ifelse(!is.na(pop_sec_off) & !is.na(pop1564) & pop1564 > 0,
                            pop_sec_off / pop1564, NA_real_),
         SCHOOL = ifelse(!is.na(ger_sec) & !is.na(share_off),
                         ger_sec * share_off, NA_real_))
# SCHOOL is already in percent units

# -----------------------------------
# Merge (PWT + WDI + SCHOOL) and filter to MRW universe
# -----------------------------------
df <- pwt_core %>%
  rename(iso3c = countrycode) %>%
  left_join(wdi_1564, by = c("iso3c", "year")) %>%
  left_join(enr %>% select(iso3c, year, SCHOOL), by = c("iso3c","year")) %>%
  filter(iso3c %in% mrw_iso3c) %>%
  mutate(
    # GDP per adult (working-age 15–64)
    y_per_adult = ifelse(!is.na(pop1564) & pop1564 > 0, (rgdpna * 1e6) / pop1564, NA_real_)
  )

# -----------------------------------
# Compute 1960 -> 2023 window
# -----------------------------------
tab_ext <- summarise_window(df, ext_window_start, ext_window_end) %>% arrange(country)

# -----------------------------------
# Load your MRW base and get 1960 level to anchor
# -----------------------------------
stopifnot(file.exists("data/mrw.csv"))
mrw <- read_csv("data/mrw.csv")

# Ensure iso3c exists in mrw; if not, map from 'country'
if (!("iso3c" %in% names(mrw))) {
  stopifnot("country" %in% names(mrw))
  mrw <- mrw %>%
    mutate(country = trimws(country),
           iso3c = ifelse(country %in% names(name_overrides),
                          name_overrides[country],
                          suppressWarnings(countrycode(country, "country.name", "iso3c"))))
}

mrw_1960_col <- "rgdpw60"

mrw_1960 <- mrw %>%
  transmute(iso3c, yA_1960_mrw = .data[[mrw_1960_col]])

# -----------------------------------
# Rebase: make 1960 match MRW; scale 2023 by same factor (keeps growth)
# Also convert all rates/shares to percent (×100)
# -----------------------------------
rebased <- tab_ext %>%
  left_join(mrw_1960, by = "iso3c") %>%
  mutate(scale = ifelse(!is.na(yA_1960_mrw) & !is.na(yA_y1) & yA_y1 > 0,
                        yA_1960_mrw / yA_y1, NA_real_),
         yA_1960 = ifelse(!is.na(yA_1960_mrw), yA_1960_mrw, yA_y1),
         yA_2023 = yA_y2 * ifelse(is.na(scale), 1, scale),
         gy_pct  = gy * 100,         # % p.a.
         n_pct   = n  * 100,         # % p.a.
         IY_pct  = IY * 100,         # share -> %
         SCHOOL_pct = SCHOOL         # already in %
  ) %>%
  select(iso3c, country, yA_1960, yA_2023, gy_pct, n_pct, IY_pct, SCHOOL_pct)


# -----------------------------------
# Merge back into MRW DB and save
# -----------------------------------
mrw_with_2023 <- mrw %>%
  left_join(rebased %>% select(-country), by = "iso3c")

mrw_with_2023_final <- mrw_with_2023 %>%
  # drop unwanted columns
  dplyr::select(-iso3c, -yA_1960) %>%
  # rename the 2023 level
  dplyr::rename(rgdpw23 = yA_2023) %>%
  # keep original MRW columns and add the 1985–2023 metrics with _23 suffix
  dplyr::mutate(
    gdpgrowth_23  = gy_pct,
    popgrowth_23  = n_pct,
    i_y_23        = IY_pct,
    school_23     = SCHOOL_pct
  ) %>%
  # drop the temporary source columns for the new metrics
  dplyr::select(-gy_pct, -n_pct, -IY_pct, -SCHOOL_pct) %>%
  # optional: set a tidy column order
  dplyr::select(
    number, country, n, i, o,
    rgdpw60, rgdpw85,
    gdpgrowth, popgrowth, i_y, school,
    rgdpw23, gdpgrowth_23, popgrowth_23, i_y_23, school_23,
    dplyr::everything()
  )

# export
readr::write_csv(mrw_with_2023_final, "data/mrw_with_2023_final.csv")


