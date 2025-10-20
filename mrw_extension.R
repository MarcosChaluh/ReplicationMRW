suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
})

source("mrw_tables_figures_utils.R")

dirs <- ensure_output_dirs()

data_dir <- dirs$data_dir

tables_dir <- dirs$tables_dir

ext_plus <- file.path(data_dir, "mrw_with_2023_plus_controls.csv")
stopifnot(file.exists(ext_plus))

mrw23_plus <- readr::read_csv(ext_plus) %>%
  dplyr::select(
    country, n, i, o, rgdpw60, rgdpw23, gdpgrowth_23, popgrowth_23, i_y_23, school_23,
    ln_pl_i_23, alpha_23, g_meas_23, delta_23, lngd_meas_23
  )

mrw_ext_controls <- mrw23_plus %>%
  dplyr::mutate(
    ly60 = log(rgdpw60),
    ly85 = log(rgdpw23),
    ls = log(i_y_23 / 100),
    lschool = log(school_23),
    lngd = log(popgrowth_23 / 100 + 0.05),
    lngd_meas = dplyr::coalesce(lngd_meas_23, log(popgrowth_23 / 100 + 0.05)),
    ln_pl_i = ln_pl_i_23
  )

samples23c <- list(
  "Non-Oil" = dplyr::filter(mrw_ext_controls, n == 1),
  "Intermediate" = dplyr::filter(mrw_ext_controls, n == 1, i == 1),
  "OECD" = dplyr::filter(mrw_ext_controls, n == 1, o == 1)
)

models_hc_unres_23_pl <- purrr::map(samples23c, ~ stats::lm(ly85 ~ ls + lngd + lschool + ln_pl_i, data = .x))
models_hc_unres_23_meas <- purrr::map(samples23c, ~ stats::lm(ly85 ~ ls + lngd_meas + lschool, data = .x))
models_hc_unres_23_full <- purrr::map(samples23c, ~ stats::lm(ly85 ~ ls + lngd_meas + lschool + ln_pl_i, data = .x))

term_labels_hc_pl <- c(
  "(Intercept)" = "Constant",
  ls = "log(s)",
  lngd = "log(n+g+d)",
  lschool = "log(s_h)",
  ln_pl_i = "log(p^I)"
)

term_labels_hc_meas <- c(
  "(Intercept)" = "Constant",
  ls = "log(s)",
  lngd_meas = "log(n+g_meas+δ)",
  lschool = "log(s_h)"
)

term_labels_hc_full <- c(
  "(Intercept)" = "Constant",
  ls = "log(s)",
  lngd_meas = "log(n+g_meas+δ)",
  lschool = "log(s_h)",
  ln_pl_i = "log(p^I)"
)

table_hc_unres_23_pl <- build_table(models_hc_unres_23_pl, term_labels_hc_pl, STAT_ALPHA_BETA, digits = 2)
table_hc_unres_23_meas <- build_table(models_hc_unres_23_meas, term_labels_hc_meas, STAT_ALPHA_BETA, digits = 2)
table_hc_unres_23_full <- build_table(models_hc_unres_23_full, term_labels_hc_full, STAT_ALPHA_BETA, digits = 2)

readr::write_csv(table_hc_unres_23_pl, file.path(tables_dir, "mrw_table_human_capital_unrestricted_with_price_1960_2023.csv"))
readr::write_csv(table_hc_unres_23_meas, file.path(tables_dir, "mrw_table_human_capital_unrestricted_measured_ngd_1960_2023.csv"))
readr::write_csv(table_hc_unres_23_full, file.path(tables_dir, "mrw_table_human_capital_unrestricted_full_controls_1960_2023.csv"))

cat(
  "HC unrestricted extensions written:\n",
  "- with price control: ", file.path(tables_dir, "mrw_table_human_capital_unrestricted_with_price_1960_2023.csv"), "\n",
  "- with measured n+g+δ: ", file.path(tables_dir, "mrw_table_human_capital_unrestricted_measured_ngd_1960_2023.csv"), "\n",
  "- full controls: ", file.path(tables_dir, "mrw_table_human_capital_unrestricted_full_controls_1960_2023.csv"), "\n",
  sep = ""
)
