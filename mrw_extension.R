# ====== HC UNRESTRICTED EXTENSIONS with PWT controls (1960–2023) ======

ext_plus <- file.path(data_dir, "mrw_with_2023_plus_controls.csv")
stopifnot(file.exists(ext_plus))

mrw23_plus <- readr::read_csv(ext_plus) %>%
  dplyr::select(
    country, n, i, o, rgdpw60, rgdpw23, gdpgrowth_23, popgrowth_23, i_y_23, school_23,
    ln_pl_i_23, alpha_23, g_meas_23, delta_23, lngd_meas_23
  )

# Merge with MRW base already in memory as `mrw`
mrw_ext_controls <- mrw23_plus %>%
  dplyr::mutate(
    # LHS and core RHS (1960 → 2023)
    ly60    = log(rgdpw60),
    ly85    = log(rgdpw23),
    ls      = log(i_y_23 / 100),
    lschool = log(school_23),
    # Baseline MRW n+g+δ and measured alternative
    lngd        = log(popgrowth_23/100 + 0.05),
    lngd_meas   = dplyr::coalesce(lngd_meas_23, log(popgrowth_23/100 + 0.05)),
    # PWT controls
    ln_pl_i = ln_pl_i_23
  )

samples23c <- list(
  'Non-Oil'      = dplyr::filter(mrw_ext_controls, n == 1),
  'Intermediate' = dplyr::filter(mrw_ext_controls, n == 1, i == 1),
  'OECD'         = dplyr::filter(mrw_ext_controls, n == 1, o == 1)
)

# 1) HC unrestricted + price control
models_hc_unres_23_pl  <- purrr::map(samples23c, ~ lm(ly85 ~ ls + lngd + lschool + ln_pl_i, data = .x))

# 2) HC unrestricted + measured (n+g+δ)
models_hc_unres_23_meas <- purrr::map(samples23c, ~ lm(ly85 ~ ls + lngd_meas + lschool, data = .x))

# 3) HC unrestricted + full controls (price + measured (n+g+δ))
models_hc_unres_23_full <- purrr::map(samples23c, ~ lm(ly85 ~ ls + lngd_meas + lschool + ln_pl_i, data = .x))

# Tables (reuse your stat pack that reports implied α, β from ls and lschool)
term_labels_hc_pl <- c(
  '(Intercept)' = "Constant",
  ls = "log(s)",
  lngd = "log(n+g+d)",
  lschool = "log(s_h)",
  ln_pl_i = "log(p^I)"
)

term_labels_hc_meas <- c(
  '(Intercept)' = "Constant",
  ls = "log(s)",
  lngd_meas = "log(n+g_meas+δ)",
  lschool = "log(s_h)"
)

term_labels_hc_full <- c(
  '(Intercept)' = "Constant",
  ls = "log(s)",
  lngd_meas = "log(n+g_meas+δ)",
  lschool = "log(s_h)",
  ln_pl_i = "log(p^I)"
)

table_hc_unres_23_pl   <- build_table(models_hc_unres_23_pl,   term_labels_hc_pl,   stat_alpha_beta, digits = 2)
table_hc_unres_23_meas <- build_table(models_hc_unres_23_meas, term_labels_hc_meas, stat_alpha_beta, digits = 2)
table_hc_unres_23_full <- build_table(models_hc_unres_23_full, term_labels_hc_full, stat_alpha_beta, digits = 2)

readr::write_csv(table_hc_unres_23_pl,   file.path(tables_dir, "mrw_table_human_capital_unrestricted_with_price_1960_2023.csv"))
readr::write_csv(table_hc_unres_23_meas, file.path(tables_dir, "mrw_table_human_capital_unrestricted_measured_ngd_1960_2023.csv"))
readr::write_csv(table_hc_unres_23_full, file.path(tables_dir, "mrw_table_human_capital_unrestricted_full_controls_1960_2023.csv"))

cat("HC unrestricted extensions written:\n",
    "- with price control: ", file.path(tables_dir, "mrw_table_human_capital_unrestricted_with_price_1960_2023.csv"), "\n",
    "- with measured n+g+δ: ", file.path(tables_dir, "mrw_table_human_capital_unrestricted_measured_ngd_1960_2023.csv"), "\n",
    "- full controls: ", file.path(tables_dir, "mrw_table_human_capital_unrestricted_full_controls_1960_2023.csv"), "\n")
# ====== END EXTENSIONS ======
