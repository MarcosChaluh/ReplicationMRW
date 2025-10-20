# ===== Add PWT-based controls for 1985–2023 and append to your dataset =====

# Window for controls to match your extended regressions
ctl_window_start <- 1960L
ctl_window_end   <- 2023L

# Summarise PWT controls over 1960–2023
controls_23 <- df %>%
  dplyr::filter(year >= ctl_window_start, year <= ctl_window_end) %>%
  dplyr::group_by(iso3c, country) %>%
  dplyr::summarise(
    # log price of investment (mean over window; guard against non-positive)
    ln_pl_i_23 = {
      v <- pl_i[is.finite(pl_i) & pl_i > 0]
      if (length(v)) log(mean(v, na.rm = TRUE)) else NA_real_
    },
    # alpha = 1 - labor share
    alpha_23   = 1 - mean(labsh, na.rm = TRUE),
    # measured TFP growth (fraction p.a.) using endpoints & actual span
    g_meas_23  = {
      v0 <- dplyr::first(rtfpna[!is.na(rtfpna)], default = NA_real_)
      v1 <- dplyr::last( rtfpna[!is.na(rtfpna)],  default = NA_real_)
      t0 <- dplyr::first(year[!is.na(rtfpna)],    default = NA_integer_)
      t1 <- dplyr::last( year[!is.na(rtfpna)],    default = NA_integer_)
      span <- if (is.na(t0) | is.na(t1)) NA_integer_ else (t1 - t0)
      if (is.na(v0) | is.na(v1) | is.na(span) | span <= 0) NA_real_ else (v1 / v0)^(1/span) - 1
    },
    # average depreciation (fraction p.a.)
    delta_23   = mean(delta, na.rm = TRUE),
    .groups = "drop"
  )

# Join controls to your mid-stage table BEFORE you drop iso3c
mrw_with_2023_plus <- mrw_with_2023 %>%  # this object exists in your script right before you build _final
  dplyr::left_join(controls_23 %>% dplyr::select(-country), by = "iso3c")

# Rebuild the final table with the new variables appended and tidy order
mrw_with_2023_plus_final <- mrw_with_2023_plus %>%
  # carry forward your previous rename/selection logic
  dplyr::select(-iso3c, -yA_1960) %>%
  dplyr::rename(rgdpw23 = yA_2023) %>%
  dplyr::mutate(
    gdpgrowth_23  = gy_pct,
    popgrowth_23  = n_pct,
    i_y_23        = IY_pct,
    school_23     = SCHOOL_pct
  ) %>%
  dplyr::select(-gy_pct, -n_pct, -IY_pct, -SCHOOL_pct) %>%
  # Build measured log(n+g+δ) using your popgrowth_23 (% p.a.) and the new g, δ (fractions)
  dplyr::mutate(
    lngd_meas_23 = ifelse(is.na(popgrowth_23) | is.na(g_meas_23) | is.na(delta_23),
                          NA_real_,
                          log(popgrowth_23/100 + g_meas_23 + delta_23))
  ) %>%
  # Put the new controls at the END
  dplyr::select(
    number, country, n, i, o,
    rgdpw60, rgdpw85,
    gdpgrowth, popgrowth, i_y, school,
    rgdpw23, gdpgrowth_23, popgrowth_23, i_y_23, school_23,
    dplyr::everything(),        # keep any other columns you might have
    ln_pl_i_23, alpha_23, g_meas_23, delta_23, lngd_meas_23
  )

# Save both the controls-only table and the augmented MRW dataset
readr::write_csv(controls_23, "data/pwt_controls_1985_2023.csv")
readr::write_csv(mrw_with_2023_plus_final, "data/mrw_with_2023_plus_controls.csv")

message(
  "Added variables (1985–2023): ln_pl_i_23, alpha_23, g_meas_23, delta_23, lngd_meas_23.\n",
  "- Controls table: results/tables/pwt_controls_1985_2023.csv\n",
  "- Augmented MRW:  results/tables/mrw_with_2023_plus_controls.csv"
)