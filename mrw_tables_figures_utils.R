# Utility helpers shared across MRW reporting scripts

`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

format_with_stars <- function(estimate, p_value, digits = 2) {
  if (is.na(estimate)) {
    return("")
  }
  base <- formatC(estimate, format = "f", digits = digits)
  if (is.na(p_value)) {
    return(base)
  }
  stars <- if (p_value < 0.01) {
    "***"
  } else if (p_value < 0.05) {
    "**"
  } else if (p_value < 0.1) {
    "*"
  } else {
    ""
  }
  paste0(base, stars)
}

build_table <- function(models, term_map, stat_specs = list(), digits = 2) {
  model_names <- names(models)
  tidy_models <- purrr::map(models, broom::tidy)

  coef_rows <- purrr::map_dfr(seq_along(term_map), function(i) {
    term_name <- names(term_map)[i]
    label <- unname(term_map[i])
    values <- purrr::map_chr(model_names, function(model_name) {
      tm <- tidy_models[[model_name]]
      row <- tm[tm$term == term_name, , drop = FALSE]
      if (nrow(row) == 0) {
        return("")
      }
      format_with_stars(row$estimate, row$p.value, digits = digits)
    })
    value_row <- tibble::as_tibble_row(stats::setNames(as.list(values), model_names))
    dplyr::bind_cols(tibble::tibble(term = label), value_row)
  })

  if (length(stat_specs) == 0) {
    return(coef_rows)
  }

  stat_rows <- purrr::map_dfr(names(stat_specs), function(label) {
    spec <- stat_specs[[label]]
    fn <- spec$fn
    digits_spec <- spec$digits %||% digits
    fmt <- spec$fmt
    values <- purrr::map_chr(models, function(mod) {
      value <- fn(mod)
      if (is.na(value)) {
        return("")
      }
      if (!is.null(fmt)) {
        return(sprintf(fmt, value))
      }
      formatC(value, format = "f", digits = digits_spec)
    })
    value_row <- tibble::as_tibble_row(stats::setNames(as.list(values), model_names))
    dplyr::bind_cols(tibble::tibble(term = label), value_row)
  })

  dplyr::bind_rows(coef_rows, stat_rows)
}

calc_adj_r2 <- function(mod) {
  summary(mod)$adj.r.squared
}

calc_n <- function(mod) {
  stats::nobs(mod)
}

calc_sigma <- function(mod) {
  summary(mod)$sigma
}

implied_alpha_ls <- function(mod) {
  beta <- stats::coef(mod)[["ls"]]
  if (is.null(beta) || is.na(beta)) {
    return(NA_real_)
  }
  beta / (1 + beta)
}

implied_alpha_lslngd <- function(mod) {
  beta <- stats::coef(mod)[["ls_lngd"]]
  if (is.null(beta) || is.na(beta)) {
    return(NA_real_)
  }
  beta / (1 + beta)
}

implied_alpha_hc <- function(mod) {
  beta_s <- stats::coef(mod)[["ls"]]
  beta_h <- stats::coef(mod)[["lschool"]]
  if (any(is.null(c(beta_s, beta_h))) || any(is.na(c(beta_s, beta_h)))) {
    return(NA_real_)
  }
  denom <- 1 + beta_s + beta_h
  beta_s / denom
}

implied_beta_hc <- function(mod) {
  beta_s <- stats::coef(mod)[["ls"]]
  beta_h <- stats::coef(mod)[["lschool"]]
  if (any(is.null(c(beta_s, beta_h))) || any(is.na(c(beta_s, beta_h)))) {
    return(NA_real_)
  }
  denom <- 1 + beta_s + beta_h
  beta_h / denom
}

implied_alpha_hc_res <- function(mod) {
  beta_s <- stats::coef(mod)[["ls_lngd"]]
  beta_h <- stats::coef(mod)[["lsch_ngd"]]
  if (any(is.null(c(beta_s, beta_h))) || any(is.na(c(beta_s, beta_h)))) {
    return(NA_real_)
  }
  denom <- 1 + beta_s + beta_h
  beta_s / denom
}

implied_beta_hc_res <- function(mod) {
  beta_s <- stats::coef(mod)[["ls_lngd"]]
  beta_h <- stats::coef(mod)[["lsch_ngd"]]
  if (any(is.null(c(beta_s, beta_h))) || any(is.na(c(beta_s, beta_h)))) {
    return(NA_real_)
  }
  denom <- 1 + beta_s + beta_h
  beta_h / denom
}

implied_lambda <- function(mod) {
  beta <- stats::coef(mod)[["ly60"]]
  if (is.null(beta) || is.na(beta)) {
    return(NA_real_)
  }
  -log(beta + 1) / 25
}

STAT_BASE <- list(
  "R^2" = list(fn = calc_adj_r2, digits = 2),
  "N" = list(fn = calc_n, fmt = "%.0f"),
  "s.e.e." = list(fn = calc_sigma, digits = 2)
)

STAT_ALPHA <- c(
  STAT_BASE,
  list("Implied alpha" = list(fn = implied_alpha_ls, digits = 2))
)

STAT_ALPHA_RES <- c(
  STAT_BASE,
  list("Implied alpha" = list(fn = implied_alpha_lslngd, digits = 2))
)

STAT_ALPHA_BETA <- c(
  STAT_BASE,
  list(
    "Implied alpha" = list(fn = implied_alpha_hc, digits = 2),
    "Implied beta" = list(fn = implied_beta_hc, digits = 2)
  )
)

STAT_ALPHA_BETA_RES <- c(
  STAT_BASE,
  list(
    "Implied alpha" = list(fn = implied_alpha_hc_res, digits = 2),
    "Implied beta" = list(fn = implied_beta_hc_res, digits = 2)
  )
)

STAT_LAMBDA <- c(
  STAT_BASE,
  list("Implied lambda" = list(fn = implied_lambda, digits = 5))
)

STAT_LAMBDA_ALPHA_BETA_RES <- c(
  STAT_BASE,
  list(
    "Implied lambda" = list(fn = implied_lambda, digits = 5),
    "Implied alpha" = list(fn = implied_alpha_hc_res, digits = 2),
    "Implied beta" = list(fn = implied_beta_hc_res, digits = 2)
  )
)

TERM_LABELS_BASIC <- c(
  "(Intercept)" = "Constant",
  ls = "log(s)",
  lngd = "log(n+g+d)"
)

TERM_LABELS_RESTRICTED <- c(
  "(Intercept)" = "Constant",
  ls_lngd = "log(s) - log(n+g+d)"
)

TERM_LABELS_HC <- c(
  "(Intercept)" = "Constant",
  ls = "log(s)",
  lngd = "log(n+g+d)",
  lschool = "log(s_h)"
)

TERM_LABELS_HC_RES <- c(
  "(Intercept)" = "Constant",
  ls_lngd = "log(s) - log(n+g+d)",
  lsch_ngd = "log(s_h) - log(n+g+d)"
)

TERM_LABELS_CONVERGENCE <- c(
  "(Intercept)" = "Constant",
  ly60 = "log(y_0)"
)

TERM_LABELS_CONVERGENCE_COND <- c(
  "(Intercept)" = "Constant",
  ly60 = "log(y_0)",
  ls = "log(s)",
  lngd = "log(n+g+d)"
)

TERM_LABELS_CONVERGENCE_COND_HC <- c(
  "(Intercept)" = "Constant",
  ly60 = "log(y_0)",
  ls = "log(s)",
  lngd = "log(n+g+d)",
  lschool = "log(s_h)"
)

TERM_LABELS_CONVERGENCE_RES <- c(
  "(Intercept)" = "Constant",
  ly60 = "log(y_0)",
  ls_lngd = "log(s) - log(n+g+d)",
  lsch_ngd = "log(s_h) - log(n+g+d)"
)

ensure_output_dirs <- function(data_dir = "data",
                               results_dir = "results",
                               tables_dir = file.path(results_dir, "tables"),
                               figures_dir = file.path(results_dir, "figures")) {
  if (!dir.exists(data_dir)) {
    dir.create(data_dir)
  }
  if (!dir.exists(results_dir)) {
    dir.create(results_dir)
  }
  if (!dir.exists(tables_dir)) {
    dir.create(tables_dir, recursive = TRUE)
  }
  if (!dir.exists(figures_dir)) {
    dir.create(figures_dir, recursive = TRUE)
  }
  list(
    data_dir = data_dir,
    results_dir = results_dir,
    tables_dir = tables_dir,
    figures_dir = figures_dir
  )
}

build_samples <- function(mrw) {
  list(
    "Non-Oil" = dplyr::filter(mrw, n == 1),
    "Intermediate" = dplyr::filter(mrw, n == 1, i == 1),
    "OECD" = dplyr::filter(mrw, n == 1, o == 1)
  )
}

write_model_table <- function(table, tables_dir, prefix, slug) {
  filename <- sprintf("%s_%s.csv", prefix, slug)
  readr::write_csv(table, file.path(tables_dir, filename))
}

build_convergence_plot <- function(data_d, growth_years_label) {
  mean_income <- mean(data_d$ly60, na.rm = TRUE)
  mean_growth <- mean(data_d$growth, na.rm = TRUE)

  growth_rate <- data_d$growth * 100 / 25

  resid_income_sp <- stats::resid(stats::lm(ly60 ~ lngd + linv, data = data_d))
  resid_growth_sp <- stats::resid(stats::lm(growth ~ lngd + linv, data = data_d))

  resid_income_hc <- stats::resid(stats::lm(ly60 ~ lngd + linv + lschool, data = data_d))
  resid_growth_hc <- stats::resid(stats::lm(growth ~ lngd + linv + lschool, data = data_d))

  plot_data <- dplyr::bind_rows(
    tibble::tibble(
      panel = "Unconditional",
      log_income = data_d$ly60,
      growth_rate = growth_rate
    ),
    tibble::tibble(
      panel = "Conditional on saving and population growth",
      log_income = resid_income_sp + mean_income,
      growth_rate = (resid_growth_sp + mean_growth) * 100 / 25
    ),
    tibble::tibble(
      panel = "Conditional on human capital, saving and population growth",
      log_income = resid_income_hc + mean_income,
      growth_rate = (resid_growth_hc + mean_growth) * 100 / 25
    )
  )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = log_income, y = growth_rate)) +
    ggplot2::geom_point(color = "#1b9e77", alpha = 0.8) +
    ggplot2::facet_wrap(~panel, ncol = 1) +
    ggplot2::labs(
      x = "Log GDP Per Worker in 1960",
      y = sprintf("Growth Rate: %s", growth_years_label),
      title = "Conditional Convergence Across Samples"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}

run_mrw_reporting <- function(mrw, table_prefix, figure_prefix, growth_years_label,
                              tables_dir, figures_dir, digits_table = 2, digits_conv = 5) {
  samples <- build_samples(mrw)

  models_unres <- purrr::map(samples, ~ stats::lm(ly85 ~ ls + lngd, data = .x))
  models_res <- purrr::map(samples, ~ stats::lm(ly85 ~ ls_lngd, data = .x))
  models_hc_unres <- purrr::map(samples, ~ stats::lm(ly85 ~ ls + lngd + lschool, data = .x))
  models_hc_res <- purrr::map(samples, ~ stats::lm(ly85 ~ ls_lngd + lsch_ngd, data = .x))

  table_specs <- list(
    list(models = models_unres, terms = TERM_LABELS_BASIC, stats = STAT_ALPHA, slug = "unrestricted", digits = digits_table),
    list(models = models_res, terms = TERM_LABELS_RESTRICTED, stats = STAT_ALPHA_RES, slug = "restricted", digits = digits_table),
    list(models = models_hc_unres, terms = TERM_LABELS_HC, stats = STAT_ALPHA_BETA, slug = "human_capital_unrestricted", digits = digits_table),
    list(models = models_hc_res, terms = TERM_LABELS_HC_RES, stats = STAT_ALPHA_BETA_RES, slug = "human_capital_restricted", digits = digits_table)
  )

  purrr::walk(table_specs, function(spec) {
    tbl <- build_table(spec$models, spec$terms, spec$stats, digits = spec$digits)
    write_model_table(tbl, tables_dir, table_prefix, spec$slug)
  })

  models_roc <- purrr::map(samples, ~ stats::lm(growth ~ ly60, data = .x))
  models_roc_cond <- purrr::map(samples, ~ stats::lm(growth ~ ly60 + ls + lngd, data = .x))
  models_roc_cond_hc <- purrr::map(samples, ~ stats::lm(growth ~ ly60 + ls + lngd + lschool, data = .x))
  models_roc_res <- purrr::map(samples, ~ stats::lm(growth ~ ly60 + ls_lngd + lsch_ngd, data = .x))

  conv_specs <- list(
    list(models = models_roc, terms = TERM_LABELS_CONVERGENCE, stats = STAT_LAMBDA, slug = "unconditional_convergence", digits = digits_conv),
    list(models = models_roc_cond, terms = TERM_LABELS_CONVERGENCE_COND, stats = STAT_LAMBDA, slug = "conditional_convergence", digits = digits_conv),
    list(models = models_roc_cond_hc, terms = TERM_LABELS_CONVERGENCE_COND_HC, stats = STAT_LAMBDA, slug = "conditional_convergence_human_capital", digits = digits_conv),
    list(models = models_roc_res, terms = TERM_LABELS_CONVERGENCE_RES, stats = STAT_LAMBDA_ALPHA_BETA_RES, slug = "conditional_convergence_restricted", digits = 4)
  )

  purrr::walk(conv_specs, function(spec) {
    tbl <- build_table(spec$models, spec$terms, spec$stats, digits = spec$digits)
    write_model_table(tbl, tables_dir, table_prefix, spec$slug)
  })

  figure <- build_convergence_plot(samples[["Intermediate"]], growth_years_label)
  ggplot2::ggsave(
    filename = file.path(figures_dir, sprintf("%s.png", figure_prefix)),
    plot = figure,
    width = 8,
    height = 10,
    dpi = 300
  )

  invisible(list(tables = table_prefix, figure = figure_prefix))
}
