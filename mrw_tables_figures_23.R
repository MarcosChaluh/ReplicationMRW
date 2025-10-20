
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
  library(broom)
  library(tidyr)
  library(ggplot2)
})

'%||%' <- function(x, y) {
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
  tidy_models <- map(models, tidy)
  
  coef_rows <- map_dfr(seq_along(term_map), function(i) {
    term_name <- names(term_map)[i]
    label <- unname(term_map[i])
    values <- map_chr(model_names, function(model_name) {
      tm <- tidy_models[[model_name]]
      row <- tm[tm$term == term_name, , drop = FALSE]
      if (nrow(row) == 0) {
        return("")
      }
      format_with_stars(row$estimate, row$p.value, digits = digits)
    })
    tibble(term = label, !!!set_names(values, model_names))
  })
  
  if (length(stat_specs) > 0) {
    stat_rows <- map_dfr(names(stat_specs), function(label) {
      spec <- stat_specs[[label]]
      fn <- spec$fn
      digits_spec <- spec$digits %||% digits
      fmt <- spec$fmt
      values <- map_chr(models, function(mod) {
        value <- fn(mod)
        if (is.na(value)) {
          return("")
        }
        if (!is.null(fmt)) {
          return(sprintf(fmt, value))
        }
        formatC(value, format = "f", digits = digits_spec)
      })
      tibble(term = label, !!!set_names(values, model_names))
    })
    bind_rows(coef_rows, stat_rows)
  } else {
    coef_rows
  }
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
  beta <- coef(mod)[["ls"]]
  if (is.null(beta) || is.na(beta)) {
    return(NA_real_)
  }
  beta / (1 + beta)
}

implied_alpha_lslngd <- function(mod) {
  beta <- coef(mod)[["ls_lngd"]]
  if (is.null(beta) || is.na(beta)) {
    return(NA_real_)
  }
  beta / (1 + beta)
}

implied_alpha_hc <- function(mod) {
  beta_s <- coef(mod)[["ls"]]
  beta_h <- coef(mod)[["lschool"]]
  if (any(is.null(c(beta_s, beta_h))) || any(is.na(c(beta_s, beta_h)))) {
    return(NA_real_)
  }
  denom <- 1 + beta_s + beta_h
  beta_s / denom
}

implied_beta_hc <- function(mod) {
  beta_s <- coef(mod)[["ls"]]
  beta_h <- coef(mod)[["lschool"]]
  if (any(is.null(c(beta_s, beta_h))) || any(is.na(c(beta_s, beta_h)))) {
    return(NA_real_)
  }
  denom <- 1 + beta_s + beta_h
  beta_h / denom
}

implied_alpha_hc_res <- function(mod) {
  beta_s <- coef(mod)[["ls_lngd"]]
  beta_h <- coef(mod)[["lsch_ngd"]]
  if (any(is.null(c(beta_s, beta_h))) || any(is.na(c(beta_s, beta_h)))) {
    return(NA_real_)
  }
  denom <- 1 + beta_s + beta_h
  beta_s / denom
}

implied_beta_hc_res <- function(mod) {
  beta_s <- coef(mod)[["ls_lngd"]]
  beta_h <- coef(mod)[["lsch_ngd"]]
  if (any(is.null(c(beta_s, beta_h))) || any(is.na(c(beta_s, beta_h)))) {
    return(NA_real_)
  }
  denom <- 1 + beta_s + beta_h
  beta_h / denom
}

implied_lambda <- function(mod) {
  beta <- coef(mod)[["ly60"]]
  if (is.null(beta) || is.na(beta)) {
    return(NA_real_)
  }
  -log(beta + 1) / 25
}

data_dir <- file.path("data")
results_dir <- file.path("results")
tables_dir <- file.path(results_dir, "tables")
figures_dir <- file.path(results_dir, "figures")

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

ext_file <- "data/mrw_with_2023_final.csv"
stopifnot(file.exists(ext_file))

mrw23_raw <- readr::read_csv(ext_file)

# Keep only what we need; join by country
mrw23 <- mrw23_raw %>%
  dplyr::select(country, n, i, o, rgdpw60, rgdpw23, gdpgrowth_23, popgrowth_23, i_y_23, school_23)

# Standardize the 1960–2023 window to your existing variable names
# so that all helper functions (implied alpha/beta) still work.

mrw <- mrw23 %>%
  dplyr::mutate(
    # use 1960 as "initial" and 2023 as "terminal"
    ly60 = log(rgdpw60),
    ly85 = log(rgdpw23),
    growth = ly85 - ly60,
    # constructs for s, n+g+δ, human capital
    linv       = log(i_y_23/100),
    lngd     = log(popgrowth_23/100 + 0.05),  # keep MRW’s g+δ = 5%
    ls = log(i_y_23 / 100),
    lschool  = log(school_23),
    ls_lngd  = ls - lngd,
    lsch_ngd = lschool - lngd
  )

data_reg <- filter(mrw, n == 1)
data_d <- filter(data_reg, i == 1)
data_oecd <- filter(data_reg, o == 1)

samples <- list(
  'Non-Oil' = data_reg,
  'Intermediate' = data_d,
  'OECD' = data_oecd
)

models_unres <- map(samples, ~ lm(ly85 ~ ls + lngd, data = .x))
models_res <- map(samples, ~ lm(ly85 ~ ls_lngd, data = .x))
models_hc_unres <- map(samples, ~ lm(ly85 ~ ls + lngd + lschool, data = .x))
models_hc_res <- map(samples, ~ lm(ly85 ~ ls_lngd + lsch_ngd, data = .x))

stat_base <- list(
  'R^2' = list(fn = calc_adj_r2, digits = 2),
  'N' = list(fn = calc_n, fmt = "%.0f"),
  's.e.e.' = list(fn = calc_sigma, digits = 2)
)

stat_alpha <- c(
  stat_base,
  list('Implied alpha' = list(fn = implied_alpha_ls, digits = 2))
)

stat_alpha_res <- c(
  stat_base,
  list('Implied alpha' = list(fn = implied_alpha_lslngd, digits = 2))
)

stat_alpha_beta <- c(
  stat_base,
  list(
    'Implied alpha' = list(fn = implied_alpha_hc, digits = 2),
    'Implied beta' = list(fn = implied_beta_hc, digits = 2)
  )
)

stat_alpha_beta_res <- c(
  stat_base,
  list(
    'Implied alpha' = list(fn = implied_alpha_hc_res, digits = 2),
    'Implied beta' = list(fn = implied_beta_hc_res, digits = 2)
  )
)

stat_lambda_alpha_beta_res <- c(
  stat_base,
  list(
    'Implied lambda' = list(fn = implied_lambda, digits = 5),
    'Implied alpha' = list(fn = implied_alpha_hc_res, digits = 2),
    'Implied beta' = list(fn = implied_beta_hc_res, digits = 2)
  )
)

term_labels_basic <- c(
  '(Intercept)' = "Constant",
  ls = "log(s)",
  lngd = "log(n+g+d)"
)

term_labels_restricted <- c(
  '(Intercept)' = "Constant",
  ls_lngd = "log(s) - log(n+g+d)"
)

term_labels_hc <- c(
  '(Intercept)' = "Constant",
  ls = "log(s)",
  lngd = "log(n+g+d)",
  lschool = "log(s_h)"
)

term_labels_hc_res <- c(
  '(Intercept)' = "Constant",
  ls = "log(s)",
  ls_lngd = "log(s) - log(n+g+d)",
  lsch_ngd = "log(s_h) - log(n+g+d)"
)

table_unres <- build_table(models_unres, term_labels_basic, stat_alpha, digits = 2)
write_csv(table_unres, file.path(tables_dir, "mrw_23_table_unrestricted.csv"))

table_res <- build_table(models_res, term_labels_restricted, stat_alpha_res, digits = 2)
write_csv(table_res, file.path(tables_dir, "mrw_23_table_restricted.csv"))

term_labels_hc_res_table <- c(
  '(Intercept)' = "Constant",
  ls_lngd = "log(s) - log(n+g+d)",
  lsch_ngd = "log(s_h) - log(n+g+d)"
)

table_hc_unres <- build_table(models_hc_unres, term_labels_hc, stat_alpha_beta, digits = 2)
write_csv(table_hc_unres, file.path(tables_dir, "mrw_23_table_human_capital_unrestricted.csv"))

table_hc_res <- build_table(models_hc_res, term_labels_hc_res_table, stat_alpha_beta_res, digits = 2)
write_csv(table_hc_res, file.path(tables_dir, "mrw_23_table_human_capital_restricted.csv"))

models_roc <- map(samples, ~ lm(growth ~ ly60, data = .x))
models_roc_cond <- map(samples, ~ lm(growth ~ ly60 + ls + lngd, data = .x))
models_roc_cond_hc <- map(samples, ~ lm(growth ~ ly60 + ls + lngd + lschool, data = .x))
models_roc_res <- map(samples, ~ lm(growth ~ ly60 + ls_lngd + lsch_ngd, data = .x))

stat_lambda <- c(
  stat_base,
  list('Implied lambda' = list(fn = implied_lambda, digits = 5))
)

term_labels_convergence <- c(
  '(Intercept)' = "Constant",
  ly60 = "log(y_0)"
)

table_roc <- build_table(models_roc, term_labels_convergence, stat_lambda, digits = 5)
write_csv(table_roc, file.path(tables_dir, "mrw_23_table_unconditional_convergence.csv"))

term_labels_convergence_cond <- c(
  '(Intercept)' = "Constant",
  ly60 = "log(y_0)",
  ls = "log(s)",
  lngd = "log(n+g+d)"
)

table_roc_cond <- build_table(models_roc_cond, term_labels_convergence_cond, stat_lambda, digits = 5)
write_csv(table_roc_cond, file.path(tables_dir, "mrw_23_table_conditional_convergence.csv"))

term_labels_convergence_cond_hc <- c(
  '(Intercept)' = "Constant",
  ly60 = "log(y_0)",
  ls = "log(s)",
  lngd = "log(n+g+d)",
  lschool = "log(s_h)"
)

table_roc_cond_hc <- build_table(models_roc_cond_hc, term_labels_convergence_cond_hc, stat_lambda, digits = 5)
write_csv(table_roc_cond_hc, file.path(tables_dir, "mrw_23_table_conditional_convergence_human_capital.csv"))

term_labels_convergence_res <- c(
  '(Intercept)' = "Constant",
  ly60 = "log(y_0)",
  ls_lngd = "log(s) - log(n+g+d)",
  lsch_ngd = "log(s_h) - log(n+g+d)"
)

table_roc_res <- build_table(models_roc_res, term_labels_convergence_res, stat_lambda_alpha_beta_res, digits = 4)
write_csv(table_roc_res, file.path(tables_dir, "mrw_23_table_conditional_convergence_restricted.csv"))

mean_income <- mean(data_d$ly60, na.rm = TRUE)
mean_growth <- mean(data_d$growth, na.rm = TRUE)

growth_rate <- (data_d$growth) * 100 / 25

resid_income_sp <- resid(lm(ly60 ~ lngd + linv, data = data_d))
resid_growth_sp <- resid(lm(growth ~ lngd + linv, data = data_d))

resid_income_hc <- resid(lm(ly60 ~ lngd + linv + lschool, data = data_d))
resid_growth_hc <- resid(lm(growth ~ lngd + linv + lschool, data = data_d))

plot_data <- bind_rows(
  tibble(
    panel = "Unconditional",
    log_income = data_d$ly60,
    growth_rate = growth_rate
  ),
  tibble(
    panel = "Conditional on saving and population growth",
    log_income = resid_income_sp + mean_income,
    growth_rate = (resid_growth_sp + mean_growth) * 100 / 25
  ),
  tibble(
    panel = "Conditional on human capital, saving and population growth",
    log_income = resid_income_hc + mean_income,
    growth_rate = (resid_growth_hc + mean_growth) * 100 / 25
  )
)

convergence_plot <- ggplot(plot_data, aes(x = log_income, y = growth_rate)) +
  geom_point(color = "#1b9e77", alpha = 0.8) +
  facet_wrap(~panel, ncol = 1) +
  labs(
    x = "Log GDP Per Worker in 1960",
    y = "Growth Rate: 1960 - 2023",
    title = "Conditional Convergence Across Samples"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

ggsave(file.path(figures_dir, "mrw_23_conditional_convergence.png"), convergence_plot,
       width = 8, height = 10, dpi = 300)

print("Tables written to results/tables")
print("Figure written to results/figures/mrw_23_conditional_convergence.png")

