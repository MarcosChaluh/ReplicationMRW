suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
  library(broom)
  library(tidyr)
  library(ggplot2)
  library(tibble)
})

source("mrw_tables_figures_utils.R")

dirs <- ensure_output_dirs()

ext_file <- file.path(dirs$data_dir, "mrw_with_2023_final.csv")
stopifnot(file.exists(ext_file))

mrw23_raw <- readr::read_csv(ext_file)

mrw <- mrw23_raw %>%
  dplyr::select(country, n, i, o, rgdpw60, rgdpw23, gdpgrowth_23, popgrowth_23, i_y_23, school_23) %>%
  dplyr::mutate(
    ly60 = log(rgdpw60),
    ly85 = log(rgdpw23),
    growth = ly85 - ly60,
    linv = log(i_y_23 / 100),
    lngd = log(popgrowth_23 / 100 + 0.05),
    ls = log(i_y_23 / 100),
    lschool = log(school_23),
    ls_lngd = ls - lngd,
    lsch_ngd = lschool - lngd
  )

run_mrw_reporting(
  mrw = mrw,
  table_prefix = "mrw_23_table",
  figure_prefix = "mrw_23_conditional_convergence",
  growth_years_label = "1960 - 2023",
  tables_dir = dirs$tables_dir,
  figures_dir = dirs$figures_dir
)

message("Tables written to ", dirs$tables_dir)
message("Figure written to ", file.path(dirs$figures_dir, "mrw_23_conditional_convergence.png"))

