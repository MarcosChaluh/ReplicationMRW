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

mrw <- readr::read_csv(file.path(dirs$data_dir, "mrw.csv")) %>%
  dplyr::select(-number) %>%
  dplyr::mutate(
    lngd = log(popgrowth / 100 + 0.05),
    ls = log(i_y / 100),
    ls_lngd = ls - lngd,
    lschool = log(school),
    lsch_ngd = lschool - lngd,
    ly60 = log(rgdpw60),
    ly85 = log(rgdpw85),
    linv = log(i_y),
    growth = ly85 - ly60
  )

run_mrw_reporting(
  mrw = mrw,
  table_prefix = "mrw_table",
  figure_prefix = "mrw_conditional_convergence",
  growth_years_label = "1960 - 1985",
  tables_dir = dirs$tables_dir,
  figures_dir = dirs$figures_dir
)

message("Tables written to ", dirs$tables_dir)
message("Figure written to ", file.path(dirs$figures_dir, "mrw_conditional_convergence.png"))

