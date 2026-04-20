suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

load_app_data <- function(app_data_dir = 'app_data') {
  req_files <- c(
    'records_clean.rds',
    'records_analysis.rds',
    'species_master.rds',
    'settings_defaults.csv',
    'excluded_records.csv'
  )

  missing <- req_files[!file.exists(file.path(app_data_dir, req_files))]
  if (length(missing) > 0) {
    stop(sprintf('Missing app data files: %s', paste(missing, collapse = ', ')))
  }

  list(
    records_clean = readRDS(file.path(app_data_dir, 'records_clean.rds')),
    records_analysis = readRDS(file.path(app_data_dir, 'records_analysis.rds')),
    species_master = readRDS(file.path(app_data_dir, 'species_master.rds')),
    settings_defaults = read_csv(file.path(app_data_dir, 'settings_defaults.csv'), show_col_types = FALSE),
    excluded_records = read_csv(file.path(app_data_dir, 'excluded_records.csv'), show_col_types = FALSE)
  )
}
