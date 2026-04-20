#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(sf)
})

normalize_occurrence_fields <- function(raw_df) {
  if (nrow(raw_df) == 0) stop('normalize_occurrence_fields: raw_df has no rows.')

  required <- c('key', 'species', 'eventDate', 'decimalLongitude', 'decimalLatitude', 'datasetName')
  missing <- setdiff(required, names(raw_df))
  if (length(missing) > 0) {
    stop(sprintf('normalize_occurrence_fields: missing required columns: %s', paste(missing, collapse = ', ')))
  }

  species_col <- if ('species' %in% names(raw_df)) as.character(raw_df$species) else rep(NA_character_, nrow(raw_df))
  accepted_col <- if ('accepted_name_resolved' %in% names(raw_df)) as.character(raw_df$accepted_name_resolved) else rep(NA_character_, nrow(raw_df))
  scientific_col <- if ('scientificName' %in% names(raw_df)) as.character(raw_df$scientificName) else rep(NA_character_, nrow(raw_df))

  clean <- raw_df %>%
    transmute(
      record_id = as.character(key),
      species_working = coalesce(species_col, accepted_col, scientific_col, 'Unknown species'),
      observation_date = suppressWarnings(as.Date(eventDate)),
      year = as.integer(format(observation_date, '%Y')),
      decimalLongitude = suppressWarnings(as.numeric(str_replace_all(as.character(decimalLongitude), ',', '.'))),
      decimalLatitude = suppressWarnings(as.numeric(str_replace_all(as.character(decimalLatitude), ',', '.'))),
      source_dataset = coalesce(datasetName, 'GBIF unknown dataset'),
      exclusion_flag = is.na(decimalLongitude) | is.na(decimalLatitude) | is.na(year),
      exclusion_reason = case_when(
        is.na(year) ~ 'missing_or_invalid_date',
        is.na(decimalLongitude) | is.na(decimalLatitude) ~ 'missing_coordinates',
        TRUE ~ NA_character_
      ),
      quality_flag = if_else(exclusion_flag, 'excluded', 'ok')
    )

  clean
}

derive_analysis_grids <- function(clean_df) {
  if (nrow(clean_df) == 0) stop('derive_analysis_grids: clean_df has no rows.')

  kept <- clean_df %>% filter(!exclusion_flag)
  if (nrow(kept) == 0) stop('derive_analysis_grids: no non-excluded records to analyze.')

  pts <- st_as_sf(kept, coords = c('decimalLongitude', 'decimalLatitude'), crs = 4326, remove = FALSE)
  merc <- st_transform(pts, 3857)

  coords <- st_coordinates(merc)
  grid_size_m <- 10000

  kept$grid_id <- paste0(
    floor(coords[, 1] / grid_size_m), '_',
    floor(coords[, 2] / grid_size_m)
  )

  kept
}

write_app_data_outputs <- function(records_clean, records_analysis, species_master, settings_defaults, excluded_records, out_dir = 'app_data') {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  saveRDS(records_clean, file.path(out_dir, 'records_clean.rds'))
  saveRDS(records_analysis, file.path(out_dir, 'records_analysis.rds'))
  saveRDS(species_master, file.path(out_dir, 'species_master.rds'))
  write_csv(settings_defaults, file.path(out_dir, 'settings_defaults.csv'))
  write_csv(excluded_records, file.path(out_dir, 'excluded_records.csv'))
}

main <- function() {
  raw_path <- Sys.getenv('RAW_OCC_PATH', unset = 'data_raw/gbif/occurrences_raw.csv')
  if (!file.exists(raw_path)) {
    stop(sprintf('Missing critical input file: %s. Run scripts/00_fetch_gbif_occurrences.R first.', raw_path))
  }

  first_line <- readLines(raw_path, n = 1, warn = FALSE)
  delim <- if (grepl(';', first_line, fixed = TRUE)) ';' else ','
  raw <- read_delim(raw_path, delim = delim, show_col_types = FALSE, locale = locale(decimal_mark = ','))

  records_clean <- normalize_occurrence_fields(raw)
  records_analysis <- derive_analysis_grids(records_clean)

  species_master <- records_clean %>%
    distinct(species_working) %>%
    arrange(species_working)

  settings_defaults <- tibble(
    setting = c(
      'split_year',
      'dd_threshold_occupied_grids',
      'decline_threshold_cr',
      'decline_threshold_en',
      'decline_threshold_vu',
      'commonness_fraction',
      're_historical_min'
    ),
    value = c('2000', '5', '-80', '-50', '-30', '0.75', '1'),
    description = c(
      'Year splitting historical/current periods',
      'DD if occupied grids below this threshold in both periods',
      'CR provisional if percent decline <= this value',
      'EN provisional if percent decline <= this value',
      'VU provisional if percent decline <= this value',
      'No-trend commonness fraction',
      'Minimum historical occupancy to trigger RE check'
    )
  )

  excluded <- records_clean %>% filter(exclusion_flag)

  write_app_data_outputs(
    records_clean = records_clean,
    records_analysis = records_analysis,
    species_master = species_master,
    settings_defaults = settings_defaults,
    excluded_records = excluded
  )

  message('Preprocess phase complete.')
}

if (identical(environment(), globalenv())) {
  main()
}
