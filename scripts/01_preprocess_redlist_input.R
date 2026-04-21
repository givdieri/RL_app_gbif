#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(sf)
})

log_df_structure <- function(df, label, preview_cols = 12) {
  cols <- names(df)
  head_cols <- if (length(cols) > 0) paste(utils::head(cols, preview_cols), collapse = ', ') else '<none>'
  type_pairs <- if (ncol(df) > 0) {
    paste(sprintf('%s:%s', names(df), vapply(df, function(x) class(x)[1], character(1))), collapse = ', ')
  } else {
    '<none>'
  }
  message(sprintf(
    '[df_debug] %s -> rows=%s cols=%s col_names=[%s] col_types=[%s]',
    label, nrow(df), ncol(df), head_cols, type_pairs
  ))
}

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
  log_df_structure(clean, 'normalize_occurrence_fields.output')

  clean
}

derive_analysis_grids <- function(clean_df) {
  if (nrow(clean_df) == 0) stop('derive_analysis_grids: clean_df has no rows.')

  kept <- clean_df %>% filter(!exclusion_flag)
  if (nrow(kept) == 0) stop('derive_analysis_grids: no non-excluded records to analyze.')

  pts <- st_as_sf(kept, coords = c('decimalLongitude', 'decimalLatitude'), crs = 4326, remove = FALSE)

  discover_ifbl_grid_paths <- function() {
    env_path <- Sys.getenv('IFBL_GRID_PATH', unset = '')
    env_paths <- if (nzchar(env_path)) str_split(env_path, ';|,')[[1]] |> str_trim() else character(0)
    env_paths <- env_paths[file.exists(env_paths)]

    discovered <- c(
      file.path('spatial', 'ifbl04x04.shp'),
      file.path('spatial', 'IFBL_kwartierhokken.kml'),
      list.files('spatial', pattern = '\\\\.shp$', full.names = TRUE),
      list.files('spatial', pattern = '\\\\.kml$', full.names = TRUE)
    )
    discovered <- unique(discovered[file.exists(discovered)])
    unique(c(env_paths, discovered))
  }

  extract_ifbl_id <- function(grid_sf, preferred = c('uurhok', 'kwartier')) {
    if (is.na(st_crs(grid_sf))) {
      warning('IFBL grid has no CRS metadata; assuming EPSG:4326')
      st_crs(grid_sf) <- 4326
    }

    nm <- names(grid_sf)
    id_candidates <- c('IFBL', 'ifbl_id', 'ifbl', 'CODE', 'code', 'GRID_ID', 'grid_id', 'Name', 'name')
    id_col <- id_candidates[id_candidates %in% nm][1]
    if (is.na(id_col) || !nzchar(id_col)) return(rep(NA_character_, nrow(kept)))

    pts_ifbl <- st_transform(pts, st_crs(grid_sf))
    joined <- suppressWarnings(st_join(pts_ifbl, grid_sf[, id_col, drop = FALSE], left = TRUE))
    as.character(joined[[id_col]])
  }

  grid_paths <- discover_ifbl_grid_paths()
  if (length(grid_paths) == 0) {
    stop('No IFBL grid file found. Provide spatial/ifbl_grid.shp, spatial/ifbl_grid.kml, or IFBL_GRID_PATH.')
  }

  kept$ifbl_uurhok <- NA_character_
  kept$ifbl_kwartier <- NA_character_

  if (length(grid_paths) > 0) {
    for (p in grid_paths) {
      grid_sf <- tryCatch(st_read(p, quiet = TRUE), error = function(e) NULL)
      if (is.null(grid_sf) || nrow(grid_sf) == 0) next

      id_vals <- extract_ifbl_id(grid_sf)
      if (all(is.na(id_vals) | id_vals == '')) next

      path_lc <- tolower(basename(p))
      target_col <- if (str_detect(path_lc, 'kwartier|1x1|kml')) 'ifbl_kwartier' else 'ifbl_uurhok'
      fill_idx <- is.na(kept[[target_col]]) | kept[[target_col]] == ''
      kept[[target_col]][fill_idx] <- id_vals[fill_idx]
    }
  }

  kept$ifbl_grid_id <- dplyr::coalesce(kept$ifbl_kwartier, kept$ifbl_uurhok)
  kept$grid_source <- dplyr::case_when(
    !is.na(kept$ifbl_kwartier) & kept$ifbl_kwartier != '' ~ 'ifbl_kwartier',
    !is.na(kept$ifbl_uurhok) & kept$ifbl_uurhok != '' ~ 'ifbl_uurhok',
    TRUE ~ NA_character_
  )
  kept$grid_id <- kept$ifbl_grid_id

  missing_ifbl_idx <- is.na(kept$ifbl_grid_id) | kept$ifbl_grid_id == ''
  dropped <- kept[missing_ifbl_idx, , drop = FALSE] %>%
    mutate(
      exclusion_flag = TRUE,
      exclusion_reason = 'no_ifbl_grid_match',
      quality_flag = 'excluded'
    )
  kept <- kept[!missing_ifbl_idx, , drop = FALSE]

  if (nrow(kept) == 0) {
    stop('derive_analysis_grids: no records matched IFBL grid. Check IFBL grid coverage/CRS and input coordinates.')
  }

  log_df_structure(kept, 'derive_analysis_grids.analysis')
  log_df_structure(dropped, 'derive_analysis_grids.excluded_ifbl')
  list(analysis = kept, excluded_ifbl = dropped)
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
  log_df_structure(raw, 'main.raw_input')

  records_clean <- normalize_occurrence_fields(raw)
  print( "showing structure of records_clean")
  str(records_clean)
  analysis_result <- derive_analysis_grids(records_clean)
  records_analysis <- analysis_result$analysis

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

  excluded <- bind_rows(
    records_clean %>% filter(exclusion_flag),
    analysis_result$excluded_ifbl %>% select(any_of(names(records_clean)))
  )
  log_df_structure(records_analysis, 'main.records_analysis')
  log_df_structure(excluded, 'main.excluded_records')

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
