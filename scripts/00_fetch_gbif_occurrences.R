#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(stringdist)
  library(jsonlite)
  library(tibble)
  library(sf)
})

log_msg <- function(...) {
  cat(sprintf('[%s] %s\n', Sys.time(), paste(..., collapse = ' ')))
}

log_df_structure <- function(df, label, preview_cols = 12) {
  cols <- names(df)
  head_cols <- if (length(cols) > 0) paste(utils::head(cols, preview_cols), collapse = ', ') else '<none>'
  type_pairs <- if (ncol(df) > 0) {
    paste(sprintf('%s:%s', names(df), vapply(df, function(x) class(x)[1], character(1))), collapse = ', ')
  } else {
    '<none>'
  }
  log_msg(sprintf(
    '[df_debug] %s -> rows=%s cols=%s col_names=[%s] col_types=[%s]',
    label, nrow(df), ncol(df), head_cols, type_pairs
  ))
}

gbif_get_json <- function(path, params = list()) {
  base <- 'https://api.gbif.org/v1'
  q <- if (length(params) > 0) {
    paste(
      purrr::imap_chr(params, ~ paste0(utils::URLencode(.y, reserved = TRUE), '=', utils::URLencode(as.character(.x), reserved = TRUE))),
      collapse = '&'
    )
  } else {
    ''
  }
  url <- paste0(base, path, if (nzchar(q)) paste0('?', q) else '')
  txt <- tryCatch(readr::read_file(url), error = function(e) NULL)
  if (is.null(txt) || !nzchar(txt)) return(NULL)
  tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
}

name_backbone_api <- function(name) {
  gbif_get_json('/species/match', params = list(name = name, kingdom = 'Fungi', strict = 'false', verbose = 'false'))
}

name_suggest_api <- function(name, limit = 20) {
  res <- gbif_get_json('/species/suggest', params = list(q = name, rank = 'species', limit = as.integer(limit)))
  if (is.null(res)) return(tibble())
  as_tibble(res)
}

occ_search_api <- function(taxonKey, limit = 300, start = 0, country = 'BE', hasCoordinate = TRUE, datasetKey = NULL) {
  params <- list(
    taxonKey = as.integer(taxonKey),
    limit = as.integer(limit),
    offset = as.integer(start),
    country = country,
    hasCoordinate = tolower(as.character(hasCoordinate))
  )
  if (!is.null(datasetKey) && nzchar(datasetKey)) params$datasetKey <- datasetKey
  gbif_get_json(
    '/occurrence/search',
    params = params
  )
}

get_default_geo_cfg <- function() {
  discover_ifbl_grid_paths <- function() {
    env_path <- Sys.getenv('IFBL_GRID_PATH', unset = '')
    env_paths <- if (nzchar(env_path)) str_split(env_path, ';|,')[[1]] |> str_trim() else character(0)
    env_paths <- env_paths[file.exists(env_paths)]

    discovered <- c(
      file.path('spatial', 'ifbl_grid.shp'),
      file.path('spatial', 'ifbl_grid.kml'),
      file.path('legacy_code', 'ifbl04x04.shp'),
      file.path('legacy_code', 'IFBL_kwartierhokken.kml'),
      file.path('data_aux', 'ifbl', 'ifbl_grid.shp'),
      file.path('data_aux', 'ifbl', 'ifbl_grid.kml'),
      list.files('spatial', pattern = '\\\\.shp$', full.names = TRUE),
      list.files('spatial', pattern = '\\\\.kml$', full.names = TRUE)
    )
    discovered <- unique(discovered[file.exists(discovered)])
    unique(c(env_paths, discovered))
  }

  load_filter_polygon <- function() {
    for (p in discover_ifbl_grid_paths()) {
      g <- tryCatch(st_read(p, quiet = TRUE), error = function(e) NULL)
      if (is.null(g) || nrow(g) == 0) next
      if (is.na(st_crs(g))) st_crs(g) <- 4326
      geom <- tryCatch(st_geometry(g), error = function(e) NULL)
      if (is.null(geom)) next
      geom_4326 <- tryCatch(st_transform(geom, 4326), error = function(e) NULL)
      if (is.null(geom_4326)) next
      merged <- tryCatch(st_union(geom_4326), error = function(e) NULL)
      if (is.null(merged)) next
      return(st_as_sfc(merged))
    }
    NULL
  }

  poly_from_ifbl <- load_filter_polygon()
  if (!is.null(poly_from_ifbl)) {
    return(list(
      use_polygon = TRUE,
      polygon = poly_from_ifbl,
      country = 'BE',
      stateProvince = 'Vlaanderen'
    ))
  }

  # Approximate polygon fallback for Flanders, with Belgium fallback.
  flanders_poly <- matrix(c(2.50, 50.68, 5.92, 50.68, 5.92, 51.51, 2.50, 51.51, 2.50, 50.68), ncol = 2, byrow = TRUE)

  list(
    use_polygon = TRUE,
    polygon = st_polygon(list(flanders_poly)) |> st_sfc(crs = 4326),
    country = 'BE',
    stateProvince = 'Vlaanderen'
  )
}

resolve_taxa_gbif <- function(taxa_vec) {
  if (length(taxa_vec) == 0) {
    stop('resolve_taxa_gbif: taxa_vec is empty.')
  }

  map_dfr(taxa_vec, function(taxon_name) {
    bb_result <- tryCatch(
      list(
        ok = TRUE,
        value = name_backbone_api(name = taxon_name)
      ),
      error = function(e) list(ok = FALSE, error = e$message)
    )
    bb <- if (bb_result$ok) bb_result$value else NULL

    if (!is.null(bb) && length(bb) > 0) {
      usage_key <- bb$usageKey %||% NA_integer_
      accepted_usage_key <- bb$acceptedUsageKey %||% usage_key

      return(tibble(
        input_name = taxon_name,
        query_name = taxon_name,
        matched_name = bb$scientificName %||% bb$canonicalName %||% NA_character_,
        accepted_name = bb$acceptedScientificName %||% bb$species %||% bb$canonicalName %||% (bb$scientificName %||% NA_character_),
        rank = bb$rank %||% NA_character_,
        status = bb$status %||% NA_character_,
        usageKey = as.integer(usage_key),
        acceptedUsageKey = as.integer(accepted_usage_key),
        occ_taxonKey = as.integer(accepted_usage_key),
        match_type = 'exact_backbone',
        confidence = as.numeric(bb$confidence %||% NA_real_),
        note = NA_character_
      ))
    }

    sugg_result <- tryCatch(
      list(ok = TRUE, value = name_suggest_api(name = taxon_name, limit = 20)),
      error = function(e) list(ok = FALSE, error = e$message)
    )
    sugg <- if (sugg_result$ok) sugg_result$value else tibble()

    if (nrow(sugg) == 0) {
      return(tibble(
        input_name = taxon_name,
        query_name = taxon_name,
        matched_name = NA_character_,
        accepted_name = NA_character_,
        rank = NA_character_,
        status = NA_character_,
        usageKey = NA_integer_,
        acceptedUsageKey = NA_integer_,
        occ_taxonKey = NA_integer_,
        match_type = 'failed',
        confidence = NA_real_,
        note = if (!sugg_result$ok) {
          paste('GBIF name_suggest error:', sugg_result$error)
        } else if (!bb_result$ok) {
          paste('GBIF name_backbone error:', bb_result$error)
        } else {
          'No GBIF suggestions found'
        }
      ))
    }

    ranked <- sugg %>%
      mutate(
        kingdom = coalesce(kingdom, ''),
        is_fungi = str_to_lower(kingdom) == 'fungi',
        candidate_name = coalesce(canonicalName, scientificName, ''),
        dist = stringdist::stringdist(str_to_lower(taxon_name), str_to_lower(candidate_name), method = 'jw')
      ) %>%
      arrange(desc(is_fungi), dist)
    best <- ranked[1, ]

    if (!isTRUE(best$is_fungi)) {
      return(tibble(
        input_name = taxon_name,
        query_name = taxon_name,
        matched_name = best$candidate_name,
        accepted_name = NA_character_,
        rank = best$rank %||% NA_character_,
        status = best$status %||% NA_character_,
        usageKey = as.integer(best$key),
        acceptedUsageKey = NA_integer_,
        occ_taxonKey = as.integer(best$key),
        match_type = 'failed',
        confidence = NA_real_,
        note = 'Best fuzzy match is not in kingdom Fungi'
      ))
    }

    accepted_usage <- as.integer(best$acceptedKey %||% best$key)

    tibble(
      input_name = taxon_name,
      query_name = taxon_name,
      matched_name = best$candidate_name,
      accepted_name = best$candidate_name,
      rank = best$rank %||% NA_character_,
      status = best$status %||% NA_character_,
      usageKey = as.integer(best$key),
      acceptedUsageKey = accepted_usage,
      occ_taxonKey = accepted_usage,
      match_type = 'fuzzy_suggest',
      confidence = NA_real_,
      note = paste0('String distance jw=', round(best$dist, 4))
    )
  })
}

fetch_occurrences_gbif <- function(taxon_keys, geo_cfg, page_size = 300, max_pages = 10, dataset_keys = character(0)) {
  if (length(taxon_keys) == 0) {
    stop('fetch_occurrences_gbif: no taxon keys provided.')
  }

  out <- list()

  for (key in taxon_keys) {
    start <- 0
    page <- 1
    repeat {
      if (page > max_pages) {
        log_msg('Reached max_pages for taxon key', key)
        break
      }

      res <- tryCatch(
        occ_search_api(taxonKey = key, limit = page_size, start = start, hasCoordinate = TRUE, country = geo_cfg$country),
        error = function(e) e
      )

      if (inherits(res, 'error')) {
        log_msg('Error while fetching key', key, ':', res$message)
        break
      }

      dat <- as_tibble(res$results %||% tibble())
      if (nrow(dat) == 0) break

      dat <- dat %>% mutate(requested_taxonKey = key, page = page)
      out[[length(out) + 1]] <- dat

      end_of_records <- isTRUE(res$endOfRecords) || nrow(dat) < page_size
      if (end_of_records) break

      start <- start + page_size
      page <- page + 1
      Sys.sleep(0.2)
    }
  }

  if (length(dataset_keys) > 0) {
    for (ds in dataset_keys) {
      for (key in taxon_keys) {
        res_ds <- tryCatch(
          occ_search_api(
            taxonKey = key,
            limit = min(page_size, 200),
            start = 0,
            hasCoordinate = TRUE,
            country = geo_cfg$country,
            datasetKey = ds
          ),
          error = function(e) e
        )
        if (inherits(res_ds, 'error')) next
        dat_ds <- as_tibble(res_ds$results %||% tibble())
        if (nrow(dat_ds) == 0) next
        dat_ds <- dat_ds %>% mutate(requested_taxonKey = key, page = 1, supplemental_dataset = ds)
        out[[length(out) + 1]] <- dat_ds
      }
    }
  }

  if (length(out) == 0) {
    return(tibble())
  }

  bind_rows(out) %>% distinct(key, .keep_all = TRUE)
}

apply_flanders_filter <- function(df, geo_cfg) {
  if (nrow(df) == 0) {
    return(list(kept = df, excluded = tibble()))
  }
  log_df_structure(df, 'apply_flanders_filter.input')

  req_cols <- c('key', 'species', 'decimalLongitude', 'decimalLatitude', 'datasetName', 'eventDate')
  missing <- setdiff(req_cols, names(df))
  for (m in missing) df[[m]] <- NA

  missing_coord_idx <- is.na(df$decimalLongitude) | is.na(df$decimalLatitude)
  missing_coord <- df[missing_coord_idx, , drop = FALSE] %>%
    mutate(exclusion_reason = 'missing_coordinates')
  df_coords <- df[!missing_coord_idx, , drop = FALSE]

  if (nrow(df_coords) == 0) {
    return(list(kept = df_coords, excluded = missing_coord))
  }

  normalize_state <- function(x) {
    x |>
      as.character() |>
      stringr::str_to_lower() |>
      stringr::str_replace_all('[^a-z]+', '')
  }
  require_stateprovince <- tolower(Sys.getenv('GBIF_REQUIRE_VLAANDEREN_STATEPROVINCE', unset = 'false')) == 'true'
  state_norm <- normalize_state(df_coords$stateProvince)
  state_ok_idx <- state_norm %in% c('vlaanderen', 'flanders')

  pts <- st_as_sf(df_coords, coords = c('decimalLongitude', 'decimalLatitude'), crs = 4326, remove = FALSE)
  spatial_idx <- if (isTRUE(geo_cfg$use_polygon)) {
    lengths(st_within(pts, geo_cfg$polygon)) > 0
  } else {
    warning('Polygon filter disabled. Falling back to Belgium-only filter.')
    rep(TRUE, nrow(df_coords))
  }

  kept_idx <- if (require_stateprovince) spatial_idx & state_ok_idx else spatial_idx
  kept <- df_coords[kept_idx, , drop = FALSE]

  excluded_geo <- df_coords[!spatial_idx, , drop = FALSE] %>%
    mutate(exclusion_reason = 'outside_flanders_polygon')
  excluded_state <- if (require_stateprovince) {
    df_coords[spatial_idx & !state_ok_idx, , drop = FALSE] %>%
      mutate(exclusion_reason = 'outside_vlaanderen_stateProvince')
  } else {
    tibble()
  }
  excluded <- bind_rows(excluded_geo, excluded_state, missing_coord)
  log_df_structure(kept, 'apply_flanders_filter.kept')
  log_df_structure(excluded, 'apply_flanders_filter.excluded')

  list(kept = kept, excluded = excluded)
}

write_fetch_outputs <- function(raw_occ_df, taxon_match_log_df, exclusion_log_df, metadata, out_dir = 'data_raw/gbif') {
  flatten_non_atomic_cols <- function(df) {
    out <- df
    for (nm in names(out)) {
      col <- out[[nm]]
      if (is.list(col)) {
        out[[nm]] <- purrr::map_chr(col, function(val) {
          if (is.null(val) || (length(val) == 1 && is.na(val))) return(NA_character_)
          if (length(val) == 0) return(NA_character_)
          if (length(val) == 1 && is.atomic(val)) return(as.character(val))
          jsonlite::toJSON(val, auto_unbox = TRUE)
        })
      } else if (is.matrix(col) || is.data.frame(col)) {
        out[[nm]] <- apply(as.matrix(col), 1, function(val) jsonlite::toJSON(as.list(val), auto_unbox = TRUE))
      }
    }
    out
  }

  raw_occ_df <- flatten_non_atomic_cols(raw_occ_df)
  exclusion_log_df <- flatten_non_atomic_cols(exclusion_log_df)

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  write_csv(raw_occ_df, file.path(out_dir, 'occurrences_raw.csv'))
  write_csv(taxon_match_log_df, file.path(out_dir, 'taxon_match_log.csv'))
  write_csv(exclusion_log_df, file.path(out_dir, 'exclusion_log.csv'))
  write_json(metadata, file.path(out_dir, 'download_metadata.json'), pretty = TRUE, auto_unbox = TRUE)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

load_gbif_example_occurrences <- function() {
  local_example <- 'main/toy_data/fungal_occurences_Vlaanderen.csv'
  if (!file.exists(local_example)) {
    stop('Fallback example file not found: main/toy_data/fungal_occurences_Vlaanderen.csv')
  }

  first_line <- readLines(local_example, n = 1, warn = FALSE)
  delim <- if (grepl(';', first_line, fixed = TRUE)) ';' else ','
  occ <- readr::read_delim(local_example, delim = delim, show_col_types = FALSE, locale = readr::locale(decimal_mark = ','))

  occ %>%
    mutate(
      key = as.character(key),
      datasetName = as.character(datasetName)
    ) %>%
    filter(str_to_lower(kingdom) == 'fungi')
}

main <- function() {
  default_taxa <- c(
    'Amanita muscaria',
    'Amanita citrina',
    'Amanita excelsa',
    'Amanita citrina var. citrina',
    'Amanita battarea',
    'Amanita eliae',
    'Pluteus cervinus',
    'Pluteus hongoi',
    'Pluteus thomsonii',
    'Psathyrella conopilus',
    'Psathyrella piluliformis',
    'Xylaria hypoxylon',
    'Camarops polysperma'
  )

  taxa_env <- Sys.getenv('GBIF_TAXA_CSV', unset = '')
  taxa <- if (nzchar(taxa_env)) str_split(taxa_env, ',')[[1]] |> str_trim() |> unique() else default_taxa
  supplemental_dataset_env <- Sys.getenv(
    'GBIF_SUPPLEMENTAL_DATASET_KEYS_CSV',
    unset = '039f9753-1e6a-4ffc-83f9-daac3686d165,d6b42d28-ec58-4aba-9dab-332afda295b1'
  )
  supplemental_dataset_keys <- if (nzchar(supplemental_dataset_env)) {
    str_split(supplemental_dataset_env, ',')[[1]] |> str_trim() |> unique()
  } else {
    character(0)
  }

  if (length(taxa) == 0) stop('No taxa provided. Set GBIF_TAXA_CSV or edit default_taxa in script.')

  geo_cfg <- get_default_geo_cfg()

  log_msg('Resolving taxa...')
  taxon_log <- resolve_taxa_gbif(taxa)

  matched <- taxon_log %>% filter(match_type != 'failed', !is.na(occ_taxonKey))
  allow_example_fallback <- tolower(Sys.getenv('GBIF_ALLOW_EXAMPLE_FALLBACK', unset = 'false')) == 'true'
  if (nrow(matched) == 0) {
    if (!allow_example_fallback) {
      dir.create('data_raw/gbif', recursive = TRUE, showWarnings = FALSE)
      write_csv(taxon_log, 'data_raw/gbif/taxon_match_log.csv')
      fail_reasons <- unique(na.omit(taxon_log$note))
      stop(sprintf(
        'No taxa could be resolved to GBIF usage keys. First logged reason: %s',
        ifelse(length(fail_reasons) > 0, fail_reasons[[1]], 'no reason captured')
      ))
    }
    warning('GBIF API unavailable during taxon resolution. Using local toy fallback file.')
    raw <- load_gbif_example_occurrences()
    geo_cfg$use_polygon <- FALSE
    taxon_log <- bind_rows(
      taxon_log,
      tibble(
        input_name = '__fallback__',
        query_name = '__fallback__',
        matched_name = 'local toy fallback',
        accepted_name = 'local toy fallback',
        rank = NA_character_,
        status = NA_character_,
        usageKey = NA_integer_,
        acceptedUsageKey = NA_integer_,
        occ_taxonKey = NA_integer_,
        match_type = 'fallback_local_toy',
        confidence = NA_real_,
        note = 'API unavailable; fallback to local toy example file'
      )
    )
  } else {
    log_msg('Fetching GBIF occurrences...')
    raw <- fetch_occurrences_gbif(
      taxon_keys = matched$occ_taxonKey,
      geo_cfg = geo_cfg,
      page_size = as.integer(Sys.getenv('GBIF_PAGE_SIZE', unset = 100)),
      max_pages = as.integer(Sys.getenv('GBIF_MAX_PAGES', unset = 30)),
      dataset_keys = supplemental_dataset_keys
    )
  }

  if (nrow(raw) == 0) {
    stop('No occurrence records returned from GBIF. Check taxa/geography settings.')
  }
  log_df_structure(raw, 'fetch_occurrences.raw_before_filter')

  log_msg('Applying Flanders filter...')
  filtered <- apply_flanders_filter(raw, geo_cfg)
  log_df_structure(filtered$kept, 'fetch_occurrences.filtered_kept')
  log_df_structure(filtered$excluded, 'fetch_occurrences.filtered_excluded')

  metadata <- list(
    generated_at_utc = format(Sys.time(), tz = 'UTC', usetz = TRUE),
    source = 'GBIF via /v1/occurrence/search HTTP API',
    query_country = geo_cfg$country,
    filter_mode = if (isTRUE(geo_cfg$use_polygon)) 'polygon_flanders' else 'country_belgium_fallback',
    taxa_input = taxa,
    taxa_resolved_n = nrow(matched),
    occurrences_raw_n = nrow(raw),
    occurrences_kept_n = nrow(filtered$kept),
    occurrences_excluded_n = nrow(filtered$excluded)
  )

  keep_cols <- c(
    'key',
    'species',
    'scientificName',
    'acceptedScientificName',
    'taxonRank',
    'kingdom',
    'countryCode',
    'stateProvince',
    'decimalLatitude',
    'decimalLongitude',
    'eventDate',
    'year',
    'basisOfRecord',
    'institutionCode',
    'collectionCode',
    'datasetName',
    'datasetKey',
    'occurrenceID',
    'requested_taxonKey',
    'page'
  )
  filtered$kept <- filtered$kept[, intersect(keep_cols, names(filtered$kept)), drop = FALSE]
  filtered$excluded <- filtered$excluded[, intersect(c(keep_cols, 'exclusion_reason'), names(filtered$excluded)), drop = FALSE]

  write_fetch_outputs(
    raw_occ_df = filtered$kept,
    taxon_match_log_df = taxon_log,
    exclusion_log_df = filtered$excluded,
    metadata = metadata
  )

  if (!isTRUE(geo_cfg$use_polygon)) {
    warning('Using Belgium fallback geography filter. This should be treated as lower confidence.')
  }

  log_msg('Fetch phase complete.')
}

if (identical(environment(), globalenv())) {
  main()
}
