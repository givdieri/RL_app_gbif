suppressPackageStartupMessages({
  library(dplyr)
})

compute_period_occupancy <- function(records_analysis, species, split_year) {
  sp <- records_analysis %>% filter(species_working == species)

  if (nrow(sp) == 0) {
    return(list(historical = 0, current = 0, historical_grids = character(), current_grids = character()))
  }

  hist <- sp %>% filter(year < split_year) %>% distinct(grid_id)
  curr <- sp %>% filter(year >= split_year) %>% distinct(grid_id)

  list(
    historical = nrow(hist),
    current = nrow(curr),
    historical_grids = hist$grid_id,
    current_grids = curr$grid_id
  )
}

compute_criterion_a <- function(records_analysis, species, settings) {
  split_year <- as.integer(settings$split_year)
  dd_threshold <- as.numeric(settings$dd_threshold_occupied_grids)
  th_cr <- as.numeric(settings$decline_threshold_cr)
  th_en <- as.numeric(settings$decline_threshold_en)
  th_vu <- as.numeric(settings$decline_threshold_vu)
  commonness_fraction <- as.numeric(settings$commonness_fraction)
  re_historical_min <- as.numeric(settings$re_historical_min)

  occ <- compute_period_occupancy(records_analysis, species, split_year)

  decline_pct <- if (occ$historical <= 0) {
    NA_real_
  } else {
    ((occ$current - occ$historical) / occ$historical) * 100
  }

  category <- 'LC/NT (provisional)'
  reason <- 'No substantial decline detected by Criterion A v1.'

  if (occ$historical >= re_historical_min && occ$current == 0) {
    category <- 'RE (provisional)'
    reason <- 'Historical occupancy > 0 and current occupancy = 0.'
  } else if (occ$historical < dd_threshold && occ$current < dd_threshold) {
    category <- 'DD (provisional)'
    reason <- 'Occupied grids are below DD threshold in both periods.'
  } else if (!is.na(decline_pct) && decline_pct <= th_cr) {
    category <- 'CR (provisional)'
    reason <- sprintf('Decline %.1f%% <= CR threshold %.1f%%.', decline_pct, th_cr)
  } else if (!is.na(decline_pct) && decline_pct <= th_en) {
    category <- 'EN (provisional)'
    reason <- sprintf('Decline %.1f%% <= EN threshold %.1f%%.', decline_pct, th_en)
  } else if (!is.na(decline_pct) && decline_pct <= th_vu) {
    category <- 'VU (provisional)'
    reason <- sprintf('Decline %.1f%% <= VU threshold %.1f%%.', decline_pct, th_vu)
  } else if (!is.na(decline_pct) && occ$current >= commonness_fraction * max(occ$historical, 1)) {
    category <- 'noTrend (provisional)'
    reason <- 'Current occupancy remains above commonness fraction threshold.'
  }

  list(
    species = species,
    split_year = split_year,
    historical_occupied_grids = occ$historical,
    current_occupied_grids = occ$current,
    decline_pct = decline_pct,
    provisional_category = category,
    reason = reason,
    note = 'Final category requires expert review.'
  )
}
