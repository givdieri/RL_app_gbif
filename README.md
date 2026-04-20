# RL_app_gbif

Reproducible v1 pipeline for fungal Red List support in Flanders using **GBIF as the only raw data origin**.

## Scope

- Implemented: Criterion A provisional workflow (expert-reviewed final category).
- Postponed: Criterion B.
- Data source: GBIF only, via `rgbif`.

## Required R packages

`rgbif, dplyr, purrr, readr, stringr, stringdist, jsonlite, tibble, sf, shiny, DT`

## Project structure

- `scripts/00_fetch_gbif_occurrences.R`: taxon resolution + GBIF occurrence fetch + logs.
- `scripts/01_preprocess_redlist_input.R`: normalization and app data outputs.
- `app.R` + `R/*.R`: Shiny app and modules.
- Generated outputs are written under `data_raw/` and `app_data/` (excluded from git).

## Setup

1. Install R and required packages.
2. From repository root, run fetch and preprocess scripts.

## Run pipeline

### 1) Fetch GBIF occurrences

If GBIF API access is blocked (e.g. proxy restrictions), you can enable a non-production smoke-test fallback to a bundled GBIF example archive from `rgbif` with `GBIF_ALLOW_EXAMPLE_FALLBACK=true`.

You can override the built-in species list via `GBIF_TAXA_CSV` (comma-separated scientific names), e.g.:

```bash
GBIF_TAXA_CSV="Amanita muscaria,Amanita citrina" Rscript scripts/00_fetch_gbif_occurrences.R
```

```bash
Rscript scripts/00_fetch_gbif_occurrences.R
```

Outputs:

- `data_raw/gbif/occurrences_raw.csv`
- `data_raw/gbif/taxon_match_log.csv`
- `data_raw/gbif/exclusion_log.csv`
- `data_raw/gbif/download_metadata.json`

### 2) Preprocess for app

```bash
Rscript scripts/01_preprocess_redlist_input.R
```

Outputs:

- `app_data/records_clean.rds`
- `app_data/records_analysis.rds`
- `app_data/species_master.rds`
- `app_data/settings_defaults.csv`
- `app_data/excluded_records.csv`

### 3) Run app

```bash
Rscript -e "shiny::runApp('.')"
```

## Criterion A logic (v1 provisional)

All thresholds are user-configurable in the app settings tab.

Defaults:

- split year: `2000`
- DD threshold: occupied grids `< 5` in both periods
- decline thresholds:
  - CR: `<= -80%`
  - EN: `<= -50%`
  - VU: `<= -30%`
- RE rule: historical `> 0` and current `= 0`
- noTrend commonness fraction: `0.75`

## Known limitations

- Flanders boundary uses a simple polygon approximation by default in v1.
- Grid derivation currently uses projected 10x10 km cell indexing; IFBL-native mapping is not yet implemented.
- GBIF pagination uses `occ_search`; for very large production runs, a formal GBIF download workflow can be added later.
- Final categories are expert-reviewed; app output is provisional support.
- GBIF API access requires outbound network permission to `api.gbif.org`; restrictive proxies can block fetch calls.

## Next steps

- Add Criterion B implementation.
- Improve spatial mapping with stronger IFBL-compatible grid workflows and validated Flanders boundary resources.
