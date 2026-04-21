# AGENTS.md

## Purpose
Repository-level operating guide for agents working on `RL_app_gbif`.

## Preferred workflow
0. If `Rscript` is unavailable, run `bash scripts/02_bootstrap_r_environment.sh`.
1. Use `Rscript scripts/00_fetch_gbif_occurrences.R` to fetch GBIF data.
2. Use `Rscript scripts/01_preprocess_redlist_input.R` to create app inputs.
3. Use `Rscript -e "shiny::runApp('.')"` to launch the app.

## IFBL grid conventions
- Expected folder for locally provided IFBL files: `./spatial`.
- Preferred shapefile name: `ifbl_grid.shp` (with `.dbf/.shx/.prj` siblings).
- You can always override with `IFBL_GRID_PATH=/full/path/to/file.shp`.

## Validation checklist (minimum)
- Run preprocess against toy data:
  - `RAW_OCC_PATH=main/toy_data/fungal_occurences_Vlaanderen.csv Rscript scripts/01_preprocess_redlist_input.R`
- Confirm these files are produced in `app_data/`:
  - `records_clean.rds`
  - `records_analysis.rds`
  - `species_master.rds`
  - `settings_defaults.csv`
  - `excluded_records.csv`

## Skill files
Project-local skills are stored under `./skills/`.
Use them when the task is directly related to that workflow.
