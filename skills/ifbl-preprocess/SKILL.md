# Skill: ifbl-preprocess

## When to use
Use this skill when a task asks to validate or troubleshoot IFBL grid assignment during preprocessing.

## Inputs
- Optional `RAW_OCC_PATH` (defaults to `data_raw/gbif/occurrences_raw.csv`)
- Optional `IFBL_GRID_PATH`

## Steps
1. Confirm whether `IFBL_GRID_PATH` is set.
2. If not set, check for `spatial/ifbl_grid.shp` first.
3. If still not found, look for any `*.shp` directly under `spatial/`.
4. Run preprocessing:
   - `Rscript scripts/01_preprocess_redlist_input.R`
   - or with overrides:
     - `RAW_OCC_PATH=... IFBL_GRID_PATH=... Rscript scripts/01_preprocess_redlist_input.R`
5. Verify `app_data/records_analysis.rds` exists.
6. Confirm `grid_source` contains expected values (`ifbl_grid` when join succeeds, otherwise `fallback_10km`).

## Notes
- IFBL assignment is best-effort and intentionally falls back to projected 10x10 km IDs.
- Keep failures actionable: mention exact missing path or unreadable shapefile.
