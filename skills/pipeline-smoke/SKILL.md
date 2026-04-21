# Skill: pipeline-smoke

## When to use
Use this skill for quick local verification that preprocessing and app-input generation work.

## Steps
1. Run toy preprocess smoke test:
   - `RAW_OCC_PATH=main/toy_data/fungal_occurences_Vlaanderen.csv Rscript scripts/01_preprocess_redlist_input.R`
2. Check generated files:
   - `app_data/records_clean.rds`
   - `app_data/records_analysis.rds`
   - `app_data/species_master.rds`
   - `app_data/settings_defaults.csv`
   - `app_data/excluded_records.csv`
3. Optionally inspect a short summary in R:
   - `Rscript -e "x<-readRDS('app_data/records_analysis.rds'); print(table(x$grid_source, useNA='ifany'))"`

## Expected outcome
- Script exits successfully.
- All files above exist.
- No empty analysis table when toy input contains valid coordinates and dates.
