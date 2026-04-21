#!/usr/bin/env bash
set -euo pipefail

if ! command -v apt-get >/dev/null 2>&1; then
  echo "apt-get not found; cannot auto-install R runtime in this environment." >&2
  exit 1
fi

export DEBIAN_FRONTEND=noninteractive

echo "[bootstrap] Installing Rscript runtime (r-base-core)..."
apt-get update
apt-get install -y r-base-core

echo "[bootstrap] Installing apt-packaged R dependencies used by preprocess/app..."
apt-get install -y \
  r-cran-dplyr \
  r-cran-readr \
  r-cran-stringr \
  r-cran-tibble \
  r-cran-sf \
  r-cran-shiny \
  r-cran-dt \
  r-cran-purrr \
  r-cran-stringdist \
  r-cran-jsonlite

if command -v Rscript >/dev/null 2>&1; then
  echo "[bootstrap] Rscript ready: $(Rscript --version 2>&1)"
else
  echo "[bootstrap] ERROR: Rscript still unavailable after bootstrap." >&2
  exit 1
fi
