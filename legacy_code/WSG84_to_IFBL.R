# (update path as needed)
# ## all information about uurhok and kwartierhok can be found on https://www.natuurpunt.be/werkgroepen/fon-plantenwerkgroep-0/werking/ifbl-kartering

# Load necessary libraries
library(sf)
library(readxl)
library(dplyr)

# Read in the Excel file
df <- read_csv("data_raw/gbif/occurrences_raw.csv")
# Convert to sf object using lon and lat columns
# Duplicate lon and lat columns before converting to sf object
df <- df %>%
  mutate(lon_original = decimalLongitude, lat_original = decimalLatitude)
points_sf <- st_as_sf(df, coords = c('decimalLongitude', 'decimalLatitude'), crs = 4326)

#### "UURHOKKEN" using shapefile ######
## uurhok is a XY coordinate for a square of 4x4km for example E2-44
    # Load IFBL grid shape file (update the path to your shape file)
    ifbl_grid <- st_read('spatial/ifbl04x04.shp')
    # Transform the points to the same CRS as IFBL grid
    points_transformed <- st_transform(points_sf, st_crs(ifbl_grid))
    # Spatial join: match each point with the IFBL grid square it falls into
    joined_data <- st_join(points_transformed, ifbl_grid)
    # Convert joined data back to a regular data frame
    df_joinedHOUR <- as.data.frame(joined_data)
    head(df_joinedHOUR)
    # Save the result
    write_csv2(df_joinedHOUR, 'data_raw/joined_data_with_IFBL_hoursquares.csv')
    str(df_joinedHOUR)

#### "KWARTIERHOKKEN" using kml file ######
## kwartierhok is derived from uurhok, each uurhok contains 16 kwartierhokken (squares of 1x1 km)
    # Load IFBL grid KML file (update the path to your KML file)
    ifbl_grid_kml <- st_read('spatial/IFBL_kwartierhokken.kml')
    # Spatial join: match each point with the IFBL grid square it falls into
    joined_data <- st_join(points_sf, ifbl_grid_kml)
    # Convert joined data back to a regular data frame
    df_joined <- as.data.frame(joined_data)
    # Rename 'Name' column to 'IFBL_kwartier' and drop 'Description' column
    df_joinedQUARTER <- df_joined %>%
      rename(IFBL_kwartier = Name) %>%
      select(-Description, -geometry)
    # Save the result
    write_csv2(df_joinedQUARTER, 'data_raw/data_with_IFBL_quartersquares.csv')
str(df_joinedQUARTER)
