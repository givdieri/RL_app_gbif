# (update path as needed)
# ## all information about uurhok and kwartierhok can be found on https://www.natuurpunt.be/werkgroepen/fon-plantenwerkgroep-0/werking/ifbl-kartering

# Load necessary libraries
library(sf)
library(readxl)
library(dplyr)

# Read in the Excel file
df <- read_excel('file_with_occurence_and_longitude_and_latitude_for_each.xlsx')
# Convert to sf object using lon and lat columns
# Duplicate lon and lat columns before converting to sf object
df <- df %>%
  mutate(lon_original = lon, lat_original = lat)
points_sf <- st_as_sf(df, coords = c('lon', 'lat'), crs = 4326)

#### "UURHOKKEN" using shapefile ######
## uurhok is a XY coordinate for a square of 4x4km for example E2-44
    # Load IFBL grid shape file (update the path to your shape file)
    ifbl_grid <- st_read('ifbl04x04.shp')
    # Transform the points to the same CRS as IFBL grid
    points_transformed <- st_transform(points_sf, st_crs(ifbl_grid))
    # Spatial join: match each point with the IFBL grid square it falls into
    joined_data <- st_join(points_transformed, ifbl_grid)
    # Convert joined data back to a regular data frame
    df_joined <- as.data.frame(joined_data)
    head(df_joined)
    # Save the result
    write_csv2(df_joined, 'joined_data_with_IFBL_squares.csv')


#### "KWARTIERHOKKEN" using kml file ######
## kwartierhok is derived from uurhok, each uurhok contains 16 kwartierhokken (squares of 1x1 km)
    # Load IFBL grid KML file (update the path to your KML file)
    ifbl_grid_kml <- st_read('IFBL_kwartierhokken.kml')
    # Spatial join: match each point with the IFBL grid square it falls into
    joined_data <- st_join(points_sf, ifbl_grid_kml)
    # Convert joined data back to a regular data frame
    df_joined <- as.data.frame(joined_data)
    # Rename 'Name' column to 'IFBL_kwartier' and drop 'Description' column
    df_joined <- df_joined %>%
      rename(IFBL_kwartier = Name) %>%
      select(-Description, -geometry)
    # Save the result
    write_csv2(df_joined, 'data_with_IFBL_squares.csv')

