## #######################################################################################
##
## Estimate travel times to select stops on Lake City Way
##
## Created by Nathaniel Henry, nat@henryspatialanalysis.com
## Purpose: Prepare data for a map at city council meeting
##
## #######################################################################################

## 1. Setup ----------------------------------------------------------------------------->

# Load packages
load_packages <- c('data.table', 'elevatr', 'ggplot2', 'glue', 'sf', 'terra', 'units')
load_packages |> lapply(library, character.only = TRUE) |> invisible()

# Set working directory for file i/o
work_dir <- '~/temp_data/city_walkability/lake_city_way'

# Load input data
transit_stops <- data.table::fread(file.path(work_dir, 'transit_stops.csv'))
parcels <- sf::st_read(file.path(work_dir, 'nearby_parcels_unprojected.gpkg'))


## 2. Download OSM data and crop to the study area -------------------------------------->

# Get buffered bounding box of the study area
bb_sf <- parcels |>
  sf::st_bbox() |>
  sf::st_as_sfc() |>
  sf::st_buffer(dist = units::set_units(1000, 'ft'), endCapStyle = "SQUARE") |>
  sf::st_as_sf()
bb <- bb_sf |>
  sf::st_bbox()
yaml::write_yaml(
  as.list(bb),
  file = file.path(work_dir, 'extended_bbox.yaml')
)

# Download OSM data for the full state and save it in the raw data folder
full_osm_path <- file.path(work_dir, 'osm_extract_full.pbf') |>
  normalizePath() |>
  suppressWarnings()
message("Downloading state-wide OSM extract")
options(timeout = max(300, getOption("timeout")))
utils::download.file(
  url = "https://download.geofabrik.de/north-america/us/washington-latest.osm.pbf",
  destfile = full_osm_path,
  method = 'auto'
)

# Use the osmosis command line tool to crop the full extract file to the bounding box
subset_osm_path <- file.path(work_dir, 'osm_subset.pbf') |>
  normalizePath() |>
  suppressWarnings()
osmosis_command <- glue::glue(
  "osmosis --read-pbf {full_osm_path} --bounding-box top='{bb$ymax}' left='{bb$xmin}' ",
  "bottom='{bb$ymin}' right='{bb$xmax}' --write-pbf {subset_osm_path}"
)
message("Running osmosis to subset the OSM file to the study area")
system(osmosis_command)
message("Finished downloading and subsetting OSM extract.")


## 3. Download elevation raster for the study area -------------------------------------->

message("Loading DEM from OpenTopography")
elev_raster <- elevatr::get_elev_raster(
  locations = bb_sf, z = 14, clip = 'bbox', neg_to_na = TRUE
)
terra::writeRaster(,
  x = elev_raster,
  filename = file.path(work_dir, 'dem.tif')
)

message("Plotting raster DEM for vetting")
er_df <- as.data.frame(elev_raster, xy = TRUE)
colnames(er_df)[3] <- 'Elevation'
fig <- ggplot() +
  geom_raster(data = er_df, aes(x = x, y = y, fill = Elevation)) +
  geom_sf(
    data = bb_sf,
    fill = NA, linewidth = .2, alpha = .5
  ) +
  scale_fill_gradientn(colours = viridisLite::viridis(10)) +
  theme_void()
pdf(file.path(work_dir, 'plot_raster_dem.pdf'), height = 10, width = 10)
print(fig)
dev.off()


## 4. Set up R5R and run ---------------------------------------------------------------->

# Configure parcels for R5R
parcels$id <- as.character(seq_len(nrow(parcels)))
destinations <- parcels |>
  sf::st_make_valid() |>
  sf::st_centroid(of_largest_polygon = T) |>
  sf::st_coordinates() |>
  data.table::as.data.table() |>
  _[, id := as.character(.I) ] |>
  data.table::setnames(c('X', 'Y'), c('lon', 'lat'))
transit_stops[, id := as.character(.I)]

# Set Java allowed memory before loading R5R
options(java.parameters = "-Xmx2G")
library(r5r)
# Set up R5 for the study area - this may take several minutes for the first setup
# If this function yields an error, try launching R with admin permissions
r5r_core <- r5r::setup_r5(data_path = work_dir)

travel_times_by_od_pair <- r5r::travel_time_matrix(
  r5r_core = r5r_core,
  origins = transit_stops,
  destinations = destinations,
  mode = "WALK",
  max_trip_duration = 5L
)

# Merge minimum travel times back onto parcels
min_times <- travel_times_by_od_pair[
  , .(walk_time = min(travel_time_p50)),
  by = .(id = to_id)
]
parcels_with_times <- merge(x = parcels, y = min_times, by = 'id', all.x = TRUE)
sf::st_write(parcels_with_times, dsn = file.path(work_dir, 'parcels_with_times.gpkg'))
