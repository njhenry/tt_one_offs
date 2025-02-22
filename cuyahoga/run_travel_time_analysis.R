## #######################################################################################
##
## Get walking time to the nearest transit stop for each parcel in Cuyahoga County
##
## Created by Nathaniel Henry, nat@henryspatialanalysis.com
## PURPOSE: Prepared for Liam Leveto at Cuyahoga County Planning Commission --
##   lleveto@cuyahogacounty.gov
##
## #######################################################################################

## 1. Setup ----------------------------------------------------------------------------->

# Load packages
load_packages <- c('data.table', 'elevatr', 'ggplot2', 'glue', 'sf', 'terra', 'units')
load_packages |> lapply(library, character.only = TRUE) |> invisible()

# Set working directory for file i/o
work_dir <- '~/temp_data/city_walkability/cuyahoga'
run_version <- Sys.Date() |> gsub(pattern = '-', replacement = '')

# Load input data
transit_stops_proj <- sf::st_read(file.path(work_dir, 'TOD_All_Transit_Stops.shp'))
parcels_proj <- sf::st_read(file.path(work_dir, 'TOD_EPV_Parcels_CLOSE.shp'))
unprojected_crs <- sf::st_crs("EPSG:4326")
osm_dl_path <- "https://download.geofabrik.de/north-america/us/ohio-latest.osm.pbf"


## 2. Download OSM data and crop to the study area -------------------------------------->

transit_stops <- sf::st_transform(transit_stops_proj, crs = unprojected_crs)
parcels <- sf::st_transform(parcels_proj, crs = unprojected_crs)

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
options(timeout = max(1e4, getOption("timeout")))
utils::download.file(
  url = osm_dl_path,
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
if(file.exists(full_osm_path)) unlink(full_osm_path)


## 3. Download elevation raster for the study area -------------------------------------->

message("Loading DEM from OpenTopography")
elev_raster <- elevatr::get_elev_raster(
  locations = bb_sf, z = 12, clip = 'bbox', neg_to_na = TRUE
)
terra::writeRaster(,
  x = elev_raster,
  filename = file.path(work_dir, 'dem.tif')
)


## 4. Set up R5R and run ---------------------------------------------------------------->

# Configure parcels for R5R
parcels_proj$id <- as.character(seq_len(nrow(parcels_proj)))
parcels$id <- as.character(seq_len(nrow(parcels)))
destinations <- parcels |>
  sf::st_make_valid() |>
  sf::st_centroid(of_largest_polygon = TRUE) |>
  sf::st_coordinates() |>
  data.table::as.data.table() |>
  _[, id := as.character(.I) ] |>
  data.table::setnames(c('X', 'Y'), c('lon', 'lat'))
data.table::fwrite(destinations, file = file.path(work_dir, 'dest_meta.csv'))

transit_stops$id <- as.character(seq_len(nrow(transit_stops)))
origins <- cbind(sf::st_coordinates(transit_stops)) |>
  data.table::as.data.table() |>
  data.table::setnames(new = c('lon', 'lat')) |>
  _[, id := as.character(.I)]
data.table::fwrite(origins, file = file.path(work_dir, 'orig_meta.csv'))

# Set Java allowed memory before loading R5R
options(java.parameters = "-Xmx6G")
library(r5r)
# Set up R5 for the study area - this may take several minutes for the first setup
# If this function yields an error, try launching R with admin permissions
r5r_core <- r5r::setup_r5(data_path = work_dir)

travel_times_by_od_pair <- r5r::travel_time_matrix(
  r5r_core = r5r_core,
  origins = origins,
  destinations = destinations,
  mode = "WALK",
  walk_speed = 5.0,
  max_trip_duration = 30L
)

# Merge minimum travel times back onto parcels
min_times <- travel_times_by_od_pair[
  , .(walk_time = min(travel_time_p50)),
  by = .(id = to_id)
]
parcels_with_times <- merge(x = parcels_proj, y = min_times, by = 'id', all.x = TRUE)
sf::st_write(parcels_with_times, dsn = file.path(work_dir, 'parcels_with_times.gpkg'))
