# data-raw/build_us_usgs_stations_meta.R
# Run locally. Produces data/us_usgs_stations_meta.rda
# NOTE: This will take a while the first time (~1 hour), uses your API_USGS_PAT.

devtools::load_all(".")  # so your adapter is in scope
svc <- register_US_USGS()

st <- stations.hydro_service_US_USGS(svc, update = TRUE)

# keep only the final harmonized columns you decided on
us_usgs_stations_meta <- tibble::tibble(
  country       = st$country,
  provider_id   = st$provider_id,
  provider_name = st$provider_name,
  station_id    = st$station_id,
  station_name  = st$station_name,
  lat           = st$lat,
  lon           = st$lon,
  area          = st$area,
  altitude      = st$elevation,
  state_code      = st$state_cd,     # "01".."78"
  state_name    = st$state_name
)

source_date <- Sys.Date()                 # build-time date
attr(us_usgs_stations_meta, "source_date") <- as.Date(source_date)

# Save into data/ as compressed .rda
dir.create("data", showWarnings = FALSE)
save(us_usgs_stations_meta, file = "data/us_usgs_stations_meta.rda", compress = "xz")
message("Saved data/us_usgs_stations_meta.rda with ", nrow(us_usgs_stations_meta), " rows.")
