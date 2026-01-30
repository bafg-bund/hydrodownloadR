# Keep R CMD check quiet about NSE (dplyr/data.table column names etc.)
utils::globalVariables(c(
  ".data", ":=",
  "timestamp", "value", "station_id", "country", "provider_id", "provider_name",
  "unit", "quality_code", "source_url", "lat", "lon", "area", "altitude"
))
