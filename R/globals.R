# Keep R CMD check quiet about NSE (dplyr/data.table column names etc.)
utils::globalVariables(c(
  ".data", ":=",
  "timestamp", "value", "station_id", "country", "provider_id", "provider_name",
  "unit", "quality_code", "source_url", "lat", "lon", "area", "altitude",

  # --- reported by R CMD check ---
  "AT_EHYD_ROOT_DIRNAME",
  "Hydrological year",
  "Month indicator in the hydrological year",
  "Day",
  "Calendar month",
  "Date",

  "TipoEstacaoDescLiquida",
  "TipoEstacaoEscala",
  "TipoEstacaoQualAgua",
  "Latitude", "Longitude", "Nome", "RioNome", "Codigo", "AreaDrenagem", "Altitude",
  "code",
  "station_final", "river_final", "area_num", "alt0",
  "has_discharge", "has_level", "has_quality",

  "place_id",
  "code_site", "altitude_api", "altitude_site", "altitude_station",
  "qf_desc",
  "value_dvr90_unit", "vertical_datum",
  "depth", "discharge_m3s"
))
