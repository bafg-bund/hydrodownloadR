# ---- Registration ------------------------------------------------------------

#' @keywords internal
#' @noRd
register_NO_NVE <- function() {
  register_service_usage(
    provider_id   = "NO_NVE",
    provider_name = "Norwegian Water Resources and Energy Directorate (NVE)",
    country       = "Norway",
    base_url      = "https://hydapi.nve.no",
    rate_cfg      = list(n = 3, period = 1),
    auth          = list(type = "api_key", header = "X-API-Key", env = "NVE_API_KEY")
  )
}

#' @export
timeseries_parameters.hydro_service_NO_NVE <- function(x, ...) {
  c(
    "water_discharge","water_level","water_temperature",
    "conductivity","ph",
    "suspended_sediment_concentration","organic_material_concentration",
    "turbidity_nephelometric","turbidity_formazin",
    "nitrate_n","ammonium_n",
    "soil_moisture_raw"
  )
}

# ---- Parameter mapping (private) --------------------------------------------

.no_param_map <- function(parameter) {
  switch(parameter,

         # Core trio
         water_level = list(
           unit     = "cm",
           canon    = "cm",
           param_id = 1000L,                       # NVE: Stage (API unit is meters)
           to_canon = function(v, raw_unit = NULL) {
             # Convert meters to cm when needed; pass-through otherwise
             ru <- tolower(raw_unit %||% "")
             if (ru %in% c("m","meter","metre")) v * 100 else v
           }
         ),

         water_discharge = list(
           unit     = "m^3/s",
           canon    = "m^3/s",
           param_id = 1001L,                       # NVE: Discharge
           to_canon = function(v, raw_unit = NULL) v
         ),

         water_temperature = list(
           unit     = "\u00B0C",
           canon    = "\u00B0C",
           param_id = 1003L,                       # NVE: Water temperature
           to_canon = function(v, raw_unit = NULL) v
         ),

         # --- Extras you want to test ---------------------------------------------

         conductivity = list(
           unit     = "\u00B5S/cm",
           canon    = "\u00B5S/cm",
           param_id = 1006L,                       # Conductivity
           to_canon = function(v, raw_unit = NULL) v
         ),

         ph = list(
           unit     = "pH",
           canon    = "pH",
           param_id = 1007L,                       # pH
           to_canon = function(v, raw_unit = NULL) v
         ),

         suspended_sediment_concentration = list(
           unit     = "mg/l",
           canon    = "mg/l",
           param_id = 1200L,                       # Concentration suspended (inorg.) sediment
           to_canon = function(v, raw_unit = NULL) v
         ),

         organic_material_concentration = list(
           unit     = "mg/l",
           canon    = "mg/l",
           param_id = 1208L,                       # Concentration of organic material
           to_canon = function(v, raw_unit = NULL) v
         ),

         turbidity_nephelometric = list(
           unit     = "NTU",
           canon    = "NTU",
           param_id = 1215L,                       # Turbidity (Nephelometric)
           to_canon = function(v, raw_unit = NULL) v
         ),

         turbidity_formazin = list(
           unit     = "FTU",
           canon    = "FTU",
           param_id = 1216L,                       # Turbidity (Formazin)
           to_canon = function(v, raw_unit = NULL) v
         ),

         nitrate_n = list(
           unit     = "\u00B5g/l",
           canon    = "\u00B5g/l",
           param_id = 8292L,                       # Nitrate Nitrogen
           to_canon = function(v, raw_unit = NULL) v
         ),

         ammonium_n = list(
           unit     = "\u00B5g/l",
           canon    = "\u00B5g/l",
           param_id = 8291L,                       # Ammonium Nitrogen
           to_canon = function(v, raw_unit = NULL) v
         ),

         soil_moisture_raw = list(
           unit     = "#",                         # API lists '#'; leaving as-is
           canon    = "#",
           param_id = 9306L,                       # Soil moisture raw data
           to_canon = function(v, raw_unit = NULL) v
         ),

         stop("Unsupported parameter: ", parameter)
  )
}


# ---- Auth helper ------------------------------------------------------------

# Looks for a key in this order:
# 1) explicit `api_key=` argument
# 2) options("NVE_API_KEY")
# 3) Sys.getenv("NVE_API_KEY")
.with_api_key <- function(req, api_key = NULL) {
  key <- api_key %||% getOption("NVE_API_KEY", NULL) %||% Sys.getenv("NVE_API_KEY", unset = "")
  key <- trimws(key %||% "")

  if (!nzchar(key)) {
    cli::cli_abort(c(
      "x" = "NVE HydAPI requires an API key.",
      "i" = "Request a key at: https://hydapi.nve.no/Users",
      ">" = "Provide via `api_key = \"...\"`, or set once with either:",
      " " = "  Sys.setenv(NVE_API_KEY = \"<your-key>\")   # session/env",
      " " = "  options(NVE_API_KEY = \"<your-key>\")      # R option"
    ))
  }

  httr2::req_headers(req, "X-API-Key" = key)
}


.no_empty_ts <- function(x, parameter, unit) {
  tibble::tibble(
    country         = x$country,
    provider_id     = x$provider_id,
    provider_name   = x$provider_name,
    station_id      = character(),
    parameter       = rep(parameter, 0),
    timestamp       = as.POSIXct(character(), tz = "UTC"),
    value           = numeric(),
    unit            = rep(unit, 0),
    quality_code    = character(),
    qf_desc         = character(),
    correction_code = character(),
    cor_desc        = character(),
    source_url      = character()
  )
}


# ---- Stations ---------------------------------------------------------------

#' @export
stations.hydro_service_NO_NVE <- function(x, stations = NULL, ...) {
  req  <- build_request(x, path = "/api/v1/Stations")
  req  <- .with_api_key(req, list(...)$api_key)
  resp <- perform_request(req)
  js   <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  df <- tibble::as_tibble(js$data %||% list())
  if (!nrow(df)) {
    return(tibble::tibble(
      country=x$country, provider_id=x$provider_id, provider_name=x$provider_name,
      station_id=character(), station_name=character(), station_name_ascii=character(),
      river=character(), river_ascii=character(),
      lat=numeric(), lon=numeric(), area=numeric(), elevation=numeric()
    ))
  }

  out <- tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = as.character(df$stationId),
    station_name  = as.character(df$stationName),
    station_name_ascii = to_ascii(as.character(df$stationName)),
    river         = as.character(df$riverName),
    river_ascii   = to_ascii(as.character(df$riverName)),
    lat           = suppressWarnings(as.numeric(df$latitude)),
    lon           = suppressWarnings(as.numeric(df$longitude)),
    area          = suppressWarnings(as.numeric(df$drainageBasinArea)),
    altitude      = suppressWarnings(as.numeric(df$masl))
  )

  # optional filter
  if (!is.null(stations) && length(stations)) {
    ids <- stations
    if (is.data.frame(ids) && "station_id" %in% names(ids)) ids <- ids$station_id
    ids <- suppressWarnings(as.character(unlist(ids, use.names = FALSE)))
    ids <- ids[nzchar(ids)]
    out <- dplyr::filter(out, .data$station_id %in% ids)
  }

  dplyr::distinct(out, .data$station_id, .keep_all = TRUE)
}

# ---- NVE code to description maps (private) ----------------------------------

.nve_quality_map <- c(
  `0` = "Unknown",
  `1` = "Uncontrolled",
  `2` = "PrimaryControlled",
  `3` = "SecondaryControlled"
)

.nve_correction_map <- c(
  `0`  = "No changes",
  `1`  = "Manual or ice correction",
  `2`  = "Interpolation",
  `3`  = "Computed from models/other series",
  `4`  = "Daily mean = arithmetic mean (normally curve-based)",
  `5`  = "Smoothed negative value (inflow)",
  `6`  = "Dry pipe (groundwater)",
  `7`  = "Ice in pipe (groundwater)",
  `8`  = "Damaged pipe (groundwater)",
  `9`  = "Pumping (groundwater)",
  `11` = "Start/end value linear adjustment",
  `12` = "Incomplete data source",
  `13` = "Calculated from similar/nearby station (statistical adjustment)",
  `14` = "Statistically infilled missing value",
  `15` = "Computation produced NaN/Inf (outside valid range)",
  `16` = "Value fetched from rejected period"
)

.map_desc <- function(code, map) {
  if (is.null(code)) return(rep(NA_character_, 0))
  # keep vectorized behavior; unknown codes to "Unknown code: <value>"
  key <- as.character(code)
  out <- unname(map[key])
  out[is.na(out) & !is.na(key)] <- paste0("Unknown code: ", key[is.na(out) & !is.na(key)])
  out[is.na(key)] <- NA_character_
  out
}


# ---- Timeseries (daily) -----------------------------------------------------

#' @export
timeseries.hydro_service_NO_NVE <- function(x,
                                            parameter = c(
                                              "water_discharge","water_level","water_temperature",
                                              "conductivity","ph",
                                              "suspended_sediment_concentration","organic_material_concentration",
                                              "turbidity_nephelometric","turbidity_formazin",
                                              "nitrate_n","ammonium_n",
                                              "soil_moisture_raw"
                                            ),
                                            stations = NULL, start_date = NULL, end_date = NULL, mode = c("complete","range"),
                                            exclude_quality = NULL, ...) {

  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)
  rng       <- resolve_dates(mode, start_date, end_date)

  pm <- .no_param_map(parameter)
  st <- stations.hydro_service_NO_NVE(x)

  # normalize stations -> vector of ids
  ids <- if (is.null(stations) || !length(stations)) st$station_id else {
    s <- stations
    if (is.data.frame(s) && "station_id" %in% names(s)) s <- s$station_id
    suppressWarnings(as.character(unlist(s, use.names = FALSE)))
  }
  ids <- unique(ids[nzchar(ids)])
  if (!length(ids)) return(.no_empty_ts(x, parameter, pm$unit))

  # Build ISO-8601 interval WITH spaces around "/"
  q_dates <- sprintf("%s / %s",
                     format(rng$start_date, "%Y-%m-%d"),
                     format(rng$end_date,   "%Y-%m-%d")
  )

  batches <- chunk_vec(ids, 50)
  pb <- progress::progress_bar$new(total = length(batches))

  out <- lapply(batches, function(batch) {
    pb$tick()
    one <- ratelimitr::limit_rate(function(stid) {
      stid <- as.character(stid)

      qry <- list(
        StationId      = stid,
        Parameter      = pm$param_id,
        ResolutionTime = "day",
        ReferenceTime  = q_dates
      )

      req  <- build_request(x, path = "/api/v1/Observations", query = qry)
      req  <- .with_api_key(req)

      # Don't throw on 404/204; let us return empty tibble
      req  <- httr2::req_error(req, is_error = function(resp) {
        status <- httr2::resp_status(resp)
        status >= 400 && !(status %in% c(404, 204))
      })

      resp <- perform_request(req)
      status <- httr2::resp_status(resp)
      if (status %in% c(204, 404)) return(.no_empty_ts(x, parameter, pm$unit))
      if (status %in% c(401, 403)) {
        cli::cli_abort("NVE HydAPI denied access (HTTP {status}). Provide a valid API key via `api_key` or NVE_API_KEY env.")
      }
      if (status >= 400) {
        cli::cli_warn("NVE HydAPI HTTP {status} for {stid}")
        return(.no_empty_ts(x, parameter, pm$unit))
      }

      # Parse body *after* status handling
      js <- httr2::resp_body_json(resp, simplifyVector = TRUE)
      series <- js$data
      if (is.null(series) || !length(series)) return(.no_empty_ts(x, parameter, pm$unit))

      obs <- tryCatch(series$observations[[1]], error = function(e) NULL)
      if (is.null(obs) || !length(obs)) return(.no_empty_ts(x, parameter, pm$unit))

      # Parse columns with type safety
      ts <- suppressWarnings(lubridate::ymd_hms(obs$time, tz = "UTC"))

      val_raw <- obs$value
      val <- if (is.numeric(val_raw)) as.numeric(val_raw) else
        suppressWarnings(readr::parse_number(as.character(val_raw)))

      qf  <- obs$quality
      cor <- obs$correction

      start_utc <- as.POSIXct(rng$start_date, tz = "UTC")
      end_utc   <- as.POSIXct(rng$end_date,   tz = "UTC") + 24*3600 - 1

      keep <- !is.na(ts) & ts >= start_utc & ts <= end_utc
      if (!any(keep)) return(.no_empty_ts(x, parameter, pm$unit))

      if (!is.null(exclude_quality)) {
        keep <- keep & !(as.character(qf) %in% as.character(exclude_quality))
        if (!any(keep)) return(.no_empty_ts(x, parameter, pm$unit))
      }

      # Build a per-station provenance URL (readable)
      source_url <- tryCatch({
        u <- httr2::url_parse(x$base_url)
        u$path  <- "/api/v1/Observations"
        u$query <- qry
        httr2::url_build(u)
      }, error = function(e) NA_character_)

      tibble::tibble(
        country         = x$country,
        provider_id     = x$provider_id,
        provider_name   = x$provider_name,
        station_id      = stid,
        parameter       = parameter,
        timestamp       = ts[keep],
        value           = pm$to_canon(val[keep], raw_unit = NULL),
        unit            = pm$unit,
        quality_code    = if (is.null(qf)) NA_character_ else as.character(qf[keep]),
        qf_desc         = if (is.null(qf)) NA_character_ else .map_desc(qf[keep], .nve_quality_map),
        correction_code = if (is.null(cor)) NA_character_ else as.character(cor[keep]),
        cor_desc        = if (is.null(cor)) NA_character_ else .map_desc(cor[keep], .nve_correction_map),
        source_url      = source_url %||% paste0(x$base_url, "/api/v1/Observations")
      )
    }, rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period))

    dplyr::bind_rows(lapply(batch, one))
  })

  res <- dplyr::bind_rows(out)
  if (nrow(res)) res <- dplyr::arrange(res, .data$station_id, .data$timestamp)
  res
}
