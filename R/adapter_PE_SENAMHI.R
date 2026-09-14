# R/adapter_PE_SENAMHI.R
# Servicio Nacional de Meteorología e Hidrología del Perú - SENAMHI
#
# provider_usage.R classification:
#   access_class = "public"
#   reuse_class  = "restricted_no_redistribution"
#
# This is a conservative reuse classification: SENAMHI permits public use of
# the information subject to attribution and other terms, but does not provide
# an explicit open-data licence or a clear grant to redistribute the raw data.
# Terms: https://www.senamhi.gob.pe/?p=terminos-condiciones
# Required source notice (as published by SENAMHI):
#   "Información recopilada y trabajada por el Servicio Nacional de
#   Meteorología e Hidrología del Perú. El uso que se le da a esta información
#   es de mi (nuestra) entera responsabilidad".
#
# The station catalogue is an official WFS service. Daily discharge and water
# level values are parsed from the public PHISIS monitoring graph. The graph
# endpoint is public but undocumented, so requests are deliberately rate
# limited. The service does not expose observation-level quality metadata,
# so the standardized quality fields are returned as NA.

# -- Registration --------------------------------------------------------------

#' @keywords internal
#' @noRd
register_PE_SENAMHI <- function() {
  register_service_usage(
    provider_id   = "PE_SENAMHI",
    provider_name = paste0(
      "Servicio Nacional de Meteorología e Hidrología ",
      "del Perú - SENAMHI"
    ),
    country       = "Peru",
    base_url      = "https://www.senamhi.gob.pe",
    rate_cfg      = list(n = 1, period = 1),
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_PE_SENAMHI <- function(x, ...) {
  c("water_discharge", "water_level")
}

# -- Constants -----------------------------------------------------------------

.pe_senamhi_site_base <- "https://www.senamhi.gob.pe"
.pe_senamhi_wfs_base <- "https://idesep.senamhi.gob.pe"
.pe_senamhi_tz <- "UTC"

# PHISIS monitoring observations currently exposed by the daily graph begin
# in the 2020/2021 hydrological year. Individual stations can begin later.
.pe_senamhi_first_date <- as.Date("2020-09-01")

.pe_senamhi_paths <- list(
  wfs = "/geoserver/g_dbphisis/ows",
  graph = "/mapas/mapa-monitoreohidro/include/mnt-grafica-new.php"
)

.pe_senamhi_station_columns <- c(
  "country", "provider_id", "provider_name", "station_id",
  "station_name", "station_name_ascii", "river", "river_ascii",
  "lat", "lon", "area", "altitude"
)

# River names are absent from the WFS but present in the PHISIS graph payload.
# Cache them for the current R session so that stations() only enriches a code
# once.
.pe_senamhi_cache <- new.env(parent = emptyenv())
.pe_senamhi_cache$rivers <- stats::setNames(character(), character())

# -- Empty return values --------------------------------------------------------

.pe_empty_stations <- function() {
  tibble::tibble(
    country            = character(),
    provider_id        = character(),
    provider_name      = character(),
    station_id         = character(),
    station_name       = character(),
    station_name_ascii = character(),
    river              = character(),
    river_ascii        = character(),
    lat                = numeric(),
    lon                = numeric(),
    area               = numeric(),
    altitude           = numeric()
  )
}

.pe_empty_catalogue <- function() {
  tibble::tibble(
    country            = character(),
    provider_id        = character(),
    provider_name      = character(),
    station_id         = character(),
    station_name       = character(),
    station_name_ascii = character(),
    river              = character(),
    river_ascii        = character(),
    lat                = numeric(),
    lon                = numeric(),
    area               = numeric(),
    altitude           = numeric(),
    variable_code      = character(),
    last_observation   = as.POSIXct(character(), tz = .pe_senamhi_tz),
    basin               = character()
  )
}

.pe_empty_timeseries <- function() {
  tibble::tibble(
    country       = character(),
    provider_id   = character(),
    provider_name = character(),
    station_id    = character(),
    parameter     = character(),
    timestamp     = as.POSIXct(character(), tz = .pe_senamhi_tz),
    value         = numeric(),
    unit          = character(),
    quality_code  = character(),
    quality_name  = character(),
    quality_desc  = character(),
    source_url    = character()
  )
}

# -- General helpers -----------------------------------------------------------

.pe_check_response <- function(resp, what) {
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    rlang::abort(
      paste0(
        "PE_SENAMHI ", what, " failed with HTTP status ", status, "."
      )
    )
  }
  invisible(resp)
}

.pe_limited_perform <- function(x) {
  ratelimitr::limit_rate(
    function(req) perform_request(req),
    rate = ratelimitr::rate(
      n = x$rate_cfg$n,
      period = x$rate_cfg$period
    )
  )
}

.pe_request <- function(x, base_url, path, query = NULL) {
  service <- x
  service$base_url <- base_url
  req <- build_request(service, path = path)

  if (!is.null(query) && length(query)) {
    req <- do.call(
      httr2::req_url_query,
      c(list(.req = req), query)
    )
  }

  httr2::req_headers(
    req,
    Accept = "application/json, text/html;q=0.9, */*;q=0.8",
    `Accept-Language` = "es-PE,es;q=0.9,en;q=0.7"
  )
}

.pe_clean_text <- function(x) {
  if (is.null(x) || !length(x)) {
    return(character())
  }

  x <- normalize_utf8(as.character(x))
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- trimws(gsub("[[:space:]]+", " ", x))
  x[!nzchar(x)] <- NA_character_
  x
}

.pe_scalar_chr <- function(x, default = NA_character_) {
  if (is.null(x) || !length(x) || is.null(x[[1]])) {
    return(default)
  }

  out <- .pe_clean_text(x[[1]])
  if (!length(out) || is.na(out[[1]]) || !nzchar(out[[1]])) {
    return(default)
  }
  out[[1]]
}

.pe_scalar_num <- function(x, default = NA_real_) {
  if (is.null(x) || !length(x) || is.null(x[[1]])) {
    return(default)
  }
  out <- suppressWarnings(as.numeric(x[[1]]))
  if (!length(out) || !is.finite(out[[1]])) default else out[[1]]
}

.pe_parse_wfs_time <- function(x) {
  x <- .pe_clean_text(x)
  if (!length(x)) {
    return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = .pe_senamhi_tz))
  }

  x <- sub("Z$", "", x)
  as.POSIXct(
    x,
    format = "%Y-%m-%dT%H:%M:%OS",
    tz = .pe_senamhi_tz
  )
}

.pe_date <- function(x) {
  if (is.null(x) || !length(x)) {
    return(NULL)
  }
  if (inherits(x, "Date")) {
    return(x[1])
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x[1], tz = .pe_senamhi_tz))
  }
  as.Date(x[[1]])
}

.pe_source_url <- function(station_id, variable, variable_option, anchor) {
  paste0(
    .pe_senamhi_site_base,
    .pe_senamhi_paths$graph,
    "?fecha_hora=",
    utils::URLencode(anchor, reserved = TRUE),
    "&id=",
    utils::URLencode(station_id, reserved = TRUE),
    "&variable=",
    utils::URLencode(variable, reserved = TRUE),
    "&variable_opcion=",
    utils::URLencode(variable_option, reserved = TRUE)
  )
}

.pe_graph_parameter <- function(parameter) {
  if (identical(parameter, "water_discharge")) "CAUDAL" else "NIVEL"
}

.pe_same_station <- function(returned, requested) {
  returned <- toupper(.pe_scalar_chr(returned))
  requested <- toupper(.pe_scalar_chr(requested))
  !is.na(returned) && !is.na(requested) && identical(returned, requested)
}

.pe_supports_parameter <- function(variable_code, parameter) {
  variable_code <- toupper(.pe_clean_text(variable_code))
  if (identical(parameter, "water_discharge")) {
    variable_code %in% c("C", "A")
  } else {
    variable_code %in% c("N", "A")
  }
}

.pe_unit_transform <- function(unit, parameter) {
  raw <- .pe_scalar_chr(unit)
  if (is.na(raw)) {
    raw <- if (identical(parameter, "water_discharge")) "m^3/s" else "m"
  }

  key <- tolower(raw)
  key <- gsub("\u00b3", "3", key, fixed = TRUE)
  key <- gsub("[[:space:].^_-]", "", key)

  if (identical(parameter, "water_discharge")) {
    if (key %in% c("m3/s", "m3s", "m3s1")) {
      return(list(unit = "m^3/s", multiplier = 1))
    }
    if (key %in% c("l/s", "ls", "ls1")) {
      return(list(unit = "m^3/s", multiplier = 0.001))
    }
  } else {
    if (key %in% c("m", "metro", "metros")) {
      return(list(unit = "m", multiplier = 1))
    }
    if (key %in% c("cm", "centimetro", "centimetros")) {
      return(list(unit = "m", multiplier = 0.01))
    }
    if (key %in% c("mm", "milimetro", "milimetros")) {
      return(list(unit = "m", multiplier = 0.001))
    }
    if (key %in% c("msnm", "masl")) {
      return(list(unit = "m a.s.l.", multiplier = 1))
    }
  }

  list(unit = raw, multiplier = 1)
}

# -- PHISIS graph payload ------------------------------------------------------

.pe_extract_graph_data <- function(resp) {
  body <- rawToChar(httr2::resp_body_raw(resp))
  pattern <- paste0(
    "(?s)var\\s+data\\s*=\\s*(\\{.*?\\})\\s*",
    "(?:var\\s+dataCSV|var\\s+dataTest|grafica(?:Diario|Horario)\\s*\\()"
  )
  match <- regexec(pattern, body, perl = TRUE)
  hit <- regmatches(body, match)[[1]]

  if (length(hit) < 2 || !nzchar(hit[[2]])) {
    return(NULL)
  }

  tryCatch(
    jsonlite::fromJSON(hit[[2]], simplifyVector = FALSE),
    error = function(e) NULL
  )
}

.pe_graph_request <- function(
    x,
    station_id,
    variable_code,
    parameter,
    anchor,
    daily = FALSE
) {
  variable <- .pe_graph_parameter(parameter)
  variable_code <- toupper(.pe_scalar_chr(variable_code, default = "C"))

  req <- .pe_request(
    x,
    base_url = .pe_senamhi_site_base,
    path = .pe_senamhi_paths$graph
  )

  if (isTRUE(daily)) {
    req <- httr2::req_body_form(
      req,
      fecha_hora = anchor,
      btnTipo = "D",
      variable_opcion = variable_code,
      id = station_id,
      rbtVariable = variable
    )
  } else {
    req <- do.call(
      httr2::req_url_query,
      list(
        .req = req,
        fecha_hora = anchor,
        id = station_id,
        variable = variable,
        variable_opcion = variable_code
      )
    )
  }

  req
}

.pe_graph_anchor <- function(last_observation = NULL, date = NULL) {
  if (
    !is.null(last_observation) &&
    length(last_observation) &&
    !is.na(last_observation[1])
  ) {
    return(format(
      last_observation[1],
      "%Y-%m-%d %H:%M:%S",
      tz = .pe_senamhi_tz
    ))
  }

  if (is.null(date) || !length(date) || is.na(date[1])) {
    date <- as.Date(Sys.time(), tz = .pe_senamhi_tz)
  }
  paste0(format(as.Date(date[1]), "%Y-%m-%d"), " 23:59:59")
}

# -- Station catalogue ---------------------------------------------------------

.pe_wfs_request <- function(x) {
  .pe_request(
    x,
    base_url = .pe_senamhi_wfs_base,
    path = .pe_senamhi_paths$wfs,
    query = list(
      service = "WFS",
      version = "1.0.0",
      request = "GetFeature",
      typeName = "g_dbphisis:estaciones_monitoreo",
      outputFormat = "application/json"
    )
  )
}

.pe_parse_wfs_catalogue <- function(resp, x) {
  payload <- tryCatch(
    jsonlite::fromJSON(
      rawToChar(httr2::resp_body_raw(resp)),
      simplifyVector = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(payload) || is.null(payload$features) || !length(payload$features)) {
    return(.pe_empty_catalogue())
  }

  rows <- lapply(payload$features, function(feature) {
    properties <- feature$properties
    if (is.null(properties)) properties <- list()

    station_id <- .pe_scalar_chr(feature$id)
    station_id <- sub("^[^.]+\\.", "", station_id)

    coordinates <- NULL
    if (!is.null(feature$geometry)) {
      coordinates <- feature$geometry$coordinates
    }

    lon <- .pe_scalar_num(properties$longitud)
    lat <- .pe_scalar_num(properties$latitud)
    if (!is.finite(lon) && length(coordinates) >= 1) {
      lon <- .pe_scalar_num(coordinates[[1]])
    }
    if (!is.finite(lat) && length(coordinates) >= 2) {
      lat <- .pe_scalar_num(coordinates[[2]])
    }
    station_name <- .pe_scalar_chr(properties$nom_estacion)

    tibble::tibble(
      country            = x$country,
      provider_id        = x$provider_id,
      provider_name      = x$provider_name,
      station_id         = station_id,
      station_name       = station_name,
      station_name_ascii = to_ascii(station_name),
      river              = NA_character_,
      river_ascii        = NA_character_,
      lat                = lat,
      lon                = lon,
      area               = NA_real_,
      altitude           = NA_real_,
      variable_code      = toupper(.pe_scalar_chr(properties$variable)),
      last_observation   = .pe_parse_wfs_time(properties$fecha_hora),
      basin               = .pe_scalar_chr(properties$nom_cuenca),
      status              = .pe_scalar_num(properties$estado)
    )
  })

  out <- dplyr::bind_rows(rows)
  if (!nrow(out)) {
    return(.pe_empty_catalogue())
  }

  invalid <- (
    is.na(out$station_id) |
      !nzchar(out$station_id) |
      !is.finite(out$lat) |
      !is.finite(out$lon)
  )
  inactive <- is.finite(out$status) & out$status != 1
  out <- out[!invalid & !inactive, , drop = FALSE]

  out <- out[!duplicated(out$station_id), , drop = FALSE]
  out <- out[order(out$station_name, out$station_id), , drop = FALSE]
  out$status <- NULL
  rownames(out) <- NULL
  out
}

.pe_cached_rivers <- function(station_ids) {
  cache <- .pe_senamhi_cache$rivers
  idx <- match(station_ids, names(cache))
  list(
    known = !is.na(idx),
    value = unname(cache[idx])
  )
}

.pe_store_river <- function(station_id, river) {
  river <- .pe_scalar_chr(river)
  cache <- .pe_senamhi_cache$rivers
  cache[station_id] <- river
  .pe_senamhi_cache$rivers <- cache
  invisible(river)
}

.pe_fetch_river <- function(x, station, perform) {
  parameter <- if (
    .pe_supports_parameter(station$variable_code[[1]], "water_discharge")
  ) {
    "water_discharge"
  } else {
    "water_level"
  }

  anchor <- .pe_graph_anchor(station$last_observation)
  req <- .pe_graph_request(
    x,
    station_id = station$station_id[[1]],
    variable_code = station$variable_code[[1]],
    parameter = parameter,
    anchor = anchor,
    daily = FALSE
  )

  resp <- tryCatch(perform(req), error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) >= 400) {
    return(NA_character_)
  }

  data <- .pe_extract_graph_data(resp)
  if (
    is.null(data) ||
    !.pe_same_station(data$codEstacion, station$station_id[[1]])
  ) {
    return(NA_character_)
  }
  .pe_scalar_chr(data$nomRio)
}

.pe_enrich_rivers <- function(x, catalogue, perform) {
  if (!nrow(catalogue)) {
    return(catalogue)
  }

  cached <- .pe_cached_rivers(catalogue$station_id)
  catalogue$river[cached$known] <- cached$value[cached$known]
  missing <- which(!cached$known)

  if (length(missing)) {
    rlang::inform(
      paste0(
        "PE_SENAMHI: retrieving river names for ", length(missing),
        " station(s); results are cached for this R session."
      )
    )

    batches <- chunk_vec(missing, 10)
    pb <- progress::progress_bar$new(total = length(batches))

    for (batch in batches) {
      pb$tick()
      for (i in batch) {
        river <- .pe_fetch_river(
          x,
          catalogue[i, , drop = FALSE],
          perform
        )
        .pe_store_river(catalogue$station_id[[i]], river)
        catalogue$river[[i]] <- river
      }
    }
  }

  catalogue$river_ascii <- to_ascii(catalogue$river)

  unresolved <- sum(is.na(catalogue$river) | !nzchar(catalogue$river))
  if (unresolved) {
    rlang::warn(
      paste0(
        "PE_SENAMHI: river names could not be resolved for ", unresolved,
        " station(s)."
      )
    )
  }

  catalogue
}

.pe_station_catalogue <- function(x, perform, enrich_river = FALSE) {
  resp <- perform(.pe_wfs_request(x))
  .pe_check_response(resp, "station-catalogue request")
  catalogue <- .pe_parse_wfs_catalogue(resp, x)

  if (isTRUE(enrich_river)) {
    catalogue <- .pe_enrich_rivers(x, catalogue, perform)
  }
  catalogue
}

# -- Stations (S3 method) ------------------------------------------------------

#' @export
stations.hydro_service_PE_SENAMHI <- function(x, ...) {
  perform <- .pe_limited_perform(x)
  catalogue <- .pe_station_catalogue(
    x,
    perform = perform,
    enrich_river = TRUE
  )

  if (!nrow(catalogue)) {
    return(.pe_empty_stations())
  }
  catalogue[, .pe_senamhi_station_columns, drop = FALSE]
}

# -- Station selection ---------------------------------------------------------

.pe_resolve_stations <- function(x, requested, catalogue, parameter) {
  supported <- .pe_supports_parameter(catalogue$variable_code, parameter)

  if (is.null(requested) || !length(requested)) {
    return(catalogue[supported, , drop = FALSE])
  }

  requested <- unique(.pe_clean_text(requested))
  requested <- requested[!is.na(requested) & nzchar(requested)]
  if (!length(requested)) {
    return(catalogue[FALSE, , drop = FALSE])
  }

  idx <- match(toupper(requested), toupper(catalogue$station_id))

  missing <- requested[is.na(idx)]
  if (length(missing)) {
    rlang::warn(
      paste0(
        "PE_SENAMHI: unknown station id(s) skipped: ",
        paste(utils::head(missing, 10), collapse = ", "),
        if (length(missing) > 10) {
          paste0(" ... +", length(missing) - 10, " more")
        } else {
          ""
        }
      )
    )
  }

  idx <- idx[!is.na(idx)]
  if (!length(idx)) {
    return(catalogue[FALSE, , drop = FALSE])
  }

  out <- catalogue[idx, , drop = FALSE]
  out <- out[!duplicated(out$station_id), , drop = FALSE]

  unavailable <- out$station_id[
    !.pe_supports_parameter(out$variable_code, parameter)
  ]
  if (length(unavailable)) {
    rlang::warn(
      paste0(
        "PE_SENAMHI: ", length(unavailable), " requested station(s) do not ",
        "provide '", parameter, "' and were skipped: ",
        paste(utils::head(unavailable, 10), collapse = ", "),
        if (length(unavailable) > 10) {
          paste0(" ... +", length(unavailable) - 10, " more")
        } else {
          ""
        }
      )
    )
  }

  out <- out[.pe_supports_parameter(out$variable_code, parameter), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# -- Daily time-series parsing -------------------------------------------------

.pe_daily_pairs <- function(x) {
  if (is.null(x) || !length(x)) {
    return(tibble::tibble(timestamp_ms = numeric(), value = numeric()))
  }

  timestamp_ms <- vapply(
    x,
    function(row) {
      if (is.null(row) || !length(row) || is.null(row[[1]])) {
        return(NA_real_)
      }
      suppressWarnings(as.numeric(row[[1]]))
    },
    numeric(1)
  )

  value <- vapply(
    x,
    function(row) {
      if (is.null(row) || length(row) < 2 || is.null(row[[2]])) {
        return(NA_real_)
      }
      suppressWarnings(as.numeric(row[[2]]))
    },
    numeric(1)
  )

  tibble::tibble(timestamp_ms = timestamp_ms, value = value)
}

.pe_parse_daily_timeseries <- function(
    data,
    station_id,
    parameter,
    source_url
) {
  if (is.null(data) || is.null(data$datoActual)) {
    return(.pe_empty_timeseries()[, c(
      "timestamp", "value", "unit", "quality_code", "quality_name",
      "quality_desc", "source_url"
    )])
  }

  if (!.pe_same_station(data$codEstacion, station_id)) {
    return(.pe_empty_timeseries()[, c(
      "timestamp", "value", "unit", "quality_code", "quality_name",
      "quality_desc", "source_url"
    )])
  }

  expected <- .pe_graph_parameter(parameter)
  returned <- toupper(.pe_scalar_chr(data$variable))
  if (!is.na(returned) && !identical(returned, expected)) {
    return(.pe_empty_timeseries()[, c(
      "timestamp", "value", "unit", "quality_code", "quality_name",
      "quality_desc", "source_url"
    )])
  }

  pairs <- .pe_daily_pairs(data$datoActual)
  keep <- (
    is.finite(pairs$timestamp_ms) &
      is.finite(pairs$value)
  )
  pairs <- pairs[keep, , drop = FALSE]

  if (!nrow(pairs)) {
    return(.pe_empty_timeseries()[, c(
      "timestamp", "value", "unit", "quality_code", "quality_name",
      "quality_desc", "source_url"
    )])
  }

  transform <- .pe_unit_transform(data$unidad, parameter)
  timestamp <- as.POSIXct(
    pairs$timestamp_ms / 1000,
    origin = "1970-01-01",
    tz = .pe_senamhi_tz
  )

  tibble::tibble(
    timestamp    = timestamp,
    value        = pairs$value * transform$multiplier,
    unit         = rep(transform$unit, nrow(pairs)),
    quality_code = rep(NA_character_, nrow(pairs)),
    quality_name = rep(NA_character_, nrow(pairs)),
    quality_desc = rep(NA_character_, nrow(pairs)),
    source_url   = rep(source_url, nrow(pairs))
  )
}

.pe_hydro_year_end <- function(date) {
  year <- as.integer(format(date, "%Y"))
  month <- as.integer(format(date, "%m"))
  ifelse(month >= 9L, year + 1L, year)
}

.pe_anchor_dates <- function(date_from, date_to) {
  first_year <- .pe_hydro_year_end(date_from)
  last_year <- .pe_hydro_year_end(date_to)
  years <- seq.int(first_year, last_year)
  dates <- as.Date(sprintf("%04d-08-31", years))
  dates[years == last_year & dates > date_to] <- date_to

  tibble::tibble(hydro_year_end = years, anchor_date = dates)
}

.pe_daily_request <- function(
    x,
    station_id,
    variable_code,
    parameter,
    anchor,
    perform
) {
  req <- .pe_graph_request(
    x,
    station_id = station_id,
    variable_code = variable_code,
    parameter = parameter,
    anchor = anchor,
    daily = TRUE
  )
  resp <- perform(req)
  .pe_check_response(resp, "daily time-series request")
  .pe_extract_graph_data(resp)
}

# -- Time series (S3 method) ---------------------------------------------------

#' @export
timeseries.hydro_service_PE_SENAMHI <- function(
    x,
    parameter = c("water_discharge", "water_level"),
    stations = NULL,
    start_date = NULL,
    end_date = NULL,
    mode = c("complete", "range"),
    ...
) {
  parameter <- match.arg(parameter)
  mode <- match.arg(mode)
  rng <- resolve_dates(mode, start_date, end_date)

  date_from <- .pe_date(rng$start_date)
  date_to <- .pe_date(rng$end_date)
  provider_first_date <- getOption(
    "hydrodownloadR.pe_senamhi_first_date",
    .pe_senamhi_first_date
  )
  provider_first_date <- as.Date(provider_first_date)
  if (length(provider_first_date) != 1L || is.na(provider_first_date)) {
    rlang::abort(
      paste0(
        "PE_SENAMHI: option 'hydrodownloadR.pe_senamhi_first_date' ",
        "must contain one valid date."
      )
    )
  }

  if (identical(mode, "complete")) {
    date_from <- provider_first_date
  } else if (!is.null(date_from) && date_from < provider_first_date) {
    rlang::warn(
      paste0(
        "PE_SENAMHI: start_date precedes the PHISIS monitoring archive; ",
        "using ", format(provider_first_date), "."
      )
    )
    date_from <- provider_first_date
  }

  today <- as.Date(Sys.time(), tz = .pe_senamhi_tz)
  if (is.null(date_to) || date_to > today) {
    date_to <- today
  }

  if (is.null(date_from)) date_from <- provider_first_date
  if (date_from > date_to) {
    rlang::abort("PE_SENAMHI: start_date must not be after end_date.")
  }

  perform <- .pe_limited_perform(x)
  catalogue <- .pe_station_catalogue(
    x,
    perform = perform,
    enrich_river = FALSE
  )
  targets <- .pe_resolve_stations(x, stations, catalogue, parameter)
  if (!nrow(targets)) {
    return(.pe_empty_timeseries())
  }

  estimated_requests <- nrow(targets) * nrow(.pe_anchor_dates(date_from, date_to))
  if (estimated_requests > 100) {
    rlang::inform(
      paste0(
        "PE_SENAMHI: up to ", estimated_requests,
        " rate-limited PHISIS request(s) are needed because the daily graph ",
        "serves one hydrological year at a time."
      )
    )
  }

  no_data <- new.env(parent = emptyenv())
  failed_requests <- new.env(parent = emptyenv())

  fetch_one <- function(i) {
    station_id <- targets$station_id[[i]]
    variable_code <- targets$variable_code[[i]]
    last_observation <- targets$last_observation[i]

    station_end <- date_to
    if (!is.na(last_observation)) {
      last_date <- as.Date(last_observation, tz = .pe_senamhi_tz)
      if (!is.na(last_date) && last_date < station_end) {
        station_end <- last_date
      }
    }

    if (station_end < date_from) {
      no_data[[station_id]] <- TRUE
      return(.pe_empty_timeseries())
    }

    anchors <- .pe_anchor_dates(date_from, station_end)
    chunks <- lapply(seq_len(nrow(anchors)), function(j) {
      anchor_date <- anchors$anchor_date[j]
      use_latest <- (
        !is.na(last_observation) &&
          identical(
            as.Date(last_observation, tz = .pe_senamhi_tz),
            anchor_date
          )
      )
      anchor <- if (use_latest) {
        .pe_graph_anchor(last_observation = last_observation)
      } else {
        .pe_graph_anchor(date = anchor_date)
      }

      source_url <- .pe_source_url(
        station_id,
        variable = .pe_graph_parameter(parameter),
        variable_option = variable_code,
        anchor = anchor
      )

      data <- tryCatch(
        .pe_daily_request(
          x,
          station_id = station_id,
          variable_code = variable_code,
          parameter = parameter,
          anchor = anchor,
          perform = perform
        ),
        error = function(e) {
          failed_requests[[paste(station_id, anchor, sep = "@")]] <- TRUE
          NULL
        }
      )

      if (is.null(data)) {
        return(.pe_empty_timeseries())
      }

      river <- .pe_scalar_chr(data$nomRio)
      if (!is.na(river)) {
        .pe_store_river(station_id, river)
      }

      parsed <- .pe_parse_daily_timeseries(
        data,
        station_id = station_id,
        parameter = parameter,
        source_url = source_url
      )
      if (!nrow(parsed)) {
        return(.pe_empty_timeseries())
      }

      observation_date <- as.Date(parsed$timestamp, tz = .pe_senamhi_tz)
      parsed[
        observation_date >= date_from & observation_date <= station_end,
        ,
        drop = FALSE
      ]
    })

    parsed <- dplyr::bind_rows(chunks)
    if (!nrow(parsed)) {
      no_data[[station_id]] <- TRUE
      return(.pe_empty_timeseries())
    }

    parsed <- parsed[order(parsed$timestamp), , drop = FALSE]
    parsed <- parsed[!duplicated(parsed$timestamp, fromLast = TRUE), , drop = FALSE]

    tibble::tibble(
      country       = x$country,
      provider_id   = x$provider_id,
      provider_name = x$provider_name,
      station_id    = station_id,
      parameter     = parameter,
      timestamp     = parsed$timestamp,
      value         = parsed$value,
      unit          = parsed$unit,
      quality_code  = parsed$quality_code,
      quality_name  = parsed$quality_name,
      quality_desc  = parsed$quality_desc,
      source_url    = parsed$source_url
    )
  }

  batches <- chunk_vec(seq_len(nrow(targets)), 10)
  pb <- progress::progress_bar$new(total = length(batches))
  result <- lapply(batches, function(batch) {
    pb$tick()
    dplyr::bind_rows(lapply(batch, fetch_one))
  })
  out <- dplyr::bind_rows(result)

  no_data_ids <- ls(no_data)
  if (length(no_data_ids)) {
    rlang::warn(
      paste0(
        "PE_SENAMHI: ", length(no_data_ids),
        " station(s) returned no data for '", parameter,
        "' in the requested period and were skipped: ",
        paste(utils::head(no_data_ids, 10), collapse = ", "),
        if (length(no_data_ids) > 10) {
          paste0(" ... +", length(no_data_ids) - 10, " more")
        } else {
          ""
        }
      )
    )
  }

  failed_ids <- ls(failed_requests)
  if (length(failed_ids)) {
    rlang::warn(
      paste0(
        "PE_SENAMHI: ", length(failed_ids),
        " PHISIS request(s) failed. Other successfully returned periods were ",
        "kept."
      )
    )
  }

  if (!nrow(out)) {
    return(.pe_empty_timeseries())
  }

  key <- paste(
    out$station_id,
    format(out$timestamp, "%Y-%m-%d %H:%M:%S", tz = .pe_senamhi_tz),
    sep = "@"
  )
  out <- out[!duplicated(key, fromLast = TRUE), , drop = FALSE]
  out <- out[order(out$station_id, out$timestamp), , drop = FALSE]
  rownames(out) <- NULL
  out
}
