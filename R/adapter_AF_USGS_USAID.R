# R/adapter_AF_USGS_USAID.R
#
# Historical Afghanistan hydrological observations published by
# USGS / USAID.
#
# The modern USGS monitoring-location API currently returns null
# geometries for these stations. Coordinates are therefore decoded
# from the 15-digit USGS station number:
#
#   DDMMSS DDDMMSS NN
#   latitude longitude sequence
#
# Daily discharge uses parameter code 30208 (m3/s).
# The modern Water Data API is tried first. Legacy NWIS is retained
# as a fallback because the historical observations may not yet be
# available through the modern daily-values endpoint.


# -------------------------------------------------------------------------
# Registration
# -------------------------------------------------------------------------

register_AF_USGS_USAID <- function() {
  register_service_usage(
    provider_id   = "AF_USGS_USAID",
    provider_name = "USGS / USAID Afghanistan",
    country       = "Afghanistan",
    base_url      = "https://api.waterdata.usgs.gov/ogcapi/v0/",
    rate_cfg      = list(n = 10, period = 1),
    auth          = list(
      type   = "header",
      header = "X-Api-Key"
    )
  )
}


# -------------------------------------------------------------------------
# Supported parameters
# -------------------------------------------------------------------------

#' @export
timeseries_parameters.hydro_service_AF_USGS_USAID <- function(x, ...) {
  "water_discharge"
}


.af_usgs_parameter <- function(parameter) {
  parameter <- match.arg(
    parameter,
    choices = timeseries_parameters.hydro_service_AF_USGS_USAID(NULL)
  )

  switch(
    parameter,
    water_discharge = list(
      modern_code = "30208",
      legacy_code = "30208",
      statistic   = "00003",
      parameter   = "water_discharge",
      unit        = "m3/s"
    )
  )
}


# -------------------------------------------------------------------------
# API and identifier helpers
# -------------------------------------------------------------------------

.af_usgs_pat <- function() {
  getOption(
    "API_USGS_PAT",
    Sys.getenv("API_USGS_PAT", unset = "")
  )
}


.af_usgs_with_key <- function(expr) {
  token <- .af_usgs_pat()

  if (
    nzchar(token) &&
    requireNamespace("httr", quietly = TRUE)
  ) {
    return(
      httr::with_config(
        httr::add_headers(
          "X-Api-Key" = token,
          "X-API-Key" = token
        ),
        force(expr)
      )
    )
  }

  force(expr)
}


.af_usgs_check_station_api <- function() {
  if (!requireNamespace("dataRetrieval", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg dataRetrieval} is required for {.val AF_USGS_USAID}."
    )
  }

  exports <- getNamespaceExports("dataRetrieval")

  if (!"read_waterdata_monitoring_location" %in% exports) {
    cli::cli_abort(c(
      "The installed {.pkg dataRetrieval} version is too old.",
      "i" = paste(
        "Update it with",
        "{.code install.packages('dataRetrieval')},",
        "restart R, and try again."
      )
    ))
  }

  if (
    !requireNamespace("httr2", quietly = TRUE) ||
    !"req_headers_redacted" %in% getNamespaceExports("httr2")
  ) {
    cli::cli_abort(c(
      "The installed {.pkg httr2} version is incompatible with",
      "the current {.pkg dataRetrieval} package.",
      "i" = paste(
        "Restart R and update it with",
        "{.code install.packages('httr2', type = 'binary')}."
      )
    ))
  }

  invisible(TRUE)
}


.af_usgs_has_modern_daily_api <- function() {
  requireNamespace("dataRetrieval", quietly = TRUE) &&
    "read_waterdata_daily" %in% getNamespaceExports("dataRetrieval")
}


.af_usgs_raw_ids <- function(x) {
  if (is.numeric(x)) {
    x <- sprintf("%.0f", x)
  } else {
    x <- as.character(x)
  }

  x <- trimws(x)
  x <- sub("^USAID[:-]", "", x, ignore.case = TRUE)
  x
}


.af_usgs_modern_ids <- function(x) {
  paste0("USAID-", .af_usgs_raw_ids(x))
}


.af_usgs_legacy_ids <- function(x) {
  paste0("USAID:", .af_usgs_raw_ids(x))
}


# -------------------------------------------------------------------------
# Coordinate reconstruction
# -------------------------------------------------------------------------

.af_usgs_coordinates_from_id <- function(station_number) {
  station_number <- .af_usgs_raw_ids(station_number)

  valid <- grepl("^[0-9]{15}$", station_number)

  lat <- rep(NA_real_, length(station_number))
  lon <- rep(NA_real_, length(station_number))

  if (any(valid)) {
    ids <- station_number[valid]

    lat_deg <- as.numeric(substr(ids, 1L, 2L))
    lat_min <- as.numeric(substr(ids, 3L, 4L))
    lat_sec <- as.numeric(substr(ids, 5L, 6L))

    lon_deg <- as.numeric(substr(ids, 7L, 9L))
    lon_min <- as.numeric(substr(ids, 10L, 11L))
    lon_sec <- as.numeric(substr(ids, 12L, 13L))

    lat_value <- lat_deg + lat_min / 60 + lat_sec / 3600
    lon_value <- lon_deg + lon_min / 60 + lon_sec / 3600

    coordinate_valid <-
      lat_deg <= 90 &
      lon_deg <= 180 &
      lat_min < 60 &
      lon_min < 60 &
      lat_sec < 60 &
      lon_sec < 60

    lat_value[!coordinate_valid] <- NA_real_
    lon_value[!coordinate_valid] <- NA_real_

    lat[valid] <- lat_value
    lon[valid] <- lon_value
  }

  tibble::tibble(
    lat = lat,
    lon = lon
  )
}


# -------------------------------------------------------------------------
# Metadata helpers
# -------------------------------------------------------------------------

.af_usgs_river_from_name <- function(station_name) {
  station_name <- as.character(station_name)

  river <- sub(
    ",?\\s*AFGHAN(?:ISTAN)?\\s*$",
    "",
    station_name,
    ignore.case = TRUE,
    perl = TRUE
  )

  river <- sub(
    paste0(
      "\\s+(?:AT|NEAR|NR\\.?|ABOVE|AB\\.?|BELOW|BL\\.?)",
      "\\s+.*$"
    ),
    "",
    river,
    ignore.case = TRUE,
    perl = TRUE
  )

  river <- sub(
    "\\s+R$",
    " RIVER",
    river,
    ignore.case = TRUE,
    perl = TRUE
  )

  river <- trimws(river)
  river[!nzchar(river)] <- NA_character_
  river
}


.af_usgs_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}


.af_usgs_character <- function(x) {
  if (is.list(x)) {
    return(
      vapply(
        x,
        function(value) {
          paste(as.character(value), collapse = ",")
        },
        character(1)
      )
    )
  }

  as.character(x)
}


.af_usgs_empty_stations <- function() {
  tibble::tibble(
    country       = character(),
    provider_id   = character(),
    provider_name = character(),
    station_id    = character(),
    org_id        = character(),
    station_name  = character(),
    river         = character(),
    lat           = double(),
    lon           = double(),
    area          = double(),
    altitude      = double()
  )
}


.af_usgs_empty_timeseries <- function() {
  tibble::tibble(
    country       = character(),
    provider_id   = character(),
    provider_name = character(),
    station_id    = character(),
    parameter     = character(),
    timestamp     = as.POSIXct(character(), tz = "UTC"),
    value         = double(),
    unit          = character(),
    quality_code  = character(),
    quality_name  = character(),
    quality_desc  = character(),
    source_url    = character()
  )
}


# -------------------------------------------------------------------------
# Station catalogue
# -------------------------------------------------------------------------

#' @export
stations.hydro_service_AF_USGS_USAID <- function(x, ...) {
  .af_usgs_check_station_api()

  raw <- tryCatch(
    .af_usgs_with_key(
      dataRetrieval::read_waterdata_monitoring_location(
        agency_code = "USAID",
        country_code = "AF",
        site_type = "Stream",
        properties = c(
          "monitoring_location_number",
          "monitoring_location_name",
          "agency_code",
          "country_code",
          "country_name",
          "site_type",
          "drainage_area",
          "altitude"
        ),
        skipGeometry = TRUE
      )
    ),
    error = function(e) {
      cli::cli_abort(
        c(
          "{.val AF_USGS_USAID}: station request failed.",
          "x" = conditionMessage(e)
        ),
        parent = e
      )
    }
  )

  if (is.null(raw) || nrow(raw) == 0L) {
    return(.af_usgs_empty_stations())
  }

  station_id <- .af_usgs_raw_ids(raw$monitoring_location_number)
  coordinates <- .af_usgs_coordinates_from_id(station_id)

  station_name <- as.character(raw$monitoring_location_name)

  # USGS monitoring-location metadata stores drainage area in square
  # miles and altitude in feet.
  area_km2 <- .af_usgs_numeric(raw$drainage_area) * 2.589988110336
  altitude_m <- .af_usgs_numeric(raw$altitude) * 0.3048

  result <- tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = station_id,
    org_id        = paste0("USAID-", station_id),
    station_name  = station_name,
    river         = .af_usgs_river_from_name(station_name),
    lat           = coordinates$lat,
    lon           = coordinates$lon,
    area          = area_km2,
    altitude      = altitude_m
  )

  missing_coordinates <- sum(
    is.na(result$lat) | is.na(result$lon)
  )

  if (missing_coordinates > 0L) {
    cli::cli_warn(
      paste0(
        "{missing_coordinates} Afghanistan station{?s} ",
        "{?has/have} no decodable USGS station coordinates."
      )
    )
  }

  result |>
    dplyr::filter(
      !is.na(.data$station_id),
      nzchar(.data$station_id)
    ) |>
    dplyr::distinct(.data$station_id, .keep_all = TRUE) |>
    dplyr::arrange(.data$station_id)
}


# -------------------------------------------------------------------------
# Modern daily-values request
# -------------------------------------------------------------------------

.af_usgs_modern_daily <- function(
    x,
    station_ids,
    start_date,
    end_date,
    parameter_info
) {
  if (!.af_usgs_has_modern_daily_api()) {
    return(NULL)
  }

  result <- try(
    .af_usgs_with_key(
      dataRetrieval::read_waterdata_daily(
        monitoring_location_id = .af_usgs_modern_ids(station_ids),
        parameter_code = parameter_info$modern_code,
        statistic_id = parameter_info$statistic,
        time = paste0(start_date, "/", end_date),
        properties = c(
          "monitoring_location_id",
          "time",
          "value",
          "approval_status",
          "qualifier"
        ),
        skipGeometry = TRUE,
        convertType = TRUE
      )
    ),
    silent = TRUE
  )

  if (
    inherits(result, "try-error") ||
    is.null(result) ||
    nrow(result) == 0L
  ) {
    return(NULL)
  }

  required <- c(
    "monitoring_location_id",
    "time",
    "value"
  )

  if (!all(required %in% names(result))) {
    return(NULL)
  }

  quality_code <- if ("approval_status" %in% names(result)) {
    .af_usgs_character(result$approval_status)
  } else {
    rep(NA_character_, nrow(result))
  }

  quality_desc <- if ("qualifier" %in% names(result)) {
    .af_usgs_character(result$qualifier)
  } else {
    rep(NA_character_, nrow(result))
  }

  values <- .af_usgs_numeric(result$value)
  values[values <= -999999] <- NA_real_

  tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = .af_usgs_raw_ids(
      result$monitoring_location_id
    ),
    parameter     = parameter_info$parameter,
    timestamp     = as.POSIXct(
      as.Date(result$time),
      tz = "UTC"
    ),
    value         = values,
    unit          = parameter_info$unit,
    quality_code  = quality_code,
    quality_name  = NA_character_,
    quality_desc  = quality_desc,
    source_url    = paste0(
      "https://api.waterdata.usgs.gov/ogcapi/v0/",
      "collections/daily/items"
    )
  )
}


# -------------------------------------------------------------------------
# Legacy NWIS fallback
# -------------------------------------------------------------------------

.af_usgs_legacy_value_column <- function(column_names) {
  exact <- c(
    "X_30208_00003",
    "X30208_00003",
    "30208_00003"
  )

  hit <- intersect(exact, column_names)

  if (length(hit) > 0L) {
    return(hit[[1L]])
  }

  hit <- grep(
    "30208.*00003",
    column_names,
    value = TRUE
  )

  hit <- hit[
    !grepl(
      "(_cd$|qual|remark)",
      hit,
      ignore.case = TRUE
    )
  ]

  if (length(hit) == 0L) {
    return(NA_character_)
  }

  hit[[1L]]
}


.af_usgs_legacy_quality_column <- function(
    column_names,
    value_column
) {
  expected <- paste0(value_column, "_cd")

  if (expected %in% column_names) {
    return(expected)
  }

  hit <- grep(
    "30208.*00003.*(_cd|qual)",
    column_names,
    value = TRUE,
    ignore.case = TRUE
  )

  if (length(hit) == 0L) {
    return(NA_character_)
  }

  hit[[1L]]
}


.af_usgs_legacy_daily <- function(
    x,
    station_ids,
    start_date,
    end_date,
    parameter_info
) {
  if (!requireNamespace("dataRetrieval", quietly = TRUE)) {
    return(NULL)
  }

  request <- function(ids) {
    try(
      .af_usgs_with_key(
        dataRetrieval::readNWISdv(
          siteNumbers = ids,
          parameterCd = parameter_info$legacy_code,
          startDate = start_date,
          endDate = end_date
        )
      ),
      silent = TRUE
    )
  }

  # The agency-qualified form is preferred. The unqualified form is
  # retained for compatibility with older dataRetrieval versions.
  result <- request(.af_usgs_legacy_ids(station_ids))

  if (
    inherits(result, "try-error") ||
    is.null(result) ||
    nrow(result) == 0L
  ) {
    result <- request(.af_usgs_raw_ids(station_ids))
  }

  if (
    inherits(result, "try-error") ||
    is.null(result) ||
    nrow(result) == 0L
  ) {
    return(NULL)
  }

  value_column <- .af_usgs_legacy_value_column(names(result))

  if (is.na(value_column)) {
    cli::cli_warn(
      paste0(
        "Legacy NWIS returned data, but parameter column ",
        "{.val 30208/00003} could not be identified."
      )
    )
    return(NULL)
  }

  date_column <- intersect(
    c("Date", "dateTime", "datetime", "time"),
    names(result)
  )

  if (length(date_column) == 0L) {
    return(NULL)
  }

  date_column <- date_column[[1L]]

  site_column <- intersect(
    c("site_no", "monitoring_location_number"),
    names(result)
  )

  if (length(site_column) > 0L) {
    result_station_id <- .af_usgs_raw_ids(
      result[[site_column[[1L]]]]
    )
  } else if (length(station_ids) == 1L) {
    result_station_id <- rep(
      .af_usgs_raw_ids(station_ids),
      nrow(result)
    )
  } else {
    return(NULL)
  }

  quality_column <- .af_usgs_legacy_quality_column(
    names(result),
    value_column
  )

  if (!is.na(quality_column)) {
    quality_code <- .af_usgs_character(
      result[[quality_column]]
    )
  } else {
    quality_code <- rep(NA_character_, nrow(result))
  }

  values <- .af_usgs_numeric(result[[value_column]])
  values[values <= -999999] <- NA_real_

  tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = result_station_id,
    parameter     = parameter_info$parameter,
    timestamp     = as.POSIXct(
      as.Date(result[[date_column]]),
      tz = "UTC"
    ),
    value         = values,
    unit          = parameter_info$unit,
    quality_code  = quality_code,
    quality_name  = NA_character_,
    quality_desc  = NA_character_,
    source_url    = "https://waterservices.usgs.gov/nwis/dv/"
  )
}


# -------------------------------------------------------------------------
# Public time-series method
# -------------------------------------------------------------------------

#' @export
timeseries.hydro_service_AF_USGS_USAID <- function(
    x,
    parameter = "water_discharge",
    stations = NULL,
    start_date = NULL,
    end_date = NULL,
    mode = c("range", "complete"),
    ...
) {
  mode <- match.arg(mode)
  parameter_info <- .af_usgs_parameter(parameter)

  dates <- resolve_dates(
    start_date = start_date,
    end_date = end_date,
    mode = mode
  )

  start_date <- as.character(dates$start_date)
  end_date <- as.character(dates$end_date)

  if (is.null(stations)) {
    station_inventory <- stations.hydro_service_AF_USGS_USAID(x)

    if (nrow(station_inventory) == 0L) {
      return(.af_usgs_empty_timeseries())
    }

    station_ids <- station_inventory$station_id
  } else {
    station_ids <- .af_usgs_raw_ids(stations)
  }

  station_ids <- unique(station_ids)
  station_ids <- station_ids[
    !is.na(station_ids) & nzchar(station_ids)
  ]

  if (length(station_ids) == 0L) {
    return(.af_usgs_empty_timeseries())
  }

  # Try the modern Water Data API first.
  result <- .af_usgs_modern_daily(
    x = x,
    station_ids = station_ids,
    start_date = start_date,
    end_date = end_date,
    parameter_info = parameter_info
  )

  # Historical Afghanistan daily values may not yet have been migrated
  # to the modern daily endpoint, so retain the NWIS fallback.
  if (is.null(result) || nrow(result) == 0L) {
    result <- .af_usgs_legacy_daily(
      x = x,
      station_ids = station_ids,
      start_date = start_date,
      end_date = end_date,
      parameter_info = parameter_info
    )
  }

  if (is.null(result) || nrow(result) == 0L) {
    return(.af_usgs_empty_timeseries())
  }

  result |>
    dplyr::filter(
      .data$timestamp >= as.POSIXct(start_date, tz = "UTC"),
      .data$timestamp <
        as.POSIXct(as.Date(end_date) + 1, tz = "UTC")
    ) |>
    dplyr::arrange(
      .data$station_id,
      .data$timestamp
    )
}
