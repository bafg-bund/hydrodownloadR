# ==== Switzerland (official BAFU NAWA Trend water quality API) ===============
# Base: https://data.bafu.admin.ch
# GraphQL endpoint: POST https://data.bafu.admin.ch/api
# Bulk files: https://data.bafu.admin.ch/download/water/nawa-trend/
#
# NAWA Trend contains discrete grab and composite water-quality samples. It is
# intentionally separate from CH_BAFU, which exposes continuous hydrological
# observations (Q, W and WT) from another GraphQL namespace and station list.
#
# Stations and the parameter catalogue use the GraphQL namespace:
#   water { nawa_trend { stations parameters data } }
# Observation retrieval uses BAFU's prepared annual CSV files so that all
# stations and analytes can be downloaded efficiently in one pass per year.
#
# API documentation:
# https://data.bafu.admin.ch/dataproduct-water-nawa-trend


# -- Registration -------------------------------------------------------------

#' @keywords internal
#' @noRd
register_CH_BAFU_NAWA <- function() {
  register_service_usage(
    provider_id   = "CH_BAFU_NAWA",
    provider_name = "Bundesamt f\u00FCr Umwelt (BAFU) NAWA Trend",
    country       = "Switzerland",
    base_url      = "https://data.bafu.admin.ch",
    geo_base_url  = NULL,
    # The BAFU limit is 500 requests per rolling five-minute window. Four
    # requests per three seconds leave a safety margin below that limit.
    rate_cfg      = list(n = 4, period = 3),
    auth          = list(type = "none")
  )
}


# -- Parameter grouping -------------------------------------------------------

# NAWA contains many individual analytes. Expose them through one consistent
# `water_quality` parameter and retain an English analyte name row-wise in
# `sub_parameter`. Water temperature remains a separate standard parameter.
# Discharge and water level records contained in the NAWA files belong to the
# continuous CH_BAFU adapter and are intentionally excluded here.
#' @keywords internal
#' @noRd
.ch_nawa_parameter_class <- function(measured_parameter) {
  value <- trimws(normalize_utf8(as.character(measured_parameter)))
  value_lower <- tolower(value)

  dplyr::case_when(
    is.na(value) | !nzchar(value) ~ NA_character_,
    grepl("^wassertemperatur(?:\\s|$)", value_lower, perl = TRUE) ~
      "water_temperature",
    grepl("^wasserstand(?:\\s|$)", value_lower, perl = TRUE) ~
      "water_level",
    grepl("^abfluss(?:\\s|$)", value_lower, perl = TRUE) ~
      "water_discharge",
    TRUE ~ "water_quality"
  )
}

# BAFU publishes German and French parameter names, but no English catalogue
# field. Translate the standard physicochemical and nutrient parameters that
# have unambiguous English names. Provider names for specialised chemical
# substances are retained rather than being changed by an unreliable automatic
# translation.
#' @keywords internal
#' @noRd
.ch_nawa_parameter_english <- function(measured_parameter) {
  value <- trimws(normalize_utf8(as.character(measured_parameter)))
  value[value %in% c("", "---")] <- NA_character_
  ae <- intToUtf8(0x00E4)
  ue <- intToUtf8(0x00FC)
  translations <- c(
    "Abfiltrierbare Stoffe" = "Total suspended solids",
    setNames("Alkalinity (pH 4.5)", paste0("Alkalinit", ae, "t pH4.5")),
    "Ammonium" = "Ammonium",
    "Ammonium-Stickstoff" = "Ammonium nitrogen",
    "Bromid" = "Bromide",
    "BSB5" = "Biochemical oxygen demand (BOD5)",
    "Calcium" = "Calcium",
    "Chlorid" = "Chloride",
    "CSB" = "Chemical oxygen demand (COD)",
    "DOC" = "Dissolved organic carbon (DOC)",
    setNames("Electrical conductivity",
             paste0("Elektrische Leitf", ae, "higkeit")),
    "Fluorid" = "Fluoride",
    setNames("Total hardness", paste0("Gesamth", ae, "rte")),
    "Gesamtphosphor (unfiltriert)" = "Total phosphorus (unfiltered)",
    "Gesamtstickstoff (unfiltriert)" = "Total nitrogen (unfiltered)",
    "Kalium" = "Potassium",
    setNames("Carbonate hardness", paste0("Karbonath", ae, "rte")),
    "Lufttemperatur" = "Air temperature",
    "Magnesium" = "Magnesium",
    "Natrium" = "Sodium",
    "Nitrat" = "Nitrate",
    "Nitrat-Stickstoff" = "Nitrate nitrogen",
    "Nitrit" = "Nitrite",
    "Nitrit-Stickstoff" = "Nitrite nitrogen",
    "ortho-Phosphat" = "Orthophosphate",
    "ortho-Phosphat-Phosphor (filtriert)" =
      "Orthophosphate phosphorus (filtered)",
    "Phosphat" = "Phosphate",
    "Phosphat-Phosphor" = "Phosphate phosphorus",
    "pH-Wert" = "pH",
    "Sauerstoff" = "Dissolved oxygen",
    setNames("Oxygen saturation",
             paste0("Sauerstoff-S", ae, "ttigung")),
    "Sulfat" = "Sulfate",
    "TOC" = "Total organic carbon (TOC)",
    setNames("Turbidity", paste0("Tr", ue, "bung")),
    "Wassertemperatur" = "Water temperature",
    "Aluminium" = "Aluminium",
    "Antimon" = "Antimony",
    "Arsen" = "Arsenic",
    "Barium" = "Barium",
    "Beryllium" = "Beryllium",
    "Blei" = "Lead",
    "Bor" = "Boron",
    "Cadmium" = "Cadmium",
    "Chrom" = "Chromium",
    "Cobalt" = "Cobalt",
    "Eisen" = "Iron",
    "Kupfer" = "Copper",
    "Mangan" = "Manganese",
    "Nickel" = "Nickel",
    "Quecksilber" = "Mercury",
    "Silber" = "Silver",
    "Zink" = "Zinc"
  )

  result <- unname(translations[value])

  # Translate common sample-treatment qualifiers when the underlying parameter
  # has a controlled English name.
  qualifier <- dplyr::case_when(
    grepl(" \\(gel\u00F6st\\)$", value) ~ "dissolved",
    grepl(" \\(filtriert\\)$", value) ~ "filtered",
    grepl(" \\(unfiltriert\\)$", value) ~ "unfiltered",
    TRUE ~ NA_character_
  )
  base_value <- sub(
    " \\((gel\u00F6st|filtriert|unfiltriert)\\)$",
    "",
    value
  )
  base_translation <- unname(translations[base_value])
  use_qualified <- is.na(result) & !is.na(base_translation) &
    !is.na(qualifier)
  result[use_qualified] <- paste0(
    base_translation[use_qualified],
    " (",
    qualifier[use_qualified],
    ")"
  )

  # Most specialist analyte names are chemical substance names. Preserve the
  # official spelling whenever no controlled translation is defined.
  use_provider_name <- is.na(result) & !is.na(value)
  result[use_provider_name] <- value[use_provider_name]
  result
}

#' @keywords internal
#' @noRd
.ch_nawa_field_or_lab_english <- function(x) {
  value <- trimws(normalize_utf8(as.character(x)))
  value[value %in% c("", "---")] <- NA_character_

  dplyr::case_when(
    is.na(value) ~ NA_character_,
    value == "Labor" ~ "laboratory",
    value == "Feld" ~ "field",
    value == "Berechnet" ~ "calculated",
    value == "Aus Zeitreihe ermitteln" ~ "derived_from_time_series",
    TRUE ~ value
  )
}

#' @keywords internal
#' @noRd
.ch_nawa_uncertainty_type_english <- function(x) {
  value <- trimws(normalize_utf8(as.character(x)))
  value[value %in% c("", "---")] <- NA_character_
  value_lower <- tolower(value)

  dplyr::case_when(
    is.na(value) ~ NA_character_,
    value_lower %in% c("absolut", "absolue", "absolute") ~ "absolute",
    value_lower %in% c("relativ", "relative") ~ "relative",
    TRUE ~ value
  )
}

#' @keywords internal
#' @noRd
.ch_nawa_device_method_english <- function(x) {
  value <- trimws(normalize_utf8(as.character(x)))
  value[value %in% c("", "---")] <- NA_character_

  dplyr::case_when(
    is.na(value) ~ NA_character_,
    value == "Spektrophotometrie" ~ "spectrophotometry",
    value == "Sonde" ~ "probe",
    value == "Titrimetrie" ~ "titrimetry",
    value == "Gravimetrie" ~ "gravimetry",
    value == "Infrarotspektroskopie" ~ "infrared spectroscopy",
    TRUE ~ value
  )
}

#' @export
timeseries_parameters.hydro_service_CH_BAFU_NAWA <- function(x, ...) {
  c("water_quality", "water_temperature")
}


# -- GraphQL helpers ----------------------------------------------------------

#' @keywords internal
#' @noRd
.ch_nawa_graphql_request <- function(
    x,
    query,
    variables = list(),
    throttle = FALSE) {

  # jsonlite serializes an empty R list as JSON `[]`. BAFU requires GraphQL
  # variables to be a JSON object and returns HTTP 400 for `variables: []`.
  # Omit the member entirely for variable-free queries such as stations() and
  # bafu_nawa_parameters().
  body <- list(query = query)
  if (length(variables)) body$variables <- variables

  # Serialize explicitly so variable-free requests contain no accidental
  # `variables: []` member with older httr2/jsonlite combinations.
  payload <- jsonlite::toJSON(
    body,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )

  req <- build_request(
    x,
    path = "api"
  ) |>
    httr2::req_body_raw(
      charToRaw(enc2utf8(payload)),
      type = "application/json"
    ) |>
    httr2::req_error(
      is_error = function(resp) FALSE
    )

  if (isTRUE(throttle)) {
    req <- httr2::req_throttle(
      req,
      capacity = x$rate_cfg$n,
      fill_time_s = x$rate_cfg$period,
      # Share the throttle realm with the main CH_BAFU adapter because BAFU
      # applies its request limit per client IP, not per dataset.
      realm = "CH_BAFU"
    )
  }

  req
}

#' @keywords internal
#' @noRd
.ch_nawa_graphql_response <- function(resp) {
  status <- httr2::resp_status(resp)

  if (status >= 400L) {
    detail <- tryCatch(
      httr2::resp_body_string(resp),
      error = function(e) ""
    )
    detail <- trimws(gsub("[\\r\\n]+", " ", detail))
    if (!nzchar(detail)) detail <- "No response body returned."
    if (nchar(detail) > 1500L) {
      detail <- paste0(substr(detail, 1L, 1500L), "...")
    }

    rlang::abort(paste0(
      "CH_BAFU_NAWA GraphQL request failed with HTTP ",
      status,
      ": ",
      detail
    ))
  }

  dat <- httr2::resp_body_json(resp, simplifyVector = FALSE)

  if (!is.null(dat$errors) && length(dat$errors)) {
    messages <- vapply(
      dat$errors,
      function(z) as.character(z$message %||% "Unknown GraphQL error"),
      character(1)
    )
    rlang::abort(paste0(
      "CH_BAFU_NAWA GraphQL request failed: ",
      paste(unique(messages), collapse = "; ")
    ))
  }

  payload <- dat$data$water$nawa_trend
  if (is.null(payload)) {
    rlang::abort("CH_BAFU_NAWA returned no NAWA Trend payload.")
  }

  payload
}

#' @keywords internal
#' @noRd
.ch_nawa_graphql <- function(x, query, variables = list()) {
  req <- .ch_nawa_graphql_request(
    x,
    query = query,
    variables = variables
  )

  .ch_nawa_graphql_response(perform_request(req))
}

#' @keywords internal
#' @noRd
.ch_nawa_pluck_chr <- function(rows, field) {
  vapply(
    rows,
    function(z) {
      value <- z[[field]]
      if (is.null(value) || !length(value)) return(NA_character_)
      as.character(value[[1]])
    },
    character(1)
  )
}

#' @keywords internal
#' @noRd
.ch_nawa_parse_number <- function(x) {
  x <- trimws(as.character(x))

  vapply(
    x,
    function(value) {
      if (is.na(value) || !nzchar(value)) return(NA_real_)

      match <- regexpr(
        "[+-]?(?:[0-9]+(?:[.,][0-9]*)?|[.,][0-9]+)(?:[eE][+-]?[0-9]+)?",
        value,
        perl = TRUE
      )
      if (match[[1]] < 0L) return(NA_real_)

      token <- regmatches(value, match)
      suppressWarnings(as.numeric(sub(",", ".", token, fixed = TRUE)))
    },
    numeric(1)
  )
}

#' @keywords internal
#' @noRd
.ch_nawa_value_qualifier <- function(x) {
  x <- trimws(as.character(x))

  dplyr::case_when(
    grepl("^<=", x) ~ "<=",
    grepl("^>=", x) ~ ">=",
    grepl("^<",  x) ~ "<",
    grepl("^>",  x) ~ ">",
    grepl("^(n\\.?n\\.?|nicht nachweisbar|not detected)", x,
          ignore.case = TRUE) ~ "not_detected",
    TRUE ~ NA_character_
  )
}

#' @keywords internal
#' @noRd
.ch_nawa_normalize_unit <- function(unit, measured_parameter) {
  unit <- trimws(normalize_utf8(as.character(unit)))
  measured_parameter <- trimws(
    normalize_utf8(as.character(measured_parameter))
  )

  dplyr::case_when(
    measured_parameter == "pH-Wert" &
      (is.na(unit) | unit == "" | unit == "---") ~ "pH",
    is.na(unit) | unit == "" | unit == "---" ~ NA_character_,
    unit == "\u00B0C" ~ "degC",
    unit == "\u00B5g/l" ~ "ug/l",
    unit == "\u00B5S/cm" ~ "uS/cm",
    unit == "\u00B5S20" ~ "uS20",
    unit == "m\u00B3/s" ~ "m^3/s",
    TRUE ~ unit
  )
}

#' @keywords internal
#' @noRd
.ch_nawa_cache_dir <- function() {
  base <- tryCatch(
    tools::R_user_dir("hydrodownloadR", which = "cache"),
    error = function(e) file.path(tempdir(), "hydrodownloadR_cache")
  )
  file.path(base, "CH_BAFU_NAWA")
}

# Extract simple values from the S3-compatible XML file listing without adding
# an XML package dependency. BAFU object keys contain no nested markup.
#' @keywords internal
#' @noRd
.ch_nawa_xml_values <- function(xml, tag) {
  pattern <- paste0("<", tag, ">([^<]*)</", tag, ">")
  hits <- regmatches(xml, gregexpr(pattern, xml, perl = TRUE))[[1]]
  if (!length(hits)) return(character())

  # Extract the captured text directly. Using sub() only to remove the opening
  # tag leaves the closing tag behind, so version prefixes no longer match the
  # strict `water/nawa-trend/vYYYY-MM-DD/` pattern below.
  values <- sub(pattern, "\\1", hits, perl = TRUE)

  values <- gsub("&amp;", "&", values, fixed = TRUE)
  values <- gsub("&lt;", "<", values, fixed = TRUE)
  values <- gsub("&gt;", ">", values, fixed = TRUE)
  values
}

#' @keywords internal
#' @noRd
.ch_nawa_s3_list <- function(x, prefix, delimiter = NULL) {
  query <- list(
    `list-type` = "2",
    prefix = prefix
  )
  if (!is.null(delimiter)) query$delimiter <- delimiter

  req <- build_request(
    x,
    path = "download",
    query = query
  ) |>
    httr2::req_timeout(seconds = 60)

  httr2::resp_body_string(perform_request(req))
}

# Discover the newest published NAWA version and its annual bulk files. This
# prevents a package update whenever BAFU publishes a new snapshot.
#' @keywords internal
#' @noRd
.ch_nawa_bulk_index <- function(x) {
  root_prefix <- "water/nawa-trend/"
  root_xml <- .ch_nawa_s3_list(
    x,
    prefix = root_prefix,
    delimiter = "/"
  )

  version_prefixes <- unique(.ch_nawa_xml_values(root_xml, "Prefix"))
  version_prefixes <- version_prefixes[grepl(
    "^water/nawa-trend/v[0-9]{4}-[0-9]{2}-[0-9]{2}/$",
    version_prefixes
  )]

  if (!length(version_prefixes)) {
    rlang::abort("CH_BAFU_NAWA could not discover a published data version.")
  }

  version_prefix <- sort(version_prefixes, decreasing = TRUE)[[1]]
  version <- basename(sub("/$", "", version_prefix))
  file_xml <- .ch_nawa_s3_list(x, prefix = version_prefix)
  keys <- unique(.ch_nawa_xml_values(file_xml, "Key"))

  data_keys <- keys[grepl(
    "/data/water_nawa-trend_data_[0-9]{4}\\.csv(?:\\.gz)?$",
    keys,
    perl = TRUE
  )]

  years <- suppressWarnings(as.integer(sub(
    "^.*_([0-9]{4})\\.csv(?:\\.gz)?$",
    "\\1",
    data_keys,
    perl = TRUE
  )))

  annual_files <- tibble::tibble(
    year = years,
    key = data_keys,
    compressed = grepl("\\.gz$", data_keys)
  ) |>
    dplyr::filter(!is.na(.data$year)) |>
    dplyr::arrange(.data$year, dplyr::desc(.data$compressed)) |>
    dplyr::distinct(.data$year, .keep_all = TRUE) |>
    dplyr::select(.data$year, .data$key)

  if (!nrow(annual_files)) {
    rlang::abort(paste0(
      "CH_BAFU_NAWA found version ", version,
      " but no annual observation files."
    ))
  }

  list(
    version = version,
    version_prefix = version_prefix,
    annual_files = annual_files
  )
}

#' @keywords internal
#' @noRd
.ch_nawa_download_bulk_file <- function(
    x,
    key,
    cache_dir,
    update = FALSE) {

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  destination <- file.path(cache_dir, basename(key))

  if (!isTRUE(update) && file.exists(destination) &&
      isTRUE(file.info(destination)$size > 0)) {
    return(destination)
  }

  url <- paste0(
    sub("/$", "", x$base_url),
    "/download/",
    key
  )
  temporary <- tempfile(
    pattern = paste0(basename(key), "_"),
    tmpdir = cache_dir,
    fileext = ".part"
  )

  cli::cli_inform("Downloading BAFU NAWA file: {basename(key)}")

  req <- httr2::request(url) |>
    httr2::req_user_agent(
      "hydrodownloadR (+https://github.com/bafg-bund/hydrodownloadR)"
    ) |>
    httr2::req_timeout(seconds = 600) |>
    httr2::req_retry(max_tries = 4)

  tryCatch(
    httr2::req_perform(req, path = temporary),
    error = function(e) {
      if (file.exists(temporary)) unlink(temporary)
      stop(e)
    }
  )

  copied <- file.copy(temporary, destination, overwrite = TRUE)
  unlink(temporary)
  if (!isTRUE(copied)) {
    rlang::abort(paste0(
      "CH_BAFU_NAWA could not store downloaded file: ",
      destination
    ))
  }

  destination
}

# Official German column labels in the prepared annual CSV files.
#' @keywords internal
#' @noRd
.ch_nawa_bulk_columns <- function() {
  c(
    station_id = "Messstelle ID",
    station_name = "Messstelle Name",
    naqua_sampling_date = "NAQUA Probenahme Datum",
    naqua_sampling_time = "NAQUA Probenahme Uhrzeit",
    sampling_start_raw = "NAWA Probenahme Beginn (Datum und Uhrzeit)",
    sampling_end_raw = "NAWA Probenahme Ende (Datum und Uhrzeit)",
    sampling_duration_raw = "NAWA Probenahme Dauer (Stunden)",
    field_or_lab = "Erhebung Feld/Labor",
    measured_parameter = "Parameter",
    value_raw = "Messwert",
    determination_limit_raw = "Bestimmungsgrenze",
    nawa_detection_limit_raw = "NAWA Nachweisgrenze",
    unit_raw = "Einheit",
    measurement_uncertainty_type = "Messunsicherheit absolut/relativ",
    measurement_uncertainty_raw = "Messunsicherheit",
    device_method = "Ger\u00E4t/Methode"
  )
}

#' @keywords internal
#' @noRd
.ch_nawa_read_bulk_file <- function(path) {
  raw <- suppressMessages(readr::read_delim(
    file = path,
    delim = ";",
    col_types = readr::cols(.default = readr::col_character()),
    locale = readr::locale(encoding = "UTF-8"),
    trim_ws = TRUE,
    na = c("", "NA"),
    name_repair = "minimal",
    progress = FALSE,
    show_col_types = FALSE
  ))

  if (ncol(raw)) {
    names(raw)[[1]] <- sub("^\ufeff", "", names(raw)[[1]])
  }

  columns <- .ch_nawa_bulk_columns()
  missing <- setdiff(unname(columns), names(raw))
  if (length(missing)) {
    rlang::abort(paste0(
      "CH_BAFU_NAWA annual file is missing expected column(s): ",
      paste(missing, collapse = ", ")
    ))
  }

  raw <- raw[, unname(columns), drop = FALSE]
  names(raw) <- names(columns)
  tibble::as_tibble(raw)
}

#' @keywords internal
#' @noRd
.ch_nawa_parse_datetime <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "---")] <- NA_character_

  # The prepared BAFU files normally use `dd.mm.yyyy HH:MM`. Parse explicit
  # formats with base R instead of lubridate::parse_date_time(): the latter
  # builds a dynamic PCRE expression which fails with some Windows R/PCRE
  # combinations when an ISO order containing `T` is included.
  out <- as.POSIXct(
    rep(NA_real_, length(x)),
    origin = "1970-01-01",
    tz = "UTC"
  )
  if (!length(x)) return(out)

  # Retain harmless fallbacks for ISO-style values without asking the parser
  # to interpret `T` as a format token.
  normalized <- sub("T", " ", x, fixed = TRUE)
  normalized <- sub("Z$", "", normalized)

  formats <- c(
    "%d.%m.%Y %H:%M:%OS",
    "%d.%m.%Y %H:%M",
    "%d.%m.%Y",
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%d %H:%M",
    "%Y-%m-%d"
  )

  for (format in formats) {
    pending <- is.na(out) & !is.na(normalized)
    if (!any(pending)) break

    parsed <- suppressWarnings(as.POSIXct(
      normalized[pending],
      format = format,
      tz = "UTC"
    ))
    out[pending] <- parsed
  }

  out
}

#' @keywords internal
#' @noRd
.ch_nawa_naqua_datetime <- function(date, time) {
  date <- trimws(as.character(date))
  time <- trimws(as.character(time))
  date[date %in% c("", "---")] <- NA_character_
  time[time %in% c("", "---") | is.na(time)] <- "00:00"

  .ch_nawa_parse_datetime(ifelse(
    is.na(date),
    NA_character_,
    paste(date, time)
  ))
}

#' @keywords internal
#' @noRd
.ch_nawa_empty_result <- function() {
  tibble::tibble(
    country = character(),
    provider_id = character(),
    provider_name = character(),
    station_id = character(),
    parameter = character(),
    sub_parameter = character(),
    timestamp = as.POSIXct(character(), tz = "UTC"),
    value = character(),
    unit = character(),
    quality_code = character(),
    quality_name = character(),
    quality_desc = character(),
    source_url = character(),
    source_file = character(),
    determination_limit = numeric(),
    nawa_detection_limit = numeric(),
    sampling_start = as.POSIXct(character(), tz = "UTC"),
    timestamp_source = character(),
    sampling_duration_hours = numeric(),
    field_or_lab = character(),
    measurement_uncertainty_type = character(),
    measurement_uncertainty = numeric(),
    device_method = character()
  )
}


# -- Stations (S3 method) -----------------------------------------------------

#' @export
stations.hydro_service_CH_BAFU_NAWA <- function(x, ...) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    rlang::abort(c(
      "CH_BAFU_NAWA requires package 'sf' to transform LV95 coordinates.",
      i = "Install it with install.packages('sf')."
    ))
  }

  query <- paste(
    "query Stations {",
    "  water {",
    "    nawa_trend {",
    "      stations(limit: 10000) {",
    "        id",
    "        name",
    "        waterBodyName",
    "        canton",
    "        xCoordinateLv95",
    "        yCoordinateLv95",
    "        predecessor",
    "        successor",
    "      }",
    "    }",
    "  }",
    "}",
    sep = "\n"
  )

  limited <- ratelimitr::limit_rate(
    function() .ch_nawa_graphql(x, query = query)$stations,
    rate = ratelimitr::rate(
      n = x$rate_cfg$n,
      period = x$rate_cfg$period
    )
  )

  rows <- limited()
  if (is.null(rows) || !length(rows)) return(tibble::tibble())

  station_id   <- .ch_nawa_pluck_chr(rows, "id")
  station_name <- normalize_utf8(.ch_nawa_pluck_chr(rows, "name"))
  river_name   <- normalize_utf8(.ch_nawa_pluck_chr(rows, "waterBodyName"))
  x_lv95 <- suppressWarnings(as.numeric(
    .ch_nawa_pluck_chr(rows, "xCoordinateLv95")
  ))
  y_lv95 <- suppressWarnings(as.numeric(
    .ch_nawa_pluck_chr(rows, "yCoordinateLv95")
  ))

  lon <- rep(NA_real_, length(rows))
  lat <- rep(NA_real_, length(rows))
  valid_xy <- is.finite(x_lv95) & is.finite(y_lv95)

  if (any(valid_xy)) {
    points <- data.frame(
      row_id = which(valid_xy),
      x = x_lv95[valid_xy],
      y = y_lv95[valid_xy]
    ) |>
      sf::st_as_sf(
        coords = c("x", "y"),
        crs = 2056,
        remove = FALSE
      ) |>
      sf::st_transform(4326)

    xy <- sf::st_coordinates(points)
    lon[valid_xy] <- xy[, 1]
    lat[valid_xy] <- xy[, 2]
  }

  tibble::tibble(
    country            = x$country,
    provider_id        = x$provider_id,
    provider_name      = x$provider_name,
    station_id         = station_id,
    station_name       = station_name,
    station_name_ascii = to_ascii(station_name),
    river              = river_name,
    river_ascii        = to_ascii(river_name),
    lat                = lat,
    lon                = lon,
    area               = NA_real_,
    altitude           = NA_real_
  ) |>
    dplyr::filter(!is.na(.data$station_id), nzchar(.data$station_id)) |>
    dplyr::distinct(.data$station_id, .keep_all = TRUE) |>
    dplyr::arrange(.data$station_id)
}


# -- Full provider parameter catalogue ---------------------------------------

#' Retrieve the complete BAFU NAWA Trend parameter catalogue
#'
#' The catalogue includes all physicochemical parameters, nutrients,
#' micropollutants and the few hydrological parameters stored in NAWA. The
#' `parameter` column shows how each provider parameter is handled by
#' `timeseries()`. All entries classified as `water_quality` are returned
#' together when `parameter = "water_quality"` is requested.
#'
#' @param x A `CH_BAFU_NAWA` hydro service.
#' @param ... Unused.
#' @return A tibble containing the provider's parameter catalogue.
#' @export
bafu_nawa_parameters <- function(x, ...) {
  if (!inherits(x, "hydro_service_CH_BAFU_NAWA")) {
    rlang::abort("bafu_nawa_parameters() requires hydro_service('CH_BAFU_NAWA').")
  }

  query <- paste(
    "query Parameters {",
    "  water {",
    "    nawa_trend {",
    "      parameters(limit: 10000) {",
    "        parameterId",
    "        germanDesignation",
    "        frenchDesignation",
    "        parameterGroup",
    "        casI",
    "        casIi",
    "        inchikey",
    "        molarMassGMol",
    "      }",
    "    }",
    "  }",
    "}",
    sep = "\n"
  )

  rows <- .ch_nawa_graphql(x, query = query)$parameters
  if (is.null(rows) || !length(rows)) return(tibble::tibble())

  tibble::tibble(
    # The data filter uses measuredParameter, whose values are the German
    # designations, not the catalogue's compact parameterId values (for
    # example, "Elektrische Leitfaehigkeit" rather than "el_Lf").
    provider_parameter = normalize_utf8(
      .ch_nawa_pluck_chr(rows, "germanDesignation")
    ),
    parameter_id = normalize_utf8(
      .ch_nawa_pluck_chr(rows, "parameterId")
    ),
    name_de = normalize_utf8(
      .ch_nawa_pluck_chr(rows, "germanDesignation")
    ),
    name_fr = normalize_utf8(
      .ch_nawa_pluck_chr(rows, "frenchDesignation")
    ),
    parameter_group = normalize_utf8(
      .ch_nawa_pluck_chr(rows, "parameterGroup")
    ),
    cas_1 = .ch_nawa_pluck_chr(rows, "casI"),
    cas_2 = .ch_nawa_pluck_chr(rows, "casIi"),
    inchikey = .ch_nawa_pluck_chr(rows, "inchikey"),
    molar_mass_g_mol = .ch_nawa_parse_number(
      .ch_nawa_pluck_chr(rows, "molarMassGMol")
    )
  ) |>
    dplyr::filter(
      !is.na(.data$provider_parameter),
      nzchar(.data$provider_parameter)
    ) |>
    dplyr::mutate(
      parameter = .ch_nawa_parameter_class(.data$provider_parameter),
      sub_parameter = dplyr::if_else(
        .data$parameter == "water_quality",
        .ch_nawa_parameter_english(.data$provider_parameter),
        NA_character_
      )
    ) |>
    dplyr::relocate(
      dplyr::all_of(c("parameter", "sub_parameter")),
      .after = dplyr::all_of("provider_parameter")
    ) |>
    dplyr::distinct(.data$provider_parameter, .keep_all = TRUE) |>
    dplyr::arrange(.data$provider_parameter)
}


# -- Time-series parsing ------------------------------------------------------

#' @keywords internal
#' @noRd
.ch_nawa_clean_text <- function(x) {
  x <- trimws(normalize_utf8(as.character(x)))
  x[x %in% c("", "---")] <- NA_character_
  x
}

#' @keywords internal
#' @noRd
.ch_nawa_parse_bulk_rows <- function(rows, parameter) {
  if (is.null(rows) || !nrow(rows)) return(tibble::tibble())

  parameter_group <- match.arg(
    parameter,
    choices = c("water_quality", "water_temperature")
  )
  measured_parameter <- .ch_nawa_clean_text(rows$measured_parameter)
  sub_parameter_values <- if (identical(parameter_group, "water_quality")) {
    .ch_nawa_parameter_english(measured_parameter)
  } else {
    rep(NA_character_, length(measured_parameter))
  }
  sampling_start <- .ch_nawa_parse_datetime(rows$sampling_start_raw)
  sampling_end <- .ch_nawa_parse_datetime(rows$sampling_end_raw)
  naqua_timestamp <- .ch_nawa_naqua_datetime(
    rows$naqua_sampling_date,
    rows$naqua_sampling_time
  )

  # Grab samples commonly have only an end timestamp. NAQUA date/time is a
  # final fallback for any record lacking both NAWA timestamps.
  timestamp <- sampling_start
  use_end <- is.na(timestamp) & !is.na(sampling_end)
  timestamp[use_end] <- sampling_end[use_end]
  use_naqua <- is.na(timestamp) & !is.na(naqua_timestamp)
  timestamp[use_naqua] <- naqua_timestamp[use_naqua]

  timestamp_source <- dplyr::case_when(
    !is.na(sampling_start) ~ "sampling_start",
    !is.na(sampling_end) ~ "sampling_end",
    !is.na(naqua_timestamp) ~ "naqua_date_time",
    TRUE ~ NA_character_
  )

  value_raw <- .ch_nawa_clean_text(rows$value_raw)
  qualifier <- .ch_nawa_value_qualifier(value_raw)
  determination_limit <- .ch_nawa_parse_number(
    rows$determination_limit_raw
  )
  nawa_detection_limit <- .ch_nawa_parse_number(
    rows$nawa_detection_limit_raw
  )
  numeric_value <- .ch_nawa_parse_number(value_raw)
  censored <- qualifier %in% c("<", "<=", ">", ">=", "not_detected")

  quality_code <- dplyr::case_when(
    qualifier %in% c("<", "<=") & !is.na(determination_limit) ~
      "below_determination_limit",
    qualifier %in% c("<", "<=") & !is.na(nawa_detection_limit) ~
      "below_detection_limit",
    qualifier %in% c("<", "<=") ~ "left_censored",
    qualifier %in% c(">", ">=") ~ "right_censored",
    qualifier == "not_detected" ~ "not_detected",
    is.na(value_raw) ~ "missing",
    is.na(numeric_value) ~ "non_numeric",
    TRUE ~ NA_character_
  )

  quality_name <- dplyr::case_when(
    censored ~ "censored",
    quality_code == "missing" ~ "missing",
    quality_code == "non_numeric" ~ "non_numeric",
    TRUE ~ NA_character_
  )

  # This is a compact English interpretation of the reported value syntax.
  # BAFU's free-text `Bemerkung Messwert` field is not equivalent to this
  # description and is intentionally not exposed in the simplified result.
  quality_desc <- dplyr::case_when(
    quality_code == "below_determination_limit" ~
      "Reported below the analytical determination limit",
    quality_code == "below_detection_limit" ~
      "Reported below the analytical detection limit",
    quality_code == "left_censored" ~
      "Reported as a left-censored value",
    quality_code == "right_censored" ~
      "Reported as a right-censored value",
    quality_code == "not_detected" ~ "Reported as not detected",
    quality_code == "missing" ~ "No measurement value reported",
    quality_code == "non_numeric" ~
      "Provider value could not be parsed as numeric",
    TRUE ~ NA_character_
  )

  tibble::tibble(
    station_id = .ch_nawa_clean_text(rows$station_id),
    parameter = parameter_group,
    sub_parameter = sub_parameter_values,
    timestamp = timestamp,
    # Preserve provider strings such as `<0.008`; converting these values to a
    # plain number would discard the analytical qualifier.
    value = value_raw,
    unit = .ch_nawa_normalize_unit(
      rows$unit_raw,
      measured_parameter = measured_parameter
    ),
    quality_code = quality_code,
    quality_name = quality_name,
    quality_desc = quality_desc,
    determination_limit = determination_limit,
    nawa_detection_limit = nawa_detection_limit,
    sampling_start = sampling_start,
    timestamp_source = timestamp_source,
    sampling_duration_hours = .ch_nawa_parse_number(
      rows$sampling_duration_raw
    ),
    field_or_lab = .ch_nawa_field_or_lab_english(rows$field_or_lab),
    measurement_uncertainty_type = .ch_nawa_uncertainty_type_english(
      rows$measurement_uncertainty_type
    ),
    measurement_uncertainty = .ch_nawa_parse_number(
      rows$measurement_uncertainty_raw
    ),
    device_method = .ch_nawa_device_method_english(rows$device_method)
  )
}


# -- Time series (S3 method) --------------------------------------------------

#' @export
timeseries.hydro_service_CH_BAFU_NAWA <- function(
    x,
    parameter = c("water_quality", "water_temperature"),
    stations = NULL,
    start_date = NULL,
    end_date = NULL,
    mode = c("complete", "range"),
    exclude_quality = NULL,
    cache_dir = .ch_nawa_cache_dir(),
    update = FALSE,
    ...) {

  stopifnot(inherits(x, "hydro_service"))
  parameter <- match.arg(parameter)
  mode <- match.arg(mode)
  rng <- resolve_dates(mode, start_date, end_date)

  dots <- rlang::list2(...)
  if ("provider_parameter" %in% names(dots)) {
    cli::cli_warn(c(
      "!" = "`provider_parameter` is no longer used by CH_BAFU_NAWA.",
      "i" = paste0(
        "Use `parameter = \"water_quality\"`; ",
        "all water-quality analytes are returned together."
      )
    ))
  }

  if (is.na(rng$start_date) || is.na(rng$end_date) ||
      rng$start_date > rng$end_date) {
    rlang::abort("CH_BAFU_NAWA requires a valid start_date <= end_date.")
  }

  station_ids <- NULL
  if (!is.null(stations)) {
    station_ids <- unique(trimws(as.character(stations)))
    station_ids <- station_ids[
      !is.na(station_ids) & nzchar(station_ids)
    ]
    if (!length(station_ids)) return(.ch_nawa_empty_result())
  }

  index <- .ch_nawa_bulk_index(x)
  requested_years <- seq.int(
    as.integer(format(rng$start_date, "%Y")),
    as.integer(format(rng$end_date, "%Y"))
  )
  files <- index$annual_files |>
    dplyr::filter(.data$year %in% requested_years)

  if (!nrow(files)) return(.ch_nawa_empty_result())

  version_cache <- file.path(cache_dir, index$version)
  out <- vector("list", nrow(files))
  source_files <- character(nrow(files))
  pb <- progress::progress_bar$new(
    total = nrow(files),
    clear = FALSE
  )

  for (i in seq_len(nrow(files))) {
    key <- files$key[[i]]
    path <- .ch_nawa_download_bulk_file(
      x,
      key = key,
      cache_dir = version_cache,
      update = update
    )

    raw <- .ch_nawa_read_bulk_file(path)
    measured_parameter <- .ch_nawa_clean_text(raw$measured_parameter)
    parameter_class <- .ch_nawa_parameter_class(measured_parameter)
    keep <- parameter_class == parameter

    if (!is.null(station_ids)) {
      keep <- keep & trimws(as.character(raw$station_id)) %in% station_ids
    }

    raw <- raw[!is.na(keep) & keep, , drop = FALSE]
    out[[i]] <- .ch_nawa_parse_bulk_rows(raw, parameter = parameter)
    source_files[[i]] <- paste0(
      sub("/$", "", x$base_url),
      "/download/",
      key
    )
    if (nrow(out[[i]])) {
      out[[i]]$source_file <- source_files[[i]]
    }

    pb$tick()
  }

  res <- dplyr::bind_rows(out)
  if (!nrow(res)) return(.ch_nawa_empty_result())

  res <- res |>
    dplyr::filter(
      !is.na(.data$station_id),
      !is.na(.data$timestamp),
      as.Date(.data$timestamp, tz = "UTC") >= rng$start_date,
      as.Date(.data$timestamp, tz = "UTC") <= rng$end_date
    )

  if (!is.null(exclude_quality)) {
    excluded <- as.character(exclude_quality)
    res <- res |>
      dplyr::filter(
        is.na(.data$quality_code) |
          !(.data$quality_code %in% excluded)
      )
  }
  if (!nrow(res)) return(.ch_nawa_empty_result())

  res <- res |>
    dplyr::mutate(
      country = x$country,
      provider_id = x$provider_id,
      provider_name = x$provider_name,
      source_url = paste0(
        x$base_url,
        "/dataproduct-water-nawa-trend"
      )
    ) |>
    dplyr::arrange(
      .data$station_id,
      .data$timestamp,
      .data$sub_parameter
    ) |>
    dplyr::select(
      country,
      provider_id,
      provider_name,
      station_id,
      parameter,
      sub_parameter,
      timestamp,
      value,
      unit,
      quality_code,
      quality_name,
      quality_desc,
      source_url,
      source_file,
      determination_limit,
      nawa_detection_limit,
      sampling_start,
      timestamp_source,
      sampling_duration_hours,
      field_or_lab,
      measurement_uncertainty_type,
      measurement_uncertainty,
      device_method
    )

  attr(res, "bafu_version") <- index$version
  attr(res, "source_files") <- unique(source_files[nzchar(source_files)])
  res
}
