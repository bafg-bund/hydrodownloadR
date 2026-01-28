# ==== Spain (ROAN - Anuario de Aforos) adapter ===============================
# Stations:
#   - Station list (rivers only) provided as a ZIP of CSVs:
#       https://www.miteco.gob.es/content/dam/miteco/es/agua/temas/
#       evaluacion-de-los-recursos-hidricos/sistema-informacion-anuario-aforos/
#       listado-estaciones-aforo.zip
#     We extract the CSV whose name contains "Situac" and "Rio" (e.g.
#     "Situac_estaciones_aforo_Rio.csv").
#   - Columns used (based on ROAN/SAIH-ROEA schema):
#       COD_HIDRO      to station_id
#       NOM_ANUARIO    to station_name
#       RIO            to river
#       COTA_Z         to altitude (m)
#       CUENCA_TOTAL   to area (km2)
#       COORD_UTMX_H30_ETRS89 / COORD_UTMY_H30_ETRS89 to UTM ETRS89 / 30N
#     We convert UTM ETRS89 (zone 30) coordinates to WGS84 (EPSG:4326).
#
# Time series:
#   - Daily mean discharge from ROAN web service:
#       https://sig.mapama.gob.es/WebServices/clientews/redes-seguimiento/
#       default.aspx?nombre=ROAN_RIOS_DIARIO_CAUDAL&...
#     Parameters:
#       nombre = "ROAN_RIOS_DIARIO_CAUDAL"
#       claves  = "INDROEA|ANO_INI|ANO_FIN"
#       valores = "{gauge_id}|{start_year}|{end_year}"
#       origen  = 1008
#   - Service returns an HTML table with columns:
#       "Estacion", "Ano", "Dia", "Oct", "Nov", "Dic", "Ene", ..., "Sep"
#     where "Ano" is a hydrological year like "2019-20",
#     months Oct-Dec belong to the first calendar year, Jan-Sep to the next.
#   - Values are stored as integers (e.g. 074 = 0.74 m3/s), so we divide by 100.
#   - We always request a year span that covers the user's requested date range,
#     then filter by date in R (like your CL_DGA adapter).
#
# Notes:
#   - Adapter exposes only parameter = "water_discharge".
#   - Station metadata comes from the MITECO "listado-estaciones-aforo" ZIP.
#   - Daily data are returned with a `timestamp` at midnight UTC.
#   - Coordinates are assumed to be ETRS89 / UTM zone 30N (EPSG:25830) and
#     transformed to WGS84 / EPSG:4326.

# -----------------------------------------------------------------------------
# Registration
# -----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
register_ES_ROAN <- function() {
  register_service_usage(
    provider_id   = "ES_ROAN",
    provider_name = "ROAN (Anuario de Aforos, MITECO)",
    country       = "Spain",
    base_url      = "https://sig.mapama.gob.es/WebServices/clientews/redes-seguimiento",
    rate_cfg      = list(n = 1L, period = 1),  # 1 request / second
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_ES_ROAN <- function(x, ...) {
  # currently only daily discharge
  c("water_discharge")
}

# -----------------------------------------------------------------------------
# Station list (from MITECO listado-estaciones-aforo.zip)
# -----------------------------------------------------------------------------

.es_roan_metadata_zip_url <- function() {
  paste0(
    "https://www.miteco.gob.es/content/dam/miteco/es/agua/temas/",
    "evaluacion-de-los-recursos-hidricos/sistema-informacion-anuario-aforos/",
    "listado-estaciones-aforo.zip"
  )
}

.es_roan_read_metadata <- function() {
  url <- .es_roan_metadata_zip_url()

  tmp_zip <- tempfile(fileext = ".zip")

  req <- httr2::request(url) |>
    httr2::req_user_agent(
      "hydrodownloadR (+https://github.com/your-org/hydrodownloadR)"
    )

  resp <- try(perform_request(req), silent = TRUE)
  if (inherits(resp, "try-error")) {
    rlang::abort(paste("ES_ROAN: failed to download station ZIP from", url))
  }

  bin <- httr2::resp_body_raw(resp)
  writeBin(bin, tmp_zip)

  files <- utils::unzip(tmp_zip, list = TRUE)
  if (!NROW(files)) {
    rlang::abort("ES_ROAN: station ZIP appears to be empty.")
  }

  # Heuristic: river station situation file, e.g. "Situac_estaciones_aforo_Rio.csv"
  cand <- files$Name[
    grepl("Situac", files$Name, ignore.case = TRUE) &
      grepl("Rio", files$Name,   ignore.case = TRUE) &
      grepl("\\.csv$", files$Name, ignore.case = TRUE)
  ]

  if (!length(cand)) {
    rlang::abort("ES_ROAN: could not find 'Situac*Rio*.csv' in station ZIP.")
  }

  target <- cand[1]
  outdir <- tempfile("es_roan_meta_")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  utils::unzip(tmp_zip, files = target, exdir = outdir)

  csv_path <- file.path(outdir, target)

  raw <- suppressWarnings(
    readr::read_delim(
      file         = csv_path,
      delim        = ";",
      locale       = readr::locale(encoding = "Latin1"),
      trim_ws      = TRUE,
      show_col_types = FALSE
    )
  )

  names(raw) <- trimws(names(raw))
  raw
}

# Convert ETRS89 / UTM zone 30N (EPSG:25830) to WGS84 (EPSG:4326).
.es_roan_utm30_to_wgs84 <- function(east, north) {
  east  <- suppressWarnings(as.numeric(east))
  north <- suppressWarnings(as.numeric(north))

  lon <- rep(NA_real_, length(east))
  lat <- rep(NA_real_, length(east))

  ok <- !is.na(east) & !is.na(north)
  if (!any(ok)) {
    return(list(lon = lon, lat = lat))
  }

  pts <- data.frame(X = east[ok], Y = north[ok])

  sf_pts <- sf::st_as_sf(
    pts,
    coords = c("X", "Y"),
    crs    = 25830,  # ETRS89 / UTM zone 30N
    remove = FALSE
  )

  sf_ll  <- sf::st_transform(sf_pts, 4326)
  coords <- sf::st_coordinates(sf_ll)

  lon[ok] <- coords[, "X"]
  lat[ok] <- coords[, "Y"]

  list(lon = lon, lat = lat)
}

#' @export
stations.hydro_service_ES_ROAN <- function(x, ...) {
  raw_tbl <- .es_roan_read_metadata()

  if (!NROW(raw_tbl)) {
    rlang::abort("ES_ROAN: station metadata table appears to be empty.")
  }

  # ---- columns from ROAN schema ---------------------------------------------

  # ID (COD_HIDRO)
  if (!"COD_HIDRO" %in% names(raw_tbl)) {
    rlang::abort("ES_ROAN: 'COD_HIDRO' column not found in metadata.")
  }
  station_id <- trimws(as.character(raw_tbl[["COD_HIDRO"]]))

  # Station name (NOM_ANUARIO)
  name_col    <- raw_tbl[["NOM_ANUARIO"]]
  station_name <- normalize_utf8(trimws(as.character(name_col)))

  # River name (RIO)
  river_col <- raw_tbl[["RIO"]]
  river     <- normalize_utf8(trimws(as.character(river_col)))

  # Altitude (COTA_Z, m)
  alt_col  <- raw_tbl[["COTA_Z"]]
  altitude <- suppressWarnings(as.numeric(as.character(alt_col)))

  # Area (CUENCA_RECEP, km2)
  area_col <- raw_tbl[["CUENCA_RECEP"]]
  area     <- suppressWarnings(as.numeric(as.character(area_col)))

  # UTM coords (ETRS89 / 30N)
  utm_x <- if ("COORD_UTMX_H30_ETRS89" %in% names(raw_tbl)) {
    raw_tbl[["COORD_UTMX_H30_ETRS89"]]
  } else {
    rep(NA_real_, NROW(raw_tbl))
  }

  utm_y <- if ("COORD_UTMY_H30_ETRS89" %in% names(raw_tbl)) {
    raw_tbl[["COORD_UTMY_H30_ETRS89"]]
  } else {
    rep(NA_real_, NROW(raw_tbl))
  }

  coords <- .es_roan_utm30_to_wgs84(utm_x, utm_y)
  lon    <- coords$lon
  lat    <- coords$lat

  out <- tibble::tibble(
    country            = x$country,
    provider_id        = x$provider_id,
    provider_name      = x$provider_name,
    station_id         = station_id,
    station_name       = station_name,
    station_name_ascii = to_ascii(station_name),
    river              = river,
    river_ascii        = to_ascii(river),
    lat                = lat,
    lon                = lon,
    area               = area,
    altitude           = altitude
  )

  # Drop rows without a usable ID
  out <- out[!is.na(out$station_id) & nzchar(out$station_id), , drop = FALSE]

  out
}

# -----------------------------------------------------------------------------
# Internal helpers for time series (ROAN web service)
# -----------------------------------------------------------------------------

.es_roan_ws_base <- function() {
  "https://sig.mapama.gob.es/WebServices/clientews/redes-seguimiento/default.aspx"
}

.es_roan_download_table <- function(station_id, start_year, end_year) {
  # Encode the claves/valores part to include '|' safely
  claves  <- utils::URLencode("INDROEA|ANO_INI|ANO_FIN", reserved = TRUE)
  valores <- utils::URLencode(
    sprintf("%s|%d|%d", station_id, start_year, end_year),
    reserved = TRUE
  )

  url <- sprintf(
    "%s?nombre=ROAN_RIOS_DIARIO_CAUDAL&claves=%s&valores=%s&origen=1008",
    .es_roan_ws_base(), claves, valores
  )

  req <- httr2::request(url) |>
    httr2::req_user_agent(
      "hydrodownloadR (+https://github.com/your-org/hydrodownloadR)"
    )

  resp <- try(perform_request(req), silent = TRUE)
  if (inherits(resp, "try-error")) {
    rlang::warn(
      paste0("ES_ROAN: HTTP request failed for station ", station_id, ".")
    )
    return(tibble::tibble())
  }

  html_txt <- httr2::resp_body_string(resp)

  # Parse HTML and find the first <table>
  html_doc <- try(xml2::read_html(html_txt), silent = TRUE)
  if (inherits(html_doc, "try-error")) {
    rlang::warn(
      paste0("ES_ROAN: could not parse HTML for station ", station_id, ".")
    )
    return(tibble::tibble())
  }

  tab_nodes <- rvest::html_elements(html_doc, "table")
  if (!length(tab_nodes)) {
    rlang::warn(
      paste0("ES_ROAN: no <table> element found for station ", station_id, ".")
    )
    return(tibble::tibble())
  }

  tbl <- rvest::html_table(tab_nodes[[1]], fill = TRUE)
  tibble::as_tibble(tbl)
}

.es_roan_parse_daily_table <- function(df) {
  if (is.null(df) || !NROW(df)) {
    return(tibble::tibble(date = as.Date(character()), value = numeric()))
  }

  # Fix header encoding *and* strip diacritics so we work with pure ASCII
  names(df) <- to_ascii(normalize_utf8(trimws(names(df))))

  # Expect at least: Estacion, Ano, Dia + months
  if (ncol(df) < 4L) {
    return(tibble::tibble(date = as.Date(character()), value = numeric()))
  }

  # Month columns: from 4th column onwards
  month_cols <- names(df)[-(1:3)]
  if (!length(month_cols)) {
    return(tibble::tibble(date = as.Date(character()), value = numeric()))
  }

  # Convert raw values (e.g. "074") to numeric m3/s (0.74)
  for (m in month_cols) {
    df[[m]] <- suppressWarnings(
      as.numeric(gsub(",", ".", df[[m]], fixed = TRUE))
    )
  }

  # We expect something like "Ano" and "Dia" after to_ascii()
  # If needed, try to recover them heuristically.
  if (!"Ano" %in% names(df)) {
    ano_name <- names(df)[grepl("Ano", names(df), ignore.case = TRUE)][1]
    if (is.na(ano_name)) {
      return(tibble::tibble(date = as.Date(character()), value = numeric()))
    }
    names(df)[names(df) == ano_name] <- "Ano"
  }

  if (!"Dia" %in% names(df)) {
    dia_name <- names(df)[grepl("Dia", names(df), ignore.case = TRUE)][1]
    if (is.na(dia_name)) {
      return(tibble::tibble(date = as.Date(character()), value = numeric()))
    }
    names(df)[names(df) == dia_name] <- "Dia"
  }

  # Pivot to long format: one row per (hydro year, day, month)
  df_long <- tidyr::pivot_longer(
    df,
    cols      = tidyselect::all_of(month_cols),
    names_to  = "Mes",
    values_to = "value"
  )

  # Month mapping (headers are already ASCII)
  month_map <- c(
    "Oct" = 10L, "Nov" = 11L, "Dic" = 12L,
    "Ene" = 1L,  "Feb" = 2L,  "Mar" = 3L,
    "Abr" = 4L,  "May" = 5L,  "Jun" = 6L,
    "Jul" = 7L,  "Ago" = 8L,  "Sep" = 9L
  )

  df_long$Mes_num <- unname(month_map[as.character(df_long$Mes)])

  # Extract first calendar year from "Ano" (string like "2019-20" or "2019-2020")
  ano_chr <- as.character(df_long$Ano)
  ano_ini <- suppressWarnings(
    as.integer(sub("^\\s*(\\d{4}).*$", "\\1", ano_chr))
  )
  ano_fin <- ano_ini + 1L   # hydrological year spans one year

  df_long$Ano_ini <- ano_ini
  df_long$Ano_fin <- ano_fin

  # Determine real calendar year based on month (Oct-Dec vs Jan-Sep)
  df_long$year <- ifelse(df_long$Mes_num >= 10L, df_long$Ano_ini, df_long$Ano_fin)

  # Day of month
  dia <- suppressWarnings(as.integer(df_long$Dia))

  # Build date
  date_chr <- sprintf("%04d-%02d-%02d", df_long$year, df_long$Mes_num, dia)
  date     <- suppressWarnings(as.Date(date_chr))

  keep <- !is.na(date) & !is.na(df_long$value)
  if (!any(keep)) {
    return(tibble::tibble(date = as.Date(character()), value = numeric()))
  }

  tibble::tibble(
    date  = date[keep],
    value = df_long$value[keep]
  )
}



.es_roan_fetch_q_daily <- function(x,
                                   station_id,
                                   rng,
                                   mode) {
  site <- trimws(as.character(station_id))

  # Determine year span for the ROAN request
  if (identical(mode, "range")) {
    start_year <- as.integer(format(rng$start_date, "%Y"))
    end_year   <- as.integer(format(rng$end_date,   "%Y"))
  } else {
    # "complete" - ask for a wide range; ROAN only returns available years.
    start_year <- 1911L
    end_year   <- as.integer(format(Sys.Date(), "%Y"))
  }

  raw_tbl <- try(
    .es_roan_download_table(site, start_year, end_year),
    silent = TRUE
  )
  if (inherits(raw_tbl, "try-error") || !NROW(raw_tbl)) {
    rlang::warn(paste0("ES_ROAN: download failed for station ", site))
    return(tibble::tibble())
  }

  parsed <- .es_roan_parse_daily_table(raw_tbl)
  if (!NROW(parsed)) {
    return(tibble::tibble())
  }

  # Filter by date range if requested
  if (identical(mode, "range")) {
    keep <- parsed$date >= rng$start_date & parsed$date <= rng$end_date
    parsed <- parsed[keep, , drop = FALSE]
    if (!NROW(parsed)) {
      return(tibble::tibble())
    }
  }

  ts_final <- as.POSIXct(parsed$date, tz = "UTC")

  tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = site,
    parameter     = "water_discharge",
    timestamp     = ts_final,
    value         = parsed$value,
    unit          = "m^3/s",
    quality_code  = NA_character_,
    source_url    = x$base_url
  )
}

# -----------------------------------------------------------------------------
# Public time series method
# -----------------------------------------------------------------------------

#' @export
timeseries.hydro_service_ES_ROAN <- function(x,
                                             parameter = c("water_discharge"),
                                             stations    = NULL,
                                             start_date  = NULL,
                                             end_date    = NULL,
                                             mode        = c("complete", "range"),
                                             exclude_quality = NULL,
                                             ...) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)

  if (!identical(parameter, "water_discharge")) {
    rlang::abort("ES_ROAN: only parameter = 'water_discharge' is supported.")
  }

  rng <- resolve_dates(mode, start_date, end_date)

  # --------------------------------------------------------------------------
  # station_id vector
  #   - default: all river gauges from metadata
  #   - if stations are given explicitly, use them as-is
  # --------------------------------------------------------------------------
  if (is.null(stations)) {
    st <- stations.hydro_service_ES_ROAN(x)
    st <- st[!is.na(st$station_id), , drop = FALSE]
    station_vec <- st$station_id
  } else {
    station_vec <- stations
  }

  station_vec <- unique(trimws(as.character(station_vec)))
  if (!length(station_vec)) {
    return(tibble::tibble())
  }

  # batching + rate limit -----------------------------------------------------

  batches <- chunk_vec(station_vec, 20L)

  pb <- progress::progress_bar$new(
    total  = length(batches),
    format = "ES_ROAN [:bar] :current/:total (:percent) eta: :eta"
  )

  fetch_one <- function(st_id) {
    .es_roan_fetch_q_daily(
      x          = x,
      station_id = st_id,
      rng        = rng,
      mode       = mode
    )
  }

  limited <- ratelimitr::limit_rate(
    fetch_one,
    rate = ratelimitr::rate(
      n      = x$rate_cfg$n %||% 1L,
      period = x$rate_cfg$period %||% 1
    )
  )

  out <- lapply(batches, function(batch) {
    pb$tick()
    dplyr::bind_rows(lapply(batch, limited))
  })

  res <- dplyr::bind_rows(out)

  if (!NROW(res)) {
    return(res)
  }

  # Order by station_id, then timestamp (historical to today)
  res <- dplyr::arrange(res, station_id, timestamp)

  res
}
