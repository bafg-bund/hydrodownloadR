# ==== Chile (DGA via CR2 Explorador) adapter =================================
# Stations:
#   - Official DGA station list (Excel) linked from:
#       https://dga.mop.gob.cl/estadisticas-estaciones-dga/
#     "Listado de estaciones activas de la Direcci\u00F3n General de Aguas"
#     Columns (per site description): c\u00F3digo, nombre, tipo de estaci\u00F3n, cuenca,
#     coordenadas.  We auto-detect columns by name and coerce the code column
#     to text to preserve leading zeros.
#
# Time series:
#   - Daily mean discharge compiled by CR2 (Caudales Medios Diarios - CR2),
#     exposed via Explorador Clim\u00E1tico:
#       https://explorador.cr2.cl
#     We use the same "request.php?options=..." pattern as the existing
#     `chile()` helper you showed, for `variable.id = "qflxDaily"`.
#   - We always download the full series for a gauge and then filter by
#     date range in R (like your `chile()` function).
#
# Notes:
#   - Adapter exposes only parameter = "water_discharge".
#   - Station metadata comes from DGA; time series from CR2 (which compiles
#     DGA + other official data).
#   - Daily data are returned with a `timestamp` at midnight UTC.

# -----------------------------------------------------------------------------
# Registration
# -----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
register_CL_DGA <- function() {
  register_service_usage(
    provider_id   = "CL_DGA",
    provider_name = "Direcci\u00F3n General de Aguas (via CR2 Explorador)",
    country       = "Chile",
    base_url      = "https://explorador.cr2.cl",
    rate_cfg      = list(n = 1L, period = 1),  # 1 request / second
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_CL_DGA <- function(x, ...) {
  # currently only daily discharge
  c("water_discharge")
}

# -----------------------------------------------------------------------------
# Station list (from DGA Excel)
# -----------------------------------------------------------------------------

.cl_dga_stationlist_page <- function() {
  "https://dga.mop.gob.cl/estadisticas-estaciones-dga/"
}

.cl_dga_stationlist_url <- function() {
  # Scrape the Estadisticas page to find the current .xlsx with the
  # "Listado de estaciones vigentes / activas".
  page_url <- .cl_dga_stationlist_page()

  req <- httr2::request(page_url) |>
    httr2::req_user_agent(
      "hydrodownloadR (+https://github.com/your-org/hydrodownloadR)"
    )

  resp <- try(perform_request(req), silent = TRUE)
  if (inherits(resp, "try-error")) {
    rlang::abort("CL_DGA: failed to fetch Estadisticas page for station list.")
  }

  html <- try(
    xml2::read_html(httr2::resp_body_string(resp)),
    silent = TRUE
  )
  if (inherits(html, "try-error")) {
    rlang::abort("CL_DGA: could not parse Estadisticas HTML for station list.")
  }

  links <- rvest::html_elements(html, "a")
  hrefs <- trimws(rvest::html_attr(links, "href"))
  hrefs <- hrefs[!is.na(hrefs)]

  # Heuristic: the station list is an .xlsx whose path typically contains
  # "Listado-estaciones" and/or "Nacional".
  xlsx <- hrefs[grepl("\\.xlsx($|\\?)", hrefs, ignore.case = TRUE)]

  if (!length(xlsx)) {
    rlang::abort(
      "CL_DGA: could not find any .xlsx link on Estadisticas page."
    )
  }


  preferred <- xlsx[
    grepl("Listado",     xlsx, ignore.case = TRUE) |
      grepl("Estacion",  xlsx, ignore.case = TRUE) |
      grepl("Estaciones",xlsx, ignore.case = TRUE) |
      grepl("Nacional",  xlsx, ignore.case = TRUE) |
      grepl("Informe",   xlsx, ignore.case = TRUE)
  ]

  cand <- if (length(preferred)) preferred else xlsx
  url <- cand[1]
  if (!grepl("^https?://", url)) {
    url <- xml2::url_absolute(url, page_url)
  }
  url
}

.cl_dga_read_stationlist <- function() {
  url <- .cl_dga_stationlist_url()

  tmp <- tempfile(fileext = ".xlsx")

  req <- httr2::request(url) |>
    httr2::req_user_agent(
      "hydrodownloadR (+https://github.com/your-org/hydrodownloadR)"
    )

  resp <- try(perform_request(req), silent = TRUE)
  if (inherits(resp, "try-error")) {
    rlang::abort(paste("CL_DGA: failed to download station list from", url))
  }

  bin <- httr2::resp_body_raw(resp)
  writeBin(bin, tmp)

  # First pass: detect which column is the station code (e.g. "Estaci\u00F3n")
  raw0 <- readxl::read_xlsx(tmp, sheet = 1, skip = 15)
  nm0  <- names(raw0)

  id_idx <- which(
    grepl("estaci", nm0, ignore.case = TRUE) |
      grepl("c[o\u00F3]digo", nm0, ignore.case = TRUE)
  )[1]

  if (is.na(id_idx)) {
    # Fallback: just use this read if we didn't find a clear id column
    return(raw0)
  }

  col_types <- rep("guess", length(nm0))
  col_types[id_idx] <- "text"  # preserve leading zeros in codes

  readxl::read_xlsx(
    tmp,
    sheet     = 1,
    skip      = 15,
    col_types = col_types
  )
}

# Datum = UTM zone (18, 19, ...); we map to EPSG:327xx (southern hemisphere).
.cl_dga_utm_to_wgs84 <- function(east, north, datum) {
  east  <- suppressWarnings(as.numeric(east))
  north <- suppressWarnings(as.numeric(north))
  zone  <- suppressWarnings(as.integer(datum))

  lon <- rep(NA_real_, length(east))
  lat <- rep(NA_real_, length(east))

  ok <- !is.na(east) & !is.na(north) & !is.na(zone) &
    zone >= 1 & zone <= 60

  if (!any(ok)) {
    return(list(lon = lon, lat = lat))
  }

  zones <- sort(unique(zone[ok]))

  for (z in zones) {
    idx <- ok & zone == z
    if (!any(idx)) next

    epsg <- 32700 + z  # WGS84 / UTM zone zS (Chile is in southern hemisphere)
    pts  <- data.frame(E = east[idx], N = north[idx])

    sf_pts <- sf::st_as_sf(
      pts,
      coords = c("E", "N"),
      crs    = epsg,
      remove = FALSE
    )
    sf_ll   <- sf::st_transform(sf_pts, 4326)
    coords  <- sf::st_coordinates(sf_ll)

    lon[idx] <- coords[, "X"]
    lat[idx] <- coords[, "Y"]
  }

  list(lon = lon, lat = lat)
}

.cl_dga_dms_to_dd <- function(x, is_lon = FALSE) {
  if (is.null(x)) return(NA_real_)

  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_

  res  <- rep(NA_real_, length(x))
  sign <- rep(1, length(x))

  # explicit hemisphere letters (if present)
  s_idx <- grepl("[Ss]$", x)
  w_idx <- grepl("[WwOo]$", x)  # O = Oeste
  sign[s_idx | w_idx] <- -1

  # Extract D / M / S groups
  m <- regexec("(-?\\d+)\\D+(\\d+)\\D+(\\d+(?:\\.\\d*)?)", x)
  parts <- regmatches(x, m)

  for (i in seq_along(parts)) {
    p <- parts[[i]]
    if (length(p) != 4) next

    d  <- suppressWarnings(as.numeric(p[2]))
    m_ <- suppressWarnings(as.numeric(p[3]))
    s  <- suppressWarnings(as.numeric(p[4]))
    if (is.na(d) || is.na(m_) || is.na(s)) next

    val    <- d + m_ / 60 + s / 3600
    res[i] <- val * sign[i]
  }

  # Chile is S/W; if still positive, flip sign
  pos <- !is.na(res) & res > 0
  if (any(pos)) {
    res[pos] <- -res[pos]
  }

  res
}

.cl_dga_split_station_name <- function(estacion_col) {
  # estacion_col: vector from "Estaci\u00F3n" column
  original <- normalize_utf8(trimws(as.character(estacion_col)))
  river    <- rep(NA_character_, length(original))
  location <- original

  # Match things like "RIO CAMARONES EN CONANOXA"
  has_pat <- grepl("R[\u00cdI]O\\s+.+\\s+EN\\s+.+",
                   original,
                   ignore.case = TRUE)

  river[has_pat] <- sub(
    "^(R[\u00cdI]O\\s+.+?)\\s+EN\\s+(.+)$",
    "\\1",
    original[has_pat],
    ignore.case = TRUE
  )

  location[has_pat] <- sub(
    "^(R[\u00cdI]O\\s+.+?)\\s+EN\\s+(.+)$",
    "\\2",
    original[has_pat],
    ignore.case = TRUE
  )

  list(
    original = original,
    river    = river,
    location = location
  )
}


#' @export
stations.hydro_service_CL_DGA <- function(x, ...) {
  raw_tbl <- .cl_dga_read_stationlist()

  if (!NROW(raw_tbl)) {
    rlang::abort("CL_DGA: station list .xlsx appears to be empty.")
  }

  # --- filter by Tipo de Estaci\u00F3n -------------------------------------------
  tipo_col <- col_or_null(raw_tbl, "Tipo de Estaci\u00F3n")
  station_type <- NULL

  if (!is.null(tipo_col)) {
    tipo_chr <- normalize_utf8(trimws(as.character(tipo_col)))

    # only keep stations where type string contains FLUVIOMETRICA
    has_fl <- grepl("FLUVIOMETRICA", tipo_chr, ignore.case = TRUE)

    keep_type <- has_fl

    raw_tbl      <- raw_tbl[keep_type, , drop = FALSE]
    station_type <- tipo_chr[keep_type]
  }

  if (!NROW(raw_tbl)) {
    # no stations match -> return empty tibble
    return(tibble::tibble(
      country               = x$country,
      provider_id           = x$provider_id,
      provider_name         = x$provider_name,
      station_id            = character(),
      station_name          = character(),
      river                 = character(),
      lat                   = numeric(),
      lon                   = numeric(),
      area                  = numeric(),
      altitude              = numeric(),
      station_name_original = character(),
      station_type          = character()
    ))
  }

  # if there was no tipo_col at all, but we still have rows:
  if (is.null(station_type)) {
    station_type <- rep(NA_character_, NROW(raw_tbl))
  }

  # --- columns via col_or_null() ---------------------------------------------

  # Station code (ID)
  station_id_col <- col_or_null(raw_tbl, "C\u00F3digo")

  # Station name (full DGA label, e.g. "RIO CAMARONES EN CONANOXA")
  estacion_col <- col_or_null(raw_tbl, "Estaci\u00F3n")

  # Coordinates (UTM and Datum)
  utm_n_col <- col_or_null(raw_tbl, "UTM WGS84 Norte(m)")
  utm_e_col <- col_or_null(raw_tbl, "UTM WGS84 Este(m)")
  datum_col <- col_or_null(raw_tbl, "Datum")

  # DMS coordinates
  lat_dms_col <- col_or_null(raw_tbl, "Latitud")
  lon_dms_col <- col_or_null(raw_tbl, "Longitud")

  # Altitude
  alt_col <- col_or_null(raw_tbl, "Altitud m.s.n.m")

  # --- basic cleaning --------------------------------------------------------

  station_id <- trimws(as.character(station_id_col))

  # Split station name into river + location, but keep original label
  name_split <- .cl_dga_split_station_name(estacion_col)
  station_name_original <- name_split$original
  river                  <- name_split$river
  station_name           <- name_split$location

  utm_n   <- utm_n_col
  utm_e   <- utm_e_col
  datum   <- datum_col
  lat_dms <- lat_dms_col
  lon_dms <- lon_dms_col
  alt_raw <- alt_col

  # --- 1) primary: UTM to WGS84 ----------------------------------------------

  utm_coords <- .cl_dga_utm_to_wgs84(
    east  = utm_e,
    north = utm_n,
    datum = datum
  )
  lon_num <- utm_coords$lon
  lat_num <- utm_coords$lat

  # --- 2) fallback: DMS Latitud/Longitud ------------------------------------

  lat_dms_dd <- .cl_dga_dms_to_dd(lat_dms, is_lon = FALSE)
  lon_dms_dd <- .cl_dga_dms_to_dd(lon_dms, is_lon = TRUE)

  missing_lat <- is.na(lat_num) & !is.na(lat_dms_dd)
  missing_lon <- is.na(lon_num) & !is.na(lon_dms_dd)

  lat_num[missing_lat] <- lat_dms_dd[missing_lat]
  lon_num[missing_lon] <- lon_dms_dd[missing_lon]

  altitude <- suppressWarnings(as.numeric(as.character(alt_raw)))

  # --- final tibble ----------------------------------------------------------

  out <- tibble::tibble(
    country               = x$country,
    provider_id           = x$provider_id,
    provider_name         = x$provider_name,
    station_id            = station_id,
    station_name_original = station_name_original, # full DGA label
    station_name          = station_name,          # location only
    river                 = river,                 # e.g. "RIO CAMARONES"
    lat                   = lat_num,
    lon                   = lon_num,
    area                  = NA_real_,
    altitude              = altitude,
    station_type          = station_type           # only types with FLUVIOMETRICA
  )

  # Drop rows without a usable ID
  out <- out[!is.na(out$station_id) & nzchar(out$station_id), ]

  out
}

# -----------------------------------------------------------------------------
# Internal helpers for time series (CR2 Explorador)
# -----------------------------------------------------------------------------

.cl_cr2_download_q_daily <- function(site) {
  # site: DGA station code as character, e.g. "09140001"
  site <- trimws(as.character(site))

  # Use a dynamic end timestamp instead of the old hard-coded 2021 value.
  # Add one day to avoid timezone edge cases.
  end_epoch <- as.integer(as.POSIXct(Sys.Date() + 1, tz = "UTC"))

  options_str <- paste0(
    "{",
    "\"variable\":{\"id\":\"qflxDaily\",\"var\":\"caudal\",",
    "\"intv\":\"daily\",\"season\":\"year\",\"stat\":\"mean\",",
    "\"minFrac\":80},",
    "\"time\":{\"start\":-946771200,\"end\":", end_epoch, ",",
    "\"months\":\"A\\u00f1o completo\"},",
    "\"anomaly\":{\"enabled\":false,\"type\":\"dif\",\"rank\":\"no\",",
    "\"start_year\":1980,\"end_year\":2010,\"minFrac\":70},",
    "\"map\":{\"stat\":\"mean\",\"minFrac\":10,",
    "\"borderColor\":\"7F7F7F\",\"colorRamp\":\"Jet\",",
    "\"showNaN\":false,",
    "\"limits\":{\"range\":[5,95],\"size\":[4,12],\"type\":\"prc\"}},",
    "\"series\":{\"sites\":[\"", site, "\"],\"start\":null,\"end\":null},",
    "\"export\":{\"map\":\"Shapefile\",\"series\":\"CSV\",",
    "\"view\":{\"frame\":\"Vista Actual\",\"map\":\"roadmap\",",
    "\"clat\":-38.148818103572275,",
    "\"clon\":-76.04504453124999,",
    "\"zoom\":5,\"width\":579,\"height\":628}},",
    "\"action\":[\"export_series\"]",
    "}"
  )

  website <- paste0(
    "https://explorador.cr2.cl/request.php?options=",
    utils::URLencode(options_str, reserved = TRUE)
  )

  Sys.sleep(0.25)

  # --------------------------------------------------------------------------
  # 1) Ask CR2 for the temporary CSV URL
  # --------------------------------------------------------------------------
  req <- httr2::request(website) |>
    httr2::req_user_agent(
      "hydrodownloadR (+https://github.com/bafg-bund/hydrodownloadR)"
    ) |>
    httr2::req_timeout(60)

  resp <- perform_request(req)
  body_txt <- httr2::resp_body_string(resp)

  js <- try(jsonlite::fromJSON(body_txt), silent = TRUE)
  if (inherits(js, "try-error")) {
    rlang::warn(paste0(
      "CL_DGA/CR2: could not parse JSON response for station ", site
    ))
    return(tibble::tibble())
  }

  if (length(js$errors) > 0) {
    rlang::warn(paste0(
      "CL_DGA/CR2: response contains errors for station ", site, ": ",
      paste(js$errors, collapse = "; ")
    ))
    return(tibble::tibble())
  }

  csv_url <- js$export$series$url

  if (is.null(csv_url) || !nzchar(csv_url)) {
    rlang::warn(paste0(
      "CL_DGA/CR2: no CSV URL in response for station ", site
    ))
    return(tibble::tibble())
  }

  # --------------------------------------------------------------------------
  # 2) Read the temporary CSV via httr2, not utils::download.file()
  # --------------------------------------------------------------------------
  read_csv_httr2 <- function(url) {
    tmp <- tempfile(fileext = ".csv")

    req_csv <- httr2::request(url) |>
      httr2::req_user_agent(
        "hydrodownloadR (+https://github.com/bafg-bund/hydrodownloadR)"
      ) |>
      httr2::req_timeout(60) |>
      httr2::req_retry(max_tries = 3)

    resp_csv <- perform_request(req_csv)
    writeBin(httr2::resp_body_raw(resp_csv), tmp)

    out <- readr::read_delim(tmp, show_col_types = FALSE)
    names(out) <- sub("\\s+", "", names(out))

    tibble::as_tibble(out)
  }

  out <- try(read_csv_httr2(csv_url), silent = TRUE)

  # Some R/curl setups fail SSL on www.explorador.cr2.cl.
  # Try the same temporary path without "www.".
  if (inherits(out, "try-error")) {
    csv_url2 <- sub(
      "^https://www\\.explorador\\.cr2\\.cl",
      "https://explorador.cr2.cl",
      csv_url
    )

    if (!identical(csv_url2, csv_url)) {
      out <- try(read_csv_httr2(csv_url2), silent = TRUE)
    }
  }

  if (inherits(out, "try-error")) {
    rlang::warn(paste0(
      "CL_DGA/CR2: CSV download failed for station ", site
    ))
    return(tibble::tibble())
  }

  attr(out, "source_url") <- csv_url
  out
}

.cl_fetch_q_daily <- function(x,
                              station_id,
                              rng,
                              mode) {
  site <- trimws(as.character(station_id))

  ts_raw <- try(.cl_cr2_download_q_daily(site), silent = TRUE)
  if (inherits(ts_raw, "try-error") || !NROW(ts_raw)) {
    rlang::warn(paste0("CL_DGA/CR2: download failed for station ", site))
    return(tibble::tibble())
  }

  required_cols <- c("agno", "mes", "dia", "valor")
  if (!all(required_cols %in% names(ts_raw))) {
    rlang::warn(paste0(
      "CL_DGA/CR2: unexpected columns for station ", site,
      " (missing one of: ", paste(required_cols, collapse = ", "), ")."
    ))
    return(tibble::tibble())
  }

  year  <- suppressWarnings(as.integer(ts_raw$agno))
  month <- suppressWarnings(as.integer(ts_raw$mes))
  day   <- suppressWarnings(as.integer(ts_raw$dia))

  date_chr <- sprintf("%04d-%02d-%02d", year, month, day)
  date     <- suppressWarnings(as.Date(date_chr))

  value <- suppressWarnings(as.numeric(ts_raw$valor))

  keep <- !is.na(date) & !is.na(value)

  if (identical(mode, "range")) {
    keep <- keep &
      date >= rng$start_date &
      date <= rng$end_date
  }

  if (!any(keep)) {
    return(tibble::tibble())
  }

  date  <- date[keep]
  value <- value[keep]

  ts_final <- as.POSIXct(date, tz = "UTC")

  tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = site,
    parameter     = "water_discharge",
    timestamp     = ts_final,
    value         = value,
    unit          = "m^3/s",
    quality_code  = NA_character_,
    quality_name  = NA_character_,
    quality_desc  = NA_character_,
    source_url    = x$base_url
  )
}



#' @export
timeseries.hydro_service_CL_DGA <- function(x,
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
    rlang::abort("CL_DGA: only parameter = 'water_discharge' is supported.")
  }

  rng <- resolve_dates(mode, start_date, end_date)

  # --------------------------------------------------------------------------
  # station_id vector
  # - default: only stations with station_type containing "FLUVIOMETRICA"
  # - if stations are given explicitly, use them as-is
  # --------------------------------------------------------------------------
  if (is.null(stations)) {
    st <- stations.hydro_service_CL_DGA(x)

    if ("station_type" %in% names(st)) {
      is_fl <- grepl("FLUVIOMETRICA", st$station_type, ignore.case = TRUE)
      st    <- st[is_fl & !is.na(st$station_id), , drop = FALSE]
    }

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
    format = "CL_DGA [:bar] :current/:total (:percent) eta: :eta"
  )

  fetch_one <- function(st_id) {
    .cl_fetch_q_daily(
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

  dplyr::bind_rows(out)
}
