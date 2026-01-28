# ==== Slovenia (ARSO hydrological archive) adapter ===========================
# Stations:
#   - Live station metadata from XML feed:
#       http://www.arso.gov.si/xml/vode/hidro_podatki_zadnji.xml
#     One <postaja> per station, attributes:
#       sifra         - station code
#       wgs84_dolzina - longitude (decimal degrees, WGS84)
#       wgs84_sirina  - latitude  (decimal degrees, WGS84)
#       kota_0        - gauge zero (m a.s.l.)
#     Child elements:
#       <reka>, <merilno_mesto>, <ime_kratko>, ...
#     We derive:
#       station_id   = sifra
#       station_name = ime_kratko (fallback: merilno_mesto)
#       river        = reka
#       lat/lon      from WGS84 attributes
#       altitude     from kota_0
#       area         = NA (not available here)
#
# Time series:
#   - Daily mean water level, discharge, temperature and suspended sediment
#     from ARSO hydrological archive:
#       https://vode.arso.gov.si/hidarhiv/pov_arhiv_tab.php
#   - Request pattern (as in your Python fetcher):
#       ?p_postaja=<gauge_id>
#       &p_od_leto=<start_year>
#       &p_do_leto=<end_year>
#       &b_oddo_CSV=Izvoz+dnevnih+vrednosti+v+CSV
#   - Response is a ;-separated CSV with columns:
#       "Datum", "vodostaj (cm)", "pretok (m3/s)", ...
#   - We always request a year span and then:
#       * parse "Datum" as %d.%m.%Y
#       * convert "vodostaj (cm)" -> m
#       * keep "pretok (m3/s)" as is
#       * filter by date range in R if mode = "range".
#
# Notes:
#   - Exposed parameters:
#       "water_discharge",
#       "water_level",
#       "water_temperature",
#       "suspended_sediment_transport",
#       "suspended_sediment_concentration".
#   - Station metadata & time series both ultimately come from ARSO.
#   - Daily data are returned with `timestamp` at midnight UTC.

# -----------------------------------------------------------------------------
# Registration
# -----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
register_SI_ARSO <- function() {
  register_service_usage(
    provider_id   = "SI_ARSO",
    provider_name = "ARSO hydrological archive",
    country       = "Slovenia",
    base_url      = "https://vode.arso.gov.si",
    rate_cfg      = list(n = 1L, period = 1),  # 1 request / second
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_SI_ARSO <- function(x, ...) {
  c(
    "water_discharge",
    "water_level",
    "water_temperature",
    "suspended_sediment_transport",
    "suspended_sediment_concentration"
  )
}

.si_param_map <- function(parameter) {
  pm <- switch(
    parameter,
    water_discharge = list(
      par   = "^pretok",              # "pretok (m^3/s)"
      unit  = "m^3/s",
      mult  = 1
    ),
    water_level = list(
      par   = "^vodostaj",            # "vodostaj (cm)" to m
      unit  = "m",
      mult  = 0.01                    # cm to m
    ),
    water_temperature = list(
      par   = "temp.*vode",           # "temp. vode (\u00B0C)"
      unit  = "\u00B0C",
      mult  = 1
    ),
    suspended_sediment_transport = list(
      # "transport suspend. materiala (kg/s)"
      # "transport suspendiranega materiala (kg/s)"
      par   = "transport\\s*suspend",
      unit  = "kg/s",
      mult  = 1
    ),
    suspended_sediment_concentration = list(
      # "vsebnost suspend. materiala (g/m3)"
      # "vsebnost suspendiranega materiala (g/m3)"
      par   = "vsebnost\\s*suspend",
      unit  = "g/m3",
      mult  = 1
    ),
    rlang::abort(
      "SI_ARSO supports 'water_discharge', 'water_level', ",
      "'water_temperature', 'suspended_sediment_transport', ",
      "and 'suspended_sediment_concentration'."
    )
  )

  pm$parameter <- parameter
  pm
}

# -----------------------------------------------------------------------------
# Station metadata from XML "latest data" feed
#   http://www.arso.gov.si/xml/vode/hidro_podatki_zadnji.xml
# -----------------------------------------------------------------------------

.si_arso_meta_from_xml <- function(x) {
  url <- "http://www.arso.gov.si/xml/vode/hidro_podatki_zadnji.xml"

  req <- httr2::request(url) |>
    httr2::req_user_agent(
      "hydrodownloadR (+https://github.com/your-org/hydrodownloadR)"
    )

  resp <- try(perform_request(req), silent = TRUE)
  if (inherits(resp, "try-error")) {
    rlang::abort("SI_ARSO: failed to download XML station feed.")
  }

  txt <- httr2::resp_body_string(resp, encoding = "UTF-8")

  doc <- try(xml2::read_xml(txt), silent = TRUE)
  if (inherits(doc, "try-error")) {
    rlang::abort("SI_ARSO: could not parse XML station feed.")
  }

  posts <- xml2::xml_find_all(doc, "//postaja")
  if (!length(posts)) {
    rlang::abort("SI_ARSO: no <postaja> elements found in XML.")
  }

  # small helper to pull child text safely
  get_child_text <- function(nodes, tag) {
    vapply(
      nodes,
      function(n) {
        node <- xml2::xml_find_first(n, tag)
        if (inherits(node, "xml_missing") || length(node) == 0L) {
          return(NA_character_)
        }
        xml2::xml_text(node)
      },
      character(1)
    )
  }

  station_id    <- xml2::xml_attr(posts, "sifra")
  lon           <- xml2::xml_attr(posts, "wgs84_dolzina")
  lat           <- xml2::xml_attr(posts, "wgs84_sirina")
  kota0         <- xml2::xml_attr(posts, "kota_0")

  river         <- get_child_text(posts, "reka")
  merilno_mesto <- get_child_text(posts, "merilno_mesto")
  ime_kratko    <- get_child_text(posts, "ime_kratko")

  station_id <- trimws(as.character(station_id))

  station_name <- ime_kratko
  station_name[is.na(station_name) | !nzchar(station_name)] <- merilno_mesto[
    is.na(station_name) | !nzchar(station_name)
  ]

  out <- tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = station_id,
    station_name  = station_name,
    river         = river,
    lat           = suppressWarnings(as.numeric(lat)),
    lon           = suppressWarnings(as.numeric(lon)),
    area          = NA_real_,
    altitude      = suppressWarnings(as.numeric(kota0))
  )

  out <- out[!is.na(out$station_id) & nzchar(out$station_id), , drop = FALSE]
  out <- dplyr::distinct(out, station_id, .keep_all = TRUE)

  out
}

.si_arso_meta <- function(x) {
  .si_arso_meta_from_xml(x)
}

#' @export
stations.hydro_service_SI_ARSO <- function(x, ...) {
  .si_arso_meta(x)
}

# -----------------------------------------------------------------------------
# Internal helpers for time series (ARSO daily CSV)
# -----------------------------------------------------------------------------

.si_arso_download_csv <- function(x, station_id, start_year, end_year) {
  # station_id: ARSO gauge ID (same as p_postaja)
  # start_year / end_year: integers

  query <- list(
    p_postaja  = station_id,
    p_od_leto  = start_year,
    p_do_leto  = end_year,
    b_oddo_CSV = "Izvoz dnevnih vrednosti v CSV"
  )

  # build_request() + perform_request() from http_utils.R
  req <- build_request(
    x       = x,
    path    = "hidarhiv/pov_arhiv_tab.php",
    query   = query,
    headers = list()
  )

  resp <- try(perform_request(req), silent = TRUE)
  if (inherits(resp, "try-error")) {
    rlang::warn(paste0(
      "SI_ARSO: request failed for station ", station_id,
      " (years ", start_year, "-", end_year, ")."
    ))
    return(NULL)
  }

  status <- httr2::resp_status(resp)
  if (status >= 400) {
    rlang::warn(paste0(
      "SI_ARSO: HTTP ", status, " for station ", station_id,
      " (years ", start_year, "-", end_year, ")."
    ))
    return(NULL)
  }

  httr2::resp_body_string(resp, encoding = "UTF-8")
}

.si_arso_parse_daily <- function(raw_txt, station_id, parameter) {
  if (is.null(raw_txt) || !nzchar(raw_txt)) {
    return(tibble::tibble())
  }

  # Read CSV via base R to avoid readr/vroom parse warnings
  con <- textConnection(raw_txt)
  on.exit(close(con), add = TRUE)

  df <- try(
    utils::read.csv2(
      file             = con,
      header           = TRUE,
      sep              = ";",    # read.csv2 default
      dec              = ",",    # read.csv2 default - matches SI locale
      stringsAsFactors = FALSE,
      check.names      = FALSE
    ),
    silent = TRUE
  )

  if (inherits(df, "try-error") || !NROW(df)) {
    rlang::warn(paste0("SI_ARSO: failed to parse CSV for station ", station_id))
    return(tibble::tibble())
  }

  # ---- Date -----------------------------------------------------------------
  if (!"Datum" %in% names(df)) {
    rlang::warn(paste0(
      "SI_ARSO: no 'Datum' column in CSV for station ", station_id
    ))
    return(tibble::tibble())
  }

  date <- suppressWarnings(as.Date(df[["Datum"]], format = "%d.%m.%Y"))
  if (all(is.na(date))) {
    rlang::warn(paste0(
      "SI_ARSO: could not parse dates for station ", station_id
    ))
    return(tibble::tibble())
  }

  # ---- Parameter column based on map ----------------------------------------
  pm <- .si_param_map(parameter)
  nm <- names(df)

  col_idx <- grep(pm$par, nm, ignore.case = TRUE)
  if (!length(col_idx)) {
    rlang::warn(paste0(
      "SI_ARSO: no column matching pattern '", pm$par,
      "' for parameter '", parameter, "' at station ", station_id
    ))
    return(tibble::tibble())
  }

  col_name <- nm[col_idx[1L]]
  val_raw  <- df[[col_name]]

  # Already numeric in many cases (dec="," handled by read.csv2),
  # but be defensive:
  value <- suppressWarnings(as.numeric(val_raw))

  mult  <- pm$mult %||% 1
  value <- value * mult

  keep <- !is.na(date) & !is.na(value)
  if (!any(keep)) {
    return(tibble::tibble())
  }

  date      <- date[keep]
  value     <- value[keep]
  timestamp <- as.POSIXct(date, tz = "UTC")

  tibble::tibble(
    date       = date,
    timestamp  = timestamp,
    value      = value,
    unit       = pm$unit
  )
}

.si_arso_fetch_daily <- function(x,
                                 station_id,
                                 parameter,
                                 rng,
                                 mode) {
  st_id <- trimws(as.character(station_id))
  if (!nzchar(st_id)) return(tibble::tibble())

  # Years for ARSO request ----------------------------------------------------
  if (identical(mode, "range")) {
    start_year <- as.integer(format(rng$start_date, "%Y"))
    end_year   <- as.integer(format(rng$end_date, "%Y"))
  } else {
    # complete: ask for a broad span and let ARSO / filtering handle details
    start_year <- 1900L
    end_year   <- as.integer(format(Sys.Date(), "%Y"))
  }

  raw_txt <- .si_arso_download_csv(
    x          = x,
    station_id = st_id,
    start_year = start_year,
    end_year   = end_year
  )

  ts_parsed <- .si_arso_parse_daily(
    raw_txt   = raw_txt,
    station_id = st_id,
    parameter = parameter
  )

  if (!NROW(ts_parsed)) {
    return(tibble::tibble())
  }

  date  <- ts_parsed$date
  value <- ts_parsed$value
  unit  <- ts_parsed$unit
  ts    <- ts_parsed$timestamp

  # Filter by requested range if mode = "range" ------------------------------
  if (identical(mode, "range")) {
    keep <- date >= rng$start_date & date <= rng$end_date
    if (!any(keep)) {
      return(tibble::tibble())
    }
    date  <- date[keep]
    value <- value[keep]
    unit  <- unit[keep]
    ts    <- ts[keep]
  }

  tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = st_id,
    parameter     = parameter,
    timestamp     = ts,
    value         = value,
    unit          = unit,
    quality_code  = NA_character_,
    source_url    = x$base_url
  )
}

# -----------------------------------------------------------------------------
# Public time series interface
# -----------------------------------------------------------------------------

#' @export
timeseries.hydro_service_SI_ARSO <- function(x,
                                             parameter = c(
                                               "water_discharge",
                                               "water_level",
                                               "water_temperature",
                                               "suspended_sediment_transport",
                                               "suspended_sediment_concentration"
                                             ),
                                             stations    = NULL,
                                             start_date  = NULL,
                                             end_date    = NULL,
                                             mode        = c("complete", "range"),
                                             exclude_quality = NULL,
                                             ...) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)

  rng <- resolve_dates(mode, start_date, end_date)

  # --------------------------------------------------------------------------
  # station_id vector
  # --------------------------------------------------------------------------
  if (is.null(stations)) {
    st <- stations.hydro_service_SI_ARSO(x)
    station_vec <- st$station_id
  } else {
    station_vec <- stations
  }

  station_vec <- unique(trimws(as.character(station_vec)))
  station_vec <- station_vec[nzchar(station_vec)]

  if (!length(station_vec)) {
    return(tibble::tibble())
  }

  # batching + rate limit -----------------------------------------------------
  batches <- chunk_vec(station_vec, 20L)

  pb <- progress::progress_bar$new(
    total  = length(batches),
    format = "SI_ARSO [:bar] :current/:total (:percent) eta: :eta"
  )

  fetch_one <- function(st_id) {
    .si_arso_fetch_daily(
      x          = x,
      station_id = st_id,
      parameter  = parameter,
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
