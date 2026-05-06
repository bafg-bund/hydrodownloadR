# -----------------------------------------------------------------------------
# ES_CEDEX - CEDEX Anuario de Aforos (CANAL portal)
#   Stations:
#     canal-mapa_gr_cuenca.asp -> canal-mapa_estaciones.asp?gr_cuenca_id=...
#     -> canal-datos.asp?ref_ceh=... (metadata)
#   Timeseries (preferred clean route):
#     canal-datos_anual.asp?ref_ceh=...
#     -> canal-datos_dia.asp?ref_ceh=...&ano_hidr=YYYY (daily table)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Registration
# -----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
register_ES_CEDEX <- function() {
  register_service_usage(
    provider_id   = "ES_CEDEX",
    provider_name = "CEDEX (Anuario de Aforos) - Canales",
    country       = "Spain",
    base_url      = "https://ceh.cedex.es/anuarioaforos/afo/",
    rate_cfg      = list(n = 1L, period = 1),
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_ES_CEDEX <- function(x, ...) {
  c("water_discharge", "water_level")
}

# -----------------------------------------------------------------------------
# small helpers
# -----------------------------------------------------------------------------

.es_or <- function(a, b) if (!is.null(a) && length(a) && !all(is.na(a))) a else b

.es_cedex_base <- function() "https://ceh.cedex.es/anuarioaforos/afo"
.es_cedex_cookie_jar <- function() tempfile("cedex_cookies_", fileext = ".txt")

.es_cedex_req <- function(url, cookie_jar) {
  httr2::request(url) |>
    httr2::req_user_agent("hydrodownloadR (+https://github.com/your-org/hydrodownloadR)") |>
    httr2::req_options(cookiejar = cookie_jar, cookiefile = cookie_jar)
}

.es_cedex_read_html <- function(url, cookie_jar) {
  resp <- perform_request(.es_cedex_req(url, cookie_jar))
  raw  <- httr2::resp_body_raw(resp)

  txt <- rawToChar(raw)

  # CEDEX pages are often ISO-8859-1-ish
  if (!isTRUE(stringi::stri_enc_isutf8(txt))) {
    txt <- iconv(txt, from = "ISO-8859-1", to = "UTF-8")
  }

  xml2::read_html(
    txt,
    options = c("HUGE", "RECOVER", "NOERROR", "NOWARNING")
  )
}

.es_cedex_doc_text <- function(doc) {
  tds <- rvest::html_elements(doc, "td")
  txt <- if (length(tds)) rvest::html_text2(tds) else rvest::html_text2(doc)
  if (length(txt) > 1) txt <- paste(txt, collapse = "\n")

  txt <- gsub("\r", "\n", txt, fixed = TRUE)
  txt <- gsub("[ \t]+", " ", txt)
  txt <- gsub("\n\\s*\n+", "\n", txt)
  trimws(txt)
}

.es_cedex_num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))

.es_cedex_parse_packed_dms <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return(NA_real_)
  if (abs(x) < 180) return(x)

  sgn <- if (x < 0) -1 else 1
  x <- abs(x)

  deg <- floor(x / 10000)
  min <- floor((x %% 10000) / 100)
  sec <- x %% 100

  sgn * (deg + min / 60 + sec / 3600)
}

.es_cedex_ed50_to_wgs84 <- function(lat_ed50, lon_ed50) {
  lat_ed50 <- suppressWarnings(as.numeric(lat_ed50))
  lon_ed50 <- suppressWarnings(as.numeric(lon_ed50))
  if (!is.finite(lat_ed50) || !is.finite(lon_ed50)) return(list(lat = NA_real_, lon = NA_real_))

  if (requireNamespace("sf", quietly = TRUE)) {
    pt <- sf::st_as_sf(
      data.frame(lon = lon_ed50, lat = lat_ed50),
      coords = c("lon", "lat"),
      crs = 4230  # ED50 geographic
    )
    ll <- sf::st_transform(pt, 4326)
    cc <- sf::st_coordinates(ll)
    return(list(lon = cc[1, 1], lat = cc[1, 2]))
  }

  # fallback: keep as-is (still useful)
  list(lon = lon_ed50, lat = lat_ed50)
}

# -----------------------------------------------------------------------------
# Metadata parsing from canal-datos.asp (txt-based, robust for this portal)
# -----------------------------------------------------------------------------

.es_cedex_extract_station_name <- function(txt) {
  m <- stringr::str_match(txt, "Lugar\\s+([^\\n]+?)(?:\\s+Municipio\\s+|\\n|$)")
  if (!is.na(m[1, 2])) return(normalize_utf8(trimws(m[1, 2])))

  m2 <- stringr::str_match(txt, "CANAL\\s+\\d+\\s*:\\s*([^\\n]+)")
  if (!is.na(m2[1, 2])) return(normalize_utf8(trimws(m2[1, 2])))

  NA_character_
}

.es_cedex_extract_corriente <- function(txt) {
  m <- stringr::str_match(txt, "Corriente\\s+([^\\n\\(]+)")
  river <- if (!is.na(m[1, 2])) normalize_utf8(trimws(m[1, 2])) else NA_character_
  list(river = river)
}

.es_cedex_extract_area_km2 <- function(txt, which = c("rio", "canal")) {
  which <- match.arg(which)

  pat <- if (which == "rio") {
    # Accept both "rio" and "r?o" where ? is one optional byte-ish char (covers "río" and common mojibake)
    "Sup\\.\\s*cuenca\\s*r(?:i.{0,2})o:\\s*([0-9\\.,]+)\\s*km2"
  } else {
    "Sup\\.\\s*cuenca\\s*canal:\\s*([0-9\\.,]+)\\s*km2"
  }

  m <- stringr::str_match(txt, pat)
  if (!is.na(m[1, 2])) return(.es_cedex_num(m[1, 2]))
  NA_real_
}

.es_cedex_extract_latlon_from_txt <- function(txt) {
  # find "Latitud\nLongitud" block and read numeric tokens until "Sup"
  start <- regexpr("(?i)\\bLatitud\\b\\s*\\n\\s*\\bLongitud\\b", txt, perl = TRUE)
  if (start[1] < 0) return(list(lat_raw = NA_character_, lon_raw = NA_character_))

  tail_txt <- substr(txt, start[1], nchar(txt))

  stop <- regexpr("(?i)\\n\\s*Sup\\b", tail_txt, perl = TRUE)
  if (stop[1] > 0) {
    tail_txt <- substr(tail_txt, 1, stop[1] - 1)
  }

  nums <- unlist(regmatches(tail_txt, gregexpr("-?\\d+(?:[\\.,]\\d+)?", tail_txt, perl = TRUE)))
  if (length(nums) < 2) return(list(lat_raw = NA_character_, lon_raw = NA_character_))

  # Per your observation: the last two numbers are Lat, Lon
  list(
    lat_raw = nums[length(nums) - 1],
    lon_raw = nums[length(nums)]
  )
}

.es_cedex_latlon_to_wgs84 <- function(lat_raw, lon_raw) {
  lat_ed50 <- .es_cedex_parse_packed_dms(lat_raw)
  lon_ed50 <- .es_cedex_parse_packed_dms(lon_raw)

  # some pages may miss the sign; Spain is mostly negative longitudes
  if (is.finite(lon_ed50) && lon_ed50 > 5.5 && lon_ed50 < 30) lon_ed50 <- -lon_ed50

  .es_cedex_ed50_to_wgs84(lat_ed50, lon_ed50)
}

.es_cedex_canal_metadata <- function(ref_ceh, cookie_jar) {
  url <- paste0(.es_cedex_base(), "/canal-datos.asp?ref_ceh=", ref_ceh)
  doc <- .es_cedex_read_html(url, cookie_jar)
  txt <- .es_cedex_doc_text(doc)

  station_name <- .es_cedex_extract_station_name(txt)
  corr         <- .es_cedex_extract_corriente(txt)

  ll_raw <- .es_cedex_extract_latlon_from_txt(txt)
  ll     <- .es_cedex_latlon_to_wgs84(ll_raw$lat_raw, ll_raw$lon_raw)

  area_rio <- .es_cedex_extract_area_km2(txt, "rio")

  tibble::tibble(
    ref_ceh      = as.character(ref_ceh),
    station_name = station_name,
    river_name   = corr$river,
    lat          = ll$lat,
    lon          = ll$lon,
    area_km2_rio = area_rio
  )
}

# -----------------------------------------------------------------------------
# Crawl: cuencas -> station ids
# -----------------------------------------------------------------------------

.es_cedex_cuencas <- function(cookie_jar) {
  url <- paste0(.es_cedex_base(), "/canal-mapa_gr_cuenca.asp")
  doc <- .es_cedex_read_html(url, cookie_jar)

  a <- rvest::html_elements(doc, "a")
  href <- rvest::html_attr(a, "href")
  txt  <- normalize_utf8(trimws(rvest::html_text2(a)))

  keep <- !is.na(href) & grepl("canal-mapa_estaciones\\.asp\\?gr_cuenca_id=", href)
  href <- href[keep]
  txt  <- txt[keep]

  gr_id <- suppressWarnings(as.integer(sub(".*gr_cuenca_id=([0-9]+).*", "\\1", href)))

  tibble::tibble(
    gr_cuenca_id   = gr_id,
    gr_cuenca_name = txt
  ) |>
    dplyr::filter(!is.na(gr_cuenca_id)) |>
    dplyr::distinct(gr_cuenca_id, .keep_all = TRUE) |>
    dplyr::arrange(gr_cuenca_id)
}

.es_cedex_canal_list_for_cuenca <- function(gr_cuenca_id, cookie_jar) {
  url <- paste0(.es_cedex_base(), "/canal-mapa_estaciones.asp?gr_cuenca_id=", gr_cuenca_id)
  doc <- .es_cedex_read_html(url, cookie_jar)

  a <- rvest::html_elements(doc, "a")
  href <- rvest::html_attr(a, "href")

  keep <- !is.na(href) & grepl("canal-datos\\.asp\\?ref_ceh=", href)
  href <- href[keep]

  ref_ceh <- suppressWarnings(as.integer(sub(".*ref_ceh=([0-9]+).*", "\\1", href)))

  tibble::tibble(ref_ceh = ref_ceh) |>
    dplyr::filter(!is.na(ref_ceh)) |>
    dplyr::distinct(ref_ceh, .keep_all = TRUE) |>
    dplyr::arrange(ref_ceh)
}

# -----------------------------------------------------------------------------
# stations() - defaults to meta and returns only the desired columns in order
# -----------------------------------------------------------------------------

#' @export
stations.hydro_service_ES_CEDEX <- function(x, ..., details = c("meta", "list")) {
  details <- match.arg(details)

  jar <- .es_cedex_cookie_jar()
  cu  <- .es_cedex_cuencas(jar)

  st_list <- lapply(cu$gr_cuenca_id, function(id) {
    one <- .es_cedex_canal_list_for_cuenca(id, jar)
    one$gr_cuenca_id <- id
    one
  }) |>
    dplyr::bind_rows() |>
    dplyr::left_join(cu, by = "gr_cuenca_id")

  # base skeleton
  out <- st_list |>
    dplyr::transmute(
      country            = x$country,
      provider_id        = x$provider_id,
      provider_name      = x$provider_name,
      station_id         = as.character(ref_ceh),
      station_name       = NA_character_,
      station_name_ascii = NA_character_,
      river              = NA_character_,
      river_ascii        = NA_character_,
      lat                = NA_real_,
      lon                = NA_real_,
      area               = NA_real_,
      altitude           = NA_real_
    ) |>
    dplyr::filter(!is.na(station_id) & nzchar(station_id))

  if (identical(details, "list")) {
    return(out |>
             dplyr::select(
               country, provider_id, provider_name, station_id,
               station_name, station_name_ascii,
               river, river_ascii, lat, lon, area, altitude
             )
    )
  }

  meta <- lapply(unique(out$station_id), function(rc) {
    try(.es_cedex_canal_metadata(rc, jar), silent = TRUE)
  })
  meta <- dplyr::bind_rows(meta[!vapply(meta, inherits, logical(1), "try-error")])

  if (!NROW(meta)) {
    return(out |>
             dplyr::select(
               country, provider_id, provider_name, station_id,
               station_name, station_name_ascii,
               river, river_ascii, lat, lon, area, altitude
             )
    )
  }

  out |>
    dplyr::left_join(meta, by = c("station_id" = "ref_ceh")) |>
    dplyr::mutate(
      station_name       = dplyr::coalesce(station_name.y, station_name.x),
      station_name_ascii = to_ascii(dplyr::coalesce(station_name.y, station_name.x)),
      river              = dplyr::coalesce(river_name, river),
      river_ascii        = to_ascii(dplyr::coalesce(river_name, river)),
      lat                = dplyr::coalesce(lat.y, lat.x),
      lon                = dplyr::coalesce(lon.y, lon.x),
      area               = dplyr::coalesce(area_km2_rio, area),
      station_name.x = NULL, station_name.y = NULL,
      lat.x = NULL, lon.x = NULL, lat.y = NULL, lon.y = NULL
    ) |>
    dplyr::select(
      country, provider_id, provider_name, station_id,
      station_name, station_name_ascii,
      river, river_ascii, lat, lon, area, altitude
    )
}

# -----------------------------------------------------------------------------
# Time series via anual -> dia pages (clean GET endpoints)
# -----------------------------------------------------------------------------

.es_cedex_anual_url <- function(ref_ceh) {
  paste0(.es_cedex_base(), "/canal-datos_anual.asp?ref_ceh=", ref_ceh)
}

.es_cedex_dia_url <- function(ref_ceh, ano_hidr) {
  paste0(.es_cedex_base(), "/canal-datos_dia.asp?ref_ceh=", ref_ceh, "&ano_hidr=", ano_hidr)
}

# Spain hydrological year: starts Oct 1
.es_cedex_hy_start_year <- function(d) {
  y <- as.integer(format(d, "%Y"))
  m <- as.integer(format(d, "%m"))
  ifelse(m >= 10L, y, y - 1L)
}

.es_cedex_available_hy <- function(ref_ceh, jar, read_fun) {
  doc <- read_fun(.es_cedex_anual_url(ref_ceh))

  a <- rvest::html_elements(doc, "a")
  href <- rvest::html_attr(a, "href")

  keep <- !is.na(href) & grepl("canal-datos_dia\\.asp\\?", href) & grepl("ano_hidr=", href)
  href <- href[keep]
  if (!length(href)) return(integer())

  yrs <- suppressWarnings(as.integer(sub(".*ano_hidr=([0-9]{4}).*", "\\1", href)))
  yrs <- yrs[!is.na(yrs)]
  sort(unique(yrs))
}

.es_cedex_pick_daily_table <- function(doc) {
  tabs <- rvest::html_elements(doc, "table")
  if (!length(tabs)) return(NULL)

  # Cheap per-table metadata: header text (first 1-2 rows) + number of <tr>
  info <- lapply(tabs, function(tb) {
    # count rows without parsing table
    tr_n <- length(xml2::xml_find_all(tb, ".//tr"))

    # header-like text from first 2 rows only (fast)
    hdr_nodes <- xml2::xml_find_all(tb, ".//tr[position() <= 2]/*[self::th or self::td]")
    hdr_txt <- paste(xml2::xml_text(hdr_nodes), collapse = " ")
    hdr_txt <- toupper(to_ascii(normalize_utf8(trimws(hdr_txt))))

    # score: must contain DIA + CAUDAL ideally; ALTURA is optional
    score <- 0L
    if (grepl("\\bDIA\\b", hdr_txt))    score <- score + 100L
    if (grepl("\\bCAUDAL\\b", hdr_txt)) score <- score + 100L
    if (grepl("\\bALTURA\\b", hdr_txt)) score <- score + 20L

    # prefer larger tables among candidates
    score <- score + min(tr_n, 400L)

    list(score = score, tr_n = tr_n, hdr = hdr_txt)
  })

  scores <- vapply(info, `[[`, integer(1), "score")
  tr_ns  <- vapply(info, `[[`, integer(1), "tr_n")

  # Prefer tables that look like the daily table (score >= 200 means DIA+CAUDAL found)
  cand <- which(scores >= 200L)
  if (length(cand)) {
    # among candidates: pick best score, then max rows
    best <- cand[order(scores[cand], tr_ns[cand], decreasing = TRUE)][1]
    return(tabs[[best]])
  }

  # Fallback: pick the largest table by row count
  tabs[[which.max(tr_ns)]]
}

.es_cedex_parse_dia_doc <- function(doc) {
  tb <- .es_cedex_pick_daily_table(doc)
  if (is.null(tb)) return(tibble::tibble())

  df <- rvest::html_table(tb, fill = TRUE)
  if (is.null(df) || !NROW(df)) return(tibble::tibble())

  if (is.null(names(df)) || anyNA(names(df))) {
    names(df) <- paste0("X", seq_len(NCOL(df)))
  }

  to_num <- function(x) {
    x <- trimws(as.character(x))
    x[x %in% c("", "\u00A0", "NA", "NaN")] <- NA_character_
    suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
  }

  out <- tibble::tibble()

  x1 <- trimws(as.character(df[[1]]))
  is_date <- grepl("^\\d{2}/\\d{2}/\\d{4}$", x1)

  if (sum(is_date, na.rm = TRUE) >= 2) {
    date <- suppressWarnings(as.Date(x1[is_date], format = "%d/%m/%Y"))
    h    <- to_num(df[[2]][is_date])
    q    <- to_num(df[[3]][is_date])

    out <- tibble::tibble(
      date            = date,
      water_level     = h,
      water_discharge = q
    ) |>
      dplyr::filter(!is.na(date))

    return(out)
  }

  # fallback triplets
  row1 <- trimws(as.character(unlist(df[1, , drop = TRUE], use.names = FALSE)))
  idx0 <- which(grepl("^\\d{2}/\\d{2}/\\d{4}$", row1))[1]
  if (is.na(idx0)) return(tibble::tibble())

  tokens <- row1[idx0:length(row1)]
  n3 <- floor(length(tokens) / 3)
  if (n3 < 1) return(tibble::tibble())

  dates <- tokens[seq(1, 3 * n3, by = 3)]
  alt   <- tokens[seq(2, 3 * n3, by = 3)]
  q     <- tokens[seq(3, 3 * n3, by = 3)]

  ok <- grepl("^\\d{2}/\\d{2}/\\d{4}$", dates)
  if (!any(ok)) return(tibble::tibble())

  tibble::tibble(
    date            = suppressWarnings(as.Date(dates[ok], "%d/%m/%Y")),
    water_level     = to_num(alt[ok]),
    water_discharge = to_num(q[ok])
  ) |>
    dplyr::filter(!is.na(date))
}



.es_cedex_fetch_station_via_dia <- function(x, ref_ceh, rng, mode, parameter) {
  ref_ceh <- trimws(as.character(ref_ceh))
  jar <- .es_cedex_cookie_jar()

  # rate-limit but ALSO catch parser failures per-page
  read_fun <- ratelimitr::limit_rate(
    function(url) {
      try(.es_cedex_read_html(url, jar), silent = TRUE)
    },
    rate = ratelimitr::rate(
      n      = x$rate_cfg$n %||% 1L,
      period = x$rate_cfg$period %||% 1
    )
  )

  # annual page
  doc_a <- read_fun(.es_cedex_anual_url(ref_ceh))
  if (inherits(doc_a, "try-error") || is.null(doc_a)) return(tibble::tibble())

  years_avail <- .es_cedex_available_hy(ref_ceh, jar, read_fun = function(...) doc_a)
  # ^ we already fetched anual doc; .es_cedex_available_hy expects a doc from read_fun
  # easiest: just re-implement years extraction here to avoid another fetch
  a <- rvest::html_elements(doc_a, "a")
  href <- rvest::html_attr(a, "href")
  keep <- !is.na(href) & grepl("canal-datos_dia\\.asp\\?", href) & grepl("ano_hidr=", href)
  href <- href[keep]
  years_avail <- suppressWarnings(as.integer(sub(".*ano_hidr=([0-9]{4}).*", "\\1", href)))
  years_avail <- sort(unique(years_avail[!is.na(years_avail)]))

  if (!length(years_avail)) return(tibble::tibble())

  years_need <- if (identical(mode, "range")) {
    y0 <- .es_cedex_hy_start_year(rng$start_date)
    y1 <- .es_cedex_hy_start_year(rng$end_date)
    intersect(years_avail, seq.int(y0, y1))
  } else {
    years_avail
  }
  if (!length(years_need)) return(tibble::tibble())

  dat_list <- lapply(years_need, function(y) {
    url <- .es_cedex_dia_url(ref_ceh, y)
    doc <- read_fun(url)
    if (inherits(doc, "try-error") || is.null(doc)) return(NULL)

    df <- .es_cedex_parse_dia_doc(doc)
    if (!NROW(df)) return(NULL)
    df$source_url <- url
    df
  })

  dat <- dplyr::bind_rows(dat_list)
  if (!NROW(dat)) return(tibble::tibble())

  if (identical(mode, "range")) {
    dat <- dat |>
      dplyr::filter(date >= rng$start_date, date <= rng$end_date)
    if (!NROW(dat)) return(tibble::tibble())
  }

  out <- list()

  if ("water_discharge" %in% parameter) {
    out[["water_discharge"]] <- tibble::tibble(
      parameter  = "water_discharge",
      date       = dat$date,
      value      = dat$water_discharge,
      unit       = "m^3/s",
      source_url = dat$source_url
    )
  }

  if ("water_level" %in% parameter) {
    out[["water_level"]] <- tibble::tibble(
      parameter  = "water_level",
      date       = dat$date,
      value      = dat$water_level,
      unit       = "m",
      source_url = dat$source_url
    )
  }

  res <- dplyr::bind_rows(out) |>
    dplyr::filter(!is.na(value))

  if (!NROW(res)) return(tibble::tibble())

  tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = ref_ceh,
    parameter     = res$parameter,
    timestamp     = as.POSIXct(res$date, tz = "UTC"),
    value         = res$value,
    unit          = res$unit,
    quality_code  = NA_character_,
    quality_name  = NA_character_,
    quality_desc  = NA_character_,
    source_url    = res$source_url
  )
}

# -----------------------------------------------------------------------------
# Public timeseries method
# -----------------------------------------------------------------------------

#' @export
timeseries.hydro_service_ES_CEDEX <- function(x,
                                              parameter = c("water_discharge", "water_level"),
                                              stations    = NULL,
                                              start_date  = NULL,
                                              end_date    = NULL,
                                              mode        = c("complete", "range"),
                                              exclude_quality = NULL,
                                              ...) {
  mode <- match.arg(mode)

  parameter <- intersect(parameter, timeseries_parameters.hydro_service_ES_CEDEX(x))
  if (!length(parameter)) return(tibble::tibble())

  rng <- resolve_dates(mode, start_date, end_date)

  station_vec <- if (is.null(stations)) {
    stations.hydro_service_ES_CEDEX(x, details = "list")$station_id
  } else {
    stations
  }

  station_vec <- unique(trimws(as.character(station_vec)))
  station_vec <- station_vec[nzchar(station_vec)]
  if (!length(station_vec)) return(tibble::tibble())

  batches <- chunk_vec(station_vec, 20L)

  pb <- progress::progress_bar$new(
    total  = length(batches),
    format = "ES_CEDEX [:bar] :current/:total (:percent) eta: :eta"
  )

  out <- lapply(batches, function(batch) {
    pb$tick()
    dplyr::bind_rows(lapply(batch, function(st_id) {
      .es_cedex_fetch_station_via_dia(
        x         = x,
        ref_ceh   = st_id,
        rng       = rng,
        mode      = mode,
        parameter = parameter
      )
    }))
  })

  res <- dplyr::bind_rows(out)

  if (!NROW(res)) return(tibble::tibble())

  # Convert Date -> POSIXct (UTC)
  ts_parsed <- as.POSIXct(res$timestamp, tz = "UTC")

  base <- tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = res$station_id,              # <-- your station id variable in this function
    parameter     = res$parameter,
    timestamp     = ts_parsed,
    value         = res$value,
    unit          = res$unit,
    quality_code  = NA_character_,
    quality_name  = NA_character_,
    quality_desc  = NA_character_,
    source_url    = res$source_url        # keep per-row year URL (best provenance)
  )
  dplyr::arrange(base, station_id, parameter, timestamp)
}

