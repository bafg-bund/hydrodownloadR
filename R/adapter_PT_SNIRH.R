# R/adapter_PT_SNIRH.R
# Agencia Portuguesa do Ambiente - SNIRH

# -- Registration --------------------------------------------------------------

#' @keywords internal
#' @noRd
register_PT_SNIRH <- function() {
  register_service_usage(
    provider_id   = "PT_SNIRH",
    provider_name = "Agencia Portuguesa do Ambiente - SNIRH",
    country       = "Portugal",
    base_url      = "https://snirh.apambiente.pt",
    rate_cfg      = list(n = 1, period = 1),
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_PT_SNIRH <- function(x, ...) {
  c("water_discharge", "water_level")
}

# -- Constants -----------------------------------------------------------------

.pt_snirh_network_id <- "920123705" # Rede Hidrometrica
.pt_snirh_first_date <- as.Date("1899-12-30")
.pt_snirh_tz <- "Europe/Lisbon"

.pt_snirh_paths <- list(
  home       = "/index.php",
  stations   = "/snirh/_dadosbase/site/xml/xml_listaestacoes.php",
  details    = "/snirh/_dadosbase/site/janela.php",
  parameters = "/snirh/_dadosbase/site/_ajax_listaparscomdados.php",
  data       = "/snirh/_dadosbase/site/janela_verdados.php"
)

# -- General helpers -----------------------------------------------------------

.pt_empty_stations <- function() {
  tibble::tibble(
    country       = character(),
    provider_id   = character(),
    provider_name = character(),
    station_id    = character(),
    org_id        = character(),
    station_name  = character(),
    river         = character(),
    lat           = numeric(),
    lon           = numeric(),
    area          = numeric(),
    altitude      = numeric()
  )
}

.pt_empty_timeseries <- function() {
  tibble::tibble(
    country       = character(),
    provider_id   = character(),
    provider_name = character(),
    station_id    = character(),
    parameter     = character(),
    timestamp     = as.POSIXct(character(), tz = .pt_snirh_tz),
    value         = numeric(),
    unit          = character(),
    quality_code  = character(),
    quality_name  = character(),
    quality_desc  = character(),
    source_url    = character()
  )
}

.pt_check_response <- function(resp, what) {
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    rlang::abort(
      paste0(
        "PT_SNIRH ", what, " failed with HTTP status ", status, "."
      )
    )
  }
  invisible(resp)
}

.pt_limited_perform <- function(x) {
  ratelimitr::limit_rate(
    function(req) perform_request(req),
    rate = ratelimitr::rate(
      n = x$rate_cfg$n,
      period = x$rate_cfg$period
    )
  )
}

.pt_request <- function(x, path, cookie_file = NULL, query = NULL) {
  req <- build_request(x, path = path)

  if (!is.null(query) && length(query)) {
    req <- do.call(
      httr2::req_url_query,
      c(list(.req = req), query)
    )
  }

  if (!is.null(cookie_file)) {
    req <- httr2::req_cookie_preserve(req, cookie_file)
  }

  httr2::req_headers(
    req,
    `Accept-Language` = "pt-PT,pt;q=0.9,en;q=0.7"
  )
}

.pt_read_html <- function(resp) {
  xml2::read_html(
    httr2::resp_body_raw(resp),
    options = c("RECOVER", "NOERROR", "NOWARNING")
  )
}

.pt_clean_text <- function(x) {
  x <- normalize_utf8(as.character(x))
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  trimws(gsub("[[:space:]]+", " ", x))
}

.pt_ascii <- function(x) {
  out <- iconv(.pt_clean_text(x), from = "", to = "ASCII//TRANSLIT")
  out[is.na(out)] <- .pt_clean_text(x)[is.na(out)]
  out
}

.pt_key <- function(x) {
  x <- toupper(.pt_ascii(x))
  x <- gsub("[^A-Z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

.pt_normalized_name <- function(x) {
  x <- tolower(.pt_ascii(x))
  trimws(gsub("[^a-z0-9]+", " ", x))
}

.pt_num_one <- function(x) {
  if (is.null(x) || !length(x) || is.na(x)) {
    return(NA_real_)
  }

  x <- trimws(as.character(x))
  x <- gsub("\u00a0", "", x, fixed = TRUE)
  x <- gsub("[[:space:]]+", "", x)
  x <- gsub("[^0-9,.+-]", "", x)

  if (!nzchar(x) || x %in% c("-", "+", ".", ",")) {
    return(NA_real_)
  }

  has_comma <- grepl(",", x, fixed = TRUE)
  has_dot   <- grepl(".", x, fixed = TRUE)

  if (has_comma && has_dot) {
    comma_pos <- max(gregexpr(",", x, fixed = TRUE)[[1]])
    dot_pos   <- max(gregexpr(".", x, fixed = TRUE)[[1]])

    if (comma_pos > dot_pos) {
      x <- gsub(".", "", x, fixed = TRUE)
      x <- sub(",", ".", x, fixed = TRUE)
    } else {
      x <- gsub(",", "", x, fixed = TRUE)
    }
  } else if (has_comma) {
    x <- sub(",", ".", x, fixed = TRUE)
  }

  suppressWarnings(as.numeric(x))
}

.pt_num <- function(x) {
  unname(vapply(x, .pt_num_one, numeric(1)))
}

.pt_get_col <- function(df, keys) {
  if (!nrow(df)) {
    return(character())
  }

  idx <- match(keys, .pt_key(names(df)))
  idx <- idx[!is.na(idx)]

  if (!length(idx)) {
    return(rep(NA_character_, nrow(df)))
  }

  as.character(df[[idx[[1]]]])
}

.pt_date <- function(x) {
  if (is.null(x) || !length(x)) {
    return(NULL)
  }
  if (inherits(x, "Date")) {
    return(x[[1]])
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x[[1]], tz = .pt_snirh_tz))
  }
  as.Date(x[[1]])
}

.pt_source_url <- function(x, org_id, parameter_id, date_from, date_to) {
  paste0(
    x$base_url,
    .pt_snirh_paths$data,
    "?sites=", utils::URLencode(as.character(org_id), reserved = TRUE),
    "&pars=", utils::URLencode(as.character(parameter_id), reserved = TRUE),
    "&tmin=", utils::URLencode(format(date_from, "%d/%m/%Y"), reserved = TRUE),
    "&tmax=", utils::URLencode(format(date_to, "%d/%m/%Y"), reserved = TRUE)
  )
}

# -- SNIRH session -------------------------------------------------------------

# SNIRH keeps the selected monitoring network in a PHP session.  httr2 starts
# every request with a clean cookie state, so all requests in one adapter call
# must share the same cookie file.
.pt_open_session <- function(x, perform) {
  cookie_file <- tempfile("hydrodownloadR_PT_SNIRH_", fileext = ".cookies")
  ok <- FALSE
  on.exit({
    if (!ok && file.exists(cookie_file)) {
      unlink(cookie_file)
    }
  }, add = TRUE)

  home_query <- list(idMain = 2, idItem = 1)

  req <- .pt_request(
    x,
    path = .pt_snirh_paths$home,
    cookie_file = cookie_file,
    query = home_query
  )
  resp <- perform(req)
  .pt_check_response(resp, "session initialization")

  req <- .pt_request(
    x,
    path = .pt_snirh_paths$home,
    cookie_file = cookie_file,
    query = home_query
  )

  req <- do.call(
    httr2::req_body_form,
    c(
      list(.req = req),
      stats::setNames(
        list(.pt_snirh_network_id, 1),
        c("f_redes_seleccao[]", "aplicar_filtro")
      )
    )
  )

  selected_network_resp <- perform(req)
  .pt_check_response(selected_network_resp, "hydrometric-network selection")

  ok <- TRUE
  list(
    cookie_file = cookie_file,
    selected_network_resp = selected_network_resp
  )
}

.pt_close_session <- function(session) {
  if (
    !is.null(session$cookie_file) &&
    file.exists(session$cookie_file)
  ) {
    unlink(session$cookie_file)
  }
  invisible(NULL)
}

# -- Station parsing -----------------------------------------------------------

.pt_station_code_from_label <- function(x) {
  x <- .pt_clean_text(x)
  out <- sub("^.*\\(([^()]*)\\)[[:space:]]*$", "\\1", x, perl = TRUE)
  out[!grepl("\\([^()]*\\)[[:space:]]*$", x, perl = TRUE)] <- NA_character_
  .pt_clean_text(out)
}

.pt_station_name_from_label <- function(x) {
  x <- gsub("\u25a0", "", .pt_clean_text(x), fixed = TRUE)
  .pt_clean_text(sub("[[:space:]]*\\([^()]*\\)[[:space:]]*$", "", x))
}

.pt_parse_station_markers <- function(doc, selected_network_doc = NULL) {
  markers <- xml2::xml_find_all(doc, ".//marker[@site]")

  if (length(markers)) {
    label <- xml2::xml_attr(markers, "estacao")
    return(tibble::tibble(
      station_id   = .pt_station_code_from_label(label),
      org_id       = as.character(xml2::xml_attr(markers, "site")),
      station_name = .pt_station_name_from_label(label),
      lat           = .pt_num(xml2::xml_attr(markers, "lat")),
      lon           = .pt_num(xml2::xml_attr(markers, "lng"))
    ))
  }

  if (is.null(selected_network_doc)) {
    return(tibble::tibble(
      station_id = character(), org_id = character(),
      station_name = character(), lat = numeric(), lon = numeric()
    ))
  }

  options <- xml2::xml_find_all(
    selected_network_doc,
    ".//select[@name='f_estacoes[]']/option[@value]"
  )

  if (!length(options)) {
    return(tibble::tibble(
      station_id = character(), org_id = character(),
      station_name = character(), lat = numeric(), lon = numeric()
    ))
  }

  label <- xml2::xml_text(options)
  tibble::tibble(
    station_id   = .pt_station_code_from_label(label),
    org_id       = as.character(xml2::xml_attr(options, "value")),
    station_name = .pt_station_name_from_label(label),
    lat           = NA_real_,
    lon           = NA_real_
  )
}

.pt_parse_details_table <- function(doc) {
  tables <- xml2::xml_find_all(
    doc,
    ".//table[.//thead and .//tbody]"
  )

  if (!length(tables)) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  header_count <- vapply(
    tables,
    function(tab) {
      length(xml2::xml_find_all(
        tab,
        ".//thead[1]//tr[1]/*[self::td or self::th]"
      ))
    },
    integer(1)
  )

  table <- tables[[which.max(header_count)]]
  headers <- .pt_clean_text(xml2::xml_text(xml2::xml_find_all(
    table,
    ".//thead[1]//tr[1]/*[self::td or self::th]"
  )))
  rows <- xml2::xml_find_all(table, ".//tbody[1]/tr")

  if (!length(headers) || !length(rows)) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  values <- lapply(rows, function(row) {
    cells <- .pt_clean_text(xml2::xml_text(
      xml2::xml_find_all(row, "./td|./th")
    ))
    length(cells) <- length(headers)
    cells
  })

  out <- as.data.frame(
    do.call(rbind, values),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(out) <- headers
  out
}

.pt_station_catalogue <- function(x, session, perform) {
  marker_req <- .pt_request(
    x,
    path = .pt_snirh_paths$stations,
    cookie_file = session$cookie_file
  )
  marker_resp <- perform(marker_req)
  .pt_check_response(marker_resp, "station-list request")

  selected_doc <- .pt_read_html(session$selected_network_resp)
  markers <- .pt_parse_station_markers(
    .pt_read_html(marker_resp),
    selected_network_doc = selected_doc
  )

  details_req <- .pt_request(
    x,
    path = .pt_snirh_paths$details,
    cookie_file = session$cookie_file,
    query = list(obj_janela = "INFO_ESTACOES")
  )
  details_resp <- perform(details_req)
  .pt_check_response(details_resp, "station-metadata request")
  details <- .pt_parse_details_table(.pt_read_html(details_resp))

  if (!nrow(details)) {
    if (!nrow(markers)) {
      return(.pt_empty_stations())
    }

    return(tibble::tibble(
      country       = x$country,
      provider_id   = x$provider_id,
      provider_name = x$provider_name,
      station_id    = markers$station_id,
      org_id        = markers$org_id,
      station_name  = markers$station_name,
      river         = NA_character_,
      lat           = markers$lat,
      lon           = markers$lon,
      area          = NA_real_,
      altitude      = NA_real_
    ))
  }

  station_id   <- .pt_clean_text(.pt_get_col(details, "CODIGO"))
  station_name <- .pt_clean_text(.pt_get_col(details, "NOME"))
  lat          <- .pt_num(.pt_get_col(
    details,
    c("LATITUDE_N", "LATITUDE_ON", "LATITUDE")
  ))
  lon          <- .pt_num(.pt_get_col(
    details,
    c("LONGITUDE_W", "LONGITUDE_OW", "LONGITUDE")
  ))
  altitude     <- .pt_num(.pt_get_col(details, c("ALTITUDE_M", "ALTITUDE")))
  river        <- .pt_clean_text(.pt_get_col(details, "RIO"))
  river[
    is.na(river) |
      !nzchar(river) |
      river == "-"
  ] <- NA_character_
  area         <- .pt_num(.pt_get_col(
    details,
    c("AREA_DRENADA_KM2", "AREA_DRENADA")
  ))

  marker_match <- match(station_id, markers$station_id)
  org_id <- markers$org_id[marker_match]

  missing_name <- is.na(station_name) | !nzchar(station_name)
  station_name[missing_name] <- markers$station_name[marker_match[missing_name]]
  lat[is.na(lat)] <- markers$lat[marker_match[is.na(lat)]]
  lon[is.na(lon)] <- markers$lon[marker_match[is.na(lon)]]

  missing_uid <- sum(is.na(org_id) | !nzchar(org_id))
  if (missing_uid) {
    rlang::warn(
      paste0(
        "PT_SNIRH: ", missing_uid,
        " station(s) have metadata but no internal site id. ",
        "They are returned by stations() but cannot be used by timeseries()."
      )
    )
  }

  out <- tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = station_id,
    org_id        = as.character(org_id),
    station_name  = station_name,
    river         = river,
    lat           = lat,
    lon           = lon,
    area          = area,
    altitude      = altitude
  )

  out <- out[
    !is.na(out$station_id) &
      nzchar(out$station_id) &
      !duplicated(out$station_id),
    ,
    drop = FALSE
  ]
  rownames(out) <- NULL
  out
}

# -- Stations (S3 method) ------------------------------------------------------

#' @export
stations.hydro_service_PT_SNIRH <- function(x, ...) {
  perform <- .pt_limited_perform(x)
  session <- .pt_open_session(x, perform)
  on.exit(.pt_close_session(session), add = TRUE)
  .pt_station_catalogue(x, session, perform)
}

# -- Parameter discovery -------------------------------------------------------

.pt_available_parameters <- function(x, org_id, session, perform) {
  req <- .pt_request(
    x,
    path = .pt_snirh_paths$parameters,
    cookie_file = session$cookie_file,
    query = list(sites = as.character(org_id))
  )
  resp <- perform(req)
  .pt_check_response(resp, "parameter-list request")

  doc <- .pt_read_html(resp)
  options <- xml2::xml_find_all(doc, ".//option[@value]")

  if (!length(options)) {
    return(tibble::tibble(
      parameter_id = character(),
      parameter_name = character()
    ))
  }

  tibble::tibble(
    parameter_id = as.character(xml2::xml_attr(options, "value")),
    parameter_name = .pt_clean_text(
      gsub("\u25a0", "", xml2::xml_text(options), fixed = TRUE)
    )
  )
}

.pt_select_parameter <- function(parameters, parameter) {
  if (!nrow(parameters)) {
    return(NULL)
  }

  name <- .pt_normalized_name(parameters$parameter_name)
  excluded <- grepl(
    "\\b(anual|mensal|maximo|minimo|maxima|minima)\\b",
    name,
    perl = TRUE
  )
  rank <- rep(Inf, length(name))

  if (parameter == "water_level") {
    rank[grepl("\\bnivel medio diario\\b", name, perl = TRUE)] <- 1
    rank[
      is.infinite(rank) &
        grepl("\\bnivel hidrometrico instantaneo\\b", name, perl = TRUE)
    ] <- 2
    rank[
      is.infinite(rank) &
        grepl("\\bnivel instantaneo\\b", name, perl = TRUE)
    ] <- 3
    rank[
      is.infinite(rank) &
        grepl("\\bnivel\\b", name, perl = TRUE) &
        !excluded
    ] <- 4
  } else {
    rank[grepl("\\bcaudal medio diario\\b", name, perl = TRUE)] <- 1
    rank[
      is.infinite(rank) &
        grepl("\\bcaudal diario\\b", name, perl = TRUE)
    ] <- 2
    rank[
      is.infinite(rank) &
        grepl("\\bcaudal instantaneo\\b", name, perl = TRUE)
    ] <- 3
    rank[
      is.infinite(rank) &
        grepl("\\bcaudal\\b", name, perl = TRUE) &
        !excluded
    ] <- 4
  }

  if (all(is.infinite(rank))) {
    return(NULL)
  }

  parameters[which.min(rank), , drop = FALSE]
}

# -- Time-series parsing -------------------------------------------------------

.pt_extract_flag <- function(x) {
  match <- regexec("\\(([[:alnum:]_-]+)\\)", x, perl = TRUE)
  hit <- regmatches(x, match)
  unname(vapply(
    hit,
    function(z) if (length(z) >= 2) tolower(z[[2]]) else NA_character_,
    character(1)
  ))
}

.pt_quality_name <- c(
  vc = "calculated_missing_input",
  vd = "calculated_differs_from_input"
)

.pt_quality_desc <- c(
  vc = "Calculated because the entered value is missing.",
  vd = "Calculated value differs from the entered value."
)

.pt_quality_legend <- function(doc) {
  node <- xml2::xml_find_first(
    doc,
    paste0(
      ".//*[",
      "contains(concat(' ', normalize-space(@class), ' '), ",
      "' flags_nos_dados ')",
      "]"
    )
  )

  if (inherits(node, "xml_missing")) {
    return(character())
  }

  text <- .pt_clean_text(xml2::xml_text(node))
  starts <- gregexpr(
    "\\(([[:alnum:]_-]+)\\)\\s*:",
    text,
    perl = TRUE
  )[[1]]

  if (!length(starts) || starts[[1]] < 0) {
    return(character())
  }

  ends <- c(starts[-1] - 1L, nchar(text))
  entries <- substring(text, starts, ends)
  codes <- tolower(sub(
    "^\\(([[:alnum:]_-]+)\\).*$",
    "\\1",
    entries,
    perl = TRUE
  ))
  descriptions <- trimws(sub(
    "^\\([[:alnum:]_-]+\\)\\s*:\\s*",
    "",
    entries,
    perl = TRUE
  ))

  keep <- nzchar(codes) & nzchar(descriptions) & !duplicated(codes)
  stats::setNames(descriptions[keep], codes[keep])
}

.pt_extract_unit <- function(series_header, fallback) {
  hit <- regexec("\\(([^()]*)\\)[[:space:]]*$", series_header, perl = TRUE)
  unit <- regmatches(series_header, hit)[[1]]

  if (length(unit) < 2 || !nzchar(trimws(unit[[2]]))) {
    return(fallback)
  }

  unit <- trimws(unit[[2]])
  normalized <- gsub("<c2><b3>", "3", unit, fixed = TRUE)
  normalized <- tolower(gsub("[[:space:]^]", "", normalized))

  if (normalized %in% c("m3/s", "m3s-1", "m3s")) {
    return("m^3/s")
  }
  unit
}

.pt_parse_timestamp <- function(x) {
  x <- .pt_clean_text(x)
  out <- as.POSIXct(
    x,
    format = "%d/%m/%Y %H:%M",
    tz = .pt_snirh_tz
  )
  missing <- is.na(out)
  if (any(missing)) {
    out[missing] <- as.POSIXct(
      x[missing],
      format = "%d/%m/%Y",
      tz = .pt_snirh_tz
    )
  }
  out
}

.pt_parse_timeseries <- function(
    resp,
    parameter,
    fallback_unit,
    source_url
) {
  doc <- .pt_read_html(resp)
  tables <- xml2::xml_find_all(
    doc,
    paste0(
      ".//table[.//td[",
      "contains(concat(' ', normalize-space(@class), ' '), ' tbl_val ')",
      "]]"
    )
  )

  if (!length(tables)) {
    return(.pt_empty_timeseries())
  }

  table <- tables[[length(tables)]]
  header <- .pt_clean_text(xml2::xml_text(xml2::xml_find_all(
    table,
    paste0(
      ".//td[",
      "contains(concat(' ', normalize-space(@class), ' '), ' tbl_tit ')",
      "]"
    )
  )))
  series_header <- if (length(header)) header[[length(header)]] else ""
  unit <- .pt_extract_unit(series_header, fallback_unit)

  rows <- xml2::xml_find_all(
    table,
    paste0(
      ".//tr[td[",
      "contains(concat(' ', normalize-space(@class), ' '), ' tbl_val ')",
      "]]"
    )
  )

  if (!length(rows)) {
    return(.pt_empty_timeseries())
  }

  parsed <- lapply(rows, function(row) {
    cells <- .pt_clean_text(xml2::xml_text(
      xml2::xml_find_all(row, "./td")
    ))
    if (length(cells) < 2) {
      return(NULL)
    }
    list(timestamp = cells[[1]], value = cells[[2]])
  })
  parsed <- Filter(Negate(is.null), parsed)

  if (!length(parsed)) {
    return(.pt_empty_timeseries())
  }

  timestamp_raw <- vapply(parsed, `[[`, character(1), "timestamp")
  value_raw     <- vapply(parsed, `[[`, character(1), "value")
  quality_code <- .pt_extract_flag(value_raw)
  value <- .pt_num(gsub("\\([^)]*\\)", "", value_raw, perl = TRUE))
  timestamp <- .pt_parse_timestamp(timestamp_raw)

  keep <- !is.na(timestamp) & is.finite(value)
  if (!any(keep)) {
    return(.pt_empty_timeseries())
  }

  quality_name <- unname(.pt_quality_name[quality_code])
  quality_desc <- unname(.pt_quality_legend(doc)[quality_code])
  missing_desc <- is.na(quality_desc) | !nzchar(quality_desc)
  quality_desc[missing_desc] <- unname(
    .pt_quality_desc[quality_code[missing_desc]]
  )

  tibble::tibble(
    timestamp    = timestamp[keep],
    value        = value[keep],
    unit         = rep(unit, sum(keep)),
    quality_code = quality_code[keep],
    quality_name = quality_name[keep],
    quality_desc = quality_desc[keep],
    source_url   = rep(source_url, sum(keep))
  )
}

.pt_resolve_stations <- function(x, requested, catalogue) {
  if (is.null(requested) || !length(requested)) {
    return(catalogue[
      !is.na(catalogue$org_id) & nzchar(catalogue$org_id),
      c("station_id", "org_id"),
      drop = FALSE
    ])
  }

  requested <- unique(.pt_clean_text(requested))
  requested <- requested[!is.na(requested) & nzchar(requested)]
  if (!length(requested)) {
    return(catalogue[FALSE, c("station_id", "org_id"), drop = FALSE])
  }
  code_match <- match(toupper(requested), toupper(catalogue$station_id))
  uid_match  <- match(requested, catalogue$org_id)
  idx <- ifelse(!is.na(code_match), code_match, uid_match)

  missing <- requested[is.na(idx)]
  if (length(missing)) {
    rlang::warn(
      paste0(
        "PT_SNIRH: unknown station id(s) skipped: ",
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
    return(catalogue[FALSE, c("station_id", "org_id"), drop = FALSE])
  }

  out <- catalogue[idx, c("station_id", "org_id"), drop = FALSE]
  out <- out[!duplicated(out$station_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# -- Time series (S3 method) ---------------------------------------------------

#' @export
timeseries.hydro_service_PT_SNIRH <- function(
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

  date_from <- .pt_date(rng$start_date)
  date_to   <- .pt_date(rng$end_date)
  if (is.null(date_from)) date_from <- .pt_snirh_first_date
  if (is.null(date_to)) date_to <- as.Date(Sys.time(), tz = .pt_snirh_tz)

  if (date_from > date_to) {
    rlang::abort("PT_SNIRH: start_date must not be after end_date.")
  }

  fallback_unit <- if (parameter == "water_discharge") "m^3/s" else "m"
  perform <- .pt_limited_perform(x)
  session <- .pt_open_session(x, perform)
  on.exit(.pt_close_session(session), add = TRUE)

  catalogue <- .pt_station_catalogue(x, session, perform)
  targets <- .pt_resolve_stations(x, stations, catalogue)
  if (!nrow(targets)) {
    return(.pt_empty_timeseries())
  }

  no_parameter <- new.env(parent = emptyenv())
  no_data <- new.env(parent = emptyenv())

  fetch_one <- function(i) {
    station_id <- targets$station_id[[i]]
    org_id <- targets$org_id[[i]]

    available <- try(
      .pt_available_parameters(x, org_id, session, perform),
      silent = TRUE
    )
    if (inherits(available, "try-error")) {
      no_parameter[[station_id]] <- TRUE
      return(.pt_empty_timeseries())
    }

    selected <- .pt_select_parameter(available, parameter)
    if (is.null(selected) || !nrow(selected)) {
      no_parameter[[station_id]] <- TRUE
      return(.pt_empty_timeseries())
    }

    parameter_id <- selected$parameter_id[[1]]
    source_url <- .pt_source_url(
      x,
      org_id = org_id,
      parameter_id = parameter_id,
      date_from = date_from,
      date_to = date_to
    )

    req <- .pt_request(
      x,
      path = .pt_snirh_paths$data,
      cookie_file = session$cookie_file,
      query = list(
        sites = org_id,
        pars = parameter_id,
        tmin = format(date_from, "%d/%m/%Y"),
        tmax = format(date_to, "%d/%m/%Y")
      )
    )

    resp <- try(perform(req), silent = TRUE)
    if (inherits(resp, "try-error") || httr2::resp_status(resp) >= 400) {
      no_data[[station_id]] <- TRUE
      return(.pt_empty_timeseries())
    }

    d <- .pt_parse_timeseries(
      resp,
      parameter = parameter,
      fallback_unit = fallback_unit,
      source_url = source_url
    )
    if (!nrow(d)) {
      no_data[[station_id]] <- TRUE
      return(.pt_empty_timeseries())
    }

    tibble::tibble(
      country       = x$country,
      provider_id   = x$provider_id,
      provider_name = x$provider_name,
      station_id    = station_id,
      parameter     = parameter,
      timestamp     = d$timestamp,
      value         = d$value,
      unit          = d$unit,
      quality_code  = d$quality_code,
      quality_name  = d$quality_name,
      quality_desc  = d$quality_desc,
      source_url    = d$source_url
    )
  }

  batches <- chunk_vec(seq_len(nrow(targets)), 25)
  pb <- progress::progress_bar$new(total = length(batches))
  result <- lapply(batches, function(batch) {
    pb$tick()
    dplyr::bind_rows(lapply(batch, fetch_one))
  })
  out <- dplyr::bind_rows(result)

  missing_parameter_ids <- ls(no_parameter)
  if (length(missing_parameter_ids)) {
    rlang::warn(
      paste0(
        "PT_SNIRH: ", length(missing_parameter_ids),
        " station(s) have no supported '", parameter,
        "' series and were skipped: ",
        paste(utils::head(missing_parameter_ids, 10), collapse = ", "),
        if (length(missing_parameter_ids) > 10) {
          paste0(" ... +", length(missing_parameter_ids) - 10, " more")
        } else {
          ""
        }
      )
    )
  }

  no_data_ids <- ls(no_data)
  if (length(no_data_ids)) {
    rlang::warn(
      paste0(
        "PT_SNIRH: ", length(no_data_ids),
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

  out
}
