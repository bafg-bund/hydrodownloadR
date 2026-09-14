# ==== Switzerland (official BAFU Open Data GraphQL API) ======================
# Base: https://data.bafu.admin.ch
# GraphQL endpoint: POST https://data.bafu.admin.ch/api
#
# The official API provides station metadata and pre-aggregated historical
# observations. This adapter uses data_1day_mean directly, so it does not need
# to reconstruct daily means from the near-real-time 10-minute feed.
#
# Available parameters in the official observations dataset:
#   Q  -> water_discharge
#   W  -> water_level
#   WT -> water_temperature
#
# API documentation:
# https://data.bafu.admin.ch/dataproduct-water-observations


# -- Registration -------------------------------------------------------------

#' @keywords internal
#' @noRd
register_CH_BAFU <- function() {
  register_service_usage(
    provider_id   = "CH_BAFU",
    provider_name = "Bundesamt f\u00FCr Umwelt (BAFU) Open Data",
    country       = "Switzerland",
    base_url      = "https://data.bafu.admin.ch",
    geo_base_url  = NULL,
    # BAFU permits 500 requests per rolling 5-minute window. This setting
    # leaves a safety margin below that limit.
    rate_cfg      = list(n = 4, period = 3),
    auth          = list(type = "none")
  )
}


# -- Parameter mapping --------------------------------------------------------

#' @export
timeseries_parameters.hydro_service_CH_BAFU <- function(x, ...) {
  c(
    "water_discharge",
    "water_level",
    "water_temperature"
  )
}

#' @keywords internal
#' @noRd
.ch_param_map <- function(parameter) {
  switch(
    parameter,
    water_discharge = list(code = "Q",  unit = "m^3/s"),
    water_level = list(code = "W", unit = "m a.s.l."),
    water_temperature = list(code = "WT", unit = "degC"),
    stop("Unsupported parameter for CH_BAFU: ", parameter)
  )
}


# -- GraphQL helpers ----------------------------------------------------------

#' @keywords internal
#' @noRd
.ch_graphql_request <- function(x, query, variables = list(), throttle = FALSE) {
  # jsonlite serializes an empty R list as JSON `[]`. BAFU requires GraphQL
  # variables to be a JSON object and returns HTTP 400 for `variables: []`.
  # Omit the member for variable-free requests such as stations().
  body <- list(query = query)
  if (length(variables)) body$variables <- variables

  # Build the JSON text explicitly. This avoids version-dependent handling of
  # empty R lists by httr2/jsonlite and guarantees that variable-free requests
  # contain only the GraphQL query.
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
    # Keep error responses available so .ch_graphql_response() can include
    # BAFU's explanation instead of losing it behind a generic HTTP 400.
    httr2::req_error(
      is_error = function(resp) FALSE
    )

  # req_perform_parallel() needs the rate limit on each request. Requests for
  # this provider share one token bucket through the common realm.
  if (isTRUE(throttle)) {
    req <- httr2::req_throttle(
      req,
      capacity = x$rate_cfg$n,
      fill_time_s = x$rate_cfg$period,
      realm = "CH_BAFU"
    )
  }

  req
}

#' @keywords internal
#' @noRd
.ch_graphql_response <- function(resp) {
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
      "CH_BAFU GraphQL request failed with HTTP ",
      status,
      ": ",
      detail
    ))
  }

  dat <- httr2::resp_body_json(resp, simplifyVector = FALSE)

  # GraphQL may return HTTP 200 even when the query itself failed.
  if (!is.null(dat$errors) && length(dat$errors)) {
    messages <- vapply(
      dat$errors,
      function(z) as.character(z$message %||% "Unknown GraphQL error"),
      character(1)
    )
    rlang::abort(paste0(
      "CH_BAFU GraphQL request failed: ",
      paste(unique(messages), collapse = "; ")
    ))
  }

  observations <- dat$data$water$observations
  if (is.null(observations)) {
    rlang::abort("CH_BAFU returned no water-observations payload.")
  }

  observations
}

#' @keywords internal
#' @noRd
.ch_graphql <- function(x, query, variables = list()) {
  req <- .ch_graphql_request(
    x,
    query = query,
    variables = variables
  )

  .ch_graphql_response(perform_request(req))
}

# Convert a Swiss calendar-date boundary to the UTC timestamp required by the
# GraphQL filter. BAFU's daily rows are timestamped late on the preceding UTC
# calendar date, so UTC midnight is not the correct request boundary.
#' @keywords internal
#' @noRd
.ch_date_bound_utc <- function(date) {
  date <- as.Date(date)
  local_midnight <- as.POSIXct(
    paste(format(date, "%Y-%m-%d"), "00:00:00"),
    tz = "Europe/Zurich"
  )

  format(
    local_midnight,
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
}

# Split an inclusive date range into non-overlapping windows. Each returned
# end date is inclusive; the GraphQL request converts it to an exclusive bound.
#' @keywords internal
#' @noRd
.ch_split_windows <- function(start_date, end_date, max_days) {
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)

  if (is.na(start_date) || is.na(end_date) || start_date > end_date) {
    return(list())
  }

  max_days <- max(1L, as.integer(max_days))
  out <- list()
  current <- start_date
  i <- 1L

  while (current <= end_date) {
    window_end <- min(end_date, current + max_days - 1L)
    out[[i]] <- list(start = current, end = window_end)
    current <- window_end + 1L
    i <- i + 1L
  }

  out
}

#' @keywords internal
#' @noRd
.ch_quality_name <- function(code) {
  dplyr::case_when(
    code == "1" ~ "provisional",
    code == "2" ~ "validated",
    code == "3" ~ "definitive",
    TRUE ~ NA_character_
  )
}

#' @keywords internal
#' @noRd
.ch_quality_desc <- function(code) {
  dplyr::case_when(
    code == "1" ~ "Provisional BAFU value",
    code == "2" ~ "Validated BAFU value",
    code == "3" ~ "Definitive or replaced BAFU value",
    TRUE ~ NA_character_
  )
}

# Convert one GraphQL result array without constructing and binding thousands
# of one-row tibbles. BAFU timestamps are UTC instants, but daily values must be
# assigned to the corresponding Swiss calendar date.
#' @keywords internal
#' @noRd
.ch_parse_daily_rows <- function(rows) {
  if (is.null(rows) || !length(rows)) return(tibble::tibble())

  station_id <- vapply(
    rows,
    function(z) as.character(z$station$no %||% NA_character_)[[1]],
    character(1)
  )
  timestamp_chr <- vapply(
    rows,
    function(z) as.character(z$timestamp %||% NA_character_)[[1]],
    character(1)
  )
  value <- vapply(
    rows,
    function(z) suppressWarnings(as.numeric(z$value %||% NA_real_))[[1]],
    numeric(1)
  )
  quality_code <- vapply(
    rows,
    function(z) as.character(z$releaseState %||% NA_character_)[[1]],
    character(1)
  )

  timestamp_raw <- suppressWarnings(lubridate::ymd_hms(
    timestamp_chr,
    tz = "UTC",
    quiet = TRUE
  ))

  tibble::tibble(
    station_id  = station_id,
    date         = as.Date(timestamp_raw, tz = "Europe/Zurich"),
    value        = value,
    quality_code = quality_code,
    quality_name = .ch_quality_name(quality_code),
    quality_desc = .ch_quality_desc(quality_code)
  )
}


# -- Stations (S3 method) -----------------------------------------------------

#' @export
stations.hydro_service_CH_BAFU <- function(x, ...) {
  query <- paste(
    "query Stations {",
    "  water {",
    "    observations {",
    "      stations(limit: 10000) {",
    "        no",
    "        name",
    "        riverName",
    "        latitude",
    "        longitude",
    "        elevation",
    "      }",
    "    }",
    "  }",
    "}",
    sep = "\n"
  )

  limited <- ratelimitr::limit_rate(
    function() .ch_graphql(x, query = query)$stations,
    rate = ratelimitr::rate(
      n = x$rate_cfg$n,
      period = x$rate_cfg$period
    )
  )

  rows <- limited()
  if (is.null(rows) || !length(rows)) return(tibble::tibble())

  out <- dplyr::bind_rows(lapply(rows, function(z) {
    station_name <- normalize_utf8(z$name %||% NA_character_)
    river_name   <- normalize_utf8(z$riverName %||% NA_character_)

    tibble::tibble(
      country            = x$country,
      provider_id        = x$provider_id,
      provider_name      = x$provider_name,
      station_id         = as.character(z$no %||% NA_character_),
      station_name       = as.character(station_name),
      station_name_ascii = to_ascii(station_name),
      river              = as.character(river_name),
      river_ascii        = to_ascii(river_name),
      lat                = suppressWarnings(as.numeric(z$latitude %||% NA_real_)),
      lon                = suppressWarnings(as.numeric(z$longitude %||% NA_real_)),
      area               = NA_real_,
      altitude           = suppressWarnings(as.numeric(z$elevation %||% NA_real_))
    )
  }))

  out |>
    dplyr::filter(!is.na(.data$station_id), nzchar(.data$station_id)) |>
    dplyr::distinct(.data$station_id, .keep_all = TRUE) |>
    dplyr::arrange(.data$station_id)
}


# -- Time series (S3 method) --------------------------------------------------

#' @export
timeseries.hydro_service_CH_BAFU <- function(
    x,
    parameter = c(
      "water_discharge",
      "water_level",
      "water_temperature"
    ),
    stations = NULL,
    start_date = NULL,
    end_date = NULL,
    mode = c("complete", "range"),
    exclude_quality = NULL,
    max_active = 6L,
    ...) {

  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)
  rng       <- resolve_dates(mode, start_date, end_date)
  pm        <- .ch_param_map(parameter)

  if (is.na(rng$start_date) || is.na(rng$end_date) ||
      rng$start_date > rng$end_date) {
    rlang::abort("CH_BAFU requires a valid start_date <= end_date.")
  }

  ids <- stations %||% character()
  if (!length(ids)) {
    ids <- stations.hydro_service_CH_BAFU(x)$station_id
  }

  ids <- unique(as.character(ids))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (!length(ids)) return(tibble::tibble())

  # One daily record is expected per station, parameter, and day. Batches of
  # 20 stations plus an adaptive date-window size keep each response safely
  # below BAFU's hard 10,000-row limit while avoiding many small requests for
  # single-station complete histories.
  batches <- chunk_vec(ids, 20L)

  jobs <- list()
  job_index <- 1L
  for (batch in batches) {
    max_days <- max(1L, floor(8000 / length(batch)))
    windows <- .ch_split_windows(
      rng$start_date,
      rng$end_date,
      max_days = max_days
    )

    for (window in windows) {
      jobs[[job_index]] <- list(batch = batch, window = window)
      job_index <- job_index + 1L
    }
  }

  if (!length(jobs)) return(tibble::tibble())

  max_active <- suppressWarnings(as.integer(max_active))
  if (!length(max_active) || is.na(max_active[[1]]) || max_active[[1]] < 1L) {
    max_active <- 1L
  } else {
    max_active <- max_active[[1]]
  }
  # BAFU recommends bounded concurrency. Do not allow this adapter to create
  # an unreasonably large burst even if a larger value is supplied.
  max_active <- min(max_active, 8L)

  query <- paste(
    "query DailyMean(",
    "  $from: AWSDateTime!,",
    "  $to: AWSDateTime!,",
    "  $stationNos: [String!]!,",
    "  $parameter: String!",
    ") {",
    "  water {",
    "    observations {",
    "      data_1day_mean(",
    "        where: {",
    "          station: { no: { _in: $stationNos } }",
    "          parameterName: { _eq: $parameter }",
    "          timestamp: { _gte: $from, _lt: $to }",
    "        }",
    "        limit: 10000",
    "      ) {",
    "        timestamp",
    "        value",
    "        releaseState",
    "        station { no }",
    "      }",
    "    }",
    "  }",
    "}",
    sep = "\n"
  )

  job_variables <- function(job) {
    win <- job$window
    list(
      from = .ch_date_bound_utc(win$start),
      # The API uses a half-open interval [from, to). These are Swiss
      # calendar-date boundaries converted to UTC.
      to = .ch_date_bound_utc(win$end + 1L),
      stationNos = unname(as.list(as.character(job$batch))),
      parameter = pm$code
    )
  }

  parse_job_response <- function(resp) {
    rows <- .ch_graphql_response(resp)$data_1day_mean
    .ch_parse_daily_rows(rows)
  }

  pb <- progress::progress_bar$new(
    total = length(jobs),
    clear = FALSE
  )
  out <- vector("list", length(jobs))

  # httr2 >= 1.1.1 correctly shares throttling across parallel requests.
  # Work in small waves so that hundreds of raw JSON responses are not kept in
  # memory alongside the parsed multi-million-row result.
  parallel_ok <- utils::packageVersion("httr2") >= "1.1.1" &&
    length(jobs) > 1L && max_active > 1L

  if (parallel_ok) {
    wave_size <- max_active * 4L
    waves <- chunk_vec(seq_along(jobs), wave_size)

    for (indices in waves) {
      reqs <- lapply(indices, function(i) {
        .ch_graphql_request(
          x,
          query = query,
          variables = job_variables(jobs[[i]]),
          throttle = TRUE
        )
      })

      responses <- httr2::req_perform_parallel(
        reqs,
        on_error = "stop",
        progress = FALSE,
        max_active = min(max_active, length(reqs))
      )

      for (j in seq_along(indices)) {
        i <- indices[[j]]
        out[[i]] <- parse_job_response(responses[[j]])
        pb$tick()
      }
    }
  } else {
    fetch_one <- ratelimitr::limit_rate(
      function(job) {
        req <- .ch_graphql_request(
          x,
          query = query,
          variables = job_variables(job)
        )
        parse_job_response(perform_request(req))
      },
      rate = ratelimitr::rate(
        n = x$rate_cfg$n,
        period = x$rate_cfg$period
      )
    )

    for (i in seq_along(jobs)) {
      out[[i]] <- fetch_one(jobs[[i]])
      pb$tick()
    }
  }

  res <- dplyr::bind_rows(out)
  if (!nrow(res)) return(tibble::tibble())

  if (!is.null(exclude_quality)) {
    excluded <- as.character(exclude_quality)
    res <- res |>
      dplyr::filter(
        is.na(.data$quality_code) |
          !(.data$quality_code %in% excluded)
      )
  }

  if (!nrow(res)) return(tibble::tibble())

  res |>
    dplyr::filter(
      !is.na(.data$station_id),
      !is.na(.data$date),
      .data$date >= rng$start_date,
      .data$date <= rng$end_date
    ) |>
    dplyr::mutate(
      country       = x$country,
      provider_id   = x$provider_id,
      provider_name = x$provider_name,
      parameter     = parameter,
      timestamp     = as.POSIXct(.data$date, tz = "UTC"),
      unit          = pm$unit,
      source_url    = paste0(
        x$base_url,
        "/dataproduct-water-observations"
      )
    ) |>
    dplyr::distinct(
      .data$station_id,
      .data$parameter,
      .data$timestamp,
      .keep_all = TRUE
    ) |>
    dplyr::arrange(
      .data$station_id,
      .data$parameter,
      .data$timestamp
    ) |>
    dplyr::select(
      country,
      provider_id,
      provider_name,
      station_id,
      parameter,
      timestamp,
      value,
      unit,
      quality_code,
      quality_name,
      quality_desc,
      source_url
    )
}
