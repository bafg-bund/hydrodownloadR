# R/adapter_IL_IWA.R
# Israel - Israel Water Authority Hydrological Service
#
# Official data are published through the data.gov.il CKAN Action API:
#   Station catalogue: package "hydro_station"
#   Daily mean discharge: package "level_discharge"
#   Water-quality samples: package "qual_per_hydro_station"
#
# Resource IDs are discovered from package_show rather than hard-coded.

# -----------------------------------------------------------------------------
# Registration
# -----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
register_IL_IWA <- function() {
  register_service_usage(
    provider_id   = "IL_IWA",
    provider_name = "Israel Water Authority - Hydrological Service",
    country       = "Israel",
    base_url      = "https://data.gov.il/api/3/action",
    # The CKAN backend occasionally returns HTTP 409 when requests arrive in
    # quick succession. One request per second is both reliable and polite.
    rate_cfg      = list(n = 1L, period = 1),
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_IL_IWA <- function(x, ...) {
  c("water_discharge", "water_quality")
}

# -----------------------------------------------------------------------------
# Constants
# -----------------------------------------------------------------------------

.IL_IWA_STATION_PACKAGE <- "hydro_station"
.IL_IWA_DISCHARGE_PACKAGE <- "level_discharge"
.IL_IWA_QUALITY_PACKAGE <- "qual_per_hydro_station"
.IL_IWA_PAGE_SIZE <- 10000L
.IL_IWA_STATION_CHUNK_SIZE <- 100L

# Station-catalogue field names
.IL_IWA_ST_ID <- "\u05d6\u05d9\u05d4\u05d5\u05d9 \u05ea\u05d7\u05e0\u05d4"
.IL_IWA_ST_NAME_HE <- "\u05e9\u05dd \u05e2\u05d1\u05e8\u05d9\u05ea"
.IL_IWA_ST_NAME_EN <- "\u05e9\u05dd \u05d0\u05e0\u05d2\u05dc\u05d9\u05ea"
.IL_IWA_ST_AREA <- "\u05e9\u05d8\u05d7 \u05d4\u05d9\u05e7\u05d5\u05d5\u05ea (\u05e7\u05de''\u05e8)"
.IL_IWA_ST_X <- "\u05e0.\u05e6. X (\u05e8\u05d5\u05d7\u05d1)"
.IL_IWA_ST_Y <- "\u05e0.\u05e6. Y (\u05e8\u05d5\u05d7\u05d1)"
.IL_IWA_ST_BASIN <- paste(
  "\u05ea\u05d7\u05d5\u05dd \u05d4\u05ea\u05e0\u05e7\u05d6\u05d5\u05ea",
  "\u05e9\u05dc \u05e0\u05d7\u05dc \u05e8\u05d0\u05e9\u05d9"
)
.IL_IWA_ST_STATUS <- "\u05e1\u05d8\u05d8\u05d5\u05e1 \u05ea\u05d7\u05e0\u05d4 \u05e0\u05d5\u05db\u05d7\u05d9"
.IL_IWA_STATUS_ACTIVE <- "\u05e4\u05e2\u05d9\u05dc\u05d4"
.IL_IWA_STATUS_INACTIVE <- "\u05dc\u05d0 \u05e4\u05e2\u05d9\u05dc\u05d4"

# Daily-discharge field names
.IL_IWA_TS_ST_ID <- paste(
  "\u05d6\u05d9\u05d4\u05d5\u05d9 \u05ea\u05d7\u05e0\u05d4",
  "\u05d4\u05d9\u05d3\u05e8\u05d5\u05de\u05d8\u05e8\u05d9\u05ea"
)
.IL_IWA_TS_DATE <- "\u05ea\u05d0\u05e8\u05d9\u05da"
.IL_IWA_TS_VALUE <- c(
  paste(
    "\u05e1\u05e4\u05d9\u05e7\u05d4 \u05d9\u05d5\u05de\u05d9\u05ea \u05de\u05de\u05d5\u05e6\u05e2\u05ea",
    "(\u05de''\u05e7/\u05e9\u05e0\u05d9\u05d4)"
  ),
  paste(
    "\u05e1\u05e4\u05d9\u05e7\u05d4 \u05d9\u05d5\u05de\u05d9\u05ea \u05de\u05de\u05d5\u05e6\u05e2\u05ea",
    "(\u05de\u05d8\u05e8 \u05e7\u05d5\u05d1/\u05e9\u05e0\u05d9\u05d4)"
  )
)

# Water-quality field names
.IL_IWA_WQ_ST_ID <- "\u05de\u05e1\u05e4\u05e8 \u05d6\u05d9\u05d4\u05d5\u05d9"
.IL_IWA_WQ_TIME <- "\u05ea\u05d0\u05e8\u05d9\u05da \u05d3\u05d2\u05d9\u05de\u05d4"
.IL_IWA_WQ_QUALIFIER <- paste0(
  "\u05e1\u05d9\u05de\u05df \u05d2\u05d3\u05d5\u05dc/",
  "\u05e7\u05d8\u05df"
)
.IL_IWA_WQ_VALUE <- "\u05ea\u05d5\u05e6\u05d0\u05d4"
.IL_IWA_WQ_CODE <- "\u05e1\u05de\u05dc \u05e4\u05e8\u05de\u05d8\u05e8"
.IL_IWA_WQ_DESCRIPTION <- "\u05ea\u05d0\u05d5\u05e8 \u05de\u05e7\u05d5\u05e6\u05e8"
.IL_IWA_WQ_UNIT <- "\u05d9\u05d7\u05d9\u05d3\u05ea \u05de\u05d9\u05d3\u05d4"
.IL_IWA_WQ_SAMPLER <- "\u05de\u05d5\u05e1\u05d3 \u05d3\u05d5\u05d2\u05dd"
.IL_IWA_WQ_LAB <- "\u05de\u05e2\u05d1\u05d3\u05d4"

# -----------------------------------------------------------------------------
# CKAN helpers
# -----------------------------------------------------------------------------

.il_iwa_perform_request <- function(req, max_tries = 6L) {
  req |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_retry(
      max_tries = max_tries,
      is_transient = function(resp) {
        status <- httr2::resp_status(resp)
        status >= 500L || status %in% c(408L, 409L, 425L, 429L)
      },
      backoff = ~ min(30, 2 ^ (.x - 1)) + stats::runif(1, 0, 0.25)
    ) |>
    httr2::req_perform()
}

.il_iwa_action <- function(x, action, query = list()) {
  req <- build_request(x, path = action, query = query)
  resp <- tryCatch(
    .il_iwa_perform_request(req),
    error = function(e) {
      resource <- query$resource_id %||% NA_character_
      context <- if (!is.na(resource)) {
        paste0(" for resource '", resource, "'")
      } else {
        ""
      }
      rlang::abort(
        paste0(
          "IL_IWA: data.gov.il request '", action, "'", context,
          " failed after retries: ", conditionMessage(e)
        )
      )
    }
  )

  body <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = TRUE),
    error = function(e) {
      rlang::abort(
        paste0("IL_IWA: could not parse the data.gov.il response: ", conditionMessage(e))
      )
    }
  )

  if (!is.list(body) || !isTRUE(body$success) || is.null(body$result)) {
    rlang::abort(paste0("IL_IWA: CKAN action '", action, "' failed."))
  }

  body$result
}

.il_iwa_as_tibble <- function(records) {
  if (is.null(records) || !length(records)) return(tibble::tibble())
  if (is.data.frame(records)) {
    return(tibble::as_tibble(records, .name_repair = "minimal"))
  }
  if (is.matrix(records)) {
    return(tibble::as_tibble(records, .name_repair = "minimal"))
  }
  if (is.list(records)) return(dplyr::bind_rows(records))
  tibble::tibble()
}

.il_iwa_require_columns <- function(df, columns, context) {
  missing <- setdiff(columns, names(df))
  if (length(missing)) {
    rlang::abort(
      paste0(
        "IL_IWA: unexpected ", context, " schema; missing column(s): ",
        paste(missing, collapse = ", "), "."
      )
    )
  }
  invisible(TRUE)
}

.il_iwa_pick_column <- function(df, candidates, context) {
  column <- intersect(candidates, names(df))
  if (!length(column)) {
    rlang::abort(
      paste0(
        "IL_IWA: unexpected ", context, " schema; none of the expected ",
        "columns are present: ", paste(candidates, collapse = ", "), "."
      )
    )
  }
  column[[1]]
}

.il_iwa_package_resources <- function(x, package_id) {
  result <- .il_iwa_action(x, "package_show", list(id = package_id))
  resources <- .il_iwa_as_tibble(result$resources)

  if (!nrow(resources) || !all(c("id", "name") %in% names(resources))) {
    rlang::abort(
      paste0("IL_IWA: package '", package_id, "' has no usable resources.")
    )
  }

  if ("datastore_active" %in% names(resources)) {
    active <- tolower(as.character(resources$datastore_active)) %in% c("true", "1")
    resources <- resources[active, , drop = FALSE]
  }

  if (!nrow(resources)) {
    rlang::abort(
      paste0("IL_IWA: package '", package_id, "' has no active DataStore resource.")
    )
  }

  resources
}

.il_iwa_make_page_fetcher <- function(x) {
  ratelimitr::limit_rate(
    function(query) .il_iwa_action(x, "datastore_search", query),
    rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period)
  )
}

.il_iwa_fetch_resource <- function(fetch_page, resource_id, filters = NULL,
                                   search = NULL,
                                   exact_station_ids = NULL,
                                   exact_station_field = NULL,
                                   page_size = .IL_IWA_PAGE_SIZE) {
  offset <- 0L
  pages <- list()

  repeat {
    query <- list(
      resource_id = resource_id,
      limit = as.integer(page_size),
      offset = as.integer(offset),
      sort = "_id asc"
    )

    if (!is.null(filters)) {
      # Strip jsonlite's class before handing the value to httr2. Keeping the
      # class can result in different query serialization across R versions.
      query$filters <- as.character(
        jsonlite::toJSON(
          filters,
          auto_unbox = TRUE,
          null = "null"
        )
      )
    }

    if (!is.null(search)) {
      query$q <- paste(as.character(search), collapse = " ")
    }

    result <- fetch_page(query)
    raw_page <- .il_iwa_as_tibble(result$records)
    raw_page_size <- nrow(raw_page)
    if (!raw_page_size) break

    page <- raw_page
    if (!is.null(exact_station_ids) && !is.null(exact_station_field) &&
        exact_station_field %in% names(page)) {
      page <- page[
        as.character(page[[exact_station_field]]) %in%
          as.character(exact_station_ids),
        ,
        drop = FALSE
      ]
    }

    if (nrow(page)) pages[[length(pages) + 1L]] <- page
    offset <- offset + raw_page_size

    total <- suppressWarnings(as.numeric(result$total %||% NA_real_))
    if (is.finite(total) && offset >= total) break
    if (raw_page_size < page_size) break
  }

  dplyr::bind_rows(pages)
}

# A single station is queried through CKAN's ASCII-only `q` parameter. This
# avoids a reproducible HTTP 409 on some data.gov.il nodes when the Hebrew field
# name is sent in `filters`. Results are filtered by station ID again locally,
# so full-text matches can never introduce records from another station.
.il_iwa_fetch_station_resource <- function(fetch_page, resource_id, ids,
                                           station_id_field) {
  if (is.null(ids)) {
    return(.il_iwa_fetch_resource(fetch_page, resource_id))
  }

  ids <- as.character(ids)
  if (length(ids) == 1L) {
    return(
      .il_iwa_fetch_resource(
        fetch_page = fetch_page,
        resource_id = resource_id,
        search = ids,
        exact_station_ids = ids,
        exact_station_field = station_id_field
      )
    )
  }

  filters <- stats::setNames(list(ids), station_id_field)
  tryCatch(
    .il_iwa_fetch_resource(
      fetch_page = fetch_page,
      resource_id = resource_id,
      filters = filters,
      exact_station_ids = ids,
      exact_station_field = station_id_field
    ),
    error = function(e) {
      if (!grepl("HTTP 409", conditionMessage(e), fixed = TRUE)) stop(e)

      pieces <- lapply(
        ids,
        function(id) {
          .il_iwa_fetch_resource(
            fetch_page = fetch_page,
            resource_id = resource_id,
            search = id,
            exact_station_ids = id,
            exact_station_field = station_id_field
          )
        }
      )
      dplyr::bind_rows(pieces)
    }
  )
}

# The resource names contain their period (for example, 2000--2010 or 2020
# onwards). This deliberately uses conservative calendar-year windows; the
# final exact date filter is always applied after download.
.il_iwa_resource_overlaps <- function(resource_name, start_date, end_date) {
  years <- regmatches(
    resource_name,
    gregexpr("(?:19|20)[0-9]{2}", resource_name, perl = TRUE)
  )[[1]]

  if (!length(years)) return(TRUE)

  years <- suppressWarnings(as.integer(years))
  years <- years[!is.na(years)]
  if (!length(years)) return(TRUE)

  onwards <- grepl("\u05d5\u05d0\u05d9\u05dc\u05da", resource_name, fixed = TRUE) ||
    grepl("onward", resource_name, ignore.case = TRUE)
  through <- grepl("\u05e2\u05d3 \u05e1\u05d5\u05e3", resource_name, fixed = TRUE) ||
    grepl("through|until", resource_name, ignore.case = TRUE)

  if (through) {
    resource_start <- as.Date("1900-01-01")
    resource_end <- as.Date(sprintf("%04d-12-31", max(years)))
  } else if (onwards) {
    resource_start <- as.Date(sprintf("%04d-01-01", min(years)))
    resource_end <- as.Date("9999-12-31")
  } else if (length(years) == 1L) {
    resource_start <- as.Date("1900-01-01")
    resource_end <- as.Date(sprintf("%04d-12-31", years[[1]]))
  } else {
    resource_start <- as.Date(sprintf("%04d-01-01", min(years)))
    resource_end <- as.Date(sprintf("%04d-12-31", max(years)))
  }

  resource_start <= end_date && resource_end >= start_date
}

.il_iwa_normalize_station_ids <- function(stations) {
  if (is.null(stations) || !length(stations)) return(NULL)

  if (is.data.frame(stations)) {
    if (!"station_id" %in% names(stations)) {
      rlang::abort(
        "IL_IWA: a station data frame must contain a 'station_id' column."
      )
    }
    stations <- stations$station_id
  }

  ids <- unique(trimws(as.character(unlist(stations, use.names = FALSE))))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (!length(ids)) return(NULL)

  bad <- ids[!grepl("^[0-9]+$", ids)]
  if (length(bad)) {
    rlang::abort(
      paste0(
        "IL_IWA: station IDs must contain digits only. Invalid ID(s): ",
        paste(utils::head(bad, 5L), collapse = ", "), "."
      )
    )
  }

  ids
}

# Romanize Hebrew names for the provider-neutral name fields. ICU's Any-Latin
# transliterator provides the best available conversion for unpointed Hebrew.
# Since not every ICU build contains the same transliteration tables, retain a
# deterministic letter-by-letter fallback so this can never regress to "????".
.il_iwa_romanize_hebrew_fallback <- function(x) {
  source_chars <- intToUtf8(
    c(seq.int(0x05D0L, 0x05EAL), 0x05BEL, 0x05F3L, 0x05F4L),
    multiple = TRUE
  )
  target_chars <- c(
    "'", "b", "g", "d", "h", "v", "z", "h", "t", "y",
    "kh", "kh", "l", "m", "m", "n", "n", "s", "'", "f",
    "p", "ts", "ts", "q", "r", "sh", "t", "-", "'", "\""
  )

  out <- as.character(x)
  for (i in seq_along(source_chars)) {
    out <- gsub(source_chars[[i]], target_chars[[i]], out, fixed = TRUE)
  }

  # Remove Hebrew cantillation and vowel marks after the base letters have
  # been converted.
  gsub("[\u0591-\u05BD\u05BF-\u05C7]", "", out, perl = TRUE)
}

.il_iwa_romanize <- function(x) {
  if (is.null(x)) return(x)

  out <- normalize_utf8(x)
  has_hebrew <- !is.na(out) & grepl("[\u0590-\u05FF]", out, perl = TRUE)

  if (any(has_hebrew)) {
    source <- out[has_hebrew]
    romanized <- tryCatch(
      stringi::stri_trans_general(
        source,
        id = "Any-Latin; Latin-ASCII"
      ),
      error = function(e) rep(NA_character_, length(source))
    )

    failed <- is.na(romanized) |
      grepl("[\u0590-\u05FF]", romanized, perl = TRUE) |
      grepl("?", romanized, fixed = TRUE)
    if (any(failed)) {
      romanized[failed] <- .il_iwa_romanize_hebrew_fallback(source[failed])
    }
    out[has_hebrew] <- romanized
  }

  out <- tryCatch(
    stringi::stri_trans_general(out, id = "Latin-ASCII"),
    error = function(e) out
  )
  out <- gsub("[\u2018\u2019\u05F3]", "'", out, perl = TRUE)
  out <- gsub("[\u201C\u201D\u05F4]", "\"", out, perl = TRUE)
  out <- gsub("[\u2010-\u2015\u05BE]", "-", out, perl = TRUE)
  trimws(gsub("[[:space:]]+", " ", out, perl = TRUE))
}

# Split the catalogue's combined "river - site" labels without mistaking
# hyphens inside names (for example HA-EIN or BE'ER-SHEVA) for separators.
# A compact hyphen is accepted only when its prefix has the same number of
# words as the river prefix in the Hebrew label. Labels whose site consists
# only of a station number retain the full official designation, since a bare
# number is not a useful station name.
.il_iwa_split_station_label <- function(label, local_label) {
  label <- trimws(as.character(label))
  local_label <- trimws(as.character(local_label))

  station_name <- label
  river <- rep(NA_character_, length(label))

  word_count <- function(value) {
    value <- trimws(value)
    if (is.na(value) || !nzchar(value)) return(0L)
    length(strsplit(value, "[[:space:]]+", perl = TRUE)[[1]])
  }

  for (i in seq_along(label)) {
    current <- label[[i]]
    if (is.na(current) || !nzchar(current)) next

    separator <- regexpr(
      "\\s+-\\s*|\\s*-\\s+",
      current,
      perl = TRUE
    )
    separator_at <- as.integer(separator[[1]])
    separator_length <- attr(separator, "match.length")[[1]]

    if (separator_at < 1L) {
      separator <- regexpr("-", current, fixed = TRUE)
      separator_at <- as.integer(separator[[1]])
      separator_length <- attr(separator, "match.length")[[1]]

      if (separator_at > 0L) {
        local_current <- local_label[[i]]
        local_separator <- if (is.na(local_current)) {
          -1L
        } else {
          as.integer(regexpr(
            "[-\u2010-\u2015\u05BE]",
            local_current,
            perl = TRUE
          )[[1]])
        }

        # The local label disambiguates compact separators from hyphens that
        # are part of a transliterated place name.
        if (local_separator > 0L) {
          label_words <- word_count(substr(current, 1L, separator_at - 1L))
          local_words <- word_count(
            substr(local_current, 1L, local_separator - 1L)
          )
          if (label_words != local_words) separator_at <- -1L
        }
      }
    }

    if (separator_at < 1L) next

    prefix <- trimws(substr(current, 1L, separator_at - 1L))
    suffix <- trimws(substr(
      current,
      separator_at + separator_length,
      nchar(current)
    ))
    if (!nzchar(prefix) || !nzchar(suffix)) next

    river[[i]] <- prefix
    if (!grepl("^[0-9]+[A-Za-z]?$", suffix, perl = TRUE)) {
      station_name[[i]] <- suffix
    }
  }

  list(station_name = station_name, river = river)
}

# -----------------------------------------------------------------------------
# Stations
# -----------------------------------------------------------------------------

#' @export
stations.hydro_service_IL_IWA <- function(x, stations = NULL, ...) {
  resources <- .il_iwa_package_resources(x, .IL_IWA_STATION_PACKAGE)
  resource_id <- as.character(resources$id[[1]])

  fetch_page <- .il_iwa_make_page_fetcher(x)
  raw <- .il_iwa_fetch_resource(fetch_page, resource_id)

  required <- c(
    .IL_IWA_ST_ID, .IL_IWA_ST_NAME_HE, .IL_IWA_ST_NAME_EN,
    .IL_IWA_ST_AREA, .IL_IWA_ST_X, .IL_IWA_ST_Y,
    .IL_IWA_ST_BASIN, .IL_IWA_ST_STATUS
  )
  .il_iwa_require_columns(raw, required, "station catalogue")

  station_id <- as.character(raw[[.IL_IWA_ST_ID]])
  station_name_local <- trimws(as.character(raw[[.IL_IWA_ST_NAME_HE]]))
  station_name_english <- trimws(as.character(raw[[.IL_IWA_ST_NAME_EN]]))
  station_name <- .il_iwa_romanize(station_name_english)
  use_local <- is.na(station_name) | !nzchar(station_name)
  station_name[use_local] <- .il_iwa_romanize(station_name_local[use_local])

  easting <- suppressWarnings(as.numeric(raw[[.IL_IWA_ST_X]]))
  northing <- suppressWarnings(as.numeric(raw[[.IL_IWA_ST_Y]]))
  lon <- rep(NA_real_, nrow(raw))
  lat <- rep(NA_real_, nrow(raw))

  valid_xy <- is.finite(easting) & is.finite(northing)
  if (any(valid_xy)) {
    points_itm <- sf::st_as_sf(
      data.frame(
        easting = easting[valid_xy],
        northing = northing[valid_xy]
      ),
      coords = c("easting", "northing"),
      crs = 2039,
      remove = FALSE
    )
    points_wgs84 <- sf::st_transform(points_itm, 4326)
    coordinates <- sf::st_coordinates(points_wgs84)
    lon[valid_xy] <- coordinates[, 1]
    lat[valid_xy] <- coordinates[, 2]
  }

  basin_local <- trimws(as.character(raw[[.IL_IWA_ST_BASIN]]))
  basin <- .il_iwa_romanize(basin_local)
  label_parts <- .il_iwa_split_station_label(
    station_name,
    station_name_local
  )
  station_name <- label_parts$station_name
  river <- basin
  use_label_river <- !is.na(label_parts$river) & nzchar(label_parts$river)
  river[use_label_river] <- label_parts$river[use_label_river]
  river[is.na(river) | !nzchar(trimws(river))] <- NA_character_

  status_raw <- trimws(as.character(raw[[.IL_IWA_ST_STATUS]]))
  status <- dplyr::case_when(
    status_raw == .IL_IWA_STATUS_ACTIVE ~ "active",
    status_raw == .IL_IWA_STATUS_INACTIVE ~ "inactive",
    TRUE ~ .il_iwa_romanize(status_raw)
  )

  out <- tibble::tibble(
    country            = x$country,
    provider_id        = x$provider_id,
    provider_name      = x$provider_name,
    station_id         = station_id,
    station_name       = station_name,
    station_name_local = station_name_local,
    river              = river,
    lat                = lat,
    lon                = lon,
    area               = suppressWarnings(as.numeric(raw[[.IL_IWA_ST_AREA]])),
    altitude           = NA_real_,
    status             = status
  )

  wanted <- .il_iwa_normalize_station_ids(stations)
  if (!is.null(wanted)) {
    out <- dplyr::filter(out, .data$station_id %in% wanted)
  }

  dplyr::arrange(out, .data$station_id)
}

# -----------------------------------------------------------------------------
# Time-series normalization
# -----------------------------------------------------------------------------

.il_iwa_empty_ts <- function(x) {
  tibble::tibble(
    country       = character(),
    provider_id   = character(),
    provider_name = character(),
    station_id    = character(),
    parameter     = character(),
    timestamp     = as.POSIXct(character(), tz = "UTC"),
    value         = numeric(),
    unit          = character(),
    quality_code  = character(),
    quality_name  = character(),
    quality_desc  = character(),
    source_url    = character()
  )
}

.il_iwa_empty_quality_ts <- function(x) {
  tibble::tibble(
    country               = character(),
    provider_id           = character(),
    provider_name         = character(),
    station_id            = character(),
    parameter             = character(),
    sub_parameter         = character(),
    parameter_code        = character(),
    parameter_name        = character(),
    source_record_id      = integer(),
    timestamp             = as.POSIXct(character(), tz = "UTC"),
    value                 = numeric(),
    unit                  = character(),
    quality_code          = character(),
    quality_name          = character(),
    quality_desc          = character(),
    sampling_organization = character(),
    laboratory            = character(),
    source_url            = character()
  )
}

.il_iwa_empty_for_parameter <- function(x, parameter) {
  if (identical(parameter, "water_discharge")) return(.il_iwa_empty_ts(x))
  .il_iwa_empty_quality_ts(x)
}

.il_iwa_column_or_na <- function(df, column) {
  if (column %in% names(df)) return(df[[column]])
  rep(NA_character_, nrow(df))
}

.il_iwa_clean_character <- function(x) {
  out <- trimws(normalize_utf8(as.character(x)))
  out[is.na(out) | !nzchar(out)] <- NA_character_
  out
}

.il_iwa_parse_source_datetime <- function(x) {
  suppressWarnings(
    lubridate::parse_date_time(
      as.character(x),
      orders = c("dmy HMS", "dmy HM", "dmy"),
      tz = "Asia/Jerusalem",
      quiet = TRUE
    )
  )
}

.il_iwa_quality_sub_parameter <- function(code) {
  code_key <- toupper(.il_iwa_clean_character(code))
  lookup <- c(
    "6:2FT" = "6_2_fts",
    "ADONA" = "adona",
    "AL" = "aluminum",
    "AS" = "arsenic",
    "B" = "boron",
    "BA" = "barium",
    "BE" = "beryllium",
    "BR" = "bromide",
    "CA" = "calcium",
    "CAFFE" = "caffeine",
    "CARBO" = "carbamazepine",
    "CD" = "cadmium",
    "CL" = "chloride",
    "CO" = "cobalt",
    "CR" = "chromium",
    "CU" = "copper",
    "DO" = "dissolved_oxygen",
    "DP-P" = "dissolved_phosphorus",
    "EC" = "conductivity",
    "ECFD" = "field_conductivity",
    "F" = "fluoride",
    "FE" = "iron_total",
    "HARD" = "hardness",
    "HCO3" = "bicarbonate",
    "K" = "potassium",
    "MG" = "magnesium",
    "MN" = "manganese",
    "NA" = "sodium",
    "NI" = "nickel",
    "NO2" = "nitrite",
    "NO3" = "nitrate",
    "ORP" = "oxidation_reduction_potential",
    "PB" = "lead",
    "PFBA" = "pfba",
    "PFBS" = "pfbs",
    "PFDA" = "pfda",
    "PFDOA" = "pfdoa",
    "PFESA" = "pfesa",
    "PFHPA" = "pfhpa",
    "PFHXA" = "pfhxa",
    "PFHXS" = "pfhxs",
    "PFNA" = "pfna",
    "PFOA" = "pfoa",
    "PFOS" = "pfos",
    "PFPEA" = "pfpea",
    "PHFD" = "ph",
    "PO4" = "phosphate",
    "SE" = "selenium",
    "SILT" = "silt",
    "SO4" = "sulfate",
    "SR" = "strontium",
    "T" = "water_temperature",
    "TDS" = "total_dissolved_solids",
    "TN-N" = "total_nitrogen",
    "TOC" = "total_organic_carbon",
    "TPFAS" = "total_pfas",
    "TURB" = "turbidity",
    "ZN" = "zinc"
  )

  out <- unname(lookup[code_key])
  missing <- is.na(out) & !is.na(code_key)
  if (any(missing)) {
    fallback <- tolower(code_key[missing])
    fallback <- gsub("[^a-z0-9]+", "_", fallback, perl = TRUE)
    fallback <- gsub("^_+|_+$", "", fallback, perl = TRUE)
    out[missing] <- fallback
  }
  out
}

.il_iwa_normalize_quality_unit <- function(unit, code) {
  out <- .il_iwa_clean_character(unit)
  key <- tolower(gsub("[[:space:]]+", "", out, perl = TRUE))
  code_key <- toupper(.il_iwa_clean_character(code))

  out[which(key == "mg/l")] <- "mg/L"
  out[which(key %in% c("microgr/l", "microgram/l", "ug/l", "\u00b5g/l"))] <-
    "\u00b5g/L"
  out[which(key == "celsius")] <- "\u00b0C"
  out[which(key == "ms/cm")] <- "mS/cm"
  out[which(key == "mv")] <- "mV"
  out[which(key == "ntu")] <- "NTU"
  out[which(key == "unit")] <- "1"
  out[which(code_key == "PHFD")] <- "pH"
  out
}

.il_iwa_tidy_discharge <- function(raw, x, resource_id, start_date, end_date) {
  if (!nrow(raw)) return(.il_iwa_empty_ts(x))

  .il_iwa_require_columns(
    raw,
    c(.IL_IWA_TS_ST_ID, .IL_IWA_TS_DATE),
    "daily-discharge resource"
  )
  value_column <- .il_iwa_pick_column(
    raw,
    .IL_IWA_TS_VALUE,
    "daily-discharge resource"
  )

  dates <- suppressWarnings(
    as.Date(as.character(raw[[.IL_IWA_TS_DATE]]), format = "%d/%m/%Y")
  )
  keep <- !is.na(dates) & dates >= start_date & dates <= end_date
  if (!any(keep)) return(.il_iwa_empty_ts(x))

  source_url <- paste0(
    x$base_url,
    "/datastore_search?resource_id=",
    resource_id
  )

  tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = as.character(raw[[.IL_IWA_TS_ST_ID]][keep]),
    parameter     = "water_discharge",
    timestamp     = as.POSIXct(dates[keep], tz = "UTC"),
    value         = suppressWarnings(as.numeric(raw[[value_column]][keep])),
    unit          = "m^3/s",
    quality_code  = NA_character_,
    quality_name  = NA_character_,
    quality_desc  = NA_character_,
    source_url    = source_url
  )
}

.il_iwa_tidy_quality <- function(raw, x, resource_id, start_date, end_date) {
  if (!nrow(raw)) return(.il_iwa_empty_quality_ts(x))

  .il_iwa_require_columns(
    raw,
    c(
      .IL_IWA_WQ_ST_ID, .IL_IWA_WQ_TIME, .IL_IWA_WQ_VALUE,
      .IL_IWA_WQ_CODE, .IL_IWA_WQ_DESCRIPTION, .IL_IWA_WQ_UNIT
    ),
    "water-quality resource"
  )

  local_time <- .il_iwa_parse_source_datetime(raw[[.IL_IWA_WQ_TIME]])
  dates <- as.Date(local_time, tz = "Asia/Jerusalem")
  parameter_code <- .il_iwa_clean_character(raw[[.IL_IWA_WQ_CODE]])
  keep <- !is.na(dates) & dates >= start_date & dates <= end_date
  if (!any(keep)) return(.il_iwa_empty_quality_ts(x))

  qualifier <- .il_iwa_clean_character(
    .il_iwa_column_or_na(raw, .IL_IWA_WQ_QUALIFIER)[keep]
  )
  qualifier_name <- rep(NA_character_, length(qualifier))
  qualifier_name[which(qualifier == "<")] <- "below reporting limit"
  qualifier_name[which(qualifier == ">")] <- "above reporting limit"

  sub_parameter <- .il_iwa_quality_sub_parameter(parameter_code[keep])
  parameter_name <- .il_iwa_romanize(
    .il_iwa_clean_character(raw[[.IL_IWA_WQ_DESCRIPTION]][keep])
  )
  missing_name <- is.na(parameter_name) | !nzchar(parameter_name)
  parameter_name[missing_name] <- gsub(
    "_", " ", sub_parameter[missing_name], fixed = TRUE
  )

  source_url <- paste0(
    x$base_url,
    "/datastore_search?resource_id=",
    resource_id
  )

  tibble::tibble(
    country               = x$country,
    provider_id           = x$provider_id,
    provider_name         = x$provider_name,
    station_id            = as.character(raw[[.IL_IWA_WQ_ST_ID]][keep]),
    parameter             = "water_quality",
    sub_parameter         = sub_parameter,
    parameter_code        = parameter_code[keep],
    parameter_name        = parameter_name,
    source_record_id      = suppressWarnings(
      as.integer(.il_iwa_column_or_na(raw, "_id")[keep])
    ),
    timestamp             = lubridate::with_tz(local_time[keep], "UTC"),
    value                 = suppressWarnings(
      as.numeric(raw[[.IL_IWA_WQ_VALUE]][keep])
    ),
    unit                  = .il_iwa_normalize_quality_unit(
      raw[[.IL_IWA_WQ_UNIT]][keep],
      parameter_code[keep]
    ),
    quality_code          = qualifier,
    quality_name          = qualifier_name,
    quality_desc          = NA_character_,
    sampling_organization = .il_iwa_romanize(
      .il_iwa_clean_character(
        .il_iwa_column_or_na(raw, .IL_IWA_WQ_SAMPLER)[keep]
      )
    ),
    laboratory            = .il_iwa_romanize(
      .il_iwa_clean_character(
        .il_iwa_column_or_na(raw, .IL_IWA_WQ_LAB)[keep]
      )
    ),
    source_url            = source_url
  )
}

#' @export
timeseries.hydro_service_IL_IWA <- function(x,
                                            parameter = "water_discharge",
                                            stations = NULL,
                                            start_date = NULL,
                                            end_date = NULL,
                                            mode = c("complete", "range"),
                                            ...) {
  supported <- timeseries_parameters.hydro_service_IL_IWA(x)
  parameter <- match.arg(parameter, supported)
  mode <- match.arg(mode)

  range <- resolve_dates(mode, start_date, end_date)
  if (is.na(range$start_date) || is.na(range$end_date) ||
      range$start_date > range$end_date) {
    rlang::abort("IL_IWA: invalid date range.")
  }

  if (identical(parameter, "water_discharge")) {
    package_id <- .IL_IWA_DISCHARGE_PACKAGE
    station_id_field <- .IL_IWA_TS_ST_ID
    source_kind <- "discharge"
  } else {
    package_id <- .IL_IWA_QUALITY_PACKAGE
    station_id_field <- .IL_IWA_WQ_ST_ID
    source_kind <- "quality"
  }

  resources <- .il_iwa_package_resources(x, package_id)
  overlaps <- vapply(
    as.character(resources$name),
    .il_iwa_resource_overlaps,
    logical(1),
    start_date = range$start_date,
    end_date = range$end_date
  )
  resources <- resources[overlaps, , drop = FALSE]
  if (!nrow(resources)) return(.il_iwa_empty_for_parameter(x, parameter))

  station_ids <- .il_iwa_normalize_station_ids(stations)
  station_chunks <- if (is.null(station_ids)) {
    list(NULL)
  } else {
    chunk_vec(station_ids, .IL_IWA_STATION_CHUNK_SIZE)
  }

  fetch_page <- .il_iwa_make_page_fetcher(x)
  results <- list()

  total_jobs <- nrow(resources) * length(station_chunks)
  progress_bar <- progress::progress_bar$new(
    total = total_jobs,
    format = paste0(
      "IL_IWA ", parameter,
      " [:bar] :current/:total (:percent) eta: :eta"
    ),
    clear = FALSE,
    width = 80
  )

  for (i in seq_len(nrow(resources))) {
    resource_id <- as.character(resources$id[[i]])

    for (ids in station_chunks) {
      raw <- .il_iwa_fetch_station_resource(
        fetch_page = fetch_page,
        resource_id = resource_id,
        ids = ids,
        station_id_field = station_id_field
      )

      tidy <- switch(
        source_kind,
        discharge = .il_iwa_tidy_discharge(
          raw = raw,
          x = x,
          resource_id = resource_id,
          start_date = range$start_date,
          end_date = range$end_date
        ),
        quality = .il_iwa_tidy_quality(
          raw = raw,
          x = x,
          resource_id = resource_id,
          start_date = range$start_date,
          end_date = range$end_date
        )
      )

      if (nrow(tidy)) results[[length(results) + 1L]] <- tidy
      progress_bar$tick()
    }
  }

  if (!length(results)) return(.il_iwa_empty_for_parameter(x, parameter))

  out <- dplyr::bind_rows(results)
  if (identical(source_kind, "quality")) {
    return(
      out |>
        dplyr::arrange(
          .data$station_id, .data$timestamp, .data$sub_parameter,
          .data$source_record_id
        )
    )
  }

  out |>
    dplyr::distinct(
      .data$station_id, .data$parameter, .data$timestamp,
      .keep_all = TRUE
    ) |>
    dplyr::arrange(.data$station_id, .data$timestamp)
}
