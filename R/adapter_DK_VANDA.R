# ==== Denmark (VandA / Miljoportal) adapter =================================
# Base: "https://vandah.miljoeportal.dk"
# Optional GeoJSON for coordinates:     set geo_base_url if there is one; else leave NULL

# -- Registration -------------------------------------------------------------

#' @keywords internal
#' @noRd
register_DK_VANDA <- function() {
  register_service_usage(
    provider_id   = "DK_VANDA",
    provider_name = "VandA (Milj\u00F8portal) API",
    country       = "Denmark",
    base_url      = "https://vandah.miljoeportal.dk",   # TODO: confirm base
    geo_base_url  = NULL,                               # set if a GeoJSON host exists
    rate_cfg      = list(n = 3, period = 1),
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_DK_VANDA <- function(x, ...) {
  c("water_discharge", "water_level")
}

# -- Parameter mapping --------------------------------------------------------
.dk_param_map <- function(parameter) {
  switch(parameter,
         water_level     = list(path = "/api/water-levels",  unit = "cm"),
         water_discharge = list(path = "/api/water-flows",   unit = "m^3/s"),
         stop("Unsupported parameter for DK VANDA: ", parameter)
  )
}

# -- Stations (S3 method) ----------------------------------------------------

#' @export
stations.hydro_service_DK_VANDA <- function(x, ...) {
  STATIONS_PATH <- "/api/stations"

  limited <- ratelimitr::limit_rate(
    function() {
      # --- request -----------------------------------------------------------
      req  <- build_request(x, path = STATIONS_PATH)
      resp <- perform_request(req)
      dat  <- httr2::resp_body_json(resp, simplifyVector = TRUE)

      df <- tibble::as_tibble(dat)
      n  <- nrow(df)

      # --- columns -----------------------------------------------------------
      code   <- col_or_null(df, "stationId")
      if (is.null(code)) code <- col_or_null(df, "id")

      name0  <- col_or_null(df, "name")
      desc0  <- col_or_null(df, "description")
      loc    <- col_or_null(df, "location")  # list-col / df / NULL

      name0 <- normalize_utf8(name0)
      desc0 <- normalize_utf8(desc0)
      alt0   <- col_or_null(df, "altitude")   %||% col_or_null(df, "elevation")   %||% col_or_null(df, "height")      %||% NA_character_

      # --- name split: "River, StationName" ---------------------------------
      has_comma <- !is.na(name0) & grepl(",", name0, fixed = TRUE)
      river_from_name   <- ifelse(has_comma,
                                  trimws(sub("^([^,]+),.*$", "\\1", name0, perl = TRUE)),
                                  NA_character_)
      station_from_name <- ifelse(has_comma,
                                  trimws(sub("^[^,]+,\\s*(.*)$", "\\1", name0, perl = TRUE)),
                                  name0)

      river_final   <- river_from_name
      station_final <- station_from_name

      # --- area (km^2) from description (numeric) ----------------------------
      area_num <- parse_area_km2(desc0)

      # --- coordinates: robust extraction then EPSG -> 4326 -----------------
      loc_x_flat <- if ("location.x"    %in% names(df)) df[["location.x"]]    else NULL
      loc_y_flat <- if ("location.y"    %in% names(df)) df[["location.y"]]    else NULL
      loc_s_flat <- if ("location.srid" %in% names(df)) df[["location.srid"]] else NULL

      get_loc_field <- function(loc_col, field, fallback_flat = NULL) {
        if (!is.null(fallback_flat)) {
          return(suppressWarnings(as.numeric(fallback_flat)))
        }
        if (is.data.frame(loc_col) && field %in% names(loc_col)) {
          return(suppressWarnings(as.numeric(loc_col[[field]])))
        }
        out <- rep(NA_real_, n)
        if (is.list(loc_col)) {
          for (i in seq_len(n)) {
            z <- loc_col[[i]]
            if (is.null(z)) next
            if (is.list(z) && !is.null(z[[field]])) {
              out[i] <- suppressWarnings(as.numeric(z[[field]]))
            } else if (is.data.frame(z) && field %in% names(z)) {
              out[i] <- suppressWarnings(as.numeric(z[[field]]))
            }
          }
        }
        out
      }

      get_loc_srid <- function(loc_col, fallback_flat = NULL) {
        if (!is.null(fallback_flat)) return(as.character(fallback_flat))
        if (is.data.frame(loc_col) && "srid" %in% names(loc_col)) {
          return(as.character(loc_col[["srid"]]))
        }
        out <- rep(NA_character_, n)
        if (is.list(loc_col)) {
          for (i in seq_len(n)) {
            z <- loc_col[[i]]
            if (is.null(z)) next
            if (is.list(z) && !is.null(z[["srid"]])) {
              out[i] <- as.character(z[["srid"]])
            } else if (is.data.frame(z) && "srid" %in% names(z)) {
              out[i] <- as.character(z[["srid"]])
            }
          }
        }
        out
      }

      x_proj <- get_loc_field(loc, "x", fallback_flat = loc_x_flat)
      y_proj <- get_loc_field(loc, "y", fallback_flat = loc_y_flat)
      srid_s <- get_loc_srid (loc,      fallback_flat = loc_s_flat)

      srid_i <- suppressWarnings(as.integer(gsub("[^0-9]", "", srid_s)))
      srid_i[is.na(srid_i)] <- 25832L

      lon <- rep(NA_real_, n)
      lat <- rep(NA_real_, n)
      ok  <- is.finite(x_proj) & is.finite(y_proj)

      if (any(ok, na.rm = TRUE)) {
        if (!requireNamespace("sf", quietly = TRUE)) {
          stop("Package 'sf' is required for coordinate transformation. Please install.packages('sf').")
        }
        for (crs in unique(srid_i[ok])) {
          idx <- ok & srid_i == crs
          pts <- sf::st_as_sf(
            data.frame(x = x_proj[idx], y = y_proj[idx]),
            coords = c("x", "y"), crs = crs
          )
          pts_wgs <- sf::st_transform(pts, 4326)
          ll <- sf::st_coordinates(pts_wgs)
          lon[idx] <- ll[, 1]
          lat[idx] <- ll[, 2]
        }
      }

      # --- output schema -----------------------------------------------------
      tibble::tibble(
        country            = x$country,
        provider_id        = x$provider_id,
        provider_name      = x$provider_name,
        station_id         = as.character(code),
        station_name       = as.character(station_final),
        station_name_ascii = to_ascii(station_final),
        river              = as.character(river_final),
        river_ascii        = to_ascii(river_final),
        lat                = lat,
        lon                = lon,
        area               = area_num,
        altitude           = as.numeric(alt0)
      )
    },
    rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period)
  )

  limited()
}


# -- Time series (S3) ---------------------------------------------------------

#' @export
timeseries.hydro_service_DK_VANDA <- function(x,
                                              parameter = c("water_discharge","water_level"),
                                              stations = NULL,
                                              start_date = NULL, end_date = NULL,
                                              mode = c("complete","range"),
                                              exclude_quality = NULL,
                                              prefer = c("both","dvr90","raw"),
                                              ...) {
  parameter <- match.arg(parameter)
  prefer    <- match.arg(prefer)
  mode      <- match.arg(mode)
  rng       <- resolve_dates(mode, start_date, end_date)

  pm <- .dk_param_map(parameter)

  # Always send a full-day UTC window in RFC3339 without seconds
  date_queries <- list(
    from = paste0(format(rng$start_date, "%Y-%m-%d"), "T00:00Z"),
    to   = paste0(format(rng$end_date,   "%Y-%m-%d"), "T23:59Z")
  )

  ids <- stations %||% character()
  batches <- if (length(ids)) chunk_vec(ids, 50) else list(NULL)

  pb <- progress::progress_bar$new(total = length(batches))
  out <- lapply(batches, function(batch) {
    pb$tick()

    station_vec <- if (is.null(batch)) {
      st <- stations.hydro_service_DK_VANDA(x)
      st$station_id
    } else batch

    one_station <- ratelimitr::limit_rate(function(st_id) {
      q <- c(list(stationId = st_id), date_queries)

      req  <- build_request(x, path = pm$path, query = q, ...)
      if (.Platform$OS.type == "unix") {
        req <- httr2::req_options(req, curl_options = list(HTTP_VERSION = 1L))
        req <- httr2::req_headers(req, Connection = "close")
      }
      resp <- perform_request(req)

      status <- httr2::resp_status(resp)
      if (status == 404) return(tibble::tibble())
      if (status %in% c(401, 403)) {
        rlang::warn(paste0("DK_VANDA: access denied for station ", st_id, " (", status, ")."))
        return(tibble::tibble())
      }

      dat <- httr2::resp_body_json(resp, simplifyVector = TRUE)
      if (is.null(dat) || length(dat) == 0L) return(tibble::tibble())
      df_raw <- if (!is.null(dat$results)) dat$results else dat

      df <- tryCatch({
        if (is.data.frame(df_raw)) tibble::as_tibble(df_raw)
        else if (is.list(df_raw) && length(df_raw) > 0L) suppressWarnings(dplyr::bind_rows(df_raw))
        else tibble::tibble()
      }, error = function(e) tibble::tibble())
      if (nrow(df) == 0L) return(df)

      # Common fields
      ts_raw  <- col_or_null(df, "dateTime") %||%
        col_or_null(df, "measurementDateTime") %||%
        col_or_null(df, "time") %||%
        col_or_null(df, "datetime") %||%
        col_or_null(df, "timestamp")

      val_raw <- col_or_null(df, "value") %||%
        col_or_null(df, "result") %||%
        col_or_null(df, "y") %||%
        col_or_null(df, "mean")

      qf_raw  <- col_or_null(df, "qualityFlag") %||%
        col_or_null(df, "status") %||%
        col_or_null(df, "flag")

      ts_parsed <- suppressWarnings(lubridate::as_datetime(ts_raw, tz = "UTC"))

      keep <- !is.na(ts_parsed) &
        ts_parsed >= as.POSIXct(rng$start_date, tz = "UTC") &
        ts_parsed <= as.POSIXct(rng$end_date,   tz = "UTC") + 86399

      if (!is.null(exclude_quality) && !is.null(qf_raw)) {
        keep <- keep & !(qf_raw %in% exclude_quality)
      }
      if (!any(keep, na.rm = TRUE)) return(tibble::tibble())

      # Extract raw numeric value
      value_raw <- suppressWarnings(as.numeric(val_raw[keep]))
      qf_chr    <- if (is.null(qf_raw)) NA_character_ else as.character(qf_raw[keep])

      # Unit approach you requested
      unit_out <- pm$unit

      # Convert discharge from l/s -> m^3/s (API delivers l/s, we store m^3/s)
      if (parameter == "water_discharge") {
        value_raw <- value_raw / 1000
      }

      # --- Water level special handling: 'resultElevationCorrected' ----------
      value_dvr90 <- NULL
      if (parameter == "water_level") {
        val_corr <- col_or_null(df, "resultElevationCorrected") %||%
          col_or_null(df, "valueElevationCorrected") %||%
          col_or_null(df, "elevationCorrected")

        if (!is.null(val_corr)) {
          value_dvr90 <- suppressWarnings(as.numeric(val_corr[keep]))
        }
      }

      # --- Build per-15min output tibble (then aggregate daily) --------------
      base <- tibble::tibble(
        country       = x$country,
        provider_id   = x$provider_id,
        provider_name = x$provider_name,
        station_id    = st_id,
        parameter     = parameter,
        timestamp     = ts_parsed[keep],
        value         = value_raw,
        unit          = unit_out,
        quality_code  = qf_chr,
        source_url    = paste0(x$base_url, pm$path)
      )

      # Add corrected series when available and requested
      if (parameter == "water_level" && !is.null(value_dvr90)) {
        if (prefer == "dvr90") {
          base$value          <- value_dvr90
          base$unit           <- "m"
          base$vertical_datum <- "DVR90"
        } else if (prefer == "both") {
          base$value_dvr90      <- value_dvr90
          base$value_dvr90_unit <- "m"
          base$vertical_datum   <- "DVR90"
        } else {
          # prefer == "raw": do nothing extra
        }
      }

      # --- Aggregate 15-min to daily means (UTC) -----------------------------
      base$date <- as.Date(base$timestamp, tz = "UTC")

      agg_cols <- c("value")
      if ("value_dvr90" %in% names(base)) agg_cols <- c("value", "value_dvr90")

      daily <- base |>
        dplyr::group_by(country, provider_id, provider_name, station_id, parameter, date) |>
        dplyr::summarise(
          dplyr::across(dplyr::all_of(agg_cols), ~ mean(.x, na.rm = TRUE)),
          unit             = dplyr::first(unit),
          quality_code     = {
            qc <- quality_code[!is.na(quality_code)]
            if (length(qc)) qc[[1]] else NA_character_
          },
          source_url       = dplyr::first(source_url),
          value_dvr90_unit = if ("value_dvr90_unit" %in% names(base)) dplyr::first(value_dvr90_unit) else NULL,
          vertical_datum   = if ("vertical_datum" %in% names(base)) dplyr::first(vertical_datum) else NULL,
          .groups = "drop"
        ) |>
        dplyr::mutate(timestamp = as.POSIXct(date, tz = "UTC")) |>
        dplyr::select(-date)

      # Restore desired column order explicitly
      if ("value_dvr90" %in% names(daily)) {
        daily <- daily |>
          dplyr::select(country, provider_id, provider_name, station_id, parameter,
                        timestamp, value, unit, quality_code, source_url,
                        value_dvr90, value_dvr90_unit, vertical_datum)
      } else if ("vertical_datum" %in% names(daily)) {
        daily <- daily |>
          dplyr::select(country, provider_id, provider_name, station_id, parameter,
                        timestamp, value, unit, quality_code, source_url,
                        vertical_datum)
      } else {
        daily <- daily |>
          dplyr::select(country, provider_id, provider_name, station_id, parameter,
                        timestamp, value, unit, quality_code, source_url)
      }

      daily
    }, rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period))

    dplyr::bind_rows(lapply(station_vec, one_station))
  })

  dplyr::bind_rows(out)
}

