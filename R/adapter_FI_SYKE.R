# -----------------------------------------------------------------------------
#   Base: https://paikkatieto.ymparisto.fi

# Constructor / registration ---------------------------------------------------

register_FI_SYKE <- function() {
  register_service(
    provider_id   = "FI_SYKE",
    provider_name = "Finnish Environment Institute (SYKE)",
    country       = "FI",
    base_url      = "http://rajapinnat.ymparisto.fi",   # TODO: confirm base
    rate_cfg      = list(n = 3, period = 1),
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_FI_SYKE <- function(x, ...) {
  c("water_discharge","water_level",
    "water_temperature","water_temperature_profile",
    "runoff","water_velocity")
}



# -- Parameter mapping --------------------------------------------------------
.fi_param_map <- function(parameter) {
  switch(parameter,
         water_discharge            = list(path = "Virtaama",        unit = "m^3/s"),
         water_level                = list(path = "Vedenkorkeus",    unit = "cm"),     # raw cm (FI returns cm in Arvo)
         water_temperature          = list(path = "LampoPintavesi",  unit = "°C"),     # surface
         water_temperature_profile  = list(path = "LampoLuotaus",   unit = "°C",
                                           depth_field = "Syvyys", depth_unit = "cm"),  # profile depth
         runoff                     = list(path = "Valuma",          unit = "l/s/km2"),
         rlang::abort("FI_SYKE supports 'water_discharge', 'water_level', 'water_temperature', 'water_temperature_profile', or 'runoff'.")
  )
}


# helper: build OData filter for one or many Paikka_Id
.fi_make_id_filter <- function(ids) {
  ids <- unique(trimws(as.character(stats::na.omit(ids))))
  if (!length(ids)) return("(false)")
  nums <- suppressWarnings(as.integer(ids))
  if (all(!is.na(nums))) {
    paste0("(Paikka_Id eq ", paste0(nums, collapse = " or Paikka_Id eq "), ")")
  } else {
    esc <- gsub("'", "''", ids)
    paste0("(Paikka_Id eq '", paste0(esc, collapse = "' or Paikka_Id eq '"), "')")
  }
}

# -- Stations (S3) ------------------------------------------------------------

#' @export
stations.hydro_service_FI_SYKE <- function(x, ...) {
  STATIONS_PATH <- "/api/Hydrologiarajapinta/1.1/odata/Paikka"

  limited <- ratelimitr::limit_rate(
    function() {
      # --- first request -----------------------------------------------------
      req  <- build_request(x, path = STATIONS_PATH)
      resp <- perform_request(req)
      dat  <- httr2::resp_body_json(resp, simplifyVector = TRUE)

      # OData usually returns: $metadata URL, $value (data), and $odata.nextLink
      pull_val  <- function(ct) ct$value
      pull_next <- function(ct) ct[["odata.nextLink"]]

      pages <- list(pull_val(dat))
      next_link <- pull_next(dat)

      # Helper: turn an absolute or relative nextLink into a build_request() call
      build_req_from_next <- function(next_link) {
        u <- httr2::url_parse(next_link)
        # If relative, reuse base host from x$base_url
        if (is.null(u$scheme) || !nzchar(u$scheme)) {
          # next_link like "/api/.../odata/Paikka?$skip=500"
          path <- if (nzchar(u$path)) paste0("/", u$path) else "/"
          return(build_request(x, path = path, query = u$query %||% list()))
        } else {
          # Absolute URL on same host; rebuild to keep consistent request pipeline
          path <- if (nzchar(u$path)) paste0("/", u$path) else "/"
          return(build_request(x, path = path, query = u$query %||% list()))
        }
      }

      # --- pagination --------------------------------------------------------
      while (!is.null(next_link) && nzchar(next_link)) {
        req2  <- build_req_from_next(next_link)
        resp2 <- perform_request(req2)
        dat2  <- httr2::resp_body_json(resp2, simplifyVector = TRUE)
        pages[[length(pages) + 1]] <- pull_val(dat2)
        next_link <- pull_next(dat2)
      }

      # --- rows --------------------------------------------------------------
      # Bind pages defensively
      bind_page <- function(v) {
        if (is.data.frame(v)) return(tibble::as_tibble(v))
        if (is.list(v) && length(v) && is.list(v[[1]])) {
          return(suppressWarnings(dplyr::bind_rows(lapply(v, tibble::as_tibble))))
        }
        tibble::tibble()
      }
      df <- suppressWarnings(dplyr::bind_rows(lapply(pages, bind_page)))
      n  <- nrow(df)

      if (!n || nrow(df) == 0L) {
        return(tibble::tibble(
          country            = x$country,
          provider_id        = x$provider_id,
          provider_name      = x$provider_name,
          place_id           = character(), # needed for time series data
          station_id         = character(),
          station_name       = character(),
          station_name_ascii = character(),
          river              = character(),
          river_ascii        = character(),
          lat                = numeric(),
          lon                = numeric(),
          area               = numeric(),
          altitude           = numeric()
        ))
      }

      # --- columns -----------------------------------------------------------
      code  <- col_or_null(df, "Nro") %||% col_or_null(df, "id")
      Paikka_Id <- col_or_null(df, "Paikka_Id")

      name0 <- col_or_null(df, "Nimi")
      name0 <- normalize_utf8(name0)
      river0 <- col_or_null(df, "PaaVesalNimi")
      river0 <- normalize_utf8(river0)

      # Some stations have this structure in the column 'Nimi': "River, Station"
      has_comma <- !is.na(name0) & grepl(",", name0, fixed = TRUE)
      station_from_name <- ifelse(has_comma,
                                  trimws(sub("^[^,]+,\\s*(.*)$", "\\1", name0, perl = TRUE)),
                                  name0)

      station_final <- station_from_name

      # area_num <- rep(NA_real_, n)
      # area0   <- col_or_null(df, "area")   %||% col_or_null(df, "Area")   %||% col_or_null(df, "catchmentarea")      %||% NA_character_
      # alt0   <- col_or_null(df, "altitude")   %||% col_or_null(df, "elevation")   %||% col_or_null(df, "height")      %||% NA_character_


      # --- coordinates: EPSG:3067 (ETRS-TM35FIN) -> 4326 --------------------
      # Input columns: KoordErTmIta (E), KoordErTmPohj (N)
      e_proj <- suppressWarnings(as.numeric(col_or_null(df, "KoordErTmIta")))
      n_proj <- suppressWarnings(as.numeric(col_or_null(df, "KoordErTmPohj")))

      lon <- rep(NA_real_, n)
      lat <- rep(NA_real_, n)
      ok  <- is.finite(e_proj) & is.finite(n_proj)

      if (any(ok, na.rm = TRUE)) {
        if (!requireNamespace("sf", quietly = TRUE)) {
          stop("Package 'sf' is required for coordinate transformation. Please install.packages('sf').")
        }
        pts <- sf::st_as_sf(
          data.frame(x = e_proj[ok], y = n_proj[ok]),
          coords = c("x", "y"), crs = 3067
        )
        pts_wgs <- sf::st_transform(pts, 4326)
        ll <- sf::st_coordinates(pts_wgs)
        lon[ok] <- ll[, 1]
        lat[ok] <- ll[, 2]
      }

      # --- output schema -----------------------------------------------------
      out <- tibble::tibble(
        country            = x$country,
        provider_id        = x$provider_id,
        provider_name      = x$provider_name,
        place_id           = as.character(Paikka_Id),
        station_id         = as.character(code),
        station_name       = as.character(station_final),
        station_name_ascii = to_ascii(station_final),
        river              = as.character(river0),
        river_ascii        = to_ascii(river0),
        lat                = lat,
        lon                = lon
        # area               = area_num,
        # altitude           = as.numeric(alt0)
      )

      # de-dup in case multiple rows per Paikka_Id
      out <- dplyr::distinct(out, station_id, .keep_all = TRUE)

      meta <- get0("fi_syke_runoff_meta", inherits = TRUE)
      if (!is.null(meta) && is.data.frame(meta) && nrow(meta)) {
        meta2 <- dplyr::select(meta, place_id,
                               area_meta = area,
                               altitude_meta = altitude)
        out <- dplyr::left_join(out, meta2, by = "place_id")

        # Prefer existing API values; if missing, fill from meta
        if (!"area" %in% names(out)) out$area <- NA_real_
        if (!"altitude" %in% names(out)) out$altitude <- NA_real_

        out$area     <- dplyr::coalesce(out$area,     out$area_meta)
        out$altitude <- dplyr::coalesce(out$altitude, out$altitude_meta)

        out <- dplyr::select(out, -dplyr::any_of(c("area_meta","altitude_meta")))
      }
      out
    },
    rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period)
  )

  limited()
}

# --- helper: build OData filter for one or many Paikka_Id --------------------
.fi_make_id_filter <- function(ids) {
  ids <- unique(trimws(as.character(stats::na.omit(ids))))
  if (!length(ids)) return("(false)")
  nums <- suppressWarnings(as.integer(ids))
  if (all(!is.na(nums))) {
    paste0("(Paikka_Id eq ", paste0(nums, collapse = " or Paikka_Id eq "), ")")
  } else {
    esc <- gsub("'", "''", ids)
    paste0("(Paikka_Id eq '", paste0(esc, collapse = "' or Paikka_Id eq '"), "')")
  }
}
# -- Vertical-datum corrections: fetch ALL rows, then filter locally ----------
# Returns long table: place_id, vertical_datum, corr_cm

# -- Vertical-datum corrections: fetch ALL rows, then filter locally ----------
# Returns LONG table: place_id, vertical_datum, corr_cm
.fi_fetch_vertical_corrections_all <- function(x, place_ids = NULL) {
  path <- "/api/Hydrologiarajapinta/1.1/odata/VedenkTasoTieto"

  req  <- build_request(x, path = path)  # no filter; endpoint filter is unreliable
  resp <- perform_request(req)
  dat  <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  pull_val  <- function(ct) ct$value
  pull_next <- function(ct) ct[["odata.nextLink"]]

  pages <- list(pull_val(dat))
  next_link <- pull_next(dat)

  build_req_from_next <- function(next_link) {
    u <- httr2::url_parse(next_link)
    path2 <- if (nzchar(u$path)) paste0("/", u$path) else "/"
    build_request(x, path = path2, query = u$query %||% list())
  }
  while (!is.null(next_link) && nzchar(next_link)) {
    resp2 <- perform_request(build_req_from_next(next_link))
    dat2  <- httr2::resp_body_json(resp2, simplifyVector = TRUE)
    pages[[length(pages) + 1]] <- pull_val(dat2)
    next_link <- pull_next(dat2)
  }

  bind_page <- function(v) {
    if (is.data.frame(v)) return(tibble::as_tibble(v))
    if (is.list(v) && length(v) && is.list(v[[1]])) {
      return(suppressWarnings(dplyr::bind_rows(lapply(v, tibble::as_tibble))))
    }
    tibble::tibble()
  }
  df <- suppressWarnings(dplyr::bind_rows(lapply(pages, bind_page)))
  if (is.null(df) || !nrow(df)) {
    return(tibble::tibble(place_id = character(),
                          vertical_datum = character(),
                          corr_cm = numeric()))
  }

  # NOTE: capital K in TasoKoordinaatisto
  pid  <- col_or_null(df, "Paikka_Id")
  corr <- suppressWarnings(as.numeric(col_or_null(df, "Tasokorjaus")))
  vdat <- col_or_null(df, "TasoKoordinaatisto") %||% col_or_null(df, "Tasokoordinaatisto")

  out <- tibble::tibble(
    place_id       = as.character(pid),
    vertical_datum = as.character(vdat),
    corr_cm        = corr
  )

  if (!is.null(place_ids)) {
    keep_ids <- unique(trimws(as.character(stats::na.omit(place_ids))))
    out <- out[out$place_id %in% keep_ids, , drop = FALSE]
  }
  out
}

# -- Resolve runoff area mapping (Excel or data.frame) -----------------------
# Accepts:
#   - data.frame with columns like place_id/Paikka_Id and area_km2/area/catchment_area
#   - or a path to an Excel file (readxl required)
# Returns tibble: place_id (chr), area_km2 (num), altitude_m (num, optional)
.fi_resolve_runoff_area_map <- function(runoff_area) {
  if (is.null(runoff_area)) {
    return(tibble::tibble(place_id = character(), area_km2 = numeric(), altitude_m = numeric()))
  }

  df <- NULL
  if (is.data.frame(runoff_area)) {
    df <- runoff_area
  } else if (is.character(runoff_area) && length(runoff_area) == 1L) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Please install 'readxl' to load Excel runoff areas, e.g. install.packages('readxl')")
    }
    df <- readxl::read_excel(runoff_area)
  } else {
    rlang::abort("`runoff_area` must be a data.frame or a single Excel file path.")
  }

  if (!nrow(df)) {
    return(tibble::tibble(place_id = character(), area_km2 = numeric(), altitude_m = numeric()))
  }

  nm <- tolower(names(df))

  # place_id candidates
  pid_col_ix <- which(nm %in% c("place_id","paikka_id","paikkaid","station_id","id","ref"))
  if (!length(pid_col_ix)) rlang::abort("Could not find a place_id / Paikka_Id column in `runoff_area`.")
  pid <- df[[pid_col_ix[1]]]

  # area (km2) candidates
  area_col_ix <- which(nm %in% c("area_km2","catchment_area_km2","catchment_km2","area","catchmentarea"))
  if (!length(area_col_ix)) rlang::abort("Could not find an area (km2) column in `runoff_area`.")
  area <- df[[area_col_ix[1]]]

  # altitude (optional)
  alt_col_ix <- which(nm %in% c("altitude","elevation","altitude_m","elev_m","z"))
  altitude <- if (length(alt_col_ix)) df[[alt_col_ix[1]]] else NA_real_

  tibble::tibble(
    place_id   = trimws(as.character(stats::na.omit(pid))),
    area_km2   = suppressWarnings(as.numeric(area)),
    altitude_m = suppressWarnings(as.numeric(altitude))
  ) |>
    dplyr::filter(is.finite(.data$area_km2))
}


#' @export
timeseries.hydro_service_FI_SYKE <- function(x,
                                             parameter = c("water_discharge","water_level",
                                                           "water_temperature","water_temperature_profile","runoff","water_velocity"),
                                             stations = NULL,
                                             start_date = NULL, end_date = NULL,
                                             mode = c("complete","range"),
                                             runoff_area = NULL,
                                             exclude_quality = NULL,
                                             ...) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)
  rng       <- resolve_dates(mode, start_date, end_date)
  pm        <- .fi_param_map(parameter)

  from_dt <- paste0(format(rng$start_date, "%Y-%m-%d"), "T00:00:00")
  to_dt   <- paste0(format(rng$end_date,   "%Y-%m-%d"), "T23:59:59")
  time_field <- "Aika"

  # ID list (Paikka_Id via place_id) and chunking
  all_ids <- if (is.null(stations) || !length(stations)) {
    st <- stations.hydro_service_FI_SYKE(x)
    st$place_id
  } else stations
  all_ids <- unique(trimws(as.character(stats::na.omit(all_ids))))
  if (!length(all_ids)) return(tibble::tibble(
    country=x$country,
    provider_id=x$provider_id,
    provider_name=x$provider_name,
    station_id=character(),
    parameter=character(),
    timestamp=as.POSIXct(character()),
    value=numeric(),
    unit=character(),
    quality_code=character(),
    source_url=character()
  ))

  ids_per_query <- as.integer(list(...)$ids_per_query %||% 10L)
  id_chunks <- chunk_vec(all_ids, ids_per_query)

  pb <- progress::progress_bar$new(total = length(id_chunks))
  fetch_chunk <- ratelimitr::limit_rate(function(id_chunk) {
    pb$tick()

    id_clause <- .fi_make_id_filter(id_chunk)
    time_clause <- if (mode == "range") {
      paste0("(", time_field, " ge datetime'", from_dt, "' and ",
             time_field, " le datetime'", to_dt,   "')")
    } else NULL
    filter_str <- paste0(id_clause, if (!is.null(time_clause)) paste0(" and ", time_clause) else "")

    path  <- paste0("/api/Hydrologiarajapinta/1.1/odata/", pm$path)
    query <- rlang::list2(Arvo = "", `$filter` = filter_str, `$orderby` = paste0(time_field, " asc"))

    # First page
    req  <- build_request(x, path = path, query = query)
    resp <- perform_request(req)
    status <- httr2::resp_status(resp)
    if (status == 404) return(tibble::tibble())
    if (status %in% c(401, 403)) {
      rlang::warn(paste0("FI_SYKE: access denied for station chunk (", length(id_chunk), " ids). Status ", status))
      return(tibble::tibble())
    }

    dat <- httr2::resp_body_json(resp, simplifyVector = TRUE)
    pull_val  <- function(ct) ct$value
    pull_next <- function(ct) ct[["odata.nextLink"]]
    pages <- list(pull_val(dat))
    next_link <- pull_next(dat)

    # Pagination
    build_req_from_next <- function(next_link) {
      u <- httr2::url_parse(next_link)
      path2 <- if (nzchar(u$path)) paste0("/", u$path) else "/"
      build_request(x, path = path2, query = u$query %||% list())
    }
    while (!is.null(next_link) && nzchar(next_link)) {
      resp2 <- perform_request(build_req_from_next(next_link))
      dat2  <- httr2::resp_body_json(resp2, simplifyVector = TRUE)
      pages[[length(pages) + 1]] <- pull_val(dat2)
      next_link <- pull_next(dat2)
    }

    # Bind & parse
    bind_page <- function(v) {
      if (is.data.frame(v)) return(tibble::as_tibble(v))
      if (is.list(v) && length(v) && is.list(v[[1]])) {
        return(suppressWarnings(dplyr::bind_rows(lapply(v, tibble::as_tibble))))
      }
      tibble::tibble()
    }
    df <- suppressWarnings(dplyr::bind_rows(lapply(pages, bind_page)))
    if (is.null(df) || nrow(df) == 0L) return(tibble::tibble())

    ts_raw  <- col_or_null(df, "Aika") %||% col_or_null(df, "time") %||%
      col_or_null(df, "timestamp") %||% col_or_null(df, "dateTime")
    val_raw <- col_or_null(df, "Arvo") %||% col_or_null(df, "value") %||%
      col_or_null(df, "result") %||% col_or_null(df, "y") %||%
      col_or_null(df, "mean")
    qf_raw  <- col_or_null(df, "Laatu") %||% col_or_null(df, "quality") %||%
      col_or_null(df, "qualityFlag") %||% col_or_null(df, "flag")
    sid_col <- col_or_null(df, "Paikka_Id")

    # (optional) depth for profiles
    depth_num <- NULL
    depth_unit <- NULL
    if (identical(parameter, "water_temperature_profile")) {
      depth_field <- pm$depth_field %||% "Syvyys"
      dr <- col_or_null(df, depth_field) %||% col_or_null(df, "depth")
      if (!is.null(dr)) {
        depth_num  <- suppressWarnings(as.numeric(dr))
        depth_unit <- pm$depth_unit %||% "m"
      }
    }

    ts_parsed <- suppressWarnings(lubridate::as_datetime(ts_raw, tz = "UTC"))

    keep <- rep(TRUE, length(ts_parsed))
    if (mode == "range") {
      keep <- !is.na(ts_parsed) &
        ts_parsed >= as.POSIXct(rng$start_date, tz = "UTC") &
        ts_parsed <= as.POSIXct(rng$end_date,   tz = "UTC") + 86399
    }
    if (!is.null(exclude_quality) && !is.null(qf_raw)) {
      keep <- keep & !(qf_raw %in% exclude_quality)
    }
    if (!any(keep, na.rm = TRUE)) return(tibble::tibble())

    res <- tibble::tibble(
      country       = x$country,
      provider_id   = x$provider_id,
      provider_name = x$provider_name,
      place_id    = if (!is.null(sid_col)) as.character(sid_col[keep]) else NA_character_,  # <- FIXED name
      parameter     = parameter,
      timestamp     = ts_parsed[keep],
      value         = suppressWarnings(as.numeric(val_raw[keep])),
      unit          = pm$unit,
      quality_code  = if (is.null(qf_raw)) NA_character_ else as.character(qf_raw[keep]),
      source_url    = paste0(x$base_url, path)
    )

    # append depth columns only for profile and place after `unit`
    if (!is.null(depth_num)) {
      res$depth      <- depth_num[keep]
      res$depth_unit <- depth_unit
      if (requireNamespace("dplyr", quietly = TRUE)) {
        res <- dplyr::relocate(res, depth, depth_unit, .after = unit)
      } else {
        nm  <- names(res); pos <- match("unit", nm)
        left <- nm[seq_len(pos)]; right <- setdiff(nm[(pos+1):length(nm)], c("depth","depth_unit"))
        res <- res[, c(left, "depth", "depth_unit", right), drop = FALSE]
      }
    }
    res  # <- ALWAYS return res
  }, rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period))

  # preserves order / sorts
  res <- dplyr::bind_rows(lapply(id_chunks, fetch_chunk))
  if (nrow(res)) {
    if (!is.null(stations) && length(stations)) {
      lvl <- unique(trimws(as.character(stations)))
      res$place_id <- factor(res$place_id, levels = lvl)
      res <- res[order(res$place_id, res$timestamp), , drop = FALSE]
      res$place_id <- as.character(res$place_id)
    } else {
      res <- res[order(suppressWarnings(as.integer(res$place_id)), res$timestamp), , drop = FALSE]
    }
  }

  # ---- Convert runoff (l/s/km2) -> discharge (m^3/s) ------------------------
  # ---- Runoff: keep l/s/km2 AND add discharge (m^3/s) when area is known ----
  if (nrow(res) && identical(parameter, "runoff")) {
    meta <- get0("fi_syke_runoff_meta", inherits = TRUE)
    if (!is.null(meta) && nrow(meta)) {
      res <- dplyr::left_join(res, meta[, c("place_id","area"), drop = FALSE], by = "place_id")
    } else {
      # ensure 'area' column exists (NA) so downstream code is stable
      if (!"area" %in% names(res)) res$area <- NA_real_
    }

    # m^3/s = (l/s/km^2 * km^2) / 1000 ; will be NA if area is NA
    res$discharge_m3s <- (res$value * res$area) / 1000

    # place area + discharge after unit
    if (requireNamespace("dplyr", quietly = TRUE)) {
      res <- dplyr::relocate(res, area, discharge_m3s, .after = unit)
    }
  }


  if (nrow(res) && identical(parameter, "water_level")) {
    # Fetch all corrections for the sites present in this result
    vc_all <- .fi_fetch_vertical_corrections_all(x, place_ids = unique(res$place_id))
    if (nrow(vc_all)) {

      # 1) Build a wide correction table: one column per datum (in cm)
      sanitize_datum <- function(s) toupper(gsub("[^A-Za-z0-9]+", "_", as.character(s)))
      datums <- sort(unique(vc_all$vertical_datum))
      join_df <- tibble::tibble(place_id = unique(res$place_id))

      for (d in datums) {
        d_san <- sanitize_datum(d)
        sub <- vc_all[vc_all$vertical_datum == d, c("place_id","corr_cm"), drop = FALSE]
        # If >1 rows per (place_id, datum), keep first
        sub <- stats::aggregate(corr_cm ~ place_id, data = sub, FUN = function(z) suppressWarnings(as.numeric(z[1])))
        colname <- paste0("level_correction_", d_san, "_cm")
        names(sub)[names(sub) == "corr_cm"] <- colname
        join_df <- dplyr::left_join(join_df, sub, by = "place_id")
      }

      # 2) Join corrections to time series and compute value_datum_*_cm = value + level_correction_*_cm
      res <- dplyr::left_join(res, join_df, by = "place_id")

      # Ensure raw unit is cm; if not, we still compute in cm assuming 'value' is cm.
      # (FI water_level returns cm, so this is fine.)
      vd_cols <- lc_cols <- character(0)
      for (d in datums) {
        d_san <- sanitize_datum(d)
        corr_col <- paste0("level_correction_", d_san, "_cm")
        if (corr_col %in% names(res)) {
          vd_col <- paste0("value_datum_", d_san, "_cm")
          res[[vd_col]] <- res$value + res[[corr_col]]
          vd_cols <- c(vd_cols, vd_col)
          lc_cols <- c(lc_cols, corr_col)
        }
      }

      # 3) Place columns after `unit`: interleave level_correction_*_cm and value_datum_*_cm
      if (requireNamespace("dplyr", quietly = TRUE)) {
        cols_to_move <- as.vector(rbind(lc_cols, vd_cols))
        cols_to_move <- cols_to_move[cols_to_move %in% names(res)]
        res <- dplyr::relocate(res, dplyr::all_of(cols_to_move), .after = unit)
      } else {
        nm  <- names(res); pos <- match("unit", nm)
        left <- nm[seq_len(pos)]
        right <- setdiff(nm[(pos + 1):length(nm)], c(vd_cols, lc_cols))
        res <- res[, c(left, as.vector(rbind(vd_cols, lc_cols)), right), drop = FALSE]
      }

      # No per-datum unit columns; suffix "_cm" makes units explicit.
    }
  }
  res
}
