# ==== Poland (IMGW Public Data) adapter ======================================
# Base: "https://danepubliczne.imgw.pl"
# Near-real-time JSON + embedded metadata: /api/data/hydro
# Historical daily (per-year zip): /data/dane_pomiarowo_obserwacyjne/dane_hydrologiczne/dobowe/{YYYY}/zjaw_{YYYY}.zip

# -----------------------------------------------------------------------------
# Registration
# -----------------------------------------------------------------------------

#' @export
register_PL_IMGW <- function() {
  register_service(
    provider_id   = "PL_IMGW",
    provider_name = "Poland – IMGW Public Data",
    country       = "PL",
    base_url      = "https://danepubliczne.imgw.pl",
    rate_cfg      = list(n = 3, period = 1),
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_PL_IMGW <- function(x, ...) {
  c("water_discharge", "water_level", "water_temperature")
}
# -----------------------------------------------------------------------------
# Parameter mapping (private)
# -----------------------------------------------------------------------------

.pl_param_map <- function(parameter) {
  switch(parameter,
         water_level = list(
           unit            = "cm",
           hist_file_month = function(y, m)
             sprintf("/data/dane_pomiarowo_obserwacyjne/dane_hydrologiczne/dobowe/%d/codz_%04d_%02d.zip", y, y, as.integer(m)),
           hist_file_year  = function(y)
             sprintf("/data/dane_pomiarowo_obserwacyjne/dane_hydrologiczne/dobowe/%d/codz_%04d.zip", y, y),
           hist_value_col  = "Water level [cm]"
         ),
         water_discharge = list(
           unit            = "m^3/s",
           hist_file_month = function(y, m)
             sprintf("/data/dane_pomiarowo_obserwacyjne/dane_hydrologiczne/dobowe/%d/codz_%04d_%02d.zip", y, y, as.integer(m)),
           hist_file_year  = function(y)
             sprintf("/data/dane_pomiarowo_obserwacyjne/dane_hydrologiczne/dobowe/%d/codz_%04d.zip", y, y),
           hist_value_col  = "Flow [m^3/s]"
         ),
         water_temperature = list(
           unit            = "°C",
           hist_file_month = function(y, m)
             sprintf("/data/dane_pomiarowo_obserwacyjne/dane_hydrologiczne/dobowe/%d/codz_%04d_%02d.zip", y, y, as.integer(m)),
           hist_file_year  = function(y)
             sprintf("/data/dane_pomiarowo_obserwacyjne/dane_hydrologiczne/dobowe/%d/codz_%04d.zip", y, y),
           hist_value_col  = "Water temperature [deg. C]"
         ),
         rlang::abort("PL_IMGW supports 'water_discharge', 'water_level', 'water_temperature'.")
  )
}

# -----------------------------------------------------------------------------
# Small helpers (private)
# -----------------------------------------------------------------------------

.num_pl <- function(x) suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))

.mv_level_cm <- function(x) { x <- suppressWarnings(as.numeric(x)); x[x == 9999] <- NA_real_; x }
.mv_flow_ms  <- function(x) { x <- suppressWarnings(as.numeric(x)); x[x %in% c(99999.999, 999)] <- NA_real_; x }
.mv_temp_c   <- function(x) { x <- suppressWarnings(as.numeric(x)); x[x == 99.9] <- NA_real_; x }

# Cache location resolver
.pl_cache_base <- function(cache_dir = NULL) {
  if (!is.null(cache_dir) && dir.exists(cache_dir)) return(normalizePath(cache_dir, winslash = "/"))
  # Prefer a user cache dir if available, fall back to tempdir()
  base <- tryCatch(tools::R_user_dir("hydrodownloadR", which = "cache"), error = function(e) tempdir())
  if (!dir.exists(base)) dir.create(base, recursive = TRUE, showWarnings = FALSE)
  base
}


# Read + normalize a historical IMGW daily CSV (handles headerless files & 2024 fully-quoted rows)
.pl_read_hist_csv <- function(csv_file) {
  expected_names <- c(
    "Station code",
    "Station name",
    "River",
    "Hydrological year",
    "Month indicator in the hydrological year",
    "Day",
    "Water level [cm]",
    "Flow [m^3/s]",
    "Water temperature [deg. C]",
    "Calendar month"
  )

  loc <- readr::locale(encoding = "CP1250", decimal_mark = ".")
  first_line <- tryCatch(readr::read_lines(csv_file, n_max = 1, locale = loc), error = function(e) "")
  has_semicol <- is.character(first_line) && length(first_line) && grepl(";", first_line, fixed = TRUE)
  has_comma   <- is.character(first_line) && length(first_line) && grepl(",", first_line, fixed = TRUE)
  delim <- if (has_semicol) ";" else ","

  .norm_chr_cols <- function(df) {
    chr_cols <- names(df)[vapply(df, is.character, logical(1))]
    for (cc in chr_cols) df[[cc]] <- normalize_utf8(df[[cc]])
    df
  }

  # --- Strategy: always read headerless, then assign names; special 2024 fix ---
  # 1) Try readr, headerless
  df0 <- suppressWarnings(
    readr::read_delim(
      file = csv_file, delim = delim, locale = loc,
      col_names = FALSE, show_col_types = FALSE, trim_ws = TRUE,
      quote = "\"", na = c("", "NA")
    )
  )

  # 1a) 2024-style fully-quoted single-column fallback
  if (is.data.frame(df0) && ncol(df0) == 1 && (has_comma || has_semicol)) {
    lines <- readr::read_lines(csv_file, locale = loc)
    # Strip only one leading and one trailing quote if present
    lines <- sub('^"', '', lines)
    lines <- sub('"$', '', lines)
    lines <- gsub('""', '"', lines, fixed = TRUE)
    sep <- if (any(grepl(";", lines, fixed = TRUE))) ";" else ","
    df0 <- utils::read.table(text = lines, sep = sep, quote = "\"",
                             dec = ".", header = FALSE, fill = TRUE,
                             comment.char = "", stringsAsFactors = FALSE,
                             na.strings = c("", "NA"))
  }

  # If column count is off (rare), try base read.table headerless as a last resort
  if (!is.data.frame(df0) || ncol(df0) != length(expected_names)) {
    df0 <- tryCatch(
      utils::read.table(file = csv_file, sep = delim, quote = "\"",
                        dec = ".", header = FALSE, fill = TRUE,
                        comment.char = "", stringsAsFactors = FALSE,
                        na.strings = c("", "NA")),
      error = function(e) data.frame()
    )
  }

  if (!nrow(df0) || ncol(df0) != length(expected_names)) {
    # Give up gracefully
    return(tibble::tibble())
  }

  # Assign canonical names
  names(df0) <- expected_names
  df <- tibble::as_tibble(df0)

  # Drop first row ONLY if it is an exact header row (position-wise match)
  is_exact_header <- {
    r1 <- as.character(unlist(df[1, ], use.names = FALSE))
    all(trimws(r1) == expected_names)
  }
  if (is_exact_header) {
    df <- dplyr::slice(df, -1)
  }

  # Normalize strings to UTF-8
  df <- .norm_chr_cols(df)

  # Build Date and drop the source columns + "Calendar month"
  df <- df |>
    dplyr::mutate(
      `Hydrological year` = suppressWarnings(as.integer(`Hydrological year`)),
      `Month indicator in the hydrological year` = suppressWarnings(as.integer(`Month indicator in the hydrological year`)),
      Day = suppressWarnings(as.integer(Day)),
      Date = as.Date(
        sprintf("%04d-%02d-%02d",
                `Hydrological year`,
                `Month indicator in the hydrological year`,
                Day),
        format = "%Y-%m-%d"
      )
    ) |>
    dplyr::select(
      -`Hydrological year`,
      -`Month indicator in the hydrological year`,
      -Day,
      -`Calendar month`
    )

  # Move Date after "River" (if present)
  if ("River" %in% names(df)) {
    df <- dplyr::relocate(df, Date, .after = "River")
  }

  tibble::as_tibble(df)
}

# Parse "18° 17' 14,311\" E" or "49° 59' 37,035\" N" to decimal degrees
.pl_dms_to_decimal <- function(x, kind = c("lat", "lon")) {
  kind <- match.arg(kind)
  if (length(x) == 0L) return(numeric())

  parse_one <- function(s) {
    if (is.na(s) || !nzchar(s)) return(NA_real_)
    s0 <- normalize_utf8(as.character(s))
    s0 <- gsub(",", ".", s0, fixed = TRUE)
    s0 <- gsub("\\s+", " ", s0)
    s0 <- gsub("[°º]", " ", s0)
    s0 <- gsub("(deg|Deg|DEG)", " ", s0)
    s0 <- gsub("[′’']",  " ", s0)
    s0 <- gsub("[″”\"]", " ", s0)
    s0 <- trimws(s0)

    hemi <- NA_character_
    if (grepl("[Nn]", s0)) hemi <- "N"
    if (grepl("[Ss]", s0)) hemi <- "S"
    if (grepl("[Ee]", s0)) hemi <- "E"
    if (grepl("[Ww]", s0)) hemi <- "W"

    nums <- regmatches(s0, gregexpr("[-+]?[0-9]+(?:\\.[0-9]+)?", s0))[[1]]
    if (length(nums) == 0L) return(NA_real_)
    nums <- suppressWarnings(as.numeric(nums))

    if (length(nums) == 1L && !grepl("[NnSsEeWw]", s0)) {
      val <- nums[1L]
      if (!is.na(hemi)) {
        if (hemi %in% c("S","W")) val <- -abs(val) else val <- abs(val)
      }
      return(val)
    }

    d <- nums[1L]; m <- if (length(nums) >= 2L) nums[2L] else 0; s <- if (length(nums) >= 3L) nums[3L] else 0
    sign_val <- if (!is.na(d) && d < 0) -1 else 1
    if (!is.na(hemi)) sign_val <- if (hemi %in% c("S","W")) -1 else 1

    d <- abs(d)
    dec <- d + (m %||% 0)/60 + (s %||% 0)/3600
    dec * sign_val
  }

  vapply(x, parse_one, numeric(1))
}


# --- Column parser: try decimal; if many NAs, parse DMS row-wise -------------
# Returns numeric degrees intended for EPSG:4326 (WGS84)
parse_coord_col <- function(x, kind = c("lat", "lon")) {
  kind <- match.arg(kind)
  if (is.null(x)) return(numeric(0))
  if (!length(x))  return(numeric(0))

  # classify rows
  dec_idx <- .is_decimalish(x)
  dms_idx <- !dec_idx & grepl("[°º′’'″”\"]|\\b[NSWE]\\b", x, ignore.case = TRUE)

  out <- rep(NA_real_, length(x))

  # parse decimal-like rows
  if (any(dec_idx)) {
    x_dec <- gsub(",", ".", x[dec_idx], fixed = TRUE)
    out[dec_idx] <- suppressWarnings(as.numeric(x_dec))
  }

  # parse DMS-like rows
  if (any(dms_idx, na.rm = TRUE)) {
    out[dms_idx] <- .pl_dms_to_decimal(x[dms_idx], kind = kind)
  }

  # everything else stays NA

  # enforce WGS84 ranges
  if (kind == "lat") {
    out[out < -90 | out > 90] <- NA_real_
  } else {
    out[out < -180 | out > 180] <- NA_real_
  }

  out
}

# Read IMGW station list CSV (headerless, CP1250) from HTTP response
.pl_read_station_list_csvtext <- function(txt) {
  # text already decoded to UTF-8; parse quoted CSV, 4 columns, no header
  df <- suppressWarnings(
    readr::read_csv(
      I(txt),
      col_names = FALSE,
      locale = readr::locale(encoding = "UTF-8"),
      show_col_types = FALSE,
      trim_ws = TRUE,
      quote = "\""
    )
  )
  # Expect at least first 3 columns: id, name, river (4th often a code we ignore)
  if (!nrow(df)) return(tibble::tibble())
  n <- ncol(df)
  if (n < 3) return(tibble::tibble())
  names(df)[1:min(4, n)] <- c("station_id", "station_name", "river", "col4")[1:min(4, n)]
  tibble::as_tibble(df[, c("station_id","station_name","river")[1:min(3, n)], drop = FALSE])
}

# Optional override via option:
#   options(hydrodownloadR.PL_IMGW_metadata_xlsx = "/some/where/Metadata_GRDC_30.10.2025.xlsx")
.pl_meta_xlsx_path <- function() {
  opt <- getOption("hydrodownloadR.PL_IMGW_metadata_xlsx", NULL)
  if (is.character(opt) && nzchar(opt)) return(opt)
  .pl_meta_paths()$raw_xlsx
}

# ---- Build the RDA cache from the XLSX (once) --------------------------------
.pl_build_metadata_cache <- function(verbose = TRUE) {
  paths <- .pl_meta_paths()
  xlsx  <- paths$raw_xlsx
  rds   <- paths$rds

  if (!file.exists(xlsx)) {
    if (verbose) rlang::inform(sprintf("PL_IMGW: metadata XLSX not found at %s", xlsx))
    return(NULL)
  }

  md <- .pl_parse_imgw_metadata_xlsx(xlsx)
  if (is.null(md) || !nrow(md)) {
    if (verbose) rlang::warn("PL_IMGW: failed to parse IMGW metadata XLSX.")
    return(NULL)
  }

  stamp <- tryCatch(file.info(xlsx)$mtime, error = function(e) Sys.time())
  md$source_file  <- normalizePath(xlsx, winslash = "/", mustWork = FALSE)
  md$source_stamp <- as.POSIXct(stamp, tz = "UTC")

  dir.create(dirname(rds), recursive = TRUE, showWarnings = FALSE)
  saveRDS(md, rds, compress = "xz")  # <— RDS, not RDA
  if (verbose) {
    rlang::inform(sprintf(
      "PL_IMGW: compiled metadata cache (RDS) with %d stations at %s (UTC).",
      nrow(md), format(md$source_stamp[1], "%Y-%m-%d %H:%M:%S %Z", tz = "UTC")
    ))
  }
  invisible(md)
}


.is_decimalish <- function(x) {
  x0 <- gsub(",", ".", as.character(x), fixed = TRUE)
  # allow leading/trailing spaces and +/-, pure numbers only
  grepl("^\\s*[+-]?[0-9]+(?:\\.[0-9]+)?\\s*$", x0)
}

# ---- 2) Parse the IMGW Excel into a tidy metadata tibble ---------------------
.pl_parse_imgw_metadata_xlsx <- function(xlsx_path) {
  if (!file.exists(xlsx_path)) return(NULL)
  if (!requireNamespace("readxl", quietly = TRUE)) {
    rlang::warn("Package 'readxl' is required to read IMGW metadata Excel. Install it to enable enrichment.")
    return(NULL)
  }

  df <- tryCatch(readxl::read_excel(xlsx_path), error = function(e) NULL)
  if (is.null(df) || !nrow(df)) return(NULL)

  # --- Header normalization: remove CR/LF, diacritics, punctuation, spaces ----
  norm_text <- function(s) {
    s <- normalize_utf8(s)
    s <- gsub("\\s+", " ", s)              # collapse whitespace (incl. CR/LF)
    s <- tolower(s)
    s <- iconv(s, to = "ASCII//TRANSLIT")  # drop accents (Ż -> Z, Ł -> L, etc.)
    gsub("[^a-z0-9]+", "", s)              # keep only [a-z0-9]
  }
  raw_names <- names(df)
  norm_names <- vapply(raw_names, norm_text, character(1))
  # fast lookup map: normalized -> original index
  name_map <- stats::setNames(seq_along(norm_names), norm_names)

  pick_col <- function(candidates_norm) {
    # candidates_norm: character vector of *normalized* keys
    for (cn in candidates_norm) {
      idx <- name_map[[cn]]
      if (!is.null(idx)) return(df[[idx]])
    }
    NULL
  }

  # Build normalized candidate keys for each field
  key <- function(...) norm_text(c(...))

  station_code   <- pick_col(key("Station code","Kod stacji","ID","Kod","objID","gauge_id"))
  station_name   <- pick_col(key("Station name","Nazwa stacji","Nazwa","STATION_NAME"))
  river_name     <- pick_col(key("River/Lake","River","Rzeka/Jezioro","Rzeka","STREAM_NAME"))
  lat_col_raw    <- pick_col(key("Latitude (decimal degree)","Latitude","Szerokość geograficzna","GEOGR1"))
  lon_col_raw    <- pick_col(key("Longitude (decimal degree)","Longitude","Długość geograficzna","GEOGR2",
                                 # explicitly cover CR/LF headers like "Longitude\r\n(decimal degree)"
                                 "Longitude (decimal degree)"))
  area_col_raw   <- pick_col(key("Catchment area (square kilometre)","Powierzchnia zlewni [km2]","PLO_STA","Catchment area"))
  alt_col_raw    <- pick_col(key("Height of gauge zero (m above sea level)","Wysokość n.p.m.","H0 [m a.s.l.]","Height of gauge zero"))
  vrf_col_raw    <- pick_col(key("Vertical reference system","Układ wysokościowy","Vertical datum"))

  # --- Coords: accept decimal or DMS -----------------------------------------
  to_num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))

  # parse_coord_col <- function(x) {
  #   if (is.null(x)) return(rep(NA_real_, nrow(df)))
  #   # try decimal first
  #   dec <- to_num(x)
  #   # if most are NA, try DMS -> decimal (rowwise)
  #   na_ratio <- mean(is.na(dec))
  #   if (isTRUE(na_ratio > 0.5)) {
  #     return(vapply(as.character(x), .pl_dms_to_decimal, numeric(1)))
  #   }
  #   dec
  # }

  lat <- parse_coord_col(lat_col_raw, kind = "lat")
  lon <- parse_coord_col(lon_col_raw, kind = "lon")

  # --- Other numerics ---------------------------------------------------------
  area <- if (is.null(area_col_raw)) rep(NA_real_, nrow(df)) else to_num(area_col_raw)
  alt  <- if (is.null(alt_col_raw))  rep(NA_real_, nrow(df)) else to_num(alt_col_raw)

  out <- tibble::tibble(
    station_id         = as.character(station_code),
    station_name       = normalize_utf8(as.character(station_name)),
    river              = normalize_utf8(as.character(river_name)),
    lat_md             = lat,
    lon_md             = lon,
    area_md            = area,
    altitude_md        = alt,
    vertical_datum_md  = if (!is.null(vrf_col_raw)) normalize_utf8(as.character(vrf_col_raw)) else NA_character_
  )

  # Clean up invalid rows
  out <- out[!is.na(out$station_id) & nzchar(out$station_id), , drop = FALSE]
  out <- out[!duplicated(out$station_id), , drop = FALSE]
  if (!nrow(out)) return(NULL)

  out
}


# ---- Load the RDA cache; build it if missing (best-effort) -------------------
.pl_load_metadata_cache <- function(verbose = TRUE) {
  paths <- .pl_meta_paths()
  rds   <- paths$rds
  rda   <- paths$legacy_rda

  # Prefer RDS
  if (file.exists(rds)) {
    md <- tryCatch(readRDS(rds), error = function(e) NULL)
    if (is.data.frame(md) && nrow(md)) return(md)
    if (verbose) rlang::warn("PL_IMGW: metadata RDS exists but could not be read or was empty.")
  }

  # Legacy migration path: if an old RDA exists (even if misnamed RDS)
  if (file.exists(rda)) {
    # Try RDS first (some older code savedRDS with .rda suffix)
    md <- tryCatch(readRDS(rda), error = function(e) NULL)
    if (is.null(md)) {
      # Then try a true RDA via load()
      env <- new.env(parent = emptyenv())
      ok  <- tryCatch({ load(rda, envir = env); TRUE }, error = function(e) FALSE)
      if (ok) {
        # Pick first data.frame object
        obj_names <- ls(env, all.names = TRUE)
        for (nm in obj_names) {
          obj <- get(nm, envir = env)
          if (is.data.frame(obj) && nrow(obj)) { md <- obj; break }
        }
      }
    }
    if (is.data.frame(md) && nrow(md)) {
      # Successful migration: write proper RDS and remove legacy .rda
      dir.create(dirname(rds), recursive = TRUE, showWarnings = FALSE)
      saveRDS(md, rds, compress = "xz")
      try(unlink(rda), silent = TRUE)
      if (verbose) rlang::inform("PL_IMGW: migrated legacy pl_imgw_meta.rda to pl_imgw_meta.rds.")
      return(md)
    } else if (verbose) {
      rlang::warn("PL_IMGW: legacy pl_imgw_meta.rda exists but could not be read.")
    }
  }

  if (verbose) rlang::inform("PL_IMGW: metadata cache not found.")
  NULL
}



# Ensure metadata cache exists; if missing, build from XLSX; then load.
.pl_ensure_and_load_metadata <- function(verbose = TRUE) {
  md <- .pl_load_metadata_cache(verbose = verbose)
  if (!is.null(md)) return(md)
  .pl_build_metadata_cache(verbose = verbose)
  .pl_load_metadata_cache(verbose = verbose)
}


# Find a project root (dev mode) by walking up from getwd()
.pl_find_proj_root <- function() {
  cur <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = FALSE), error = function(e) ".")
  prev <- ""
  repeat {
    if (!nzchar(cur) || identical(cur, prev)) break
    has_rproj <- length(list.files(cur, pattern = "\\.Rproj$", all.files = TRUE, no.. = TRUE)) > 0
    has_desc  <- file.exists(file.path(cur, "DESCRIPTION"))
    if (has_rproj || has_desc) return(cur)
    prev <- cur
    cur  <- dirname(cur)
  }
  NULL
}

# Robust paths for IMGW metadata (XLSX source + compiled RDS)
.pl_meta_paths <- function() {
  # 0) Explicit override: let users point at any XLSX they have
  opt_xlsx <- getOption("hydrodownloadR.PL_IMGW_metadata_xlsx", NULL)
  if (is.character(opt_xlsx) && nzchar(opt_xlsx)) {
    user_data_dir <- tryCatch(tools::R_user_dir("hydrodownloadR", which = "data"),
                              error = function(e) file.path(path.expand("~"), "hydrodownloadR", "data"))
    dir.create(user_data_dir, recursive = TRUE, showWarnings = FALSE)
    return(list(
      raw_xlsx   = normalizePath(opt_xlsx, winslash = "/", mustWork = FALSE),
      rds        = file.path(user_data_dir, "pl_imgw_meta.rds"),
      legacy_rda = file.path(user_data_dir, "pl_imgw_meta.rda")
    ))
  }

  # 1) Dev mode: if we are inside the package project and XLSX exists in ./data-raw
  proj <- .pl_find_proj_root()
  if (!is.null(proj)) {
    proj_raw <- file.path(proj, "data-raw", "Metadata_GRDC_30.10.2025.xlsx")
    if (file.exists(proj_raw)) {
      user_data_dir <- tryCatch(tools::R_user_dir("hydrodownloadR", which = "data"),
                                error = function(e) file.path(path.expand("~"), "hydrodownloadR", "data"))
      dir.create(user_data_dir, recursive = TRUE, showWarnings = FALSE)
      return(list(
        raw_xlsx   = normalizePath(proj_raw, winslash = "/", mustWork = FALSE),
        rds        = file.path(user_data_dir, "pl_imgw_meta.rds"),
        legacy_rda = file.path(user_data_dir, "pl_imgw_meta.rda")
      ))
    }
  }

  # 2) User-mode fallback: compiled cache in user data dir; XLSX optional
  user_data_dir <- tryCatch(tools::R_user_dir("hydrodownloadR", which = "data"),
                            error = function(e) file.path(path.expand("~"), "hydrodownloadR", "data"))
  user_raw_dir  <- file.path(dirname(user_data_dir), "data-raw")
  dir.create(user_data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(user_raw_dir,  recursive = TRUE, showWarnings = FALSE)

  list(
    raw_xlsx   = file.path(user_raw_dir, "Metadata_GRDC_30.10.2025.xlsx"),  # may or may not exist
    rds        = file.path(user_data_dir, "pl_imgw_meta.rds"),
    legacy_rda = file.path(user_data_dir, "pl_imgw_meta.rda")
  )
}




# -----------------------------------------------------------------------------
# Stations (S3)
# -----------------------------------------------------------------------------

#' @export
stations.hydro_service_PL_IMGW <- function(x, use_local_metadata = TRUE, ...) {
  # --- Base station list (public CSV) ----------------------------------------
  path <- "/data/dane_pomiarowo_obserwacyjne/dane_hydrologiczne/lista_stacji_hydro.csv"
  req  <- build_request(x, path = path)
  resp <- perform_request(req)

  csv_txt <- tryCatch(
    httr2::resp_body_string(resp, encoding = "CP1250"),
    error = function(e) ""
  )

  base_df <- if (nzchar(csv_txt)) .pl_read_station_list_csvtext(csv_txt) else tibble::tibble()
  if (!nrow(base_df)) {
    return(tibble::tibble(
      country            = character(),
      provider_id        = character(),
      provider_name      = character(),
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

  base_df$station_name <- normalize_utf8(base_df$station_name)
  base_df$river        <- normalize_utf8(base_df$river)
  base_df$station_id   <- as.character(base_df$station_id)

  out <- tibble::tibble(
    country            = x$country,
    provider_id        = x$provider_id,
    provider_name      = x$provider_name,
    station_id         = base_df$station_id,
    station_name       = base_df$station_name,
    station_name_ascii = to_ascii(base_df$station_name),
    river              = base_df$river,
    river_ascii        = to_ascii(base_df$river),
    lat                = NA_real_,
    lon                = NA_real_,
    area               = NA_real_,
    altitude           = NA_real_
  )

  # --- Enrich from local cache (compiled from XLSX) --------------------------
  if (isTRUE(use_local_metadata)) {
    md <- .pl_ensure_and_load_metadata(verbose = FALSE)
    if (is.data.frame(md) && nrow(md)) {
      out <- dplyr::left_join(out, md, by = "station_id", suffix = c("", "_md"))

      out$station_name <- ifelse(!is.na(out$station_name_md) & nzchar(out$station_name_md),
                                 out$station_name_md, out$station_name)
      out$river        <- ifelse(!is.na(out$river_md) & nzchar(out$river_md),
                                 out$river_md, out$river)
      out$lat          <- ifelse(!is.na(out$lat_md), out$lat_md, out$lat)
      out$lon          <- ifelse(!is.na(out$lon_md), out$lon_md, out$lon)
      out$area         <- ifelse(!is.na(out$area_md), out$area_md, out$area)
      out$altitude     <- ifelse(!is.na(out$altitude_md), out$altitude_md, out$altitude)

      out$station_name_ascii <- to_ascii(out$station_name)
      out$river_ascii        <- to_ascii(out$river)

      # Inform once: timestamp + provenance
      stamp <- tryCatch(format(md$source_stamp[1], "%Y-%m-%d %H:%M:%S %Z", tz = "UTC"),
                        error = function(e) NA_character_)
      if (!is.na(stamp)) {
        rlang::inform(paste0(
          "PL_IMGW: station metadata enriched from IMGW Excel (", stamp, " UTC). These metadata were provided by IMGW."
        ))
      } else {
        rlang::inform("PL_IMGW: station metadata enriched from IMGW Excel. These metadata were provided by IMGW.")
      }

      out <- dplyr::select(out, -dplyr::ends_with("_md"))
    } else {
      rlang::inform("PL_IMGW: no local IMGW metadata cache found; returning base list without enrichment.")
    }
  }


  # Final dedup + NA id guard
  out <- out[!is.na(out$station_id) & nzchar(out$station_id) & !duplicated(out$station_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}


# -----------------------------------------------------------------------------
# Timeseries (S3)
# -----------------------------------------------------------------------------
# --- Safe GET for historical ZIPs: don't error on 404 etc. -------------------
.pl_safe_get <- function(x, path) {
  req <- build_request(x, path = path)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)  # don't throw on 4xx/5xx
  resp <- perform_request(req)
  list(resp = resp, status = httr2::resp_status(resp))
}
# ---------- MASTER (parameter-agnostic, WIDE) --------------------------------
# Wide schema columns (minimal):
#   station_id (chr), timestamp (POSIXct, UTC date),
#   wl_cm (dbl), q_m3s (dbl), tw_c (dbl),
#   source_url (chr)
# We store provider/country in the object header to avoid repeating per row.

.pl_master_cache_path <- function(cache_dir = NULL) {
  base <- .pl_cache_base(cache_dir)
  file.path(base, "PL_IMGW_all_timeseries.rds")
}

.pl_master_wide_save <- function(ts_wide, x, cache_dir = NULL, compress = "xz") {
  path <- .pl_master_cache_path(cache_dir)
  obj  <- list(
    type          = "wide",
    stamp         = Sys.time(),
    meta          = list(country = x$country, provider_id = x$provider_id, provider_name = x$provider_name),
    data_wide     = ts_wide
  )
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(obj, path, compress = compress)
  ts <- format(file.info(path)$mtime, "%Y-%m-%d %H:%M:%S %Z", tz = "UTC")
  rlang::inform(sprintf(
    "PL_IMGW: saved WIDE master with %s rows at %s\n→ %s\nUse update = TRUE to refresh.",
    format(nrow(ts_wide), big.mark = ","), ts, path
  ))
  invisible(path)
}

.pl_master_load <- function(cache_dir = NULL) {
  path <- .pl_master_cache_path(cache_dir)
  if (!file.exists(path)) return(NULL)
  obj <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(obj)) return(NULL)
  obj
}

.pl_master_note <- function(cache_dir = NULL) {
  path <- .pl_master_cache_path(cache_dir)
  if (!file.exists(path)) return(invisible())
  ts <- format(file.info(path)$mtime, "%Y-%m-%d %H:%M:%S %Z", tz = "UTC")
  rlang::inform(sprintf("PL_IMGW: using WIDE master saved %s. Set update = TRUE to rebuild.", ts))
}

# ---------- Convert a parsed CSV (wide) into master-wide row(s) --------------
# df is result of .pl_read_hist_csv (has Date, 'Station code', and 3 value cols)
# Returns tibble: station_id, timestamp, wl_cm, q_m3s, tw_c, source_url
.pl_chunk_to_wide <- function(df, src_url) {
  if (!nrow(df)) return(tibble::tibble())
  if (!all(c("Date","Station code") %in% names(df))) return(tibble::tibble())

  ts  <- as.POSIXct(df$Date, tz = "UTC")

  wl  <- if ("Water level [cm]" %in% names(df)) df[["Water level [cm]"]] else NA_real_
  q   <- if ("Flow [m^3/s]" %in% names(df))     df[["Flow [m^3/s]"]]     else NA_real_
  tw  <- if ("Water temperature [deg. C]" %in% names(df)) df[["Water temperature [deg. C]"]] else NA_real_

  # Sentinels + numeric
  wl <- .mv_level_cm(wl)
  q  <- .mv_flow_ms(q)
  tw <- .mv_temp_c(tw)

  tibble::tibble(
    station_id = as.character(df[["Station code"]]),
    timestamp  = ts,
    wl_cm      = suppressWarnings(as.numeric(wl)),
    q_m3s      = suppressWarnings(as.numeric(q)),
    tw_c       = suppressWarnings(as.numeric(tw)),
    source_url = src_url
  )
}

# ---------- Build the WIDE master from source (no per-month/year caches) -----
.pl_build_master_from_source <- function(
    x,
    cache_dir = NULL,
    from_year = 1951L,
    to_year   = as.integer(format(Sys.Date(), "%Y"))
) {
  pm_url <- .pl_param_map("water_level")  # URL builders only
  years  <- seq.int(as.integer(from_year), as.integer(to_year))

  pby <- progress::progress_bar$new(
    total  = length(years),
    format = "PL_IMGW master build [:bar] :current/:total (:percent) Year=:current_year"
  )

  .lim_get <- ratelimitr::limit_rate(
    function(path) .pl_safe_get(x, path),
    rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period)
  )

  parts <- list()

  for (yy in years) {
    pby$tick(tokens = list(current_year = yy))

    # Prefer YEAR ZIP
    year_path <- pm_url$hist_file_year(yy)
    res_y <- .lim_get(year_path)
    if (res_y$status == 200) {
      df_wide <- .pl_fetch_zip_to_df(x, year_path)
      if (nrow(df_wide)) {
        parts[[length(parts)+1L]] <- .pl_chunk_to_wide(df_wide, paste0(x$base_url, year_path))
        next
      }
    } else if (res_y$status >= 400 && res_y$status != 404) {
      rlang::warn(sprintf("PL_IMGW: HTTP %d for %s", res_y$status, year_path))
    }

    # Fallback: MONTH ZIPs
    pbm <- progress::progress_bar$new(total = 12, format = sprintf("  %d monthly files [:bar] :current/:total", yy))
    any_ok <- FALSE
    for (mm in 1:12) {
      pbm$tick()
      month_path <- pm_url$hist_file_month(yy, mm)
      res_m <- .lim_get(month_path)
      if (res_m$status == 200) {
        df_wide_m <- .pl_fetch_zip_to_df(x, month_path)
        if (nrow(df_wide_m)) {
          parts[[length(parts)+1L]] <- .pl_chunk_to_wide(df_wide_m, paste0(x$base_url, month_path))
          any_ok <- TRUE
        }
      } else if (res_m$status >= 400 && res_m$status != 404) {
        rlang::warn(sprintf("PL_IMGW: HTTP %d for %s", res_m$status, month_path))
      }
    }
    if (!any_ok) {
      rlang::inform(sprintf("PL_IMGW: no data found for year %d (no year ZIP and no month ZIPs).", yy))
    }
  }

  master_wide <- suppressWarnings(dplyr::bind_rows(parts))
  if (!nrow(master_wide)) {
    rlang::warn("PL_IMGW: master build produced no rows.")
    return(invisible(NULL))
  }

  # Deduplicate if a date/station appears multiple times (prefer non-NA values)
  master_wide <- master_wide |>
    dplyr::arrange(.data$station_id, .data$timestamp) |>
    dplyr::distinct(.data$station_id, .data$timestamp, .keep_all = TRUE)

  # Save single WIDE master
  .pl_master_wide_save(master_wide, x, cache_dir = cache_dir, compress = "xz")
  invisible(master_wide)
}

.pl_fetch_zip_to_df <- function(x, url_path) {
  res <- .pl_safe_get(x, url_path)
  if (res$status != 200) return(tibble::tibble())
  # temp zip and dir
  zf <- tempfile(fileext = ".zip")
  on.exit(try(unlink(zf), silent = TRUE), add = TRUE)
  writeBin(httr2::resp_body_raw(res$resp), zf)
  exdir <- tempfile("pl_imgw_zip_")
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(try(unlink(exdir, recursive = TRUE, force = TRUE), silent = TRUE), add = TRUE)
  utils::unzip(zf, exdir = exdir, overwrite = TRUE)
  csvs <- list.files(exdir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  if (!length(csvs)) return(tibble::tibble())
  .pl_read_hist_csv(csvs[1])
}

# Project a WIDE master object to DK schema for a single parameter + window + stations
.project_master <- function(master_obj, parameter, st_ids, win_start, win_end, x, restrict = FALSE) {
  wide <- master_obj$data_wide
  meta <- master_obj$meta %||% list(country = x$country, provider_id = x$provider_id, provider_name = x$provider_name)

  # Station filter (only if user actually restricted)
  if (isTRUE(restrict)) {
    wide <- wide[wide$station_id %in% st_ids, , drop = FALSE]
    if (!nrow(wide)) return(.empty_ts_PL_IMGW(x))
  }

  # Time window
  keep <- wide$timestamp >= win_start & wide$timestamp <= win_end
  wide <- wide[keep, , drop = FALSE]
  if (!nrow(wide)) return(.empty_ts_PL_IMGW(x))

  # Pick the value column + unit for requested parameter
  col_map <- list(
    water_level       = list(col = "wl_cm", unit = "cm"),
    water_discharge   = list(col = "q_m3s", unit = "m3/s"),
    water_temperature = list(col = "tw_c",  unit = "°C")
  )
  cm   <- col_map[[parameter]]
  vals <- wide[[cm$col]]

  # Build DK schema
  out <- tibble::tibble(
    country       = meta$country,
    provider_id   = meta$provider_id,
    provider_name = meta$provider_name,
    station_id    = as.character(wide$station_id),
    parameter     = parameter,
    timestamp     = as.POSIXct(wide$timestamp, tz = "UTC"),
    value         = suppressWarnings(as.numeric(vals)),
    unit          = cm$unit,
    quality_code  = NA_character_,
    qf_desc       = NA_character_,
    source_url    = as.character(wide$source_url)
  )

  # Drop all-NA values for this parameter
  out <- out[!is.na(out$value), , drop = FALSE]
  if (!nrow(out)) return(.empty_ts_PL_IMGW(x))

  # Order + dedup (safety)
  out <- out[order(out$station_id, out$timestamp), , drop = FALSE]
  out <- out[!duplicated(out[c("station_id", "timestamp", "parameter")]), , drop = FALSE]
  out
}



#' @export
timeseries.hydro_service_PL_IMGW <- function(x,
                                             parameter = c("water_discharge","water_level","water_temperature"),
                                             stations = NULL,
                                             start_date = NULL, end_date = NULL,
                                             mode = c("complete","range"),
                                             cache_dir = NULL, update = FALSE,
                                             prefer_master = TRUE, save_master = FALSE, ...
) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)

  # ---- Window (force earliest start to 1951-01-01) ----
  rng <- resolve_dates(mode, start_date, end_date)
  rng$start_date <- as.Date("1951-01-01")
  win_start <- as.POSIXct(paste0(format(rng$start_date, "%Y-%m-%d"), " 00:00:00"), tz = "UTC")
  win_end   <- as.POSIXct(paste0(format(rng$end_date,   "%Y-%m-%d"), " 23:59:59"), tz = "UTC")

  # ---- Stations & validation ----
  st_all <- stations.hydro_service_PL_IMGW(x)
  if (!nrow(st_all)) return(.empty_ts_PL_IMGW(x))

  if (is.null(stations) || !length(stations)) {
    st_ids <- unique(as.character(st_all$station_id))
  } else {
    user_ids <- unique(as.character(stations))
    allowed  <- unique(as.character(st_all$station_id))
    invalid  <- setdiff(user_ids, allowed)
    if (length(invalid)) {
      rlang::warn(paste0(
        "PL_IMGW: dropped ", length(invalid), " invalid station id(s). Examples: ",
        paste(utils::head(invalid, 5), collapse = ", "),
        if (length(invalid) > 5) paste0(" … (+", length(invalid) - 5, " more)") else ""
      ))
    }
    st_ids <- intersect(user_ids, allowed)
  }
  if (!length(st_ids)) return(.empty_ts_PL_IMGW(x))
  restrict <- length(stations) > 0L

  # ---- Fast path: use existing WIDE master (unless update=TRUE or prefer_master=FALSE) ----
  if (!update && prefer_master) {
    master <- .pl_master_load(cache_dir)
    if (!is.null(master) && identical(master$type, "wide") && !is.null(master$data_wide)) {
      .pl_master_note(cache_dir)
      return(.project_master(master, parameter, st_ids, win_start, win_end, x, restrict))
    }
  }

  # ---- Build/refresh master from source when:
  #      - update = TRUE, OR
  #      - no master exists (regardless of prefer_master)
  need_build <- isTRUE(update)
  if (!need_build) {
    cur0 <- .pl_master_load(cache_dir)
    need_build <- is.null(cur0)
  }

  if (need_build) {
    y_from <- 1951L
    y_to   <- as.integer(format(win_end, "%Y"))
    rlang::inform(sprintf(
      "PL_IMGW: building WIDE master cache from %d to %d (one-time build; later calls use the cache).",
      y_from, y_to
    ))
    invisible(.pl_build_master_from_source(x, cache_dir = cache_dir, from_year = y_from, to_year = y_to))
  }

  # ---- Use (new or existing) master
  cur <- .pl_master_load(cache_dir)
  if (is.null(cur) || is.null(cur$data_wide)) return(.empty_ts_PL_IMGW(x))
  .pl_master_note(cache_dir)
  .project_master(cur, parameter, st_ids, win_start, win_end, x, restrict)
}


# -----------------------------------------------------------------------------
# Empty TS helper (private)
# -----------------------------------------------------------------------------

.empty_ts_PL_IMGW <- function(x) {
  tibble::tibble(
    country      = x$country %||% "PL",
    provider_id  = x$provider_id %||% "PL_IMGW",
    provider_name= x$provider_name %||% "Poland – IMGW Public Data",
    station_id   = character(),
    parameter    = character(),
    timestamp    = as.POSIXct(character()),
    value        = numeric(),
    unit         = character(),
    quality_code = character(),
    qf_desc      = character(),
    source_url   = character()
  )
}
