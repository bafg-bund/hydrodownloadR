# data-raw/build_pl_imgw_meta.R
# Create data/pl_imgw_meta.rda from data-raw/Metadata_GRDC_30.10.2025.xlsx

library(tibble)
library(dplyr)

xlsx_path <- file.path("data-raw", "Metadata_GRDC_30.10.2025.xlsx")

stopifnot(file.exists(xlsx_path))

# ---- helpers (ASCII-safe) ----
normalize_utf8 <- function(x) {
  x <- enc2utf8(as.character(x))
  x[is.na(x)] <- NA_character_
  x
}

# detect decimal-like numbers (with dot or comma)
is_decimalish <- function(x) {
  x0 <- gsub(",", ".", as.character(x), fixed = TRUE)
  grepl("^\\s*[+-]?[0-9]+(?:\\.[0-9]+)?\\s*$", x0)
}

# Parse DMS like: 18° 17' 14,311" E  (all Unicode escaped)
dms_to_decimal <- function(x, kind = c("lat","lon")) {
  kind <- match.arg(kind)
  if (!length(x)) return(numeric(0))
  parse_one <- function(s) {
    if (is.na(s) || !nzchar(s)) return(NA_real_)
    s0 <- normalize_utf8(s)
    s0 <- gsub(",", ".", s0, fixed = TRUE)
    s0 <- gsub("\\s+", " ", s0)
    s0 <- gsub("[\u00B0\u00BA]", " ", s0)  # ° º
    s0 <- gsub("(deg|Deg|DEG)", " ", s0)
    s0 <- gsub("[\u2032\u2019']", " ", s0) # ′ ’ '
    s0 <- gsub("[\u2033\u201D\"]", " ", s0) # ″ ” "
    s0 <- trimws(s0)

    hemi <- NA_character_
    if (grepl("\\b[Nn]\\b", s0)) hemi <- "N"
    if (grepl("\\b[Ss]\\b", s0)) hemi <- "S"
    if (grepl("\\b[Ee]\\b", s0)) hemi <- "E"
    if (grepl("\\b[Ww]\\b", s0)) hemi <- "W"

    nums <- regmatches(s0, gregexpr("[-+]?[0-9]+(?:\\.[0-9]+)?", s0))[[1]]
    if (!length(nums)) return(NA_real_)
    nums <- suppressWarnings(as.numeric(nums))

    d <- nums[1]; m <- if (length(nums) >= 2) nums[2] else 0; sc <- if (length(nums) >= 3) nums[3] else 0
    sign_val <- if (!is.na(d) && d < 0) -1 else 1
    if (!is.na(hemi)) sign_val <- if (hemi %in% c("S","W")) -1 else 1
    d <- abs(d)
    dec <- d + m/60 + sc/3600
    dec * sign_val
  }
  vapply(x, parse_one, numeric(1))
}

parse_coord_col <- function(x, kind = c("lat","lon")) {
  kind <- match.arg(kind)
  if (is.null(x)) return(rep(NA_real_, length.out = length(x)))
  x_chr <- as.character(x)

  dec_idx <- is_decimalish(x_chr)
  dms_idx <- !dec_idx & (grepl("[\u00B0\u00BA\u2032\u2019'\u2033\u201D\"]", x_chr) | grepl("\\b[NSWE]\\b", x_chr, ignore.case = TRUE))

  out <- rep(NA_real_, length(x_chr))
  if (any(dec_idx)) {
    out[dec_idx] <- suppressWarnings(as.numeric(gsub(",", ".", x_chr[dec_idx], fixed = TRUE)))
  }
  if (any(dms_idx, na.rm = TRUE)) {
    out[dms_idx] <- dms_to_decimal(x_chr[dms_idx], kind = kind)
  }

  if (kind == "lat") out[out < -90 | out > 90] <- NA_real_ else out[out < -180 | out > 180] <- NA_real_
  out
}

# ---- read XLSX (local dev-time only) ----
if (!requireNamespace("readxl", quietly = TRUE)) stop("Install readxl to build data.")
raw <- readxl::read_excel(xlsx_path)
stopifnot(nrow(raw) > 0)

# Column name normalizer
norm_key <- function(s) {
  s <- normalize_utf8(s)
  s <- gsub("\\s+", " ", s)
  s <- tolower(s)
  s <- iconv(s, to = "ASCII//TRANSLIT")
  gsub("[^a-z0-9]+", "", s)
}
nmap <- setNames(seq_along(names(raw)), vapply(names(raw), norm_key, character(1)))
pick <- function(...) {
  keys <- vapply(c(...), norm_key, character(1))
  for (k in keys) {
    idx <- nmap[[k]]
    if (!is.null(idx)) return(raw[[idx]])
  }
  NULL
}

station_id  <- pick("Station code","Kod stacji","ID","objID","gauge_id")
station_nm  <- pick("Station name","Nazwa stacji","STATION_NAME")
river_nm    <- pick("River/Lake","River","STREAM_NAME","Rzeka/Jezioro","Rzeka")
lat_raw     <- pick("Latitude (decimal degree)","Latitude","GEOGR1")
lon_raw     <- pick("Longitude (decimal degree)","Longitude","GEOGR2","Longitude (decimal degree)")
area_raw    <- pick("Catchment area (square kilometre)","PLO_STA","Powierzchnia zlewni [km2]","Catchment area")
alt_raw     <- pick("Height of gauge zero (m above sea level)","H0 [m a.s.l.]","Wysokosc n.p.m.","Height of gauge zero")
vdm_raw     <- pick("Vertical reference system","Vertical datum","Uklad wysokosciowy")

lat <- parse_coord_col(lat_raw, "lat")
lon <- parse_coord_col(lon_raw, "lon")
to_num <- function(v) suppressWarnings(as.numeric(gsub(",", ".", as.character(v), fixed = TRUE)))
area <- if (is.null(area_raw)) rep(NA_real_, length(lat)) else to_num(area_raw)
alt  <- if (is.null(alt_raw))  rep(NA_real_, length(lat)) else to_num(alt_raw)

pl_imgw_meta <- tibble(
  station_id        = as.character(station_id),
  station_name      = normalize_utf8(station_nm),
  river             = normalize_utf8(river_nm),
  lat_md            = lat,
  lon_md            = lon,
  area_md           = area,
  altitude_md       = alt,
  vertical_datum_md = if (!is.null(vdm_raw)) normalize_utf8(vdm_raw) else NA_character_
) |>
  filter(!is.na(station_id) & nzchar(station_id)) |>
  distinct(station_id, .keep_all = TRUE)

stopifnot(nrow(pl_imgw_meta) > 0)

attr(pl_imgw_meta, "source_stamp") <- file.info(xlsx_path)$mtime
attr(pl_imgw_meta, "source_note")  <- "IMGW metadata supplied to GRDC (email)."

# Writes data/pl_imgw_meta.rda with the correct RDA format for packages
usethis::use_data(pl_imgw_meta, overwrite = TRUE, compress = "xz")
