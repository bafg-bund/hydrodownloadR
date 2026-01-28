# data-raw/fr_hubeau_meta_build.R
# Build precomputed FR_HUBEAU metadata (area + altitudes) and save to data/.

# Minimal deps; keep everything local so this script can run even before the pkg is installed
library(tibble)
library(dplyr)
library(jsonlite)

# local helper so we don't rely on package internals at build time
col_or_null <- function(df, name) if (name %in% names(df)) df[[name]] else NULL

# ---------- pull referentials (API v2/v1) ------------------------------------
.fr_get_ref_tables <- function(x, page_size = 10000L) {
  get_paged <- function(path) {
    out <- list(); page <- 1L
    repeat {
      req <- build_request(x, path = path, query = list(format = "json", size = page_size, page = page))
      res <- perform_request(req)
      payload <- .fr_parse_json(res)
      dat <- if (!is.null(payload)) payload[["data"]] else NULL
      if (is.null(dat) || !length(dat)) break
      out[[length(out) + 1L]] <- dat
      page <- page + 1L
      if (is.null(payload[["next"]])) break
    }
    if (!length(out)) return(tibble())
    dplyr::bind_rows(out)
  }

  hm <- get_paged("/api/v2/hydrometrie/referentiel/stations") %>% as_tibble()
  tp <- get_paged("/api/v1/temperature/station")               %>% as_tibble()
  list(hm = hm, tp = tp)
}

# ---------- build station index (normalize altitude units) -------------------
.fr_build_station_index <- function(hm, tp) {
  # hydrometry altitude_ref_alti_station is in mm -> convert to meters
  hm_tbl <- if (nrow(hm)) tibble(
    code_site    = col_or_null(hm, "code_site"),
    station_id   = col_or_null(hm, "code_station"),
    altitude_api = suppressWarnings(as.numeric(col_or_null(hm, "altitude_ref_alti_station"))) / 1000
  ) else tibble()

  # temperature altitude is already meters
  tp_tbl <- if (nrow(tp)) tibble(
    code_site    = col_or_null(tp, "code_site"),
    station_id   = col_or_null(tp, "code_station"),
    altitude_api = suppressWarnings(as.numeric(col_or_null(tp, "altitude")))
  ) else tibble()

  bind_rows(hm_tbl, tp_tbl) %>%
    filter(!is.na(station_id)) %>%
    distinct(station_id, .keep_all = TRUE)
}

# ---------- orchestrate ------------------------------------------------------
build_fr_hubeau_meta <- function(write_excel = TRUE) {
  message("Building FR_HUBEAU metadata (site scrape + station scrape)...")

  s  <- hydro_service("FR_HUBEAU")
  rt <- .fr_get_ref_tables(s)
  idx <- .fr_build_station_index(rt$hm, rt$tp)

  # Site-level (area, site altitude, vertical datum)
  site_meta <- .fr_fetch_site_meta(
    stats::na.omit(idx$code_site),
    rate = list(n = 3, period = 1),
    max_conns = 3, use_cache = TRUE, cache_ttl_days = 90
  )

  # Station-level (gauge-zero altitude)
  st_meta <- .fr_fetch_station_meta(
    stats::na.omit(idx$station_id),
    rate = list(n = 3, period = 1)
  )

  meta <- idx %>%
    left_join(site_meta, by = "code_site") %>%
    left_join(st_meta,   by = "station_id") %>%
    mutate(
      altitude_api      = suppressWarnings(as.numeric(altitude_api)),
      altitude_site     = suppressWarnings(as.numeric(altitude_site)),
      altitude_station  = suppressWarnings(as.numeric(altitude_station)),
      area              = suppressWarnings(as.numeric(area)),
      retrieved_at      = Sys.time()
    )

  # Optional Excel snapshot in data-raw/ (only if writexl available)
  if (isTRUE(write_excel) && requireNamespace("writexl", quietly = TRUE)) {
    dir.create("data-raw", showWarnings = FALSE)
    stamp   <- format(Sys.Date(), "%Y%m%d")
    xlsxpth <- file.path("data-raw", paste0("FR_HUBEAU_meta_", stamp, ".xlsx"))
    writexl::write_xlsx(meta, xlsxpth)
    message("Saved Excel to: ", xlsxpth)
  } else if (isTRUE(write_excel)) {
    message("Skipping Excel export (package 'writexl' not installed).")
  }

  # Package data (RDA)
  fr_hubeau_meta <- meta %>%
    select(
      code_site, station_id, area,
      altitude_api, altitude_site, altitude_station,
      vertical_datum_site,       # may be NA where not found
      retrieved_at
    )

  attr(fr_hubeau_meta, "metadata_date") <- as.character(Sys.Date())

  dir.create("data", showWarnings = FALSE)
  # Use base save to avoid requiring usethis; compress with xz
  save(fr_hubeau_meta, file = "data/fr_hubeau_meta.rda", compress = "xz")

  message("Saved RDA to: data/fr_hubeau_meta.rda (metadata_date = ",
          attr(fr_hubeau_meta, "metadata_date"), ")")

  invisible(fr_hubeau_meta)
}

# NOTE:
# - Do NOT auto-run here. Call build_fr_hubeau_meta() manually when you want to refresh.
# - Ensure you have a data doc stub in R/data-fr_hubeau_meta.R:
#     #' FR_HUBEAU precomputed station metadata
#     #'
#     #' @format A tibble with columns: code_site, station_id, area, altitude_api,
#     #' altitude_site, altitude_station, vertical_datum_site, retrieved_at
#     #' @keywords datasets
#     "fr_hubeau_meta"
