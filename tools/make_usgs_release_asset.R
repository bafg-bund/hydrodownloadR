#!/usr/bin/env Rscript

# tools/make_usgs_release_asset_usgs_meta.R
# Build compact USGS station metadata bundle for hydrodownloadRdata release asset.

suppressPackageStartupMessages({
  if (!requireNamespace("dataRetrieval", quietly = TRUE)) stop("Install 'dataRetrieval' first.")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Install 'tibble' first.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install 'dplyr' first.")
  if (!requireNamespace("sf", quietly = TRUE)) stop("Install 'sf' first.")
})

# --- Optional API key passthrough (same env/option as adapter)
.usgs_pat <- function() {
  tok <- getOption("API_USGS_PAT", Sys.getenv("API_USGS_PAT", ""))
  if (!nzchar(tok)) return(NULL)
  tok
}

.usgs_with_key <- function(expr) {
  tok <- .usgs_pat()
  if (is.null(tok)) return(force(expr))
  if (!requireNamespace("httr", quietly = TRUE)) return(force(expr))
  httr::with_config(
    httr::add_headers("X-Api-Key" = tok, "X-API-Key" = tok),
    force(expr)
  )
}

save_rds_atomic <- function(obj, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  saveRDS(obj, tmp)
  if (file.exists(path)) unlink(path)
  ok <- file.rename(tmp, path)
  if (!ok) file.copy(tmp, path, overwrite = TRUE)
  invisible(TRUE)
}

has_monitoring_location <- function() {
  tryCatch(
    exists("read_waterdata_monitoring_location", where = asNamespace("dataRetrieval"), inherits = FALSE),
    error = function(e) FALSE
  )
}

state_table <- function() {
  tbl <- tryCatch(get("stateCd", envir = asNamespace("dataRetrieval")), error = function(e) NULL)
  if (is.null(tbl)) tbl <- tryCatch(dataRetrieval::stateCd, error = function(e) NULL)
  if (is.null(tbl)) stop("Could not access dataRetrieval::stateCd")

  # datasets::state.abb is a dataset
  tmp <- new.env(parent = emptyenv())
  utils::data(list = "state.abb", package = "datasets", envir = tmp)

  allowed <- c(tmp$state.abb, "DC", "GU", "MP", "PR", "VI")
  tbl <- tbl[!is.na(tbl$STUSAB) & tbl$STUSAB %in% allowed, , drop = FALSE]
  tbl <- tbl[!is.na(tbl$STATE), , drop = FALSE]
  tbl <- tbl[order(as.integer(tbl$STATE)), , drop = FALSE]
  tbl
}

request_state <- function(state_code_numeric, limit_per_page = 10000, sleep_between = 0.6) {
  st_code <- sprintf("%02d", as.integer(state_code_numeric))
  props <- c(
    "monitoring_location_number",
    "monitoring_location_name",
    "state_name",
    "drainage_area",
    "altitude"
  )

  one <- try(
    .usgs_with_key({
      dataRetrieval::read_waterdata_monitoring_location(
        state_code  = st_code,
        agency_code = "USGS",
        properties  = props,
        limit       = limit_per_page
      )
    }),
    silent = TRUE
  )

  if (!inherits(one, "try-error") && !is.null(one)) Sys.sleep(sleep_between)
  if (inherits(one, "try-error")) NULL else one
}

bind_dedupe <- function(accum, extra) {
  if (is.null(accum)) return(extra)
  out <- dplyr::bind_rows(accum, extra)
  dplyr::distinct(out, .data$station_id, .keep_all = TRUE)
}

build_usgs_bundle <- function(out_rds,
                              max_passes    = 3,
                              fail_wait     = 300,
                              pass_cooldown = 900,
                              limit_per_page = 10000,
                              sleep_between = 0.6) {
  if (!has_monitoring_location()) {
    stop("Your 'dataRetrieval' is missing read_waterdata_monitoring_location(). Update the package.")
  }

  out_dir <- dirname(out_rds)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  partial_path <- sub("\\.rds$", "_partial.rds", out_rds)

  st_tbl <- state_table()
  all_states <- as.character(st_tbl$STATE) # keep leading zeros

  # Resume support
  accum <- NULL
  done_states <- character(0)

  if (file.exists(partial_path)) {
    p <- tryCatch(readRDS(partial_path), error = function(e) NULL)
    if (is.list(p) && !is.null(p$data)) {
      accum <- p$data
      done_states <- as.character(p$done_states %||% character(0))
      message("Resuming from partial checkpoint: ", partial_path)
      message("Already completed states: ", length(done_states))
    }
  }

  pending <- setdiff(all_states, done_states)

  message("Building USGS monitoring-location index over ", length(all_states), " states/territories.")
  message("Pending states: ", length(pending))
  message("Output RDS: ", out_rds)

  MI2_TO_KM2 <- 2.58999

  for (pass in seq_len(max_passes)) {
    if (!length(pending)) break

    if (pass > 1) {
      message("Cooldown between passes: ", pass_cooldown, " seconds")
      Sys.sleep(pass_cooldown)
    }

    message("Pass ", pass, "/", max_passes, " over ", length(pending), " pending states...")

    next_pending <- character(0)

    for (st_code in pending) {
      st_name <- st_tbl$STATE_NAME[match(st_code, st_tbl$STATE)]
      message("Requesting state ", st_code, " (", st_name, ")")

      one <- request_state(st_code, limit_per_page = limit_per_page, sleep_between = sleep_between)

      if (is.null(one) || !inherits(one, "sf") || nrow(one) == 0) {
        message("  -> failed/empty. Waiting ", fail_wait, "s and queueing for later.")
        Sys.sleep(fail_wait)
        next_pending <- c(next_pending, st_code)
        next
      }

      coords <- sf::st_coordinates(one)

      st_df <- tibble::tibble(
        station_id   = as.character(one$monitoring_location_number),
        station_name = as.character(one$monitoring_location_name),
        lon          = suppressWarnings(as.numeric(coords[, 1])),
        lat          = suppressWarnings(as.numeric(coords[, 2])),
        area         = suppressWarnings(as.numeric(one$drainage_area)) * MI2_TO_KM2,
        .state_code  = as.character(st_code)
      )

      # basic cleanup
      st_df <- dplyr::filter(
        st_df,
        !is.na(.data$station_id),
        nzchar(.data$station_id),
        !is.na(.data$lat),
        !is.na(.data$lon)
      )

      accum <- bind_dedupe(accum, st_df)
      done_states <- unique(c(done_states, st_code))

      # checkpoint after each successful state
      save_rds_atomic(
        list(data = accum, done_states = done_states, updated = Sys.time()),
        partial_path
      )

      message("  -> ok. Total stations so far: ", nrow(accum))
      Sys.sleep(sleep_between)
    }

    pending <- unique(next_pending)
    message("End of pass ", pass, ". Remaining pending states: ", length(pending))
  }

  if (is.null(accum) || !nrow(accum)) stop("No data collected. Aborting.")

  # Final compact bundle: only the requested columns
  final <- accum |>
    dplyr::distinct(.data$station_id, .keep_all = TRUE) |>
    dplyr::transmute(
      station_id,
      station_name,
      lat,
      lon,
      area
    )

  attr(final, "source_date") <- as.Date(Sys.Date())
  attr(final, "source") <- "USGS waterdata monitoring locations by state via dataRetrieval"
  attr(final, "dataRetrieval_version") <- as.character(utils::packageVersion("dataRetrieval"))
  attr(final, "n_stations") <- nrow(final)

  # Write final output (xz-compressed RDS; still a .rds file)
  saveRDS(final, out_rds, compress = "xz")
  message("Wrote final RDS: ", out_rds, " (n=", nrow(final), ")")

  invisible(final)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- CLI / defaults (no private paths)
default_out <- file.path("release-assets", "us_usgs_stations_meta.rds")

args <- commandArgs(trailingOnly = TRUE)
out_rds <- if (length(args) >= 1 && nzchar(args[1])) args[1] else default_out

build_usgs_bundle(out_rds = out_rds)
