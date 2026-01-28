# ==== Netherlands (Rijkswaterstaat - DDL) adapter ============================
# Base docs: https://rijkswaterstaatdata.nl/waterdata/ (how-to + examples)
# Endpoints:
#   - Catalog:  /METADATASERVICES_DBO/OphalenCatalogus
#   - Data:     /ONLINEWAARNEMINGENSERVICES_DBO/OphalenWaarnemingen
#
# Notes:
# - Typical interval is 10-min; server caps responses at ~160,000 rows we chunk by month.
# - Coordinates come as X/Y with "Coordinatenstelsel" (often EPSG:25831 ETRS89/UTM31N), convert to 4326.
# - AQUO codes used in practice:
#     * Water level:       Grootheid.Code = "WATHTE" (unit cm, datum often NAP)
#     * Discharge (debit): Grootheid.Code = "Q"      (unit m^3/s)   # adjust if RWS changes
#     * Temperature:       Grootheid.Code = "T"      (unit C)
# - Add optional header X-API-KEY if RWS introduces keys (beta mention).

# -- Registration -------------------------------------------------------------

#' @keywords internal
#' @noRd
register_NL_RWS <- function() {
  register_service_usage(
    provider_id   = "NL_RWS",
    provider_name = "Rijkswaterstaat DDL",
    country       = "Netherlands",
    base_url      = "https://waterwebservices.rijkswaterstaat.nl",
    geo_base_url  = NULL,
    rate_cfg      = list(n = 3, period = 1),
    auth          = list(type = "none")  # optionally support X-API-KEY later
  )
}

#' @export
timeseries_parameters.hydro_service_NL_RWS <- function(x, ...) {
  c("water_discharge")
}

# -- Parameter mapping --------------------------------------------------------

.nl_param_map <- function(parameter = c("water_discharge")) {
  parameter <- match.arg(parameter)
  # Only discharge supported
  list(
    grootheid = "Q",
    eenheid   = "m^3/s",
    compartiment = "OW",
    hoedanigheid = NULL,
    unit_out  = "m^3/s",
    # Station pre-filter: only surface water
    compartments_filter = c("Oppervlaktewater")
  )
}



# -- Stations (S3) ------------------------------------------------------------

#' @export
stations.hydro_service_NL_RWS <- function(x, ...) {
  CATALOG_PATH <- "/METADATASERVICES_DBO/OphalenCatalogus"

  limited <- ratelimitr::limit_rate(function() {
    # Request minimal but sufficient catalog (Compartimenten + Grootheden)
    body <- list(CatalogusFilter = list(
      Compartimenten = TRUE, Grootheden = TRUE, Parameters = TRUE, Eenheden = TRUE,
      Hoedanigheden = TRUE, Groeperingen = FALSE
    ))

    req <- build_request(x, path = CATALOG_PATH) |>
      httr2::req_method("POST") |>
      httr2::req_headers(`Content-Type` = "application/json") |>
      httr2::req_body_json(body)

    resp <- perform_request(req)
    dat  <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    # The catalog returns tables: AquoMetadataLijst, LocatieLijst, AquoMetadataLocatieLijst, ...
    meta   <- tibble::as_tibble(dat$AquoMetadataLijst %||% list())
    locs   <- tibble::as_tibble(dat$LocatieLijst       %||% list())
    cross  <- tibble::as_tibble(dat$AquoMetadataLocatieLijst %||% list())

    # Basic fields
    # Expect: code (station code), naam, x, y, coordinatenstelsel, etc.
    code <- locs$Code %||% locs$locatie_message_id
    name <- normalize_utf8(locs$Naam %||% NA_character_)

    # Coordinate transform
    n <- nrow(locs)
    lon <- rep(NA_real_, n); lat <- rep(NA_real_, n)
    if (n && requireNamespace("sf", quietly = TRUE)) {
      epsg <- suppressWarnings(as.integer(locs$Coordinatenstelsel))
      epsg[is.na(epsg)] <- 25831L
      ok <- is.finite(locs$X) & is.finite(locs$Y) & !is.na(epsg)
      for (crs in unique(epsg[ok])) {
        idx <- which(ok & epsg == crs)
        pts <- sf::st_as_sf(data.frame(x = locs$X[idx], y = locs$Y[idx]),
                            coords = c("x","y"), crs = crs)
        pts_wgs <- sf::st_transform(pts, 4326)
        xy <- sf::st_coordinates(pts_wgs)
        lon[idx] <- xy[,1]; lat[idx] <- xy[,2]
      }
    } else if (n) {
      rlang::warn("Package 'sf' not installed; NL_RWS stations will have NA lon/lat.")
    }

    tibble::tibble(
      country            = x$country,
      provider_id        = x$provider_id,
      provider_name      = x$provider_name,
      station_id         = as.character(code),
      station_name       = as.character(name),
      station_name_ascii = to_ascii(name),
      river              = NA_character_,
      river_ascii        = NA_character_,
      lat                = lat,
      lon                = lon,
      area = NA_real_, altitude = NA_real_
    )
  }, rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period))

  limited()
}



# -- Time series (S3) ---------------------------------------------------------

# robust column picker: first present match
.pick_col <- function(df, candidates) {
  nm <- names(df)
  hit <- candidates[candidates %in% nm][1]
  if (is.na(hit)) return(NULL)
  hit
}

.nl_filter_stations_by_compartment <- function(x, allowed_compartments, preselected_ids = NULL) {
  CATALOG_PATH <- "/METADATASERVICES_DBO/OphalenCatalogus"
  body <- list(CatalogusFilter = list(
    Compartimenten = TRUE, Grootheden = TRUE, Parameters = TRUE, Eenheden = TRUE,
    Hoedanigheden = TRUE, Groeperingen = FALSE
  ))

  req <- build_request(x, path = CATALOG_PATH) |>
    httr2::req_method("POST") |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_body_json(body)

  resp <- perform_request(req)
  dat  <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  meta <- tibble::as_tibble(dat$AquoMetadataLijst %||% list())
  locs <- tibble::as_tibble(dat$LocatieLijst       %||% list())
  xref <- tibble::as_tibble(dat$AquoMetadataLocatieLijst %||% list())

  if (!nrow(meta) || !nrow(locs) || !nrow(xref)) return(character())

  # --- detect key columns per table -----------------------------------------
  # meta key
  meta_key <- .pick_col(meta, c("AquoMetadata_MessageID","AquoMetadataMessageID","AquoMetadataId","AquoMetadata_ID","MessageID"))
  if (is.null(meta_key)) {
    rlang::warn("NL_RWS: could not detect AquoMetadata key column in meta; available: {paste(names(meta), collapse=', ')}")
    return(character())
  }
  # xref keys (link from AquoMetadata -> Locatie)
  xref_meta_key <- .pick_col(xref, c("AquoMetaData_MessageID","AquoMetadataMessageID","AquoMetadataId","AquoMetadata_ID"))
  xref_loc_key  <- .pick_col(xref, c("Locatie_MessageID","LocatieMessageID","LocatieId","Locatie_ID","MessageID_Locatie"))
  if (is.null(xref_meta_key) || is.null(xref_loc_key)) {
    rlang::warn("NL_RWS: could not detect expected key columns in AquoMetadataLocatieLijst.")
    return(character())
  }
  # locs keys (station id)
  locs_key  <- .pick_col(locs, c("MessageID","MessageId","Locatie_MessageID","LocatieMessageID","LocatieId"))
  code_col  <- .pick_col(locs, c("Code","LocatieCode","StationCode"))
  if (is.null(locs_key) || is.null(code_col)) {
    rlang::warn("NL_RWS: could not detect location key/Code in LocatieLijst.")
    return(character())
  }

  # --- filter meta by allowed compartments ----------------------------------
  # meta$Compartiment is a df-column; pull Omschrijving safely
  comp_df <- meta$Compartiment
  comp_desc <- if (is.data.frame(comp_df) && "Omschrijving" %in% names(comp_df)) comp_df[["Omschrijving"]] else NULL
  comp_ok <- !is.null(comp_desc) & comp_desc %in% allowed_compartments
  if (!any(comp_ok, na.rm = TRUE)) return(character())
  meta_ok <- meta[comp_ok, , drop = FALSE]

  # --- join chain: meta -> xref -> locs -------------------------------------
  xref_ok <- dplyr::semi_join(
    xref,
    tibble::tibble(!!xref_meta_key := meta_ok[[meta_key]]),
    by = setNames(xref_meta_key, xref_meta_key)
  )
  if (!nrow(xref_ok)) return(character())

  stations_ok <- dplyr::inner_join(
    tibble::tibble(!!locs_key := xref_ok[[xref_loc_key]]),
    tibble::tibble(!!locs_key := locs[[locs_key]], Code = locs[[code_col]]),
    by = setNames(locs_key, locs_key)
  )$Code

  stations_ok <- unique(as.character(stats::na.omit(stations_ok)))
  if (!is.null(preselected_ids)) {
    stations_ok <- intersect(stations_ok, as.character(preselected_ids))
  }
  stations_ok
}

.nl_quality_desc <- function(x) {
  # Map common RWS status labels to concise English
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x %in% c("Gecontroleerd", "Geverifieerd") ~ "validated",
    x %in% c("Ongecontroleerd")               ~ "unvalidated",
    x %in% c("Voorlopig")                     ~ "provisional",
    TRUE ~ x  # fallback: pass through original text
  )
}


#' @export
timeseries.hydro_service_NL_RWS <- function(x,
                                            parameter = c("water_discharge"),
                                            stations = NULL,
                                            start_date = NULL, end_date = NULL,
                                            mode = c("complete","range"),
                                            exclude_quality = NULL,
                                            ...) {
  DATA_PATH <- "/ONLINEWAARNEMINGENSERVICES_DBO/OphalenWaarnemingen"
  parameter <- match.arg(parameter)  # will be "water_discharge"
  rng       <- resolve_dates(mode, start_date, end_date)
  pm        <- .nl_param_map(parameter)

  # --- station ids filtered to Compartiment = "Oppervlaktewater" ------------
  station_vec <- .nl_filter_stations_by_compartment(
    x,
    allowed_compartments = pm$compartments_filter,  # "Oppervlaktewater"
    preselected_ids      = stations
  )
  if (!length(station_vec)) return(tibble::tibble())

  # fetch X/Y once (optional)
  catalog_xy <- local({
    CATALOG_PATH <- "/METADATASERVICES_DBO/OphalenCatalogus"
    body <- list(CatalogusFilter = list(Compartimenten = TRUE, Grootheden = TRUE))
    req  <- build_request(x, path = CATALOG_PATH) |>
      httr2::req_method("POST") |>
      httr2::req_headers(`Content-Type` = "application/json") |>
      httr2::req_body_json(body)
    resp <- perform_request(req)
    dat  <- httr2::resp_body_json(resp, simplifyVector = TRUE)
    locs <- tibble::as_tibble(dat$LocatieLijst %||% list())
    tibble::as_tibble(locs)[, intersect(c("Code","X","Y","Coordinatenstelsel"), names(locs))]
  })

  one_station <- ratelimitr::limit_rate(function(st_id) {
    row <- suppressMessages(dplyr::filter(catalog_xy, .data$Code == st_id))
    loc <- list(Code = st_id,
                X = suppressWarnings(as.numeric(row$X[1] %||% NA)),
                Y = suppressWarnings(as.numeric(row$Y[1] %||% NA)))

    aquo <- list(AquoPlusWaarnemingMetadata = list(
      AquoMetadata = compact_list(list(
        Compartiment = list(Code = pm$compartiment),  # "OW"
        Grootheid    = list(Code = pm$grootheid),     # "Q"
        Eenheid      = if (!is.null(pm$eenheid)) list(Code = pm$eenheid) else NULL,
        Hoedanigheid = if (!is.null(pm$hoedanigheid)) list(Code = pm$hoedanigheid) else NULL
      ))
    ))

    body <- c(
      list(Locatie = loc),
      aquo,
      list(Periode = list(
        Begindatumtijd = paste0(format(rng$start_date, "%Y-%m-%d"), "T00:00:00.000+00:00"),
        Einddatumtijd  = paste0(format(rng$end_date,   "%Y-%m-%d"), "T23:59:59.999+00:00")
      ))
    )

    req <- build_request(x, path = DATA_PATH) |>
      httr2::req_method("POST") |>
      httr2::req_headers(`Content-Type` = "application/json") |>
      httr2::req_body_json(body)

    resp <- try(perform_request(req), silent = TRUE)
    if (inherits(resp, "try-error")) return(tibble::tibble())
    dat <- try(httr2::resp_body_json(resp, simplifyVector = TRUE), silent = TRUE)
    if (inherits(dat, "try-error") || is.null(dat$WaarnemingenLijst)) return(tibble::tibble())

    wl <- dat$WaarnemingenLijst$MetingenLijst
    if (is.null(wl) || !length(wl)) return(tibble::tibble())

    dfs <- lapply(wl, function(w) {
      df <- tibble::as_tibble(w)

      ts  <- df$Tijdstip %||% df$Tijdstempel %||% df$Datumtijd
      val <- tryCatch(df$Meetwaarde$Waarde_Numeriek, error = function(e) NULL)
      qf  <- tryCatch(df$WaarnemingMetadata$StatuswaardeLijst, error = function(e) NULL)

      ts  <- tryCatch(as.Date(ts), error = function(e) as.Date(NA))
      val <- suppressWarnings(as.numeric(val))
      quality_code <- if (is.null(qf)) NA_character_ else as.character(qf)

      tibble::tibble(
        country       = x$country,
        provider_id   = x$provider_id,
        provider_name = x$provider_name,
        station_id    = st_id,
        parameter     = parameter,     # always "water_discharge"
        timestamp     = ts,
        value         = val,
        unit          = pm$unit_out,   # "m3/s"
        quality_code  = quality_code,
        quality_desc  = .nl_quality_desc(quality_code),
        source_url    = paste0(x$base_url, DATA_PATH)
      )
    })

    out <- dplyr::bind_rows(dfs)
    if (!nrow(out)) return(out)

    # filter by date range (timestamp is Date)
    out <- dplyr::filter(out, .data$timestamp >= rng$start_date,
                         .data$timestamp <= rng$end_date)

    if (!is.null(exclude_quality) && "quality_code" %in% names(out)) {
      out <- dplyr::filter(out, !.data$quality_code %in% exclude_quality)
    }

    out <- dplyr::arrange(out, .data$timestamp)
    dplyr::distinct(out, .data$station_id, .data$timestamp, .keep_all = TRUE)
  }, rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period))

  pb  <- progress::progress_bar$new(total = length(station_vec))
  res <- lapply(station_vec, function(id) { pb$tick(); one_station(id) })
  dplyr::bind_rows(res)
}




# --- small helpers -----------------------------------------------------------

compact_list <- function(x) { x[!vapply(x, is.null, logical(1))] }
