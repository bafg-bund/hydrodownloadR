# ==== Netherlands (Rijkswaterstaat DDAPI20) adapter ==========================
# Discharge-only adapter for the new WaterWebservices (DDAPI20 / WADAR)

# -- Registration -------------------------------------------------------------

#' @keywords internal
#' @noRd
register_NL_RWS <- function() {
  register_service(
    provider_id   = "NL_RWS",
    provider_name = "Rijkswaterstaat DDL",
    country       = "Netherlands",
    base_url      = "https://ddapi20-waterwebservices.rijkswaterstaat.nl",
    geo_base_url  = "https://geo.rijkswaterstaat.nl/services/ogc/hws/DDAPI20/ows",
    rate_cfg      = list(n = 3, period = 1),
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_NL_RWS <- function(x, ...) {
  "water_discharge"
}

# -- Parameter mapping --------------------------------------------------------

.nl_param_map <- function(parameter = c("water_discharge")) {
  parameter <- match.arg(parameter)

  list(
    grootheid = "Q",
    eenheid = "m3/s",
    compartiment = "OW",
    hoedanigheid = NULL,
    proces_type = "meting",
    unit_out = "m3/s",
    compartments_filter = c("Oppervlaktewater")
  )
}

# -- Small helpers ------------------------------------------------------------

compact_list <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

.pick_col <- function(df, candidates) {
  nm <- names(df)
  hit <- candidates[candidates %in% nm][1]
  if (is.na(hit)) return(NULL)
  hit
}

.nl_quality_name <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x %in% c("Gecontroleerd", "Geverifieerd") ~ "validated",
    x %in% c("Ongecontroleerd") ~ "raw/unvalidated",
    x %in% c("Voorlopig") ~ "provisional",
    TRUE ~ x
  )
}

# -- Station filter by compartment -------------------------------------------

.nl_filter_stations_by_compartment <- function(x, allowed_compartments, preselected_ids = NULL) {
  CATALOG_PATH <- "/METADATASERVICES/OphalenCatalogus"

  body <- list(
    CatalogusFilter = list(
      Compartimenten = TRUE,
      Grootheden     = TRUE,
      Parameters     = TRUE,
      Eenheden       = TRUE,
      Hoedanigheden  = TRUE,
      Groeperingen   = FALSE
    )
  )

  req <- build_request(x, path = CATALOG_PATH) |>
    httr2::req_method("POST") |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_body_json(body)

  resp <- perform_request(req)
  dat  <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  meta <- tibble::as_tibble(dat$AquoMetadataLijst %||% list())
  locs <- tibble::as_tibble(dat$LocatieLijst %||% list())
  xref <- tibble::as_tibble(dat$AquoMetadataLocatieLijst %||% list())

  if (!nrow(meta) || !nrow(locs) || !nrow(xref)) return(character())

  meta_key <- .pick_col(meta, c(
    "AquoMetadata_MessageID", "AquoMetadataMessageID", "AquoMetadataId",
    "AquoMetadata_ID", "MessageID"
  ))

  xref_meta_key <- .pick_col(xref, c(
    "AquoMetaData_MessageID", "AquoMetadataMessageID", "AquoMetadataId", "AquoMetadata_ID"
  ))

  xref_loc_key <- .pick_col(xref, c(
    "Locatie_MessageID", "LocatieMessageID", "LocatieId", "Locatie_ID", "MessageID_Locatie"
  ))

  locs_key <- .pick_col(locs, c(
    "MessageID", "MessageId", "Locatie_MessageID", "LocatieMessageID", "LocatieId"
  ))

  code_col <- .pick_col(locs, c("Code", "LocatieCode", "StationCode"))

  if (is.null(meta_key) || is.null(xref_meta_key) || is.null(xref_loc_key) ||
      is.null(locs_key) || is.null(code_col)) {
    return(character())
  }

  comp_df <- meta$Compartiment
  comp_desc <- if (is.data.frame(comp_df) && "Omschrijving" %in% names(comp_df)) {
    comp_df[["Omschrijving"]]
  } else {
    NULL
  }

  comp_ok <- !is.null(comp_desc) & comp_desc %in% allowed_compartments
  if (!any(comp_ok, na.rm = TRUE)) return(character())

  meta_ok <- meta[comp_ok, , drop = FALSE]

  xref_ok <- dplyr::semi_join(
    xref,
    tibble::tibble(join_id = meta_ok[[meta_key]]),
    by = stats::setNames("join_id", xref_meta_key)
  )

  if (!nrow(xref_ok)) return(character())

  stations_ok <- dplyr::inner_join(
    tibble::tibble(join_loc = xref_ok[[xref_loc_key]]),
    tibble::tibble(join_loc = locs[[locs_key]], Code = locs[[code_col]]),
    by = "join_loc"
  )$Code

  stations_ok <- unique(as.character(stats::na.omit(stations_ok)))

  if (!is.null(preselected_ids)) {
    stations_ok <- intersect(stations_ok, as.character(preselected_ids))
  }

  stations_ok
}

# -- Stations (S3) ------------------------------------------------------------

#' @export
stations.hydro_service_NL_RWS <- function(x, ...) {
  CATALOG_PATH <- "/METADATASERVICES/OphalenCatalogus"

  limited <- ratelimitr::limit_rate(function() {
    body <- list(
      CatalogusFilter = list(
        Compartimenten = TRUE,
        Grootheden     = TRUE,
        Parameters     = TRUE,
        Eenheden       = TRUE,
        Hoedanigheden  = TRUE,
        Groeperingen   = FALSE
      )
    )

    req <- build_request(x, path = CATALOG_PATH) |>
      httr2::req_method("POST") |>
      httr2::req_headers(`Content-Type` = "application/json") |>
      httr2::req_body_json(body)

    resp <- perform_request(req)
    dat  <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    locs <- tibble::as_tibble(dat$LocatieLijst %||% list())
    if (!nrow(locs)) {
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

    code_col <- .pick_col(locs, c("Code", "LocatieCode", "StationCode"))
    name_col <- .pick_col(locs, c("Naam", "Name", "Omschrijving"))
    x_col    <- .pick_col(locs, c("X", "Lon", "Longitude"))
    y_col    <- .pick_col(locs, c("Y", "Lat", "Latitude"))

    code <- if (!is.null(code_col)) as.character(locs[[code_col]]) else rep(NA_character_, nrow(locs))
    name <- if (!is.null(name_col)) normalize_utf8(locs[[name_col]]) else rep(NA_character_, nrow(locs))

    # DDAPI20 docs indicate ETRS89 coordinates; use directly here
    lon <- if (!is.null(x_col)) suppressWarnings(as.numeric(locs[[x_col]])) else rep(NA_real_, nrow(locs))
    lat <- if (!is.null(y_col)) suppressWarnings(as.numeric(locs[[y_col]])) else rep(NA_real_, nrow(locs))

    tibble::tibble(
      country            = x$country,
      provider_id        = x$provider_id,
      provider_name      = x$provider_name,
      station_id         = code,
      station_name       = as.character(name),
      station_name_ascii = to_ascii(name),
      river              = NA_character_,
      river_ascii        = NA_character_,
      lat                = lat,
      lon                = lon,
      area               = NA_real_,
      altitude           = NA_real_
    )
  }, rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period))

  limited()
}

# -- Time series (S3) ---------------------------------------------------------

#' @export
timeseries.hydro_service_NL_RWS <- function(x,
                                            parameter = c("water_discharge"),
                                            stations = NULL,
                                            start_date = NULL,
                                            end_date = NULL,
                                            mode = c("complete", "range"),
                                            exclude_quality = NULL,
                                            ...) {
  DATA_PATH <- "/ONLINEWAARNEMINGENSERVICES/OphalenWaarnemingen"
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)
  rng       <- resolve_dates(mode, start_date, end_date)
  pm        <- .nl_param_map(parameter)

  station_vec <- .nl_filter_stations_by_compartment(
    x,
    allowed_compartments = pm$compartments_filter,
    preselected_ids      = stations
  )

  if (!length(station_vec)) {
    return(tibble::tibble(
      country       = character(),
      provider_id   = character(),
      provider_name = character(),
      station_id    = character(),
      parameter     = character(),
      timestamp     = as.Date(character()),
      value         = numeric(),
      unit          = character(),
      quality_code  = character(),
      quality_name  = character(),
      quality_desc  = character(),
      source_url    = character()
    ))
  }

  one_station <- ratelimitr::limit_rate(function(st_id) {
    body <- list(
      Locatie = list(Code = st_id),
      AquoPlusWaarnemingMetadata = list(
        AquoMetadata = compact_list(list(
          Compartiment = list(Code = pm$compartiment),
          Grootheid    = list(Code = pm$grootheid),
          Eenheid      = if (!is.null(pm$eenheid)) list(Code = pm$eenheid) else NULL,
          Hoedanigheid = if (!is.null(pm$hoedanigheid)) list(Code = pm$hoedanigheid) else NULL,
          ProcesType   = pm$proces_type
        ))
      ),
      Periode = list(
        Begindatumtijd = paste0(format(rng$start_date, "%Y-%m-%d"), "T00:00:00.000+01:00"),
        Einddatumtijd  = paste0(format(rng$end_date,   "%Y-%m-%d"), "T23:59:59.999+01:00")
      )
    )

    req <- build_request(x, path = DATA_PATH) |>
      httr2::req_method("POST") |>
      httr2::req_headers(`Content-Type` = "application/json") |>
      httr2::req_body_json(body)

    resp <- try(perform_request(req), silent = TRUE)
    if (inherits(resp, "try-error")) return(tibble::tibble())

    dat <- try(httr2::resp_body_json(resp, simplifyVector = TRUE), silent = TRUE)
    if (inherits(dat, "try-error") || is.null(dat$WaarnemingenLijst)) {
      return(tibble::tibble())
    }

    # Known issue: same metadata can be split across multiple MetingenLijsten
    wl_raw <- dat$WaarnemingenLijst

    wl_list <- if (is.list(wl_raw) && !is.null(wl_raw$MetingenLijst)) {
      list(wl_raw)
    } else if (is.list(wl_raw)) {
      wl_raw
    } else {
      list()
    }

    if (!length(wl_list)) return(tibble::tibble())

    dfs <- lapply(wl_list, function(w) {
      ml <- w$MetingenLijst
      if (is.null(ml) || !length(ml)) return(tibble::tibble())

      sub_dfs <- lapply(ml, function(m) {
        df <- tibble::as_tibble(m)

        ts <- df$Tijdstip %||% df$Tijdstempel %||% df$Datumtijd
        ts <- tryCatch(as.Date(ts), error = function(e) as.Date(NA))

        val <- tryCatch(df$Meetwaarde$Waarde_Numeriek, error = function(e) NULL)
        val <- suppressWarnings(as.numeric(val))
        qf_num <- tryCatch(df$WaarnemingMetadata$Kwaliteitswaardecode, error = function(e) NULL)
        quality_code <- if (is.null(qf_num)) NA_character_ else as.character(qf_num)

        qf <- tryCatch(df$WaarnemingMetadata$Statuswaarde, error = function(e) NULL)
        quality_name <- if (is.null(qf)) NA_character_ else as.character(qf)

        out <- tibble::tibble(
          country       = x$country,
          provider_id   = x$provider_id,
          provider_name = x$provider_name,
          station_id    = st_id,
          parameter     = parameter,
          timestamp     = ts,
          value         = val,
          unit          = pm$unit_out,
          quality_code  = quality_code,
          quality_name  = quality_name,
          quality_desc  = .nl_quality_name(quality_name),
          source_url    = paste0(x$base_url, DATA_PATH)
        )

        out <- dplyr::filter(
          out,
          !is.na(.data$timestamp),
          .data$timestamp >= rng$start_date,
          .data$timestamp <= rng$end_date
        )

        if (!is.null(exclude_quality)) {
          out <- dplyr::filter(out, !.data$quality_code %in% exclude_quality)
        }

        out
      })

      dplyr::bind_rows(sub_dfs)
    })

    out <- dplyr::bind_rows(dfs)
    if (!nrow(out)) return(out)

    out |>
      dplyr::arrange(.data$timestamp) |>
      dplyr::distinct(.data$station_id, .data$timestamp, .keep_all = TRUE)
  }, rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period))

  pb <- progress::progress_bar$new(total = length(station_vec))
  res <- lapply(station_vec, function(id) {
    pb$tick()
    one_station(id)
  })

  dplyr::bind_rows(res)
}
