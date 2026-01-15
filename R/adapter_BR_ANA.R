# R/adapter_BR_ANA.R
# Brazil – ANA (HidroWeb) adapter
# Auth via SNIRH header-based endpoint; downloads via hidroweb REST.
# Token is supplied as Bearer to the download endpoints.
#
# Requires: {httr2}, {cli}, {utils}, {tibble}, {dplyr}, {tidyr}, {jsonlite}, {lubridate}, {odbc}, {DBI}, {readr}, {rappdirs}
# Also uses your package helpers: build_request(), resolve_dates(), to_ascii()

# -----------------------------------------------------------------------------
# Registration
# -----------------------------------------------------------------------------

#' @export
register_BR_ANA <- function() {
  register_service(
    provider_id   = "BR_ANA",
    provider_name = "Brazil – ANA HidroWeb",
    country       = "BR",
    base_url      = "https://www.ana.gov.br/hidrowebservice",
    rate_cfg      = list(n = 3, period = 1),
    auth          = list(
      type   = "bearer",
      env_id = "ANA_IDENTIFICADOR",
      env_pw = "ANA_SENHA"
    )
  )
}

#' @export
timeseries_parameters.hydro_service_BR_ANA <- function(x, ...) {
  c("water_discharge","water_level")
}

# ---- parameter → endpoint map ----------------------------------------------
.br_param_map <- function(parameter) {
  switch(parameter,
         water_discharge = list(
           path       = "EstacoesTelemetricas/HidroSerieVazao/v1",
           unit       = "m^3/s",
           value_keys = c("Media","media","Valor","valor")
         ),
         water_level = list(
           path       = "EstacoesTelemetricas/HidroSerieCotas/v1",
           unit       = "cm",  # keep as cm (no conversion)
           value_keys = c("Media","media","Valor","valor")
         ),
         stop("Unknown parameter for BR_ANA: ", parameter)
  )
}

# ---- credentials & token ----------------------------------------------------
.br_ana_creds <- function(...) {
  dots <- list(...)
  id   <- dots$identificador %||% getOption("ANA_IDENTIFICADOR", NULL) %||% Sys.getenv("ANA_IDENTIFICADOR", unset = "")
  pw   <- dots$senha          %||% getOption("ANA_SENHA",         NULL) %||% Sys.getenv("ANA_SENHA",         unset = "")
  id <- trimws(id %||% ""); pw <- trimws(pw %||% "")
  list(id = id, pw = pw)
}

.br_ana_state <- new.env(parent = emptyenv())

.br_ana_get_token <- function(x, force = FALSE, ...) {
  if (!force && !is.null(.br_ana_state$token) && nzchar(.br_ana_state$token)) {
    return(.br_ana_state$token)
  }
  creds <- .br_ana_creds(...)
  if (!nzchar(creds$id) || !nzchar(creds$pw)) {
    cli::cli_abort(c(
      "x" = "ANA HidroWeb requires credentials.",
      "i" = "Set Identificador & Senha in .Renviron (ANA_IDENTIFICADOR / ANA_SENHA) or pass `identificador=` and `senha=`."
    ))
  }

  req <- build_request(
    x,
    path    = "EstacoesTelemetricas/OAUth/v1",
    query   = list(),
    headers = list(
      accept        = "*/*",
      Identificador = creds$id,
      Senha         = creds$pw
    )
  )
  req  <- httr2::req_error(req, is_error = function(resp) httr2::resp_status(resp) >= 400)
  resp <- httr2::req_perform(req)

  ctype    <- httr2::resp_header(resp, "content-type") %||% ""
  body_txt <- try(httr2::resp_body_string(resp), silent = TRUE)
  body_txt <- if (inherits(body_txt, "try-error")) "" else body_txt
  if (grepl("text/html", ctype, TRUE) || grepl("<html", body_txt, TRUE)) {
    cli::cli_abort(c(
      "x" = "Unexpected HTML from token endpoint (likely credentials rejected).",
      "i" = "Verify `ANA_IDENTIFICADOR` and `ANA_SENHA` in your environment.",
      " " = "Endpoint: {x$base_url}/EstacoesTelemetricas/OAUth/v1"
    ))
  }

  js <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  token <- js$items$tokenautenticacao
  if (!nzchar(token)) {
    cli::cli_abort(c(
      "x" = "Auth succeeded but no token field was found in JSON.",
      "i" = "Looked for: tokenautenticacao / accesstoken / access_token / Token."
    ))
  }
  .br_ana_state$token <- token
  token
}

# ---- cache helpers (stations) -----------------------------------------------
.br_ana_cache_dir <- function() {
  base <- tryCatch(rappdirs::user_cache_dir(), error = function(e) NULL)
  if (is.null(base)) base <- file.path(tempdir(), "hydrodownloadR_cache")
  dir <- file.path(base, "hydrodownloadR", "BR_ANA")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}
.br_ana_cache_file <- function(cache_dir = .br_ana_cache_dir()) file.path(cache_dir, "BR_ANA_stations.rds")
.br_ana_seed_path <- function() {
  pkg <- tryCatch(utils::packageName(), error = function(e) NULL) %||% "hydrodownloadR"
  system.file("extdata", "BR_ANA_stations_seed.rds", package = pkg, mustWork = FALSE)
}

# ---- ZIP/MDB resolution -----------------------------------------------------
.br_ana_resolve_mdb <- function(zip_or_mdb, dest_dir = "data-raw/BR_ANA") {
  if (!file.exists(zip_or_mdb)) cli::cli_abort("Path does not exist: {zip_or_mdb}")
  ext <- tolower(tools::file_ext(zip_or_mdb))
  if (ext == "mdb") return(normalizePath(zip_or_mdb, winslash = "/", mustWork = TRUE))
  if (ext == "zip") {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    utils::unzip(zip_or_mdb, exdir = dest_dir)
    mdbs <- list.files(dest_dir, pattern = "\\.mdb$", recursive = TRUE, full.names = TRUE)
    if (!length(mdbs)) cli::cli_abort("No .mdb found after unzipping {basename(zip_or_mdb)}.")
    return(normalizePath(mdbs[1], winslash = "/", mustWork = TRUE))
  }
  cli::cli_abort("Unsupported file type: {ext}. Provide a .zip or .mdb.")
}

# ---- MDB reading (ODBC first, mdbtools fallback) ----------------------------
.br_fix_chr_utf8 <- function(df, try_from = c("UTF-8","CP1252","latin1")) {
  is_chr <- vapply(df, is.character, logical(1))
  if (!any(is_chr)) return(df)
  fix1 <- function(x) {
    if (all(enc2utf8(x) == x, na.rm = TRUE)) return(enc2utf8(x))
    for (f in try_from) {
      y <- suppressWarnings(iconv(x, from = f, to = "UTF-8"))
      if (!any(is.na(y) & !is.na(x))) return(y)
    }
    suppressWarnings(iconv(x, from = "CP1252", to = "UTF-8", sub = "byte"))
  }
  df[is_chr] <- lapply(df[is_chr], fix1)
  df
}
.br_mdb_connect <- function(mdb_path) {
  drivers <- c(
    "{Microsoft Access Driver (*.mdb, *.accdb)}",
    "Microsoft Access Driver (*.mdb, *.accdb)",
    "MDBTools","MDBTools Driver","MDBTools ODBC",
    "Driver do Microsoft Access (*.mdb)","Access"
  )
  last_err <- NULL
  for (drv in drivers) {
    con <- try(DBI::dbConnect(
      odbc::odbc(),
      .connection_string = sprintf("Driver=%s;Dbq=%s;", drv, normalizePath(mdb_path, winslash = "/")),
      encoding = "windows-1252",
      timeout  = 10
    ), silent = TRUE)
    if (!inherits(con, "try-error")) return(con)
    last_err <- con
  }
  attr(last_err, "hydro_msg") <- paste(
    "No usable ODBC driver for Access found.",
    "Install 'Microsoft Access Database Engine 2016 (x64)' or ensure 'mdbtools' is available."
  )
  last_err
}
.br_read_mdb_table <- function(mdb_path, table) {
  con <- .br_mdb_connect(mdb_path)
  if (!inherits(con, "try-error")) {
    on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
    return(DBI::dbReadTable(con, table))
  }
  if (nzchar(Sys.which("mdb-export"))) {
    out <- file.path(tempdir(), paste0(table, ".csv"))
    status <- try(system2("mdb-export", c(shQuote(mdb_path), shQuote(table)), stdout = out, stderr = TRUE), silent = TRUE)
    if (!inherits(status, "try-error") && file.exists(out)) {
      return(readr::read_csv(out, show_col_types = FALSE, progress = FALSE))
    }
  }
  base_msg <- attr(con, "hydro_msg")
  cli::cli_abort(c(
    "x" = sprintf("Could not read table '%s' from MDB.", table),
    if (!is.null(base_msg)) c("!" = base_msg) else NULL,
    ">" = "Option A: Install Microsoft Access Database Engine (x64).",
    ">" = "Option B: Install 'mdbtools' for CSV fallback."
  ))
}

# ---- small utils ------------------------------------------------------------
.br_num <- function(x) { if (is.numeric(x)) return(x); x <- gsub(",", ".", as.character(x), fixed = TRUE); suppressWarnings(as.numeric(x)) }
.br_pick_first <- function(df, candidates) { ix <- match(candidates, names(df)); candidates[which(!is.na(ix))][1] }
.br_norm_names <- function(df) {
  nm <- names(df)
  nm <- gsub("\\s+", " ", nm); nm <- trimws(nm)
  nm <- gsub("á|à|ã|â","a",nm,ignore.case=TRUE); nm <- gsub("é|ê","e",nm,ignore.case=TRUE)
  nm <- gsub("í","i",nm,ignore.case=TRUE); nm <- gsub("ó|ô|õ","o",nm,ignore.case=TRUE)
  nm <- gsub("ú","u",nm,ignore.case=TRUE); nm <- gsub("[^A-Za-z0-9 ]"," ",nm); nm <- gsub("\\s+"," ",nm)
  names(df) <- nm; df
}
.br_as_flag <- function(x) {
  if (is.logical(x)) return(replace(x, is.na(x), FALSE))
  if (is.numeric(x)) return(replace(x > 0, is.na(x), FALSE))
  if (is.character(x) && all(grepl("^[0-9]*$", x[!is.na(x)]))) {
    nx <- suppressWarnings(as.numeric(x))
    return(replace(nx > 0, is.na(nx), FALSE))
  }
  y <- tolower(paste0(x))
  replace(grepl("vaza|desc|liquid|qual|cota|nivel", y, perl = TRUE), is.na(x), FALSE)
}
.br_inventory_meta <- function(zip_or_mdb, mdb_path) {
  nm <- basename(zip_or_mdb %||% mdb_path)
  m <- regexec("Inventario(\\d{2})_(\\d{2})_(\\d{4})", nm, ignore.case = TRUE)
  mt <- regmatches(nm, m)[[1]]
  inv_date <- if (length(mt) == 4) {
    as.Date(sprintf("%s-%s-%s", mt[4], mt[3], mt[2]))
  } else {
    as.Date(file.info(mdb_path)$mtime)
  }
  list(
    inventory_date = inv_date,
    mdb_path       = normalizePath(mdb_path, winslash = "/", mustWork = FALSE),
    mdb_size       = unname(file.info(mdb_path)$size)
  )
}

# -----------------------------------------------------------------------------
# S3: stations()
# -----------------------------------------------------------------------------

#' @export
stations.hydro_service_BR_ANA <- function(x,
                                          zip_or_mdb = NULL,
                                          dest_dir   = "data-raw/BR_ANA",
                                          cache_dir  = .br_ana_cache_dir(),
                                          update     = FALSE) {
  stopifnot(inherits(x, "hydro_service"))

  cache_file <- .br_ana_cache_file(cache_dir)

  if (!isTRUE(update) && file.exists(cache_file)) {
    cli::cli_inform(c(
      "i" = "Loaded ANA station catalogue from cache: {cache_file}",
      ">" = "To refresh, download the latest 'InventarioDD_MM_YYYY.zip' from:",
      " " = "   https://www.snirh.gov.br/hidroweb/download",
      " " = "then call stations(..., zip_or_mdb = 'path/to/Inventario*.zip', update = TRUE)."
    ))
    return(readRDS(cache_file))
  }

  seed <- .br_ana_seed_path()
  if (!isTRUE(update) && file.exists(seed) && !file.exists(cache_file)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(seed, cache_file, overwrite = TRUE)
    cli::cli_inform(c(
      "v" = "Installed initial ANA station catalogue from package seed.",
      " " = "Cache: {cache_file}",
      ">" = "To refresh with current data, download 'InventarioDD_MM_YYYY.zip' and call",
      " " = "   stations(x, zip_or_mdb = '/path/to/Inventario*.zip', update = TRUE)"
    ))
    return(readRDS(cache_file))
  }

  if (is.null(zip_or_mdb) || !nzchar(zip_or_mdb)) {
    cli::cli_abort(c(
      "x" = "No cached/seeded catalogue available and no file provided.",
      ">" = "Download the latest 'InventarioDD_MM_YYYY.zip' from:",
      " " = "   https://www.snirh.gov.br/hidroweb/download",
      " " = "Then call: stations(x, zip_or_mdb = '/path/to/Inventario*.zip', update = TRUE)"
    ))
  }

  mdb_path <- .br_ana_resolve_mdb(zip_or_mdb, dest_dir = dest_dir)

  est <- .br_read_mdb_table(mdb_path, "Estacao")
  rio <- .br_read_mdb_table(mdb_path, "Rio")
  est <- .br_fix_chr_utf8(est); rio <- .br_fix_chr_utf8(rio)
  est <- .br_norm_names(est);  rio <- .br_norm_names(rio)

  c_Codigo    <- c("Codigo","Codigo Estacao","CodigoEstacao","Cod Estacao")
  c_Nome      <- c("Nome","Nome Estacao","NomeEstacao")
  c_RioCodigo <- c("RioCodigo","Codigo Rio","Cod Rio","CodigoRio")
  c_Lat       <- c("Latitude","Lat")
  c_Lon       <- c("Longitude","Long","Lon")
  c_Alt       <- c("Altitude","Cota","Alt")
  c_Area      <- c("AreaDrenagem","Area Drenagem","Area Dren","Area de Drenagem")
  c_TDesc     <- c("TipoEstacaoDescLiquida","Tipo Estacao Desc Liquida","Tipo Descarga Liquida")
  c_Level     <- c("TipoEstacaoEscala")
  c_TQual     <- c("TipoEstacaoQualAgua","Tipo Estacao Qual Agua","Tipo Qualidade Agua")

  colE <- list(
    Codigo        = .br_pick_first(est, c_Codigo),
    Nome          = .br_pick_first(est, c_Nome),
    RioCodigo     = .br_pick_first(est, c_RioCodigo),
    Latitude      = .br_pick_first(est, c_Lat),
    Longitude     = .br_pick_first(est, c_Lon),
    Altitude      = .br_pick_first(est, c_Alt),
    AreaDren      = .br_pick_first(est, c_Area),
    TipoDesc      = .br_pick_first(est, c_TDesc),
    TipoEscala    = .br_pick_first(est, c_Level),
    TipoQual      = .br_pick_first(est, c_TQual)
  )
  if (any(vapply(colE, is.na, logical(1)))) {
    miss <- names(colE)[is.na(unlist(colE))]
    cli::cli_abort(c("x" = "Some required Estacao columns not found.",
                     "!" = paste("Missing:", paste(miss, collapse = ", "))))
  }

  est2 <- tibble::tibble(
    Codigo       = est[[colE$Codigo]],
    Nome         = est[[colE$Nome]],
    `Rio Codigo` = est[[colE$RioCodigo]],
    Latitude     = .br_num(est[[colE$Latitude]]),
    Longitude    = .br_num(est[[colE$Longitude]]),
    Altitude     = .br_num(est[[colE$Altitude]]),
    AreaDrenagem = .br_num(est[[colE$AreaDren]]),
    TipoEstacaoDescLiquida = if (!is.na(colE$TipoDesc)) as.character(est[[colE$TipoDesc]]) else NA_character_,
    TipoEstacaoEscala      = if (!is.na(colE$TipoEscala)) as.character(est[[colE$TipoEscala]]) else NA_character_,
    TipoEstacaoQualAgua    = if (!is.na(colE$TipoQual)) as.character(est[[colE$TipoQual]]) else NA_character_
  )

  cR_Codigo <- c("Codigo","Codigo Rio","Cod Rio","CodigoRio")
  cR_Nome   <- c("Nome","Nome Rio","NomeRio")
  colR <- list(
    Codigo = .br_pick_first(rio, cR_Codigo),
    Nome   = .br_pick_first(rio, cR_Nome)
  )
  if (any(vapply(colR, is.na, logical(1)))) {
    miss <- names(colR)[is.na(unlist(colR))]
    cli::cli_abort(c("x" = "Some required Rio columns not found.",
                     "!" = paste("Missing:", paste(miss, collapse = ", "))))
  }

  rio2 <- tibble::tibble(
    `Rio Codigo` = rio[[colR$Codigo]],
    RioNome      = rio[[colR$Nome]]
  )

  st <- est2 |>
    dplyr::left_join(rio2, by = "Rio Codigo") |>
    dplyr::mutate(
      has_discharge = .br_as_flag(TipoEstacaoDescLiquida),
      has_level     = .br_as_flag(TipoEstacaoEscala),
      has_quality   = .br_as_flag(TipoEstacaoQualAgua),
      Latitude      = dplyr::if_else(Latitude  >= -35 & Latitude  <= 7,  Latitude,  NA_real_),
      Longitude     = dplyr::if_else(Longitude <= -28 & Longitude >= -75, Longitude, NA_real_),
      station_name_ascii = to_ascii(Nome),
      river_ascii        = to_ascii(RioNome),
      country            = "BR",
      provider_id        = "BR_ANA",
      provider_name      = "Brazil – ANA HidroWeb"
    ) |>
    dplyr::relocate(country, provider_id, provider_name, .before = Codigo) |>
    dplyr::relocate(RioNome, .after = "Rio Codigo") |>
    dplyr::distinct(Codigo, .keep_all = TRUE)

  meta <- .br_inventory_meta(zip_or_mdb, mdb_path)
  attr(st, "inventory_date") <- meta$inventory_date
  attr(st, "source_mdb")     <- meta$mdb_path
  attr(st, "source_size")    <- meta$mdb_size

  st_final <- st |>
    dplyr::mutate(
      country       = x$country       %||% "BR",
      provider_id   = x$provider_id   %||% "BR_ANA",
      provider_name = x$provider_name %||% "Brazil – ANA HidroWeb",
      code          = as.character(Codigo),
      station_final = trimws(as.character(Nome)),
      river_final   = trimws(as.character(RioNome)),
      lat           = Latitude,
      lon           = Longitude,
      area_num      = AreaDrenagem,
      alt0          = suppressWarnings(as.numeric(Altitude))
    ) |>
    dplyr::transmute(
      country,
      provider_id,
      provider_name,
      station_id         = code,
      station_name       = station_final,
      station_name_ascii = to_ascii(station_final),
      river              = river_final,
      river_ascii        = to_ascii(river_final),
      lat,
      lon,
      area               = area_num,
      altitude           = alt0,
      has_discharge,
      has_level,
      has_quality
    ) |>
    dplyr::distinct(station_id, .keep_all = TRUE)

  attributes(st_final)[c("inventory_date","source_mdb","source_size")] <-
    attributes(st)[c("inventory_date","source_mdb","source_size")]

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(st_final, cache_file)
  cli::cli_inform(c(
    "v" = "Station catalogue updated → {cache_file}",
    if (!is.null(attr(st_final, "inventory_date")))
      sprintf("   Inventory date: %s  |  N stations: %s",
              format(attr(st_final, "inventory_date")), nrow(st_final))
  ))
  st_final
}

# -----------------------------------------------------------------------------
# Monthly-matrix → daily parser (Vazao_01.._31, Cota_01.._31 [+ _Status])
# -----------------------------------------------------------------------------
.br_ana_parse_monthly_matrix <- function(df, parameter = c("water_discharge","water_level"), tz = "UTC") {
  parameter <- match.arg(parameter)
  prefix <- if (parameter == "water_discharge") "Vazao" else "Cota"

  day_cols    <- grep(paste0("^", prefix, "_\\d{2}$"), names(df), value = TRUE)
  status_cols <- grep(paste0("^", prefix, "_\\d{2}_Status$"), names(df), value = TRUE)
  if (!length(day_cols)) return(tibble::tibble())

  base_col <- NULL
  for (cand in c("Data_Hora_Dado","DataHoraDado","DataLeitura","Data","data")) {
    if (cand %in% names(df)) { base_col <- cand; break }
  }
  if (is.null(base_col)) return(tibble::tibble())

  keep_cols <- c(base_col, day_cols, status_cols)
  d0 <- tibble::as_tibble(df[keep_cols])

  d_val <- tidyr::pivot_longer(
    d0, cols = tidyselect::all_of(day_cols),
    names_to = "day_key", values_to = "value_chr"
  )
  d_val$day <- as.integer(sub("^.*_(\\d{2})$", "\\1", d_val$day_key))

  if (length(status_cols)) {
    d_sta <- tidyr::pivot_longer(
      d0, cols = tidyselect::all_of(status_cols),
      names_to = "status_key", values_to = "quality_code"
    )
    d_sta$day <- as.integer(sub("^.*_(\\d{2})_Status$", "\\1", d_sta$status_key))
    d_val <- dplyr::left_join(
      d_val,
      d_sta[, c(base_col, "day", "quality_code")],
      by = c(base_col, "day")
    )
  } else {
    d_val$quality_code <- NA_character_
  }

  base_dates <- as.Date(substr(d_val[[base_col]], 1, 10))
  d_val$date <- base_dates + (d_val$day - 1L)
  d_val$timestamp <- as.POSIXct(paste0(format(d_val$date, "%Y-%m-%d"), " 00:00:00"), tz = tz)
  d_val$value <- suppressWarnings(as.numeric(gsub(",", ".", d_val$value_chr, fixed = TRUE)))

  dplyr::transmute(d_val, timestamp, value, quality_code) |>
    dplyr::filter(!is.na(timestamp), !is.na(value))
}

# Datetime key detection
.br_ana_pick_datetime <- function(df) {
  cands <- c(
    "Data_Hora_Dado","DataHoraDado",
    "dataLeitura","DataLeitura","data_leitura",
    "data","Data","dataHora","DataHora",
    "dataLeituraUTC","DataLeituraUTC","timestamp","datetime","time"
  )
  i <- match(cands, names(df))
  cands[which(!is.na(i))][1] %||% NA_character_
}

# -----------------------------------------------------------------------------
# S3: timeseries()
# -----------------------------------------------------------------------------

#' @export
timeseries.hydro_service_BR_ANA <- function(x,
                                            parameter = c("water_discharge","water_level"),
                                            stations = NULL,
                                            start_date = NULL, end_date = NULL,
                                            mode = c("complete","range"),
                                            ...) {
  stopifnot(inherits(x, "hydro_service"))
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)

  # Dates: "complete" (1900..today) or "range" with given start/end
  rng <- resolve_dates(mode, start_date, end_date)

  pm  <- .br_param_map(parameter)

  station_vec <- stations %||% {
    st <- stations.hydro_service_BR_ANA(x)
    st$station_id
  }
  station_vec <- unique(as.character(station_vec))
  if (!length(station_vec)) return(tibble::tibble())

  # ANA ≤366 days per request → slice by year boundaries
  .year_slices <- function(a, b) {
    a <- as.Date(a); b <- as.Date(b)
    yrs <- seq.int(as.integer(format(a, "%Y")), as.integer(format(b, "%Y")))
    lapply(yrs, function(y) {
      c(max(as.Date(sprintf("%04d-01-01", y)), a),
        min(as.Date(sprintf("%04d-12-31", y)), b))
    })
  }

  .req <- function(path, query) {
    tok <- .br_ana_get_token(x)
    build_request(
      x,
      path    = path,
      query   = query,
      headers = list(Authorization = paste("Bearer", tok))
    ) |>
      httr2::req_error(is_error = function(resp) httr2::resp_status(resp) >= 400)
  }

  .items <- function(resp) {
    js <- httr2::resp_body_json(resp, simplifyVector = TRUE)
    if (is.list(js) && !is.null(js$items)) js$items else js
  }

  out_list <- vector("list", length(station_vec))
  pb <- progress::progress_bar$new(total = length(station_vec))

  FILTER_TYPE <- "DATA_LEITURA"  # hard-coded per your request
  TZ_LOCAL    <- "UTC"           # hard-coded per your request

  rate_fun <- ratelimitr::limit_rate(
    function(st_id) {
      res_station <- list()
      for (sl in .year_slices(rng$start_date, rng$end_date)) {
        q <- list(
          `Código da Estação`          = suppressWarnings(as.integer(st_id)),
          `Tipo Filtro Data`           = FILTER_TYPE,
          `Data Inicial (yyyy-MM-dd)`  = format(sl[1], "%Y-%m-%d"),
          `Data Final (yyyy-MM-dd)`    = format(sl[2], "%Y-%m-%d")
        )
        req  <- .req(pm$path, q)
        resp <- httr2::req_perform(req)
        if (httr2::resp_status(resp) == 404) next

        items <- .items(resp)
        if (is.null(items) || (is.list(items) && length(items) == 0L)) next

        df <- try(tibble::as_tibble(jsonlite::flatten(items)), silent = TRUE)
        if (inherits(df, "try-error") || !nrow(df)) next

        # 1) Prefer monthly matrix → daily rows
        daily <- .br_ana_parse_monthly_matrix(df, parameter = parameter, tz = TZ_LOCAL)
        if (nrow(daily)) {
          res_station[[length(res_station) + 1L]] <- tibble::tibble(
            country       = x$country,
            provider_id   = x$provider_id,
            provider_name = x$provider_name,
            station_id    = st_id,
            parameter     = parameter,
            timestamp     = daily$timestamp,
            value         = daily$value,
            unit          = pm$unit,
            quality_code  = daily$quality_code,
            source_url    = paste0(x$base_url, "/", pm$path)
          )
          next
        }

        # 2) Fallback: point-wise layout
        dt_col <- .br_ana_pick_datetime(df)
        ts_parsed <- if (!is.na(dt_col)) {
          suppressWarnings(lubridate::as_datetime(df[[dt_col]], tz = TZ_LOCAL))
        } else {
          as.POSIXct(NA_character_, tz = TZ_LOCAL)
        }

        # value
        val <- {
          outv <- rep(NA_real_, nrow(df))
          for (k in pm$value_keys) if (k %in% names(df)) { outv <- suppressWarnings(as.numeric(df[[k]])); break }
          outv
        }
        # quality
        qf  <- {
          cands <- intersect(c("qualidade","quality","qualityFlag","status","flag"), names(df))
          if (length(cands)) as.character(df[[cands[1]]]) else rep(NA_character_, nrow(df))
        }

        keep <- !is.na(ts_parsed) &
          as.Date(ts_parsed) >= as.Date(sl[1]) &
          as.Date(ts_parsed) <= as.Date(sl[2])
        if (!any(keep, na.rm = TRUE)) next

        res_station[[length(res_station) + 1L]] <- tibble::tibble(
          country       = x$country,
          provider_id   = x$provider_id,
          provider_name = x$provider_name,
          station_id    = st_id,
          parameter     = parameter,
          timestamp     = ts_parsed[keep],
          value         = suppressWarnings(as.numeric(val[keep])),
          unit          = pm$unit,
          quality_code  = qf[keep],
          source_url    = paste0(x$base_url, "/", pm$path)
        )
      }
      if (length(res_station)) dplyr::bind_rows(res_station) else tibble::tibble()
    },
    rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period)
  )

  for (i in seq_along(station_vec)) {
    pb$tick()
    out_list[[i]] <- rate_fun(station_vec[[i]])
  }

  out <- dplyr::bind_rows(out_list)
  if (!nrow(out)) return(out)

  out |>
    dplyr::filter(
      !is.na(timestamp),
      as.Date(timestamp) >= as.Date(rng$start_date),
      as.Date(timestamp) <= as.Date(rng$end_date)
    ) |>
    dplyr::arrange(station_id, timestamp)
}
