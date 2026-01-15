# R/adapter_FR_HUBEAU.R
# France – Hub’Eau (Eaufrance) adapter
# Hydrométrie v2 + Température v1
# Docs (hydrométrie): https://hubeau.eaufrance.fr/page/api-hydrometrie
# Docs (température): https://hubeau.eaufrance.fr/page/api-temperature-continu
# Units/time: H=mm, Q=l/s (convert), UTC timestamps. [see docs]

# ---- registration ------------------------------------------------------------

register_FR_HUBEAU <- function() {
  register_service(
    provider_id   = "FR_HUBEAU",
    provider_name = "France – Hub’Eau (Eaufrance) API",
    country       = "FR",
    base_url      = "https://hubeau.eaufrance.fr",
    rate_cfg      = list(n = 3, period = 1),
    auth          = list(type = "none")
  )
}

# keep parameter list discoverable
#' @export
timeseries_parameters.hydro_service_FR_HUBEAU <- function(x, ...) {
  c("water_discharge", "water_level", "water_temperature")
}

# ---- parameter mapping -------------------------------------------------------

# Canonical unit converters
.fr_conv_mm_to_cm  <- function(x) x / 10      # H: mm -> cm
.fr_conv_lps_to_m3 <- function(x) x / 1000    # Q: l/s -> m^3/s

# For a tidy error message
.fr_supported_params <- function() c("water_discharge", "water_level", "water_temperature")

# Consistent switch-based mapper (no 'resolution' arg)
.fr_param_map <- function(parameter) {
  switch(parameter,
         water_discharge = list(
           api           = "hydrometrie",
           stations_path = "/api/v2/hydrometrie/referentiel/stations",
           ts_path       = "/api/v2/hydrometrie/obs_elab",
           fixed_qs      = list(grandeur_hydro_elab = "QmnJ"),  # daily mean discharge
           unit          = "m^3/s",
           convert       = .fr_conv_lps_to_m3
         ),

         water_level = list(
           api           = "hydrometrie",
           stations_path = "/api/v2/hydrometrie/referentiel/stations",
           ts_path       = "/api/v2/hydrometrie/observations_tr",  # realtime H
           fixed_qs      = list(grandeur_hydro = "H"),
           unit          = "cm",
           convert       = .fr_conv_mm_to_cm
         ),

         water_temperature = list(
           api           = "temperature",
           stations_path = "/api/v1/temperature/station",
           ts_path       = "/api/v1/temperature/chronique",
           fixed_qs      = list(),
           unit          = "°C",
           convert       = function(x) x
         ),

         # default (unsupported parameter)
         rlang::abort(paste0(
           "FR_HUBEAU supports ", paste(.fr_supported_params(), collapse = ", "), "."
         ))
  )
}


.fr_parse_json <- function(res) {
  txt <- tryCatch(
    if (is.raw(res$body)) rawToChar(res$body) else as.character(res$body),
    error = function(e) ""
  )
  if (!nzchar(txt)) return(NULL)
  Encoding(txt) <- "UTF-8"          # mark as UTF-8 (no recode)
  #   txt <- enc2utf8(txt)              # normalize to UTF-8 (safe no-op if already UTF-8)
  # simplifyVector=TRUE -> data frames where possible
  tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
}

# Best-effort URL (perform_request may provide $url; else NA)
.fr_used_url <- function(res, req) {
  `%^%` <- function(a,b) if (!is.null(a) && !is.na(a) && nzchar(a)) a else b
  (res$url %^% req$url) %^% NA_character_
}

# --- number normalization (FR) -----------------------------------------------
.fr_num_fr <- function(x) {
  if (is.null(x) || !length(x)) return(NA_real_)
  x <- as.character(x)

  # Normalize whitespace (incl. NBSP/thin NBSP)
  x <- gsub("\u202F|\u00A0", " ", x)
  x <- trimws(x)

  # Keep only digits, sign, and separators; drop all letters/symbols (km, m, ², etc.)
  x2 <- gsub("[^0-9,.-]", "", x)
  if (!nzchar(x2)) return(NA_real_)

  # Decide decimal separator by the last separator pattern
  if (grepl(",\\d{1,3}$", x2)) {
    # decimal comma → drop thousands dots, then comma→dot
    x2 <- gsub("\\.", "", x2)
    x2 <- sub(",", ".", x2)
  } else if (grepl("\\.\\d{1,3}$", x2)) {
    # decimal dot → drop thousands commas
    x2 <- gsub(",", "", x2)
  } else {
    # looks like an integer; drop any stray separators
    x2 <- gsub("[^0-9-]", "", x2)
  }

  suppressWarnings(as.numeric(x2))
}


# --- HTML extractors (prefer xml2; regex fallback) ---------------------------
.fr_html_extract_area <- function(html_txt) {
  if (!nzchar(html_txt)) return(NA_real_)
  if (requireNamespace("xml2", quietly = TRUE)) {
    doc <- tryCatch(xml2::read_html(html_txt, encoding = "UTF-8"), error = function(e) NULL)
    if (!is.null(doc)) {
      node <- xml2::xml_find_first(
        doc,
        "//span[normalize-space(.)='Surface de bassin versant topographique']/following-sibling::span[1]"
      )
      if (!is.null(node)) {
        valtxt <- xml2::xml_text(node, trim = TRUE)
        # remove trailing "km²"/"km2" (case-insensitive), then parse
        valtxt_num <- sub("(?i)\\s*km\\s*(?:²|2)\\s*$", "", valtxt, perl = TRUE)
        num <- .fr_num_fr(valtxt_num)
        if (!is.na(num)) return(num)
      }
    }
  }
  # regex fallback (DOTALL), tolerate km² or km2
  rx <- "(?s)Surface de bassin versant topographique.*?<span>\\s*([0-9][0-9\\s.,\u00A0\u202F]*)\\s*km(?:\\s*²|2)"
  m  <- regexec(rx, html_txt, perl = TRUE)
  g  <- regmatches(html_txt, m)
  if (length(g) && length(g[[1]]) >= 2) return(.fr_num_fr(g[[1]][2]))
  NA_real_
}

.fr_html_extract_gauge_zero <- function(html_txt) {
  if (!nzchar(html_txt)) return(NA_real_)
  if (requireNamespace("xml2", quietly = TRUE)) {
    doc <- tryCatch(xml2::read_html(html_txt, encoding = "UTF-8"), error = function(e) NULL)
    if (!is.null(doc)) {
      # xml2 resolves &#039; to an apostrophe ' in text()
      node <- xml2::xml_find_first(
        doc,
        "//span[normalize-space(.)=\"Cote du zéro d'échelle\"]/following-sibling::span[1]"
      )
      if (!is.null(node)) {
        valtxt <- xml2::xml_text(node, trim = TRUE)
        # value typically like "12,34 m" or "12.34 m"
        valtxt <- sub("\\s*m\\s*$", "", valtxt)
        num <- .fr_num_fr(valtxt)
        if (!is.na(num)) return(num)
      }
    }
  }
  # regex fallback — accept either literal ' or &#039;
  rx <- "(?s)Cote du z[ée]ro d(?:'|&#039;)échelle.*?<span>\\s*([0-9][0-9\\s.,\u00A0\u202F]*)\\s*m"
  m  <- regexec(rx, html_txt, perl = TRUE)
  g  <- regmatches(html_txt, m)
  if (length(g) && length(g[[1]]) >= 2) return(.fr_num_fr(g[[1]][2]))
  NA_real_
}

# Prefer xml2 DOM; fallback to regex. All return numeric meters.

# Site altitude: look for exact label "Altitude" on the site fiche.
.fr_html_extract_site_altitude <- function(html_txt) {
  if (!nzchar(html_txt)) return(NA_real_)
  if (requireNamespace("xml2", quietly = TRUE)) {
    doc <- tryCatch(xml2::read_html(html_txt, encoding = "UTF-8"), error = function(e) NULL)
    if (!is.null(doc)) {
      # 1) exact "Altitude"
      node <- xml2::xml_find_first(doc, "//span[normalize-space(.)='Altitude']/following-sibling::span[1]")
      if (!is.null(node)) {
        val <- xml2::xml_text(node, trim = TRUE)
        val <- sub("\\s*m\\s*$", "", val)
        num <- .fr_num_fr(val)
        if (!is.na(num)) return(num)
      }
      # 2) fallbacks previously seen on some fiches
      labels <- c("Altitude du site", "Altitude du repère", "Altitude du repère géodésique")
      for (lb in labels) {
        node2 <- xml2::xml_find_first(doc, sprintf("//span[normalize-space(.)='%s']/following-sibling::span[1]", lb))
        if (!is.null(node2)) {
          val <- xml2::xml_text(node2, trim = TRUE)
          val <- sub("\\s*m\\s*$", "", val)
          num <- .fr_num_fr(val)
          if (!is.na(num)) return(num)
        }
      }
    }
  }
  # Regex fallback
  rx <- "(?s)Altitude[^<]*?</span>\\s*<span>\\s*([0-9][0-9\\s.,\u00A0\u202F]*)\\s*m"
  m  <- regexec(rx, html_txt, perl = TRUE)
  g  <- regmatches(html_txt, m)
  if (length(g) && length(g[[1]]) >= 2) return(.fr_num_fr(g[[1]][2]))
  NA_real_
}

# Station altitude (gauge zero): label "Cote du zéro d'échelle" on station fiche.# Gauge zero ("Cote du zéro d'échelle") from station fiche
.fr_html_extract_station_gauge_zero <- function(html_txt) {
  if (!nzchar(html_txt)) return(NA_real_)

  # 1) DOM path (preferred)
  if (requireNamespace("xml2", quietly = TRUE)) {
    doc <- tryCatch(xml2::read_html(html_txt, encoding = "UTF-8"), error = function(e) NULL)
    if (!is.null(doc)) {
      # Find the *label span* then take its immediate following value span
      # Normalize NBSP (\u00A0) and thin NBSP (\u202F) to regular spaces inside the XPath
      xp <- "//span[contains(@class,'identity__label') and normalize-space(translate(., '\u00A0\u202F', '  '))=\"Cote du zéro d'échelle\"]/following-sibling::span[1]"
      node <- xml2::xml_find_first(doc, xp)
      if (!inherits(node, "xml_missing") && !is.null(node)) {
        txt <- xml2::xml_text(node, trim = TRUE)
        # Missing value?
        if (!nzchar(txt) || grepl("Non renseign", txt, ignore.case = TRUE)) return(NA_real_)
        # Expect formats like "282,11 m", "282 m"
        num_txt <- sub("\\s*m\\s*$", "", txt)
        val <- .fr_num_fr(num_txt)
        return(if (!is.na(val)) val else NA_real_)
      }
    }
  }

  # 2) Regex fallback (only the immediate sibling span of the label)
  rx <- "(?is)Cote du z[ée]ro d(?:'|&#039;)échelle\\s*</span>\\s*<span>\\s*([0-9][0-9\\s.,\u00A0\u202F]*)\\s*m\\s*</span>"
  m  <- regexec(rx, html_txt, perl = TRUE)
  g  <- regmatches(html_txt, m)
  if (length(g) && length(g[[1]]) >= 2) {
    val <- .fr_num_fr(g[[1]][2])
    return(if (!is.na(val)) val else NA_real_)
  }

  NA_real_
}


# --- Site meta fetcher (area, gauge zero, site altitude) ---------------------
# Speedups: curl multi (waves), bounded concurrency, cache w/ TTL
.fr_fetch_site_meta <- function(code_sites,
                                rate = list(n = 3, period = 1),
                                max_conns = NULL,
                                use_cache = TRUE,
                                cache_ttl_days = 90,
                                sequential_retry_on_403 = TRUE,
                                jitter_sec = 0.25,
                                warmup = TRUE) {
  code_sites <- unique(stats::na.omit(as.character(code_sites)))
  if (!length(code_sites)) {
    return(tibble::tibble(code_site = character(), area = double(), altitude_site = double()))
  }

  # ----- cache gate -----------------------------------------------------------
  cache <- if (use_cache) .fr_site_cache_read() else tibble::tibble(code_site=character(), area=double(), altitude_site=double(), fetched_at=as.POSIXct(character()))
  ttl_cutoff <- Sys.time() - cache_ttl_days*24*3600
  in_cache <- cache$code_site %in% code_sites
  fresh <- in_cache & cache$fetched_at >= ttl_cutoff & !is.na(cache$area) & !is.na(cache$altitude_site)
  cached_ok <- cache[in_cache & fresh, c("code_site","area","altitude_site")]
  need_ids  <- setdiff(code_sites, cached_ok$code_site)
  if (!length(need_ids)) return(dplyr::arrange(cached_ok, match(cached_ok$code_site, code_sites)))

  # ----- concurrency + headers + cookies -------------------------------------
  if (is.null(max_conns)) max_conns <- max(1L, min(rate$n, 6L))  # be extra polite
  chunks <- split(need_ids, ceiling(seq_along(need_ids) / max_conns))

  ua  <- sprintf("Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) hydrodownloadR/%s Safari/537.36",
                 as.character(utils::packageVersion("curl")))
  jar <- file.path(.fr_cache_dir(), "hydro_eaufrance.cookies.txt")

  # warm-up: set cookies once per run (homepage)
  if (warmup) {
    invisible(.fr_http_get_html("https://www.hydro.eaufrance.fr/", ua, jar))
    Sys.sleep(0.3)
  }

  pb <- progress::progress_bar$new(
    total = length(need_ids),
    format = "FR_HUBEAU site-meta [:bar] :current/:total (:percent) :eta",
    clear = FALSE, width = 70
  )

  parse_one <- function(html_txt, cs) {
    tibble::tibble(
      code_site     = cs,
      area          = .fr_html_extract_area(html_txt),
      altitude_site = .fr_html_extract_site_altitude(html_txt),
      vertical_datum_site = .fr_html_extract_site_datum(html_txt),
      fetched_at    = Sys.time()
    )
  }

  fetched_rows <- list()
  failed_403   <- character()

  for (chunk in chunks) {
    pool <- curl::new_pool()
    res_chunk  <- vector("list", length(chunk))
    code_chunk <- as.character(chunk)
    status_vec <- integer(length(chunk))

    for (i in seq_along(code_chunk)) {
      cs   <- code_chunk[[i]]
      url  <- sprintf("https://www.hydro.eaufrance.fr/sitehydro/%s/fiche", cs)
      h    <- curl::new_handle()
      curl::handle_setopt(h,
                          cookiefile = jar, cookiejar = jar,
                          http_version = 2L, timeout = 20
      )
      curl::handle_setheaders(h,
                              "User-Agent" = ua,
                              "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                              "Accept-Language" = "fr-FR,fr;q=0.9,en;q=0.5",
                              "Referer" = "https://www.hydro.eaufrance.fr/"
      )

      curl::curl_fetch_multi(
        url,
        done = (function(ix, cs_id) {
          force(ix); force(cs_id)
          function(res) {
            status_vec[[ix]] <<- res$status_code
            html <- tryCatch(rawToChar(res$content), error=function(e) "")
            Encoding(html) <- "UTF-8"
            res_chunk[[ix]] <<- tryCatch(parse_one(html, cs_id),
                                         error=function(e) tibble::tibble(code_site = cs_id, area=NA_real_, altitude_site=NA_real_, fetched_at=Sys.time()))
            pb$tick()
          }
        })(i, cs),
        fail = (function(ix, cs_id) {
          force(ix); force(cs_id)
          function(err) {
            status_vec[[ix]] <<- 0L
            res_chunk[[ix]] <<- tibble::tibble(code_site = cs_id, area=NA_real_, altitude_site=NA_real_, fetched_at=Sys.time())
            pb$tick()
          }
        })(i, cs),
        pool = pool,
        handle = h
      )
    }

    curl::multi_run(pool = pool)

    # record any 403s from this wave
    if (any(status_vec == 403L, na.rm = TRUE)) {
      failed_403 <- c(failed_403, code_chunk[which(status_vec == 403L)])
    }

    fetched_rows[[length(fetched_rows)+1L]] <- dplyr::bind_rows(res_chunk)

    # pacing: respect n/period and add a tiny jitter
    Sys.sleep(rate$period + stats::runif(1, 0, jitter_sec))
  }

  new_tbl <- dplyr::bind_rows(fetched_rows)

  # ----- sequential retry for 403s (very gentle) ------------------------------
  if (sequential_retry_on_403 && length(failed_403)) {
    for (cs in unique(failed_403)) {
      Sys.sleep(rate$period + stats::runif(1, 0, jitter_sec))
      url <- sprintf("https://www.hydro.eaufrance.fr/sitehydro/%s/fiche", cs)
      got <- .fr_http_get_html(url, ua, jar)
      if (!is.na(got$status) && got$status == 200L) {
        row <- tryCatch(parse_one(got$html, cs),
                        error=function(e) tibble::tibble(code_site = cs, area=NA_real_, altitude_site=NA_real_, fetched_at=Sys.time()))
        # replace the NA row for this cs
        idx <- which(new_tbl$code_site == cs)
        if (length(idx)) new_tbl[idx[1], ] <- row else new_tbl <- dplyr::bind_rows(new_tbl, row)
      }
    }
  }

  # ----- cache update and ordered return --------------------------------------
  if (use_cache) {
    merged <- dplyr::bind_rows(cache, new_tbl)
    merged <- merged[order(merged$code_site, merged$fetched_at, decreasing = TRUE), ]
    merged <- merged[!duplicated(merged$code_site), ]
    .fr_site_cache_write(merged)
    out <- merged
  } else {
    out <- new_tbl
  }

  out <- out[out$code_site %in% code_sites,
             c("code_site","area","altitude_site","vertical_datum_site")]
  dplyr::arrange(out, match(out$code_site, code_sites))
}


# Fetch gauge-zero altitude (m) from /stationhydro/{station_id}/fiche
.fr_fetch_station_meta <- function(station_ids, rate = list(n = 3, period = 1)) {
  station_ids <- unique(stats::na.omit(as.character(station_ids)))
  if (!length(station_ids)) {
    return(tibble::tibble(station_id = character(), altitude_station = double()))
  }

  limiter <- ratelimitr::limit_rate(function(url) httr::GET(url, httr::timeout(20)),
                                    rate = do.call(ratelimitr::rate, rate))
  pb <- progress::progress_bar$new(
    total = length(station_ids),
    format = "FR_HUBEAU station-meta [:bar] :current/:total (:percent) :eta",
    clear = FALSE, width = 70
  )

  rows <- lapply(station_ids, function(sid) {
    on.exit(pb$tick(), add = TRUE)
    url <- sprintf("https://www.hydro.eaufrance.fr/stationhydro/%s/fiche", sid)
    r <- tryCatch(limiter(url), error = function(e) NULL)
    if (is.null(r) || httr::http_error(r)) {
      return(tibble::tibble(station_id = sid, altitude_station = NA_real_))
    }
    html <- tryCatch(httr::content(r, as = "text", encoding = "UTF-8"), error = function(e) "")
    tibble::tibble(
      station_id       = sid,
      altitude_station = .fr_html_extract_station_gauge_zero(html) # m (Cote du zéro d'échelle)
    )
  })

  dplyr::bind_rows(rows)
}



# Fetch areas for a vector of code_site values (character), rate-limited and with progress
.fr_fetch_site_areas <- function(code_sites, rate = list(n = 3, period = 1)) {
  code_sites <- unique(stats::na.omit(code_sites))
  if (!length(code_sites)) return(tibble::tibble(code_site = character(), area = double()))

  # Simple rate limiter wrapper around httr::GET
  limiter <- ratelimitr::limit_rate(function(url) {
    httr::GET(url, httr::timeout(20))
  }, rate = do.call(ratelimitr::rate, rate))

  pb <- progress::progress_bar$new(
    total = length(code_sites),
    format = "FR_HUBEAU area [:bar] :current/:total (:percent) :eta",
    clear = FALSE, width = 70
  )

  res_list <- lapply(code_sites, function(cs) {
    on.exit(pb$tick(), add = TRUE)
    url <- sprintf("https://www.hydro.eaufrance.fr/sitehydro/%s/fiche", cs)
    r <- tryCatch(limiter(url), error = function(e) NULL)
    if (is.null(r) || httr::http_error(r)) {
      return(list(code_site = cs, area = NA_real_))
    }
    html_txt <- tryCatch(httr::content(r, as = "text", encoding = "UTF-8"), error = function(e) "")
    area_km2 <- .fr_html_extract_area(html_txt)
    list(code_site = cs, area = area_km2)
  })

  dplyr::bind_rows(lapply(res_list, tibble::as_tibble))
}

# Site-level vertical datum: label after "Système altimétrique"
.fr_html_extract_site_datum <- function(html_txt) {
  if (!nzchar(html_txt)) return(NA_character_)
  if (requireNamespace("xml2", quietly = TRUE)) {
    doc <- tryCatch(xml2::read_html(html_txt, encoding = "UTF-8"), error = function(e) NULL)
    if (!is.null(doc)) {
      # Attempt 1: exact match after converting NBSP + NNBSP to regular space
      xp1 <- "//span[normalize-space(translate(., '\u00A0\u202F', '  '))='Système altimétrique']/following-sibling::span[1]"
      node <- xml2::xml_find_first(doc, xp1)
      if (!inherits(node, "xml_missing") && !is.null(node)) {
        val <- xml2::xml_text(node, trim = TRUE)
        if (nzchar(val)) return(val)
      }

      # Attempt 2: looser 'contains' on the label, then take the 2nd span in the field
      xp2 <- "//div[contains(@class,'identity__field')][span[contains(translate(., '\u00A0\u202F', '  '), 'Système') and contains(., 'altim')]]/span[position()=2]"
      node <- xml2::xml_find_first(doc, xp2)
      if (!inherits(node, "xml_missing") && !is.null(node)) {
        val <- xml2::xml_text(node, trim = TRUE)
        if (nzchar(val)) return(val)
      }

      # Attempt 3: R-side scan of label/value pairs
      labs <- xml2::xml_find_all(doc, "//div[contains(@class,'identity__field')]/span[1]")
      if (length(labs)) {
        lab_txt <- xml2::xml_text(labs, trim = TRUE)
        lab_norm <- gsub("[\u00A0\u202F]", " ", lab_txt)       # NBSP/NNBSP -> space
        lab_norm <- gsub("\\s+", " ", lab_norm)                # collapse
        hit <- which(lab_norm == "Système altimétrique")
        if (length(hit)) {
          sib <- xml2::xml_find_first(labs[[hit[1]]], "following-sibling::span[1]")
          val <- xml2::xml_text(sib, trim = TRUE)
          if (nzchar(val)) return(val)
        }
      }
    }
  }

  # Regex fallback
  rx <- "(?is)Syst[èe]me\\s*altim[ée]trique\\s*</span>\\s*<span>\\s*([^<]+?)\\s*</span>"
  m  <- regexec(rx, html_txt, perl = TRUE)
  g  <- regmatches(html_txt, m)
  if (length(g) && length(g[[1]]) >= 2) return(trimws(g[[1]][2]))
  NA_character_
}



.fr_http_get_html <- function(url, ua, jar, timeout = 20) {
  h <- curl::new_handle()
  curl::handle_setopt(h,
                      cookiefile = jar, cookiejar = jar,
                      http_version = 2L,                  # HTTP/2 if available
                      timeout = timeout
  )
  curl::handle_setheaders(h,
                          "User-Agent" = ua,
                          "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                          "Accept-Language" = "fr-FR,fr;q=0.9,en;q=0.5",
                          "Referer" = "https://www.hydro.eaufrance.fr/"
  )
  res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) NULL)
  if (is.null(res)) return(list(status = NA_integer_, html = ""))
  html <- tryCatch(rawToChar(res$content), error = function(e) "")
  Encoding(html) <- "UTF-8"
  list(status = res$status_code, html = html)
}


# ---- simple cache helpers ----------------------------------------------------
.fr_cache_dir <- function() {
  dir <- tools::R_user_dir("hydrodownloadR", "cache")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}
.fr_site_cache_path <- function() file.path(.fr_cache_dir(), "fr_hubeau_site_meta.rds")

.fr_site_cache_read <- function() {
  p <- .fr_site_cache_path()
  if (file.exists(p)) {
    x <- tryCatch(readRDS(p), error = function(e) NULL)
    if (is.null(x)) tibble::tibble(code_site=character(), area=double(), altitude_site=double(), fetched_at=as.POSIXct(character()))
    else tibble::as_tibble(x)
  } else {
    tibble::tibble(code_site=character(), area=double(), altitude_site=double(), fetched_at=as.POSIXct(character()))
  }
}
.fr_site_cache_write <- function(tbl) {
  p <- .fr_site_cache_path()
  # keep one row per code_site (latest fetched_at wins)
  tbl <- tbl[order(tbl$code_site, tbl$fetched_at, decreasing = TRUE), ]
  tbl <- tbl[!duplicated(tbl$code_site), ]
  saveRDS(tbl, p)
  invisible(tbl)
}

.fr_load_precomputed_meta <- function() {
  # 1) try lazy-loaded object (when installed/loaded)
  meta <- get0("fr_hubeau_meta", envir = asNamespace("hydrodownloadR"), ifnotfound = NULL)
  if (!is.null(meta)) {
    return(list(
      tab  = tibble::as_tibble(meta),
      date = attr(meta, "metadata_date", exact = TRUE) %||%
        tryCatch(as.character(as.Date(max(meta$retrieved_at, na.rm = TRUE))), error = function(e) NA_character_)
    ))
  }

  # 2) try loading the .rda directly if it exists in this source tree
  fp <- file.path(system.file(package = "hydrodownloadR"), "data", "fr_hubeau_meta.rda")
  if (!nzchar(fp) || !file.exists(fp)) {
    # fallback for dev mode: when running from source, system.file() can be empty.
    # Look on disk relative to project root:
    fp <- "data/fr_hubeau_meta.rda"
  }
  if (file.exists(fp)) {
    env <- new.env(parent = emptyenv())
    try(load(fp, envir = env), silent = TRUE)
    meta <- get0("fr_hubeau_meta", envir = env, ifnotfound = NULL)
    if (!is.null(meta)) {
      return(list(
        tab  = tibble::as_tibble(meta),
        date = attr(meta, "metadata_date", exact = TRUE) %||%
          tryCatch(as.character(as.Date(max(meta$retrieved_at, na.rm = TRUE))), error = function(e) NA_character_)
      ))
    }
  }

  # 3) last resort: do nothing
  NULL
}

.fr_pick_col <- function(df, choices) {
  for (nm in choices) if (nm %in% names(df)) return(nm)
  NULL
}

.fr_try_req <- function(do_req, retries = 3, base_sleep = 0.5) {
  for (i in seq_len(retries)) {
    res <- try(do_req(), silent = TRUE)
    if (!inherits(res, "try-error")) return(res)
    Sys.sleep(base_sleep * (2^(i - 1)) + stats::runif(1, 0, 0.2))
  }
  do_req() # last attempt; let errors surface
}


# ---- stations() --------------------------------------------------------------



#' @export
stations.hydro_service_FR_HUBEAU <- function(
    x,
    update = FALSE,
    verbose = TRUE,
    include_hydrometry   = TRUE,
    include_temperature  = TRUE,
    ...
) {
  hm_path <- "/api/v2/hydrometrie/referentiel/stations"
  tp_path <- "/api/v1/temperature/station"

  get_paged <- function(path, page_size = 10000L) {
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
    if (!length(out)) return(NULL)
    dplyr::bind_rows(out)
  }

  # pull referentials (optionally)
  hm <- if (include_hydrometry)  tibble::as_tibble(get_paged(hm_path) %||% tibble::tibble()) else tibble::tibble()
  tp <- if (include_temperature) tibble::as_tibble(get_paged(tp_path) %||% tibble::tibble()) else tibble::tibble()

  # hydrometry rows (note: API altitude is in mm → m)
  hm_tbl <- if (nrow(hm)) tibble::tibble(
    code_site    = col_or_null(hm, "code_site"),
    station_id   = col_or_null(hm, "code_station"),
    station_name = normalize_utf8(col_or_null(hm, "libelle_site")),
    river        = normalize_utf8(col_or_null(hm, "libelle_cours_eau")),
    lat          = suppressWarnings(as.numeric(col_or_null(hm, "latitude_station"))),
    lon          = suppressWarnings(as.numeric(col_or_null(hm, "longitude_station"))),
    altitude_api = suppressWarnings(as.numeric(col_or_null(hm, "altitude_ref_alti_station"))),
    area         = NA_real_,
    source_api   = "hydrometrie",
    has_discharge   = TRUE,
    has_level       = TRUE,
    has_temperature = FALSE
  ) else tibble::tibble()

  # temperature rows
  tp_tbl <- if (nrow(tp)) tibble::tibble(
    code_site    = col_or_null(tp, "code_site"),
    station_id   = col_or_null(tp, "code_station"),
    station_name = normalize_utf8(col_or_null(tp, "libelle_station")),
    river        = normalize_utf8(col_or_null(tp, "libelle_cours_eau")),
    lat          = suppressWarnings(as.numeric(col_or_null(tp, "latitude"))),
    lon          = suppressWarnings(as.numeric(col_or_null(tp, "longitude"))),
    altitude_api = suppressWarnings(as.numeric(col_or_null(tp, "altitude"))),
    area         = NA_real_,
    source_api   = "temperature",
    has_discharge   = FALSE,
    has_level       = FALSE,
    has_temperature = TRUE
  ) else tibble::tibble()

  both <- dplyr::bind_rows(hm_tbl, tp_tbl)

  if (!nrow(both)) {
    return(tibble::tibble(
      country = x$country, provider_id = x$provider_id, provider_name = x$provider_name,
      station_id=character(), station_name=character(), station_name_ascii=character(),
      river=character(), river_ascii=character(),
      lat=double(), lon=double(), area=double(),
      altitude_api=double(), altitude_site=double(), altitude_station=double(),
      source_api=character(), has_discharge=logical(), has_level=logical(), has_temperature=logical()
    ))
  }

  # dedupe by station_id (defensive — hydrometry vs temperature are disjoint in practice)
  both <- dplyr::distinct(both, .data$station_id, .keep_all = TRUE)

  # ----------------------- precomputed meta (fast path) -----------------------
  if (!isTRUE(update)) {
    meta <- .fr_load_precomputed_meta()
    if (!is.null(meta)) {
      if (isTRUE(verbose)) {
        packageStartupMessage(sprintf(
          "FR_HUBEAU: using precomputed station metadata (built %s). For the freshest values, call stations(..., update = TRUE).",
          meta$date %||% "unknown date"
        ))
      }
      both <- dplyr::left_join(
        both,
        dplyr::select(meta$tab, code_site, station_id, area, altitude_api, altitude_site, altitude_station),
        by = c("code_site", "station_id"),
        multiple = "first", relationship = "many-to-one"
      )

      # coalesce area (prefer precomputed if present)
      if (all(c("area.x","area.y") %in% names(both))) {
        both$area <- dplyr::coalesce(both$area.y, both$area.x)
        both <- dplyr::select(both, -dplyr::any_of(c("area.x","area.y")))
      }

      # coalesce altitude_api if we happen to have both
      if (any(c("altitude_api.x","altitude_api.y") %in% names(both))) {
        both$altitude_api <- dplyr::coalesce(both$altitude_api.x, both$altitude_api.y)
        both <- dplyr::select(both, -dplyr::any_of(c("altitude_api.x","altitude_api.y")))
      }

      attr(both, "fr_hubeau_meta_date")   <- meta$date
      attr(both, "fr_hubeau_meta_source") <- "precomputed"
    } else if (isTRUE(verbose)) {
      packageStartupMessage("FR_HUBEAU: no precomputed metadata found; may fetch live if update=TRUE.")
    }
  }

  # ------------------------- live meta (slow path) ----------------------------
  need_live <- isTRUE(update) ||
    !all(c("altitude_site","altitude_station") %in% names(both))

  if (need_live) {
    # Site-level scrape (area + site altitude)
    site_meta <- .fr_fetch_site_meta(
      stats::na.omit(both$code_site),
      rate = list(n = 3, period = 1),
      max_conns = 3, use_cache = TRUE, cache_ttl_days = 90
    )

    if (nrow(site_meta)) {
      both <- dplyr::left_join(
        both, site_meta, by = "code_site",
        multiple = "first", relationship = "many-to-one"
      )
      # coalesce area again if join created .x/.y
      if (all(c("area.x","area.y") %in% names(both))) {
        both$area <- dplyr::coalesce(both$area.y, both$area.x)
        both <- dplyr::select(both, -dplyr::any_of(c("area.x","area.y")))
      }
    } else {
      both$altitude_site <- both$altitude_site %||% NA_real_
    }

    # Station-level scrape (gauge-zero altitude)
    st_meta <- .fr_fetch_station_meta(stats::na.omit(both$station_id))
    if (nrow(st_meta)) {
      both <- dplyr::left_join(
        both, st_meta, by = "station_id",
        multiple = "first", relationship = "many-to-one"
      )
    } else {
      both$altitude_station <- both$altitude_station %||% NA_real_
    }

    if (isTRUE(verbose)) {
      packageStartupMessage("FR_HUBEAU: fetched live metadata (slow path).")
    }
    attr(both, "fr_hubeau_meta_date")   <- as.character(Sys.Date())
    attr(both, "fr_hubeau_meta_source") <- "live"
  }

  # --------------------------- finishing touches ------------------------------
  both$station_name_ascii <- to_ascii(normalize_utf8(both$station_name))
  both$river_ascii        <- to_ascii(normalize_utf8(both$river))

  out <- tibble::tibble(
    country            = "FR",
    provider_id        = "FR_HUBEAU",
    provider_name      = "France – Hub’Eau (Eaufrance) API",
    station_id         = both$station_id,
    station_name       = both$station_name,
    station_name_ascii = both$station_name_ascii,
    river              = both$river,
    river_ascii        = both$river_ascii,
    lat                = both$lat,
    lon                = both$lon,
    area               = both$area,             # km² when available
    altitude_station   = both$altitude_station, # gauge zero (m)
    altitude_api       = both$altitude_api,     # API referential (m)
    altitude_site      = both$altitude_site,    # site fiche (m)
    source_api         = both$source_api,
    has_discharge      = both$has_discharge,
    has_level          = both$has_level,
    has_temperature    = both$has_temperature
  )

  # propagate meta attributes to the returned tibble if we set them on `both`
  for (nm in c("fr_hubeau_meta_date","fr_hubeau_meta_source")) {
    v <- attr(both, nm, exact = TRUE)
    if (!is.null(v)) attr(out, nm) <- v
  }

  out
}





#' @export
timeseries.hydro_service_FR_HUBEAU <- function(x,
                                              parameter   = c("water_discharge","water_level","water_temperature"),
                                              stations    = NULL,
                                              start_date  = NULL,
                                              end_date    = NULL,
                                              mode        = c("complete","range"),
                                              exclude_quality = NULL,                 # kept for API parity; not used here
                                              max_stations = getOption("hydrodownloadR.max_stations", 500L),
                                              ...) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)
  rng       <- resolve_dates(mode, start_date, end_date)  # list(start, end)
  pm        <- .fr_param_map(parameter)

  # --- station universe -------------------------------------------------------
  st_all <- stations.hydro_service_FR_HUBEAU(x)
  if (nrow(st_all) == 0) {
    return(.fr_empty_ts(x, parameter, pm$unit))
  }

  if (is.null(stations) || !length(stations)) {
    if (nrow(st_all) > max_stations) {
      warning(sprintf(
        "FR_HUBEAU: %d stations available; limiting to first %d. Pass 'stations=' or increase 'max_stations=' (or set options(hydrodownloadR.max_stations=...)).",
        nrow(st_all), as.integer(max_stations)
      ))
    }
    st <- utils::head(st_all, max_stations)
  } else {
    stations <- unique(as.character(stations))
    st <- dplyr::filter(st_all, .data$station_id %in% stations)
    if (length(setdiff(stations, st$station_id))) {
      bad <- setdiff(stations, st$station_id)
      warning(
        "FR_HUBEAU: dropping unknown station ids: ",
        paste(head(bad, 5L), collapse = ", "),
        if (length(bad) > 5) sprintf(" (+%d more)", length(bad) - 5) else ""
      )
    }
  }
  if (!nrow(st)) return(.fr_empty_ts(x, parameter, pm$unit))

  # --- chunking + progress (tick by stations, not chunks) --------------------
  chunk_size <- getOption("hydrodownloadR.chunk_size", 50L)
  chunks <- split(st$station_id, ceiling(seq_along(st$station_id) / chunk_size))
  pb <- progress::progress_bar$new(
    total = nrow(st),
    format = "FR_HUBEAU [:bar] :current/:total (:percent) :eta",
    clear = FALSE, width = 70
  )

  # --- per-call rate override (fallback to x$rate_cfg) -----------------------
  dots     <- list(...)
  rate_cfg <- dots$rate_cfg %||% getOption("hydrodownloadR.rate_cfg") %||% x$rate_cfg
  limiter  <- ratelimitr::limit_rate(
    function(req) perform_request(req),
    rate = do.call(ratelimitr::rate, rate_cfg)
  )

  # --- runners per API family -------------------------------------------------
  results <- lapply(chunks, function(ids) {
    # tick by number of stations in this chunk
    on.exit(pb$tick(length(ids)), add = TRUE)
    if (pm$api == "hydrometrie") {
      if (identical(pm$ts_path, "/api/v2/hydrometrie/observations_tr")) {
        .do_hm_rt_chunk(x, ids, pm, rng, limiter)
      } else {
        .do_hm_elab_chunk(x, ids, pm, rng, limiter)
      }
    } else {
      .do_temp_chunk(x, ids, pm, rng, limiter)
    }
  })

  out <- dplyr::bind_rows(results)
  if (!nrow(out)) return(.fr_empty_ts(x, parameter, pm$unit))
  out
}

# ---------- helpers: empty tibble ---------------------------------------------
.fr_empty_ts <- function(x, parameter, unit) {
  tibble::tibble(
    country        = "FR",
    provider_id    = "FR_HUBEAU",
    provider_name  = "France – Hub’Eau (Eaufrance) API",
    station_id     = character(),
    parameter      = parameter,
    timestamp      = as.POSIXct(character(), tz = "UTC"),
    value          = double(),
    unit           = unit,
    quality_code   = NA_character_,
    vertical_datum = NA_character_,
    source_url     = character()
  )
}

# ---------- hydrométrie realtime (H) with cursor ------------------------------
.do_hm_rt_chunk <- function(x, ids, pm, rng, limiter) {

  # optional fallback for vertical datum from precomputed meta (if present)
  vdatum_fallback <- local({
    meta <- .fr_load_precomputed_meta()
    if (is.null(meta) || !"vertical_datum_site" %in% names(meta$tab)) {
      structure(character(), names = character())
    } else {
      tab <- meta$tab
      stats::setNames(as.character(tab$vertical_datum_site), as.character(tab$station_id))
    }
  })

  fetch_one <- function(st_id) {
    q    <- c(list(code_entite = st_id, size = 5000L), pm$fixed_qs)
    path <- pm$ts_path
    req  <- build_request(x, path = path, query = q)
    res  <- limiter(req)
    url_used <- .fr_used_url(res, req)

    p   <- .fr_parse_json(res)
    dat <- if (!is.null(p)) p[["data"]] else NULL
    all <- list()
    if (length(dat)) all[[1]] <- dat
    cur <- if (!is.null(p)) p[["next"]] else NULL

    # Follow 'next' verbatim (avoid double-encoding)
    while (!is.null(cur) && nzchar(cur)) {
      res2 <- .fr_try_req(function() httr::GET(cur, httr::timeout(20)))   # GET the next URL as-is
      p2   <- tryCatch(jsonlite::fromJSON(httr::content(res2, as = "text", encoding = "UTF-8"), simplifyVector = TRUE),
                       error = function(e) NULL)
      d2   <- if (!is.null(p2)) p2[["data"]] else NULL
      if (!length(d2)) break
      all[[length(all) + 1L]] <- d2
      cur <- p2[["next"]]
    }

    if (!length(all)) return(NULL)
    df <- tibble::as_tibble(dplyr::bind_rows(all))

    # Keep full time (H:M:S)
    df <- dplyr::mutate(
      df,
      ts  = as.POSIXct(.data$date_obs, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      val = suppressWarnings(as.numeric(.data$resultat_obs)),
      q_fr = col_or_null(df, "libelle_qualification_obs")
    )
    df <- df[!is.na(df$ts) &
               df$ts >= as.POSIXct(rng$start) &
               df$ts <= as.POSIXct(rng$end), , drop = FALSE]
    if (!nrow(df)) return(NULL)

    # Vertical datum only for water level (H)
    vd_col  <- "code_systeme_alti_serie"
    vd_site <- unname(vdatum_fallback[st_id])
    if (!length(vd_site)) vd_site <- NA_character_
    vdatum  <- if (vd_col %in% names(df)) as.character(df[[vd_col]]) else rep(vd_site, nrow(df))

    tibble::tibble(
      country        = "FR",
      provider_id    = "FR_HUBEAU",
      provider_name  = "France – Hub’Eau (Eaufrance) API",
      station_id     = df$code_station %||% st_id,
      parameter      = "water_level",
      timestamp      = df$ts,
      value          = pm$convert(df$val),   # mm -> cm
      unit           = pm$unit,              # "cm"
      quality_code   = df$q_fr,              # keep FR label (as agreed)
      vertical_datum = vdatum,
      source_url     = url_used
    )
  }

  dplyr::bind_rows(lapply(ids, fetch_one))
}

# ---------- hydrométrie daily elaborated (QmnJ) -------------------------------
.do_hm_elab_chunk <- function(x, ids, pm, rng, limiter) {
  fetch_one <- function(st_id) {
    size <- 20000L
    acc  <- list()
    next_date <- as.Date(rng$start)

    repeat {
      q <- c(
        list(
          code_entite         = st_id,
          size                = size,
          date_debut_obs_elab = format(next_date, "%Y-%m-%d"),
          date_fin_obs_elab   = format(as.Date(rng$end), "%Y-%m-%d")
        ),
        pm$fixed_qs
      )

      req <- build_request(x, path = pm$ts_path, query = q)
      res <- .fr_try_req(function() limiter(req))
      url_used <- .fr_used_url(res, req)

      p   <- .fr_parse_json(res)
      dat <- if (!is.null(p)) p[["data"]] else NULL
      if (is.null(dat) || !length(dat)) break

      df <- tibble::as_tibble(dat)
      n  <- nrow(df)
      if (!n) break

      acc[[length(acc) + 1L]] <- df

      if (n == size) {
        # advance from last date returned
        last_date <- tryCatch(as.Date(df$date_obs_elab[n]), error = function(e) NA)
        if (is.na(last_date)) break
        next_date <- last_date + 1
        next
      } else {
        break
      }
    }

    if (!length(acc)) return(NULL)
    out <- dplyr::bind_rows(acc)

    out <- dplyr::mutate(
      out,
      ts  = as.POSIXct(.data$date_obs_elab, tz = "UTC"),
      val = suppressWarnings(as.numeric(.data$resultat_obs_elab))
    )
    out <- out[!is.na(out$ts) &
                 out$ts >= as.POSIXct(rng$start) &
                 out$ts <= as.POSIXct(rng$end), , drop = FALSE]
    if (!nrow(out)) return(NULL)

    # Dedup boundary if we overlapped a day at split
    out <- dplyr::distinct(out, .data$ts, .keep_all = TRUE)

    tibble::tibble(
      country        = "FR",
      provider_id    = "FR_HUBEAU",
      provider_name  = "France – Hub’Eau (Eaufrance) API",
      station_id     = out$code_station %||% st_id,
      parameter      = "water_discharge",
      timestamp      = out$ts,
      value          = pm$convert(out$val),     # l/s -> m^3/s
      unit           = pm$unit,                 # "m^3/s"
      quality_code   = col_or_null(out, "libelle_qualification"),
      vertical_datum = NA_character_,
      source_url     = url_used
    )
  }

  dplyr::bind_rows(lapply(ids, fetch_one))
}

# ---------- temperature (v1) --------------------------------------------------
.do_temp_chunk <- function(x, ids, pm, rng, limiter) {
  fetch_one <- function(st_id) {
    q <- list(
      code_station = st_id,
      date_debut   = format(as.Date(rng$start), "%Y-%m-%d"),
      date_fin     = format(as.Date(rng$end),   "%Y-%m-%d"),
      size         = 20000L
    )
    req <- build_request(x, path = pm$ts_path, query = q)
    res <- .fr_try_req(function() limiter(req))
    url_used <- .fr_used_url(res, req)

    p   <- .fr_parse_json(res)
    dat <- if (!is.null(p)) p[["data"]] else NULL
    if (is.null(dat) || !length(dat)) return(NULL)

    df <- tibble::as_tibble(dat)

    # build POSIXct timestamp (UTC) from available fields
    if ("date_mesure" %in% names(df) && any(grepl("T", df$date_mesure, fixed = TRUE))) {
      # ISO8601 already present, e.g. "2009-08-10T00:12:00Z"
      ts <- as.POSIXct(df$date_mesure, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    } else {
      # Combine date + time columns: "date_mesure_temp" + "heure_mesure_temp"
      d <- as.character(df[["date_mesure_temp"]] %||% NA_character_)
      h <- as.character(df[["heure_mesure_temp"]] %||% NA_character_)

      # default missing/empty times to midnight
      h[is.na(h) | !nzchar(h)] <- "00:00:00"

      # some APIs return "H:MM:SS" -> left-pad to "HH:MM:SS"
      h <- sub("^([0-9]):", "0\\1:", h)

      ts <- as.POSIXct(paste(d, h), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    }

    df <- df %>%
      dplyr::mutate(
        ts  = ts,
        val = suppressWarnings(as.numeric(.data$resultat))
      ) %>%
      dplyr::filter(
        !is.na(.data$ts),
        .data$ts >= as.POSIXct(rng$start, tz = "UTC"),
        .data$ts <= as.POSIXct(rng$end,   tz = "UTC")
      )


    if (!nrow(df)) return(NULL)

    tibble::tibble(
      country        = "FR",
      provider_id    = "FR_HUBEAU",
      provider_name  = "France – Hub’Eau (Eaufrance) API",
      station_id     = df$code_station %||% st_id,
      parameter      = "water_temperature",
      timestamp      = df$ts,
      value          = pm$convert(df$val),
      unit           = pm$unit,
      quality_code   = df$libelle_qualification,
      vertical_datum = NA_character_,
      source_url     = url_used
    )
  }

  dplyr::bind_rows(lapply(ids, fetch_one))
}
