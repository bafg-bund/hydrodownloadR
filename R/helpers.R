#' Resolve date inputs based on mode
#' @keywords internal
resolve_dates <- function(mode, start_date, end_date) {
  mode <- match.arg(mode, c("complete", "range"))
  if (mode == "complete") {
    start_date <- as.Date("1900-01-01")
    end_date   <- Sys.Date()
  } else {
    if (is.null(start_date) || is.null(end_date))
      rlang::abort("For mode='range' both start_date and end_date are required as YYYY-MM-DD format.")
    start_date <- as.Date(start_date)
    end_date   <- as.Date(end_date)
  }
  list(start_date = start_date, end_date = end_date)
}

#' Split a vector into chunks
#' @keywords internal
chunk_vec <- function(x, size) {
  if (length(x) == 0) return(list())
  split(x, ceiling(seq_along(x) / size))
}

#' Normalize likely-misencoded UTF-8 text (fix common "Ã…" mojibake)
#' @keywords internal
normalize_utf8 <- function(x) {
  if (is.null(x)) return(x)
  y  <- enc2utf8(as.character(x))
  needs_fix <- grepl("[ÃÂ][[:alnum:]]", y)
  if (any(needs_fix)) {
    y2 <- suppressWarnings(iconv(as.character(x), from = "latin1", to = "UTF-8"))
    y[needs_fix & !is.na(y2)] <- y2[needs_fix & !is.na(y2)]
  }
  y
}

#' Transliterate to ASCII (remove diacritics); fallback to original on failure
#' @keywords internal
to_ascii <- function(x) {
  if (is.null(x)) return(x)
  y <- suppressWarnings(iconv(as.character(x), to = "ASCII//TRANSLIT"))
  y[is.na(y)] <- as.character(x)[is.na(y)]
  y
}

#' Null-coalescing operator alias (rlang)
#' @keywords internal
`%||%` <- rlang::`%||%`

# Optional disk cache for plain GET-JSON endpoints (not used by httr2 flow)
#' @keywords internal
.cache <- cachem::cache_disk(dir = tools::R_user_dir("hydrodownloadR","cache"))

memo_json_get <- memoise::memoise(function(url) {
  jsonlite::fromJSON(url, simplifyVector = TRUE)
}, cache = .cache)

#' Safe column extract that doesn't warn on missing tibble columns
#' @keywords internal
col_or_null <- function(df, col) {
  if (is.null(df) || is.null(col)) return(NULL)
  nms <- tryCatch(names(df), error = function(e) NULL)
  if (is.null(nms) || !(col %in% nms)) return(NULL)
  df[[col]]
}

#' Parse area in km^2 from a free-text description (handles Danish decimal comma)
#' Examples: "Opland = 189,47 km2", "Area 12.3 km²"
#' Vectorized; returns numeric (km^2)
#' @keywords internal
parse_area_km2 <- function(x) {
  if (is.null(x)) return(NA_real_)
  x <- as.character(x)
  out <- rep(NA_real_, length(x))

  # 1) Primary path: take everything after the first "="
  has_eq <- grepl("=", x, fixed = TRUE)
  if (any(has_eq)) {
    rhs <- sub("^[^=]*=\\s*", "", x[has_eq])     # keep text after "="
    # remove trailing 'km', 'km2', 'km^2', 'km²' (case-insensitive), and spaces
    rhs <- sub("\\s*km(?:\\s*\\^?2|\\s*²)?\\s*$", "", rhs, ignore.case = TRUE, perl = TRUE)
    # keep only the first number (supports thousand groups and comma/period decimal)
    num <- sub("^.*?([0-9]+(?:[ .][0-9]{3})*(?:[\\.,][0-9]+)?).*$", "\\1", rhs, perl = TRUE)
    # drop spaces used as thousand separators, normalize comma decimals
    num <- gsub(" ", "", num, fixed = TRUE)
    num <- gsub(",", ".", num, fixed = TRUE)
    out[has_eq] <- suppressWarnings(as.numeric(num))
  }

  # 2) Fallback: find number immediately before a 'km' token
  need_fallback <- is.na(out)
  if (any(need_fallback)) {
    y <- x[need_fallback]
    m <- regexpr("([0-9]+(?:[ .][0-9]{3})*(?:[\\.,][0-9]+)?)\\s*km(?:\\^?2|²)?",
                 y, ignore.case = TRUE, perl = TRUE)
    hit <- m != -1L
    if (any(hit, na.rm = T)) {
      s   <- regmatches(y[hit], m[hit])
      num <- sub("^.*?([0-9]+(?:[ .][0-9]{3})*(?:[\\.,][0-9]+)?).*$", "\\1", s, perl = TRUE)
      num <- gsub(" ", "", num, fixed = TRUE)
      num <- gsub(",", ".", num, fixed = TRUE)
      out[which(need_fallback)[hit]] <- suppressWarnings(as.numeric(num))
    }
  }

  out
}
