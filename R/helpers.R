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

#' Normalize likely-misencoded UTF-8 text (fix common UTF-8 mojibake)
#' @keywords internal
normalize_utf8 <- function(x) {
  if (is.null(x)) return(x)
  y  <- enc2utf8(as.character(x))
  y_chr    <- as.character(y)
  needs_fix <- grepl("[\u00C2\u00C3][\u0080-\u00FF]", y_chr, perl = TRUE)
  needs_fix[is.na(y_chr)] <- FALSE
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

# ASCII-safe km^2 parser (PCRE2 compatible)
# Examples it understands: "1,00 km2", "12.345,67 km^2", "Area = 3.5 km²"
parse_area_km2 <- function(x) {
  if (is.null(x)) return(NA_real_)
  x <- as.character(x)
  out <- rep(NA_real_, length(x))

  num_re <- "([0-9]+(?:[ .][0-9]{3})*(?:[\\.,][0-9]+)?)"
  km2_suffix <- "\\s*km(?:\\^?2|\\x{00B2})?"   # NOTE: \\x{00B2} (PCRE2)

  # Path 1: text after "=" then strip trailing km^2 token
  has_eq <- grepl("=", x, fixed = TRUE)
  if (any(has_eq)) {
    rhs <- sub("^[^=]*=\\s*", "", x[has_eq], perl = TRUE)
    rhs <- sub(paste0(km2_suffix, "\\s*$"), "", rhs,
               ignore.case = TRUE, perl = TRUE)
    num <- sub(paste0("^.*?", num_re, ".*$"), "\\1", rhs, perl = TRUE)
    num <- gsub(" ", "", num, fixed = TRUE)
    num <- gsub(",", ".", num, fixed = TRUE)
    out[has_eq] <- suppressWarnings(as.numeric(num))
  }

  # Path 2: number immediately before a km^2 token
  need <- is.na(out)
  if (any(need)) {
    y <- x[need]
    pat <- paste0(num_re, km2_suffix)
    hit <- grepl(pat, y, ignore.case = TRUE, perl = TRUE)
    if (any(hit, na.rm = TRUE)) {
      z <- y[hit]
      num <- sub(paste0("^.*?", pat, ".*$"), "\\1", z,
                 ignore.case = TRUE, perl = TRUE)
      num <- gsub(" ", "", num, fixed = TRUE)
      num <- gsub(",", ".", num, fixed = TRUE)
      out[which(need)[hit]] <- suppressWarnings(as.numeric(num))
    }
  }

  out
}

#' @importFrom rlang %||%
NULL
