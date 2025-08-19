#' Resolve date inputs based on mode
#' @keywords internal
resolve_dates <- function(mode, start_date, end_date) {
  mode <- match.arg(mode)
  if (mode == "complete") {
    start_date <- as.Date("1900-01-01")
    end_date   <- Sys.Date()
  } else {
    if (is.null(start_date) || is.null(end_date))
      rlang::abort("For mode='range' both start_date and end_date are required.")
    start_date <- as.Date(start_date); end_date <- as.Date(end_date)
  }
  list(start_date = start_date, end_date = end_date)
}

#' Split a vector into chunks
#' @keywords internal
chunk_vec <- function(x, size) {
  if (length(x) == 0) return(list())
  split(x, ceiling(seq_along(x) / size))
}

# Disk cache for GET-JSON (optional helper)
#' @keywords internal
.cache <- cachem::cache_disk(dir = tools::R_user_dir("hydrodownloadR","cache"))

memo_json_get <- memoise::memoise(function(url) {
  jsonlite::fromJSON(url, simplifyVector = TRUE)
}, cache = .cache)
