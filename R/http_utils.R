#' Build a httr2 request with optional auth
#' @keywords internal
build_request <- function(x, path, query = list(), headers = list()) {
  req <- httr2::request(x$base_url) |>
    httr2::req_user_agent("hydrodownloadR (+https://github.com/your-org/hydrodownloadR)") |>
    httr2::req_url_path_append(path) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_headers(!!!headers)

  if (!is.null(x$auth) && !is.null(x$auth$type) && x$auth$type != "none") {
    if (x$auth$type == "token_env") {
      token <- Sys.getenv(x$auth$var, "")
      if (identical(token, "")) rlang::abort(paste0("Missing token in ENV: ", x$auth$var))
      req <- httr2::req_auth_bearer_token(req, token)
    }
    # add more auth types here if needed...
  }
  req
}

#' Perform request with retry and exponential backoff
#' @keywords internal
perform_request <- function(req, max_tries = 5) {
  req |>
    httr2::req_retry(
      max_tries = max_tries,
      is_transient = function(resp) {
        status <- httr2::resp_status(resp)
        status >= 500 || status %in% c(408, 429)
      },
      backoff = ~ min(60, 2 ^ (.x - 1))
    ) |>
    httr2::req_perform()
}

#' Ensure HTTP status 200 (useful for auth pings)
#' @keywords internal
#' @noRd
check_status_200 <- function(resp) {
  if (httr2::resp_status(resp) != 200) {
    rlang::abort(paste0("HTTP status is not 200: ", httr2::resp_status(resp)))
  }
  invisible(TRUE)
}
