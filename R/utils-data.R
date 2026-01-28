# R/utils-data.R (internal)
.pkg_data <- function(name) {
  obj <- get0(name, inherits = TRUE)
  if (!is.null(obj)) return(obj)
  # During devtools/roxygen, lazydata may not be preloaded - try explicit load:
  try(utils::data(list = name, package = utils::packageName(), envir = environment()), silent = TRUE)
  get0(name, inherits = TRUE)
}
