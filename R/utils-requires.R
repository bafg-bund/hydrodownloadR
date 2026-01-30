.need_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      sprintf("Package '%s' is required for this feature. Install it with install.packages('%s').", pkg, pkg),
      call. = FALSE
    )
  }
}
