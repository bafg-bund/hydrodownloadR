# Package load hook: register all providers here
.onLoad <- function(libname, pkgname) {
  register_EE_EST()
  # register_XX_YY() for other providers...
}
