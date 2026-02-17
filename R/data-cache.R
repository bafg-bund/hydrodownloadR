# R/data-cache.R
ensure_usgs_meta <- function(cache_dir = NULL) {
  if (is.null(cache_dir) || !nzchar(cache_dir)) {
    cache_dir <- rappdirs::user_cache_dir("hydrodownloadR", "bafg-bund")
  }
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  rds_path <- file.path(cache_dir, "us_usgs_stations_meta.rds")

  if (!file.exists(rds_path)) {
    if (!requireNamespace("curl", quietly = TRUE)) {
      stop("Package 'curl' is required to download USGS metadata bundle.")
    }
    if (!curl::has_internet()) {
      stop("No internet; set options(hydrodownloadR.usgs_meta_url=...) to a local file or pre-place the RDS in cache_dir")
    }

    url <- getOption(
      "hydrodownloadR.usgs_meta_url",
      "https://github.com/bafg-bund/hydrodownloadRdata/releases/download/v0.1/us_usgs_stations_meta.zip"
    )

    ext <- tolower(sub(".*\\.", "", basename(url)))
    tmp <- tempfile(fileext = paste0(".", ext))
    curl::curl_download(url, tmp, mode = "wb")

    if (ext == "zip") {
      files <- utils::unzip(tmp, exdir = cache_dir)
      rds_in_zip <- files[grepl("\\.rds$", files, ignore.case = TRUE)]
      if (length(rds_in_zip) != 1L) stop("ZIP must contain exactly one .rds file")
      ok <- file.rename(rds_in_zip, rds_path)
      if (!ok) file.copy(rds_in_zip, rds_path, overwrite = TRUE)
    } else if (ext %in% c("rds", "xz", "gz")) {
      file.copy(tmp, rds_path, overwrite = TRUE)
    } else {
      stop("Unsupported asset extension: ", ext)
    }
  }

  obj <- readRDS(rds_path)
  attr(obj, "cache_path") <- rds_path
  attr(obj, "source_url") <- getOption(
    "hydrodownloadR.usgs_meta_url",
    "https://github.com/bafg-bund/hydrodownloadRdata/releases/download/v0.1/us_usgs_stations_meta.zip"
  )
  obj
}
