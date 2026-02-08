# hydrodownloadR 0.1.2
* AT water level unit corrected 
* AU ts_name is now DMQaQc.Merged.DailyMean.24HR instead of 09HR
* BA adapter completed
* BE_XXX Normalize range to POSIXct (UTC) to avoid POSIXct–Date comparison warnings
* across platforms (incl. HPC nodes) and to include the full end day.
* CH adding missing water quality parameter
* DK fix force HTTP/1.1 for timeseries requests
* US_USGS_XXX unit conversion (area and altitude)
* including Spain CEDEX adapter


# hydrodownloadR 0.1.1
* Fix HTML manual validation for pl_imgw_meta documentation.
* Minor DESCRIPTION wording updates.

# hydrodownloadR 0.1.0

* Initial internal release.
* Registry + S3 architecture (`stations()`, `timeseries()`).
* Rate limiting, retries, UTF-8→ASCII normalization.
* WGS84 coordinate transforms (via {sf}).
