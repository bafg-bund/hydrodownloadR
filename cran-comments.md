# hydrodownloadR 0.1.2

* AT water level unit corrected 
* AU ts_name is now DMQaQc.Merged.DailyMean.24HR instead of 09HR
* BA adapter completed
* BE_XXX Normalize range to POSIXct (UTC) to avoid POSIXct–Date comparison warnings
* across platforms (incl. HPC nodes) and to include the full end day.
* CH adding missing water quality parameter
* DK fix force HTTP/1.1 for timeseries requests
* US_USGS_XXX unit conversion (area and altitude)

including Spain CEDEX adapter



# hydrodownloadR 0.1.1
* Fix HTML manual validation issue in pl_imgw_meta documentation.
* Declare minimum R version (>= 4.1.0) due to native pipe/lambda usage.

## R CMD check results
0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time

  This note occurs only in our restricted network environment; the same check is OK
  on win-builder (R-devel, Windows).
