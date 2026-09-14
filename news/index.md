# Changelog

## hydrodownloadR 0.1.4

### New data sources

- Added support for hydrological data providers in Afghanistan,
  Colombia, Israel, Mexico, Peru, and Portugal.
- Added water-quality data support for Switzerland.

### Improvements

- Japan MLIT station coordinates are now retrieved from the official
  MLIT website, with packaged metadata used as a fallback.
- Switzerland now uses the official BAFU API at data.bafu.admin.ch.
- Added station identifiers to time-series output.
- Added `quality_name` and `quality_desc` to time-series output for
  improved consistency across providers.
- Chile now supports flexible date ranges.
- Improved database update handling for Canada.
- Updated provider base URLs for Austria and Finland.
- Improved rate limiting for Brazil and fixed the `mdbtools` fallback on
  macOS.
- Adapted the Netherlands provider to API changes.
- Restricted the Denmark HTTP/1.1 workaround to Unix/HPC systems.
- Improved dynamic provider registry handling.

### Bug fixes

- Corrected Poland timestamps affected by hydrological-year handling.
- Fixed an export URL.

## hydrodownloadR 0.1.3

CRAN release: 2026-02-25

- add script for US_USGS_XXX that builds compact USGS station metadata
  bundle for hydrodownloadRdata release asset
- update USGS metadata cache
- dynamical country name as in register correction of open_data usage of
  BA_AVPS
- change elevation to altitude due to consistency
- SE SMHI altitude as numeric
- Consistency in data structure station_id of AR_INA as character
- Consistency in data structure area of EE_EST as numeric
- Same caching of PL_IMGW as other adapters

## hydrodownloadR 0.1.2

- AT water level unit corrected
- AU ts_name is now DMQaQc.Merged.DailyMean.24HR instead of 09HR
- BA adapter completed
- BE_XXX Normalize range to POSIXct (UTC) to avoid POSIXct–Date
  comparison warnings
- across platforms (incl. HPC nodes) and to include the full end day.
- CH adding missing water quality parameter
- DK fix force HTTP/1.1 for timeseries requests
- US_USGS_XXX unit conversion (area and altitude)
- including Spain CEDEX adapter

## hydrodownloadR 0.1.1

- Fix HTML manual validation for pl_imgw_meta documentation.
- Minor DESCRIPTION wording updates.

## hydrodownloadR 0.1.0

- Initial internal release.
- Registry + S3 architecture
  ([`stations()`](https://bafg-bund.github.io/hydrodownloadR/reference/stations.md),
  [`timeseries()`](https://bafg-bund.github.io/hydrodownloadR/reference/timeseries.md)).
- Rate limiting, retries, UTF-8→ASCII normalization.
- WGS84 coordinate transforms (via {sf}).
