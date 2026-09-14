# Japan MLIT stations metadata snapshot

A processed station metadata table used internally by the JP MLIT
adapter. Coordinates from the official MLIT station pages are preferred
when available. Coordinates from the supplied metadata CSV are used as a
fallback.

## Usage

``` r
data(jp_mlit_meta)
```

## Format

A tibble/data.frame with one row per station:

- station_id:

  MLIT station identifier (character).

- station_name:

  English station name (character).

- river:

  English river-system name (character).

- lat:

  Selected latitude in WGS84 decimal degrees (double).

- lon:

  Selected longitude in WGS84 decimal degrees (double).

- area:

  Drainage area in square kilometres, if available (double).

- altitude:

  Gauge-zero elevation in metres, if available (double).

- station_name_original:

  Original Japanese station name (character).

- lat_website:

  Latitude obtained from the official MLIT station page (double).

- lon_website:

  Longitude obtained from the official MLIT station page (double).

- coordinate_source:

  Source used for `lat` and `lon`. Either `"MLIT website"` or
  `"MLIT metadata CSV"` (character).

## Source

Japan Ministry of Land, Infrastructure, Transport and Tourism (MLIT).
