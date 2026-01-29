# Japan MLIT stations metadata snapshot

A tibble used by the JP MLIT adapter to speed up station discovery.

## Usage

``` r
data(jp_mlit_meta)
```

## Format

A tibble/data.frame with one row per station and typical columns:

- station_id:

  MLIT station identifier (character)

- station_name:

  Station name (character)

- river:

  River name, if available (character)

- lat:

  Latitude in WGS84 (double)

- lon:

  Longitude in WGS84 (double)

- area_km2:

  Drainage area in km^2, if available (double)

- altitude_m:

  Altitude in meters, if available (double)

- country:

  ISO country code (character)

- provider_id:

  Adapter provider id, e.g. `"JP_MLIT"` (character)

- provider_name:

  Provider name (character)

## Source

MLIT; see package README for licensing.
