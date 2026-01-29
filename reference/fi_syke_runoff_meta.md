# SYKE runoff station metadata (area & altitude)

Catchment area and altitude for Finnish SYKE runoff stations. Area may
be `NA` for a few stations; altitude may still be present. Used to
compute discharge from runoff time series:
`discharge_m3s = (value_lps_per_km2 * area_km2) / 1000`.

## Usage

``` r
data(fi_syke_runoff_meta)
```

## Format

A tibble with:

- place_id:

  Character. SYKE Paikka_Id.

- area:

  Numeric (km^2). May be NA.

- altitude:

  Numeric (m). May be NA.

## Source

Finnish Environment Institute (SYKE).
