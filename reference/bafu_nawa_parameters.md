# Retrieve the complete BAFU NAWA Trend parameter catalogue

The catalogue includes all physicochemical parameters, nutrients,
micropollutants and the few hydrological parameters stored in NAWA. The
`parameter` column shows how each provider parameter is handled by
[`timeseries()`](https://bafg-bund.github.io/hydrodownloadR/reference/timeseries.md).
All entries classified as `water_quality` are returned together when
`parameter = "water_quality"` is requested.

## Usage

``` r
bafu_nawa_parameters(x, ...)
```

## Arguments

- x:

  A `CH_BAFU_NAWA` hydro service.

- ...:

  Unused.

## Value

A tibble containing the provider's parameter catalogue.
