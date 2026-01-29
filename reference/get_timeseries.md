# Convenience: fetch time series by provider_id

Convenience: fetch time series by provider_id

(Convenience) Get time series by provider_id

## Usage

``` r
get_timeseries(
  provider_id,
  parameter,
  stations = NULL,
  start_date = NULL,
  end_date = NULL,
  mode = c("range", "complete"),
  ...
)

get_timeseries(
  provider_id,
  parameter,
  stations = NULL,
  start_date = NULL,
  end_date = NULL,
  mode = c("range", "complete"),
  ...
)
```

## Arguments

- provider_id:

  Provider identifier, e.g. "DK_VANDA".

- parameter:

  One of
  "water_discharge","water_level","water_temperature","water_velocity".

- stations:

  Optional character vector of station IDs.

- start_date, end_date:

  `YYYY-MM-DD` strings for `mode = "range"`.

- mode:

  Either `"range"` or `"complete"` (`1900-01-01` to today).

- ...:

  Passed to provider-specific methods.
