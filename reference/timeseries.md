# Retrieve time series for a provider

Retrieve time series for a provider

## Usage

``` r
timeseries(
  x,
  parameter,
  stations = NULL,
  start_date = NULL,
  end_date = NULL,
  mode = c("range", "complete"),
  ...
)
```

## Arguments

- x:

  A `hydro_service` object created by
  [`hydro_service()`](https://bafg-bund.github.io/hydrodownloadR/reference/hydro_service.md).

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

## Value

A tibble with columns: country, provider_id, provider_name, station_id,
parameter, timestamp, value, unit, quality_code, source_url.

## Examples

``` r
# Offline: construct a service object (no network)
x <- hydro_service("SE_SMHI")

if (FALSE) { # interactive() && requireNamespace("curl", quietly = TRUE) && curl::has_internet() && identical(Sys.getenv("HYDRO_EXAMPLES"), "true")
# Online (opt-in): one station for a short range
st <- head(stations(x)$station_id, 1)
ts <- timeseries(x, parameter = "water_discharge",
                 stations = st,
                 start_date = "2020-01-01", end_date = "2020-01-10")
head(ts)
}
```
