# Create a hydro service object

Create a hydro service object

## Usage

``` r
hydro_service(provider_id, ...)
```

## Arguments

- provider_id:

  ID as listed by
  [`hydro_services()`](https://bafg-bund.github.io/hydrodownloadR/reference/hydro_services.md)

- ...:

  Reserved for future use.

## Value

An object of class `"hydro_service"` (a list) containing the provider
configuration used by
[`stations()`](https://bafg-bund.github.io/hydrodownloadR/reference/stations.md)
and
[`timeseries()`](https://bafg-bund.github.io/hydrodownloadR/reference/timeseries.md)
(e.g. provider_id, provider_name, country, base_url, and other
adapter-specific settings).
