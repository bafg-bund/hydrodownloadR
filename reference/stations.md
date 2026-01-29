# List stations for a provider

List stations for a provider

## Usage

``` r
stations(x, ...)
```

## Arguments

- x:

  A `hydro_service` object created by
  [`hydro_service()`](https://bafg-bund.github.io/hydrodownloadR/reference/hydro_service.md).

- ...:

  Passed to provider-specific methods.

## Value

A tibble with station metadata.

## Examples

``` r
# Offline: enumerate providers (no network)
s <- hydro_services()
head(names(s))
#> [1] "provider_id"   "provider_name" "country"       "base_url"     
#> [5] "license"       "license_link" 

if (FALSE) { # interactive() && requireNamespace("curl", quietly = TRUE) && curl::has_internet() && identical(Sys.getenv("HYDRO_EXAMPLES"), "true")
# Online (opt-in): fetch stations
x <- hydro_service("SE_SMHI")
st <- stations(x)
head(st)
}
```
