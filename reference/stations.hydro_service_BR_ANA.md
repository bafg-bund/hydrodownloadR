# ANA stations (Brazil) - cache-first with optional update from inventory

Loads the cached ANA station catalogue (if present) or rebuilds it from
a locally downloaded SNIRH inventory (`InventarioDD_MM_YYYY.zip` /
`.mdb`) when `update = TRUE`.

## Usage

``` r
# S3 method for class 'hydro_service_BR_ANA'
stations(x, ...)
```

## Arguments

- x:

  A `hydro_service` created with `hydro_service("BR_ANA")`.

- ...:

  Named arguments:

  - `zip_or_mdb`: path to `InventarioDD_MM_YYYY.zip` or `.mdb`

  - `dest_dir`: unzip destination (default: `"data-raw/BR_ANA"`)

  - `cache_dir`: cache dir for RDS (default: user cache)

  - `update`: `TRUE` to rebuild from provided inventory

## Value

A tibble with ANA station metadata.
