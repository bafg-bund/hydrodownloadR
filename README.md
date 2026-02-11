
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hydrodownloadR

<!-- badges: start -->

[![R-CMD-check](https://github.com/bafg-bund/hydrodownloadR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bafg-bund/hydrodownloadR/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

**hydrodownloadR** provides a unified, extensible interface for
discovering hydrological stations and downloading daily time series
(e.g., water discharge, water level, water temperature, and several
other water quality parameters) from national/regional public APIs. The
package uses a provider registry with identifiers (`provider_id`,
`provider_name`) and S3 generics `stations()` / `timeseries()`. It
supports complete histories (1900-01-01 until today), per-station
selection, rate limiting and retries, optional authentication via
environment variables, UTF-8 to ASCII normalization, and coordinate
transformation to WGS84.

> Built for reproducible workflows and straightforward addition of new
> providers.

**Acknowledgements.** The repository structure is inspired by Ryan
Riggs’ [**RivRetrieve**](https://github.com/Ryan-Riggs/RivRetrieve).
Thanks to **Frederik Kratzert** (co-author of [GRDC-Caravan
paper](doi:10.5194/essd-17-4613-2025)) and **Thiago Nascimento** for
helpful exchanges and for maintaining and porting the RivRetrieve
concept to Python in
[**RivRetrieve-Python**](https://github.com/kratzert/RivRetrieve-Python).

------------------------------------------------------------------------

## Why this package exists

This package is developed and used at the Global Runoff Data Centre
(GRDC, BfG) as part of reproducible workflows to discover stations and
retrieve update time series from public APIs. It is also suitable for
other global data centres hosted by BfG (Federal Institute of Hydrology)
that rely on consistent, auditable data access and update pipelines.

------------------------------------------------------------------------

## Installation

### CRAN version

``` r
install.packages("hydrodownloadR")
```

### Development version (GitHub)

``` r
# Option A: remotes
install.packages("remotes")
remotes::install_github("bafg-bund/hydrodownloadR")

# Option B: pak (fast)
install.packages("pak")
pak::pak("bafg-bund/hydrodownloadR")
```

------------------------------------------------------------------------

## Quick start

List available providers:

``` r
library(hydrodownloadR)

hs <- hydro_services()
hs
```

`hs` includes an overview of providers and (where available) licensing /
terms information to help users understand access and reuse conditions.

Select one provider and list stations:

``` r
x <- hydro_service(hs$provider_id[1])
stn <- stations(x)
stn
```

Check which time series parameters are available for this provider:

``` r
timeseries_parameters(x)
```

Download a daily time series for one station:

``` r
ts <- timeseries(
  x,
  stations  = stn$station_id[1],
  parameter = "water_discharge",
  mode      = "complete"
)
ts
```

------------------------------------------------------------------------

## Provider-specific options and “complete history”

Upstream APIs differ. Some providers expose additional options such as
authentication, quality flags, paging strategies, or “complete history”
modes. These options may evolve as providers change their APIs. For best
results:

-   Start with defaults.
-   Check the help pages: `?hydro_service`, `?stations`, `?timeseries`,
    `?timeseries_parameters`.
-   Review provider-specific notes (when linked from the provider
    registry).

Tip: if an adapter exposes a flag that must be explicitly enabled (for
example, a complete-history mode), set it explicitly in your scripts so
results remain reproducible and robust to upstream changes.

------------------------------------------------------------------------

## Licensing and terms of use

This package provides technical access to data services. Data access and
reuse are governed by each provider’s terms and licensing conditions.
Please review and comply with those terms and licensing conditions when
using downloaded data.

------------------------------------------------------------------------

## Contributing and support

Contributions are welcome:

-   Use GitHub Issues for bug reports, feature requests, and new
    provider requests.
-   If you report a bug, please include provider_id, station_id (if
    relevant), parameter, time range, a minimal reproducible snippet,
    and `sessionInfo()`.

Pull requests are welcome. Please ensure CI is green (R CMD check) and
keep new code ASCII-friendly where possible.

------------------------------------------------------------------------

## Development note (AI assistance)

Portions of this project were drafted with AI assistance (ChatGPT) and
then reviewed, edited, and tested by the maintainer. All changes are
maintained via version control and validated with automated checks.
