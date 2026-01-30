
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hydrodownloadR

<!-- badges: start -->

[![R-CMD-check](https://github.com/bafg-bund/hydrodownloadR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bafg-bund/hydrodownloadR/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**hydrodownloadR** provides a unified, extensible interface for
discovering hydrological stations and downloading daily time series
(water discharge, water level, water temperature, flow velocity) from
national/regional public APIs. The package uses a provider registry with
identifiers (`provider_id`, `provider_name`) and S3 generics
`stations()` / `timeseries()`. It supports complete histories
(1900-01-01 until today), per-station selection, rate limiting & retries,
optional auth via environment variables, UTF-8 to ASCII normalization, and
coordinate transformation to WGS84.

> Built for reproducible workflows and easy addition of new providers.

------------------------------------------------------------------------

## Installation

### Development version (GitHub)

\`\`\`r \# Option A: remotes install.packages("remotes")
remotes::install_github("bafg-bund/hydrodownloadR")

# Option B: pak (fast)

install.packages("pak") pak::pak("bafg-bund/hydrodownloadR")
