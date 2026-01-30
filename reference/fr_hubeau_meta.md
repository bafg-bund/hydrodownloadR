# FR_HUBEAU precomputed station metadata

Preloaded metadata for Hub'Eau stations, built offline from the Hub'Eau
referentials plus scraped site/station fiches (area, site altitude,
gauge-zero altitude, vertical datum at site). Used to speed up
[`stations()`](https://bafg-bund.github.io/hydrodownloadR/reference/stations.md)
for the FR_HUBEAU provider.

## Format

A data frame/tibble with columns:

- code_site:

  Character. Hub'Eau site code.

- station_id:

  Character. Hub'Eau station code.

- area:

  Numeric (km\\^2\\). Catchment area from site fiche; may be `NA`.

- altitude_api:

  Numeric (m). API referential altitude (hydrometry in mm to m;
  temperature in m).

- altitude_site:

  Numeric (m). Site altitude parsed from the site fiche; may be `NA`.

- altitude_station:

  Numeric (m). "Cote du zero d'echelle" from station fiche; may be `NA`.

- vertical_datum_site:

  Character. Site-level vertical datum label; may be `NA`.

- retrieved_at:

  POSIXct (UTC). Timestamp when the row was scraped.

## Source

Hub'Eau APIs and https://www.hydro.eaufrance.fr/ site/station fiches.

## Details

Built by `data-raw/fr_hubeau_meta_build.R`. The file
`data/fr_hubeau_meta.rda` is shipped with the package and may be
refreshed out-of-band. A build-date string is also stored in the object
attribute `metadata_date`.
