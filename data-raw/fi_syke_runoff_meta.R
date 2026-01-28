# data-raw/fi_syke_runoff_meta.R
# Run once to (re)generate the packaged dataset used by the adapter.

if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("usethis", quietly = TRUE)) install.packages("usethis")

library(readxl)
library(dplyr)
library(stringr)

xlsx_path <- "data-raw/Runoff_Valuma_ids_18092025.xlsx"
raw <- read_excel(xlsx_path)

nm <- tolower(names(raw))
get_col <- function(cands) {
  ix <- which(nm %in% tolower(cands))
  if (length(ix)) raw[[ix[1]]] else NULL
}

place_id   <- get_col(c("place_id","paikka_id","paikkaid","id","station_id","ref"))
area_km2   <- get_col(c("area_km2","catchment_area_km2","catchment_km2","area","catchmentarea"))
altitude_m <- get_col(c("altitude","altitude_m","elevation","elev_m","z"))

stopifnot(!is.null(place_id))  # place_id is required; area/altitude can be NA

fi_syke_runoff_meta <- tibble::tibble(
  place_id = str_trim(as.character(place_id)),
  area     = suppressWarnings(as.numeric(area_km2)),   # km^2 (can be NA)
  altitude = suppressWarnings(as.numeric(altitude_m))  # m (can be NA)
) %>%
  filter(!is.na(place_id) & nzchar(place_id)) %>%
  # collapse duplicates by place_id, preferring non-NA area/altitude
  group_by(place_id) %>%
  summarise(
    area     = dplyr::first(na.omit(area),     default = NA_real_),
    altitude = dplyr::first(na.omit(altitude), default = NA_real_),
    .groups = "drop"
  )

usethis::use_data(fi_syke_runoff_meta, overwrite = TRUE, compress = "xz")
