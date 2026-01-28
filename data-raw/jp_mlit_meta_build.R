# data-raw/jp_mlit_meta_build.R

# This script:
#   - reads the raw JP MLIT station metadata CSV (Shift_JIS / cp932),
#   - renames columns,
#   - translates watersystem_name & station_name to English via OpenAI API,
#   - converts DMS lat/lon to decimal WGS84,
#   - stores `jp_mlit_meta` as an internal package dataset.

# 0) Path to raw metadata ------------------------------------------------------

jp.md.file <- file.path(
  "data-raw",
  "Specifications of Water Level and Discharge Observation Stations_20251222.csv"
)

# 1) Read raw metadata ---------------------------------------------------------

jp.raw <- readr::read_csv(
  jp.md.file,
  locale    = readr::locale(encoding = "cp932"),
  col_types = readr::cols(.default = "c")
)

jp.md <- jp.raw |>
  dplyr::rename(
    station_id       = 1,
    watersystem_code = 2,
    watersystem_name = 3,
    station_name     = 4,
    lat_deg          = 5,
    lat_min          = 6,
    lat_sec          = 7,
    lon_deg          = 8,
    lon_min          = 9,
    lon_sec          = 10,
    gauge_zero_m     = 11,
    catchment_area_km2 = 12,
    distance_from_mouth_or_confluence_km = 13
  )

# 2) Single-string translation via OpenAI --------------------------------------

openai_translate_one <- function(
    text,
    model       = "gpt-4.1-mini",   # or "gpt-4o-mini"
    api_key     = Sys.getenv("OPENAI_API_KEY"),
    target_lang = "English"
) {
  if (!nzchar(api_key)) {
    stop("OPENAI_API_KEY environment variable is not set.")
  }
  if (is.na(text) || !nzchar(text)) return(text)

  req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model,
      messages = list(
        list(
          role    = "system",
          content = "You are a translation engine for Japanese hydrological station and river names. Reply with only the translated name, no quotes, no extra text."
        ),
        list(
          role    = "user",
          content = paste0(
            "Translate this Japanese name into natural English:\n\n",
            text
          )
        )
      ),
      temperature = 0
    ))

  resp <- httr2::req_perform(req)

  # Keep as list-of-lists so we can index into $choices[[1]]$message$content
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)

  if (!is.null(body$error)) {
    stop("OpenAI API error (", body$error$code, "): ", body$error$message)
  }

  translated_vec <- body$choices[[1]]$message$content
  translated <- translated_vec[1]

  trimws(translated)
}

# 3) Translation helper (vector, unique values) --------------------------------

openai_translate_vec_unique <- function(
    x,
    sleep_sec = 0.3   # adjust if you ever hit rate limits
) {
  x <- as.character(x)
  uniq <- sort(unique(na.omit(x)))
  if (!length(uniq)) return(x)

  trans <- character(length(uniq))
  names(trans) <- uniq

  for (i in seq_along(uniq)) {
    nm <- uniq[i]
    trans[i] <- openai_translate_one(nm)
    Sys.sleep(sleep_sec)   # tiny pause between requests
  }

  out <- x
  mask <- !is.na(out)
  out[mask] <- trans[out[mask]]
  out
}

# 4) Translate watersystem & station names -------------------------------------

jp.md$watersystem_name_en <- openai_translate_vec_unique(jp.md$watersystem_name)
jp.md$station_name_en     <- openai_translate_vec_unique(jp.md$station_name)

# 5) Build jp_mlit_meta with WGS84 coords --------------------------------------

jp_mlit_meta <- jp.md |>
  dplyr::mutate(
    # numeric conversion for all relevant columns
    dplyr::across(
      c(lat_deg, lat_min, lat_sec,
        lon_deg, lon_min, lon_sec,
        catchment_area_km2, gauge_zero_m),
      ~ suppressWarnings(as.numeric(.))
    )
  ) |>
  dplyr::mutate(
    # Japan is in the NE quadrant → no sign flip needed
    lat = lat_deg + lat_min / 60 + lat_sec / 3600,
    lon = lon_deg + lon_min / 60 + lon_sec / 3600,

    area     = catchment_area_km2,
    altitude = gauge_zero_m,

    # naming for adapter:
    station_name_original = station_name,      # Japanese
    station_name          = station_name_en,   # English
    river                 = watersystem_name_en
  ) |>
  dplyr::select(
    station_id,
    station_name,
    river,
    lat,
    lon,
    area,
    altitude,
    station_name_original
  )

#  6) Store as ~/data/jp_mlit_meta.rda -----------------------------------------

out_dir <- "~/data"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}
save(jp_mlit_meta, file = "data/jp_mlit_meta.rda", compress = "xz")
message("Saved data/jp_mlit_meta.rda with ", nrow(jp_mlit_meta), " rows.")

