# R/provider_usage.R
# Central licence/Terms of Service + usage classification per provider_id.
# Keep ASCII-only.

.provider_access_classes <- c(
  "public",
  "api_key",
  "public_clickthrough",
  "offline_bundle",
  "by_request",
  "unknown"
)

.provider_reuse_classes <- c(
  "open",
  "open_terms",
  "restricted_terms",
  "restricted_no_redistribution",
  "restricted_non-commercial",
  "restricted_non-commercial-no-redestribution",
  "restricted_internal",
  "unknown"
)

provider_usage <- data.frame(
  provider_id = c(
    "AR_INA",      # Argentina - INA Alerta5
    "AT_EHYD",     # Austria - eHYD (BMLRT)
    "AU_BOM",      # Australia - Bureau of Meteorology
    "BA_AVPS",     # BA_AVPS (Bosnia and Herzegovina)
    "BE_HIC",      # Belgium (Flanders) - HIC KiWIS
    "BE_WAL",      # Belgium (Wallonia) - SPW Hydrometrie KiWIS
    "BR_ANA",      # Brazil - ANA HidroWeb / web services
    "CA_ECCC",     # Canada - ECCC / Water Survey (HYDAT etc.)
    "CH_BAFU",     # CH_BAFU - BAFU
    "CL_DGA",      # Chile - DGA
    "CZ_CHMI",     # Czechia - CHMI
    "DK_VANDA",    # Denmark - Danmarks Miljoeportal (VanDa)
    "EE_EST",      # Estonia - (Keskkonnaportaal / Estonian env. data)
    "ES_ROAN",     # Spain - ROEA / Anuario de Aforos (ROAN)
    "FI_SYKE",     # Finland - SYKE
    "FR_HUBEAU",   # France - Hub'Eau
    "IE_OPW",      # Ireland - OPW Hydrometric
    "JP_MLIT",     # Japan - MLIT
    "KR_WAMIS",    # Korea - WAMIS
    "LT_LHMT",     # Lithuania - LHMT (meteo.lt)
    "NL_RWS",      # Netherlands - Rijkswaterstaat (waterinfo)
    "NO_NVE",      # Norway - NVE
    "PL_IMGW",     # Poland - IMGW
    "RW_RWB",      # Rwanda - Rwanda Water Board
    "SE_SMHI",     # Sweden - SMHI
    "SI_ARSO",     # Slovenia - ARSO
    "UK_CEH",      # UK - UKCEH / EIDC
    "UK_NRFA",     # UK - NRFA
    "US_USGS_DR",  # USA - USGS (dataRetrieval / NWIS services)
    "US_USGS_NWIS",# USA - USGS NWIS
    "ZA_DWS"       # South Africa - DWS
  ),

  license = c(
    "Custom/Terms of Service",           # AR_INA (Argentina - INA Alerta5)
    "Custom/Terms of Service",           # AT_EHYD (Austria - eHYD)
    "CC BY 4.0",                         # AU_BOM (Australia - BoM)
    "Copyright - All rights reserved",   # BA_AVPS (Bosnia and Herzegovina)
    "Custom/Terms of Service",           # BE_HIC (Belgium Flanders - HIC)
    "Custom/Terms of Service",           # BE_WAL (Belgium Wallonia - SPW)
    "CC BY-ND 3.0",                      # BR_ANA (Brazil - ANA)
    "Open Government Licence (Canada)",  # CA_ECCC (Canada - ECCC)
    "Custom/Terms of Service",           # CH_BAFU - BAFU
    "CC BY 4.0",                         # CL_DGA (Chile - DGA)
    "CC BY 4.0",                         # CZ_CHMI (Czechia - CHMI)
    "PSI-loven (CC0/CC-BY comparable)",  # DK_VANDA (Denmark - VanDa)
    "CC BY 4.0 (see distribution terms)",# EE_EST (Estonia)
    "CC BY 4.0",                         # ES_ROAN (Spain - ROAN/ROEA)
    "CC BY 4.0",                         # FI_SYKE (Finland - SYKE)
    "CC BY 2.0",                         # FR_HUBEAU (France - Hub'Eau)
    "CC BY 4.0",                         # IE_OPW (Ireland - OPW)
    "Custom/Terms of use",               # JP_MLIT (Japan - MLIT)
    "UNKNOWN",                           # KR_WAMIS (Korea - WAMIS)
    "CC BY-SA 4.0",                      # LT_LHMT (Lithuania - LHMT)
    "CC 0",                              # NL_RWS (Netherlands - RWS)
    "NLOD (CC BY 3.0 comparable)",       # NO_NVE (Norway - NVE)
    "Custom/Terms of Service",           # PL_IMGW (Poland - IMGW)
    "Custom/Terms of Service",           # RW_RWB (Rwanda - RWB)
    "CC BY 4.0",                         # SE_SMHI (Sweden - SMHI)
    "Custom/Terms of Service",           # SI_ARSO (Slovenia - ARSO)
    "OGL 3",                             # UK_CEH (UKCEH / EIDC)
    "OGL 3",                             # UK_NRFA (UK - NRFA)
    "Public Domain (USGS policy)",       # US_USGS_DR (USA - USGS)
    "Public Domain (USGS policy)",       # US_USGS_NWIS (USA - USGS NWIS)
    "UNKNOWN"                            # ZA_DWS (South Africa - DWS)
  ),

  license_link = c(
    "https://snih.hidricosargentina.gob.ar/Terminos.aspx", # AR_INA (Argentina - INA Alerta5)
    "https://ehyd.gv.at/assets/eHYD/pdf/Messstellen_und_Daten.pdf", # AT_EHYD (Austria - eHYD)
    "https://www.bom.gov.au/copyright", # AU_BOM (Australia - BoM)
    "https://www.voda.ba/vodostaji", # BA_AVPS (Bosnia and Herzegovina)
    "https://hicws.vlaanderen.be", # BE_HIC (Belgium Flanders - HIC)
    "https://hydrometrie.wallonie.be/mentions-legales.html", # BE_WAL (Belgium Wallonia - SPW)
    "https://www.gov.br/ana/pt-br/assuntos/monitoramento-e-eventos-criticos/monitoramento-hidrologico", # BR_ANA (Brazil - ANA)
    "https://open.canada.ca/en/open-government-licence-canada", # CA_ECCC (Canada - ECCC)
    "https://api.existenz.ch/",  # CH_BAFU - BAFU
    "https://dga.mop.gob.cl/servicios-de-informacion/catastro-publico-de-aguas/", # CL_DGA (Chile - DGA)
    "https://data.gov.cz/dataset?iri=https%3A%2F%2Fdata.gov.cz%2Fzdroj%2Fdatov%C3%A9-sady%2F00020699%2F1f615a016fdc24947e6c6d6bbd530508", # CZ_CHMI (Czechia - CHMI)
    "https://miljoeportal.dk/dataansvar/vilkaar-for-brug", # DK_VANDA (Denmark - VanDa)
    "https://keskkonnaportaal.ee/et/avaandmed#Avaandmetejuriidilinealus", # EE_EST (Estonia)
    "https://datos.gob.es/en/catalogo/e05068001-servicio-de-api-de-datos-del-miteco", # ES_ROAN (Spain - ROAN/ROEA)
    "https://www.syke.fi/en-US/Open_information/Open_data/Licence", # FI_SYKE (Finland - SYKE)
    "https://www.etalab.gouv.fr/licence-ouverte-open-licence/", # FR_HUBEAU (France - Hub'Eau)
    "https://waterlevel.ie/page/api/", # IE_OPW (Ireland - OPW)
    "https://www.mlit.go.jp/link.html", # JP_MLIT (Japan - MLIT)
    "https://www.wamis.go.kr/ENG/Overview.do", # KR_WAMIS (Korea - WAMIS)
    "https://api.meteo.lt/", # LT_LHMT (Lithuania - LHMT)
    "https://waterinfo-extra.rws.nl/monitoring/gebruik-data/", # NL_RWS (Netherlands - RWS)
    "https://hydapi.nve.no/UserDocumentation/#license", # NO_NVE (Norway - NVE)
    "https://danepubliczne.imgw.pl/pl/regulations", # PL_IMGW (Poland - IMGW)
    "https://waterportal.rwb.rw/about_us", # RW_RWB (Rwanda - RWB)
    "https://www.smhi.se/data/om-smhis-data/villkor-for-anvandning", # SE_SMHI (Sweden - SMHI)
    "https://vode.arso.gov.si/hidarhiv/pov_arhiv_tab.php", # SI_ARSO (Slovenia - ARSO)
    "http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", # UK_CEH (UKCEH / EIDC)
    "http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", # UK_NRFA (UK - NRFA)
    "https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits", # US_USGS_DR (USA - USGS)
    "https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits", # US_USGS_NWIS (USA - USGS NWIS)
    "https://www.dws.gov.za/NGANet/Resources/docs/Disclaimer.htm"  # ZA_DWS (South Africa - DWS)
  ),

  access_class = c(
    "public_clickthrough",             # AR_INA (Argentina - INA Alerta5)
    "offline_bundle",     # AT_EHYD (Austria - eHYD)
    "public",             # AU_BOM (Australia - BoM)
    "public",             # BA_AVPS (Bosnia and Herzegovina)
    "public",             # BE_HIC (Belgium Flanders - HIC)
    "public",             # BE_WAL (Belgium Wallonia - SPW)
    "api_key",            # BR_ANA (Brazil - ANA)
    "public",             # CA_ECCC (Canada - ECCC)
    "public",             # CH_BAFU - BAFU
    "public",             # CL_DGA (Chile - DGA)
    "public",             # CZ_CHMI (Czechia - CHMI)
    "public",             # DK_VANDA (Denmark - VanDa)
    "public",             # EE_EST (Estonia)
    "public",             # ES_ROAN (Spain - ROAN/ROEA)
    "public",             # FI_SYKE (Finland - SYKE)
    "public",             # FR_HUBEAU (France - Hub'Eau)
    "public",             # IE_OPW (Ireland - OPW)
    "public_clickthrough",# JP_MLIT (Japan - MLIT)
    "public",             # KR_WAMIS (Korea - WAMIS)
    "public",             # LT_LHMT (Lithuania - LHMT)
    "public",             # NL_RWS (Netherlands - RWS)
    "api_key",             # NO_NVE (Norway - NVE)
    "public_clickthrough",# PL_IMGW (Poland - IMGW)
    "public_clickthrough",# RW_RWB (Rwanda - RWB)
    "public",             # SE_SMHI (Sweden - SMHI)
    "public_clickthrough",# SI_ARSO (Slovenia - ARSO)
    "public",             # UK_CEH (UKCEH / EIDC)
    "public",             # UK_NRFA (UK - NRFA)
    "api_key",             # US_USGS_DR (USA - USGS)
    "api_key",             # US_USGS_NWIS (USA - USGS NWIS)
    "public"          # ZA_DWS (South Africa - DWS)
  ),

  reuse_class = c(
    "open",                        # AR_INA (Argentina - INA Alerta5)
    "restricted_internal",         # AT_EHYD (Austria - eHYD)
    "open",                        # AU_BOM (Australia - BoM)
    "restricted_no_redistribution",                        # BA_AVPS (Bosnia and Herzegovina)
    "open",                        # BE_HIC (Belgium Flanders - HIC)
    "restricted_non-commercial-no-redestribution",   # BE_WAL (Belgium Wallonia - SPW)
    "open_terms",                  # BR_ANA (Brazil - ANA)
    "open",                        # CA_ECCC (Canada - ECCC)
    "open",                        # CH_BAFU - BAFU
    "open",                        # CL_DGA (Chile - DGA)
    "open",                        # CZ_CHMI (Czechia - CHMI)
    "open_terms",                  # DK_VANDA (Denmark - VanDa)
    "open",                        # EE_EST (Estonia)
    "open",                        # ES_ROAN (Spain - ROAN/ROEA)
    "open",                        # FI_SYKE (Finland - SYKE)
    "open",                        # FR_HUBEAU (France - Hub'Eau)
    "open",                        # IE_OPW (Ireland - OPW)
    "open_terms",                  # JP_MLIT (Japan - MLIT)
    "unknown",                     # KR_WAMIS (Korea - WAMIS)
    "open",                        # LT_LHMT (Lithuania - LHMT)
    "open",                        # NL_RWS (Netherlands - RWS)
    "open",                        # NO_NVE (Norway - NVE)
    "restricted_terms",            # PL_IMGW (Poland - IMGW)
    "open",                        # RW_RWB (Rwanda - RWB)
    "open",                        # SE_SMHI (Sweden - SMHI)
    "open_terms",                  # SI_ARSO (Slovenia - ARSO)
    "open",                        # UK_CEH (UKCEH / EIDC)
    "open",                        # UK_NRFA (UK - NRFA)
    "open_terms",                  # US_USGS_DR (USA - USGS)
    "open_terms",                  # US_USGS_NWIS (USA - USGS NWIS)
    "restricted_non-commercial"    # ZA_DWS (South Africa - DWS)
  ),

  is_open_data = c(
    TRUE,  # AR_INA (Argentina - INA Alerta5)
    FALSE, # AT_EHYD (Austria - eHYD)
    TRUE,  # AU_BOM (Australia - BoM)
    TRUE,  # BA_AVPS (Bosnia and Herzegovina)
    TRUE,  # BE_HIC (Belgium Flanders - HIC)
    FALSE, # BE_WAL (Belgium Wallonia - SPW)
    TRUE,  # BR_ANA (Brazil - ANA)
    TRUE,  # CA_ECCC (Canada - ECCC)
    TRUE,  # CH_BAFU - BAFU
    TRUE,  # CL_DGA (Chile - DGA)
    TRUE,  # CZ_CHMI (Czechia - CHMI)
    TRUE,  # DK_VANDA (Denmark - VanDa)
    TRUE,  # EE_EST (Estonia)
    TRUE,  # ES_ROAN (Spain - ROAN/ROEA)
    TRUE,  # FI_SYKE (Finland - SYKE)
    TRUE,  # FR_HUBEAU (France - Hub'Eau)
    TRUE,  # IE_OPW (Ireland - OPW)
    TRUE,  # JP_MLIT (Japan - MLIT)
    NA,    # KR_WAMIS (Korea - WAMIS)
    TRUE,  # LT_LHMT (Lithuania - LHMT)
    TRUE,  # NL_RWS (Netherlands - RWS)
    TRUE,  # NO_NVE (Norway - NVE)
    FALSE, # PL_IMGW (Poland - IMGW)
    TRUE,  # RW_RWB (Rwanda - RWB)
    TRUE,  # SE_SMHI (Sweden - SMHI)
    TRUE,  # SI_ARSO (Slovenia - ARSO)
    TRUE,  # UK_CEH (UKCEH / EIDC)
    TRUE,  # UK_NRFA (UK - NRFA)
    TRUE,  # US_USGS_DR (USA - USGS)
    TRUE,  # US_USGS_NWIS (USA - USGS NWIS)
    FALSE  # ZA_DWS (South Africa - DWS)
  ),

  stringsAsFactors = FALSE
)


.validate_provider_usage <- function(df) {
  if (anyDuplicated(df$provider_id)) stop("provider_usage: provider_id not unique")

  bad_access <- setdiff(unique(df$access_class), .provider_access_classes)
  if (length(bad_access)) stop("provider_usage: invalid access_class: ", paste(bad_access, collapse = ", "))

  bad_reuse <- setdiff(unique(df$reuse_class), .provider_reuse_classes)
  if (length(bad_reuse)) stop("provider_usage: invalid reuse_class: ", paste(bad_reuse, collapse = ", "))

  invisible(TRUE)
}
.validate_provider_usage(provider_usage)

provider_usage_get <- function(provider_id) {
  i <- match(provider_id, provider_usage$provider_id)
  if (is.na(i)) stop("provider_usage_get: unknown provider_id: ", provider_id)
  row <- provider_usage[i, , drop = FALSE]
  as.list(row[1, c("license","license_link","access_class","reuse_class","is_open_data")])
}

# Key trick: same argument pattern as register_service(), but it injects usage fields.
register_service_usage <- function(provider_id, ...) {
  u <- provider_usage_get(provider_id)
  dots <- list(...)
  do.call(register_service, c(list(provider_id = provider_id), dots, u))
}
