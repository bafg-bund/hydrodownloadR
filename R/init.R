init.path <- "Z:/M/M4/grdc/5_Products/hydrodownloadR"
setwd(init.path)
# 1) Paket (Dateien) anlegen – kein RStudio-Open/keine Projektaktivierung
usethis::create_package(init.path, rstudio = FALSE, open = FALSE)


# 2) Projekt in dieser Session aktivieren (ohne RStudio-API)
usethis::proj_set(init.path)

# 3) (Optional) .Rproj-Datei schreiben, aber NICHT automatisch öffnen
usethis::use_rstudio()

usethis::use_roxygen_md()
usethis::use_package("httr2")
usethis::use_package("jsonlite")
usethis::use_package("cli")
usethis::use_package("rlang")
usethis::use_package("tibble")
usethis::use_package("dplyr")
usethis::use_package("lubridate")
usethis::use_package("ratelimitr")
usethis::use_package("progress")
usethis::use_package("memoise")
usethis::use_package("cachem")
usethis::use_testthat()
usethis::use_pkgdown()
# # optional:
# usethis::use_package("keyring")   # für Secrets
# usethis::use_package("vcr")       # HTTP-Kassetten für Tests
# usethis::use_package("httptest2") # Alternative zum Testen
usethis::proj_sitrep()
usethis::use_git()
usethis::use_github_action("check-standard")
usethis::use_git()                 # falls noch nicht geschehen
# usethis::use_github(private = TRUE)  # oder host=... für Enterprise
# usethis::use_pkgdown_github_pages()
usethis::use_pkgdown()

# 2) Site lokal bauen (öffnet Browser, legt standardmäßig in "docs/" ab)
pkgdown::build_site()

usethis::use_gpl_license(version = 3)
usethis::use_tidy_description()
usethis::use_readme_rmd()
usethis::use_news_md()
