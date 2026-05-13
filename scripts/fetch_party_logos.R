library(dplyr)
library(httr2)
library(jsonlite)
library(readxl)

logo_dir <- "logos"
dir.create(logo_dir, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

is_blank <- function(x) {
  is.null(x) || length(x) == 0 || is.na(x[[1]]) || x[[1]] == ""
}

standardise_name <- function(x) {
  x <- iconv(trimws(as.character(x)), from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  gsub("[^a-z0-9]+", "_", x)
}

slugify <- function(...) {
  parts <- list(...)
  slug <- do.call(paste, c(lapply(parts, standardise_name), sep = "_"))
  gsub("(^_+|_+$)", "", gsub("_+", "_", slug))
}

party_sources <- tibble::tribble(
  ~country, ~region, ~party, ~wiki_lang, ~wiki_title,
  "Netherlands", "Netherlands", "BBB", "en", "BoerBurgerBeweging",
  "Netherlands", "Netherlands", "CDA", "en", "Christian Democratic Appeal",
  "Netherlands", "Netherlands", "CU", "en", "Christian Union (Netherlands)",
  "Netherlands", "Netherlands", "D66", "en", "Democrats 66",
  "Netherlands", "Netherlands", "Denk", "en", "DENK (political party)",
  "Netherlands", "Netherlands", "FvD", "en", "Forum for Democracy",
  "Netherlands", "Netherlands", "GL", "en", "GroenLinks",
  "Netherlands", "Netherlands", "JA21", "en", "JA21",
  "Netherlands", "Netherlands", "NSC", "en", "New Social Contract",
  "Netherlands", "Netherlands", "PvdA", "en", "Labour Party (Netherlands)",
  "Netherlands", "Netherlands", "PvdD", "en", "Party for the Animals",
  "Netherlands", "Netherlands", "PVV", "en", "Party for Freedom",
  "Netherlands", "Netherlands", "SGP", "en", "Reformed Political Party",
  "Netherlands", "Netherlands", "SP", "en", "Socialist Party (Netherlands)",
  "Netherlands", "Netherlands", "Volt", "en", "Volt Netherlands",
  "Netherlands", "Netherlands", "VVD", "en", "People's Party for Freedom and Democracy",
  "Switzerland", "Switzerland", "SVP", "en", "Swiss People's Party",
  "Switzerland", "Switzerland", "SP", "en", "Social Democratic Party of Switzerland",
  "Switzerland", "Switzerland", "FDP", "en", "FDP.The Liberals",
  "Switzerland", "Switzerland", "The Centre", "en", "The Centre (political party)",
  "Switzerland", "Switzerland", "GPS", "en", "Green Party of Switzerland",
  "Switzerland", "Switzerland", "GLP", "en", "Green Liberal Party of Switzerland",
  "Switzerland", "Switzerland", "EVP", "en", "Evangelical People's Party of Switzerland",
  "Switzerland", "Switzerland", "EDU", "en", "Federal Democratic Union of Switzerland",
  "Switzerland", "Switzerland", "LdT", "en", "Ticino League",
  "Switzerland", "Switzerland", "MCG", "en", "Geneva Citizens' Movement",
  "Belgium", "Wallonia", "DéFi", "en", "DéFI",
  "Belgium", "Wallonia", "Ecolo", "en", "Ecolo",
  "Belgium", "Wallonia", "LE", "en", "Les Engagés",
  "Belgium", "Wallonia", "MR", "en", "Reformist Movement",
  "Belgium", "Wallonia", "PS", "en", "Socialist Party (Belgium)",
  "Belgium", "Wallonia", "PTB-PVDA", "en", "Workers' Party of Belgium",
  "Belgium", "Flanders", "CD&V", "en", "Christian Democratic and Flemish",
  "Belgium", "Flanders", "Groen", "en", "Groen (political party)",
  "Belgium", "Flanders", "N-VA", "en", "New Flemish Alliance",
  "Belgium", "Flanders", "OVLD (Anders)", "en", "Open Flemish Liberals and Democrats",
  "Belgium", "Flanders", "VB", "en", "Vlaams Belang",
  "Belgium", "Flanders", "Vooruit", "en", "Vooruit (political party)"
) |>
  mutate(logo_slug = slugify(country, region, party))

official_domains <- tibble::tribble(
  ~country, ~region, ~party, ~official_domain,
  "Netherlands", "Netherlands", "BBB", "boerburgerbeweging.nl",
  "Netherlands", "Netherlands", "CDA", "cda.nl",
  "Netherlands", "Netherlands", "CU", "christenunie.nl",
  "Netherlands", "Netherlands", "D66", "d66.nl",
  "Netherlands", "Netherlands", "Denk", "bewegingdenk.nl",
  "Netherlands", "Netherlands", "FvD", "fvd.nl",
  "Netherlands", "Netherlands", "GL", "groenlinks.nl",
  "Netherlands", "Netherlands", "JA21", "ja21.nl",
  "Netherlands", "Netherlands", "NSC", "partijnieuwsociaalcontract.nl",
  "Netherlands", "Netherlands", "PvdA", "pvda.nl",
  "Netherlands", "Netherlands", "PvdD", "partijvoordedieren.nl",
  "Netherlands", "Netherlands", "PVV", "pvv.nl",
  "Netherlands", "Netherlands", "SGP", "sgp.nl",
  "Netherlands", "Netherlands", "SP", "sp.nl",
  "Netherlands", "Netherlands", "Volt", "voltnederland.org",
  "Netherlands", "Netherlands", "VVD", "vvd.nl",
  "Switzerland", "Switzerland", "SVP", "svp.ch",
  "Switzerland", "Switzerland", "SP", "sp-ps.ch",
  "Switzerland", "Switzerland", "FDP", "fdp.ch",
  "Switzerland", "Switzerland", "The Centre", "die-mitte.ch",
  "Switzerland", "Switzerland", "GPS", "gruene.ch",
  "Switzerland", "Switzerland", "GLP", "grunliberale.ch",
  "Switzerland", "Switzerland", "EVP", "evppev.ch",
  "Switzerland", "Switzerland", "EDU", "edu-schweiz.ch",
  "Switzerland", "Switzerland", "LdT", "legaticinesi.ch",
  "Switzerland", "Switzerland", "MCG", "mcge.ch",
  "Belgium", "Wallonia", "DéFi", "defi.eu",
  "Belgium", "Wallonia", "Ecolo", "ecolo.be",
  "Belgium", "Wallonia", "LE", "lesengages.be",
  "Belgium", "Wallonia", "MR", "mr.be",
  "Belgium", "Wallonia", "PS", "ps.be",
  "Belgium", "Wallonia", "PTB-PVDA", "ptb.be",
  "Belgium", "Flanders", "CD&V", "cdenv.be",
  "Belgium", "Flanders", "Groen", "groen.be",
  "Belgium", "Flanders", "N-VA", "n-va.be",
  "Belgium", "Flanders", "OVLD (Anders)", "openvld.be",
  "Belgium", "Flanders", "VB", "vlaamsbelang.org",
  "Belgium", "Flanders", "Vooruit", "vooruit.org"
)

party_sources <- party_sources |>
  left_join(official_domains, by = c("country", "region", "party"))

api_get <- function(url, query = list()) {
  req <- request(url) |>
    req_user_agent("Fanero Quarto dashboard logo fetcher (research project; contact via local user)") |>
    req_timeout(20)

  req <- do.call(req_url_query, c(list(req), query))

  req |>
    req_perform() |>
    resp_body_json(simplifyVector = FALSE)
}

page_info <- function(lang, title) {
  api_get(
    sprintf("https://%s.wikipedia.org/w/api.php", lang),
    list(
      action = "query",
      titles = title,
      prop = "pageprops|pageimages",
      pithumbsize = 900,
      redirects = 1,
      format = "json",
      formatversion = 2
    )
  )$query$pages[[1]]
}

wikidata_logo_filename <- function(qid) {
  if (is.null(qid) || is.na(qid) || qid == "") return(NA_character_)
  entity <- api_get(
    "https://www.wikidata.org/w/api.php",
    list(
      action = "wbgetentities",
      ids = qid,
      props = "claims",
      format = "json"
    )
  )
  claims <- entity$entities[[qid]]$claims
  if (is.null(claims$P154) || length(claims$P154) == 0) return(NA_character_)
  claims$P154[[1]]$mainsnak$datavalue$value
}

commons_file_url <- function(filename) {
  if (is_blank(filename)) return(NA_character_)
  paste0(
    "https://commons.wikimedia.org/wiki/Special:Redirect/file/",
    utils::URLencode(filename[[1]], reserved = TRUE)
  )
}

extension_from <- function(path_or_url, default = "png") {
  clean <- sub("[?#].*$", "", path_or_url)
  ext <- tolower(tools::file_ext(clean))
  if (ext %in% c("svg", "png", "jpg", "jpeg", "webp")) ext else default
}

download_logo <- function(url, dest) {
  request(url) |>
    req_user_agent("Fanero Quarto dashboard logo fetcher (research project; contact via local user)") |>
    req_timeout(30) |>
    req_perform(path = dest)
}

valid_logo_file <- function(path) {
  if (!file.exists(path) || file.info(path)$size <= 100) return(FALSE)
  ext <- tolower(tools::file_ext(path))
  header_raw <- readBin(path, what = "raw", n = min(200, file.info(path)$size))
  header_int <- as.integer(header_raw)
  header_ascii <- as.raw(header_int[header_int %in% c(9, 10, 13, 32:126)])
  header <- rawToChar(header_ascii)
  if (grepl("<!doctype html|<html", header, ignore.case = TRUE)) return(FALSE)
  if (ext %in% c("png", "jpg", "jpeg", "webp")) {
    if (requireNamespace("magick", quietly = TRUE)) {
      return(tryCatch({
        magick::image_read(path)
        TRUE
      }, error = function(e) FALSE))
    }
    return(TRUE)
  }
  TRUE
}

existing_logo_file <- function(slug) {
  candidates <- list.files(
    logo_dir,
    pattern = paste0("^", slug, "\\.(svg|png|jpg|jpeg|webp)$"),
    full.names = FALSE,
    ignore.case = TRUE
  )
  if (length(candidates) == 0) return(NA_character_)
  candidates <- candidates[vapply(file.path(logo_dir, candidates), valid_logo_file, logical(1))]
  if (length(candidates) > 0) candidates[[1]] else NA_character_
}

official_domain_icon_url <- function(domain) {
  if (is_blank(domain)) return(NA_character_)
  paste0("https://www.google.com/s2/favicons?domain=", domain[[1]], "&sz=256")
}

manual_logo_sources <- tibble::tribble(
  ~country, ~region, ~party, ~manual_logo_filename, ~manual_logo_url_override,
  "Belgium", "Flanders", "CD&V", "Logo of the Christian Democratic and Flemish (2022).svg", NA_character_,
  "Belgium", "Flanders", "Groen", "Groen logo 2022.svg", NA_character_,
  "Belgium", "Flanders", "N-VA", "Logo of the New Flemish Alliance.svg", NA_character_,
  "Belgium", "Flanders", "OVLD (Anders)", "anders_logo_grijs.svg", "https://www.anders.be/theme_anders/static/src/img/logo_grijs.svg",
  "Belgium", "Wallonia", "Ecolo", "Ecolo Logo.svg", NA_character_,
  "Belgium", "Wallonia", "LE", "Logo les Engagés 2022.svg", NA_character_,
  "Belgium", "Wallonia", "PS", "Socialist Party (Belgium) logo.svg", NA_character_,
  "Switzerland", "Switzerland", "SVP", "SVP UDC Logo.svg", NA_character_,
  "Switzerland", "Switzerland", "SP", "2022 logo of the Social Democratic Party of Switzerland.svg", NA_character_,
  "Switzerland", "Switzerland", "The Centre", "DieMitte-logo.svg", NA_character_,
  "Switzerland", "Switzerland", "GLP", "Logo Grünliberale Partei.svg", NA_character_,
  "Switzerland", "Switzerland", "EDU", "Logo EDU UDF.svg", NA_character_,
  "Switzerland", "Switzerland", "EVP", "EVP Logo Deutsch 300dpi.jpg", NA_character_,
  "Switzerland", "Switzerland", "LdT", "Logo_LEGA_Rosso_sm.svg", "https://lega-dei-ticinesi.ch/wp-content/uploads/2021/12/Logo_LEGA_Rosso_sm.svg"
) |>
  mutate(
    manual_logo_url = ifelse(
      is.na(manual_logo_url_override),
      vapply(manual_logo_filename, commons_file_url, character(1)),
      manual_logo_url_override
    ),
    manual_source_kind = ifelse(is.na(manual_logo_url_override), "manual_commons_logo", "manual_official_logo")
  )

party_sources <- party_sources |>
  left_join(manual_logo_sources, by = c("country", "region", "party"))

existing_assets <- list.files(
  logo_dir,
  pattern = "\\.(svg|png|jpg|jpeg|webp)$",
  full.names = TRUE,
  ignore.case = TRUE
)
invalid_assets <- existing_assets[!vapply(existing_assets, valid_logo_file, logical(1))]
if (length(invalid_assets) > 0) {
  unlink(invalid_assets)
  message("Removed ", length(invalid_assets), " invalid logo files from previous failed downloads.")
}

results <- vector("list", nrow(party_sources))

for (i in seq_len(nrow(party_sources))) {
  row <- party_sources[i, ]
  message(sprintf("[%02d/%02d] %s", i, nrow(party_sources), row$party))

  status <- "missing"
  source_url <- NA_character_
  source_kind <- NA_character_
  logo_file <- NA_character_
  qid <- NA_character_
  logo_filename <- NA_character_

  tryCatch({
    if (!is_blank(row$manual_logo_url)) {
      ext <- extension_from(row$manual_logo_filename, "png")
      logo_file <- paste0(row$logo_slug, ".", ext)
      dest <- file.path(logo_dir, logo_file)
      existing_manual <- existing_logo_file(row$logo_slug)

      if (!is.na(existing_manual) && identical(existing_manual, logo_file)) {
        source_url <- row$manual_logo_url
        source_kind <- row$manual_source_kind
        logo_filename <- row$manual_logo_filename
        status <- "exists"
      } else {
        temp_dest <- tempfile(pattern = paste0(row$logo_slug, "_"), tmpdir = logo_dir, fileext = paste0(".", ext))

        download_logo(row$manual_logo_url, temp_dest)
        if (valid_logo_file(temp_dest)) {
          stale <- list.files(
            logo_dir,
            pattern = paste0("^", row$logo_slug, "\\.(svg|png|jpg|jpeg|webp)$"),
            full.names = TRUE,
            ignore.case = TRUE
          )
          unlink(stale)
          file.rename(temp_dest, dest)
          source_url <- row$manual_logo_url
          source_kind <- row$manual_source_kind
          logo_filename <- row$manual_logo_filename
          status <- "downloaded"
        } else {
          unlink(temp_dest)
          status <- "error: manual logo was not a valid image"
        }
      }
    }

    existing <- existing_logo_file(row$logo_slug)
    if (!(status %in% c("exists", "downloaded")) && !is.na(existing)) {
      logo_file <- existing
      status <- "exists"
    } else if (!(status %in% c("exists", "downloaded"))) {
      tryCatch({
        page <- page_info(row$wiki_lang, row$wiki_title)
        qid <- page$pageprops$wikibase_item %||% NA_character_
        logo_filename <- wikidata_logo_filename(qid)
        source_url <- commons_file_url(logo_filename)
        source_kind <- "wikidata_p154"

        if (is.na(source_url) && !is.null(page$thumbnail$source)) {
          source_url <- page$thumbnail$source
          source_kind <- "wikipedia_pageimage"
        }
      }, error = function(e) {
        status <<- paste("wikimedia error:", conditionMessage(e))
      })

      if (is_blank(source_url)) {
        source_url <- official_domain_icon_url(row$official_domain)
        source_kind <- "official_domain_icon"
      }

      if (!is_blank(source_url)) {
        ext <- if (!is_blank(logo_filename)) extension_from(logo_filename, "svg") else "png"
        logo_file <- paste0(row$logo_slug, ".", ext)
        dest <- file.path(logo_dir, logo_file)
        download_logo(source_url, dest)
        if (valid_logo_file(dest)) {
          status <- "downloaded"
        } else {
          unlink(dest)
          logo_file <- NA_character_
          status <- "error: downloaded file was not a valid image"
        }
      }
    }
  }, error = function(e) {
    status <<- paste("error:", conditionMessage(e))
  })

  if (!(status %in% c("exists", "downloaded"))) {
    stale <- list.files(
      logo_dir,
      pattern = paste0("^", row$logo_slug, "\\.(svg|png|jpg|jpeg|webp)$"),
      full.names = TRUE,
      ignore.case = TRUE
    )
    stale <- stale[!vapply(stale, valid_logo_file, logical(1))]
    if (length(stale) > 0) unlink(stale)

    tryCatch({
      fallback_url <- official_domain_icon_url(row$official_domain)
      if (!is_blank(fallback_url)) {
        logo_file <- paste0(row$logo_slug, ".png")
        source_url <- fallback_url
        source_kind <- "official_domain_icon"
        dest <- file.path(logo_dir, logo_file)
        download_logo(source_url, dest)
        if (valid_logo_file(dest)) {
          status <- "downloaded"
        } else {
          unlink(dest)
          logo_file <- NA_character_
          status <- "error: official-domain icon was not a valid image"
        }
      }
    }, error = function(e) {
      status <<- paste("error:", conditionMessage(e))
    })
  }

  results[[i]] <- row |>
    mutate(
      logo_file = logo_file,
      source_kind = source_kind,
      source_url = source_url,
      wikidata_id = qid,
      wikidata_logo_filename = logo_filename,
      status = status
    )
}

manifest <- bind_rows(results)
write.csv(manifest, file.path(logo_dir, "logo_manifest.csv"), row.names = FALSE, na = "")

cat("Downloaded", sum(manifest$status == "downloaded"), "logos to", logo_dir, "\n")
