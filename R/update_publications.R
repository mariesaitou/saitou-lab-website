#!/usr/bin/env Rscript

# Build data/publications_auto.csv from the public ORCID works list. OpenAlex
# supplies author/citation metadata and Crossref fills gaps. ORCID determines
# which works appear; this generated file should not be edited by hand.

required_packages <- c("jsonlite")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Missing R package(s): ", paste(missing_packages, collapse = ", "),
       ". Run install.packages('jsonlite').", call. = FALSE)
}

orcid <- Sys.getenv("LAB_ORCID", "0000-0002-8794-3217")
mailto <- Sys.getenv("CROSSREF_MAILTO", "")
output_path <- "data/publications_auto.csv"

empty_publications <- function() {
  data.frame(
    id = character(), doi = character(), title = character(), authors = character(),
    year = integer(), publication_date = character(), journal = character(), type = character(), publisher_url = character(),
    open_access = logical(), citation_count = integer(), source = character(),
    stringsAsFactors = FALSE
  )
}

read_json_url <- function(url) {
  con <- url(url, open = "rb", headers = c(
    "User-Agent" = "research-lab-site/0.1",
    "Accept" = "application/json"
  ))
  on.exit(close(con), add = TRUE)
  jsonlite::fromJSON(con, simplifyVector = FALSE)
}

null_to_na <- function(x) {
  if (is.null(x) || !length(x) || identical(x, "")) NA_character_ else as.character(x[[1]])
}

doi_key <- function(x) {
  x <- tolower(trimws(x))
  sub("^https?://(dx\\.)?doi\\.org/", "", x)
}

author_name <- function(authorship) {
  name <- authorship$author$display_name
  if (is.null(name)) "" else name
}

clean_text <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  value <- as.character(x[[1]])
  value <- gsub("<[^>]+>", " ", value)
  trimws(gsub("[[:space:]]+", " ", value))
}

fetch_orcid_summaries <- function(orcid) {
  endpoint <- paste0("https://pub.orcid.org/v3.0/", orcid, "/works")
  groups <- read_json_url(endpoint)$group
  summaries <- lapply(groups, function(group) {
    summary <- group$`work-summary`[[1]]
    identifiers <- group$`external-ids`$`external-id`
    doi_values <- vapply(identifiers, function(identifier) {
      if (identical(identifier$`external-id-type`, "doi") &&
          identical(identifier$`external-id-relationship`, "self")) {
        identifier$`external-id-value`
      } else ""
    }, character(1))
    doi <- doi_values[nzchar(doi_values)][1]
    if (!length(doi)) doi <- NA_character_
    type <- null_to_na(summary$type)
    type <- if (identical(type, "journal-article")) "article" else type
    journal <- clean_text(summary$`journal-title`$value)
    if (!is.na(journal) && grepl("biorxiv", journal, ignore.case = TRUE)) type <- "preprint"
    date_part <- function(name, fallback) {
      value <- suppressWarnings(as.integer(clean_text(summary$`publication-date`[[name]]$value)))
      if (is.na(value)) fallback else value
    }
    pub_year <- date_part("year", NA_integer_)
    pub_month <- date_part("month", NA_integer_)
    pub_day <- date_part("day", 1L)
    publication_date <- if (is.na(pub_year) || is.na(pub_month)) NA_character_ else
      sprintf("%04d-%02d-%02d", pub_year, pub_month, pub_day)
    data.frame(
      doi = doi_key(doi),
      title = clean_text(summary$title$title$value),
      year = pub_year,
      publication_date = publication_date,
      journal = journal,
      type = type,
      publisher_url = clean_text(summary$url$value),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, summaries)
}

fetch_openalex <- function(orcid) {
  # OpenAlex can temporarily contain more than one author record for the same
  # ORCID. Resolve all matches and use the record with the largest works_count
  # instead of relying on whichever duplicate the works filter happens to pick.
  author_endpoint <- paste0(
    "https://api.openalex.org/authors?filter=orcid:",
    utils::URLencode(orcid), "&per-page=20"
  )
  authors <- read_json_url(author_endpoint)$results
  if (!length(authors)) stop("No OpenAlex author found for ORCID ", orcid)
  works_counts <- vapply(authors, function(author) {
    if (is.null(author$works_count)) 0 else as.numeric(author$works_count)
  }, numeric(1))
  author_id <- sub("^https://openalex.org/", "", authors[[which.max(works_counts)]]$id)
  message("Using OpenAlex author ", author_id, " (", max(works_counts), " works)")

  filter <- utils::URLencode(paste0("author.id:", author_id), reserved = TRUE)
  cursor <- "*"
  works <- list()
  repeat {
    endpoint <- paste0(
      "https://api.openalex.org/works?filter=", filter,
      "&per-page=200&cursor=", utils::URLencode(cursor, reserved = TRUE)
    )
    page <- read_json_url(endpoint)
    works <- c(works, page$results)
    cursor <- page$meta$next_cursor
    if (is.null(cursor) || !length(page$results)) break
  }
  works
}

crossref_lookup <- function(doi) {
  if (is.na(doi) || !nzchar(doi)) return(NULL)
  endpoint <- paste0("https://api.crossref.org/works/", utils::URLencode(doi, reserved = TRUE))
  if (nzchar(mailto)) endpoint <- paste0(endpoint, "?mailto=", utils::URLencode(mailto))
  tryCatch(read_json_url(endpoint)$message, error = function(e) NULL)
}

works <- tryCatch(
  fetch_openalex(orcid),
  error = function(e) {
    message("OpenAlex update failed: ", conditionMessage(e))
    list()
  }
)

orcid_works <- tryCatch(
  fetch_orcid_summaries(orcid),
  error = function(e) stop("ORCID update failed: ", conditionMessage(e), call. = FALSE)
)

rows <- lapply(works, function(work) {
  doi <- doi_key(null_to_na(work$doi))
  journal <- null_to_na(work$primary_location$source$display_name)
  publisher_url <- null_to_na(work$primary_location$landing_page_url)
  crossref <- if (is.na(journal) || is.na(publisher_url)) crossref_lookup(doi) else NULL
  if (is.na(journal) && !is.null(crossref$`container-title`)) {
    journal <- null_to_na(crossref$`container-title`)
  }
  if (is.na(publisher_url) && !is.na(doi)) publisher_url <- paste0("https://doi.org/", doi)

  data.frame(
    id = null_to_na(work$id),
    doi = doi,
    title = null_to_na(work$title),
    authors = paste(vapply(work$authorships, author_name, character(1)), collapse = "; "),
    year = if (is.null(work$publication_year)) NA_integer_ else as.integer(work$publication_year),
    publication_date = null_to_na(work$publication_date),
    journal = journal,
    type = null_to_na(work$type),
    publisher_url = publisher_url,
    open_access = isTRUE(work$open_access$is_oa),
    citation_count = if (is.null(work$cited_by_count)) NA_integer_ else as.integer(work$cited_by_count),
    source = "OpenAlex/Crossref",
    stringsAsFactors = FALSE
  )
})

openalex_publications <- if (length(rows)) do.call(rbind, rows) else empty_publications()

# ORCID is the inclusion list. Match its works to richer OpenAlex rows by DOI,
# then by normalized title. If OpenAlex has not indexed a new ORCID item yet,
# retain the ORCID metadata and enrich it from Crossref when possible.
publications <- lapply(seq_len(nrow(orcid_works)), function(i) {
  item <- orcid_works[i, ]
  doi_match <- !is.na(item$doi) & nzchar(item$doi) &
    doi_key(openalex_publications$doi) == doi_key(item$doi)
  title_key <- tolower(gsub("[^[:alnum:]]", "", item$title))
  title_match <- tolower(gsub("[^[:alnum:]]", "", openalex_publications$title)) == title_key
  matches <- if (any(doi_match, na.rm = TRUE)) which(doi_match) else which(title_match)
  if (length(matches)) {
    row <- openalex_publications[matches[1], , drop = FALSE]
  } else {
    crossref <- crossref_lookup(item$doi)
    crossref_authors <- if (!is.null(crossref$author)) {
      paste(vapply(crossref$author, function(author) {
        paste(Filter(nzchar, c(null_to_na(author$given), null_to_na(author$family))), collapse = " ")
      }, character(1)), collapse = "; ")
    } else NA_character_
    row <- empty_publications()[rep(1, 1), , drop = FALSE]
    row$id <- NA_character_
    row$doi <- item$doi
    row$title <- item$title
    row$authors <- crossref_authors
    row$year <- item$year
    row$publication_date <- item$publication_date
    row$journal <- item$journal
    row$type <- item$type
    row$publisher_url <- item$publisher_url
    row$open_access <- NA
    row$citation_count <- NA_integer_
    row$source <- "ORCID/Crossref"
  }
  # Prefer the user-curated ORCID title, date, type and URL.
  for (column in c("doi", "title", "year", "publication_date", "journal", "type", "publisher_url")) {
    value <- item[[column]]
    if (!is.na(value) && nzchar(as.character(value))) row[[column]] <- value
  }
  row$source <- "ORCID/OpenAlex/Crossref"
  row
})
publications <- if (length(publications)) do.call(rbind, publications) else empty_publications()
if (nrow(publications)) {
  publication_start_year <- as.integer(Sys.getenv("LAB_PUBLICATION_START_YEAR", "2015"))
  publications <- publications[
    publications$type %in% c("article", "preprint") &
      (is.na(publications$year) | publications$year >= publication_start_year),
  ]
  publications <- publications[!duplicated(ifelse(is.na(publications$doi), publications$id, publications$doi)), ]
  # Prefer a journal article over its identically titled preprint.
  normalized_title <- tolower(gsub("[^[:alnum:]]", "", publications$title))
  type_priority <- ifelse(publications$type == "article", 1L, 0L)
  publications <- publications[order(type_priority, decreasing = TRUE), ]
  normalized_title <- normalized_title[order(type_priority, decreasing = TRUE)]
  publications <- publications[!duplicated(normalized_title), ]
  publications <- publications[order(publications$year, publications$title, decreasing = TRUE, na.last = TRUE), ]
}

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(publications, output_path, row.names = FALSE, na = "")
message("Wrote ", nrow(publications), " ORCID-listed publications to ", output_path)
