normalize_doi <- function(x) {
  x <- tolower(trimws(ifelse(is.na(x), "", x)))
  sub("^https?://(dx\\.)?doi\\.org/", "", x)
}

read_publication_csv <- function(path) {
  if (!file.exists(path) || file.info(path)$size == 0) return(data.frame())
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"), check.names = FALSE)
}

ensure_columns <- function(data, columns) {
  for (column in columns) {
    if (!column %in% names(data)) data[[column]] <- rep(NA, nrow(data))
  }
  data
}

load_publications <- function(
    auto_path = "data/publications_auto.csv",
    notes_path = "data/publication_notes.csv",
    manual_path = "data/publication_manual.csv") {
  auto <- read_publication_csv(auto_path)
  manual <- read_publication_csv(manual_path)
  notes <- read_publication_csv(notes_path)

  core <- c("doi", "title", "authors", "year", "publication_date", "journal", "type", "publisher_url", "open_access", "citation_count")
  auto <- ensure_columns(auto, core)
  manual <- ensure_columns(manual, core)
  publications <- rbind(auto[core], manual[core])
  if (!nrow(publications)) return(publications)

  publications$key <- normalize_doi(publications$doi)
  missing_key <- !nzchar(publications$key)
  publications$key[missing_key] <- paste0("title:", tolower(trimws(publications$title[missing_key])))

  note_columns <- c("doi", "video_url", "code_url", "data_url", "preprint_url",
                    "short_annotation", "figure_note", "featured", "display_priority")
  notes <- ensure_columns(notes, note_columns)
  notes$key <- normalize_doi(notes$doi)
  notes <- notes[nzchar(notes$key), c("key", note_columns[-1]), drop = FALSE]
  publications <- merge(publications, notes, by = "key", all.x = TRUE, sort = FALSE)
  title_key <- tolower(gsub("[^[:alnum:]]", "", publications$title))
  has_doi <- nzchar(normalize_doi(publications$doi))
  publications <- publications[order(has_doi, decreasing = TRUE), ]
  title_key <- title_key[order(has_doi, decreasing = TRUE)]
  publications <- publications[!duplicated(title_key), ]
  publications <- publications[!duplicated(publications$key), ]
  # Hide a preprint when a sufficiently similar peer-reviewed article exists.
  title_tokens <- function(x) {
    unique(strsplit(tolower(gsub("[^[:alnum:] ]", " ", x)), "[[:space:]]+")[[1]])
  }
  articles <- which(tolower(publications$type) == "article")
  preprints <- which(tolower(publications$type) == "preprint")
  superseded <- vapply(preprints, function(preprint_i) {
    preprint_tokens <- title_tokens(publications$title[preprint_i])
    any(vapply(articles, function(article_i) {
      article_tokens <- title_tokens(publications$title[article_i])
      title_score <- length(intersect(preprint_tokens, article_tokens)) /
        length(union(preprint_tokens, article_tokens))
      author_set <- function(x) unique(tolower(trimws(strsplit(ifelse(is.na(x), "", x), ";", fixed = TRUE)[[1]])))
      preprint_authors <- author_set(publications$authors[preprint_i])
      article_authors <- author_set(publications$authors[article_i])
      author_score <- length(intersect(preprint_authors, article_authors)) /
        max(1, length(union(preprint_authors, article_authors)))
      title_score >= 0.52 || (title_score >= 0.23 && author_score >= 0.75)
    }, logical(1)))
  }, logical(1))
  if (length(superseded) && any(superseded)) {
    publications <- publications[-preprints[superseded], , drop = FALSE]
  }
  priority <- suppressWarnings(as.numeric(publications$display_priority))
  priority[is.na(priority)] <- 0
  publications <- publications[order(priority, publications$year, decreasing = TRUE, na.last = TRUE), ]
  rownames(publications) <- NULL
  publications
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}

link_html <- function(url, label) {
  if (is.na(url) || !nzchar(trimws(url))) return("")
  sprintf('<a href="%s">%s</a>', html_escape(url), html_escape(label))
}

render_publications <- function(publications) {
  if (!nrow(publications)) {
    cat("Publication data has not been generated yet. Run `Rscript R/update_publications.R`.\n")
    return(invisible(NULL))
  }
  lab_names <- tolower(c("Marie Saitou", "Célian Diblasi", "Celian Diblasi",
    "Domniki Manousi", "Maëlys Chapis", "Maelys Chapis", "Erik Sandertun Røed",
    "Jun Soung Kwak", "Junsoung Kwak", "Akira Harding", "Pauline Buso"))
  authors_html <- function(authors) {
    if (is.na(authors) || !nzchar(authors)) return("")
    people <- trimws(strsplit(authors, ";", fixed = TRUE)[[1]])
    people <- vapply(people, function(person) {
      escaped <- html_escape(person)
      if (tolower(person) %in% lab_names) paste0("<strong>", escaped, "</strong>") else escaped
    }, character(1))
    paste(people, collapse = "; ")
  }
  normalize_journal <- function(journal) {
    if (is.na(journal) || !nzchar(trimws(journal))) return("")
    key <- tolower(trimws(journal))
    key <- gsub("&amp;#58;", ":", key, fixed = TRUE)
    if (grepl("^genome biology and evolution$", key)) return("Genome Biology and Evolution")
    if (grepl("^g3", key)) return("G3: Genes, Genomes, Genetics")
    if (grepl("^bmc genomics$", key)) return("BMC Genomics")
    if (grepl("^cell reports$", key)) return("Cell Reports")
    if (grepl("^communications biology$", key)) return("Communications Biology")
    if (grepl("^science advances$", key)) return("Science Advances")
    if (grepl("^plos computational biology$", key)) return("PLOS Computational Biology")
    if (grepl("^biorxiv", key)) return("bioRxiv")
    journal
  }
  article_rows <- publications[tolower(publications$type) == "article", , drop = FALSE]
  recent_articles <- article_rows[is.na(article_rows$year) | article_rows$year > 2020, , drop = FALSE]
  earlier_articles <- article_rows[!is.na(article_rows$year) & article_rows$year <= 2020, , drop = FALSE]
  sections <- list(
    "Preprints" = publications[tolower(publications$type) == "preprint", , drop = FALSE],
    "Peer-reviewed publications" = recent_articles,
    "Earlier publications" = earlier_articles
  )
  for (section_name in names(sections)) {
    section <- sections[[section_name]]
    open_attr <- if (section_name == "Earlier publications") "" else " open"
    section_years <- suppressWarnings(as.integer(section$year))
    section_years <- section_years[!is.na(section_years)]
    year_range <- if (!length(section_years)) {
      ""
    } else if (min(section_years) == max(section_years)) {
      as.character(min(section_years))
    } else {
      paste0(min(section_years), "–", max(section_years))
    }
    cat('<details class="publication-section"', open_attr, '>\n', sep = "")
    cat('<summary><span>', html_escape(section_name),
        if (nzchar(year_range)) paste0('<small class="publication-years">', year_range, '</small>') else "",
        '</span><span class="publication-count">', nrow(section), '</span></summary>\n', sep = "")
    cat('<div class="publication-section-body">\n')
    if (!nrow(section)) {
      cat('<p>No items are available.</p>\n')
    } else {
      for (i in seq_len(nrow(section))) {
        item <- section[i, ]
        title <- html_escape(ifelse(is.na(item$title), "Untitled", item$title))
        journal <- html_escape(normalize_journal(item$journal))
        links <- Filter(nzchar, c(
          link_html(item$publisher_url, ifelse(section_name == "Preprints", "Preprint", "Publisher")),
          link_html(item$video_url, "Video"), link_html(item$code_url, "Code"), link_html(item$data_url, "Data")
        ))
        cat('<article class="publication">\n')
        cat('<h4>', title, '</h4>\n', sep = "")
        if (nzchar(authors_html(item$authors))) cat('<p class="pub-authors">', authors_html(item$authors), '</p>\n', sep = "")
        if (nzchar(journal)) cat('<p class="pub-details">', journal, '</p>\n', sep = "")
        if (length(links)) cat('<p class="pub-links">', paste(links, collapse = " · "), '</p>\n', sep = "")
        if (!is.na(item$short_annotation) && nzchar(item$short_annotation)) cat('<p>', html_escape(item$short_annotation), '</p>\n', sep = "")
        cat('</article>\n\n')
      }
    }
    cat('</div>\n</details>\n')
  }
  invisible(NULL)
}

render_latest_publications <- function(publications, n = 3, kind = c("article", "preprint")) {
  if (!nrow(publications)) {
    cat("Latest publications will appear after the publication update runs.\n")
    return(invisible(NULL))
  }
  kind <- match.arg(kind)
  publications <- publications[tolower(publications$type) == kind, , drop = FALSE]
  if (!nrow(publications)) {
    cat("<p>No items are available yet.</p>\n")
    return(invisible(NULL))
  }
  publications <- publications[order(publications$year, decreasing = TRUE, na.last = TRUE), ]
  publications <- head(publications, n)
  cat('<div class="publication-grid">\n')
  for (i in seq_len(nrow(publications))) {
    item <- publications[i, ]
    title <- html_escape(ifelse(is.na(item$title), "Untitled", item$title))
    venue <- html_escape(ifelse(is.na(item$journal), "", item$journal))
    year <- ifelse(is.na(item$year), "", as.character(item$year))
    details <- paste(Filter(nzchar, c(venue, year)), collapse = " · ")
    url <- item$publisher_url
    title_html <- if (!is.na(url) && nzchar(url)) link_html(url, item$title) else title
    cat('<article class="publication-card ', kind, '">', sep = "")
    cat('<span class="pub-kind">', ifelse(kind == "article", "Article", "Preprint"), '</span>', sep = "")
    cat('<h3>', title_html, '</h3>', sep = "")
    if (nzchar(details)) cat('<p>', details, '</p>', sep = "")
    cat('</article>\n')
  }
  cat('</div>\n')
  invisible(NULL)
}

render_home_news <- function(publications, year = 2026, extra_items = data.frame()) {
  publications <- publications[tolower(publications$type) %in% c("article", "preprint"), , drop = FALSE]
  publication_items <- data.frame(
    date = ifelse(is.na(publications$publication_date) | !nzchar(publications$publication_date),
                  sprintf("%d-01-01", publications$year), publications$publication_date),
    kind = ifelse(tolower(publications$type) == "article", "Publication", "Preprint"),
    title = publications$title,
    summary = ifelse(is.na(publications$journal), "", publications$journal),
    url = ifelse(is.na(publications$publisher_url), "", publications$publisher_url),
    stringsAsFactors = FALSE
  )
  required <- c("date", "kind", "title", "summary", "url")
  extra_items <- ensure_columns(extra_items, required)
  items <- rbind(publication_items[required], extra_items[required])
  if (!nrow(items)) return(invisible(NULL))

  dates <- suppressWarnings(as.Date(items$date))
  effective_year <- suppressWarnings(as.integer(format(dates, "%Y")))
  keep <- !is.na(effective_year) & effective_year == year
  items <- items[keep, , drop = FALSE]
  dates <- dates[keep]
  if (!nrow(items)) return(invisible(NULL))
  fallback_date <- as.Date(sprintf("%d-01-01", year))
  sort_dates <- dates
  sort_dates[is.na(sort_dates)] <- fallback_date
  items <- items[order(sort_dates, decreasing = TRUE), , drop = FALSE]
  dates <- suppressWarnings(as.Date(items$date))
  new_cutoff <- Sys.Date() - 92

  for (i in seq_len(nrow(items))) {
    item <- items[i, ]
    kind_class <- paste0("tag-", tolower(item$kind))
    title <- html_escape(ifelse(is.na(item$title), "Untitled", item$title))
    title_html <- if (!is.na(item$url) && nzchar(item$url)) link_html(item$url, item$title) else title
    summary <- html_escape(ifelse(is.na(item$summary), "", item$summary))
    item_date <- dates[i]
    date_label <- if (is.na(item_date)) as.character(year) else format(item_date, "%B %Y")
    is_new <- !is.na(item_date) && item_date >= new_cutoff

    cat('<article class="news-row', ifelse(is_new, ' is-new', ''), '">\n', sep = "")
    cat('<div class="news-meta">')
    cat('<span class="news-year">', date_label, '</span>', sep = "")
    cat('<span class="news-tags">')
    cat('<span class="news-tag ', kind_class, '">', html_escape(item$kind), '</span>', sep = "")
    if (is_new) cat('<span class="news-new">New!</span>')
    cat('</span></div>\n')
    cat('<div class="news-body"><h3>', title_html, '</h3>', sep = "")
    if (nzchar(summary)) cat('<p>', summary, '</p>', sep = "")
    cat('</div></article>\n')
  }
  invisible(NULL)
}
