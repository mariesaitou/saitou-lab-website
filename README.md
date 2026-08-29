# Research Lab website

This is a Quarto website published with GitHub Pages. Publication metadata can
be updated locally from ORCID, OpenAlex, and Crossref, then reviewed and pushed
to GitHub with the rest of the site.

## Local preview

1. Install Quarto and R.
2. In R, run `install.packages("jsonlite")` once.
3. Run `Rscript R/update_publications.R`.
4. Run `quarto preview`.

## Publication data

- `data/publications_auto.csv`: generated; do not edit.
- `data/publication_notes.csv`: optional links, annotations, and display controls, keyed by DOI.
- `data/publication_manual.csv`: preprints and records not yet available from the APIs.

The update currently includes articles and preprints from 2015 onward. Exact-title
duplicates prefer the journal article over its preprint.

## GitHub Pages setup

After pushing to a GitHub repository, open **Settings → Pages** and select
**Deploy from a branch**, then choose the `gh-pages` branch and `/ (root)`.
The workflow publishes the checked-in site data whenever changes are pushed to
`main`; it does not fetch publication data automatically.
