# Research Lab website

This is a Quarto website published with GitHub Pages. The public ORCID Works
list determines which publications appear. OpenAlex and Crossref enrich those
records; only annotations and exceptional records are edited manually.

## Local preview

1. Install Quarto and R.
2. In R, run `install.packages("jsonlite")` once.
3. Run `Rscript R/update_publications.R`.
4. Run `quarto preview`.

## Publication data

- `data/publications_auto.csv`: generated; do not edit.
- `data/publication_notes.csv`: optional links, annotations, and display controls, keyed by DOI.
- `data/publication_manual.csv`: preprints and records not yet available from the APIs.

For polite Crossref API use, add a repository secret named `CROSSREF_MAILTO`
containing a contact email address. To use a different author, set `LAB_ORCID`.
The update currently includes articles and preprints from 2015 onward; change
`LAB_PUBLICATION_START_YEAR` if an earlier cutoff is needed. Exact-title
duplicates prefer the journal article over its preprint.

## GitHub Pages setup

After pushing to a GitHub repository, open **Settings → Pages** and select
**Deploy from a branch**, then choose the `gh-pages` branch and `/ (root)`.
The workflow runs on pushes to `main`, every Monday, and on demand.
