# Software Architecture

## Overview

The Codebook Generator App is a web-based application built with R and Shiny. It helps researchers and research data management practitioners create structured codebooks for tabular datasets, reinforcing FAIR (Findable, Accessible, Interoperable, Reusable) practices. Users can upload `.csv`, `.tsv`, or `.xlsx` files, review and edit metadata such as labels, types, ranges, and units, and export the finished codebook as a CSV. A built-in language selector now renders every UI element in English or French without reloading the session.

## Design Principles

The app follows a classic client-server model: the browser hosts the interactive UI, while an R process performs all computations. Uploaded data lives only in RAM for the duration of a user session and is discarded automatically when the session ends.

The interface is split into two main regions:

* A **sidebar panel** that hosts file-upload controls, bilingual onboarding text, caution notes, and the English/French toggle.
* A **main panel** that displays the dataset preview (`DT`) and the editable metadata grid (`rhandsontable`).

Logic is separated into two layers:

* The **UI layer** defines the layout and dynamically renders text based on user actions plus the selected language. A lightweight translation dictionary and helper functions in `app.R` keep both languages synchronized.
* The **Server layer** parses uploads, derives metadata, tracks table edits, and pushes translated content back to the UI. Before variable types are inferred:
  * Common missing-value markers (`NA`, `N/A`, `na`, `n/a`, case-insensitive, plus any user-supplied tokens from the sidebar) are normalized to real `NA` values — in both text and numeric columns — and columns are re-cast to numeric where that leaves only numbers. This keeps a variable from being misclassified as character just because missing cells were typed as text, or from having sentinel values like `-99`/`999` skew its numeric range.
  * Character columns are tested against a small set of common date formats (ISO `YYYY-MM-DD`, and `DD/MM/YYYY` / `MM/DD/YYYY` with `/` or `-`); a column is only classified as `date` if every non-missing value matches one format and parses to a plausible year, so numeric-looking IDs are not misdetected as dates. Changing the custom missing-value markers re-runs this whole pipeline from the original upload, which resets any manual edits made in the attributes table.
  * CSV and TSV uploads are parsed with the matching reader (`read.csv` / `read.delim`) based on the file extension, rather than assuming a comma separator for both.
  * CSV/TSV bytes are checked for valid UTF-8 before parsing; files that aren't (e.g. Excel-on-Windows exports, commonly Latin-1/Windows-1252) are read with that encoding instead, and a leading UTF-8 BOM character is stripped from the first header. This is applied to both the main data upload and a re-uploaded previous codebook, so accented characters in French data or labels are never garbled regardless of which program produced the file.

A previously exported codebook CSV can also be re-uploaded to prefill the Label and Units of matching variables (matched by variable name, tolerant of either UI language's column headers) — useful when re-documenting an updated version of an already-documented dataset. Type, Range, and Missing Values are always re-derived from the current upload, never taken from the old codebook.

## Deployment

The live site (served by GitHub Pages from the repository root) is a static Shinylive export. `CodebookGenerator_Deploy.qmd` drives `shinylive::export()`, which writes a full bundle (including the webR runtime and R packages, ~70-100+ MB) to a local `WebApp/` folder that is git-ignored. Only `WebApp/app.json` — the small manifest embedding the current `app.R` source — is copied to the project root and committed, alongside the matching `index.html`/`shinylive/` runtime files. See the README's "Rebuilding and deploying the live app" section for the exact steps; skipping this step after an `app.R` change leaves the deployed app running stale code.

## Platform Compatibility

The application runs on Shiny Server, RStudio Connect, or Shinylive deployments and only requires a modern browser (Chrome, Firefox, Safari, Edge). Responsive styles keep the layout usable on smaller laptops and most tablets.

## External Dependencies

Key R packages include:

* `shiny`, `shinyjs`, `shinythemes`, and `shinyBS` for the reactive interface.
* `rhandsontable` for the editable codebook grid.
* `DT` for the data preview table.
* `readxl` for Excel uploads.

All translation logic is implemented with base R utilities; no additional i18n package is required.
