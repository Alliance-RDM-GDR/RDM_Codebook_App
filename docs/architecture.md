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
* The **Server layer** parses uploads, derives metadata, tracks table edits, and pushes translated content back to the UI.

## Platform Compatibility

The application runs on Shiny Server, RStudio Connect, or Shinylive deployments and only requires a modern browser (Chrome, Firefox, Safari, Edge). Responsive styles keep the layout usable on smaller laptops and most tablets.

## External Dependencies

Key R packages include:

* `shiny`, `shinyjs`, `shinythemes`, and `shinyBS` for the reactive interface.
* `rhandsontable` for the editable codebook grid.
* `DT` for the data preview table.
* `readxl` for Excel uploads.

All translation logic is implemented with base R utilities; no additional i18n package is required.
