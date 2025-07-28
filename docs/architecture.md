# Software Architecture

## Overview

The Codebook Generator App is a web-based application built using the R programming language and the Shiny framework. Its primary purpose is to support researchers and research data management practitioners in creating structured and standardized codebooks for tabular datasets, enhancing metadata quality and promoting FAIR (Findable, Accessible, Interoperable, Reusable) data practices.

The app allows users to upload `.csv`, `.tsv`, or `.xlsx` files, inspect and edit metadata such as variable labels, types, ranges, and units, and export a codebook in CSV format. 

## Design Principles

The application is architected around a **client-first design model**, where all processing and rendering occur entirely within the user's browser using [ShinyLive](https://github.com/rstudio/shinylive), which compiles R into WebAssembly. This approach maximizes data privacy and security, as no uploaded files or metadata ever leave the user's machine.

The app’s interface is organized into two main areas:

* A **sidebar panel** for data upload, language selection, and guidance.
* A **main panel** for rendering the dataset preview and interactive metadata table using `DT` and `rhandsontable`.

The logic is structured in two distinct layers:

* The **UI layer**, which defines the layout and dynamically renders content based on user actions and language selection.
* The **Server layer**, which handles data input, metadata extraction (e.g., variable type detection, range computation, and missing value analysis), and codebook export using JavaScript Blob APIs.

## Platform Compatibility

Because the app uses ShinyLive for deployment, it is platform-independent and requires only a modern web browser. It has been tested primarily on Google Chrome and Firefox and is expected to perform similarly on recent versions of Safari and Edge. The app supports responsive layout techniques using CSS media queries, making it usable on smaller desktop screens and most tablets.

## External Dependencies

The app leverages the following R packages:

* `shiny`, `shinyjs`, `shinyBS`, and `shinythemes` for the core interface and behavior.
* `rhandsontable` for editable metadata tables.
* `DT` for rendering tabular previews of the uploaded dataset.
* `readxl` for Excel file support.
* `shiny.i18n` for multi-language interface management.

These dependencies are actively maintained by the R community and are regularly updated for compatibility and security.
