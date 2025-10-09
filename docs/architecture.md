# Software Architecture

## Overview

The Codebook Generator App is a web-based application built using the R programming language and the Shiny framework. Its primary purpose is to support researchers and research data management practitioners in creating structured and standardized codebooks for tabular datasets, enhancing metadata quality and promoting FAIR (Findable, Accessible, Interoperable, Reusable) data practices.

The app allows users to upload `.csv`, `.tsv`, or `.xlsx` files, inspect and edit metadata such as variable labels, types, ranges, and units, and export a codebook in CSV format. 

## Design Principles

The application is architected around a **server-client model** using R and the [Shiny framework](https://shiny.posit.co/). The user's browser (client) handles the interface, while the R process on the server handles all data processing. This means all uploaded files and metadata are transmitted to the server's memory for processing. Critically, all data is strictly non-persistent, held only in the server's RAM for the duration of the individual user's session.

The app’s interface is organized into two main areas:

* A **sidebar panel** for data upload, language selection, and guidance.
* A **main panel** for rendering the dataset preview and interactive metadata table using `DT` and `rhandsontable`.

The logic is structured in two distinct layers:

* The **UI layer**, which defines the layout and dynamically renders content based on user actions and language selection.
* The **Server layer** (the R process) handles all core logic: receiving data from the client via HTTP/WebSockets, file parsing, metadata extraction, and dynamically updating the UI. Data is stored exclusively in session-specific reactive values in the server's volatile memory (RAM). The data is automatically destroyed and is not persisted to any disk or database when the user's session is terminated.

## Platform Compatibility

The app is deployed using a standard **Shiny Server or RStudio Connect environment**. It requires only a modern web browser and is expected to perform similarly on recent versions of Chrome, Firefox, Safari, and Edge. It has been tested primarily on Google Chrome and Firefox and is expected to perform similarly on recent versions of Safari and Edge. The app supports responsive layout techniques using CSS media queries, making it usable on smaller desktop screens and most tablets.

## External Dependencies

The app leverages the following R packages:

* `shiny`, `shinyjs`, `shinyBS`, and `shinythemes` for the core interface and behavior.
* `rhandsontable` for editable metadata tables.
* `DT` for rendering tabular previews of the uploaded dataset.
* `readxl` for Excel file support.
* `shiny.i18n` for multi-language interface management.

These dependencies are actively maintained by the R community and are regularly updated for compatibility and security.
