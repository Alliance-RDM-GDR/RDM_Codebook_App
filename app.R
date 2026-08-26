# Load required libraries
library(shiny)
library(shinyjs) 
library(rhandsontable)
library(readxl) 
library(shinythemes)
library(shinyBS)

options(shiny.maxRequestSize = 30 * 1024^2)

# Reads raw bytes to determine whether an upload is valid UTF-8 or a
# legacy 8-bit encoding (the common case for CSV/TSV files exported by
# Excel on Windows, or by older tools), so accented characters in
# French data don't turn into garbled text regardless of how the file
# was saved. "latin1" is used as the fallback rather than "CP1252"
# because it's one of the encodings R supports natively on every
# platform (including the WebR/Shinylive sandbox); it decodes accented
# Latin letters identically to CP1252, only differing in a handful of
# punctuation marks (curly quotes, em dash) in the 0x80-0x9F range.
detect_file_encoding <- function(path) {
  raw_bytes <- tryCatch(
    readBin(path, what = "raw", n = min(file.info(path)$size, 2e6)),
    error = function(e) raw(0)
  )
  if (length(raw_bytes) == 0) {
    return("UTF-8")
  }
  text <- tryCatch(rawToChar(raw_bytes[raw_bytes != as.raw(0)]), error = function(e) NA_character_)
  if (!is.na(text) && isTRUE(validUTF8(text))) "UTF-8" else "latin1"
}

# Reads a CSV/TSV upload with its detected encoding and strips a
# leading byte-order-mark character some editors leave on the first
# header when saving as "UTF-8 with BOM".
read_uploaded_table <- function(path, reader) {
  encoding <- detect_file_encoding(path)
  df <- reader(path, stringsAsFactors = FALSE, fileEncoding = encoding)
  bom_char <- intToUtf8(0xFEFF)
  names(df)[1] <- sub(paste0("^", bom_char), "", names(df)[1])
  df
}

translations <- list(
  en = list(
    app_title = "Codebook generator for data tables",
    welcome_msg = "Welcome to the Codebook Generator App! This tool helps you create a codebook for your data tables with ease. Please contact us at curators .at frdr-dfdr.ca for improvements or comments.",
    how_to_title = "How to use the app:",
    how_to_upload = "Upload your CSV, TSV, or Excel data file (Up to 30 MB) using the 'Upload your data file' button below.",
    how_to_preview = "After uploading, preview your data in the main panel and a preliminary codebook in the bottom panel.",
    how_to_docs_intro = "Check our",
    documentation_text = "documentation",
    and_text = "and",
    github_text = "GitHub repository",
    how_to_docs_suffix = "for instruction on how to fill in the codebook, specifically:",
    double_click_instruction = "Double click to write the '%s' and '%s' in the variable attributes table, and select the correct variable '%s'.",
    columns_update_instruction = "The '%s' and '%s' columns update automatically based on your selections.",
    download_instruction = "When you're ready, click 'Download the codebook' to save your codebook as a CSV file.",
    caution_title = "Caution notes:",
    caution_no_storage = "This app does not store any data. Complete the process and download the codebook.",
    caution_refresh = "Make sure you refresh the page before uploading and documenting a new data table.",
    upload_label = "Upload your data file",
    storage_note = "This app does not store data in any way.",
    download_button = "Download the Codebook",
    data_preview_title = "Data Preview",
    variable_attributes_title = "Variable Attributes",
    unsupported_file_title = "Unsupported File Type",
    unsupported_file_message = "Please upload a CSV, TSV, or Excel (.xlsx) file.",
    numeric_range_prefix = "Range:",
    date_range_prefix = "Date Range:",
    levels_prefix = "Levels:",
    values_prefix = "Values:",
    incompatible_type = "incompatible data type",
    no_missing_values = "No missing values",
    missing_tokens_label = "Custom missing value markers (optional)",
    missing_tokens_placeholder = "e.g. -99, 999, unknown",
    missing_tokens_help = "Comma-separated values treated as missing, in addition to NA/N/A. Case-insensitive; works for both text and numeric columns. Changing this re-detects variable types and resets manual edits.",
    previous_codebook_label = "Re-upload a previous codebook (optional)",
    previous_codebook_help = "Upload a codebook CSV exported earlier to prefill matching variables' Label and Units. Matched by variable name; unmatched variables are left as-is.",
    codebook_filename_suffix = "codebook",
    column_variable = "Variable",
    column_label = "Label",
    column_type = "Type",
    column_range = "Range or Levels",
    column_missing = "Missing Values",
    column_units = "Units",
    app_logo_alt = "Logo of the Digital Research Alliance of Canada"
  ),
  fr = list(
    app_title = "Générateur de dictionnaire de données pour tableaux",
    welcome_msg = "Bienvenue dans l'application Générateur de dictionnaire de données! Cet outil vous aide à créer facilement un dictionnaire pour vos tableaux de données. Pour des suggestions ou des commentaires, écrivez-nous à curators .at frdr-dfdr.ca.",
    how_to_title = "Comment utiliser l'application :",
    how_to_upload = "Téléversez votre fichier de données CSV, TSV ou Excel (jusqu'à 30 Mo) à l'aide du bouton 'Téléverser votre fichier de données' ci-dessous.",
    how_to_preview = "Après le téléversement, affichez vos données dans le panneau principal ainsi qu'un dictionnaire préliminaire dans le panneau inférieur.",
    how_to_docs_intro = "Consultez notre",
    documentation_text = "documentation",
    and_text = "et",
    github_text = "dépôt GitHub",
    how_to_docs_suffix = "pour obtenir les instructions sur la façon de remplir le dictionnaire, notamment :",
    double_click_instruction = "Double-cliquez pour remplir les colonnes '%s' et '%s' dans la table des attributs de variables et sélectionnez le type de variable '%s' approprié.",
    columns_update_instruction = "Les colonnes '%s' et '%s' se mettent automatiquement à jour selon vos sélections.",
    download_instruction = "Lorsque tout est prêt, cliquez sur 'Télécharger le dictionnaire' pour enregistrer votre dictionnaire en fichier CSV.",
    caution_title = "Mises en garde :",
    caution_no_storage = "Cette application n'enregistre aucune donnée. Terminez le processus et téléchargez le dictionnaire.",
    caution_refresh = "Rechargez la page avant de téléverser et documenter une nouvelle table de données.",
    upload_label = "Téléverser votre fichier de données",
    storage_note = "Cette application n'enregistre aucune donnée.",
    download_button = "Télécharger le dictionnaire",
    data_preview_title = "Aperçu des données",
    variable_attributes_title = "Attributs des variables",
    unsupported_file_title = "Type de fichier non pris en charge",
    unsupported_file_message = "Téléversez un fichier CSV, TSV ou Excel (.xlsx).",
    numeric_range_prefix = "Plage :",
    date_range_prefix = "Plage de dates :",
    levels_prefix = "Modalités :",
    values_prefix = "Valeurs :",
    incompatible_type = "type de données incompatible",
    no_missing_values = "Aucune valeur manquante",
    missing_tokens_label = "Marqueurs de valeurs manquantes personnalisés (facultatif)",
    missing_tokens_placeholder = "ex. : -99, 999, inconnu",
    missing_tokens_help = "Valeurs séparées par des virgules traitées comme manquantes, en plus de NA/N/A. Insensible à la casse; fonctionne pour les colonnes texte et numériques. Modifier ce champ relance la détection des types et réinitialise les modifications manuelles.",
    previous_codebook_label = "Reprendre un dictionnaire précédent (facultatif)",
    previous_codebook_help = "Téléversez un dictionnaire CSV exporté précédemment pour préremplir les colonnes Étiquette et Unités des variables correspondantes. La correspondance se fait par nom de variable; les variables non trouvées restent inchangées.",
    codebook_filename_suffix = "dictionnaire",
    column_variable = "Variable",
    column_label = "Étiquette",
    column_type = "Type",
    column_range = "Plage ou modalités",
    column_missing = "Valeurs manquantes",
    column_units = "Unités",
    app_logo_alt = "Logo de l'Alliance de recherche numérique du Canada"
  )
)

attribute_column_keys <- c(
  Variable = "column_variable",
  Label = "column_label",
  Type = "column_type",
  Range_or_Levels = "column_range",
  Missing_Values = "column_missing",
  Units = "column_units"
)

normalize_lang <- function(lang) {
  if (is.null(lang) || !lang %in% names(translations)) {
    "en"
  } else {
    lang
  }
}

translate_text <- function(lang, key) {
  lang <- normalize_lang(lang)
  lang_values <- translations[[lang]]
  if (!is.null(lang_values[[key]])) {
    lang_values[[key]]
  } else if (!is.null(translations[["en"]][[key]])) {
    translations[["en"]][[key]]
  } else {
    key
  }
}

describe_column_values <- function(column, type, lang) {
  lang <- normalize_lang(lang)
  type <- tolower(as.character(type))
  incompatible <- translate_text(lang, "incompatible_type")
  
  if (type == "numeric") {
    if (!is.numeric(column)) {
      suppressWarnings(column <- as.numeric(column))
    }
    non_missing <- column[!is.na(column)]
    if (!length(non_missing)) {
      return(incompatible)
    }
    prefix <- translate_text(lang, "numeric_range_prefix")
    range_text <- paste(min(non_missing), "-", max(non_missing))
    return(paste(prefix, range_text))
  }
  
  if (type == "factor") {
    values <- sort(unique(as.character(column)))
    values <- values[!is.na(values)]
    if (!length(values)) {
      return("")
    }
    prefix <- translate_text(lang, "levels_prefix")
    return(paste(prefix, paste(values, collapse = ", ")))
  }
  
  if (type == "character") {
    values <- sort(unique(as.character(column)))
    values <- values[!is.na(values)]
    if (!length(values)) {
      return("")
    }
    prefix <- translate_text(lang, "values_prefix")
    return(paste(prefix, paste(values, collapse = ", ")))
  }
  
  if (type == "date") {
    parsed <- parse_date_column(column)
    if (is.null(parsed)) {
      suppressWarnings(parsed <- as.Date(column))
    }
    column <- parsed
    non_missing <- column[!is.na(column)]
    if (!length(non_missing)) {
      return(incompatible)
    }
    prefix <- translate_text(lang, "date_range_prefix")
    range_text <- paste(min(non_missing), "-", max(non_missing))
    return(paste(prefix, range_text))
  }
  
  ""
}

describe_missing_values <- function(column, lang, missing_strings = c("na", "n/a")) {
  lang <- normalize_lang(lang)
  x_char <- as.character(column)
  x_trim <- trimws(x_char)
  x_lower <- tolower(x_trim)
  x_lower[is.na(x_lower)] <- ""
  full_missing_strings <- unique(c(tolower(trimws(missing_strings)), ""))
  num_missing <- sum(is.na(column) | x_lower %in% full_missing_strings)
  if (num_missing > 0) {
    as.character(num_missing)
  } else {
    translate_text(lang, "no_missing_values")
  }
}

# Recognized date formats, tried in order. Regexes require an explicit
# separator so plain numeric IDs (e.g. "20230101") are never mistaken
# for dates. When both day-first and month-first candidates match the
# same "\d\d/\d\d/\d\d\d\d" shape, day-first is tried first (more common
# outside the US); a component >12 in the wrong slot makes as.Date()
# return NA and the next candidate is tried instead.
date_format_candidates <- list(
  list(regex = "^\\d{4}-\\d{1,2}-\\d{1,2}$", format = "%Y-%m-%d"),
  list(regex = "^\\d{4}/\\d{1,2}/\\d{1,2}$", format = "%Y/%m/%d"),
  list(regex = "^\\d{1,2}/\\d{1,2}/\\d{4}$", format = "%d/%m/%Y"),
  list(regex = "^\\d{1,2}/\\d{1,2}/\\d{4}$", format = "%m/%d/%Y"),
  list(regex = "^\\d{1,2}-\\d{1,2}-\\d{4}$", format = "%d-%m-%Y"),
  list(regex = "^\\d{1,2}-\\d{1,2}-\\d{4}$", format = "%m-%d-%Y")
)

# Tries each candidate format and returns a Date vector only if every
# non-missing value matches the same format, parses to a real date, and
# falls in a plausible year range. Returns NULL if no candidate fits,
# so callers can fall back to base as.Date() or bail out.
parse_date_column <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  values <- trimws(as.character(x))
  is_present <- !is.na(values) & values != ""
  non_missing <- values[is_present]
  if (length(non_missing) == 0) {
    return(as.Date(rep(NA_character_, length(values))))
  }

  for (candidate in date_format_candidates) {
    if (!all(grepl(candidate$regex, non_missing))) next
    parsed <- suppressWarnings(as.Date(values, format = candidate$format))
    parsed_present <- parsed[is_present]
    if (anyNA(parsed_present)) next
    years <- as.numeric(format(parsed_present, "%Y"))
    if (any(years < 1000 | years > 2999)) next
    return(parsed)
  }
  NULL
}

detect_date_column <- function(x) {
  if (inherits(x, "Date")) {
    return(TRUE)
  }
  values <- trimws(as.character(x))
  values <- values[!is.na(values) & values != ""]
  if (length(values) < 2) {
    return(FALSE)
  }
  !is.null(parse_date_column(x))
}

normalize_missing_tokens <- function(df, missing_strings = c("na", "n/a")) {
  missing_strings <- unique(tolower(trimws(missing_strings)))
  numeric_tokens <- suppressWarnings(as.numeric(missing_strings))
  numeric_tokens <- numeric_tokens[!is.na(numeric_tokens)]

  for (col_name in names(df)) {
    col <- df[[col_name]]

    if (is.numeric(col)) {
      if (length(numeric_tokens) > 0) {
        df[[col_name]][col %in% numeric_tokens] <- NA
      }
      next
    }

    if (!is.character(col) && !is.factor(col)) next

    col_char <- as.character(col)
    col_lower <- tolower(trimws(col_char))
    is_missing_token <- !is.na(col_char) & col_lower %in% missing_strings
    col_char[is_missing_token] <- NA

    non_missing <- col_char[!is.na(col_char)]
    if (length(non_missing) > 0) {
      as_num <- suppressWarnings(as.numeric(non_missing))
      if (!any(is.na(as_num))) {
        col_char <- suppressWarnings(as.numeric(col_char))
      }
    }
    df[[col_name]] <- col_char
  }
  df
}

build_variable_attributes <- function(df, lang, missing_tokens = c("na", "n/a")) {
  df <- normalize_missing_tokens(df, missing_tokens)

  # Map variable types to predefined levels
  mapped_types <- sapply(df, function(x) {
    t <- class(x)[1]
    if (t %in% c("integer", "numeric", "double")) {
      "numeric"
    } else if (t %in% c("date", "POSIXct", "POSIXt")) {
      "date"
    } else if (t %in% c("factor")) {
      "factor"
    } else if (t %in% c("character")) {
      if (detect_date_column(x)) "date" else "character"
    } else {
      "character"
    }
  })

  # Initialize attributes with default columns
  attr <- data.frame(
    Variable = colnames(df),
    Label = rep("", ncol(df)),
    Type = mapped_types,
    Range_or_Levels = rep("", ncol(df)),
    Missing_Values = rep("", ncol(df)),
    Units = rep("", ncol(df)),
    stringsAsFactors = FALSE
  )

  attr$Range_or_Levels <- mapply(function(var, type) {
    describe_column_values(df[[var]], type, lang)
  }, attr$Variable, attr$Type, SIMPLIFY = TRUE, USE.NAMES = FALSE)

  attr$Missing_Values <- sapply(attr$Variable, function(var) {
    describe_missing_values(df[[var]], lang, missing_tokens)
  })

  attr$Type <- factor(attr$Type, levels = c("numeric", "character", "factor", "date"))

  list(df = df, attr = attr)
}

# Finds, for each canonical attribute column, which name in an
# uploaded codebook matches it in ANY supported language (not just the
# UI's current language) - so re-uploading a French-exported codebook
# while the UI is in English still matches "Étiquette" to Label, etc.
match_canonical_columns <- function(df_names) {
  canonical_names <- names(attribute_column_keys)
  setNames(vapply(canonical_names, function(canonical) {
    key <- attribute_column_keys[[canonical]]
    variants <- unique(vapply(names(translations), function(l) translate_text(l, key), character(1)))
    match_idx <- which(df_names %in% variants)
    if (length(match_idx) >= 1) df_names[match_idx[1]] else NA_character_
  }, character(1)), canonical_names)
}

# Builds a Variable -> list(Label, Units) lookup from a previously
# exported codebook CSV, to prefill those two free-text fields when
# the same dataset is re-documented. Returns NULL if the file doesn't
# look like a codebook (no recognizable Variable column).
build_codebook_lookup <- function(df) {
  mapped <- match_canonical_columns(names(df))
  var_col <- mapped[["Variable"]]
  if (is.na(var_col)) {
    return(NULL)
  }
  label_col <- mapped[["Label"]]
  units_col <- mapped[["Units"]]

  lookup <- list()
  variable_names <- as.character(df[[var_col]])
  for (i in seq_along(variable_names)) {
    var_name <- variable_names[i]
    if (is.na(var_name) || !nzchar(var_name)) next
    lookup[[var_name]] <- list(
      Label = if (!is.na(label_col)) as.character(df[[label_col]][i]) else "",
      Units = if (!is.na(units_col)) as.character(df[[units_col]][i]) else ""
    )
  }
  lookup
}

# Prefills Label/Units on attr from a previous-codebook lookup, matched
# by Variable name. Leaves Type/Range/Missing untouched since those
# should always reflect the current upload, not the old one.
apply_previous_codebook <- function(attr, lookup) {
  if (is.null(lookup) || is.null(attr) || nrow(attr) == 0) {
    return(attr)
  }
  for (i in seq_len(nrow(attr))) {
    prev <- lookup[[attr$Variable[i]]]
    if (is.null(prev)) next
    if (nzchar(prev$Label)) attr$Label[i] <- prev$Label
    if (nzchar(prev$Units)) attr$Units[i] <- prev$Units
  }
  attr
}

restore_attribute_column_names <- function(df, lang) {
  if (is.null(df)) {
    return(df)
  }
  lang <- normalize_lang(lang)
  canonical_names <- names(attribute_column_keys)
  translated_names <- setNames(
    vapply(attribute_column_keys, function(key) translate_text(lang, key), character(1)),
    canonical_names
  )
  current_names <- names(df)
  for (canonical in canonical_names) {
    translated <- translated_names[[canonical]]
    matches <- which(current_names == translated)
    if (length(matches) == 1) {
      current_names[matches] <- canonical
    }
  }
  names(df) <- current_names
  
  for (name in canonical_names) {
    if (!name %in% names(df)) {
      df[[name]] <- ""
    }
  }
  df[, canonical_names, drop = FALSE]
}

# Define UI
ui <- fluidPage(
  style = "margin: 30px; padding: 30px;",
  theme = shinytheme("spacelab"),
  
  useShinyjs(), 
  
  # Custom CSS: Gray sidebar background, black text, smaller text on small screens
  tags$style(HTML("
    /* The 'Label' and 'Units' column headers in the table */
    table.htCore thead tr th:nth-child(2),
    table.htCore thead tr th:nth-child(6) {
      background-color: green !important;
      color: white !important;
    }
    
    /* Custom class for the sidebar panel */
    .sidebar-panel-custom {
      background-color: #fafafa !important; /* lighter gray background */
      color: #000 !important;              /* all text in black */
      font-size: 16px;                     /* default font size */
      padding: 15px;                       /* extra padding if you like */
    }
    
    @media (max-width: 768px) {
      /* On screens narrower than 768px, reduce the font a bit */
      .sidebar-panel-custom {
        font-size: 12px !important;
      }
    }
  ")),
  
  # ----------------
  # Top Row for Title
  # ----------------
  fluidRow(
    column(
      width = 12,
      div(
        uiOutput("app_title"),
        style = "margin-bottom: 20px;"
      )
    )
  ),
  
  # ----------------
  # Sidebar + Main Panel
  # ----------------
  sidebarLayout(
    sidebarPanel(
      tag = "aside",
      width = 4,
      class = "sidebar-panel-custom",  # Apply our custom class here
      
      # Logo: simpler style, shrinks on narrow sidebars
      tags$img(src = "alliance_logo.png", 
               alt = "Logo of the Digital Research Alliance of Canada / Logo de l'Alliance de recherche numérique du Canada",
               style = "max-width: 100%; height: auto; margin-bottom: 20px;"),
      
      selectInput(
        "language",
        "Language / Langue",
        choices = c("English" = "en", "Français" = "fr"),
        selected = "en"
      ),
      
      uiOutput("sidebar_instructions"),
      
      uiOutput("upload_label"),
      fileInput("datafile", label = NULL,
                accept = c(".csv", ".tsv", ".xlsx")),

      uiOutput("storage_note"),

      uiOutput("previous_codebook_label"),
      fileInput("previous_codebook_file", label = NULL, accept = c(".csv")),
      uiOutput("previous_codebook_help"),

      uiOutput("missing_tokens_label"),
      textInput("custom_missing_tokens", label = NULL, value = ""),
      uiOutput("missing_tokens_help"),
      
      actionButton(
        "download_codebook",
        label = translations$en$download_button,
        icon = icon("download")
      ),
      
      # JavaScript for the codebook download
      tags$script(HTML("
        Shiny.addCustomMessageHandler('downloadCodebook', function(message) {
          var csvContent = message.csv;
          var blob = new Blob([csvContent], { type: 'text/csv;charset=utf-8;' });
          var link = document.createElement('a');
          if (link.download !== undefined) {
            var url = URL.createObjectURL(blob);
            link.setAttribute('href', url);
            link.setAttribute('download', message.filename);
            link.style.visibility = 'hidden';
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
          }
        });
      "))
    ),
    
    mainPanel(
      width = 8,
      
      # Wrap the main content in a div with overflow-x: auto
      # so wide tables don't overlap the sidebar
      div(
        style = "max-width: 800px; auto;",
        
        uiOutput("data_preview_title"),
        DT::dataTableOutput("data_preview"),
        
        br(),
        
        uiOutput("variable_attributes_title"),
        rHandsontableOutput("variable_attributes_table")
      )
    )
  )
)

# ----------------
# Server
# ----------------
server <- function(input, output, session) {
  # Reactive value to store data
  data <- reactiveVal()

  # Reactive value to store the untouched upload, used to re-derive the
  # codebook when the custom missing-value markers change
  raw_data <- reactiveVal()

  # Reactive value to store attributes
  attributes <- reactiveVal()

  # Variable -> Label/Units lookup built from a re-uploaded previous
  # codebook, applied whenever the attribute table is (re)built
  previous_codebook_lookup <- reactiveVal(NULL)

  current_lang <- reactive({
    normalize_lang(input$language)
  })

  missing_tokens <- reactive({
    base_tokens <- c("na", "n/a")
    extra <- input$custom_missing_tokens
    if (is.null(extra) || !nzchar(trimws(extra))) {
      return(base_tokens)
    }
    extra_tokens <- strsplit(extra, ",")[[1]]
    extra_tokens <- tolower(trimws(extra_tokens))
    extra_tokens <- extra_tokens[nzchar(extra_tokens)]
    unique(c(base_tokens, extra_tokens))
  })

  missing_tokens_debounced <- debounce(missing_tokens, 500)
  
  translation_reactive <- reactive({
    lang <- current_lang()
    function(key) translate_text(lang, key)
  })
  
  column_headers <- reactive({
    tr <- translation_reactive()
    c(
      tr("column_variable"),
      tr("column_label"),
      tr("column_type"),
      tr("column_range"),
      tr("column_missing"),
      tr("column_units")
    )
  })
  
  output$app_title <- renderUI({
    tr <- translation_reactive()
    h1(tr("app_title"), style = "margin: 0; text-align: center;")
  })
  
  output$sidebar_instructions <- renderUI({
    tr <- translation_reactive()
    highlight <- function(text) {
      as.character(tags$span(style = "color: green;", text))
    }
    double_click_text <- HTML(sprintf(
      tr("double_click_instruction"),
      highlight(tr("column_label")),
      highlight(tr("column_units")),
      highlight(tr("column_type"))
    ))
    auto_update_text <- HTML(sprintf(
      tr("columns_update_instruction"),
      highlight(tr("column_range")),
      highlight(tr("column_missing"))
    ))
    
    tagList(
      p(tr("welcome_msg"), style = "font-size: 18px;"),
      h4(tr("how_to_title")),
      tags$ul(
        style = "font-size: 16px;",
        tags$li(tr("how_to_upload")),
        tags$li(tr("how_to_preview")),
        tags$li(
          tagList(
            tr("how_to_docs_intro"), " ",
            tags$a(
              href = "https://alliance-rdm-gdr.github.io/CUR_Res_OnePagers/RDM_Codebook_en.html",
              tr("documentation_text")
            ),
            " ",
            tr("and_text"), " ",
            tags$a(
              href = "https://github.com/Alliance-RDM-GDR/RDM_Codebook_App",
              tr("github_text")
            ),
            " ",
            tr("how_to_docs_suffix")
          ),
          tags$ul(
            tags$li(double_click_text),
            tags$li(auto_update_text)
          )
        ),
        tags$li(tr("download_instruction"))
      ),
      h4(tr("caution_title")),
      tags$ul(
        tags$li(tr("caution_no_storage")),
        tags$li(tr("caution_refresh"))
      )
    )
  })
  
  output$upload_label <- renderUI({
    tr <- translation_reactive()
    tags$label(`for` = "datafile", tr("upload_label"), class = "control-label")
  })
  
  output$storage_note <- renderUI({
    tr <- translation_reactive()
    tags$p(tr("storage_note"), style = "font-size: 14px; font-style: italic; color: #555;")
  })

  output$previous_codebook_label <- renderUI({
    tr <- translation_reactive()
    tags$label(`for` = "previous_codebook_file", tr("previous_codebook_label"), class = "control-label")
  })

  output$previous_codebook_help <- renderUI({
    tr <- translation_reactive()
    tags$p(tr("previous_codebook_help"), style = "font-size: 13px; font-style: italic; color: #555;")
  })

  output$missing_tokens_label <- renderUI({
    tr <- translation_reactive()
    tags$label(`for` = "custom_missing_tokens", tr("missing_tokens_label"), class = "control-label")
  })

  output$missing_tokens_help <- renderUI({
    tr <- translation_reactive()
    tags$p(tr("missing_tokens_help"), style = "font-size: 13px; font-style: italic; color: #555;")
  })

  observe({
    tr <- translation_reactive()
    updateTextInput(session, "custom_missing_tokens", placeholder = tr("missing_tokens_placeholder"))
  })
  
  output$data_preview_title <- renderUI({
    tr <- translation_reactive()
    h2(tr("data_preview_title"))
  })
  
  output$variable_attributes_title <- renderUI({
    tr <- translation_reactive()
    h2(tr("variable_attributes_title"))
  })
  
  observe({
    tr <- translation_reactive()
    updateActionButton(
      session,
      "download_codebook",
      label = tr("download_button"),
      icon = icon("download")
    )
  })
  
  # Load data when file is uploaded
  observeEvent(input$datafile, {
    req(input$datafile)
    lang <- current_lang()
    
    file_ext <- tools::file_ext(input$datafile$name)
    
    if (file_ext == "csv") {
      df <- read_uploaded_table(input$datafile$datapath, read.csv)
    } else if (file_ext == "tsv") {
      df <- read_uploaded_table(input$datafile$datapath, read.delim)
    } else if (file_ext %in% c("xlsx")) {
      df <- read_excel(input$datafile$datapath)
      df <- as.data.frame(df)
    } else {
      showModal(modalDialog(
        title = translate_text(lang, "unsupported_file_title"),
        translate_text(lang, "unsupported_file_message"),
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }

    raw_data(df)

    result <- build_variable_attributes(df, lang, missing_tokens())
    data(result$df)
    attributes(apply_previous_codebook(result$attr, previous_codebook_lookup()))
  })

  # Re-derive the codebook when the custom missing-value markers change,
  # without requiring a fresh upload. This re-detects types from the
  # original upload, so it resets any manual edits made in the table.
  observeEvent(missing_tokens_debounced(), {
    req(raw_data())
    lang <- current_lang()
    result <- build_variable_attributes(raw_data(), lang, missing_tokens_debounced())
    data(result$df)
    attributes(apply_previous_codebook(result$attr, previous_codebook_lookup()))
  }, ignoreInit = TRUE)

  # Prefill Label/Units from a previously exported codebook, matched by
  # variable name. Works whether it's uploaded before or after the data
  # file: it's stored as a lookup and (re)applied to the current
  # attribute table here too, in case data is already loaded.
  observeEvent(input$previous_codebook_file, {
    req(input$previous_codebook_file)
    prev_df <- tryCatch(
      read_uploaded_table(input$previous_codebook_file$datapath, read.csv),
      error = function(e) NULL
    )
    lookup <- if (is.null(prev_df)) NULL else build_codebook_lookup(prev_df)
    previous_codebook_lookup(lookup)

    attr <- attributes()
    if (!is.null(attr)) {
      attributes(apply_previous_codebook(attr, lookup))
    }
  })

  # Display data preview
  output$data_preview <- DT::renderDataTable({
    req(data())
    DT::datatable(data(), options = list(scrollX = TRUE, scrollY = "400px"))
  })
  
  # Display variable attributes table
  output$variable_attributes_table <- renderRHandsontable({
    req(attributes())
    attr <- attributes()
    headers <- column_headers()
    col_names <- names(attr)
    label_col <- match("Label", col_names)
    type_col <- match("Type", col_names)
    range_col <- match("Range_or_Levels", col_names)
    missing_col <- match("Missing_Values", col_names)
    units_col <- match("Units", col_names)
    
    hot <- rhandsontable(attr, rowHeaders = NULL, colHeaders = headers)
    
    if (!is.na(label_col)) {
      hot <- hot %>%
        hot_col(label_col, type = "text", width = 200)
    }
    
    if (!is.na(type_col)) {
      hot <- hot %>%
        hot_col(type_col, type = "dropdown", source = c("numeric", "character", "factor", "date"))
    }
    
    if (!is.na(range_col)) {
      hot <- hot %>%
        hot_col(range_col, readOnly = TRUE, renderer = "
          function(instance, td, row, col, prop, value, cellProperties) {
            Handsontable.renderers.TextRenderer.apply(this, arguments);
            td.style.maxWidth = '150px';
            td.style.whiteSpace = 'nowrap';
            td.style.overflow = 'hidden';
            td.style.textOverflow = 'ellipsis';
            td.title = value;
          }
        ")
    }
    
    if (!is.na(missing_col)) {
      hot <- hot %>%
        hot_col(missing_col, readOnly = TRUE, width = 120)
    }
    
    if (!is.na(units_col)) {
      hot <- hot %>%
        hot_col(units_col, type = "text", width = 100)
    }
    
    hot
  })
  
  # Update attributes after table edits
  observeEvent(input$variable_attributes_table, {
    attr <- hot_to_r(input$variable_attributes_table)
    lang <- current_lang()
    if (is.null(attr)) {
      return()
    }
    attr <- restore_attribute_column_names(as.data.frame(attr, stringsAsFactors = FALSE), lang)
    df <- data()
    req(df)
    
    for (i in seq_len(nrow(attr))) {
      variable_name <- attr$Variable[i]
      updated_type <- as.character(attr$Type[i])
      
      if (is.na(updated_type)) {
        attr$Range_or_Levels[i] <- ""
        attr$Missing_Values[i] <- ""
        next
      }
      
        # Attempt column type conversion
        if (updated_type == "numeric") {
          tryCatch({
            df[[variable_name]] <- as.numeric(df[[variable_name]])
            if (all(is.na(df[[variable_name]]))) {
            attr$Range_or_Levels[i] <- translate_text(lang, "incompatible_type")
            } else {
            attr$Range_or_Levels[i] <- describe_column_values(df[[variable_name]], "numeric", lang)
            }
          }, warning = function(w) {
            if (all(is.na(df[[variable_name]]))) {
            attr$Range_or_Levels[i] <- translate_text(lang, "incompatible_type")
            } else {
            attr$Range_or_Levels[i] <- describe_column_values(df[[variable_name]], "numeric", lang)
            }
          }, error = function(e) {
            attr$Range_or_Levels[i] <- translate_text(lang, "incompatible_type")
          })
        } else if (updated_type == "factor") {
          tryCatch({
            df[[variable_name]] <- factor(df[[variable_name]],
                                          levels = sort(unique(df[[variable_name]])))
            attr$Range_or_Levels[i] <- describe_column_values(df[[variable_name]], "factor", lang)
          }, error = function(e) {
            attr$Range_or_Levels[i] <- translate_text(lang, "incompatible_type")
          })
        } else if (updated_type == "character") {
          tryCatch({
            df[[variable_name]] <- as.character(df[[variable_name]])
            attr$Range_or_Levels[i] <- describe_column_values(df[[variable_name]], "character", lang)
          }, error = function(e) {
            attr$Range_or_Levels[i] <- translate_text(lang, "incompatible_type")
          })
        } else if (updated_type == "date") {
          tryCatch({
            parsed <- parse_date_column(df[[variable_name]])
            if (is.null(parsed)) {
              suppressWarnings(parsed <- as.Date(df[[variable_name]]))
            }
            df[[variable_name]] <- parsed
            if (all(is.na(df[[variable_name]]))) {
              attr$Range_or_Levels[i] <- translate_text(lang, "incompatible_type")
            } else {
              attr$Range_or_Levels[i] <- describe_column_values(df[[variable_name]], "date", lang)
            }
          }, error = function(e) {
            attr$Range_or_Levels[i] <- translate_text(lang, "incompatible_type")
          })
        } else {
          attr$Range_or_Levels[i] <- ""
        }

        # Re-check missing values
        attr$Missing_Values[i] <- describe_missing_values(df[[variable_name]], lang, missing_tokens())
      }
      
      data(df)
      attributes(attr)
      
    })
    
    observeEvent(current_lang(), {
      attr <- isolate(attributes())
      df <- isolate(data())
      if (is.null(attr) || is.null(df)) {
        return()
      }
      lang <- current_lang()
      attr$Range_or_Levels <- mapply(function(var, type) {
        describe_column_values(df[[var]], type, lang)
      }, attr$Variable, as.character(attr$Type), SIMPLIFY = TRUE, USE.NAMES = FALSE)
      attr$Missing_Values <- sapply(attr$Variable, function(var) {
        describe_missing_values(df[[var]], lang, missing_tokens())
      })
      attributes(attr)
    })
    
    # Handle codebook download
    observeEvent(input$download_codebook, {
    req(attributes())
    req(input$datafile)
    lang <- current_lang()
    
    original_name <- input$datafile$name
    file_base <- tools::file_path_sans_ext(original_name)
    file_base <- gsub("[^A-Za-z0-9_]", "_", file_base)
    suffix <- translate_text(lang, "codebook_filename_suffix")
    filename <- paste0(file_base, "_", suffix, ".csv")
    
    csv_string <- paste(
      capture.output(
        write.csv(attributes(), row.names = FALSE, file = "")
      ),
      collapse = "\n"
    )
    
    session$sendCustomMessage('downloadCodebook', list(
      csv = csv_string,
      filename = filename
    ))
  })
}


# Run the Shiny app
shinyApp(ui, server)
