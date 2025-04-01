# Load required libraries
library(shiny)
library(shinyjs) 
library(rhandsontable)
library(readxl) 
library(shinythemes)
library(shinyBS)

options(shiny.maxRequestSize = 30 * 1024^2)

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
        h1("Codebook generator for data tables", style = "margin: 0; text-align: center;"),
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
               alt = "Logo of the Digital Research Alliance of Canada",
               style = "max-width: 100%; height: auto; margin-bottom: 20px;"),
      
      p("Welcome to the Codebook Generator App! This tool helps you create a codebook for your data tables with ease. Please contact us at curators .at frdr-dfdr.ca for improvements or comments.", 
        style = "font-size: 18px;"),
      
      h4("How to use the app:"),
      tags$ul(
        tags$li("Upload your CSV, TSV, or Excel data file (Up to 30 MB) using the 'Upload your data file' button below."),
        tags$li("After uploading, preview your data in the main panel and a preliminary codebook in the bottom panel."),
        tags$li(
          tagList(
            "Check our ",
            tags$a(
              href = "https://alliance-rdm-gdr.github.io/RDM_CodebookGenerator/RDM_Codebook_en.html",
              "documentation"
            ),
            " to fill in the codebook, specifically:"
          ),
          tags$ul(
            tags$li(
              tagList(
                "Click to write the '",
                tags$span(style = "color: green;", "Label"),
                "'and '",
                tags$span(style = "color: green;", "Units"),
                "' in the variable attributes table, and select the correct variable",
                tags$span(style = "color: green;", "Type"),
                
              )
            ),
            tags$li(
              tagList(
                "The '",
                tags$span(style = "color: green;", "Range_or_Levels"),
                "' and '",
                tags$span(style = "color: green;", "Missing_Values"),
                "' columns update automatically based on your selections."
              )
            )
          )
        ),
        tags$li("When you're ready, click 'Download the codebook' to save your codebook as a CSV file."),
        style = "font-size: 16px;"
      ),
      
      fileInput("datafile", "Upload your data file",
                accept = c(".csv", ".tsv", ".xlsx")),
      
      p("This app does not store data in any way.", style = "font-size: 14px; font-style: italic; color: #555;"),
      
      actionButton(
        "download_codebook",
        label = tagList(
          icon("download"),
          "Download the Codebook"
        )
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
        
        h2("Data Preview"),
        DT::dataTableOutput("data_preview"),
        
        br(),
        
        h2("Variable Attributes"),
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
  
  # Reactive value to store attributes
  attributes <- reactiveVal()
  
  # Load data when file is uploaded
  observeEvent(input$datafile, {
    req(input$datafile)
    
    file_ext <- tools::file_ext(input$datafile$name)
    
    if (file_ext %in% c("csv", "tsv")) {
      df <- read.csv(input$datafile$datapath, stringsAsFactors = FALSE)
    } else if (file_ext %in% c("xlsx")) {
      df <- read_excel(input$datafile$datapath)
      df <- as.data.frame(df)
    } else {
      showModal(modalDialog(
        title = "Unsupported File Type",
        "Please upload a CSV, TSV, or Excel (.xlsx) file.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    data(df)
    
    # Map variable types to predefined levels
    mapped_types <- sapply(df, function(x) {
      t <- class(x)[1]
      if (t %in% c("integer", "numeric", "double")) {
        "numeric"
      } else if (t %in% c("character")) {
        "character"
      } else if (t %in% c("factor")) {
        "factor"
      } else if (t %in% c("date", "POSIXct", "POSIXt")) {
        "date"
      } else {
        "character"
      }
    })
    
    # Calculate missing values
    missing_values <- sapply(df, function(x) {
      x_char <- as.character(x)
      x_trim <- trimws(x_char)
      x_lower <- tolower(x_trim)
      missing_strings <- c("na", "n/a", "")
      num_missing <- sum(is.na(x) | x_lower %in% missing_strings)
      if (num_missing > 0) {
        as.character(num_missing)
      } else {
        "No missing values"
      }
    })
    
    # Initialize attributes with default columns
    attr <- data.frame(
      Variable = colnames(df),
      Label = rep("", ncol(df)),
      Type = mapped_types,
      Range_or_Levels = sapply(df, function(x) {
        if (is.numeric(x)) {
          paste0("Range: ", min(x, na.rm = TRUE), " - ", max(x, na.rm = TRUE))
        } else if (is.factor(x) || is.character(x)) {
          paste("Levels: ", paste(sort(unique(x)), collapse = ", "))
        } else if (inherits(x, "date")) {
          paste0("Date Range: ", min(x, na.rm = TRUE), " - ", max(x, na.rm = TRUE))
        } else {
          ""
        }
      }),
      Missing_Values = missing_values,
      Units = rep("", ncol(df)),
      stringsAsFactors = FALSE
    )
    
    attr$Type <- factor(attr$Type, levels = c("numeric", "character", "factor", "date"))
    
    attributes(attr)
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
    rhandsontable(attr, rowHeaders = NULL) %>%
      hot_col("Label", type = "text", width = 200) %>%
      hot_col("Type", type = "dropdown", source = c("numeric", "character", "factor", "date")) %>%
      hot_col("Range_or_Levels", readOnly = TRUE, renderer = "
        function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          td.style.maxWidth = '150px';
          td.style.whiteSpace = 'nowrap';
          td.style.overflow = 'hidden';
          td.style.textOverflow = 'ellipsis';
          td.title = value;
        }
      ") %>%
      hot_col("Missing_Values", readOnly = TRUE, width = 120) %>%
      hot_col("Units", type = "text", width = 100)
  })
  
  # Update attributes after table edits
  observeEvent(input$variable_attributes_table, {
    attr <- hot_to_r(input$variable_attributes_table)
    df <- data()
    
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
        result <- tryCatch({
          df[[variable_name]] <- as.numeric(df[[variable_name]])
          if (all(is.na(df[[variable_name]]))) {
            attr$Range_or_Levels[i] <- "incompatible data type"
          } else {
            attr$Range_or_Levels[i] <- paste0("Range: ",
                                              min(df[[variable_name]], na.rm = TRUE), " - ",
                                              max(df[[variable_name]], na.rm = TRUE))
          }
        }, warning = function(w) {
          if (all(is.na(df[[variable_name]]))) {
            attr$Range_or_Levels[i] <- "incompatible data type"
          } else {
            attr$Range_or_Levels[i] <- paste0("Range: ",
                                              min(df[[variable_name]], na.rm = TRUE), " - ",
                                              max(df[[variable_name]], na.rm = TRUE))
          }
        }, error = function(e) {
          attr$Range_or_Levels[i] <- "incompatible data type"
        })
      } else if (updated_type == "factor") {
        result <- tryCatch({
          df[[variable_name]] <- factor(df[[variable_name]],
                                        levels = sort(unique(df[[variable_name]])))
          attr$Range_or_Levels[i] <- paste("Levels: ",
                                           paste(levels(df[[variable_name]]), collapse = ", "))
        }, error = function(e) {
          attr$Range_or_Levels[i] <- "incompatible data type"
        })
      } else if (updated_type == "character") {
        result <- tryCatch({
          df[[variable_name]] <- as.character(df[[variable_name]])
          attr$Range_or_Levels[i] <- paste("Values: ",
                                           paste(sort(unique(df[[variable_name]]), na.last = TRUE),
                                                 collapse = ", "))
        }, error = function(e) {
          attr$Range_or_Levels[i] <- "incompatible data type"
        })
      } else if (updated_type == "date") {
        result <- tryCatch({
          df[[variable_name]] <- as.Date(df[[variable_name]])
          attr$Range_or_Levels[i] <- paste0("Date Range: ",
                                            min(df[[variable_name]], na.rm = TRUE), " - ",
                                            max(df[[variable_name]], na.rm = TRUE))
        }, error = function(e) {
          attr$Range_or_Levels[i] <- "incompatible data type"
        })
      } else {
        attr$Range_or_Levels[i] <- ""
      }
      
      # Re-check missing values
      x <- df[[variable_name]]
      x_char <- as.character(x)
      x_trim <- trimws(x_char)
      x_lower <- tolower(x_trim)
      missing_strings <- c("na", "n/a", "")
      num_missing <- sum(is.na(x) | x_lower %in% missing_strings)
      if (num_missing > 0) {
        attr$Missing_Values[i] <- as.character(num_missing)
      } else {
        attr$Missing_Values[i] <- "No missing values"
      }
    }
    
    data(df)
    attributes(attr)
    
    # Re-render the table
    output$variable_attributes_table <- renderRHandsontable({
      rhandsontable(attr, rowHeaders = NULL) %>%
        hot_col("Label", type = "text", width = 200) %>%
        hot_col("Type", type = "dropdown", source = c("numeric", "character", "factor", "date")) %>%
        hot_col("Range_or_Levels", readOnly = TRUE, renderer = "
          function(instance, td, row, col, prop, value, cellProperties) {
            Handsontable.renderers.TextRenderer.apply(this, arguments);
            td.style.maxWidth = '120px';
            td.style.whiteSpace = 'nowrap';
            td.style.overflow = 'hidden';
            td.style.textOverflow = 'ellipsis';
            td.title = value;
          }
        ") %>%
        hot_col("Missing_Values", readOnly = TRUE, width = 120) %>%
        hot_col("Units", type = "text", width = 100)
    })
  })
  
  # Handle codebook download
  observeEvent(input$download_codebook, {
    req(attributes())
    req(input$datafile)
    
    original_name <- input$datafile$name
    file_base <- tools::file_path_sans_ext(original_name)
    file_base <- gsub("[^A-Za-z0-9_]", "_", file_base)
    filename <- paste0(file_base, "_codebook.csv")
    
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
