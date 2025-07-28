# Codebook Generator App: Quick Start Tutorial

Welcome to the Codebook Generator App! This guide will help you get started quickly — whether you're a researcher, research data management practitioner, or developer.

## Run the App Online

Use the live version of the app:

https://alliance-rdm-gdr.github.io/RDM_Codebook_App/

No installation required.

## Run Locally (RStudio)

### 1. Clone the Repository

If you have Git:

    git clone https://github.com/Alliance-RDM-GDR/RDM_Codebook_App
    cd RDM_Codebook_App

Or download ZIP and unzip it.

### 2. Install Dependencies

Open RStudio in the project folder and run:

    install.packages(c(
      "shiny", "shinyjs", "shinythemes", "rhandsontable",
      "readxl", "DT", "shinyBS", "shiny.i18n"
    ))

### 3. Run the App

    shiny::runApp()

## Run with Docker (Optional)

### 1. Build the Image

    docker build -t codebook-app .

### 2. Run the Container

    docker run -p 3838:3838 codebook-app

Then open `http://localhost:3838` in your browser.

## Required Files

- `app.R` – Main Shiny app
- `www/` – Includes your logo and assets
- `requirements.txt` – For Shinylive deployment
- `Dockerfile` – For containerized execution

## Contact & Support

For feedback or feature suggestions, contact:  
**curators@frdr-dfdr.ca**
