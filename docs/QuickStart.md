# Codebook Generator App: Quick Start Tutorial

Welcome to the Codebook Generator App! This guide shows how to run the bilingual (English/French) interface whether you use the hosted site, RStudio, or Docker.

## Run the App Online

Use the live bilingual version:

https://alliance-rdm-gdr.github.io/RDM_Codebook_App/

No installation is required.

## Run Locally (RStudio)

### 1. Clone the repository

```bash
git clone https://github.com/Alliance-RDM-GDR/RDM_Codebook_App
cd RDM_Codebook_App
```

Or download the ZIP archive and unzip it.

### 2. Install dependencies

Open R or RStudio inside the project folder and run:

```r
install.packages(c(
  "shiny", "shinyjs", "shinythemes", "shinyBS",
  "rhandsontable", "readxl", "DT"
))
```

All bilingual text lives in the `translations` list inside `app.R`, so no extra i18n libraries are needed.

### 3. Run the app

```r
shiny::runApp()
```

## Run with Docker (optional)

### 1. Build the image

```bash
docker build -t codebook-app .
```

### 2. Run the container

```bash
docker run -p 3838:3838 codebook-app
```

Then open `http://localhost:3838` in your browser.

## Required files

- `app.R` - Main Shiny app and translation dictionary.
- `www/` - Logos and other static assets.
- `requirements.txt` - Packages for Shinylive builds.
- `Dockerfile` - Container recipe.

## Contact and support

For feedback, feature ideas, or translation updates, email `curators@frdr-dfdr.ca`.

---

# Application Codebook Generator : Guide de demarrage rapide

Bienvenue dans l'application Codebook Generator! Ce guide explique comment executer l'interface bilingue (anglais/francais) en ligne, dans RStudio ou avec Docker.

## Utiliser l'application en ligne

Version bilingue hebergee :

https://alliance-rdm-gdr.github.io/RDM_Codebook_App/

Aucune installation n'est requise.

## Utiliser RStudio en local

### 1. Cloner le depot

```bash
git clone https://github.com/Alliance-RDM-GDR/RDM_Codebook_App
cd RDM_Codebook_App
```

Vous pouvez aussi telecharger l'archive ZIP et la decompresser.

### 2. Installer les dependances

Dans R ou RStudio, lancez :

```r
install.packages(c(
  "shiny", "shinyjs", "shinythemes", "shinyBS",
  "rhandsontable", "readxl", "DT"
))
```

Toute la traduction se trouve dans la liste `translations` de `app.R`, il n'est donc pas necessaire d'ajouter une bibliotheque i18n.

### 3. Demarrer l'application

```r
shiny::runApp()
```

## Utiliser Docker (optionnel)

### 1. Construire l'image

```bash
docker build -t codebook-app .
```

### 2. Demarrer le conteneur

```bash
docker run -p 3838:3838 codebook-app
```

Ensuite ouvrez `http://localhost:3838` dans votre navigateur.

## Fichiers requis

- `app.R` - Application principale et dictionnaire de traduction.
- `www/` - Logos et autres fichiers statiques.
- `requirements.txt` - Liste pour la construction Shinylive.
- `Dockerfile` - Recette du conteneur.

## Contact et soutien

Pour vos commentaires, nouvelles fonctions ou ajouts de traduction : `curators@frdr-dfdr.ca`.
