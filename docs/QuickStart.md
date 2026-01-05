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

# Application de génération de dictionnaires de données : guide de démarrage rapide

Bienvenue dans l’application de génération de dictionnaires de données ! Ce guide explique comment utiliser l’interface bilingue (français/anglais), que ce soit via le site hébergé, RStudio ou Docker.

## Utiliser l'application en ligne

Utilisez la version bilingue en ligne :

https://alliance-rdm-gdr.github.io/RDM_Codebook_App/

Aucune installation n'est requise.

## Exécuter l’application localement (RStudio)

### 1. Cloner le dépôt

```bash
git clone https://github.com/Alliance-RDM-GDR/RDM_Codebook_App
cd RDM_Codebook_App
```

Ou télécharger l’archive ZIP et la décompresser.

### 2. Installer les dépendances

Ouvrez R ou RStudio dans le dossier du projet, puis exécutez les commandes nécessaires. 

```r
install.packages(c(
  "shiny", "shinyjs", "shinythemes", "shinyBS",
  "rhandsontable", "readxl", "DT"
))
```

Tous les textes bilingues se trouvent dans la liste de traductions du fichier app.R; aucune bibliothèque d’internationalisation supplémentaire n’est requise.

### 3. Lancer l’application

```r
shiny::runApp()
```

## Exécuter avec Docker (facultatif)

### 1. Construire l’image

```bash
docker build -t codebook-app .
```

### 2. Lancer le conteneur

```bash
docker run -p 3838:3838 codebook-app
```

Ensuite ouvrez `http://localhost:3838` dans votre navigateur.

## Fichiers requis

- `app.R` - Application principale et dictionnaire de traduction.
- `www/` - Logos et autres fichiers statiques.
- `requirements.txt` - Dépendances pour les versions Shinylive.
- `Dockerfile` - Configuration du conteneur.

## Besoin d’aide ?

Pour toute question, suggestion ou proposition d’amélioration, veuillez écrire à : `curators@frdr-dfdr.ca`.
