# Codebook Generator App

The Codebook Generator App helps researchers and research data management (RDM) teams document tabular datasets (CSV, TSV, XLSX). Built with R and Shiny, it now includes a language selector that instantly switches every UI element between English and French. The app runs either fully client-side via Shinylive or on a standard Shiny server.

## Live app

Launch the bilingual Shinylive build: https://alliance-rdm-gdr.github.io/RDM_Codebook_App/

## Features

- Upload CSV, TSV, or XLSX files up to 30 MB and preview them immediately.
- Auto-detect variable types, min/max ranges, factor levels, and missing values.
- Edit labels, types, and units in an interactive `rhandsontable`.
- Download the finished codebook as CSV with localized filenames.
- Sidebar toggle switches between English and French without reloading the session.
- No data persistence: uploads stay in volatile browser memory only.

## Quick start

1. Clone the repository  
   `git clone https://github.com/Alliance-RDM-GDR/RDM_Codebook_App`
2. Install R dependencies  
   ```r
   install.packages(c(
     "shiny","shinyjs","shinythemes","shinyBS",
     "rhandsontable","readxl","DT"
   ))
   ```
3. Run the app  
   `shiny::runApp()`

See `docs/QuickStart.md` for Docker instructions and additional details.

## Folder structure

- `app.R` - Main Shiny app plus translation dictionary.
- `www/` - Logos and static assets.
- `docs/` - Architecture overview, bilingual quick start, and other references.
- `CodebookGenerator_Deploy.qmd` - Script for creating the Shinylive bundle.
- `requirements.txt` / `environment.yml` - Reproducible environment specs.
- `Dockerfile` - Container build file.

## Updating translations

All strings live in the `translations` list near the top of `app.R`. Each key has an English and French entry. Update both entries when adding new UI copy, then verify both languages in the running app.

## Contributing

Issues and pull requests are welcome. Please see `docs/CONTRIBUTING.md` for coding standards and translation tips.

## Privacy and accessibility

- Data never leaves your browser. Nothing is logged, stored, or transmitted.
- The UI follows accessibility best practices (color contrast, keyboard navigation, alt text). Report gaps so we can improve.

## Documentation and support

- English one-pager: https://alliance-rdm-gdr.github.io/CUR_Res_OnePagers/RDM_Codebook_en.html  
- French one-pager: https://alliance-rdm-gdr.github.io/CUR_Res_OnePagers/RDM_Codebook_fr.html  
- Feedback or translation updates: `curators@frdr-dfdr.ca`

---

# Application Codebook Generator

L'application Codebook Generator aide les equipes de recherche et de gestion des donnees a documenter leurs tableaux (CSV, TSV, XLSX). Construite avec R et Shiny, elle inclut maintenant un selecteur de langue qui bascule instantanement toute l'interface entre l'anglais et le francais. Vous pouvez l'executer en mode client via Shinylive ou sur un serveur Shiny classique.

## Application en ligne

Version Shinylive bilingue : https://alliance-rdm-gdr.github.io/RDM_Codebook_App/

## Fonctionnalites

- Televerser des fichiers CSV, TSV ou XLSX (jusqu'a 30 Mo) et les previsualiser immediatement.
- Detecter automatiquement les types de variables, les plages min/max, les niveaux de facteurs et les valeurs manquantes.
- Modifier les libelles, types et unites dans un `rhandsontable` interactif.
- Telecharger le dictionnaire final au format CSV avec un nom localise.
- Utiliser le selecteur de langue dans la barre laterale sans recharger la session.
- Aucune persistance des donnees : tout reste en memoire volatile du navigateur.

## Demarrage rapide

1. Cloner le depot  
   `git clone https://github.com/Alliance-RDM-GDR/RDM_Codebook_App`
2. Installer les dependances R  
   ```r
   install.packages(c(
     "shiny","shinyjs","shinythemes","shinyBS",
     "rhandsontable","readxl","DT"
   ))
   ```
3. Lancer l'application  
   `shiny::runApp()`

Consultez `docs/QuickStart.md` pour les instructions Docker et les details supplementaires.

## Structure des dossiers

- `app.R` - Application Shiny principale avec dictionnaire de traduction.
- `www/` - Logos et ressources statiques.
- `docs/` - Architecture, guide de demarrage bilingue et autres references.
- `CodebookGenerator_Deploy.qmd` - Script pour generer le paquet Shinylive.
- `requirements.txt` / `environment.yml` - Specifications d'environnement reproductibles.
- `Dockerfile` - Construction du conteneur.

## Mise a jour des traductions

Toutes les chaines se trouvent dans la liste `translations` au debut de `app.R`. Chaque cle doit etre definie en anglais et en francais. Mettez a jour les deux versions lorsque vous ajoutez un nouveau texte, puis testez les deux langues dans l'application.

## Contribution

Les contributions sont les bienvenues. Consultez `docs/CONTRIBUTING.md` pour connaitre les normes de code et les conseils relatifs a la traduction.

## Confidentialite et accessibilite

- Les donnees ne quittent jamais votre navigateur. Rien n'est journalise ni stocke.
- L'interface suit les bonnes pratiques d'accessibilite (contraste, navigation clavier, texte alternatif). Signalez-nous toute lacune pour que nous puissions l'ameliorer.

## Documentation et soutien

- Fiche anglaise : https://alliance-rdm-gdr.github.io/CUR_Res_OnePagers/RDM_Codebook_en.html  
- Fiche francaise : https://alliance-rdm-gdr.github.io/CUR_Res_OnePagers/RDM_Codebook_fr.html  
- Commentaires ou mises a jour de traduction : `curators@frdr-dfdr.ca`
