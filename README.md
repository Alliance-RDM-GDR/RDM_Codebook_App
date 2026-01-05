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

L’application Codebook Generator aide les équipes de recherche et de gestion des données à documenter leurs jeux de données tabulaires (CSV, TSV, XLSX). Développée avec R et Shiny, l’application intègre désormais un sélecteur de langue permettant de basculer instantanément l’ensemble des éléments de l’interface entre l’anglais et le français. Elle peut fonctionner entièrement côté client grâce à Shinylive ou être déployée sur un serveur Shiny standard.

## Application en ligne

Version Shinylive bilingue : https://alliance-rdm-gdr.github.io/RDM_Codebook_App/

## Fonctionnalités

-	Téléverser r des fichiers CSV, TSV ou XLSX (jusqu’à 30 Mo) et les prévisualiser immédiatement.
-	Détecter automatiquement les types de variables, les plages min/max, les niveaux de facteurs et les valeurs manquantes.
-	Modifier les libellés, les types et les unités.
-	Télécharger le dictionnaire de données au format CSV et l’enregistrer localement.
-	Utiliser le bouton de bascule de la barre latérale pour passer de l’anglais au français sans recharger la session.
-	Aucune persistance des données — les fichiers demeurent uniquement en mémoire volatile.


## Démarrage rapide 

1. 1.	Cloner le dépôt  
   `git clone https://github.com/Alliance-RDM-GDR/RDM_Codebook_App`
2. 2.	Installer les dépendances R  
   ```r
   install.packages(c(
     "shiny","shinyjs","shinythemes","shinyBS",
     "rhandsontable","readxl","DT"
   ))
   ```
3. Lancer l’application  
   `shiny::runApp()`

Voir `docs/QuickStart.md` pour les instructions Docker et plus de détails.

## Structure des répertoires : 

- `app.R` - Application Shiny principale avec dictionnaire de traduction.
- `www/` - Logos et ressources statiques.
- `docs/` - Architecture, guide de démarrage bilingue et autres références.
- `CodebookGenerator_Deploy.qmd` - Script pour générer le paquet Shinylive.
- `requirements.txt` / `environment.yml` - Spécifications d’environnement reproductibles.
- `Dockerfile` - Construction du conteneur.

## Mise à jour des traductions

Chacune des chaînes de texte se trouve dans la liste des traductions du fichier app. R. Chaque clé comporte une version anglaise et une version française. Lors de l’ajout de nouveaux éléments d’interface, veillez à mettre à jour les deux versions, puis à vérifier les deux langues dans l’application en cours d’exécution.

## Contribution

•	Les contributions sont les bienvenues. Veuillez consulter le fichier docs/CONTRIBUTING.md pour prendre connaissance des normes de codage et des lignes directrices en matière de traduction.

## Confidentialité et accessibilité

- Les données ne quittent jamais votre navigateur. Aucune information n’est enregistrée, stockée ou transmise.
- L’interface respecte les bonnes pratiques en matière d’accessibilité (contraste des couleurs, navigation au clavier, textes alternatifs). Merci de signaler toute lacune afin que nous puissions améliorer l’expérience.

## Documentation et soutien

- Fiche anglaise : https://alliance-rdm-gdr.github.io/CUR_Res_OnePagers/RDM_Codebook_en.html  
- Fiche francaise : https://alliance-rdm-gdr.github.io/CUR_Res_OnePagers/RDM_Codebook_fr.html  
- •	Commentaires ou mises à jour de traduction : `curators@frdr-dfdr.ca`
