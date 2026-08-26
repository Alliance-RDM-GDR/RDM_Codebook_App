# Codebook Generator App

The Codebook Generator App helps researchers and research data management (RDM) teams document tabular datasets (CSV, TSV, XLSX). Built with R and Shiny, it now includes a language selector that instantly switches every UI element between English and French. The app runs either fully client-side via Shinylive or on a standard Shiny server.

## Live app

Launch the bilingual Shinylive build: https://alliance-rdm-gdr.github.io/RDM_Codebook_App/

## Features

- Upload CSV, TSV, or XLSX files up to 30 MB and preview them immediately.
- Auto-detect variable types, min/max ranges, factor levels, and missing values. Common missing-value markers (`NA`, `N/A`, `na`, `n/a`, case-insensitive) are normalized before type detection, so a numeric column that uses them for missing cells is still recognized as numeric.
- Auto-detect date columns written as `YYYY-MM-DD`, `YYYY/MM/DD`, `DD/MM/YYYY`, `MM/DD/YYYY`, or with `-` separators, without needing a numeric-only pattern that could be confused with an ID.
- Add your own missing-value markers (e.g. `-99, 999, unknown`) in the sidebar — comma-separated, case-insensitive, and applied to both text and numeric columns — so type detection and missing-value counts reflect your dataset's own conventions, not just `NA`/`N/A`.
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
- `index.html`, `edit/`, `shinylive/`, `shinylive-sw.js`, `app.json` - The deployed Shinylive site served by GitHub Pages from the repository root. See "Rebuilding and deploying the live app" below before editing `app.json` directly.

## Rebuilding and deploying the live app

The live app at https://alliance-rdm-gdr.github.io/RDM_Codebook_App/ is a static Shinylive export. `index.html` and the `shinylive/` runtime rarely change, but **`app.json` embeds the current `app.R` source and must be regenerated any time `app.R` changes**, or the live site will keep running the old code.

1. Render the export script (requires R, Quarto, and the `shinylive` package):
   ```r
   quarto::quarto_render("CodebookGenerator_Deploy.qmd")
   ```
   This runs `shinylive::export()` into a local `WebApp/` folder. That folder is a full Shinylive bundle (~70-100+ MB with the webR runtime and packages) and is git-ignored — it is too large to commit to GitHub.
2. Copy only the small manifest out of it to the project root:
   ```bash
   cp WebApp/app.json app.json
   ```
3. Commit the updated `app.json` (and `CodebookGenerator_Deploy.html`, which Quarto also regenerates). Do not commit the `WebApp/` folder itself.

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
-	Détecter automatiquement les types de variables, les plages min/max, les niveaux de facteurs et les valeurs manquantes. Les marqueurs courants de valeurs manquantes (`NA`, `N/A`, `na`, `n/a`, sans distinction de casse) sont normalisés avant la détection du type, afin qu'une colonne numérique les utilisant pour ses cellules manquantes soit tout de même reconnue comme numérique.
-	Détecter automatiquement les colonnes de dates écrites au format `AAAA-MM-JJ`, `AAAA/MM/JJ`, `JJ/MM/AAAA`, `MM/JJ/AAAA`, ou avec des séparateurs `-`, sans se fier à un format uniquement numérique qui pourrait être confondu avec un identifiant.
-	Ajouter vos propres marqueurs de valeurs manquantes (ex. : `-99, 999, inconnu`) dans le panneau latéral — séparés par des virgules, sans distinction de casse, et appliqués aux colonnes texte comme numériques — afin que la détection du type et le compte des valeurs manquantes reflètent les conventions propres à votre jeu de données, pas seulement `NA`/`N/A`.
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
- `index.html`, `edit/`, `shinylive/`, `shinylive-sw.js`, `app.json` - Le site Shinylive déployé, servi par GitHub Pages depuis la racine du dépôt. Voir « Reconstruire et déployer l'application en ligne » ci-dessous avant de modifier `app.json` directement.

## Reconstruire et déployer l'application en ligne

L'application en ligne à https://alliance-rdm-gdr.github.io/RDM_Codebook_App/ est un export Shinylive statique. `index.html` et l'environnement `shinylive/` changent rarement, mais **`app.json` intègre le code source actuel de `app.R` et doit être régénéré à chaque modification de `app.R`**, sinon le site en ligne continuera d'exécuter l'ancien code.

1. Exécuter le script d'export (nécessite R, Quarto et le paquet `shinylive`) :
   ```r
   quarto::quarto_render("CodebookGenerator_Deploy.qmd")
   ```
   Cela lance `shinylive::export()` dans un dossier local `WebApp/`. Ce dossier est un paquet Shinylive complet (environ 70 à 100+ Mo avec l'environnement webR et les paquets) et est exclu de Git — il est trop volumineux pour être ajouté à GitHub.
2. Copier uniquement le petit fichier manifeste vers la racine du projet :
   ```bash
   cp WebApp/app.json app.json
   ```
3. Valider (« commit ») le fichier `app.json` mis à jour (ainsi que `CodebookGenerator_Deploy.html`, également régénéré par Quarto). Ne pas valider le dossier `WebApp/` lui-même.

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
