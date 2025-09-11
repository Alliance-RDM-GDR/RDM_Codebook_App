# Codebook Generator App

**Welcome to the Codebook Generator App!**  

This tool helps researchers and research data management practitioners to quickly generate codebooks for data tables (CSV, TSV, or XLSX). Built using R and Shiny, it works entirely in your browser thanks to [Shinylive](https://github.com/rstudio/shinylive), requiring no server backend or data upload.

---

## 🌐 Live App

Access the live app here:  
➡️ **[Codebook Generator (Shinylive)](https://alliance-rdm-gdr.github.io/RDM_Codebook_App/)**

---

## ✨ Features

- **Client-side execution** using Shinylive (WebAssembly-based)
- **Upload** CSV, TSV, or XLSX files up to 30 MB
- **Automatic parsing** of variable types and missing values
- **Editable** fields for:
  - Variable **Label**
  - Variable **Type** (`numeric`, `character`, `factor`, `date`)
  - **Units**
- **Dynamic generation** of:
  - **Range_or_Levels**: min/max for numerics or list of factor levels
  - **Missing_Values**: counts `NA`, `na`, `n/a`, or blanks
- **Accessible download** of the final codebook as a `.csv`
- **No data storage**: all processing happens locally in your browser

---

## 📦 Example Codebook Output

| **Variable**  | **Label**            | **Type**  | **Range_or_Levels** | **Missing_Values**  | **Units** |
|---------------|----------------------|---------- |---------------------|---------------------|-----------|
| Stage         | Experimental stage   | Factor    | 1, 2, 3, 4          | NA                  | NA        |
| Intervention  | Intervention Group   | Factor    | G1, G2, G3          | NA                  | NA        |
| Age           | Participant age      | Numeric   | 18 - 26             | 1                   | Years     |
| Sex           | Biological sex       | Factor    | Men, Women          | NA                  | NA        |
| Score         | Cognitive score      | Numeric   | 1 - 20              | NA                  | AU        |

---

## 🚀 Quick Start

For local development or customization:

1. Clone this repository  
2. Install dependencies

```r
install.packages(c("shiny", "rhandsontable", "DT", "readxl", "shinythemes", "shinyBS"))
```

3. Launch the app in RStudio

```r
shiny::runApp("app.R")
```

---

## 📁 Folder Structure

- `app.R`: main app file
- `www/`: contains logo and static assets
- `docs/`: project documentation
  - `QUICK_START.md`: step-by-step usage guide
  - `CONTRIBUTING.md`: contributor guide
  - `architecture.md` : description of software architecture
- `CITATION.cff`: citation metadata
- `Dockerfile`: container deployment

---

## 🧑‍💻 Contributing

We welcome contributions! See [CONTRIBUTING.md](docs/CONTRIBUTING.md) for guidelines on submitting pull requests, translation files, and feature improvements.

---

## 🔏 Privacy

All data stays in your browser. No information is uploaded or tracked.

---

## ♿ Accessibility

We aim to follow WCAG best practices. Color contrast, keyboard navigation, and alt text have been tested. Feedback is welcome to further improve accessibility.

---

## 📄 Citation

If you use this app in your research or teaching, please cite it using the [CITATION.cff](CITATION.cff) file in the repository.

Manrique-Castano, D. & FRDR curation team. Codebook Generator App. (Version 2025-09). Zenodo. https://doi.org/10.5281/zenodo.17094365

---

## 📚 Documentation

See the full user guide [here](https://alliance-rdm-gdr.github.io/CUR_Res_OnePagers/RDM_Codebook_en.html)  

---

# Générateur de dictionnaires de données

**Bienvenue dans le Générateur de dictionnaires de données !**

Cet outil aide les chercheuses, chercheurs et praticiens de la gestion des données de recherche à créer rapidement des dictionnaires de données (« codebooks ») pour des tables de données (CSV, TSV ou XLSX). Construit avec R et Shiny, il s’exécute entièrement dans votre navigateur grâce à Shinylive, sans serveur ni téléversement de données.

---

## 🌐 Application en ligne

Accédez à l’application ici :  
➡️ **[Générateur de dictionnaires de données (Shinylive)](https://alliance-rdm-gdr.github.io/RDM_Codebook_App/)**

---

## ✨ Fonctionnalités

- **Exécution côté client** avec Shinylive (basé sur WebAssembly)
- **Téléversement** de fichiers CSV, TSV ou XLSX jusqu’à 30 Mo
- **Analyse automatique** des types de variables et des valeurs manquantes
- **Champs modifiables** pour :
  - **Libellé** de la variable
  - **Type** de la variable (`numeric`, `character`, `factor`, `date`)
  - **Unités**
- **Génération dynamique** de :
  - **Range_or_Levels**: min/max pour les numériques ou liste des niveaux pour les facteurs
  - **Missing_Values**: comptage des `NA`, `na`, `n/a`, ou cases vides
- **Téléchargement** du dictionnaire final au format `.csv`
- **Aucune conservation des données :** tout le traitement se fait localement dans votre navigateur

---

## 📦 Exemple de dictionnaire de données

| **Variable**  | **Label**            | **Type**  | **Range_or_Levels** | **Missing_Values**  | **Units** |
|---------------|----------------------|---------- |---------------------|---------------------|-----------|
| Stage         | Experimental stage   | Factor    | 1, 2, 3, 4          | NA                  | NA        |
| Intervention  | Intervention Group   | Factor    | G1, G2, G3          | NA                  | NA        |
| Age           | Participant age      | Numeric   | 18 - 26             | 1                   | Years     |
| Sex           | Biological sex       | Factor    | Men, Women          | NA                  | NA        |
| Score         | Cognitive score      | Numeric   | 1 - 20              | NA                  | AU        |

---

## 🚀 Démarrage rapide

Pour le développement local ou la personnalisation :

1. Cloner ce dépôt
2. Installer les dépendances

```r
install.packages(c("shiny", "rhandsontable", "DT", "readxl", "shinythemes", "shinyBS"))
```

3. Lancer l’application dans RStudio

```r
shiny::runApp("app.R")
```

---

## 📁 Folder Structure

- `app.R`: fichier principal de l’application
- `www/`: logo et ressources statiques
- `docs/`: documentation du projet
  - `QUICK_START.md`: guide d’utilisation pas à pas
  - `CONTRIBUTING.md`: guide du contributeur
  - `architecture.md` : description de l'architecture logicielle
- `CITATION.cff`: citation metadata
- `Dockerfile`: déploiement en conteneur

---

## 🧑‍💻 Contribution

Les contributions sont les bienvenues ! Voir [CONTRIBUTING.md](docs/CONTRIBUTING.md) pour les indications concernant les propositions de modifications, les traductions et les améliorations de fonctionnalités.

---

## 🔏 Confidentialité

Toutes les données restent dans votre navigateur. Aucune information n’est téléversée ni suivie.

---

## ♿ Accessibilité

Objectif d’alignement avec les bonnes pratiques WCAG. Le contraste des couleurs, la navigation au clavier et le texte de remplacement (alt text) ont été vérifiés. Vos retours sont bienvenus pour améliorer encore l’accessibilité.

---

## 📄 Citation

Si vous utilisez cette application dans vos cours ou vos recherches, veuillez la citer à l’aide du fichier [CITATION.cff](CITATION.cff) du dépôt.

Manrique-Castano, D. & FRDR curation team. Générateur de dictionnaires de données. (Version 2025-09). Zenodo. https://doi.org/10.5281/zenodo.17094365

---

## 📚 Documentation

Consultez le guide [utilisateur complet](https://alliance-rdm-gdr.github.io/CUR_Res_OnePagers/RDM_Codebook_fr.html)  

