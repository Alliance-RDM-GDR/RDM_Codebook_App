# Codebook Generator App

**Welcome to the Codebook Generator App!**  
This tool helps researchers and research data management practitioners to quickly generate codebooks for data tables (CSV, TSV, or XLSX). Built using R and Shiny, it works entirely in your browser thanks to [Shinylive](https://github.com/rstudio/shinylive), requiring no server backend or data upload.

---

## 🌐 Live App

Access the live app here:  
➡️ **[Codebook Generator (Shinylive)](https://rdm-codebook-generator.vercel.app/)**

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
- **No data storage**: All processing happens locally in your browser

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
install.packages(c("shiny", "rhandsontable", "DT", "readxl", "shinythemes", "shinyBS", "shiny.i18n"))
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
- `CITATION.cff`: citation metadata
- `Dockerfile`: container deployment

---

## 🧑‍💻 Contributing

We welcome contributions! See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on submitting pull requests, translation files, and feature improvements.

---

## 🔏 Privacy

All data stays in your browser. No information is uploaded or tracked.

---

## ♿ Accessibility

We aim to follow WCAG best practices. Color contrast, keyboard navigation, and alt text have been tested. Feedback is welcome to further improve accessibility.

---

## 📄 Citation

If you use this app in your research or teaching, please cite it using the [CITATION.cff](CITATION.cff) file in the repository.

---

## 📚 Documentation

See the full user guide here:  
🔗 [https://alliance-rdm-gdr.github.io/RDM_CodebookGenerator/RDM_Codebook_en.html](https://alliance-rdm-gdr.github.io/RDM_CodebookGenerator/RDM_Codebook_en.html)

