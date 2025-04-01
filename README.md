# Codebook Generator App

**Welcome to the Codebook Generator App!** This Shiny-based tool lets you quickly generate and edit a codebook for your data files (CSV, TSV, or XLSX) directly in your browser. No server is required, and no data is stored externally. The app guides you to label your variables, specify types (numeric, factor, date, etc.), note missing values, and more. Finally, you can download the codebook as a CSV file.

---

## Key Features

- **Runs Client-Side**: Uses [Shinylive](https://github.com/rstudio/shinylive) to compile R into WebAssembly, so everything happens in your browser.
- **Customizable Columns**: Edit **Label**, **Type**, **Units** (measurement units), while columns like **Range_or_Levels** and **Missing_Values** update automatically.
- **Simple & Interactive**: Upload your file, tweak the codebook table, then download the final CSV.

---

## Live Demo

Check out the **live version** of this app here:  
**[Codebook Generator App](https://alliance-rdm-gdr.github.io/RDM_Codebook_App/)**

1. **Upload** a CSV/TSV/XLSX file (up to 30 MB).
2. **Edit** columns in the codebook table (Label, Type, Units).
3. **Range_or_Levels** and **Missing_Values** automatically adjust.
4. **Download** a final CSV containing your codebook.

---

## Example Codebook

Below is a brief sample of how a codebook might look once exported:


| **Variable**  | **Label**            | **Type**  | **Range_or_Levels** | **Missing values** | **Units** |
|---------------|----------------------|---------- |---------------------|--------------------|----------|
| Stage         | Experimental stage   | Factor    | 1, 2, 3, 4          | NA                 | NA       |
| Intervention  | Intervention Group   | Factor    | G1, G2, G3          | NA                 | NA       |
| Age           | Participant age      | Numeric   | 18 - 26             | 1                  | Years    |
| Sex           | Biological sex       | Factor    | Men, Women          | NA                 | NA       |
| Score         | Cognitive score      | Numeric   | 1 - 20              | NA                 | AU       |


In this snippet:

- **Stage** is a factor with four levels (1, 2, 3, 4).  
- **Age** has one missing value and is measured in **Years**.  
- **Score** is numeric from 1–20, measured in **arbitrary units** (AU).

---

## How It Works

1. **Upload**: Select “Upload your data file” and choose a CSV, TSV, or XLSX.  
2. **Preview**: The app reads your data, identifies columns, and populates an editable codebook table.  
3. **Edit**: In the table, adjust **Label**, **Type**, **Units**, or other columns.  
4. **Auto-Update**: The app recalculates things like **Range_or_Levels** (min–max or factor levels) and **Missing_Values** (count of NA, n/a, etc.).  
5. **Download**: Click “Download the Codebook” to save the result as a `.csv`.

---

## Installation & Local Use (Optional)

If you prefer to run the app locally in R (not purely client-side):

1. **Clone** or **download** this repository.  
2. **Install** packages:  

```r
install.packages(c("shiny", "rhandsontable", "DT", "readxl", "shinythemes", "shinyBS"))
```

3. Run the app:

```r
library(shiny)
shiny::runApp("path/to/app.R")
Open the URL (usually http://127.0.0.1:xxxx) in your browser.
```

---

## Contributing

Open an issue for any bugs or feature suggestions.

---

## Accessibility

We strive to meet basic accessibility guidelines (color contrast, alt text, screen-reader headings). Input on further enhancements is greatly appreciated.

---

## Privacy

Since everything happens in your browser, your data is never stored or transmitted to a server.

