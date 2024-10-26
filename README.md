# Transferarbeit_2

[GitHub Repository](https://github.com/ClaudioLutz/Transferarbeit_2.git)

This project analyzes data from the Swiss Commercial Register (SHAB). The analysis steps are organized in `Shab-Analyse.Rmd`, an R Markdown file, which processes and visualizes the registration and deletion trends in SHAB data over time.

## Getting Started

### Cloning the Repository
To clone the repository locally, run the following command in your terminal:

```bash
git clone https://github.com/ClaudioLutz/Transferarbeit_2.git
```

### Open the R Markdown File
Open the `Shab-Analyse.Rmd` file in R or RStudio.

## Running the Analysis

Follow these steps to set up and run the analysis:

### Step 1: Install Required Libraries
In `Shab-Analyse.Rmd`, locate the code chunk named `installiere_libraries`. To ensure all required libraries are installed:

1. Change the option `eval=FALSE` to `eval=TRUE` in this chunk.
2. Run this chunk to install any missing libraries.

### Step 2: Run Code Chunks
To execute the analysis:

1. Run all code chunks in sequence.
2. If the chunk `"lade_daten_falls_nötig"` fails, re-run it. This chunk handles data scraping and will continue from where it last left off. 

   - **Tip**: To verify the chunk is running, check the `shab_data` folder. The number of files should steadily increase as the data is processed.
   - **Note**: This data download, processing, and saving to an RDS file may take one to two hours, depending on the amount of data and network speed.

### Step 3: Render Output
Once all chunks have successfully executed, knit the document to HTML by clicking **Knit** in RStudio, or run the following in the R console:

```r
rmarkdown::render("Shab-Analyse.Rmd", output_format = "html_document")
```

The output will include a table of contents (Inhaltsverzeichnis) and visualizations of SHAB registration trends.

### Additional Notes
- **Troubleshooting**: If errors occur during data loading, ensure all dependencies are correctly installed, and re-run any interrupted chunks.
- **Customization**: Adjust the `toc_depth` setting in the YAML header to control how many heading levels are shown in the Inhaltsverzeichnis.

## License
This project is licensed under the MIT License.
