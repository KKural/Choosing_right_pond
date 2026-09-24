# Reproducibility package for the AJCJ manuscript

## Choosing the right pond: Spatial clustering of street graffiti by visual complexity

This project contains the data, two R scripts, saved permutation results, generated outputs and appendices.

## Package structure

### 1. Root files

- `Choosing_the_right_pond.Rproj`
- `README.txt`

### 2. Data folder

Path: `Data`

The Data folder contains only these three folders:

- **`Data/Graffiti`**  
  Contains one analysis file: `df_graffiti_items_anonymized.csv`. This file includes the finalized yes/no B.1 overpainting variable used for Appendix 11.

- **`Data/Source_files`**  
  Contains four source groups:

  - **`Data/Source_files/Appendices`**  
    Active files: `observation_form.docx` and `observer_training_manual.pdf`. The PDF copy of the observation form and page-image folders are retained as supporting source material.

  - **`Data/Source_files/Figures`**  
    Active files: `graffiti_type_examples.png` and `relative_visibility.png`. These are copied into `Output/figures` as Figures 1 and 2. Original category photographs are retained in `Data/Source_files/Figures/source_photos`.

  - **`Data/Source_files/Manuscript`**  
    Active files: `references.bib`, `apa_5th_edition.csl`, and `reference.docx`. These are used directly when the Rmd is rendered.

  - **`Data/Source_files/Archive`**  
    Historical files retained for provenance. The active analysis does not read from this folder.

- **`Data/Shapefiles`**  
  The `WBN3` and `BRUGGEN 2` shapefiles and their required sidecar files.

**Keep all shapefile components together.** A shapefile cannot be reconstructed from the `.shp` file alone.

### 3. Scripts folder

Path: `Scripts`

- **`analysis.R`**  
  The complete reproducibility script. It reads the data, performs the analysis, reuses or creates permutation results, saves the RDS objects, writes CSV and Excel results, creates all four manuscript figures, creates the 12 appendices, creates the combined appendix Word file, and renders the manuscript when the local Rmd is present.

- **`functions.R`**  
  Functions used by `analysis.R`. Do not run this file separately.

There are no separate build, reporting, figure, or appendix scripts. Those steps are incorporated into `analysis.R` and `functions.R`.

### 4. Output folder

Path: `Output`

- **`Output/cache`**  
  Saved permutation results, the analysis-results RDS, the segment-by-type matrix, and `manuscript_results.rds`. The manuscript reads numerical values directly from `Output/cache/manuscript_results.rds`.

- **`Output/figures`**  
  The four manuscript figures and the Figure 3 count file.

- **`Output/results`**  
  CSV and Excel result files created from the analysis objects.

- **`Output/tables`**  
  The generated Table 1 Word file.

The Output folder does not contain internal verification files or a second reporting RDS outside the cache.

### 5. Appendix folder

Path: `Appendix`

The folder contains the 12 individual appendices and:

- `AJCJ_combined_appendices.docx`

## How to reproduce the analysis

1. Open `Choosing_the_right_pond.Rproj` in RStudio.
2. Keep the project folder as the working directory.
3. Open `Scripts/analysis.R` and run the complete script, or run this command in the RStudio Terminal:

   ```bash
   Rscript Scripts/analysis.R
   ```

4. Wait for the script to finish. The first uncached run can take considerable time. Compatible saved permutations in `Output/cache` are reused on later runs.
5. Review the files in `Output` and `Appendix`.




## Required R packages

`here`, `dplyr`, `tidyr`, `ggplot2`, `sf`, `purrr`, `flextable`, `officer`, `janitor`, `tibble`, `vegan`, `writexl`, `ggspatial`, `patchwork`, `extrafont`, `ragg`, `rmarkdown`, `knitr`, `digest`, and their dependencies.


## Important: Keep filenames and folder structure unchanged

Do not rename, move, or delete the required files in `Data`, `Scripts`, `Output`, or `Appendix`. The scripts use project-relative paths and will fail if required names or locations are changed.
