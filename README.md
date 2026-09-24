# Reproducibility package for the AJCJ manuscript

## Choosing the right pond: Spatial clustering of street graffiti by visual complexity

This package contains the data, two R scripts, saved permutation results, generated outputs and appendices needed to reproduce the analysis.

## Important: Keep filenames and folder structure unchanged

Do not rename or move the required files in `Data`, `Scripts`, or `Output/cache`. The scripts use project-relative paths. Files in `Output/figures`, `Output/results`, `Output/tables`, and `Appendix` are generated outputs and can be recreated by running the complete script.


## Package structure

### 1. Root files

- `Choosing_the_right_pond.Rproj`
- `README.md`

### 2. Data folder

Path: `Data`

The Data folder contains only these three folders:

- **`Data/Graffiti`**  
  Contains one analysis file: `df_graffiti_items_anonymized.csv`. This file includes the finalized yes/no B.1 overpainting variable used for Appendix 11.

- **`Data/Source_files`**  
  This is a flat folder with no subfolders. It contains five source files:

  - `Appendix_1.docx`
  - `Appendix_1.pdf`
  - `Appendix_2.pdf`
  - `graffiti_type_examples.png`
  - `relative_visibility.png`

  The script uses `Appendix_1.pdf` and `Appendix_2.pdf` to create Appendices 1 and 2 and to insert them into the combined appendix Word file. `Appendix_1.docx` is retained as the editable source of the observation form. The two PNG files are copied into `Output/figures` as Figures 1 and 2. Temporary page images for the combined appendix are created from the two PDFs during the run and are not stored as package inputs.

- **`Data/Shapefiles`**  
  Contains the `WBN3` and `BRUGGEN 2` shapefiles and their required sidecar files.

**Keep all shapefile components together.** A shapefile cannot be reconstructed from the `.shp` file alone.

### 3. Scripts folder

Path: `Scripts`

- **`analysis.R`**  
  The complete reproducibility script. It reads the data, performs the analysis, reuses or creates permutation results, saves the RDS objects, writes CSV and Excel results, creates all four manuscript figures, creates the 12 appendices, and creates the combined appendix Word file.

- **`functions.R`**  
  Functions used by `analysis.R`. Do not run this file separately.

There are no separate build, reporting, figure, or appendix scripts. Those steps are incorporated into `analysis.R` and `functions.R`.

## Required R packages

`here`, `dplyr`, `tidyr`, `ggplot2`, `sf`, `purrr`, `flextable`, `officer`, `janitor`, `tibble`, `vegan`, `writexl`, `ggspatial`, `patchwork`, `extrafont`, `ragg`, `pdftools`, and their dependencies.

### 4. Output folder

Path: `Output`

- **`Output/cache`**  
  Contains the saved permutation results and analysis RDS objects. The cached permutation files are included to avoid the substantial delay caused by rerunning every permutation and sensitivity analysis.

  To perform a completely fresh reproduction, delete all `.rds` files in `Output/cache` and then run the complete script without an option:

  ```bash
  Rscript Scripts/analysis.R
  ```

  The script will recreate the deleted RDS files and reproduce the results. This uncached run will take considerably longer than a run that reuses the supplied cache.

- **`Output/figures`**  
  Contains the four manuscript figures and the Figure 3 count file.

- **`Output/results`**  
  Contains the CSV and Excel result files created from the analysis objects.

- **`Output/tables`**  
  Contains the generated Table 1 Word file.

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

4. Wait for the script to finish. Compatible saved permutations in `Output/cache` are reused. A completely uncached run can take considerable time.

5. Review the files in `Output` and `Appendix`.


