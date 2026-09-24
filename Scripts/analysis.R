# Reproduce the AJCJ graffiti analysis and submission outputs.
# Run from the project root: Rscript Scripts/analysis.R

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(sf)
  library(purrr)
  library(flextable)
  library(officer)
  library(janitor)
  library(tibble)
  library(vegan)
  library(writexl)
  library(ggspatial)
})

source(here::here("Scripts", "functions.R"))

# Analysis configuration -------------------------------------------------------
# Settings are defined once and reused throughout the analysis and appendices.
analysis_config <- list(
  main_permutations = 1000L,
  main_seed = 1234L,
  spatial_permutations = 1000L,
  spatial_seed = 1234L,
  spatial_default_blocks = 25L,
  spatial_block_requests = c(10L, 25L, 50L, 100L),
  no_masterpiece_permutations = 1000L,
  misclassification_reestimations = 20L,
  misclassification_permutations = 199L,
  misclassification_error_rates = c(0.05, 0.10, 0.15),
  misclassification_scenarios = c(
    "Adjacent boundary only",
    "Diffuse with cross-pole bleed"),
  misclassification_seed = 20260417L,
  alpha = 0.05,
  secondary_alpha = 0.01
)

# Record the software versions used for this run in the cached result object.
analysis_r_version <- paste0(R.version$major, ".", R.version$minor)
analysis_vegan_version <- utils::packageDescription("vegan", fields = "Version")

# Output directories -----------------------------------------------------------
appendix_dir    <- here::here("Appendix")
figures_dir     <- here::here("Output", "figures")
tables_dir      <- here::here("Output", "tables")
all_results_dir <- here::here("Output", "results")
cache_dir       <- here::here("Output", "cache")

for (d in c(appendix_dir, figures_dir, tables_dir, all_results_dir, cache_dir))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Read data --------------------------------------------------------------------
df_graffiti_data_clean <- read.csv(
  here::here("Data", "Graffiti", "df_graffiti_items_anonymized.csv")
) |>
  dplyr::select(street_segment, observer_id, graffiti_type,
                graffiti_types_grouped, overwriting) |>
  dplyr::mutate(
    street_segment = as.character(street_segment),
    graffiti_types_grouped = dplyr::recode(graffiti_types_grouped, "No graffiti" = "no graffiti")
  )

# Street-segment status --------------------------------------------------------
graffiti_status <- df_graffiti_data_clean |>
  dplyr::group_by(street_segment) |>
  dplyr::summarise(
    no_graffiti  = all(graffiti_types_grouped == "no graffiti"),
    only_others  = all(graffiti_types_grouped == "Others"),
    has_graffiti = any(graffiti_types_grouped != "no graffiti" &
                       graffiti_types_grouped != "Others"),
    .groups = "drop"
  )

sf_ghent_street_segments <- dplyr::bind_rows(
  sf::st_read(dsn = here::here("Data", "Shapefiles", "WBN3.shp"), quiet = TRUE),
  sf::st_read(dsn = here::here("Data", "Shapefiles", "BRUGGEN 2.shp"), quiet = TRUE)
) |>
  dplyr::select(-TYPE, -OPNDATUM, -VORM, -LBLVORM) |>
  dplyr::mutate(
    LBLTYPE = factor(
      dplyr::case_match(LBLTYPE,
        "kruispuntzone" ~ "intersection",
        "wegsegment"    ~ "street segment",
        "overbrugging"  ~ "bridge",
        .default = NA_character_),
      levels = c("street segment", "intersection", "bridge")),
    UIDN = as.character(UIDN)
  ) |>
  dplyr::left_join(graffiti_status, by = c("UIDN" = "street_segment")) |>
  dplyr::mutate(
    observed_status = dplyr::case_when(
      has_graffiti ~ "At least with one graffiti",
      only_others  ~ "Only Others",
      no_graffiti  ~ "Zero graffiti",
      TRUE         ~ "Not observed"),
    dplyr::across(c(no_graffiti, only_others, has_graffiti), ~ coalesce(.x, FALSE))
  ) |>
  dplyr::select(-no_graffiti, -has_graffiti, -only_others)

count_total_segments         <- nrow(sf_ghent_street_segments)
unique_observers             <- dplyr::n_distinct(df_graffiti_data_clean$observer_id)
count_observed_segments_raw  <- dplyr::n_distinct(df_graffiti_data_clean$street_segment)
count_segments_with_graffiti <- sum(sf_ghent_street_segments$observed_status == "At least with one graffiti", na.rm = TRUE)
count_only_others            <- sum(sf_ghent_street_segments$observed_status == "Only Others",    na.rm = TRUE)
count_zero_graffiti          <- sum(sf_ghent_street_segments$observed_status == "Zero graffiti",  na.rm = TRUE)
count_observed_segments      <- count_segments_with_graffiti + count_only_others + count_zero_graffiti
count_not_observed           <- sum(sf_ghent_street_segments$observed_status == "Not observed",   na.rm = TRUE)

# Appendix 1: Observation form -------------------------------------------------
file.copy(
  here::here("Data", "Source_files", "Appendix_1.pdf"),
  file.path(appendix_dir, "Appendix_01_observation_form.pdf"),
  overwrite = TRUE
)

# Appendix 2: Training material ------------------------------------------------
file.copy(
  here::here("Data", "Source_files", "Appendix_2.pdf"),
  file.path(appendix_dir, "Appendix_02_training_material.pdf"),
  overwrite = TRUE
)

# Appendix 3: Segment-type map -------------------------------------------------
app3_title <- paste0("Appendix 3: Street segment classification (N = ", count_total_segments, ")")

Appendix_3_Figure <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = sf_ghent_street_segments, ggplot2::aes(fill = LBLTYPE)) +
  ggplot2::scale_fill_manual(
    values = c("street segment" = "grey60", "intersection" = "grey40", "bridge" = "grey20"),
    labels = c("Street segment", "Intersection", "Bridge"), name = "Segment type") +
  ggplot2::labs(title = NULL) + custom_theme +
  ggplot2::theme(
    text = ggplot2::element_text(family = "Times New Roman"),
    legend.text  = ggplot2::element_text(size = 10, family = "Times New Roman"),
    legend.title = ggplot2::element_text(size = 12, family = "Times New Roman", face = "bold"),
    legend.key.size = grid::unit(0.5, "lines"),
    legend.position = c(0.1, 0.1), legend.justification = c(0, 0),
    legend.background = ggplot2::element_rect(fill = "transparent", colour = NA)
  ) + ggspatial::annotation_scale(location = "bl", width_hint = 0.5)

app3_png <- file.path(tempdir(), "Appendix_3_Figure.png")
ggplot2::ggsave(app3_png, Appendix_3_Figure, width = 8, height = 6, dpi = 300)
save_plot_docx(app3_png, file.path(appendix_dir, "Appendix_03_segment_types.docx"),
               title = app3_title,
               explanation = "This map shows the street-segment units used in the study area.",
               width = 6.5, height = 5)
if (file.exists(app3_png)) file.remove(app3_png)

# Appendix 4: Observation status map ------------------------------------------
app4_title <- paste0("Appendix 4: Graffiti observation status (N segments = ", count_total_segments, ")")

Appendix_4_Figure <- ggplot2::ggplot(sf_ghent_street_segments) +
  ggplot2::geom_sf(ggplot2::aes(fill = observed_status)) +
  ggplot2::scale_fill_manual(
    values = c("At least with one graffiti" = "grey20", "Only Others" = "grey45",
               "Not observed" = "grey75", "Zero graffiti" = "grey95"),
    name = "Street segment",
    labels = c(
      paste0("With classified graffiti (", count_segments_with_graffiti, ")"),
      paste0("Not observed (", count_not_observed, ")"),
      paste0("With only 'Other' graffiti (", count_only_others, ")"),
      paste0("With no graffiti (", count_zero_graffiti, ")"))) +
  ggplot2::labs(title = NULL) + custom_theme +
  ggplot2::theme(
    text = ggplot2::element_text(family = "Times New Roman", size = 12),
    legend.text = ggplot2::element_text(size = 10, family = "Times New Roman"),
    legend.key.size = grid::unit(0.5, "lines"),
    legend.position = c(0.2, 0.2),
    legend.background = ggplot2::element_rect(fill = "white", colour = "grey80")
  ) + ggspatial::annotation_scale(location = "bl", width_hint = 0.5)

app4_png <- file.path(tempdir(), "Appendix_4_Figure.png")
ggplot2::ggsave(app4_png, Appendix_4_Figure, width = 8, height = 7, dpi = 300)
save_plot_docx(app4_png, file.path(appendix_dir, "Appendix_04_observation_status.docx"),
               title = app4_title,
               explanation = "This map shows which street segments were observed, excluded, or not observed.",
               width = 6.5, height = 5.5)
if (file.exists(app4_png)) file.remove(app4_png)

# Graffiti type summaries ------------------------------------------------------
df_graffiti_summary <- df_graffiti_data_clean |>
  dplyr::filter(graffiti_types_grouped != "no graffiti") |>
  janitor::tabyl(graffiti_types_grouped) |>
  janitor::adorn_totals("row") |>
  janitor::adorn_pct_formatting(digits = 1) |>
  dplyr::arrange(n)

df_graffiti_data_final <- df_graffiti_data_clean |>
  dplyr::filter(graffiti_types_grouped != "Others",
                graffiti_types_grouped != "no graffiti")

df_graffiti_summary_no_others <- df_graffiti_data_final |>
  janitor::tabyl(graffiti_types_grouped) |>
  janitor::adorn_totals("row") |>
  janitor::adorn_pct_formatting(digits = 1) |>
  dplyr::arrange(n)

n_observed_focal_segments <- dplyr::n_distinct(df_graffiti_data_final$street_segment)

df_graffiti_summary_stats <- df_graffiti_data_final |>
  dplyr::group_by(street_segment, graffiti_types_grouped) |>
  dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
  tidyr::complete(street_segment, graffiti_types_grouped, fill = list(count = 0)) |>
  dplyr::group_by(graffiti_types_grouped) |>
  dplyr::summarise(
    min = min(count), max = max(count),
    mean = mean(count), sd = sd(count),
    total = sum(count), .groups = "drop") |>
  dplyr::mutate(percentage = (total / sum(total)) * 100) |>
  dplyr::arrange(total)

# Table 1 Word file ------------------------------------------------------------
fmt_int <- function(x) format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_num <- function(x, d = 2) formatC(as.numeric(x), format = "f", digits = d, big.mark = ",")
fmt_pct <- function(x, d = 2) formatC(as.numeric(x), format = "f", digits = d)

Table_1 <- df_graffiti_summary_stats |>
  dplyr::transmute(`Graffiti type` = graffiti_types_grouped,
                   Minimum = fmt_int(min), Maximum = fmt_int(max),
                   Mean = fmt_num(mean), SD = fmt_num(sd),
                   `Total count` = fmt_int(total), Percentage = fmt_pct(percentage))

Table_1_ft <- flextable::flextable(Table_1) |>
  apa_table_style(left_columns = "Graffiti type", font_size = 10) |>
  flextable::italic(j = "SD", part = "header")

doc_t1 <- officer::read_docx()
doc_t1 <- officer::body_add_fpar(doc_t1, appendix_text_fpar("Table 1"))
doc_t1 <- officer::body_add_fpar(doc_t1, appendix_title_fpar(
  "Numbers of Graffiti per Street Segment by Visual Complexity"))
doc_t1 <- flextable::body_add_flextable(doc_t1, Table_1_ft)
doc_t1 <- officer::body_add_fpar(doc_t1, appendix_note_fpar(
  paste0("N = ", fmt_int(n_observed_focal_segments), " street segments.")))
tmp_t1 <- tempfile(fileext = ".docx"); print(doc_t1, target = tmp_t1)
file.copy(tmp_t1, file.path(tables_dir, "Table_1_Numbers_of_graffiti_per_street_segment.docx"), overwrite = TRUE)
if (file.exists(tmp_t1)) file.remove(tmp_t1)

# Analytic count matrix --------------------------------------------------------
df_counts_wide <- df_graffiti_data_final |>
  dplyr::count(street_segment, graffiti_types_grouped, name = "count") |>
  tidyr::complete(street_segment, graffiti_types_grouped, fill = list(count = 0)) |>
  tidyr::pivot_wider(names_from = graffiti_types_grouped, values_from = count)

data_matrix_observed <- as.matrix(df_counts_wide[, -1])
rownames(data_matrix_observed) <- df_counts_wide$street_segment

target_comparisons <- c("Masterpiece vs Tag", "Masterpiece vs SITS", "Tag vs SITS")
transformation_order <- c("No Transformation", "Square Root Transformation",
                           "Log Transformation + 0.5", "Log Transformation + 1")

# Observed MHI -----------------------------------------------------------------
observed_MHI <- compute_MHI(data_matrix_observed)

df_observed_MHI <- dplyr::bind_rows(lapply(names(observed_MHI), function(name) {
  as.data.frame(as.table(observed_MHI[[name]]$matrix)) |>
    dplyr::filter(Var1 != Var2) |>
    dplyr::transmute(`Log Transformations` = name,
                     `Graffiti Types`      = paste(Var1, "vs", Var2),
                     `Observed MHI Value`  = as.numeric(Freq)) |>
    dplyr::filter(`Graffiti Types` %in% target_comparisons)
})) |>
  dplyr::mutate(
    `Log Transformations` = factor(`Log Transformations`, levels = transformation_order),
    `Graffiti Types`      = factor(`Graffiti Types`,      levels = target_comparisons)) |>
  dplyr::arrange(`Log Transformations`, `Graffiti Types`)

# Main Fixed-Fixed permutation -------------------------------------------------
n_iter_3 <- analysis_config$main_permutations
perm_file_3 <- file.path(cache_dir, paste0("df_permutated_MHI_3types_FF_", n_iter_3, "iter.rds"))

permutated_MHI_3 <- run_FF_permutation(
  dm = data_matrix_observed, transformation_order = transformation_order,
  target_pairs = target_comparisons, n_iterations = n_iter_3,
  seed = analysis_config$main_seed, cache_file = perm_file_3)

df_combined <- build_combined_table(df_observed_MHI, permutated_MHI_3,
                                    transformation_order, target_comparisons)

# Observation details ----------------------------------------------------------
observation_details <- data.frame(
  Label = c("Total Observers", "Total Street Segments", "Observed Street Segments",
            "Not Observed Street Segments", "Zero Graffiti Street Segments",
            "Street Segments with at least One Classified Graffiti Type",
            "Street Segments Only with Other"),
  Count = c(unique_observers, count_total_segments, count_observed_segments,
            count_not_observed, count_zero_graffiti,
            count_segments_with_graffiti, count_only_others)
)

# Segment geometry (Appendix 5) ------------------------------------------------
df_segment_geometry_summary <- dplyr::bind_rows(
  summarize_segment_geometry(sf_ghent_street_segments, "All segments"),
  summarize_segment_geometry(
    dplyr::filter(sf_ghent_street_segments, observed_status != "Not observed"),
    "Observed segments"),
  summarize_segment_geometry(
    dplyr::filter(sf_ghent_street_segments, observed_status == "At least with one graffiti"),
    "Analytic focal segments")
) |> sf::st_drop_geometry()

App5_display <- df_segment_geometry_summary |>
  dplyr::transmute(
    Sample,
    `Number of segments` = fmt_int(N),
    `Mean length (m)` = fmt_num(`Mean Length (m)`, 1),
    `Standard deviation of length (m)` = fmt_num(`SD Length (m)`, 1),
    `Mean area (square metres)` = fmt_num(`Mean Area (m2)`, 1),
    `Standard deviation of area (square metres)` = fmt_num(`SD Area (m2)`, 1)
  )
App5_ft <- flextable::flextable(App5_display) |>
  apa_table_style(left_columns = "Sample", font_size = 8, max_width = 9.8)

save_flextable_appendix(App5_ft, file.path(appendix_dir, "Appendix_05_segment_geometry.docx"),
  captions     = "Table A5|Street-Segment Geometry by Sample",
  explanations = "Length is the source street-segment LENGTE attribute; area is calculated from the polygon geometry. Neither measure describes writable surfaces.",
  section = appendix_landscape,
  document_caption = "Appendix 5: Street-Segment Geometry")

# Appendix 6: Transformation sensitivity --------------------------------------
if (!all(df_combined$`Observed MHI Value` < df_combined$`Mean Permutated MHI`)) {
  stop("Appendix 6 direction statement is not supported by every transformation result.")
}
App6_ft <- make_mhi_flextable(df_combined)
save_flextable_appendix(App6_ft, file.path(appendix_dir, "Appendix_06_transformation_sensitivity.docx"),
  captions     = "Table A6|Transformation Sensitivity for the Three Grouped Types",
  explanations = paste0("The lower co-presence pattern remains stable across transformations. Based on ",
    format(n_iter_3, big.mark = ","), " Fixed-Fixed permutations."),
  section = appendix_portrait,
  document_caption = "Appendix 6: Transformation Sensitivity")

# Appendix 7: Spatial sensitivity ---------------------------------------------
sf_focal <- sf_ghent_street_segments |>
  dplyr::filter(UIDN %in% rownames(data_matrix_observed))
sf_focal <- sf_focal[match(rownames(data_matrix_observed), sf_focal$UIDN), ]

n_blocks <- analysis_config$spatial_default_blocks
block_id <- assign_spatial_blocks(sf_focal, n_blocks)
df_spatial_blocks <- tibble::tibble(street_segment = sf_focal$UIDN, block_id = block_id)
df_spatial_block_summary_main <- summarize_block_assignment(block_id) |>
  dplyr::mutate(requested_blocks = n_blocks,
                constructed_cells = ceiling(sqrt(n_blocks))^2, .before = 1)

n_iter_spatial   <- analysis_config$spatial_permutations
perm_file_spatial <- file.path(cache_dir, paste0("df_permutated_MHI_3types_spatialFF_",
  n_iter_spatial, "iter_", n_blocks, "blocks.rds"))

permutated_MHI_3_spatial <- run_spatial_FF_permutation(
  dm = data_matrix_observed, block_id = block_id,
  transformation_order = transformation_order, target_pairs = target_comparisons,
  n_iterations = n_iter_spatial, seed = analysis_config$spatial_seed,
  cache_file = perm_file_spatial)

df_combined_spatial <- build_combined_table(df_observed_MHI, permutated_MHI_3_spatial,
                                            transformation_order, target_comparisons)
if (!all(df_combined_spatial$`Observed MHI Value` <
         df_combined_spatial$`Mean Permutated MHI`)) {
  stop("Appendix 7 direction statement is not supported by every main spatial result.")
}

block_sizes <- analysis_config$spatial_block_requests
.sens <- lapply(block_sizes, function(nb) {
  pf <- file.path(cache_dir, paste0("df_permutated_MHI_3types_spatialFF_",
    n_iter_spatial, "iter_", nb, "blocks.rds"))
  bid <- if (nb == n_blocks) block_id else assign_spatial_blocks(sf_focal, nb)
  pnb <- if (nb == n_blocks) permutated_MHI_3_spatial else
    run_spatial_FF_permutation(data_matrix_observed, bid, transformation_order,
                               target_comparisons, n_iter_spatial,
                               analysis_config$spatial_seed, pf)
  res <- if (nb == n_blocks) df_combined_spatial else
    build_combined_table(df_observed_MHI, pnb, transformation_order, target_comparisons)
  list(
    mhi  = res |> dplyr::filter(`Log Transformations` == "Log Transformation + 1") |>
      dplyr::mutate(n_blocks = nb, realized = length(unique(bid))) |>
      dplyr::select(n_blocks, realized, `Graffiti Types`,
                    `Observed MHI Value`, `Mean Permutated MHI`, `P Value`),
    diag = summarize_block_assignment(bid) |>
      dplyr::mutate(requested_blocks = nb,
                    constructed_cells = ceiling(sqrt(nb))^2, .before = 1)
  )
})
df_block_sensitivity       <- dplyr::bind_rows(lapply(.sens, `[[`, "mhi"))
df_block_assignment_diagnostics <- dplyr::bind_rows(lapply(.sens, `[[`, "diag"))
rm(.sens)

App7_block_display <- df_block_sensitivity |>
  dplyr::mutate(
    dplyr::across(where(is.factor), as.character),
    `Graffiti Types` = apa_pair_label(`Graffiti Types`),
    `Observed MHI Value` = apa_no_leading_zero(`Observed MHI Value`),
    `Mean Permutated MHI` = apa_no_leading_zero(`Mean Permutated MHI`),
    `P Value` = apa_p_value(`P Value`)
  ) |>
  dplyr::rename(
    `Requested blocks` = n_blocks,
    `Occupied blocks` = realized,
    Pair = `Graffiti Types`,
    `Observed MHI` = `Observed MHI Value`,
    `Mean null MHI` = `Mean Permutated MHI`,
    p = `P Value`
  )
App7_diag_display <- df_block_assignment_diagnostics |>
  dplyr::rename(
    `Requested blocks` = requested_blocks,
    `Constructed cells` = constructed_cells,
    `Occupied blocks` = realized_blocks,
    `Minimum segments` = min_seg,
    `Median segments` = median_seg,
    `Maximum segments` = max_seg
  )

save_flextable_appendix(
  tables = list(make_mhi_flextable(df_combined_spatial),
    flextable::flextable(App7_block_display) |>
      apa_table_style(left_columns = "Pair", font_size = 8, max_width = 9.8) |>
      flextable::italic(j = "p", part = "header"),
    flextable::flextable(App7_diag_display) |>
      apa_table_style(font_size = 9)),
  docx_path = file.path(appendix_dir, "Appendix_07_spatial_sensitivity.docx"),
  captions  = c("Table A7.1|Spatially Constrained Fixed-Fixed Sensitivity",
                "Table A7.2|Block-Count Sensitivity",
                "Table A7.3|Block-Assignment Diagnostics"),
  explanations = c(
    paste0("The pattern remains stable under spatially constrained permutations. Based on ", format(n_iter_spatial, big.mark=","), " permutations."),
    "This table checks sensitivity to the number of spatial blocks.",
    "A square grid has ceiling(sqrt(requested blocks)) cells per side; occupied cells contain at least one analytic segment."),
  table_sections = list(appendix_portrait, appendix_portrait, appendix_landscape),
  document_caption = "Appendix 7: Spatially Constrained Fixed-Fixed Sensitivity")

# Appendix 8: Six-type sensitivity --------------------------------------------
df_6 <- df_graffiti_data_clean |>
  dplyr::filter(graffiti_type %in% c("tag","throw up","stencil","illustration","slogan","masterpiece"))
six_types <- sort(unique(df_6$graffiti_type))
all_pairs_6 <- sort(combn(six_types, 2, FUN = function(x) paste(x[1],"vs",x[2])))

df_wide_6 <- df_6 |>
  dplyr::count(street_segment, graffiti_type) |>
  tidyr::complete(street_segment, graffiti_type, fill = list(n = 0)) |>
  tidyr::pivot_wider(names_from = graffiti_type, values_from = n) |>
  dplyr::arrange(street_segment)
data_matrix_6 <- as.matrix(df_wide_6[, six_types]); rownames(data_matrix_6) <- df_wide_6$street_segment

df_obs_6 <- dplyr::bind_rows(lapply(transformation_order, function(tr) {
  mhi <- compute_MHI(data_matrix_6)[[tr]]$matrix
  as.data.frame(as.table(mhi)) |> dplyr::filter(Var1 != Var2) |>
    dplyr::transmute(`Log Transformations`=tr, `Graffiti Types`=paste(Var1,"vs",Var2),
                     `Observed MHI Value`=as.numeric(Freq)) |>
    dplyr::filter(`Graffiti Types` %in% all_pairs_6)
})) |> dplyr::mutate(
  `Log Transformations` = factor(`Log Transformations`, levels = transformation_order),
  `Graffiti Types`      = factor(`Graffiti Types`,      levels = all_pairs_6)) |>
  dplyr::arrange(`Log Transformations`, `Graffiti Types`)

n_iter_6 <- analysis_config$main_permutations
perm_file_6 <- file.path(cache_dir, paste0("df_permutated_MHI_6types_FF_", n_iter_6, "iter.rds"))
permutated_MHI_6 <- run_FF_permutation(data_matrix_6, transformation_order,
                                       all_pairs_6, n_iter_6,
                                       analysis_config$main_seed, perm_file_6)
df_combined_6 <- build_combined_table(df_obs_6, permutated_MHI_6, transformation_order, all_pairs_6)
if (!all(df_combined_6$`Observed MHI Value` < df_combined_6$`Mean Permutated MHI`)) {
  stop("Appendix 8 direction statement is not supported by every six-type result.")
}

save_flextable_appendix(make_mhi_flextable(df_combined_6),
  file.path(appendix_dir, "Appendix_08_six_type_sensitivity.docx"),
  captions = "Table A8|Six-Type Disaggregation Sensitivity",
  explanations = paste0("The overall pattern remains stable. Based on ",
    format(n_iter_6, big.mark=","), " Fixed-Fixed permutations."),
  section = appendix_portrait,
  document_caption = "Appendix 8: Six-Type Disaggregation Sensitivity")

# Appendix 9: Other-inclusion sensitivity -------------------------------------
four_types <- c("Masterpiece","SITS","Tag","Other")
all_pairs_4 <- sort(combn(four_types, 2, FUN = function(x) paste(x[1],"vs",x[2])))

df_wide_4 <- df_graffiti_data_clean |>
  dplyr::filter(graffiti_types_grouped != "no graffiti") |>
  dplyr::mutate(type4 = dplyr::case_when(
    graffiti_types_grouped == "Masterpiece" ~ "Masterpiece",
    graffiti_types_grouped == "SITS"        ~ "SITS",
    graffiti_types_grouped == "Tag"         ~ "Tag",
    graffiti_types_grouped == "Others"      ~ "Other",
    TRUE ~ NA_character_)) |>
  dplyr::filter(!is.na(type4)) |>
  dplyr::count(street_segment, type4) |>
  tidyr::complete(street_segment, type4 = four_types, fill = list(n = 0)) |>
  tidyr::pivot_wider(names_from = type4, values_from = n) |>
  dplyr::arrange(street_segment)
data_matrix_4 <- as.matrix(df_wide_4[, four_types]); rownames(data_matrix_4) <- df_wide_4$street_segment

df_obs_4 <- dplyr::bind_rows(lapply(transformation_order, function(tr) {
  mhi <- compute_MHI(data_matrix_4)[[tr]]$matrix
  as.data.frame(as.table(mhi)) |> dplyr::filter(Var1 != Var2) |>
    dplyr::transmute(`Log Transformations`=tr, `Graffiti Types`=paste(Var1,"vs",Var2),
                     `Observed MHI Value`=as.numeric(Freq)) |>
    dplyr::filter(`Graffiti Types` %in% all_pairs_4)
})) |> dplyr::mutate(
  `Log Transformations` = factor(`Log Transformations`, levels = transformation_order),
  `Graffiti Types`      = factor(`Graffiti Types`,      levels = all_pairs_4)) |>
  dplyr::arrange(`Log Transformations`, `Graffiti Types`)

n_iter_4 <- analysis_config$main_permutations
perm_file_4 <- file.path(cache_dir, paste0("df_permutated_MHI_4types_FF_", n_iter_4, "iter.rds"))
permutated_MHI_4 <- run_FF_permutation(data_matrix_4, transformation_order,
                                       all_pairs_4, n_iter_4,
                                       analysis_config$main_seed, perm_file_4)
df_combined_4 <- build_combined_table(df_obs_4, permutated_MHI_4, transformation_order, all_pairs_4)
app9_main_pairs <- df_combined_4 |>
  dplyr::filter(as.character(`Graffiti Types`) %in%
    c("Masterpiece vs Tag", "Masterpiece vs SITS", "SITS vs Tag"))
if (nrow(app9_main_pairs) != length(transformation_order) * length(target_comparisons) ||
    !all(app9_main_pairs$`Observed MHI Value` <
         app9_main_pairs$`Mean Permutated MHI`)) {
  stop("Appendix 9 main-pair direction statement is not supported by every result.")
}

save_flextable_appendix(make_mhi_flextable(df_combined_4),
  file.path(appendix_dir, "Appendix_09_other_category_sensitivity.docx"),
  captions = "Table A9|Sensitivity Including the Other Category",
  explanations = paste0("The main pairwise comparisons remain lower than expected. Based on ",
    format(n_iter_4, big.mark=","), " Fixed-Fixed permutations."),
  section = appendix_portrait,
  document_caption = "Appendix 9: Sensitivity Including the Other Category")

# Appendix 10: Misclassification sensitivity ----------------------------------
irr_scenarios  <- analysis_config$misclassification_scenarios
irr_rates      <- analysis_config$misclassification_error_rates
irr_n_reps     <- analysis_config$misclassification_reestimations
irr_n_perm     <- analysis_config$misclassification_permutations
irr_transitions <- dplyr::bind_rows(lapply(irr_scenarios, function(sc) {
  dplyr::bind_rows(lapply(irr_rates, function(er) {
    matrix <- make_confusion_matrix(er, sc, colnames(data_matrix_observed))
    data.frame(Scenario = sc, `Error Rate` = er,
               `Original Type` = rownames(matrix), matrix,
               check.names = FALSE)
  }))
}))
irr_cache_file <- file.path(cache_dir, paste0("df_irrsens_", irr_n_reps, "rep_", irr_n_perm, "perm.rds"))

if (file.exists(irr_cache_file)) {
  df_irr_draws <- readRDS(irr_cache_file)
} else {
  set.seed(analysis_config$misclassification_seed)
  runs <- vector("list", length(irr_scenarios) * length(irr_rates) * irr_n_reps)
  k <- 1
  for (sc in irr_scenarios) for (er in irr_rates) {
    conf <- make_confusion_matrix(er, sc, colnames(data_matrix_observed))
    if (any(abs(rowSums(conf) - 1) > 1e-12)) stop("Invalid misclassification probabilities.")
    for (rep in seq_len(irr_n_reps)) {
      runs[[k]] <- ff_pvals_log1(simulate_misclassification(data_matrix_observed, conf),
                                 target_comparisons, irr_n_perm) |>
        dplyr::mutate(Scenario = sc, `Error Rate` = er, Replicate = rep,
                      `Deficit (%)` = (`Null Mean MHI` - `Observed MHI Value`) / `Null Mean MHI` * 100)
      k <- k + 1
    }
  }
  df_irr_draws <- dplyr::bind_rows(runs)
  saveRDS(df_irr_draws, irr_cache_file)
}

df_irr_summary <- df_irr_draws |>
  dplyr::group_by(Scenario, `Error Rate`, `Graffiti Types`) |>
  dplyr::summarise(
    `Median Observed MHI`  = round(median(`Observed MHI Value`, na.rm=TRUE), 3),
    `Median Null Mean MHI` = round(median(`Null Mean MHI`,       na.rm=TRUE), 3),
    `Median Deficit (%)`   = round(median(`Deficit (%)`,         na.rm=TRUE), 1),
    `Min Deficit (%)`      = round(min(`Deficit (%)`,            na.rm=TRUE), 1),
    `Max P Value`          = round(max(`P Value`,                na.rm=TRUE), 3),
    `Share P < 0.05`       = sprintf("%.2f", mean(`P Value` < analysis_config$alpha, na.rm=TRUE)),
    `Share P < 0.01`       = sprintf("%.2f", mean(`P Value` < analysis_config$secondary_alpha, na.rm=TRUE)),
    .groups = "drop") |>
  dplyr::arrange(Scenario, as.numeric(`Error Rate`), `Graffiti Types`) |>
  dplyr::mutate(`Error Rate` = paste0(round(as.numeric(`Error Rate`) * 100), "%"))

df_irr_grouped_verification <- df_irr_draws |>
  dplyr::group_by(Scenario, `Error Rate`, `Graffiti Types`) |>
  dplyr::summarise(
    `Valid re-estimations` = sum(stats::complete.cases(
      `Observed MHI Value`, `Null Mean MHI`, `P Value`, `Deficit (%)`)),
    `Number p < .05` = sum(`P Value` < analysis_config$alpha, na.rm = TRUE),
    `Percentage p < .05` = 100 * mean(`P Value` < analysis_config$alpha, na.rm = TRUE),
    `Number observed < null` = sum(`Observed MHI Value` < `Null Mean MHI`, na.rm = TRUE),
    `Minimum p` = min(`P Value`, na.rm = TRUE),
    `Maximum p` = max(`P Value`, na.rm = TRUE),
    `Minimum observed MHI` = min(`Observed MHI Value`, na.rm = TRUE),
    `Maximum observed MHI` = max(`Observed MHI Value`, na.rm = TRUE),
    `Minimum deficit (%)` = min(`Deficit (%)`, na.rm = TRUE),
    `Maximum deficit (%)` = max(`Deficit (%)`, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(Scenario, as.numeric(`Error Rate`), `Graffiti Types`)

App10_display <- df_irr_grouped_verification |>
  dplyr::transmute(
    Scenario,
    `Error rate` = paste0(round(100 * as.numeric(`Error Rate`)), "%"),
    Pair = apa_pair_label(`Graffiti Types`),
    `Valid re-estimations`,
    `p < .05, n (%)` = paste0(`Number p < .05`, " (", round(`Percentage p < .05`), "%)"),
    `Observed < null, n` = `Number observed < null`,
    `p range` = paste0(apa_no_leading_zero(`Minimum p`), "-", apa_no_leading_zero(`Maximum p`)),
    `Observed MHI range` = paste0(apa_no_leading_zero(`Minimum observed MHI`), "-", apa_no_leading_zero(`Maximum observed MHI`)),
    `Deficit range (%)` = paste0(sprintf("%.1f", `Minimum deficit (%)`), " to ",
                                 sprintf("%.1f", `Maximum deficit (%)`))
  )
App10_ft <- flextable::flextable(App10_display) |>
  apa_table_style(left_columns = c("Scenario", "Pair"), font_size = 7, max_width = 10.2) |>
  flextable::italic(j = c("p < .05, n (%)", "Observed < null, n", "p range"),
                    part = "header")

App10_transition_display <- irr_transitions |>
  dplyr::arrange(Scenario, as.numeric(`Error Rate`), `Original Type`) |>
  dplyr::mutate(
    `Error Rate` = paste0(round(100 * as.numeric(`Error Rate`)), "%"),
    dplyr::across(c(Masterpiece, SITS, Tag), ~ apa_no_leading_zero(.x))
  ) |>
  dplyr::rename(`Error rate` = `Error Rate`, `Original type` = `Original Type`)
App10_transition_ft <- flextable::flextable(App10_transition_display) |>
  apa_table_style(left_columns = c("Scenario", "Original type"), font_size = 8,
                  max_width = 10.2)

save_flextable_appendix(list(App10_ft, App10_transition_ft),
  file.path(appendix_dir, "Appendix_10_misclassification_sensitivity.docx"),
  captions = c("Table A10.1|Simulated Graffiti-Type Misclassification Sensitivity",
               "Table A10.2|Assumed Classification-Transition Probabilities"),
  explanations = c(paste0("Each row summarises ", irr_n_reps,
    " re-estimations with ", irr_n_perm,
    " Fixed-Fixed permutations per re-estimation. The lower-tail p-value is (b + 1)/(",
    irr_n_perm, " + 1), where b is the number of permuted MHI values less than or equal to the observed MHI. ",
    "The deficit is 100 x (null mean MHI - observed MHI)/null mean MHI; positive values indicate less co-presence than the null mean. Classification probabilities are assumptions, not validation estimates."),
    "Rows identify the original type; the three type columns give unconditional reassignment probabilities. Each off-diagonal value equals the error rate times an assumed conditional allocation probability."),
  section = appendix_landscape,
  document_caption = "Appendix 10: Graffiti-Type Misclassification Sensitivity")

# Appendix 11: Overwriting descriptive ----------------------------------------
# The anonymized analysis file contains the finalized yes/no B.1 field. Source
# blanks were recoded before this file was created and are not distinguished here.
df_overwriting_analytic <- df_graffiti_data_final |>
  dplyr::filter(street_segment %in% rownames(data_matrix_observed),
                graffiti_types_grouped %in% c("Masterpiece", "SITS", "Tag"))

app11_three <- df_overwriting_analytic |>
  dplyr::group_by(graffiti_types_grouped) |>
  dplyr::summarise(
    yes = sum(overwriting == "yes"),
    no = sum(overwriting == "no"),
    total = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::rename(type = graffiti_types_grouped) |>
  dplyr::bind_rows(dplyr::summarise(
    df_overwriting_analytic,
    type = "All types",
    yes = sum(overwriting == "yes"),
    no = sum(overwriting == "no"),
    total = dplyr::n()
  ))

App11_display <- app11_three |>
  dplyr::select(type, yes, no, total) |>
  dplyr::rename(
    `Graffiti type` = type,
    Overpainting = yes,
    `Not overpainting` = no,
    Total = total
  )
App11_ft <- flextable::flextable(App11_display) |>
  apa_table_style(left_columns = "Graffiti type", font_size = 10)

save_flextable_appendix(App11_ft,
  file.path(appendix_dir, "Appendix_11_overpainting.docx"),
  captions = "Table A11|Overpainting Responses Among Analytic Graffiti Items",
  explanations = paste0(
    "This table reports overwriting counts for the three analytical graffiti types. ",
    "The overwriting field records whether a current item substantially overlaid earlier work. ",
    "It does not identify the type of the covered work. ",
    "The anonymized analysis file contains the finalized yes/no field; source blanks were recoded as no before this file was created."),
  section = appendix_portrait,
  document_caption = "Appendix 11: Overpainting")

# Appendix 12: No-masterpiece-segment robustness (2-type FF) -----------------
segments_with_masterpiece <- rownames(data_matrix_observed)[data_matrix_observed[, "Masterpiece"] > 0]
data_matrix_2_nomaster <- data_matrix_observed[
  !(rownames(data_matrix_observed) %in% segments_with_masterpiece),
  c("Tag", "SITS"),
  drop = FALSE
]

if (nrow(data_matrix_2_nomaster) < 2L) {
  stop("Appendix 12 check failed: fewer than 2 non-masterpiece segments remain.")
}

target_comparisons_2 <- "Tag vs SITS"
df_obs_2_nomaster <- dplyr::bind_rows(lapply(transformation_order, function(tr) {
  mhi <- compute_MHI(data_matrix_2_nomaster)[[tr]]$matrix
  as.data.frame(as.table(mhi)) |>
    dplyr::filter(Var1 != Var2) |>
    dplyr::transmute(
      `Log Transformations` = tr,
      `Graffiti Types` = paste(Var1, "vs", Var2),
      `Observed MHI Value` = as.numeric(Freq)
    ) |>
    dplyr::filter(`Graffiti Types` %in% target_comparisons_2)
})) |>
  dplyr::mutate(
    `Log Transformations` = factor(`Log Transformations`, levels = transformation_order),
    `Graffiti Types` = factor(`Graffiti Types`, levels = target_comparisons_2)
  ) |>
  dplyr::arrange(`Log Transformations`, `Graffiti Types`)

n_iter_2 <- analysis_config$no_masterpiece_permutations
perm_file_2 <- file.path(cache_dir, paste0("df_permutated_MHI_2types_FF_", n_iter_2, "iter_no_masterpiece_segments.rds"))
permutated_MHI_2_nomaster <- run_FF_permutation(
  dm = data_matrix_2_nomaster,
  transformation_order = transformation_order,
  target_pairs = target_comparisons_2,
  n_iterations = n_iter_2,
  seed = analysis_config$main_seed,
  cache_file = perm_file_2
)
df_combined_2_nomaster <- build_combined_table(
  df_obs_2_nomaster,
  permutated_MHI_2_nomaster,
  transformation_order,
  target_comparisons_2
)

df_nomaster_segment_counts <- data.frame(
  Measure = c(
    "Analytic segments (3-type main model)",
    "Segments containing at least one masterpiece",
    "Segments retained after excluding masterpiece segments",
    "Tag items in retained segments",
    "SITS items in retained segments"
  ),
  Count = c(
    nrow(data_matrix_observed),
    length(segments_with_masterpiece),
    nrow(data_matrix_2_nomaster),
    sum(data_matrix_2_nomaster[, "Tag"]),
    sum(data_matrix_2_nomaster[, "SITS"])
  )
)

App12_ft_results <- make_mhi_flextable(df_combined_2_nomaster)
App12_ft_counts <- df_nomaster_segment_counts |>
  dplyr::mutate(Count = format(Count, big.mark = ",", scientific = FALSE, trim = TRUE)) |>
  flextable::flextable() |>
  apa_table_style(left_columns = "Measure", font_size = 9)

save_flextable_appendix(
  tables = list(App12_ft_results, App12_ft_counts),
  docx_path = file.path(appendix_dir, "Appendix_12_no_masterpiece_segment_robustness.docx"),
  captions = c(
    "Table A12.1|Two-Type Fixed-Fixed Check After Excluding Masterpiece Segments",
    "Table A12.2|Segment and Item Counts for the Exclusion Sample"
  ),
  explanations = c(
    paste0(
      "This check repeats the MHI Fixed-Fixed test for Tag vs SITS after excluding all segments with at least one masterpiece. ",
      "The null matrices are regenerated from the reduced two-type matrix and preserve its segment totals and its Tag and SITS totals. Based on ",
      format(n_iter_2, big.mark = ","), " Fixed-Fixed permutations."
    ),
    "Counts report how many street segments and items remain after exclusion."
  ),
  section = appendix_portrait,
  document_caption = "Appendix 12: Robustness Check Excluding Masterpiece-Containing Street Segments"
)

# All results ------------------------------------------------------------------
all_results <- list(
  "Observation Details"           = observation_details,
  "Segment Geometry Summary"      = df_segment_geometry_summary,
  "Raw Graffiti Summary"          = df_graffiti_summary,
  "Main Graffiti Summary"         = df_graffiti_summary_no_others,
  "Graffiti Summary Stats"        = df_graffiti_summary_stats,
  "Observed MHI (3 types)"        = df_observed_MHI |>
    dplyr::mutate(dplyr::across(where(is.factor), as.character)),
  "MHI Results (3 types, FF)"     = df_combined     |> dplyr::mutate(dplyr::across(where(is.factor), as.character)),
  "MHI Results (3 types, SpatFF)" = df_combined_spatial |> dplyr::mutate(dplyr::across(where(is.factor), as.character)),
  "Spatial Block Assignment"      = df_spatial_blocks,
  "Block Sensitivity"             = df_block_sensitivity |> dplyr::mutate(dplyr::across(where(is.factor), as.character)),
  "Spatial Block Summary (Main)"  = df_spatial_block_summary_main,
  "Spatial Block Summary (Sens)"  = df_block_assignment_diagnostics,
  "MHI Results (6 types, FF)"     = df_combined_6 |> dplyr::mutate(dplyr::across(where(is.factor), as.character)),
  "MHI Results (4 types, FF)"     = df_combined_4 |> dplyr::mutate(dplyr::across(where(is.factor), as.character)),
  "MHI Results (2 types no masterpiece seg, FF)" = df_combined_2_nomaster |>
    dplyr::mutate(dplyr::across(where(is.factor), as.character)),
  "No-Masterpiece Segment Counts" = df_nomaster_segment_counts,
  "Misclass Sens Summary"         = df_irr_summary,
  "Misclass Grouped Verification" = df_irr_grouped_verification,
  "Misclass Sens Draws"           = df_irr_draws,
  "Misclass Transitions"          = irr_transitions,
  "Overwriting Counts"            = app11_three
)

for (nm in names(all_results)) {
  csv_name <- paste0(gsub("[^A-Za-z0-9]+", "_", nm), ".csv")
  write.csv(all_results[[nm]], file.path(all_results_dir, csv_name), row.names = FALSE)
}
writexl::write_xlsx(all_results, file.path(all_results_dir, "all_results.xlsx"))

# Save analysis results used to create the figures ----------------------------
analysis_results <- c(all_results, list(
  observed_MHI         = observed_MHI,
  df_combined          = df_combined,
  df_combined_spatial  = df_combined_spatial,
  df_combined_6        = df_combined_6,
  df_combined_4        = df_combined_4,
  df_combined_2_nomaster = df_combined_2_nomaster,
  df_nomaster_segment_counts = df_nomaster_segment_counts,
  df_irr_summary       = df_irr_summary,
  df_block_assignment_diagnostics = df_block_assignment_diagnostics,
  data_matrix_observed = data_matrix_observed,
  data_matrix_6        = data_matrix_6,
  data_matrix_4        = data_matrix_4,
  data_matrix_2_nomaster = data_matrix_2_nomaster,
  sf_ghent_street_segments = sf_ghent_street_segments,
  analysis_settings = list(main_permutations = n_iter_3,
    main_seed = analysis_config$main_seed,
    spatial_permutations = n_iter_spatial,
    spatial_seed = analysis_config$spatial_seed,
    spatial_block_requests = block_sizes,
    no_masterpiece_permutations = n_iter_2,
    misclassification_reestimations = irr_n_reps,
    misclassification_permutations = irr_n_perm,
    misclassification_error_rates = irr_rates,
    misclassification_scenarios = irr_scenarios,
    misclassification_seed = analysis_config$misclassification_seed,
    alpha = analysis_config$alpha,
    secondary_alpha = analysis_config$secondary_alpha,
    r_version = analysis_r_version,
    vegan_version = analysis_vegan_version)
))
saveRDS(analysis_results, file.path(cache_dir, "analysis_results.rds"))

build_manuscript_figures(here::here(), figures_dir)
combine_appendices(appendix_dir)

message("\nBuild complete. Outputs written to:\n  ",
        paste(c(appendix_dir, figures_dir, all_results_dir), collapse = "\n  "))
