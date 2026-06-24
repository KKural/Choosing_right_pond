# Load helper functions
source(here::here("Scripts", "Choosing_right_pond_functions.R"))

# Create dated output folder for this script run
folder_name <- make_folder()

appendix_dir <- here::here(folder_name, "appendix")
cache_dir <- here::here(folder_name, "cache")

if (!dir.exists(appendix_dir)) dir.create(appendix_dir, recursive = TRUE)
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

all_output_dirs <- list.dirs(here::here(), recursive = FALSE, full.names = FALSE)
candidate_outputs <- grep(
  "^[0-9]{8}_script_choosing_right_pond_outputs$",
  all_output_dirs,
  value = TRUE
)
candidate_outputs <- setdiff(candidate_outputs, folder_name)

previous_cache_dir <- if (length(candidate_outputs) > 0) {
  here::here(sort(candidate_outputs, decreasing = TRUE)[1], "cache")
} else {
  NA_character_
}

if (!is.na(previous_cache_dir) && dir.exists(previous_cache_dir) && length(list.files(cache_dir, pattern = "\\.rds$")) == 0) {
  invisible(file.copy(
    from = list.files(previous_cache_dir, pattern = "\\.rds$", full.names = TRUE),
    to = cache_dir,
    overwrite = TRUE
  ))
}

observation_form_source <- here::here("Data", "Appendix_1_Observation_Form.docx")

if (file.exists(observation_form_source)) {
  save_existing_docx_with_explanation(
    source_docx = observation_form_source,
    docx_path = file.path(appendix_dir, "Appendix_1_Observation_Form.docx"),
    explanation = "This appendix shows the structured form used to record graffiti type, medium, colour, size, surface, and readability."
  )
} else {
  stop("Missing observation form source: ", observation_form_source)
}

# Read input data
df_graffiti_data_clean <- read.csv(here::here("data", "df_graffiti_items_anonymized.csv")) |>
  dplyr::select(street_segment, observer_id, graffiti_type, graffiti_types_grouped) |>
  dplyr::mutate(
    street_segment = as.character(street_segment),
    graffiti_types_grouped = dplyr::recode(graffiti_types_grouped, "No graffiti" = "no graffiti")
  )

# Summarise graffiti status by street segment
graffiti_status <- df_graffiti_data_clean |>
  dplyr::group_by(street_segment) |>
  dplyr::summarize(
    no_graffiti  = all(graffiti_types_grouped == "no graffiti"),
    only_others  = all(graffiti_types_grouped == "Others"),
    has_graffiti = any(graffiti_types_grouped != "no graffiti" & graffiti_types_grouped != "Others"),
    .groups = "drop"
  )

# Join graffiti status to street segments
sf_ghent_street_segments <- dplyr::bind_rows(
  sf::st_read(dsn = here::here("data", "WBN3.shp"), quiet = TRUE),
  sf::st_read(dsn = here::here("data", "BRUGGEN 2.shp"), quiet = TRUE)
) |>
  dplyr::select(-TYPE, -OPNDATUM, -VORM, -LBLVORM) |>
  dplyr::mutate(
    LBLTYPE = factor(
      dplyr::case_match(
        LBLTYPE,
        "kruispuntzone" ~ "intersection",
        "wegsegment"    ~ "street segment",
        "overbrugging"  ~ "bridge",
        .default = NA_character_
      ),
      levels = c("street segment", "intersection", "bridge")
    ),
    UIDN = as.character(UIDN)
  ) |>
  dplyr::left_join(graffiti_status, by = c("UIDN" = "street_segment")) |>
  dplyr::mutate(
    observed_status = dplyr::case_when(
      has_graffiti ~ "At least with one graffiti",
      only_others  ~ "Only Others",
      no_graffiti  ~ "Zero graffiti",
      TRUE         ~ "Not observed"
    )
  ) |>
  dplyr::select(-no_graffiti, -has_graffiti, -only_others)

count_total_segments <- nrow(sf_ghent_street_segments)

# Appendix 2: Map of Ghent street segments by type --------------------
appendix_2_title <- paste0("Appendix 2: Street segment classification (N = ", count_total_segments, ")")

Appendix_2_Figure <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = sf_ghent_street_segments, ggplot2::aes(fill = LBLTYPE)) +
  ggplot2::scale_fill_manual(
    values = c(
      "street segment" = "grey60",
      "intersection"   = "grey40",
      "bridge"         = "grey20"
    ),
    labels = c("Street segment", "Intersection", "Bridge"),
    name   = "Segment type"
  ) +
  ggplot2::labs(title = NULL) +
  custom_theme +
  ggplot2::theme(
    text         = ggplot2::element_text(family = "Times New Roman"),
    plot.title   = ggplot2::element_text(size = 14, family = "Times New Roman", face = "bold", hjust = 0.5),
    legend.text  = ggplot2::element_text(size = 10, family = "Times New Roman"),
    legend.title = ggplot2::element_text(size = 12, family = "Times New Roman", face = "bold"),
    legend.key.size          = grid::unit(0.5, "lines"),
    legend.position          = c(0.1, 0.1),
    legend.justification     = c(0, 0),
    legend.background        = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.box.background    = ggplot2::element_rect(fill = "transparent", colour = NA)
  ) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.5)

file_path_Appendix_2_Figure_png <- file.path(tempdir(), "Appendix_2_Figure.png")
ggplot2::ggsave(
  filename  = file_path_Appendix_2_Figure_png,
  device    = "png",
  plot      = Appendix_2_Figure,
  width     = 8,
  height    = 6,
  dpi       = 300,
  units     = "in",
  limitsize = TRUE
)
file_path_Appendix_2_Figure_docx <- file.path(appendix_dir, "Appendix_2_Street_Segment_Types.docx")
save_plot_docx(
  png_path  = file_path_Appendix_2_Figure_png,
  docx_path = file_path_Appendix_2_Figure_docx,
  title     = appendix_2_title,
  explanation = "This map shows the street-segment units used in the study area. It separates standard street segments, intersections, and bridges.",
  width     = 6.5,
  height    = 5
)
if (file.exists(file_path_Appendix_2_Figure_png)) invisible(file.remove(file_path_Appendix_2_Figure_png))

# Count legend categories
unique_observers               <- dplyr::n_distinct(df_graffiti_data_clean$observer_id)
count_observed_segments_raw    <- dplyr::n_distinct(df_graffiti_data_clean$street_segment)
count_segments_with_graffiti   <- sum(sf_ghent_street_segments$observed_status == "At least with one graffiti", na.rm = TRUE)
count_only_others              <- sum(sf_ghent_street_segments$observed_status == "Only Others",    na.rm = TRUE)
count_zero_graffiti            <- sum(sf_ghent_street_segments$observed_status == "Zero graffiti",  na.rm = TRUE)
count_observed_segments        <- count_segments_with_graffiti + count_only_others + count_zero_graffiti
count_not_observed             <- sum(sf_ghent_street_segments$observed_status == "Not observed",   na.rm = TRUE)
# Appendix 3: Graffiti observation status map -------------------------
appendix_3_title <- paste0(
  "Appendix 3: Overview of Graffiti Observation (Street-segment network N = ",
  count_total_segments, "; observed segments N = ", count_observed_segments_raw, ")"
)
appendix_3_title <- paste(strwrap(appendix_3_title, width = 90), collapse = "
")

Appendix_3_Figure <- ggplot2::ggplot(sf_ghent_street_segments) +
  ggplot2::geom_sf(ggplot2::aes(fill = observed_status)) +
  ggplot2::scale_fill_manual(
    values = c(
      "At least with one graffiti" = "grey20",
      "Only Others"                = "grey45",
      "Not observed"               = "grey75",
      "Zero graffiti"              = "grey95"
    ),
    name = "Street segment",
    # "At least with one graffiti", "Not observed", "Only Others", "Zero graffiti"
    labels = c(
      base::paste0("With classified graffiti (", count_segments_with_graffiti, ")"),
      base::paste0("Not observed (",             count_not_observed, ")"),
      base::paste0("With only 'Other' graffiti (", count_only_others, ")"),
      base::paste0("With no Graffiti (",         count_zero_graffiti, ")")
    )
  ) +
  ggplot2::labs(title = NULL) +
  custom_theme +
  ggplot2::theme(
    plot.title            = ggplot2::element_text(size = 14, family = "Times New Roman", face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 8)),
    legend.text           = ggplot2::element_text(size = 10, family = "Times New Roman"),
    legend.key.size       = grid::unit(0.5, "lines"),
    legend.position       = c(0.2, 0.2),
    legend.background     = ggplot2::element_rect(fill = "white", colour = "grey80"),
    legend.box.background = ggplot2::element_rect(fill = "white", colour = "grey80"),
    text                  = ggplot2::element_text(family = "Times New Roman", size = 12),
    plot.margin           = ggplot2::margin(t = 14, r = 10, b = 10, l = 10)
  ) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.5)

file_path_Appendix_3_Figure_png <- file.path(tempdir(), "Appendix_3_Figure.png")
ggplot2::ggsave(
  filename  = file_path_Appendix_3_Figure_png,
  device    = "png",
  plot      = Appendix_3_Figure,
  width     = 8,
  height    = 7,
  dpi       = 300,
  units     = "in",
  limitsize = TRUE
)
file_path_Appendix_3_Figure_docx <- file.path(appendix_dir, "Appendix_3_Observation_Status.docx")
save_plot_docx(
  png_path  = file_path_Appendix_3_Figure_png,
  docx_path = file_path_Appendix_3_Figure_docx,
  title     = gsub("
", " ", appendix_3_title),
  explanation = "This map shows which street segments were observed, which were not observed, and which were excluded before the main analysis.",
  width     = 6.5,
  height    = 5.5
)
if (file.exists(file_path_Appendix_3_Figure_png)) invisible(file.remove(file_path_Appendix_3_Figure_png))

# Graffiti type summaries ---------------------------------------------------------
df_graffiti_summary <- df_graffiti_data_clean |>
  dplyr::filter(graffiti_types_grouped != "no graffiti") |>
  janitor::tabyl(graffiti_types_grouped) |>
  janitor::adorn_totals("row") |>
  janitor::adorn_pct_formatting(digits = 1) |>
  dplyr::arrange(n)

# Drop Others and no-graffiti records
df_graffiti_data_final <- df_graffiti_data_clean |>
  dplyr::filter(graffiti_types_grouped != "Others",
                graffiti_types_grouped != "no graffiti")

df_graffiti_summary_no_others <- df_graffiti_data_final |>
  janitor::tabyl(graffiti_types_grouped) |>
  janitor::adorn_totals("row") |>
  janitor::adorn_pct_formatting(digits = 1) |>
  dplyr::arrange(n)

# Table 1 summary
# Fill missing type counts with zero.
# Zero-graffiti and unobserved segments are excluded.
n_observed_focal_segments <- dplyr::n_distinct(df_graffiti_data_final$street_segment)

df_graffiti_summary_stats <- df_graffiti_data_final |>
  dplyr::group_by(street_segment, graffiti_types_grouped) |>
  dplyr::summarize(count = dplyr::n(), .groups = "drop") |>
  tidyr::complete(street_segment, graffiti_types_grouped, fill = list(count = 0)) |>
  dplyr::group_by(graffiti_types_grouped) |>
  dplyr::summarize(
    min        = min(count),
    max        = max(count),
    mean       = round(mean(count), 2),
    sd         = round(sd(count), 2),
    total      = sum(count),
    .groups    = "drop"
  ) |>
  dplyr::mutate(
    percentage = round((total / sum(total)) * 100, 2)
  ) |>
  dplyr::arrange(total)

# Build 3-type count matrix
df_street_segment_counts <- df_graffiti_data_final |>
  dplyr::group_by(street_segment, graffiti_types_grouped) |>
  dplyr::summarize(count = dplyr::n(), .groups = "drop") |>
  tidyr::complete(street_segment, graffiti_types_grouped, fill = list(count = 0))

df_street_segment_counts_wide <- df_street_segment_counts |>
  tidyr::pivot_wider(names_from = "graffiti_types_grouped", values_from = "count")

# Prepare matrix
data_matrix_observed <- as.matrix(df_street_segment_counts_wide[, -1])
rownames(data_matrix_observed) <- df_street_segment_counts_wide$street_segment

# Define comparisons
target_comparisons <- c(
  "Masterpiece vs Tag",
  "Masterpiece vs SITS",
  "Tag vs SITS"
)

transformation_order <- c("No Transformation", "Square Root Transformation",
                           "Log Transformation + 0.5", "Log Transformation + 1")

# Observed MHI values
observed_MHI <- compute_MHI(data_matrix_observed)

df_observed_MHI <- dplyr::bind_rows(
  lapply(names(observed_MHI), function(name) {
    as.data.frame(as.table(observed_MHI[[name]]$matrix)) |>
      dplyr::filter(Var1 != Var2) |>
      dplyr::transmute(
        `Log Transformations` = name,
        `Graffiti Types`      = paste(Var1, "vs", Var2),
        `Observed MHI Value`  = as.numeric(Freq)   # keep exact value
      ) |>
      dplyr::filter(`Graffiti Types` %in% target_comparisons)
  }), .id = NULL) |>
  dplyr::mutate(
    `Log Transformations` = factor(`Log Transformations`, levels = transformation_order),
    `Graffiti Types`      = factor(`Graffiti Types`,      levels = target_comparisons)
  ) |>
  dplyr::arrange(`Log Transformations`, `Graffiti Types`)

# ============================================================
# Main 3-type analysis
# Fixed-Fixed permutation
# Row and column totals are preserved.
# ============================================================

# Cache permutation results
n_iterations_3      <- 1000
permutation_file_3  <- file.path(cache_dir,
  paste0("df_permutated_MHI_3types_FF_", n_iterations_3, "iter.rds"))

permutated_MHI_3 <- run_FF_permutation(
  data_matrix          = data_matrix_observed,
  transformation_order = transformation_order,
  target_pairs         = target_comparisons,
  n_iterations         = n_iterations_3,
  cache_file           = permutation_file_3
)

# Combine MHI results
df_combined <- build_combined_table(df_observed_MHI, permutated_MHI_3, transformation_order, target_comparisons)

# Plot MHI results
MHI_plot_1 <- plot_mhi_guided(
  permutation_data = permutated_MHI_3,
  observed_data    = df_observed_MHI,
  graffiti_type    = "Masterpiece vs Tag",
  x_limits         = c(0.1, 0.32),
  title            = "Masterpiece vs Tags",
  observed_y       = -8
)

MHI_plot_2 <- plot_mhi_guided(
  permutation_data = permutated_MHI_3,
  observed_data    = df_observed_MHI,
  graffiti_type    = "Masterpiece vs SITS",
  x_limits         = c(0.15, 0.39),
  title            = "Masterpiece vs SITSs",
  observed_y       = -5
)

MHI_plot_3 <- plot_mhi_guided(
  permutation_data = permutated_MHI_3,
  observed_data    = df_observed_MHI,
  graffiti_type    = "Tag vs SITS",
  x_limits         = c(0.675, 0.84),
  title            = "Tags vs SITSs",
  observed_y       = -13
)

Figure_1 <-
  MHI_plot_1 + MHI_plot_2 + MHI_plot_3 +
  patchwork::plot_layout(ncol = 1)

Figure_1

ggsave_png(Figure_1, output = folder_name,
           width = 18, dpi = 600, height = 27, units = "cm")

# Observation details
observation_details <- data.frame(
  Label = c(
    "Total Observers",
    "Total Street Segments",
    "Observed Street Segments",
    "Not Observed Street Segments",
    "Zero Graffiti Street Segments",
    "Street Segments with at least One Classified Graffiti Type",
    "Street Segments Only with Other"
  ),
  Count = c(
    unique_observers,
    count_total_segments,
    count_observed_segments,
    count_not_observed,
    count_zero_graffiti,
    count_segments_with_graffiti,
    count_only_others
  )
)

df_segment_geometry_summary <- dplyr::bind_rows(
  summarize_segment_geometry(sf_ghent_street_segments, "All segments"),
  summarize_segment_geometry(
    dplyr::filter(sf_ghent_street_segments, observed_status != "Not observed"),
    "Observed segments"
  ),
  summarize_segment_geometry(
    dplyr::filter(sf_ghent_street_segments, observed_status == "At least with one graffiti"),
    "Analytic focal segments"
  )
) |>
  sf::st_drop_geometry()

# Appendix 4: segment geometry
Appendix_4_caption <- "Appendix 4: Street-segment geometry by sample."
Appendix_4_explanation <- paste0(
  "This table reports length and area for all street segments, observed street segments, and analytic street segments. ",
  "It shows that the analytic street segments are large enough to contain several writable surfaces. ",
  "Lengths are in meters and areas in square meters."
)

Appendix_4_Table <- df_segment_geometry_summary |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 1))) |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::bold(part = "header") |>
  flextable::align(j = 1, align = "left", part = "all") |>
  flextable::autofit() |>
  flextable::font(fontname = "Times New Roman", part = "all")

file_path_Appendix_4_Table_docx <- file.path(appendix_dir, "Appendix_4_Street_Segment_Geometry.docx")
save_flextable_appendix(
  Appendix_4_Table,
  docx_path = file_path_Appendix_4_Table_docx,
  captions = Appendix_4_caption,
  explanations = Appendix_4_explanation
)

# Appendix 5: 3-type MHI table
Appendix_5_caption <- "Appendix 5: Transformation sensitivity for the 3 grouped types."
Appendix_5_explanation <- paste0(
  "This table checks whether the main result changes under alternative count transformations. ",
  "The lower co-presence pattern remains stable across transformations. ",
  "The table reports observed MHI, the Fixed-Fixed null mean and SD from ",
  format(n_iterations_3, big.mark = ",", scientific = FALSE),
  " permutations, and one-tailed p-values under no transformation, square-root transformation, log(x + 0.5), and log(x + 1)."
)
Appendix_5_Table <- make_mhi_flextable(df_combined)

file_path_Appendix_5_Table_docx <- file.path(appendix_dir, "Appendix_5_MHI_3types_FF.docx")
save_flextable_appendix(
  Appendix_5_Table,
  docx_path = file_path_Appendix_5_Table_docx,
  captions = Appendix_5_caption,
  explanations = Appendix_5_explanation
)

# ============================================================
# Appendix 6: spatially constrained check
# ============================================================

sf_segments_focal <- sf_ghent_street_segments |>
  dplyr::filter(UIDN %in% rownames(data_matrix_observed))
sf_segments_focal <- sf_segments_focal[match(rownames(data_matrix_observed), sf_segments_focal$UIDN), ]

n_blocks <- 25
block_id <- assign_spatial_blocks(sf_segments_focal, n_blocks = n_blocks)
df_spatial_blocks <- tibble::tibble(street_segment = sf_segments_focal$UIDN, block_id = block_id)

df_spatial_block_summary_main <- summarize_block_assignment(block_id) |>
  dplyr::mutate(requested_blocks = n_blocks, .before = 1)

n_iterations_spatial <- 1000
permutation_file_spatial <- file.path(
  cache_dir,
  paste0("df_permutated_MHI_3types_spatialFF_", n_iterations_spatial, "iter_", n_blocks, "blocks.rds")
)

permutated_MHI_3_spatial <- run_spatial_FF_permutation(
  data_matrix          = data_matrix_observed,
  block_id             = block_id,
  transformation_order = transformation_order,
  target_pairs         = target_comparisons,
  n_iterations         = n_iterations_spatial,
  cache_file           = permutation_file_spatial
)

df_combined_spatial <- build_combined_table(
  observed_df = df_observed_MHI,
  permuted_df = permutated_MHI_3_spatial,
  transformation_order = transformation_order,
  pairs = target_comparisons
)

Appendix_6A_caption <- "Appendix 6A: Spatially constrained Fixed-Fixed sensitivity for the 3 grouped types."
Appendix_6A_explanation <- paste0(
  "This table checks whether the result changes when permutations are restricted within local spatial blocks. ",
  "The lower co-presence pattern remains stable. ",
  "Permutations are restricted within local spatial blocks while preserving segment totals and global type totals. ",
  "The table reports observed MHI, spatially constrained null mean and SD from ",
  format(n_iterations_spatial, big.mark = ",", scientific = FALSE),
  " permutations, and one-tailed p-values."
)
Appendix_6A_Table <- make_mhi_flextable(df_combined_spatial)

block_sizes_sensitivity <- c(10L, 25L, 50L, 100L)

.block_sensitivity_results <- lapply(block_sizes_sensitivity, function(nb) {
  if (nb == n_blocks) {
    bid <- block_id
    res <- df_combined_spatial
  } else {
    perm_file_nb <- file.path(
      cache_dir,
      paste0("df_permutated_MHI_3types_spatialFF_", n_iterations_spatial, "iter_", nb, "blocks.rds")
    )
    bid <- assign_spatial_blocks(sf_segments_focal, n_blocks = nb)
    perm_nb <- run_spatial_FF_permutation(
      data_matrix          = data_matrix_observed,
      block_id             = bid,
      transformation_order = transformation_order,
      target_pairs         = target_comparisons,
      n_iterations         = n_iterations_spatial,
      cache_file           = perm_file_nb
    )
    res <- build_combined_table(
      observed_df          = df_observed_MHI,
      permuted_df          = perm_nb,
      transformation_order = transformation_order,
      pairs                = target_comparisons
    )
  }

  mhi_rows <- res |>
    dplyr::filter(`Log Transformations` == "Log Transformation + 1") |>
    dplyr::mutate(
      requested_blocks = nb,
      realized_blocks  = length(unique(bid)),
      n_blocks         = nb
    ) |>
    dplyr::select(n_blocks, requested_blocks, realized_blocks,
                  `Graffiti Types`, `Observed MHI Value`, `Mean Permutated MHI`, `P Value`)

  diag_row <- summarize_block_assignment(bid) |>
    dplyr::mutate(requested_blocks = nb, .before = 1)

  list(mhi = mhi_rows, diag = diag_row)
})

df_block_sensitivity <- dplyr::bind_rows(lapply(.block_sensitivity_results, `[[`, "mhi"))
df_block_assignment_diagnostics <- dplyr::bind_rows(lapply(.block_sensitivity_results, `[[`, "diag"))
rm(.block_sensitivity_results)

Appendix_6B_Table <- df_block_sensitivity |>
  dplyr::mutate(
    `Observed MHI Value` = round(`Observed MHI Value`, 3),
    `Mean Permutated MHI` = round(`Mean Permutated MHI`, 3)
  ) |>
  flextable::flextable() |>
  flextable::bold(part = "header") |>
  flextable::align(align = "center", part = "all") |>
  flextable::align(j = "Graffiti Types", align = "left", part = "all") |>
  flextable::autofit() |>
  flextable::font(fontname = "Times New Roman", part = "all")

Appendix_6B_caption <- "Appendix 6B: Block-count sensitivity for the spatially constrained Fixed-Fixed null model."
Appendix_6B_explanation <- paste0(
  "This table checks whether the spatially constrained result depends on the number of blocks. ",
  "Requested grid settings are ",
  paste(block_sizes_sensitivity, collapse = ", "),
  " blocks. Realized occupied blocks may differ because some grid cells contain no analytic street segments."
)

Appendix_6C_Table <- df_block_assignment_diagnostics |>
  flextable::flextable() |>
  flextable::bold(part = "header") |>
  flextable::align(align = "center", part = "all") |>
  flextable::autofit() |>
  flextable::font(fontname = "Times New Roman", part = "all")

Appendix_6C_caption <- "Appendix 6C: Spatial-block assignment diagnostics."
Appendix_6C_explanation <- paste0(
  "This table reports how street segments are allocated across spatial blocks. ",
  "The table reports the requested grid setting, realized occupied blocks, and the distribution of street segments across blocks."
)

Appendix_6_caption <- "Appendix 6: Spatially constrained Fixed-Fixed sensitivity."
Appendix_6_explanation <- paste0(
  "This appendix checks whether the lower co-presence pattern changes when permutations are restricted within local spatial blocks. ",
  "It contains the spatially constrained results, the block-count sensitivity check, and the block-assignment diagnostics."
)

file_path_Appendix_6_docx <- file.path(appendix_dir, "Appendix_6_Spatially_Constrained_FF.docx")
save_flextable_appendix(
  tables = list(Appendix_6A_Table, Appendix_6B_Table, Appendix_6C_Table),
  docx_path = file_path_Appendix_6_docx,
  captions = c(Appendix_6A_caption, Appendix_6B_caption, Appendix_6C_caption),
  explanations = c(Appendix_6A_explanation, Appendix_6B_explanation, Appendix_6C_explanation),
  document_caption = Appendix_6_caption,
  document_explanation = Appendix_6_explanation
)

# ============================================================
# Appendix 7: 6-type sensitivity check
# Same permutation method as Appendix 5.
# ============================================================

df_graffiti_6_final <- df_graffiti_data_clean |>
  dplyr::filter(graffiti_type %in% c("tag", "throw up", "stencil", "illustration", "slogan", "masterpiece"))

six_types <- sort(unique(df_graffiti_6_final$graffiti_type))

df_counts_wide_6 <- df_graffiti_6_final |>
  dplyr::count(street_segment, graffiti_type, name = "count") |>
  tidyr::complete(street_segment, graffiti_type, fill = list(count = 0)) |>
  tidyr::pivot_wider(names_from = graffiti_type, values_from = count) |>
  dplyr::arrange(street_segment)

data_matrix_6 <- as.matrix(df_counts_wide_6[, six_types])
rownames(data_matrix_6) <- df_counts_wide_6$street_segment

transformation_order_6 <- c("No Transformation", "Square Root Transformation",
                             "Log Transformation + 0.5", "Log Transformation + 1")

# All pairwise comparisons
all_pairs <- combn(six_types, 2, FUN = function(x) paste(x[1], "vs", x[2])) |> as.character()
all_pairs <- sort(all_pairs)

# Observed MHI for all pairs
observed_all_6 <- compute_MHI(data_matrix_6)

observed_list_6 <- lapply(transformation_order_6, function(tr_name) {
  mhi_mat <- observed_all_6[[tr_name]]$matrix

  as.data.frame(as.table(mhi_mat)) |>
    dplyr::filter(Var1 != Var2) |>
    dplyr::transmute(
      `Log Transformations` = tr_name,
      `Graffiti Types`      = paste(Var1, "vs", Var2),
      `Observed MHI Value`  = as.numeric(Freq)
    ) |>
    dplyr::filter(`Graffiti Types` %in% all_pairs)
})

df_observed_MHI_6 <- dplyr::bind_rows(observed_list_6) |>
  dplyr::mutate(
    `Log Transformations` = factor(`Log Transformations`, levels = transformation_order_6),
    `Graffiti Types`      = factor(`Graffiti Types`,      levels = all_pairs)
  ) |>
  dplyr::arrange(`Log Transformations`, `Graffiti Types`)

# Fixed-Fixed permutation for 6 types
n_iterations_6      <- 1000
permutation_file_6  <- file.path(cache_dir,
  paste0("df_permutated_MHI_6types_FF_", n_iterations_6, "iter.rds"))

permutated_MHI_6 <- run_FF_permutation(
  data_matrix          = data_matrix_6,
  transformation_order = transformation_order_6,
  target_pairs         = all_pairs,
  n_iterations         = n_iterations_6,
  cache_file           = permutation_file_6
)

# Combine MHI results
df_combined_6 <- build_combined_table(df_observed_MHI_6, permutated_MHI_6, transformation_order_6, all_pairs)

# Appendix 7: 6-type MHI table
Appendix_7_caption <- "Appendix 7: Six-type disaggregation sensitivity."
Appendix_7_explanation <- paste0(
  "This table checks whether grouping the six observed graffiti types into three categories changes the result. ",
  "The overall lower co-presence pattern remains stable, with one partial exception discussed in the manuscript. ",
  "The table repeats the Fixed-Fixed MHI analysis for the original six graffiti types rather than the three grouped categories. ",
  "Each row reports observed MHI, null mean and SD from ",
  format(n_iterations_6, big.mark = ",", scientific = FALSE),
  " permutations, and one-tailed p-values across all transformations."
)
Appendix_7_Table <- make_mhi_flextable(df_combined_6)

file_path_Appendix_7_Table_docx <- file.path(appendix_dir, "Appendix_7_MHI_6types_FF.docx")
save_flextable_appendix(
  Appendix_7_Table,
  docx_path = file_path_Appendix_7_Table_docx,
  captions = Appendix_7_caption,
  explanations = Appendix_7_explanation
)

# Appendix 8: include Other
# Check whether Other changes the result
four_types <- c("Masterpiece", "SITS", "Tag", "Other")

df_street_segment_counts_4 <- df_graffiti_data_clean |>
  dplyr::filter(graffiti_types_grouped != "no graffiti") |>
  dplyr::mutate(
    type4 = dplyr::case_when(
      graffiti_types_grouped == "Masterpiece" ~ "Masterpiece",
      graffiti_types_grouped == "SITS"        ~ "SITS",
      graffiti_types_grouped == "Tag"         ~ "Tag",
      graffiti_types_grouped == "Others"      ~ "Other",
      TRUE                                     ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(type4)) |>
  dplyr::count(street_segment, type4, name = "count") |>
  tidyr::complete(street_segment, type4 = four_types, fill = list(count = 0)) |>
  tidyr::pivot_wider(names_from = type4, values_from = count) |>
  dplyr::arrange(street_segment)

data_matrix_4 <- as.matrix(df_street_segment_counts_4[, four_types])
rownames(data_matrix_4) <- df_street_segment_counts_4$street_segment

all_pairs_4 <- combn(four_types, 2, FUN = function(x) paste(x[1], "vs", x[2])) |>
  as.character() |>
  sort()

observed_MHI_4 <- compute_MHI(data_matrix_4)

df_observed_MHI_4 <- dplyr::bind_rows(
  lapply(transformation_order, function(name) {
    as.data.frame(as.table(observed_MHI_4[[name]]$matrix)) |>
      dplyr::filter(Var1 != Var2) |>
      dplyr::transmute(
        `Log Transformations` = name,
        `Graffiti Types`      = paste(Var1, "vs", Var2),
        `Observed MHI Value`  = as.numeric(Freq)
      ) |>
      dplyr::filter(`Graffiti Types` %in% all_pairs_4)
  }), .id = NULL
) |>
  dplyr::mutate(
    `Log Transformations` = factor(`Log Transformations`, levels = transformation_order),
    `Graffiti Types`      = factor(`Graffiti Types`,      levels = all_pairs_4)
  ) |>
  dplyr::arrange(`Log Transformations`, `Graffiti Types`)

n_iterations_4 <- 1000
permutation_file_4 <- file.path(
  cache_dir,
  paste0("df_permutated_MHI_4types_FF_", n_iterations_4, "iter.rds")
)

permutated_MHI_4 <- run_FF_permutation(
  data_matrix          = data_matrix_4,
  transformation_order = transformation_order,
  target_pairs         = all_pairs_4,
  n_iterations         = n_iterations_4,
  cache_file           = permutation_file_4
)

df_combined_4 <- build_combined_table(
  observed_df          = df_observed_MHI_4,
  permuted_df          = permutated_MHI_4,
  transformation_order = transformation_order,
  pairs                = all_pairs_4
)

Appendix_8_caption <- "Appendix 8: Other-inclusion sensitivity."
Appendix_8_explanation <- paste0(
  "This table checks whether excluding graffiti coded as Other changes the main result. ",
  "The main pairwise comparisons remain lower than expected. ",
  "The table repeats the Fixed-Fixed MHI analysis after retaining graffiti coded as Other as a fourth analytic category. ",
  "Each row reports observed MHI, null mean and SD from ",
  format(n_iterations_4, big.mark = ",", scientific = FALSE),
  " permutations, and one-tailed p-values across all transformations."
)
Appendix_8_Table <- make_mhi_flextable(df_combined_4)

file_path_Appendix_8_Table_docx <- file.path(appendix_dir, "Appendix_8_MHI_4types_FF.docx")
save_flextable_appendix(
  Appendix_8_Table,
  docx_path = file_path_Appendix_8_Table_docx,
  captions = Appendix_8_caption,
  explanations = Appendix_8_explanation
)

# Appendix 9: misclassification check
# Check whether coding error changes the result
# No double coding is available.
# Steps
# Simulate 5%, 10%, and 15% coding error.
# Re-estimate log+1 MHI and FF p-values.
# Summarise p-values and null differences.

irr_scenarios <- c("Adjacent boundary only", "Diffuse with cross-pole bleed")
irr_error_rates <- c(0.05, 0.10, 0.15)
irr_n_reps <- 20
irr_n_perm <- 199
irr_cache_file <- file.path(
  cache_dir,
  paste0("df_irrsens_", irr_n_reps, "rep_", irr_n_perm, "perm.rds")
)

if (file.exists(irr_cache_file)) {
  df_irr_draws <- readRDS(irr_cache_file)
} else {
  set.seed(20260417)
  irr_runs <- vector("list", length = length(irr_scenarios) * length(irr_error_rates) * irr_n_reps)
  run_idx <- 1

  for (sc in irr_scenarios) {
    for (er in irr_error_rates) {
      conf_mat <- make_confusion_matrix(er, sc, colnames(data_matrix_observed))

      for (rep_id in seq_len(irr_n_reps)) {
        perturbed_matrix <- simulate_misclassification(data_matrix_observed, conf_mat)
        res <- ff_pvals_log1(
          data_matrix = perturbed_matrix,
          target_pairs = target_comparisons,
          n_perm = irr_n_perm
        ) |>
          dplyr::mutate(
            Scenario = sc,
            `Error Rate` = er,
            Replicate = rep_id,
            `Deficit (%)` = (`Null Mean MHI` - `Observed MHI Value`) / `Null Mean MHI` * 100
          )
        irr_runs[[run_idx]] <- res
        run_idx <- run_idx + 1
      }
    }
  }

  df_irr_draws <- dplyr::bind_rows(irr_runs)
  saveRDS(df_irr_draws, irr_cache_file)
}

df_irr_summary <- df_irr_draws |>
  dplyr::group_by(Scenario, `Error Rate`, `Graffiti Types`) |>
  dplyr::summarize(
    `Median Observed MHI` = median(`Observed MHI Value`, na.rm = TRUE),
    `Median Null Mean MHI` = median(`Null Mean MHI`, na.rm = TRUE),
    `Median Deficit (%)` = median(`Deficit (%)`, na.rm = TRUE),
    `Min Deficit (%)` = min(`Deficit (%)`, na.rm = TRUE),
    `Max P Value` = max(`P Value`, na.rm = TRUE),
    `Share P < 0.05` = mean(`P Value` < 0.05, na.rm = TRUE),
    `Share P < 0.01` = mean(`P Value` < 0.01, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    `Median Observed MHI` = round(`Median Observed MHI`, 3),
    `Median Null Mean MHI` = round(`Median Null Mean MHI`, 3),
    `Median Deficit (%)` = round(`Median Deficit (%)`, 1),
    `Min Deficit (%)` = round(`Min Deficit (%)`, 1),
    `Max P Value` = round(`Max P Value`, 3),
    `Error Rate` = as.numeric(`Error Rate`)
  ) |>
  dplyr::arrange(Scenario, `Error Rate`, `Graffiti Types`)

# Appendix 9: misclassification summary
df_appendix_9 <- df_irr_summary |>
  dplyr::mutate(
    `Error Rate` = paste0(round(`Error Rate` * 100), "%"),
    `Share P < 0.05` = sprintf("%.2f", `Share P < 0.05`),
    `Share P < 0.01` = sprintf("%.2f", `Share P < 0.01`)
  )

Appendix_9_Table <- df_appendix_9 |>
  flextable::flextable() |>
  flextable::bold(part = "header") |>
  flextable::align(align = "center", part = "all") |>
  flextable::align(j = c(1, 3), align = "left", part = "all") |>
  flextable::fontsize(size = 8, part = "all") |>
  flextable::padding(padding = 1, part = "all") |>
  flextable::autofit() |>
  flextable::font(fontname = "Times New Roman", part = "all") |>
  flextable::fit_to_width(max_width = 10.2)

Appendix_9_caption <- "Appendix 9: Simulated graffiti-type misclassification sensitivity."
Appendix_9_explanation <- paste0(
  "This table checks whether plausible coding error changes the result. ",
  "The main pairwise comparisons remain lower than expected and statistically significant. ",
  "Each row summarizes ",
  format(irr_n_reps, big.mark = ",", scientific = FALSE),
  " re-estimations for a pair, error scenario, and error rate. ",
  "The analysis reassigns a share of graffiti items to other types under adjacent-boundary and diffuse error structures, ",
  "then recomputes log(x + 1) MHI results under the Fixed-Fixed null model."
)

file_path_Appendix_9_Table_docx <- file.path(appendix_dir, "Appendix_9_Misclassification_Sensitivity.docx")
save_flextable_appendix(
  Appendix_9_Table,
  docx_path = file_path_Appendix_9_Table_docx,
  captions = Appendix_9_caption,
  explanations = Appendix_9_explanation
)

# Save all results to Excel
# Workbook sheets
#   1. Observation Details            - segment/observer counts
#   2. Segment Geometry Summary       - mean/median length and area by sample
#   3. Raw Graffiti Summary           - all types incl. Others (pre-exclusion)
#   4. Main Graffiti Summary          - 3 main types only
#   5. Graffiti Summary Stats         - summary per main type
#   6. Observed MHI (3 types)         - raw values by transformation
#   7. MHI Results (3 types, FF)      - main FF test
#   8. MHI Results (3 types, SpatFF)  - spatial FF test
#   9. Spatial Block Assignment       - segment-to-block IDs
#  10. Block Sensitivity              - FF test by block size
#  11. Spatial Block Summary (Main)   - block summary
#  12. Spatial Block Summary (Sens)   - block summary by setting
#  13. MHI Results (6 types, FF)      - 6-type sensitivity check
#  14. MHI Results (4 types, FF)      - Other included
#  15. Misclass Sens Summary          - misclassification summary
#  16. Misclass Sens Draws            - misclassification draws
all_results <- list(
  "Observation Details"            = observation_details,
  "Segment Geometry Summary"       = df_segment_geometry_summary,
  "Raw Graffiti Summary"           = df_graffiti_summary,
  "Main Graffiti Summary"          = df_graffiti_summary_no_others,
  "Graffiti Summary Stats"         = df_graffiti_summary_stats,
  "Observed MHI (3 types)"         = df_observed_MHI |> dplyr::mutate(
    dplyr::across(where(is.factor), as.character),
    `Observed MHI Value` = round(`Observed MHI Value`, 3)
  ),
  "MHI Results (3 types, FF)"      = df_combined |> dplyr::mutate(dplyr::across(where(is.factor), as.character)),
  "MHI Results (3 types, SpatFF)"  = df_combined_spatial |> dplyr::mutate(dplyr::across(where(is.factor), as.character)),
  "Spatial Block Assignment"       = df_spatial_blocks,
  "Block Sensitivity"              = df_block_sensitivity |> dplyr::mutate(dplyr::across(where(is.factor), as.character)),
  "Spatial Block Summary (Main)"   = df_spatial_block_summary_main,
  "Spatial Block Summary (Sens)"   = df_block_assignment_diagnostics,
  "MHI Results (6 types, FF)"      = df_combined_6 |> dplyr::mutate(dplyr::across(where(is.factor), as.character)),
  "MHI Results (4 types, FF)"      = df_combined_4 |> dplyr::mutate(dplyr::across(where(is.factor), as.character)),
  "Misclass Sens Summary"          = df_irr_summary,
  "Misclass Sens Draws"            = df_irr_draws
)
file_path_all_results <- here::here(folder_name, "all_results.xlsx")
writexl::write_xlsx(all_results, file_path_all_results)

manuscript_metadata <- data.frame(
  key = c("data_collection_period", "reported_min_p", "alpha_05"),
  value = c("November 2017", "< 0.001", "< 0.05")
)
write.csv(
  manuscript_metadata,
  here::here(folder_name, "manuscript_metadata.csv"),
  row.names = FALSE
)


