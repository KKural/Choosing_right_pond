# Helper functions for graffiti analysis
# Required packages are loaded in the main script.
# Creates the output folder.

library(patchwork)
library(extrafont)
suppressMessages(loadfonts(device = "win"))

# Create dated output folder
make_folder <- function(date = Sys.Date(), suffix = "_script_choosing_right_pond_outputs") {
  folder_name      <- format(as.Date(date), "%Y%m%d")
  full_folder_name <- paste0(folder_name, suffix)
  if (!dir.exists(here::here(full_folder_name))) {
    dir.create(here::here(full_folder_name))
    message("Folder created: ", full_folder_name)
  } else {
    message("Folder already exists: ", full_folder_name)
  }
  return(full_folder_name)
}

# Landscape page layout for wide appendix tables
appendix_landscape <- officer::prop_section(
  page_size = officer::page_size(orient = "landscape"),
  page_margins = officer::page_mar(top = 0.6, bottom = 0.6, left = 0.6, right = 0.6)
)

appendix_caption_prop <- officer::fp_text(font.family = "Times New Roman", bold = TRUE)
appendix_text_prop <- officer::fp_text(font.family = "Times New Roman")

appendix_caption_fpar <- function(text) {
  officer::fpar(officer::ftext(text, prop = appendix_caption_prop))
}

appendix_text_fpar <- function(text) {
  officer::fpar(officer::ftext(text, prop = appendix_text_prop))
}

# Save ggplot as PNG
ggsave_png <- function(ggp, output, width = 8, height = 6, dpi = 300, units = "in") {
  if (missing(output)) {
    stop("'output' must be provided (pass the dated folder path, e.g. folder_name).")
  }
  ggplot2::ggsave(
    filename = paste0(deparse(substitute(ggp)), ".png", sep = ""),
    device   = "png",
    plot     = ggp,
    path     = output,
    width    = width,
    height   = height,
    dpi      = dpi,
    units    = units,
    limitsize = TRUE
  )
}

# Save a plot in a Word appendix file.
save_plot_docx <- function(png_path, docx_path, title, explanation = NULL, width = 6.5, height = 5) {
  doc <- officer::read_docx()
  doc <- officer::body_add_fpar(doc, value = appendix_caption_fpar(title))
  doc <- officer::body_add_par(doc, value = "", style = "Normal")
  doc <- officer::body_add_img(doc, src = png_path, width = width, height = height)
  doc <- officer::body_add_par(doc, value = "", style = "Normal")
  if (!is.null(explanation) && nzchar(explanation)) {
    doc <- officer::body_add_fpar(doc, value = appendix_text_fpar(explanation))
  }
  temp_docx <- tempfile(fileext = ".docx")
  print(doc, target = temp_docx)
  copied <- file.copy(temp_docx, docx_path, overwrite = TRUE)
  if (file.exists(temp_docx)) file.remove(temp_docx)
  if (!isTRUE(copied)) {
    stop("Could not write appendix figure file. Please close the Word document: ", docx_path)
  }
}

# Add appendix title and explanation before an existing Word file
save_docx_appendix <- function(source_docx, docx_path, title, explanation = NULL) {
  doc <- officer::read_docx()
  doc <- officer::body_add_fpar(doc, value = appendix_caption_fpar(title))
  doc <- officer::body_add_par(doc, value = "", style = "Normal")
  doc <- officer::body_add_docx(doc, src = source_docx)
  doc <- officer::body_add_par(doc, value = "", style = "Normal")
  if (!is.null(explanation) && nzchar(explanation)) {
    doc <- officer::body_add_fpar(doc, value = appendix_text_fpar(explanation))
  }
  temp_docx <- tempfile(fileext = ".docx")
  print(doc, target = temp_docx)
  copied <- file.copy(temp_docx, docx_path, overwrite = TRUE)
  if (file.exists(temp_docx)) file.remove(temp_docx)
  if (!isTRUE(copied)) {
    stop("Could not write appendix file. Please close the Word document: ", docx_path)
  }
}

# Save an existing Word appendix and add explanation at the end.
save_existing_docx_with_explanation <- function(source_docx, docx_path, explanation = NULL) {
  doc <- officer::read_docx(source_docx)
  doc <- officer::cursor_end(doc)
  if (!is.null(explanation) && nzchar(explanation)) {
    doc <- officer::body_add_par(doc, value = "", style = "Normal")
    doc <- officer::body_add_fpar(doc, value = appendix_text_fpar(explanation))
  }
  temp_docx <- tempfile(fileext = ".docx")
  print(doc, target = temp_docx)
  copied <- file.copy(temp_docx, docx_path, overwrite = TRUE)
  if (file.exists(temp_docx)) file.remove(temp_docx)
  if (!isTRUE(copied)) {
    stop("Could not write appendix file. Please close the Word document: ", docx_path)
  }
}

# Save appendix table in Word.
# Order: caption, table, explanation.
save_flextable_appendix <- function(tables, docx_path, captions, explanations = NULL,
                                   section = appendix_landscape,
                                   document_caption = NULL,
                                   document_explanation = NULL) {
  if (inherits(tables, "flextable")) {
    tables <- list(tables)
  }
  if (length(tables) != length(captions)) {
    stop("The number of tables and captions must match.")
  }
  if (is.null(explanations)) {
    explanations <- rep(NA_character_, length(tables))
  }
  if (length(explanations) != length(tables)) {
    stop("The number of tables and explanations must match.")
  }

  doc <- officer::read_docx()
  if (!is.null(section)) {
    doc <- officer::body_set_default_section(doc, section)
  }

  if (!is.null(document_caption) && nzchar(document_caption)) {
    doc <- officer::body_add_fpar(doc, value = appendix_caption_fpar(document_caption))
  }
  if (!is.null(document_explanation) && nzchar(document_explanation)) {
    doc <- officer::body_add_fpar(doc, value = appendix_text_fpar(document_explanation))
    doc <- officer::body_add_par(doc, value = "", style = "Normal")
  }

  for (i in seq_along(tables)) {
    if (i > 1) {
      doc <- officer::body_add_par(doc, value = "", style = "Normal")
    }
    doc <- officer::body_add_fpar(doc, value = appendix_caption_fpar(captions[[i]]))
    doc <- officer::body_add_par(doc, value = "", style = "Normal")
    doc <- flextable::body_add_flextable(doc, value = tables[[i]])
    doc <- officer::body_add_par(doc, value = "", style = "Normal")
    if (!is.na(explanations[[i]]) && nzchar(explanations[[i]])) {
      doc <- officer::body_add_fpar(doc, value = appendix_text_fpar(explanations[[i]]))
    }
  }

  temp_docx <- tempfile(fileext = ".docx")
  print(doc, target = temp_docx)
  copied <- file.copy(temp_docx, docx_path, overwrite = TRUE)
  if (file.exists(temp_docx)) file.remove(temp_docx)
  if (!isTRUE(copied)) {
    stop("Could not write appendix table file. Please close the Word document: ", docx_path)
  }
}

# Replace fixed text in a Word file
rename_docx_text <- function(docx_path, old_text, new_text) {
  doc <- officer::read_docx(docx_path)
  doc <- officer::body_replace_all_text(
    doc,
    old_value = old_text,
    new_value = new_text,
    only_at_cursor = FALSE
  )
  print(doc, target = docx_path)
}

# Map theme
custom_theme <- ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    axis.text       = ggplot2::element_blank(),
    axis.ticks      = ggplot2::element_blank()
  )

# Plot permuted and observed MHI.
plot_mhi <- function(permutation_data, observed_data,
                     graffiti_type,
                     x_limits,
                     seg_linewidth = 0.5,
                     text_size = 1.8,
                     observed_y = -5,
                     title = "") {

  permutation <- permutation_data |>
    dplyr::filter(`Log Transformations` == "Log Transformation + 1",
                  `Graffiti Types` == graffiti_type) |>
    dplyr::pull(`Permutated MHI Value`)

  observed_MHI <- observed_data |>
    dplyr::filter(`Log Transformations` == "Log Transformation + 1",
                  `Graffiti Types` == graffiti_type) |>
    dplyr::pull(`Observed MHI Value`)

  permutated_mean <- mean(permutation)

  bin_width <- 0.005
  breaks <- seq(min(permutation) - bin_width,
                max(permutation) + bin_width,
                by = bin_width)
  max_y <- max(hist(permutation, plot = FALSE, breaks = breaks)$counts) * 1.1

  p <- ggplot2::ggplot(data.frame(permutation), ggplot2::aes(x = permutation)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(count)),
      fill = "grey", color = "black", binwidth = bin_width
    ) +
    ggplot2::annotate(
      "segment",
      x = observed_MHI, xend = observed_MHI,
      y = 0, yend = max_y,
      color = "black", linetype = "dashed", linewidth = seg_linewidth
    ) +
    ggplot2::annotate(
      "text",
      x = observed_MHI, y = observed_y,
      label = paste("Observed MHI:", round(observed_MHI, 3)),
      color = "black", hjust = 0.3, size = text_size,
      family = "Times New Roman", fontface = "bold"
    ) +
    ggplot2::annotate(
      "text",
      x = permutated_mean, y = observed_y,
      label = sprintf("Permutated Mean MHI: %.3f", permutated_mean),
      color = "black", hjust = 0.5, size = text_size,
      family = "Times New Roman", fontface = "bold"
    ) +
    ggplot2::annotate(
      "point", x = permutated_mean, y = 0, color = "black", size = 0.7
    ) +
    ggplot2::xlim(x_limits[1], x_limits[2]) +
    ggplot2::labs(title = title, x = "MHI Value", y = "Frequency") +
    ggplot2::theme(
      legend.position = "none",
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      text = ggplot2::element_text(family = "Times New Roman", size = 7),
      axis.title = ggplot2::element_text(size = 7, family = "Times New Roman", face = "bold"),
      axis.text = ggplot2::element_text(size = 7, family = "Times New Roman"),
      axis.line = ggplot2::element_line(color = "black"),
      plot.title = ggplot2::element_text(family = "Times New Roman", size = 7, face = "bold")
    )

  return(p)
}

# Add the interpretation guide to the MHI plot.
plot_mhi_guided <- function(permutation_data, observed_data,
                            graffiti_type,
                            x_limits,
                            title = "",
                            observed_y = -5,
                            guide_text_size = 2.3,
                            guide_linewidth = 0.35,
                            ...) {
  permutation <- permutation_data |>
    dplyr::filter(`Log Transformations` == "Log Transformation + 1",
                  `Graffiti Types` == graffiti_type) |>
    dplyr::pull(`Permutated MHI Value`)

  bin_width <- 0.005
  breaks <- seq(min(permutation) - bin_width,
                max(permutation) + bin_width,
                by = bin_width)
  max_y <- max(hist(permutation, plot = FALSE, breaks = breaks)$counts) * 1.1
  x_range <- diff(x_limits)

  guide_y <- max_y * 0.88
  label_y <- max_y * 0.93

  left_arrow_x    <- x_limits[1] + 0.14 * x_range
  left_arrow_xend <- x_limits[1] + 0.27 * x_range
  left_text_x     <- (left_arrow_x + left_arrow_xend) / 2

  right_arrow_x    <- x_limits[1] + 0.79 * x_range
  right_arrow_xend <- x_limits[1] + 0.91 * x_range
  right_text_x     <- (right_arrow_x + right_arrow_xend) / 2

  plot_mhi(
    permutation_data = permutation_data,
    observed_data = observed_data,
    graffiti_type = graffiti_type,
    x_limits = x_limits,
    title = title,
    observed_y = observed_y,
    ...
  ) +
    ggplot2::annotate(
      "segment",
      x = left_arrow_xend,
      xend = left_arrow_x,
      y = guide_y,
      yend = guide_y,
      arrow = grid::arrow(length = grid::unit(0.10, "cm"), type = "closed"),
      linewidth = guide_linewidth,
      color = "black"
    ) +
    ggplot2::annotate(
      "segment",
      x = right_arrow_x,
      xend = right_arrow_xend,
      y = guide_y,
      yend = guide_y,
      arrow = grid::arrow(length = grid::unit(0.10, "cm"), type = "closed"),
      linewidth = guide_linewidth,
      color = "black"
    ) +
    ggplot2::annotate(
      "text",
      x = left_text_x,
      y = label_y,
      label = "Lower co-presence",
      size = guide_text_size,
      family = "Times New Roman",
      fontface = "bold",
      color = "black"
    ) +
    ggplot2::annotate(
      "text",
      x = right_text_x,
      y = label_y,
      label = "Higher co-presence",
      size = guide_text_size,
      family = "Times New Roman",
      fontface = "bold",
      color = "black"
    ) +
    ggplot2::coord_cartesian(clip = "off")
}

# Compute MHI matrices
# Input: count matrix
# Output: MHI matrices
compute_MHI <- function(data_matrix) {
  log05_raw <- log(data_matrix + 0.5)
  log05     <- log05_raw - min(log05_raw)

  transformations <- list(
    "No Transformation"          = data_matrix,
    "Square Root Transformation" = sqrt(data_matrix),
    "Log Transformation + 0.5"   = log05,
    "Log Transformation + 1"     = log1p(data_matrix)
  )

  MHI_matrices <- purrr::map(transformations, ~ {
    MHI_matrix <- 1 - as.matrix(vegan::vegdist(t(.x), method = "horn"))
    list(matrix = MHI_matrix)   # keep exact values
  })

  return(MHI_matrices)
}

# One-tailed p-value
calculate_p_value <- function(simulated_data, observed_value) {
  proportion <- sum(simulated_data <= observed_value) / length(simulated_data)
  return(round(proportion, 3))
}

# Generate Fixed-Fixed permutation
permute_matrix <- function(data_matrix) {
  types         <- colnames(data_matrix)
  segment_sizes <- rowSums(data_matrix)
  type_pool     <- rep(types, times = colSums(data_matrix))

  shuffled   <- sample(type_pool, length(type_pool), replace = FALSE)
  split_idx  <- rep(seq_along(segment_sizes), times = segment_sizes)
  seg_labels <- split(shuffled, split_idx)

  perm_mat <- matrix(0, nrow = length(segment_sizes), ncol = length(types))
  colnames(perm_mat) <- types
  for (i in seq_along(seg_labels)) {
    perm_mat[i, ] <- as.integer(table(factor(seg_labels[[i]], levels = types)))
  }
  perm_mat
}

# Run Fixed-Fixed permutations
run_FF_permutation <- function(data_matrix, transformation_order, target_pairs,
                               n_iterations = 1000, cache_file,
                               force_recompute = FALSE) {
  if (!force_recompute && file.exists(cache_file)) {
    result <- readRDS(cache_file)
    message("Permutation loaded from ", cache_file)
    return(result)
  }

  set.seed(1234)
  results <- vector("list", length = n_iterations * length(transformation_order))
  k <- 1

  for (iter in seq_len(n_iterations)) {
    perm_mat <- permute_matrix(data_matrix)

    for (tr_name in transformation_order) {
      mhi_perm <- compute_MHI(perm_mat)[[tr_name]]$matrix

      df_perm <- as.data.frame(as.table(mhi_perm)) |>
        dplyr::filter(Var1 != Var2) |>
        dplyr::transmute(
          `Log Transformations`  = tr_name,
          `Graffiti Types`       = paste(Var1, "vs", Var2),
          `Permutated MHI Value` = as.numeric(Freq)
        ) |>
        dplyr::filter(`Graffiti Types` %in% target_pairs)

      results[[k]] <- df_perm
      k <- k + 1
    }
  }

  result <- dplyr::bind_rows(results) |>
    dplyr::mutate(
      `Log Transformations` = factor(`Log Transformations`, levels = transformation_order),
      `Graffiti Types`      = factor(`Graffiti Types`,      levels = target_pairs)
    )

  saveRDS(result, cache_file)
  message("Permutation completed and saved to ", cache_file)
  result
}

# Combine MHI values
build_combined_table <- function(observed_df, permuted_df, transformation_order, pairs) {
  observed_df |>
    dplyr::left_join(permuted_df, by = c("Log Transformations", "Graffiti Types")) |>
    dplyr::group_by(`Log Transformations`, `Graffiti Types`) |>
    dplyr::summarize(
      `Observed MHI Value`  = dplyr::first(`Observed MHI Value`),
      `Mean Permutated MHI` = round(mean(`Permutated MHI Value`, na.rm = TRUE), 3),
      `SD Permutated MHI`   = round(sd(`Permutated MHI Value`,   na.rm = TRUE), 3),
      `P Value`             = calculate_p_value(`Permutated MHI Value`, dplyr::first(`Observed MHI Value`)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      `Observed MHI Value` = round(`Observed MHI Value`, 3),
      `P Value` = dplyr::case_when(
        is.na(`P Value`) ~ "N/A",
        `P Value` == 0   ~ "< 0.001",
        TRUE             ~ as.character(`P Value`)
      ),
      `Log Transformations` = factor(`Log Transformations`, levels = transformation_order),
      `Graffiti Types`      = factor(`Graffiti Types`,      levels = pairs)
    ) |>
    dplyr::arrange(`Log Transformations`, `Graffiti Types`)
}

# Format MHI table
make_mhi_flextable <- function(df, caption = NULL) {
  ft <- df |>
    flextable::flextable() |>
    flextable::bold(part = "header") |>
    flextable::align(j = c(1, 2), align = "left",   part = "all") |>
    flextable::align(j = 3:6,     align = "center", part = "all") |>
    flextable::merge_v(j = 1) |>
    flextable::autofit() |>
    flextable::font(fontname = "Times New Roman", part = "all")
  if (!is.null(caption) && nzchar(caption)) {
    ft <- flextable::set_caption(ft, caption = caption)
  }
  ft
}

# Summarise street-segment geometry.
summarize_segment_geometry <- function(df, sample_label) {
  dplyr::summarise(
    dplyr::mutate(df, .sample = sample_label),
    Sample = dplyr::first(.sample),
    N = dplyr::n(),
    `Mean Length (m)` = mean(LENGTE, na.rm = TRUE),
    `Median Length (m)` = median(LENGTE, na.rm = TRUE),
    `Mean Area (m2)` = mean(OPPERVL, na.rm = TRUE),
    `Median Area (m2)` = median(OPPERVL, na.rm = TRUE)
  )
}

# Reassign graffiti types under a misclassification scenario.
simulate_misclassification <- function(data_matrix, confusion_matrix) {
  stopifnot(all(dim(confusion_matrix) == c(ncol(data_matrix), ncol(data_matrix))))
  stopifnot(all(rownames(confusion_matrix) == colnames(data_matrix)))
  stopifnot(all(colnames(confusion_matrix) == colnames(data_matrix)))

  out <- matrix(0L, nrow = nrow(data_matrix), ncol = ncol(data_matrix))
  colnames(out) <- colnames(data_matrix)
  rownames(out) <- rownames(data_matrix)

  for (r in seq_len(nrow(data_matrix))) {
    for (k in seq_len(ncol(data_matrix))) {
      n_items <- data_matrix[r, k]
      if (n_items <= 0) next
      out[r, ] <- out[r, ] + as.vector(stats::rmultinom(1, n_items, confusion_matrix[k, ]))
    }
  }
  out
}

# Compute log(x + 1) Horn similarity.
compute_horn_log1 <- function(data_matrix) {
  1 - as.matrix(vegan::vegdist(t(log1p(data_matrix)), method = "horn"))
}

# Recompute log(x + 1) MHI p-values.
ff_pvals_log1 <- function(data_matrix, target_pairs, n_perm = 199) {
  pair_split <- strsplit(target_pairs, " vs ", fixed = TRUE)
  obs_mat <- compute_horn_log1(data_matrix)
  obs_vals <- vapply(pair_split, function(p) obs_mat[p[1], p[2]], numeric(1))

  perm_vals <- matrix(NA_real_, nrow = n_perm, ncol = length(target_pairs))
  for (iter in seq_len(n_perm)) {
    perm_mat <- permute_matrix(data_matrix)
    horn_perm <- compute_horn_log1(perm_mat)
    perm_vals[iter, ] <- vapply(pair_split, function(p) horn_perm[p[1], p[2]], numeric(1))
  }

  p_vals <- (colSums(perm_vals <= matrix(obs_vals, nrow = n_perm, ncol = length(obs_vals), byrow = TRUE)) + 1) / (n_perm + 1)

  tibble::tibble(
    `Graffiti Types` = target_pairs,
    `Observed MHI Value` = obs_vals,
    `Null Mean MHI` = colMeans(perm_vals),
    `P Value` = p_vals
  )
}

# Build a misclassification matrix.
make_confusion_matrix <- function(error_rate, scenario, types) {
  if (scenario == "Adjacent boundary only") {
    mat <- matrix(c(
      1 - error_rate, error_rate,      0,
      error_rate / 2, 1 - error_rate, error_rate / 2,
      0,              error_rate,      1 - error_rate
    ), nrow = 3, byrow = TRUE)
  } else if (scenario == "Diffuse with cross-pole bleed") {
    mat <- matrix(c(
      1 - error_rate, 0.85 * error_rate, 0.15 * error_rate,
      0.50 * error_rate, 1 - error_rate, 0.50 * error_rate,
      0.15 * error_rate, 0.85 * error_rate, 1 - error_rate
    ), nrow = 3, byrow = TRUE)
  } else {
    stop("Unknown scenario: ", scenario)
  }

  rownames(mat) <- types
  colnames(mat) <- types
  mat
}

# Assign spatial blocks
assign_spatial_blocks <- function(sf_obj, n_blocks = 25) {
  n_side <- ceiling(sqrt(n_blocks))
  grid <- sf::st_make_grid(sf_obj, n = c(n_side, n_side), what = "polygons")
  centroids <- sf::st_point_on_surface(sf_obj)
  as.integer(sf::st_nearest_feature(centroids, grid))
}

# Summarise block assignment
# requested_blocks is set by caller.
# realised_blocks counts occupied blocks.
summarize_block_assignment <- function(block_id) {
  tab <- table(block_id)
  tibble::tibble(
    requested_blocks  = NA_integer_,   # fill in caller
    realized_blocks   = as.integer(length(tab)),
    min_block_size    = as.integer(min(tab)),
    median_block_size = as.numeric(stats::median(tab)),
    max_block_size    = as.integer(max(tab)),
    singleton_blocks  = as.integer(sum(tab == 1L)),
    blocks_le_5       = as.integer(sum(tab <= 5L))
  )
}

# Block-constrained Fixed-Fixed permutation
# Row and column totals are preserved within blocks.
permute_matrix_block_ff <- function(data_matrix, block_id) {
  types <- colnames(data_matrix)
  out <- data_matrix

  for (b in unique(block_id)) {
    idx <- which(block_id == b)
    if (length(idx) <= 1) next

    sub <- data_matrix[idx, , drop = FALSE]
    segment_sizes <- rowSums(sub)
    if (sum(segment_sizes) <= 1) next

    type_pool <- rep(types, times = colSums(sub))
    if (length(type_pool) <= 1) next

    shuffled <- sample(type_pool, length(type_pool), replace = FALSE)
    split_idx <- rep(seq_along(segment_sizes), times = segment_sizes)
    seg_labels <- split(shuffled, split_idx)

    sub_perm <- matrix(0, nrow = length(idx), ncol = length(types))
    colnames(sub_perm) <- types
    for (i in seq_along(seg_labels)) {
      sub_perm[i, ] <- tabulate(match(seg_labels[[i]], types), nbins = length(types))
    }

    out[idx, ] <- sub_perm
  }

  out
}

# Run block-constrained permutations
run_spatial_FF_permutation <- function(data_matrix, block_id, transformation_order, target_pairs,
                                       n_iterations = 1000, cache_file,
                                       force_recompute = FALSE) {
  if (!force_recompute && file.exists(cache_file)) {
    result <- readRDS(cache_file)
    message("Spatial FF permutation loaded from ", cache_file)
    return(result)
  }

  set.seed(1234)
  results <- vector("list", length = n_iterations * length(transformation_order))
  k <- 1

  for (iter in seq_len(n_iterations)) {
    perm_mat <- permute_matrix_block_ff(data_matrix, block_id)

    for (tr_name in transformation_order) {
      mhi_perm <- compute_MHI(perm_mat)[[tr_name]]$matrix

      df_perm <- as.data.frame(as.table(mhi_perm)) |>
        dplyr::filter(Var1 != Var2) |>
        dplyr::transmute(
          `Log Transformations`  = tr_name,
          `Graffiti Types`       = paste(Var1, "vs", Var2),
          `Permutated MHI Value` = as.numeric(Freq)
        ) |>
        dplyr::filter(`Graffiti Types` %in% target_pairs)

      results[[k]] <- df_perm
      k <- k + 1
    }
  }

  result <- dplyr::bind_rows(results) |>
    dplyr::mutate(
      `Log Transformations` = factor(`Log Transformations`, levels = transformation_order),
      `Graffiti Types`      = factor(`Graffiti Types`,      levels = target_pairs)
    )

  saveRDS(result, cache_file)
  message("Spatial FF permutation completed and saved to ", cache_file)
  result
}
