# Helper functions for the AJCJ graffiti analysis.
# Sourced by Scripts/analysis.R; do not run this file separately.

library(patchwork)
library(extrafont)
suppressMessages(loadfonts(device = "win"))

appendix_landscape <- officer::prop_section(
  page_size    = officer::page_size(orient = "landscape"),
  page_margins = officer::page_mar(top = 0.6, bottom = 0.6, left = 0.6, right = 0.6),
  type = "nextPage"
)

appendix_portrait <- officer::prop_section(
  page_size    = officer::page_size(orient = "portrait"),
  page_margins = officer::page_mar(top = 1, bottom = 1, left = 1, right = 1),
  type = "nextPage"
)

appendix_caption_prop <- officer::fp_text(font.family = "Times New Roman", bold = TRUE)
appendix_text_prop    <- officer::fp_text(font.family = "Times New Roman")
appendix_title_prop   <- officer::fp_text(font.family = "Times New Roman", italic = TRUE)
appendix_note_prop    <- officer::fp_text(font.family = "Times New Roman", italic = TRUE)
appendix_heading_prop <- officer::fp_text(
  font.family = "Times New Roman", font.size = 14, bold = TRUE)

appendix_caption_fpar <- function(text)
  officer::fpar(officer::ftext(text, prop = appendix_caption_prop))
appendix_text_fpar <- function(text)
  officer::fpar(officer::ftext(text, prop = appendix_text_prop))
appendix_title_fpar <- function(text)
  officer::fpar(officer::ftext(text, prop = appendix_title_prop))
appendix_note_fpar <- function(text)
  officer::fpar(
    officer::ftext("Note. ", prop = appendix_note_prop),
    officer::ftext(text, prop = appendix_text_prop)
  )
appendix_heading_fpar <- function(text)
  officer::fpar(
    officer::ftext(text, prop = appendix_heading_prop),
    fp_p = officer::fp_par(padding.bottom = 6)
  )

split_table_caption <- function(caption) {
  parts <- strsplit(caption, "|", fixed = TRUE)[[1]]
  if (length(parts) != 2L) {
    stop("Each table caption must use 'Table number|Title'.")
  }
  trimws(parts)
}

apa_no_leading_zero <- function(x, digits = 3L) {
  out <- sprintf(paste0("%.", digits, "f"), as.numeric(x))
  sub("^(-?)0\\.", "\\1.", out)
}

apa_p_value <- function(x, digits = 3L) {
  value <- trimws(as.character(x))
  is_less <- grepl("^<", value)
  numeric_value <- suppressWarnings(as.numeric(gsub("[^0-9.eE+-]", "", value)))
  formatted <- apa_no_leading_zero(numeric_value, digits)
  ifelse(is_less, paste("<", formatted), formatted)
}

apa_pair_label <- function(x) {
  out <- as.character(x)
  out <- gsub("masterpiece", "Masterpiece", out, ignore.case = TRUE)
  out <- gsub("tag", "Tag", out, ignore.case = TRUE)
  out <- gsub("sits", "SITS", out, ignore.case = TRUE)
  out <- gsub("throw up", "Throw-up", out, ignore.case = TRUE)
  out <- gsub("stencil", "Stencil", out, ignore.case = TRUE)
  out <- gsub("illustration", "Illustration", out, ignore.case = TRUE)
  out <- gsub("slogan", "Slogan", out, ignore.case = TRUE)
  out <- gsub("other", "Other", out, ignore.case = TRUE)
  out
}

apa_table_style <- function(ft, left_columns = character(), font_size = 9,
                            max_width = NULL) {
  line <- officer::fp_border(width = 1)
  ft <- ft |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = font_size, part = "all") |>
    flextable::bold(bold = FALSE, part = "header") |>
    flextable::align(align = "center", part = "all") |>
    flextable::border_remove() |>
    flextable::hline_top(border = line, part = "header") |>
    flextable::hline_bottom(border = line, part = "header") |>
    flextable::hline_bottom(border = line, part = "body") |>
    flextable::padding(padding.top = 2, padding.bottom = 2,
                       padding.left = 3, padding.right = 3, part = "all")
  left_columns <- intersect(left_columns, ft$col_keys)
  if (length(left_columns)) {
    ft <- flextable::align(ft, j = left_columns, align = "left", part = "all")
  }
  ft <- flextable::autofit(ft)
  if (!is.null(max_width)) ft <- flextable::fit_to_width(ft, max_width = max_width)
  ft
}

ggsave_png <- function(ggp, output, width = 8, height = 6, dpi = 300, units = "in") {
  if (missing(output)) stop("'output' must be provided.")
  ggplot2::ggsave(
    filename = paste0(deparse(substitute(ggp)), ".png"),
    device = "png", plot = ggp, path = output,
    width = width, height = height, dpi = dpi, units = units, limitsize = TRUE
  )
}

save_plot_docx <- function(png_path, docx_path, title, explanation = NULL,
                           width = 6.5, height = 5) {
  doc <- officer::read_docx()
  doc <- officer::body_add_fpar(doc, value = appendix_caption_fpar(title))
  doc <- officer::body_add_par(doc, value = "", style = "Normal")
  doc <- officer::body_add_img(doc, src = png_path, width = width, height = height)
  doc <- officer::body_add_par(doc, value = "", style = "Normal")
  if (!is.null(explanation) && nzchar(explanation))
    doc <- officer::body_add_fpar(doc, value = appendix_text_fpar(explanation))
  temp <- tempfile(fileext = ".docx")
  print(doc, target = temp)
  ok <- file.copy(temp, docx_path, overwrite = TRUE)
  if (file.exists(temp)) file.remove(temp)
  if (!isTRUE(ok)) stop("Could not write: ", docx_path)
}

save_docx_appendix <- function(source_docx, docx_path, title, explanation = NULL) {
  doc <- officer::read_docx()
  doc <- officer::body_add_fpar(doc, value = appendix_caption_fpar(title))
  doc <- officer::body_add_par(doc, value = "", style = "Normal")
  doc <- officer::body_add_docx(doc, src = source_docx)
  doc <- officer::body_add_par(doc, value = "", style = "Normal")
  if (!is.null(explanation) && nzchar(explanation))
    doc <- officer::body_add_fpar(doc, value = appendix_text_fpar(explanation))
  temp <- tempfile(fileext = ".docx")
  print(doc, target = temp)
  ok <- file.copy(temp, docx_path, overwrite = TRUE)
  if (file.exists(temp)) file.remove(temp)
  if (!isTRUE(ok)) stop("Could not write: ", docx_path)
}

save_existing_docx_with_explanation <- function(source_docx, docx_path,
                                                explanation = NULL) {
  doc <- officer::read_docx(source_docx)
  doc <- officer::cursor_end(doc)
  if (!is.null(explanation) && nzchar(explanation)) {
    doc <- officer::body_add_par(doc, value = "", style = "Normal")
    doc <- officer::body_add_fpar(doc, value = appendix_text_fpar(explanation))
  }
  temp <- tempfile(fileext = ".docx")
  print(doc, target = temp)
  ok <- file.copy(temp, docx_path, overwrite = TRUE)
  if (file.exists(temp)) file.remove(temp)
  if (!isTRUE(ok)) stop("Could not write: ", docx_path)
}

save_flextable_appendix <- function(tables, docx_path, captions,
                                    explanations = NULL,
                                    section = NULL,
                                    table_sections = NULL,
                                    document_caption = NULL,
                                    document_explanation = NULL) {
  if (inherits(tables, "flextable")) tables <- list(tables)
  if (length(tables) != length(captions)) stop("tables and captions must match.")
  if (is.null(explanations)) explanations <- rep(NA_character_, length(tables))
  if (length(explanations) != length(tables)) stop("tables and explanations must match.")
  if (!is.null(section) && !is.null(table_sections))
    stop("Use either section or table_sections, not both.")
  if (!is.null(table_sections) && length(table_sections) != length(tables))
    stop("table_sections must contain one section definition per table.")
  doc <- officer::read_docx()
  if (!is.null(table_sections)) {
    doc <- officer::body_set_default_section(doc, table_sections[[length(table_sections)]])
  } else if (!is.null(section)) {
    doc <- officer::body_set_default_section(doc, section)
  }
  if (!is.null(document_caption) && nzchar(document_caption))
    doc <- officer::body_add_fpar(doc, value = appendix_heading_fpar(document_caption))
  if (!is.null(document_explanation) && nzchar(document_explanation)) {
    doc <- officer::body_add_fpar(doc, value = appendix_text_fpar(document_explanation))
    doc <- officer::body_add_par(doc, value = "", style = "Normal")
  }
  for (i in seq_along(tables)) {
    if (i > 1) doc <- officer::body_add_par(doc, value = "", style = "Normal")
    caption_parts <- split_table_caption(captions[[i]])
    doc <- officer::body_add_fpar(doc, value = appendix_text_fpar(caption_parts[[1]]))
    doc <- officer::body_add_fpar(doc, value = appendix_title_fpar(caption_parts[[2]]))
    doc <- flextable::body_add_flextable(doc, value = tables[[i]])
    if (!is.na(explanations[[i]]) && nzchar(explanations[[i]]))
      doc <- officer::body_add_fpar(doc, value = appendix_note_fpar(explanations[[i]]))
    if (!is.null(table_sections) && i < length(tables) &&
        !identical(table_sections[[i]], table_sections[[i + 1L]])) {
      doc <- officer::body_end_block_section(
        doc, officer::block_section(table_sections[[i]]))
    }
  }
  temp <- tempfile(fileext = ".docx")
  print(doc, target = temp)
  ok <- file.copy(temp, docx_path, overwrite = TRUE)
  if (file.exists(temp)) file.remove(temp)
  if (!isTRUE(ok)) stop("Could not write: ", docx_path)
}

custom_theme <- ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    axis.text  = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  )

plot_mhi <- function(permutation_data, observed_data, graffiti_type, x_limits,
                     seg_linewidth = 0.5, text_size = 1.8, observed_y = -5,
                     title = "") {
  perm <- permutation_data |>
    dplyr::filter(`Log Transformations` == "Log Transformation + 1",
                  `Graffiti Types` == graffiti_type) |>
    dplyr::pull(`Permutated MHI Value`)
  obs <- observed_data |>
    dplyr::filter(`Log Transformations` == "Log Transformation + 1",
                  `Graffiti Types` == graffiti_type) |>
    dplyr::pull(`Observed MHI Value`)
  pm  <- mean(perm)
  bw  <- 0.005
  br  <- seq(min(perm) - bw, max(perm) + bw, by = bw)
  my  <- max(hist(perm, plot = FALSE, breaks = br)$counts) * 1.1
  ggplot2::ggplot(data.frame(perm), ggplot2::aes(x = perm)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(count)),
                            fill = "grey", color = "black", binwidth = bw) +
    ggplot2::annotate("segment", x = obs, xend = obs, y = 0, yend = my,
                      color = "black", linetype = "dashed", linewidth = seg_linewidth) +
    ggplot2::annotate("text", x = obs, y = observed_y,
                      label = paste("Observed MHI:", round(obs, 3)),
                      color = "black", hjust = 0.3, size = text_size,
                      family = "Times New Roman", fontface = "bold") +
    ggplot2::annotate("text", x = pm, y = observed_y,
                      label = sprintf("Permutated Mean MHI: %.3f", pm),
                      color = "black", hjust = 0.5, size = text_size,
                      family = "Times New Roman", fontface = "bold") +
    ggplot2::annotate("point", x = pm, y = 0, color = "black", size = 0.7) +
    ggplot2::coord_cartesian(xlim = x_limits, clip = "off") +
    ggplot2::labs(title = title, x = "MHI Value", y = "Frequency") +
    ggplot2::theme(
      legend.position = "none",
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      text       = ggplot2::element_text(family = "Times New Roman", size = 7),
      axis.title = ggplot2::element_text(size = 7, family = "Times New Roman", face = "bold"),
      axis.text  = ggplot2::element_text(size = 7, family = "Times New Roman"),
      axis.line  = ggplot2::element_line(color = "black"),
      plot.title = ggplot2::element_text(family = "Times New Roman", size = 7, face = "bold")
    )
}

plot_mhi_guided <- function(permutation_data, observed_data, graffiti_type,
                            x_limits, title = "", observed_y = -5,
                            guide_text_size = 2.3, guide_linewidth = 0.35, ...) {
  perm   <- permutation_data |>
    dplyr::filter(`Log Transformations` == "Log Transformation + 1",
                  `Graffiti Types` == graffiti_type) |>
    dplyr::pull(`Permutated MHI Value`)
  bw <- 0.005; br <- seq(min(perm) - bw, max(perm) + bw, by = bw)
  my <- max(hist(perm, plot = FALSE, breaks = br)$counts) * 1.1
  xr <- diff(x_limits)
  gy <- my * 0.88; ly <- my * 0.93
  lx0 <- x_limits[1] + 0.14 * xr; lx1 <- x_limits[1] + 0.27 * xr
  rx0 <- x_limits[1] + 0.79 * xr; rx1 <- x_limits[1] + 0.91 * xr
  plot_mhi(permutation_data, observed_data, graffiti_type, x_limits,
           title = title, observed_y = observed_y, ...) +
    ggplot2::annotate("segment", x = lx1, xend = lx0, y = gy, yend = gy,
                      arrow = grid::arrow(length = grid::unit(0.10, "cm"), type = "closed"),
                      linewidth = guide_linewidth, color = "black") +
    ggplot2::annotate("segment", x = rx0, xend = rx1, y = gy, yend = gy,
                      arrow = grid::arrow(length = grid::unit(0.10, "cm"), type = "closed"),
                      linewidth = guide_linewidth, color = "black") +
    ggplot2::annotate("text", x = (lx0 + lx1) / 2, y = ly,
                      label = "Lower co-presence", size = guide_text_size,
                      family = "Times New Roman", fontface = "bold", color = "black") +
    ggplot2::annotate("text", x = (rx0 + rx1) / 2, y = ly,
                      label = "Higher co-presence", size = guide_text_size,
                      family = "Times New Roman", fontface = "bold", color = "black")
}

compute_MHI <- function(data_matrix) {
  log05 <- log(data_matrix + 0.5); log05 <- log05 - min(log05)
  trs <- list(
    "No Transformation"          = data_matrix,
    "Square Root Transformation" = sqrt(data_matrix),
    "Log Transformation + 0.5"   = log05,
    "Log Transformation + 1"     = log1p(data_matrix)
  )
  purrr::map(trs, ~ list(matrix = 1 - as.matrix(vegan::vegdist(t(.x), method = "horn"))))
}

calculate_p_value <- function(sim, obs)
  sum(sim <= obs) / length(sim)

permute_matrix <- function(dm) {
  types <- colnames(dm); ss <- rowSums(dm)
  pool  <- rep(types, times = colSums(dm))
  shuf  <- sample(pool, length(pool), replace = FALSE)
  idx   <- rep(seq_along(ss), times = ss)
  segs  <- split(shuf, idx)
  pm    <- matrix(0L, nrow = length(ss), ncol = length(types), dimnames = list(NULL, types))
  for (i in seq_along(segs))
    pm[i, ] <- as.integer(table(factor(segs[[i]], levels = types)))
  pm
}

run_FF_permutation <- function(dm, transformation_order, target_pairs,
                               n_iterations, seed, cache_file,
                               force_recompute = FALSE) {
  if (!force_recompute && file.exists(cache_file)) {
    message("Cache loaded: ", cache_file); return(readRDS(cache_file))
  }
  set.seed(seed)
  res <- vector("list", n_iterations * length(transformation_order)); k <- 1
  for (i in seq_len(n_iterations)) {
    pm <- permute_matrix(dm)
    for (tr in transformation_order) {
      mhi <- compute_MHI(pm)[[tr]]$matrix
      res[[k]] <- as.data.frame(as.table(mhi)) |>
        dplyr::filter(Var1 != Var2) |>
        dplyr::transmute(`Log Transformations` = tr,
                         `Graffiti Types`       = paste(Var1, "vs", Var2),
                         `Permutated MHI Value` = as.numeric(Freq)) |>
        dplyr::filter(`Graffiti Types` %in% target_pairs)
      k <- k + 1
    }
  }
  out <- dplyr::bind_rows(res) |>
    dplyr::mutate(`Log Transformations` = factor(`Log Transformations`, levels = transformation_order),
                  `Graffiti Types`      = factor(`Graffiti Types`,      levels = target_pairs)) |>
    dplyr::arrange(`Log Transformations`, `Graffiti Types`)
  saveRDS(out, cache_file); message("Cache saved: ", cache_file); out
}

build_combined_table <- function(observed_df, permuted_df, transformation_order, pairs) {
  permuted_df |>
    dplyr::group_by(`Log Transformations`, `Graffiti Types`) |>
    dplyr::summarise(`Mean Permutated MHI` = mean(`Permutated MHI Value`, na.rm = TRUE),
                     `SD Permutated MHI`   = sd(`Permutated MHI Value`,   na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::left_join(observed_df, by = c("Log Transformations", "Graffiti Types")) |>
    dplyr::mutate(
      `P Value` = purrr::map2_chr(
        as.character(`Graffiti Types`), as.character(`Log Transformations`),
        function(pair, tr) {
          sim <- permuted_df |> dplyr::filter(`Graffiti Types` == pair, `Log Transformations` == tr) |> dplyr::pull(`Permutated MHI Value`)
          obs <- observed_df |> dplyr::filter(`Graffiti Types` == pair, `Log Transformations` == tr) |> dplyr::pull(`Observed MHI Value`)
          p   <- calculate_p_value(sim, obs)
          if (p == 0) "< 0.001" else as.character(p)
        }),
      `Mean Permutated MHI` = as.numeric(`Mean Permutated MHI`),
      `SD Permutated MHI`   = as.numeric(`SD Permutated MHI`),
      `Observed MHI Value`  = as.numeric(`Observed MHI Value`)
    ) |>
    dplyr::select(`Log Transformations`, `Graffiti Types`,
                  `Observed MHI Value`, `Mean Permutated MHI`, `SD Permutated MHI`, `P Value`) |>
    dplyr::mutate(`Log Transformations` = factor(`Log Transformations`, levels = transformation_order),
                  `Graffiti Types`      = factor(`Graffiti Types`,      levels = pairs)) |>
    dplyr::arrange(`Log Transformations`, `Graffiti Types`)
}

format_mhi_table <- function(df) {
  df |>
    dplyr::mutate(dplyr::across(where(is.factor), as.character)) |>
    dplyr::transmute(
      Transformation = dplyr::recode(
        as.character(`Log Transformations`),
        "No Transformation" = "None",
        "Square Root Transformation" = "Square root",
        "Log Transformation + 0.5" = "Log(x + 0.5)",
        "Log Transformation + 1" = "Log(x + 1)",
        .default = as.character(`Log Transformations`)
      ),
      Pair = apa_pair_label(`Graffiti Types`),
      `Observed MHI` = apa_no_leading_zero(`Observed MHI Value`),
      `Mean null MHI` = apa_no_leading_zero(`Mean Permutated MHI`),
      `SD null MHI` = apa_no_leading_zero(`SD Permutated MHI`),
      p = apa_p_value(`P Value`)
    )
}

make_mhi_flextable <- function(df) {
  display <- format_mhi_table(df)
  ft <- flextable::flextable(display)
  ft <- apa_table_style(ft, left_columns = c("Transformation", "Pair"), font_size = 9)
  flextable::italic(ft, j = "p", part = "header")
}

assign_spatial_blocks <- function(sf_obj, n_blocks = 25) {
  coords  <- sf::st_coordinates(sf::st_centroid(sf_obj))
  sq      <- ceiling(sqrt(n_blocks))
  xb      <- seq(min(coords[,1]), max(coords[,1]), length.out = sq + 1)
  yb      <- seq(min(coords[,2]), max(coords[,2]), length.out = sq + 1)
  xbin    <- findInterval(coords[,1], xb, rightmost.closed = TRUE)
  ybin    <- findInterval(coords[,2], yb, rightmost.closed = TRUE)
  as.integer(factor(paste(xbin, ybin, sep = "_")))
}

run_spatial_FF_permutation <- function(dm, block_id, transformation_order,
                                       target_pairs, n_iterations, seed,
                                       cache_file, force_recompute = FALSE) {
  if (!force_recompute && file.exists(cache_file)) {
    message("Spatial cache loaded: ", cache_file); return(readRDS(cache_file))
  }
  set.seed(seed)
  res <- vector("list", n_iterations * length(transformation_order)); k <- 1
  for (i in seq_len(n_iterations)) {
    pm <- matrix(0L, nrow(dm), ncol(dm)); colnames(pm) <- colnames(dm)
    for (b in unique(block_id)) { idx <- which(block_id == b); pm[idx,] <- permute_matrix(dm[idx,,drop=FALSE]) }
    for (tr in transformation_order) {
      mhi <- compute_MHI(pm)[[tr]]$matrix
      res[[k]] <- as.data.frame(as.table(mhi)) |>
        dplyr::filter(Var1 != Var2) |>
        dplyr::transmute(`Log Transformations` = tr,
                         `Graffiti Types`       = paste(Var1, "vs", Var2),
                         `Permutated MHI Value` = as.numeric(Freq)) |>
        dplyr::filter(`Graffiti Types` %in% target_pairs)
      k <- k + 1
    }
  }
  out <- dplyr::bind_rows(res) |>
    dplyr::mutate(`Log Transformations` = factor(`Log Transformations`, levels = transformation_order),
                  `Graffiti Types`      = factor(`Graffiti Types`,      levels = target_pairs)) |>
    dplyr::arrange(`Log Transformations`, `Graffiti Types`)
  saveRDS(out, cache_file); message("Spatial cache saved: ", cache_file); out
}

summarize_block_assignment <- function(block_id) {
  sz <- table(block_id)
  data.frame(realized_blocks = length(sz), min_seg = min(sz),
             median_seg = median(sz), max_seg = max(sz))
}

summarize_segment_geometry <- function(sf_obj, label) {
  if (!"LENGTE" %in% names(sf_obj))
    stop("The street-segment length attribute LENGTE is missing.")
  sf_obj |>
    dplyr::mutate(length_m = as.numeric(LENGTE),
                   area_m2  = as.numeric(sf::st_area(geometry))) |>
    sf::st_drop_geometry() |>
    dplyr::summarise(
      Sample           = label, N = dplyr::n(),
      `Mean Length (m)` = mean(length_m, na.rm=TRUE),
      `SD Length (m)`   = sd(length_m,   na.rm=TRUE),
      `Mean Area (m2)`  = mean(area_m2,  na.rm=TRUE),
      `SD Area (m2)`    = sd(area_m2,    na.rm=TRUE)
    )
}

make_confusion_matrix <- function(error_rate, scenario, types) {
  n <- length(types)
  m <- matrix(0, n, n, dimnames = list(types, types)); diag(m) <- 1 - error_rate
  if (scenario == "Adjacent boundary only") {
    if (!setequal(types, c("Masterpiece", "SITS", "Tag")))
      stop("The adjacent-boundary scenario requires Masterpiece, SITS, and Tag.")
    m["Masterpiece", "SITS"] <- error_rate
    m["SITS", "Masterpiece"] <- error_rate / 2
    m["SITS", "Tag"] <- error_rate / 2
    m["Tag", "SITS"] <- error_rate
  } else {
    od <- which(row(m) != col(m), arr.ind = TRUE)
    for (i in seq_len(nrow(od))) m[od[i,1], od[i,2]] <- error_rate / (n - 1)
  }
  if (any(abs(rowSums(m) - 1) > 1e-12))
    stop("Misclassification transition probabilities must sum to one by original type.")
  m
}

simulate_misclassification <- function(dm, conf_mat) {
  types <- colnames(dm)
  t(apply(dm, 1, function(row) {
    out <- setNames(integer(length(types)), types)
    for (tp in types) if (row[tp] > 0) {
      r <- sample(types, row[tp], replace = TRUE, prob = conf_mat[tp,])
      for (x in r) out[x] <- out[x] + 1L
    }
    out
  }))
}

ff_pvals_log1 <- function(dm, target_pairs, n_perm) {
  log1_mhi <- function(counts) {
    transformed <- log1p(counts)
    proportions <- sweep(transformed, 2L, colSums(transformed), "/")
    products <- crossprod(proportions)
    2 * products / outer(diag(products), diag(products), "+")
  }
  obs_mhi <- log1_mhi(dm)
  pair_types <- strsplit(target_pairs, " vs ", fixed = TRUE)
  pair_index <- lapply(pair_types, match, table = colnames(dm))
  if (any(vapply(pair_index, function(x) anyNA(x), logical(1))))
    stop("A requested graffiti-type pair is absent from the count matrix.")
  pair_values <- function(mhi) vapply(pair_index,
    function(index) mhi[index[[1]], index[[2]]], numeric(1))
  observed <- pair_values(obs_mhi)
  permuted <- matrix(NA_real_, nrow = n_perm, ncol = length(target_pairs))
  type_pool <- rep(seq_len(ncol(dm)), times = colSums(dm))
  segment_index <- rep(seq_len(nrow(dm)), times = rowSums(dm))
  for (i in seq_len(n_perm)) {
    shuffled <- sample(type_pool, length(type_pool), replace = FALSE)
    pm <- matrix(tabulate((segment_index - 1L) * ncol(dm) + shuffled,
                          nbins = length(dm)), nrow = nrow(dm), byrow = TRUE,
                 dimnames = dimnames(dm))
    permuted[i, ] <- pair_values(log1_mhi(pm))
  }
  data.frame(`Graffiti Types` = target_pairs,
    `Null Mean MHI` = colMeans(permuted),
    `P Value` = (1 + colSums(sweep(permuted, 2L, observed, "<="))) /
      (n_perm + 1),
    `Observed MHI Value` = observed, check.names = FALSE)
}
# Figure 3: graffiti distribution across street segments.

create_graffiti_distribution_map <- function(
    df_graffiti_data_clean,
    sf_ghent_street_segments,
    output_dir,
    png_filename = "Fig3_Graffiti_Distribution.png",
    word_png_filename = NULL,
    width = 6.85,
    height = 7.04,
    dpi = 600,
    word_dpi = 300) {
  required_packages <- c("dplyr", "ggplot2", "sf")
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_packages) > 0L) {
    stop(
      "Install the required package(s) before creating Figure 3: ",
      paste(missing_packages, collapse = ", ")
    )
  }

  required_item_columns <- c("street_segment", "graffiti_types_grouped")
  missing_item_columns <- setdiff(
    required_item_columns,
    names(df_graffiti_data_clean)
  )
  if (length(missing_item_columns) > 0L) {
    stop(
      "df_graffiti_data_clean is missing required column(s): ",
      paste(missing_item_columns, collapse = ", ")
    )
  }
  if (!inherits(sf_ghent_street_segments, "sf")) {
    stop("sf_ghent_street_segments must be an sf object.")
  }
  if (!"UIDN" %in% names(sf_ghent_street_segments)) {
    stop("sf_ghent_street_segments is missing the UIDN identifier.")
  }

  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  graffiti_map_items <- df_graffiti_data_clean |>
    dplyr::transmute(
      street_segment = trimws(as.character(.data$street_segment)),
      graffiti_group = trimws(as.character(.data$graffiti_types_grouped))
    ) |>
    dplyr::mutate(
      graffiti_group = dplyr::recode(
        .data$graffiti_group,
        "No graffiti" = "no graffiti"
      )
    ) |>
    dplyr::filter(
      !is.na(.data$street_segment),
      nzchar(.data$street_segment)
    )

  segment_flags <- graffiti_map_items |>
    dplyr::group_by(.data$street_segment) |>
    dplyr::summarise(
      observed = TRUE,
      analytic = any(.data$graffiti_group %in% c("Tag", "SITS", "Masterpiece")),
      tag_present = any(.data$graffiti_group == "Tag"),
      sits_present = any(.data$graffiti_group == "SITS"),
      masterpiece_present = any(.data$graffiti_group == "Masterpiece"),
      .groups = "drop"
    )

  segments_map <- sf_ghent_street_segments |>
    dplyr::transmute(
      UIDN = trimws(as.character(.data$UIDN)),
      geometry = .data$geometry
    )

  if (anyDuplicated(segments_map$UIDN)) {
    duplicate_ids <- unique(segments_map$UIDN[duplicated(segments_map$UIDN)])
    stop(
      "Duplicate street-segment IDs in sf_ghent_street_segments: ",
      paste(utils::head(duplicate_ids, 10L), collapse = ", ")
    )
  }
  if (any(!sf::st_is_valid(segments_map))) {
    segments_map <- sf::st_make_valid(segments_map)
  }

  segments_map <- segments_map |>
    dplyr::left_join(segment_flags, by = c("UIDN" = "street_segment")) |>
    dplyr::mutate(
      dplyr::across(
        c(observed, analytic, tag_present, sits_present, masterpiece_present),
        ~ dplyr::coalesce(.x, FALSE)
      )
    )

  unmatched_observed_ids <- setdiff(segment_flags$street_segment, segments_map$UIDN)
  if (length(unmatched_observed_ids) > 0L) {
    warning(
      length(unmatched_observed_ids),
      " observed street-segment ID(s) have no matching map geometry."
    )
  }

  analytic_n <- sum(segment_flags$analytic)
  tag_n <- sum(segment_flags$tag_present)
  sits_n <- sum(segment_flags$sits_present)
  masterpiece_n <- sum(segment_flags$masterpiece_present)
  mapped_n <- nrow(segments_map)
  observed_n <- sum(segment_flags$observed)
  tag_items_n <- sum(graffiti_map_items$graffiti_group == "Tag", na.rm = TRUE)
  sits_items_n <- sum(graffiti_map_items$graffiti_group == "SITS", na.rm = TRUE)
  masterpiece_items_n <- sum(
    graffiti_map_items$graffiti_group == "Masterpiece",
    na.rm = TRUE
  )

  if (analytic_n == 0L) {
    stop("No analytic street segments were identified; Figure 3 cannot be created.")
  }

  format_percent <- function(x) {
    sub("\\.0$", "", sprintf("%.1f", x))
  }

  panel_labels <- c(
    sprintf(
      paste0(
        "A. Observation coverage\n",
        "Mapped segments: n = %s\n",
        "Observed: n = %s | Analytic: n = %s"
      ),
      format(mapped_n, big.mark = ","),
      format(observed_n, big.mark = ","),
      format(analytic_n, big.mark = ",")
    ),
    sprintf(
      "B. Tags\nItems: n = %s\nStreet segments: n = %s (%s percent)",
      format(tag_items_n, big.mark = ","),
      format(tag_n, big.mark = ","),
      format_percent(100 * tag_n / analytic_n)
    ),
    sprintf(
      "C. SITS\nItems: n = %s\nStreet segments: n = %s (%s percent)",
      format(sits_items_n, big.mark = ","),
      format(sits_n, big.mark = ","),
      format_percent(100 * sits_n / analytic_n)
    ),
    sprintf(
      "D. Masterpieces\nItems: n = %s\nStreet segments: n = %s (%s percent)",
      format(masterpiece_items_n, big.mark = ","),
      format(masterpiece_n, big.mark = ","),
      format_percent(100 * masterpiece_n / analytic_n)
    )
  )

  coverage_panel <- segments_map |>
    dplyr::mutate(
      panel = panel_labels[1],
      map_status = dplyr::case_when(
        !.data$observed ~ "Not observed",
        .data$analytic ~ "Analytic sample",
        TRUE ~ "Observed, excluded"
      )
    )

  make_presence_panel <- function(data, panel_label, present_column, present_label) {
    data |>
      dplyr::mutate(
        panel = panel_label,
        map_status = dplyr::case_when(
          !.data$observed ~ "Not observed",
          !.data$analytic ~ "Observed, excluded",
          .data[[present_column]] ~ present_label,
          TRUE ~ "Focal type absent"
        )
      )
  }

  tag_panel <- make_presence_panel(
    segments_map, panel_labels[2], "tag_present", "Tag present"
  )
  sits_panel <- make_presence_panel(
    segments_map, panel_labels[3], "sits_present", "SITS present"
  )
  masterpiece_panel <- make_presence_panel(
    segments_map,
    panel_labels[4],
    "masterpiece_present",
    "Masterpiece present"
  )

  status_levels <- c(
    "Not observed",
    "Observed, excluded",
    "Analytic sample",
    "Focal type absent",
    "Tag present",
    "SITS present",
    "Masterpiece present"
  )

  map_plot_data <- dplyr::bind_rows(
    coverage_panel,
    tag_panel,
    sits_panel,
    masterpiece_panel
  ) |>
    dplyr::mutate(
      panel = factor(.data$panel, levels = panel_labels),
      map_status = factor(.data$map_status, levels = status_levels)
    )

  map_bbox <- sf::st_bbox(segments_map)
  map_width <- as.numeric(map_bbox["xmax"] - map_bbox["xmin"])
  map_height <- as.numeric(map_bbox["ymax"] - map_bbox["ymin"])
  scale_length <- if (map_width >= 5000) {
    1000
  } else if (map_width >= 2500) {
    500
  } else {
    250
  }

  navigation_data <- data.frame(
    panel = factor(panel_labels[1], levels = panel_labels),
    scale_x0 = as.numeric(map_bbox["xmin"] + 0.07 * map_width),
    scale_x1 = as.numeric(map_bbox["xmin"] + 0.07 * map_width + scale_length),
    scale_y = as.numeric(map_bbox["ymin"] + 0.07 * map_height),
    scale_label_x = as.numeric(map_bbox["xmin"] + 0.07 * map_width + scale_length / 2),
    scale_label_y = as.numeric(map_bbox["ymin"] + 0.105 * map_height),
    north_x = as.numeric(map_bbox["xmax"] - 0.08 * map_width),
    north_y0 = as.numeric(map_bbox["ymax"] - 0.19 * map_height),
    north_y1 = as.numeric(map_bbox["ymax"] - 0.09 * map_height),
    north_label_y = as.numeric(map_bbox["ymax"] - 0.055 * map_height)
  )

  fill_values <- c(
    "Not observed" = "#E6CF6A",
    "Observed, excluded" = "#8ECAE6",
    "Analytic sample" = "#333333",
    "Focal type absent" = "#B8B8B8",
    "Tag present" = "#D55E00",
    "SITS present" = "#009E73",
    "Masterpiece present" = "#CC79A7"
  )
  present_outline_values <- c(
    "Tag present" = "#D55E00",
    "SITS present" = "#009E73",
    "Masterpiece present" = "#CC79A7"
  )
  present_linewidth_values <- c(
    "Tag present" = 0.13,
    "SITS present" = 0.13,
    "Masterpiece present" = 0.32
  )
  present_plot_data <- map_plot_data |>
    dplyr::filter(.data$map_status %in% names(present_outline_values))

  main_text_map <- ggplot2::ggplot(map_plot_data) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data$map_status),
      colour = "#FFFFFF",
      linewidth = 0.045
    ) +
    ggplot2::geom_sf(
      data = present_plot_data,
      ggplot2::aes(
        fill = .data$map_status,
        colour = .data$map_status,
        linewidth = .data$map_status
      ),
      show.legend = FALSE
    ) +
    ggplot2::geom_segment(
      data = navigation_data,
      ggplot2::aes(
        x = .data$scale_x0,
        xend = .data$scale_x1,
        y = .data$scale_y,
        yend = .data$scale_y
      ),
      inherit.aes = FALSE,
      colour = "#111111",
      linewidth = 0.8,
      lineend = "butt"
    ) +
    ggplot2::geom_segment(
      data = navigation_data,
      ggplot2::aes(
        x = .data$scale_x0,
        xend = .data$scale_x0,
        y = .data$scale_y - 0.01 * map_height,
        yend = .data$scale_y + 0.01 * map_height
      ),
      inherit.aes = FALSE,
      colour = "#111111",
      linewidth = 0.6
    ) +
    ggplot2::geom_segment(
      data = navigation_data,
      ggplot2::aes(
        x = .data$scale_x1,
        xend = .data$scale_x1,
        y = .data$scale_y - 0.01 * map_height,
        yend = .data$scale_y + 0.01 * map_height
      ),
      inherit.aes = FALSE,
      colour = "#111111",
      linewidth = 0.6
    ) +
    ggplot2::geom_text(
      data = navigation_data,
      ggplot2::aes(
        x = .data$scale_label_x,
        y = .data$scale_label_y,
        label = paste0(format(scale_length, big.mark = ","), " m")
      ),
      inherit.aes = FALSE,
      family = "Times New Roman",
      size = 3.0,
      colour = "#111111"
    ) +
    ggplot2::geom_segment(
      data = navigation_data,
      ggplot2::aes(
        x = .data$north_x,
        xend = .data$north_x,
        y = .data$north_y0,
        yend = .data$north_y1
      ),
      inherit.aes = FALSE,
      colour = "#111111",
      linewidth = 0.6,
      arrow = grid::arrow(length = grid::unit(2.2, "mm"), type = "closed")
    ) +
    ggplot2::geom_text(
      data = navigation_data,
      ggplot2::aes(
        x = .data$north_x,
        y = .data$north_label_y,
        label = "N"
      ),
      inherit.aes = FALSE,
      family = "Times New Roman",
      fontface = "bold",
      size = 3.2,
      colour = "#111111"
    ) +
    ggplot2::facet_wrap(~panel, ncol = 2) +
    ggplot2::scale_fill_manual(
      values = fill_values,
      drop = FALSE,
      name = "Street-segment status"
    ) +
    ggplot2::scale_colour_manual(values = present_outline_values, guide = "none") +
    ggplot2::scale_linewidth_manual(values = present_linewidth_values, guide = "none") +
    ggplot2::coord_sf(datum = NA, expand = FALSE, clip = "on") +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        ncol = 4,
        byrow = TRUE,
        override.aes = list(colour = "#777777", linewidth = 0.2)
      )
    ) +
    ggplot2::theme_void(base_family = "Times New Roman") +
    ggplot2::theme(
      strip.text = ggplot2::element_text(
        face = "bold",
        size = 9.0,
        lineheight = 0.95,
        margin = ggplot2::margin(b = 3)
      ),
      strip.background = ggplot2::element_blank(),
      panel.spacing = grid::unit(4.5, "mm"),
      panel.border = ggplot2::element_rect(
        colour = "#8A8A8A",
        fill = NA,
        linewidth = 0.35
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold", size = 9),
      legend.text = ggplot2::element_text(size = 8.2),
      legend.key.size = grid::unit(4.2, "mm"),
      legend.spacing.x = grid::unit(1.5, "mm"),
      legend.box.spacing = grid::unit(1.5, "mm"),
      plot.margin = ggplot2::margin(4, 4, 3, 4, unit = "mm")
    )

  png_path <- file.path(output_dir, png_filename)
  word_png_path <- NULL
  png_device <- if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png
  } else {
    "png"
  }

  ggplot2::ggsave(
    filename = png_path,
    plot = main_text_map,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    device = png_device,
    bg = "white"
  )
  if (!is.null(word_png_filename) && nzchar(word_png_filename)) {
    word_png_path <- file.path(output_dir, word_png_filename)
    ggplot2::ggsave(
      filename = word_png_path,
      plot = main_text_map,
      width = width,
      height = height,
      units = "in",
      dpi = word_dpi,
      device = png_device,
      bg = "white"
    )
  }
  counts <- data.frame(
    measure = c(
      "mapped_segments",
      "observed_segments",
      "analytic_segments",
      "tag_items",
      "tag_segments",
      "sits_items",
      "sits_segments",
      "masterpiece_items",
      "masterpiece_segments",
      "unmatched_observed_ids"
    ),
    n = c(
      mapped_n,
      observed_n,
      analytic_n,
      tag_items_n,
      tag_n,
      sits_items_n,
      sits_n,
      masterpiece_items_n,
      masterpiece_n,
      length(unmatched_observed_ids)
    )
  )
  utils::write.csv(
    counts,
    file.path(output_dir, "Fig3_Graffiti_Distribution_counts.csv"),
    row.names = FALSE
  )

  message("Created Figure 3: ", png_path)
  if (!is.null(word_png_path)) {
    message("Created Figure 3 Word image: ", word_png_path)
  }
  message(
    "Analytic segments: ", analytic_n,
    "; tags present: ", tag_n,
    "; SITS present: ", sits_n,
    "; masterpieces present: ", masterpiece_n
  )

  invisible(list(
    plot = main_text_map,
    counts = counts,
    png_path = png_path,
    word_png_path = word_png_path,
    unmatched_observed_ids = unmatched_observed_ids
  ))
}

# Rebuild the four manuscript figures from project source assets, map data, and
# saved permutation results. No statistical simulation is run here.
build_manuscript_figures <- function(project_root, figures_dir) {
  if (!requireNamespace("ragg", quietly = TRUE)) stop("The ragg package is required for Figure 3.")
  figure_source_dir <- file.path(project_root, "Data", "Source_files")
  data_file <- file.path(project_root, "Data", "Graffiti", "df_graffiti_items_anonymized.csv")
  cache_dir <- file.path(project_root, "Output", "cache")
  analysis_file <- file.path(cache_dir, "analysis_results.rds")
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
  required <- c(data_file,
    file.path(figure_source_dir, c("graffiti_type_examples.png",
                                   "relative_visibility.png")),
    file.path(project_root, "Data", "Shapefiles", c("WBN3.shp", "BRUGGEN 2.shp")),
    analysis_file)
  if (!all(file.exists(required))) stop("Missing figure input: ", paste(required[!file.exists(required)], collapse = ", "))
  results <- readRDS(analysis_file)
  main_permutations <- results$analysis_settings$main_permutations
  main_permutation_file <- file.path(
    cache_dir,
    paste0("df_permutated_MHI_3types_FF_", main_permutations, "iter.rds"))
  if (!file.exists(main_permutation_file)) {
    stop("Missing Figure 4 permutation input: ", main_permutation_file)
  }

  # Remove obsolete generated names so Output/figures contains one file per figure.
  legacy_files <- file.path(figures_dir, c(
    "Figure_1_Graffiti_Type_Examples.png",
    "Figure_2_Relative_Visibility.png",
    "Figure_3_Graffiti_Distribution_counts.csv",
    "Figure_4_Observed_vs_Permuted_MHI.png"
  ))
  legacy_files <- legacy_files[file.exists(legacy_files)]
  if (length(legacy_files) && !all(file.remove(legacy_files))) {
    stop("Could not remove obsolete generated figure files.")
  }

  # Figures 1 and 2 use the final source composites supplied with the project.
  figure_sources <- file.path(figure_source_dir, c(
    "graffiti_type_examples.png",
    "relative_visibility.png"
  ))
  figure_targets <- file.path(figures_dir, c(
    "Fig1_Graffiti_Type_Examples.png",
    "Fig2_Relative_Visibility.png"
  ))
  copied <- file.copy(figure_sources, figure_targets, overwrite = TRUE)
  if (!all(copied)) {
    stop("Could not copy the source assets for Figures 1 and 2.")
  }

  # Figure 3: redraw the type-presence map from the item records and geometry.
  items <- utils::read.csv(data_file, na.strings = character(0), stringsAsFactors = FALSE)
  segments <- dplyr::bind_rows(
    sf::st_read(file.path(project_root, "Data", "Shapefiles", "WBN3.shp"), quiet = TRUE),
    sf::st_read(file.path(project_root, "Data", "Shapefiles", "BRUGGEN 2.shp"), quiet = TRUE))
  map <- create_graffiti_distribution_map(items, segments, figures_dir,
    png_filename = "Fig3_Graffiti_Distribution.png",
    word_png_filename = NULL,
    width = 6.85,
    height = 7.04,
    dpi = 600)
  dm <- results$data_matrix_observed
  expected <- c(analytic_segments = nrow(dm),
                tag_items = sum(dm[, "Tag"]), tag_segments = sum(dm[, "Tag"] > 0),
                sits_items = sum(dm[, "SITS"]), sits_segments = sum(dm[, "SITS"] > 0),
                masterpiece_items = sum(dm[, "Masterpiece"]),
                masterpiece_segments = sum(dm[, "Masterpiece"] > 0))
  actual <- stats::setNames(map$counts$n, map$counts$measure)
  if (!identical(as.integer(actual[names(expected)]), as.integer(expected)))
    stop("Figure 3 counts differ from the saved analytic matrix.")

  # Figure 4: redraw the MHI panels from the saved observed and permutation results.
  permuted <- readRDS(main_permutation_file)
  observed <- as.data.frame(results[["Observed MHI (3 types)"]], check.names = FALSE)
  panels_mhi <- list(
    plot_mhi_guided(permuted, observed, "Masterpiece vs Tag", c(0.10, 0.32), "Masterpiece vs Tags", observed_y = -8),
    plot_mhi_guided(permuted, observed, "Masterpiece vs SITS", c(0.15, 0.39), "Masterpiece vs SITS", observed_y = -5),
    plot_mhi_guided(permuted, observed, "Tag vs SITS", c(0.675, 0.84), "Tags vs SITS", observed_y = -13))
  mhi_figure <- patchwork::wrap_plots(panels_mhi, ncol = 1)
  ggplot2::ggsave(file.path(figures_dir, "Fig4_Observed_vs_Permuted_MHI.png"),
    plot = mhi_figure, width = 17.4, height = 23.4, units = "cm", dpi = 600, bg = "white")
  invisible(map$counts)
}

# Combine the generated appendices in manuscript order. Appendices 1 and 2 are
# supplied as PDFs; temporary page images are created only for the Word file.
combine_appendices <- function(appendix_dir) {
  required <- c(
    sprintf("Appendix_%02d_%s.docx", 3:12, c(
      "segment_types", "observation_status", "segment_geometry",
      "transformation_sensitivity", "spatial_sensitivity", "six_type_sensitivity",
      "other_category_sensitivity", "misclassification_sensitivity", "overpainting",
      "no_masterpiece_segment_robustness"
    )),
    "Appendix_01_observation_form.pdf",
    "Appendix_02_training_material.pdf"
  )
  missing <- required[!file.exists(file.path(appendix_dir, required))]
  if (length(missing)) {
    stop("Missing appendix output(s): ", paste(missing, collapse = ", "))
  }

  combined_path <- file.path(appendix_dir, "AJCJ_combined_appendices.docx")
  doc <- officer::read_docx()
  doc <- officer::body_set_default_section(doc, appendix_portrait)
  doc <- officer::body_add_fpar(doc, appendix_heading_fpar("AJCJ Appendices"))
  doc <- officer::body_end_block_section(
    doc, officer::block_section(appendix_portrait))
  docx_files <- file.path(appendix_dir, required[grepl("\\.docx$", required)])
  appendix_end_sections <- list(
    appendix_portrait, appendix_portrait, appendix_portrait, appendix_portrait,
    appendix_landscape, appendix_portrait, appendix_landscape, appendix_portrait,
    appendix_portrait, appendix_landscape, appendix_portrait, appendix_portrait
  )

  for (i in seq_len(12L)) {
    if (i %in% c(1L, 2L)) {
      pdf_path <- file.path(
        appendix_dir,
        if (i == 1L) "Appendix_01_observation_form.pdf"
        else "Appendix_02_training_material.pdf")
      if (i == 2L) {
        doc <- officer::body_add_fpar(
          doc, appendix_heading_fpar("Appendix 2: Observer Training Material"))
      }
      if (!requireNamespace("pdftools", quietly = TRUE)) {
        stop("Package 'pdftools' is required to build the combined appendices.")
      }
      page_dir <- tempfile(sprintf("appendix%02d_pages_", i))
      dir.create(page_dir)
      on.exit(unlink(page_dir, recursive = TRUE, force = TRUE), add = TRUE)
      page_count <- pdftools::pdf_info(pdf_path)$pages
      pages <- file.path(
        page_dir, sprintf("appendix%02d_page_%02d.png", i, seq_len(page_count)))
      suppressMessages(pdftools::pdf_convert(
        pdf_path, format = "png", dpi = 180, filenames = pages))
      for (j in seq_along(pages)) {
        doc <- officer::body_add_img(
          doc, src = pages[[j]], width = 5.94, height = 8.4)
        if (j < length(pages)) doc <- officer::body_add_break(doc)
      }
    } else {
      src <- docx_files[grepl(sprintf("Appendix_%02d_", i), basename(docx_files))]
      if (length(src) != 1L) stop("Cannot identify Appendix ", i, ".")
      doc <- officer::body_add_docx(doc, src = src)
    }
    if (i < 12L) {
      doc <- officer::body_end_block_section(
        doc, officer::block_section(appendix_end_sections[[i]]))
    }
  }

  temp <- tempfile(fileext = ".docx")
  print(doc, target = temp)
  ok <- file.copy(temp, combined_path, overwrite = TRUE)
  if (file.exists(temp)) file.remove(temp)
  if (!isTRUE(ok)) stop("Could not write: ", combined_path)
  invisible(combined_path)
}
