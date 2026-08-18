#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)
  library(dplyr)
})

# Self-contained bootstrap (mirrors resolve_code_root() in 0-setup.R, which
# isn't sourced yet at this point) — locates this repo (BDBV2026-Processing_Code)
# by walking up from this script's own location and checking for pipelines/,
# since this repo has no data/ directory of its own.
resolve_repo_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  script_dir <- if (length(file_arg) > 0) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE))
  } else {
    normalizePath(getwd(), mustWork = FALSE)
  }

  from_script <- normalizePath(file.path(script_dir, "..", ".."), mustWork = FALSE)
  if (dir.exists(file.path(from_script, "pipelines"))) {
    return(from_script)
  }

  cwd <- normalizePath(getwd(), mustWork = FALSE)
  if (dir.exists(file.path(cwd, "pipelines"))) {
    return(cwd)
  }

  stop("Could not locate BDBV2026-Processing_Code repo root (expected a pipelines/ directory).", call. = FALSE)
}

code_root <- resolve_repo_root()
setup_path <- file.path(code_root, "pipelines", "0-setup.R")
if (!file.exists(setup_path)) {
  stop("Setup script not found: ", setup_path, call. = FALSE)
}
source(setup_path, local = FALSE)
# 0-setup.R re-resolves and overwrites code_root/data_root globally — the
# code_root value above was only needed to locate 0-setup.R in the first place.

clean_key <- function(x) {
  out <- trimws(as.character(x))
  out[is.na(out)] <- ""
  out <- iconv(out, from = "UTF-8", to = "ASCII//TRANSLIT")
  out <- gsub("['`]", "", out)
  out <- gsub("[^[:alnum:] ]", " ", out)
  out <- gsub("[[:space:]]+", " ", out)
  tolower(trimws(out))
}

province_key <- function(x) {
  clean_key(x)
}

province_slug <- function(prov_key) {
  gsub(" ", "-", prov_key)
}

safe_read_dhis2 <- function(path) {
  if (!file.exists(path)) {
    stop("Input CSV not found: ", path, call. = FALSE)
  }
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

safe_read_shapefile <- function(path) {
  if (!file.exists(path)) {
    stop("Shapefile not found: ", path, call. = FALSE)
  }
  sf_use_s2(FALSE)
  shp <- st_read(path, quiet = TRUE)
  if (!all(c("Province", "AS_") %in% names(shp))) {
    stop("Shapefile must contain columns 'Province' and 'AS_'.", call. = FALSE)
  }
  shp
}

build_area_lookup <- function(shp_prov) {
  shp_prov <- st_make_valid(shp_prov)
  target_crs <- st_crs(shp_prov)
  cent <- st_transform(shp_prov, 3857)
  cent <- st_centroid(cent)
  cent <- st_transform(cent, target_crs)
  coords <- st_coordinates(cent)

  lookup <- data.frame(
    area_raw = as.character(shp_prov$AS_),
    area_key = clean_key(shp_prov$AS_),
    x = coords[, "X"],
    y = coords[, "Y"],
    stringsAsFactors = FALSE
  )
  lookup[!duplicated(lookup$area_key) & nzchar(lookup$area_key), , drop = FALSE]
}

filter_infectious_pairs <- function(df, prov_key) {
  prov <- province_key(df$province)
  ha <- trimws(as.character(df$health_area))
  res_ha <- trimws(as.character(df$permanent_residence_health_area))
  hz <- trimws(as.character(df$health_zone))

  keep <- !is.na(prov) & prov == prov_key &
    nzchar(ha) & nzchar(res_ha) & nzchar(hz) &
    (clean_key(ha) != clean_key(res_ha)) &
    (clean_key(hz) != clean_key(res_ha))

  if ("type_of_alert" %in% names(df)) {
    # is_death_alert() (0-setup.R) rather than an exact `!= "death"` test: the
    # upstream export switched to "Décès" on 2026-08-10, which silently turned
    # this exclusion into a no-op and let death alerts into the pair maps.
    keep <- keep & !is_death_alert(df$type_of_alert)
  }

  df[keep, c("health_area", "permanent_residence_health_area", "health_zone"), drop = FALSE]
}

linelist_footnote <- function(linelist_label, detail) {
  paste0("DHIS2 line list: ", linelist_label, ". ", detail)
}

build_pair_layers <- function(df_sub, lookup, province_label) {
  empty_layers <- list(
    map_df = data.frame(
      x_alert = numeric(),
      y_alert = numeric(),
      x_res = numeric(),
      y_res = numeric(),
      alert_name = character(),
      res_name = character(),
      stringsAsFactors = FALSE
    ),
    segments_df = data.frame(x = numeric(), y = numeric(), xend = numeric(), yend = numeric()),
    points_df = data.frame(x = numeric(), y = numeric(), type = character())
  )

  if (!nrow(df_sub)) {
    return(empty_layers)
  }

  df_sub$alert_key <- clean_key(df_sub$health_area)
  df_sub$res_key <- clean_key(df_sub$permanent_residence_health_area)

  alert_idx <- match(df_sub$alert_key, lookup$area_key)
  res_idx <- match(df_sub$res_key, lookup$area_key)
  mapped <- !is.na(alert_idx) & !is.na(res_idx)

  if (!any(mapped)) {
    warning(
      "No filtered rows could be mapped to ", province_label, " shapefile health areas.",
      call. = FALSE
    )
    return(empty_layers)
  }

  map_df <- data.frame(
    x_alert = lookup$x[alert_idx[mapped]],
    y_alert = lookup$y[alert_idx[mapped]],
    x_res = lookup$x[res_idx[mapped]],
    y_res = lookup$y[res_idx[mapped]],
    alert_name = df_sub$health_area[mapped],
    res_name = df_sub$permanent_residence_health_area[mapped],
    stringsAsFactors = FALSE
  )

  list(
    map_df = map_df,
    segments_df = data.frame(
      x = map_df$x_res,
      y = map_df$y_res,
      xend = map_df$x_alert,
      yend = map_df$y_alert
    ),
    points_df = rbind(
      data.frame(x = map_df$x_res, y = map_df$y_res, type = "Residence health area"),
      data.frame(x = map_df$x_alert, y = map_df$y_alert, type = "Site of alert health area")
    )
  )
}

compute_residence_confirmed_ratio <- function(df_prov) {
  if (!"final_mve_case_classification" %in% names(df_prov)) {
    stop(
      "Input CSV missing final_mve_case_classification column.",
      call. = FALSE
    )
  }

  res_ha <- trimws(as.character(df_prov$permanent_residence_health_area))
  alert_ha <- trimws(as.character(df_prov$health_area))
  confirmed <- trimws(as.character(df_prov$final_mve_case_classification)) == "confirmed_case"

  res_keys <- clean_key(res_ha[nzchar(res_ha)])
  conf_keys <- clean_key(alert_ha[confirmed & nzchar(alert_ha)])

  res_counts <- table(res_keys)
  conf_counts <- table(conf_keys)
  all_keys <- union(names(res_counts), names(conf_counts))

  if (!length(all_keys)) {
    return(data.frame(
      area_key = character(),
      n_residence_lines = integer(),
      n_confirmed_cases = integer(),
      residence_confirmed_ratio = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    area_key = all_keys,
    n_residence_lines = as.integer(res_counts[all_keys]),
    n_confirmed_cases = as.integer(conf_counts[all_keys]),
    stringsAsFactors = FALSE
  ) |>
    mutate(
      n_residence_lines = ifelse(is.na(n_residence_lines), 0L, n_residence_lines),
      n_confirmed_cases = ifelse(is.na(n_confirmed_cases), 0L, n_confirmed_cases),
      residence_confirmed_ratio = if_else(
        n_confirmed_cases > 0,
        n_residence_lines / n_confirmed_cases,
        NA_real_
      )
    )
}

COLOR_NO_LINES <- "grey90"
COLOR_RESIDENCE_ONLY <- "#9e4a9c"

compute_residence_only_matched_areas <- function(df_prov, lookup) {
  res_ha <- trimws(as.character(df_prov$permanent_residence_health_area))
  alert_ha <- trimws(as.character(df_prov$health_area))
  confirmed <- trimws(as.character(df_prov$final_mve_case_classification)) == "confirmed_case"

  res_keys <- clean_key(res_ha[nzchar(res_ha)])
  conf_keys <- unique(clean_key(alert_ha[confirmed & nzchar(alert_ha)]))

  matched_res_keys <- unique(res_keys[res_keys %in% lookup$area_key])
  residence_only_keys <- setdiff(matched_res_keys, conf_keys)

  sort(unique(as.character(
    lookup$area_raw[lookup$area_key %in% residence_only_keys]
  )))
}

assign_ratio_fill_group <- function(n_residence_lines, n_confirmed_cases, ratio) {
  dplyr::case_when(
    n_residence_lines > 0L & n_confirmed_cases == 0L ~ "Residence only (no confirmed)",
    n_confirmed_cases == 0L ~ "No lines registered",
    ratio < 0.5 ~ "Ratio: <0.5",
    ratio < 1 ~ "Ratio: 0.5-1",
    ratio < 2 ~ "Ratio: 1-2",
    TRUE ~ "Ratio: >=2"
  )
}

ratio_fill_levels <- c(
  "No lines registered",
  "Residence only (no confirmed)",
  "Ratio: <0.5",
  "Ratio: 0.5-1",
  "Ratio: 1-2",
  "Ratio: >=2"
)

ratio_fill_colors <- c(
  "No lines registered" = COLOR_NO_LINES,
  "Residence only (no confirmed)" = COLOR_RESIDENCE_ONLY,
  "Ratio: <0.5" = "#deebf7",
  "Ratio: 0.5-1" = "#6baed6",
  "Ratio: 1-2" = "#2171b5",
  "Ratio: >=2" = "#08306b"
)

map_theme <- function() {
  theme_minimal(base_size = 11) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = "bottom",
      plot.caption = element_text(size = 9, color = "grey30", hjust = 0.5, margin = margin(t = 8))
    )
}

build_pairs_plot <- function(shp_prov, layers, province_label, linelist_label) {
  plot_obj <- ggplot() +
    geom_sf(data = shp_prov, fill = "grey96", color = "grey70", linewidth = 0.15)

  if (nrow(layers$segments_df)) {
    plot_obj <- plot_obj +
      geom_segment(
        data = layers$segments_df,
        aes(x = x, y = y, xend = xend, yend = yend),
        color = "grey35",
        alpha = 0.55,
        linewidth = 0.75
      ) +
      geom_point(
        data = layers$points_df,
        aes(x = x, y = y, fill = type),
        shape = 21,
        color = "black",
        stroke = 0.2,
        size = 1.9,
        alpha = 0.9
      ) +
      scale_fill_manual(
        values = c(
          "Residence health area" = "#2b6cb0",
          "Site of alert health area" = "#dd6b20"
        ),
        name = NULL
      )
  }

  plot_obj +
    coord_sf(expand = FALSE) +
    labs(
      title = paste0(province_label, ": Residence vs Site of Alert by Health Area"),
      caption = linelist_footnote(
        linelist_label,
        paste0("Infectious pairs mapped: ", nrow(layers$map_df))
      )
    ) +
    map_theme()
}

build_ratio_plot <- function(shp_ratio, province_label, linelist_label) {
  ggplot() +
    geom_sf(
      data = shp_ratio,
      aes(fill = fill_group),
      color = "grey70",
      linewidth = 0.15
    ) +
    scale_fill_manual(
      name = "Health area status",
      values = ratio_fill_colors,
      drop = FALSE
    ) +
    coord_sf(expand = FALSE) +
    labs(
      title = paste0(province_label, ": Residence-to-Confirmed Ratio by Health Area"),
      subtitle = paste(
        "Fill = lines with residence in health area / confirmed cases alerted in health area"
      ),
      caption = linelist_footnote(
        linelist_label,
        "Purple = residence lines only (no confirmed cases)"
      )
    ) +
    map_theme()
}

run_province_maps <- function(
    prov_key,
    province_label,
    df,
    shp,
    output_dir,
    linelist_label) {
  shp_prov <- shp[province_key(shp$Province) == prov_key, ]
  if (!nrow(shp_prov)) {
    stop("No ", province_label, " polygons found in shapefile.", call. = FALSE)
  }

  df_prov <- df[province_key(df$province) == prov_key, , drop = FALSE]
  if (!nrow(df_prov)) {
    warning("No linelist rows for ", province_label, "; skipping.", call. = FALSE)
    return(invisible(NULL))
  }

  lookup <- build_area_lookup(shp_prov)
  df_sub <- filter_infectious_pairs(df, prov_key)
  layers <- build_pair_layers(df_sub, lookup, province_label)

  ratio_tbl <- compute_residence_confirmed_ratio(df_prov)
  shp_ratio <- shp_prov |>
    mutate(area_key = clean_key(AS_)) |>
    left_join(ratio_tbl, by = "area_key")
  shp_ratio$n_residence_lines[is.na(shp_ratio$n_residence_lines)] <- 0L
  shp_ratio$n_confirmed_cases[is.na(shp_ratio$n_confirmed_cases)] <- 0L
  shp_ratio$fill_group <- assign_ratio_fill_group(
    shp_ratio$n_residence_lines,
    shp_ratio$n_confirmed_cases,
    shp_ratio$residence_confirmed_ratio
  )
  shp_ratio$fill_group <- factor(shp_ratio$fill_group, levels = ratio_fill_levels)

  plot_pairs <- build_pairs_plot(shp_prov, layers, province_label, linelist_label)
  plot_ratio <- build_ratio_plot(shp_ratio, province_label, linelist_label)

  file_slug <- province_slug(prov_key)
  png_pairs <- file.path(output_dir, paste0(file_slug, "_residence_vs_alert_health_area_map.png"))
  png_ratio <- file.path(output_dir, paste0(file_slug, "_residence_confirmed_ratio_map.png"))

  ggsave(png_pairs, plot_pairs, width = 10, height = 8, units = "in", dpi = 300)
  ggsave(png_ratio, plot_ratio, width = 10, height = 8, units = "in", dpi = 300)

  residence_only_areas <- compute_residence_only_matched_areas(df_prov, lookup)

  message("--- ", province_label, " ---")
  message("Output PNG (pairs): ", png_pairs)
  message("Output PNG (ratio): ", png_ratio)
  message("Infectious-pair rows: ", nrow(df_sub))
  message("Infectious pairs mapped: ", nrow(layers$map_df))
  message(
    "Health areas with ratio: ",
    sum(shp_ratio$n_confirmed_cases > 0L, na.rm = TRUE),
    " | residence only: ",
    sum(
      shp_ratio$n_residence_lines > 0L & shp_ratio$n_confirmed_cases == 0L,
      na.rm = TRUE
    ),
    " / ",
    nrow(shp_ratio)
  )

  invisible(list(
    province = province_label,
    infectious_pair_rows = nrow(df_sub),
    infectious_pairs_mapped = nrow(layers$map_df),
    health_areas_with_ratio = sum(shp_ratio$n_confirmed_cases > 0L, na.rm = TRUE),
    health_areas_residence_only = sum(
      shp_ratio$n_residence_lines > 0L & shp_ratio$n_confirmed_cases == 0L,
      na.rm = TRUE
    ),
    health_areas_total = nrow(shp_ratio),
    residence_only_areas = residence_only_areas,
    plots = list(
      pairs_png = png_pairs,
      ratio_png = png_ratio
    )
  ))
}

PROVINCES <- c(
  "ituri" = "Ituri",
  "nord kivu" = "Nord-Kivu"
)

epi_dir <- resolve_epi_latest_dir()
linelist_label <- basename(epi_dir)   # e.g. "2026-07-14"
# NOTE: in the upstream (Secure) script, linelist_meta was referenced here
# (line ~466) without ever being assigned when this script runs via its
# normal chain (only defined locally inside write_residence_vs_alert_report()
# in step 6, and only when step 6 is run standalone). That looks like a
# pre-existing bug upstream — sourcing this script there would error on the
# message() below. Fixed here by deriving it from the resolved date folder,
# via the parse_dhis2_linelist_date() helper adapted in 0-setup.R.
linelist_meta <- parse_dhis2_linelist_date(epi_dir)

input_csv <- file.path(epi_dir, "dhis2_processed_linelist_ctsafe.csv")
shp_path <- file.path(
  data_root,
  "assets", "geo", "rdc_aires-de-sante",
  "RDC_Aires de santÇ.shp"
)
output_dir <- file.path(
  data_root,
  "outputs", linelist_label, "residence_vs_alert_health_area_maps"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

df <- safe_read_dhis2(input_csv)
required_cols <- c(
  "province", "health_zone", "health_area",
  "permanent_residence_health_area", "final_mve_case_classification"
)
missing <- setdiff(required_cols, names(df))
if (length(missing)) {
  stop("Input CSV missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
}

shp <- safe_read_shapefile(shp_path)

message("Input CSV: ", input_csv)
message("Shapefile: ", shp_path)
message("Line list: ", linelist_meta$basename, " (", linelist_label, ")")

results <- lapply(names(PROVINCES), function(prov_key) {
  run_province_maps(
    prov_key,
    PROVINCES[[prov_key]],
    df,
    shp,
    output_dir,
    linelist_label
  )
})

report_script <- file.path(
  code_root, "pipelines", "epi_linelist_pipeline", "6-residence-vs-alert-report.R"
)
if (!file.exists(report_script)) {
  stop("Report script not found: ", report_script, call. = FALSE)
}
source(report_script, local = FALSE)
province_results <- Filter(Negate(is.null), results)
write_residence_vs_alert_report(
  data_root = data_root,
  linelist_meta = linelist_meta,
  province_results = province_results,
  maps_dir = output_dir
)
