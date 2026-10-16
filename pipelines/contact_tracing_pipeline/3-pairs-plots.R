#!/usr/bin/env Rscript
# Step 3: Plots of infector-infectee pairs.
#   - Reads step 1's infector_infectee_pairs.csv and step 2's
#     infector_infectee_summary.csv from outputs/<DATE>/contact_tracing/
#   - Writes each plot as SVG and PNG (300 dpi) to outputs/<DATE>/contact_tracing/plots/:
#       01_linelist_presence.svg       pairs by whether infector/infectee are in the linelist
#       02_conflicts.svg               people/pairs with conflicting duplicate records
#       03_classification_matrix.svg   infector vs infectee classification (both in linelist)
#       03b_contact_status_matrix.svg  infectee follow-up result vs linelist classification (infectee in linelist)
#       04_followup_results.svg       infectees' final follow-up result
#       05_contacts_per_infector.svg   how many contacts each infector has
#       06_onset_interval.svg          infectee onset minus infector onset (confirmed pairs)
#       07_pairs_over_time.svg         new pairs by week of first follow-up
#       08_onset_to_source_record.svg  weekly median/mean/IQR days from infector onset to first record as source case
#
# Styled with lshtm_theme() (LSHTM dark teal) -- see "Styling" below.

# ------------------------- Check if initialised -------------------------------
if (!exists("setup_complete")) {
  source("pipelines/0-setup.R")
} else if (!setup_complete) {
  source("pipelines/0-setup.R")
}

ct_cfg <- pipeline_configs$contact_tracing

ct_dir <- resolve_ct_latest_dir()
run_date <- basename(ct_dir)
ct_outputs_dir <- file.path(data_root, "outputs", run_date, ct_cfg$data$outputs_subdir)
plots_dir <- file.path(ct_outputs_dir, ct_cfg$data$plots_subdir)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

message("======== Source case-contact plots: ", run_date, " =========")

# ------------------------------- Load data ------------------------------------
pairs_path <- file.path(ct_outputs_dir, ct_cfg$data$output_files$pairs)
summary_path <- file.path(ct_outputs_dir, ct_cfg$data$output_files$summary)
for (p in c(pairs_path, summary_path)) {
  if (!file.exists(p)) {
    stop("Missing ", p, ". Run steps 1 and 2 first.", call. = FALSE)
  }
}

pairs <- readr::read_csv(pairs_path, col_types = readr::cols(.default = "c")) |>
  dplyr::mutate(
    dplyr::across(dplyr::ends_with(c("_in_linelist", "_has_conflict")), ~ .x == "TRUE"),
    reciprocal_pair = reciprocal_pair == "TRUE",
    first_followup_date = as.Date(first_followup_date)
  )
summary_table <- readr::read_csv(summary_path, col_types = readr::cols(n = "i", .default = "c"))

# Catch a pairs/summary file written by an older version of steps 1-2.
required_pair_cols <- c("infector_first_source_record_date", "contact_info_has_conflict",
                        "infectee_final_followup_result", "first_followup_date")
missing_cols <- setdiff(required_pair_cols, names(pairs))
if (length(missing_cols)) {
  stop("The pairs file is out of date (missing: ", paste(missing_cols, collapse = ", "),
       "). Rerun 1-make-infector-infectee-pairs.R and 2-pairs-summary-table.R first.", call. = FALSE)
}
if (!any(grepl("conflicting linelist info", summary_table$metric))) {
  stop("The summary file is out of date. Rerun 2-pairs-summary-table.R first.", call. = FALSE)
}

# -------------------------------- Styling -------------------------------------
# LSHTM-style theme (dark teal #01454f), with palette colours chosen to match.
COLOR_INK <- "#01454f"     # dark teal: text, borders, strips
COLOR_MAIN <- "#1a8c99"    # mid teal: main bars
COLOR_LIGHT <- "#e3f0f1"   # pale teal: low end of heatmap
COLOR_MUTED <- "#a3b9bc"   # grey-teal: secondary category
COLOR_ALERT <- "#e0703f"   # coral: conflicts / likely errors
PLOT_WIDTH_IN <- 9
PLOT_HEIGHT_IN <- 4.5
BASE_FONT_SIZE <- 12

lshtm_theme <- function(base_size = BASE_FONT_SIZE) {
  theme_bw(base_size = base_size) +
    theme(
      # Border and background
      panel.border = element_rect(colour = COLOR_INK, fill = NA, linewidth = 0.5),
      panel.background = element_rect(fill = "white"),
      # Grid: faint teal major lines, no minor lines (less clutter)
      panel.grid.major = element_line(colour = "#e6eef0", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      # Text, axes
      text = element_text(colour = COLOR_INK),
      axis.text = element_text(colour = COLOR_INK),
      axis.title = element_text(colour = COLOR_INK),
      axis.ticks = element_line(colour = COLOR_INK),
      plot.title = element_text(colour = COLOR_INK, face = "bold"),
      plot.subtitle = element_text(colour = "#4a7378", size = rel(0.85)),
      plot.caption = element_text(colour = COLOR_MUTED, hjust = 0, size = rel(0.75)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      # Facet strips
      strip.text.x = element_text(colour = "white", face = "bold"),
      strip.text.y = element_text(colour = "white", face = "bold", angle = 270),
      strip.background = element_rect(colour = COLOR_INK, fill = COLOR_INK, linewidth = 1.5),
      # Legend
      legend.position = "bottom",
      legend.title = element_text(colour = COLOR_INK, face = "bold"),
      legend.text = element_text(colour = COLOR_INK)
    )
}
theme_set(lshtm_theme())
ct_plot_theme <- lshtm_theme

# For horizontal bar charts: drop the gridlines running along the bars.
no_y_grid <- theme(panel.grid.major.y = element_blank())

caption_text <- paste0("Contact tracing data: ", run_date,
                       ". Values with '/' = duplicate records disagree.")

save_plot <- function(plot, name, width = PLOT_WIDTH_IN, height = PLOT_HEIGHT_IN) {
  out <- file.path(plots_dir, name)
  ggsave(out, plot = plot, device = "svg", width = width, height = height, units = "in")
  # PNG copy alongside, at 300 dpi (good for slides and documents).
  png_name <- sub("\\.svg$", ".png", name)
  ggsave(file.path(plots_dir, png_name), plot = plot, device = "png",
         width = width, height = height, units = "in", dpi = 300, bg = "white")
  message("  Wrote ", name, " + ", png_name)
}

# Blank classification/result shown as "blank" rather than dropped.
blank_label <- function(x) ifelse(is.na(x) | x == "", "blank", x)

message("")
message("MAKING PLOTS")

# ------------------- 1. Pairs by linelist presence ----------------------------
# Multipanel of pie charts (patchwork):
#   left column  -- unique infectors / unique infectees: in vs not in linelist
#   right (large) -- pairs: both / only infector / only infectee / neither
# Counts and percentages go in the legend labels, because some slices are
# too thin to label (e.g. "only infectee" is well under 1% of pairs).
# Percentages are also printed on slices big enough to fit them (>= 5%).

# Pie chart from a data frame with columns `group` (factor) and `n`.
make_pie <- function(df, colours, title, legend_position = "bottom", legend_ncol = 1) {
  df <- df |>
    dplyr::mutate(
      pct = n / sum(n),
      legend_label = paste0(group, " (", scales::comma(n), ", ", scales::percent(pct, accuracy = 0.1), ")"),
      slice_label = ifelse(pct >= 0.05, scales::percent(pct, accuracy = 1), "")
    )
  labels <- stats::setNames(df$legend_label, df$group)
  ggplot(df, aes(x = 1, y = n, fill = group)) +
    geom_col(width = 1, color = "white", linewidth = 0.8) +
    geom_text(aes(label = slice_label), position = position_stack(vjust = 0.5),
              color = "white", fontface = "bold", size = 3.5) +
    coord_polar(theta = "y", start = 0, direction = -1) +
    scale_fill_manual(values = colours, labels = labels, breaks = names(colours)) +
    guides(fill = guide_legend(ncol = legend_ncol)) +
    labs(title = title, fill = NULL) +
    theme_void(base_size = BASE_FONT_SIZE) +
    theme(
      plot.title = element_text(colour = COLOR_INK, face = "bold", hjust = 0.5),
      legend.position = legend_position,
      legend.text = element_text(colour = COLOR_INK, size = rel(0.85))
    )
}

in_ll_colours <- c("In linelist" = COLOR_INK, "Not in linelist" = COLOR_MUTED)
people_in_ll <- function(ids, in_ll) {
  people <- dplyr::distinct(data.frame(id = ids, in_ll = in_ll))
  data.frame(
    group = factor(c("In linelist", "Not in linelist"), levels = names(in_ll_colours)),
    n = c(sum(people$in_ll), sum(!people$in_ll))
  )
}

pie_infectors <- make_pie(
  people_in_ll(pairs$infector_person_id, pairs$infector_in_linelist),
  in_ll_colours,
  paste0("Source cases (", scales::comma(dplyr::n_distinct(pairs$infector_person_id)), ")")
)
pie_infectees <- make_pie(
  people_in_ll(pairs$infectee_person_id, pairs$infectee_in_linelist),
  in_ll_colours,
  paste0("Contacts (", scales::comma(dplyr::n_distinct(pairs$infectee_person_id)), ")")
)

pair_colours <- c("Both in linelist" = COLOR_INK, "Only source case in linelist" = COLOR_MAIN,
                  "Only contact in linelist" = COLOR_ALERT, "Neither in linelist" = COLOR_MUTED)
presence <- summary_table |>
  dplyr::filter(grepl("^Pairs: ", metric)) |>
  dplyr::mutate(group = dplyr::case_when(
    metric == "Pairs: both in linelist" ~ "Both in linelist",
    metric == "Pairs: only infector in linelist" ~ "Only source case in linelist",
    metric == "Pairs: only infectee in linelist" ~ "Only contact in linelist",
    metric == "Pairs: neither in linelist" ~ "Neither in linelist"
  )) |>
  dplyr::filter(!is.na(group)) |>
  dplyr::mutate(group = factor(group, levels = names(pair_colours)))

pie_pairs <- make_pie(
  presence, pair_colours,
  paste0("Source case-contact pairs (", scales::comma(sum(presence$n)), ")"),
  legend_ncol = 2
)

p1 <- patchwork::wrap_plots(
  A = pie_infectors, B = pie_infectees, C = pie_pairs,
  design = "AC\nBC", widths = c(1, 2)
) +
  patchwork::plot_annotation(
    title = "Who is in the linelist?",
    subtitle = "Unique source cases and contacts (left) and source case-contact pairs (right)",
    caption = caption_text,
    theme = theme(
      plot.title = element_text(colour = COLOR_INK, face = "bold", size = rel(1.3)),
      plot.subtitle = element_text(colour = "#4a7378"),
      plot.caption = element_text(colour = COLOR_MUTED, hjust = 0, size = rel(0.75))
    )
  )
save_plot(p1, "01_linelist_presence.svg", width = 10, height = 7)

# ------------------------- 2. Conflicts ---------------------------------------
# Unique infectors/infectees with conflicts, by source (linelist, contact
# tracing, either). Parsed from step 2's metric names, e.g.
# "Infectors with conflicting linelist info".
conflicts <- summary_table |>
  dplyr::filter(grepl("^(Infectors|Infectees) with .*conflicting.* info$", metric)) |>
  dplyr::mutate(
    role = factor(sub(" with.*", "", metric), levels = c("Infectors", "Infectees"),
                  labels = c("Source cases", "Contacts")),
    type = dplyr::case_when(
      grepl("any conflicting", metric) ~ "Any",
      grepl("contact tracing", metric) ~ "Contact tracing",
      grepl("linelist", metric) ~ "Linelist"
    ),
    type = factor(type, levels = c("Any", "Contact tracing", "Linelist")),
    is_any = type == "Any"
  )

p2 <- ggplot(conflicts, aes(x = n, y = type, fill = is_any)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = scales::comma(n)), hjust = -0.2, size = 3.5, color = COLOR_INK) +
  facet_wrap(~role) +
  scale_fill_manual(values = c(`TRUE` = COLOR_ALERT, `FALSE` = "#f0b79c"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)), labels = scales::comma) +
  labs(title = "People with conflicting information from duplicate records",
       subtitle = paste0("Linelist: their own linelist rows disagree.\n",
                         "Contact tracing: at least one of their pairs has disagreeing records."),
       x = "Number of people", y = "Conflict source", caption = caption_text) +
  ct_plot_theme() + no_y_grid
save_plot(p2, "02_conflicts.svg")

# -------------- 3. Infector vs infectee classification ------------------------
class_matrix <- pairs |>
  dplyr::filter(infector_in_linelist, infectee_in_linelist) |>
  dplyr::count(infector = blank_label(infector_final_linelist_classification),
               infectee = blank_label(infectee_final_linelist_classification))

if (nrow(class_matrix)) {
  p3 <- ggplot(class_matrix, aes(x = infectee, y = infector, fill = n)) +
    geom_tile(color = "white", linewidth = 1) +
    # White labels on dark tiles, teal on light ones, so counts stay readable.
    geom_text(aes(label = n, color = n > max(n) / 2), size = 3.5, fontface = "bold") +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = COLOR_INK), guide = "none") +
    scale_fill_gradient(low = COLOR_LIGHT, high = COLOR_INK, guide = "none") +
    labs(title = "Linelist classification: source case vs contact",
         subtitle = paste0("Pairs where both people are in the linelist (n = ",
                           sum(class_matrix$n), ")"),
         x = "Contact classification", y = "Source case classification",
         caption = caption_text) +
    ct_plot_theme() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1), panel.grid = element_blank())
  save_plot(p3, "03_classification_matrix.svg", height = 5.5)
} else {
  message("  Skipped 03_classification_matrix.svg (no pairs with both in linelist)")
}

# ------ 3b. Contacts found in the linelist: follow-up result vs classification ------
# For pairs whose infectee (contact) is in the linelist: does the contact
# tracing outcome agree with the linelist? E.g. a contact confirmed in the
# linelist but "not_a_case" at final follow-up is worth checking. Counts are
# pairs, so a contact with two infectors is counted twice.
contact_status <- pairs |>
  dplyr::filter(infectee_in_linelist) |>
  dplyr::count(classification = blank_label(infectee_final_linelist_classification),
               followup = blank_label(infectee_final_followup_result))

if (nrow(contact_status)) {
  p3b <- ggplot(contact_status, aes(x = classification, y = followup, fill = n)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = n, color = n > max(n) / 2), size = 3.5, fontface = "bold") +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = COLOR_INK), guide = "none") +
    scale_fill_gradient(low = COLOR_LIGHT, high = COLOR_INK, guide = "none") +
    labs(title = "Contacts found in the linelist: follow-up result vs linelist classification",
         subtitle = paste0("Pairs where the contact is in the linelist (n = ", sum(contact_status$n),
                           "). 'blank' = no value recorded"),
         x = "Contact linelist classification", y = "Contact final follow-up result",
         caption = caption_text) +
    ct_plot_theme() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1), panel.grid = element_blank())
  save_plot(p3b, "03b_contact_status_matrix.svg", height = 5.5)
} else {
  message("  Skipped 03b_contact_status_matrix.svg (no contacts in linelist)")
}

# --------------------- 4. Final follow-up results -----------------------------
followup <- pairs |>
  dplyr::mutate(
    result = blank_label(infectee_final_followup_result),
    infectee_group = ifelse(infectee_in_linelist, "In linelist", "Not in linelist")
  ) |>
  dplyr::count(result, infectee_group)
result_order <- followup |>
  dplyr::group_by(result) |>
  dplyr::summarise(total = sum(n), .groups = "drop") |>
  dplyr::arrange(total) |>
  dplyr::pull(result)

p4 <- ggplot(followup, aes(x = n, y = factor(result, levels = result_order), fill = infectee_group)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("In linelist" = COLOR_INK, "Not in linelist" = COLOR_MUTED)) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Contacts' final follow-up result",
       subtitle = "Result on the latest dated follow-up that has a result; 'blank' = no result recorded",
       x = "Number of pairs", y = NULL, fill = "Contact", caption = caption_text) +
  ct_plot_theme() + no_y_grid
save_plot(p4, "04_followup_results.svg", height = 5)

# ---------------------- 5. Contacts per infector ------------------------------
contact_bins <- c(0, 1, 5, 10, 20, 50, Inf)
contact_labels <- c("1", "2-5", "6-10", "11-20", "21-50", ">50")
per_infector <- pairs |>
  dplyr::count(infector_person_id, name = "n_contacts") |>
  dplyr::mutate(bin = cut(n_contacts, breaks = contact_bins, labels = contact_labels)) |>
  dplyr::count(bin, .drop = FALSE)

p5 <- ggplot(per_infector, aes(x = bin, y = n)) +
  geom_col(fill = COLOR_MAIN, width = 0.7) +
  geom_text(aes(label = n), vjust = -0.4, size = 3.5, color = COLOR_INK) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Number of contacts per source case",
       subtitle = paste0(dplyr::n_distinct(pairs$infector_person_id), " source cases"),
       x = "Contacts listed", y = "Number of source cases", caption = caption_text) +
  ct_plot_theme()
save_plot(p5, "05_contacts_per_infector.svg")

# ------------------ 6. Onset-to-onset interval --------------------------------
# Only pairs where both are confirmed with a single, unambiguous raw onset
# date. This is a crude serial interval: no adjustment for who infected whom
# or reporting delays. Negative values (infectee onset first) are highlighted
# as likely data errors or reversed direction.
onset_pairs <- pairs |>
  dplyr::filter(infector_final_linelist_classification == "confirmed_case",
                infectee_final_linelist_classification == "confirmed_case",
                !grepl("/", infector_onset_date), !grepl("/", infectee_onset_date)) |>
  dplyr::mutate(interval_days = as.numeric(as.Date(infectee_onset_date) - as.Date(infector_onset_date))) |>
  dplyr::filter(!is.na(interval_days)) |>
  dplyr::mutate(direction = ifelse(interval_days < 0, "Before source case (likely error)", "After source case"))

if (nrow(onset_pairs)) {
  med <- stats::median(onset_pairs$interval_days[onset_pairs$interval_days >= 0])
  p6 <- ggplot(onset_pairs, aes(x = interval_days, fill = direction)) +
    geom_histogram(binwidth = 2, boundary = 0, color = "white") +
    geom_vline(xintercept = med, linetype = "dashed", color = COLOR_INK) +
    scale_fill_manual(values = c("After source case" = COLOR_MAIN,
                                 "Before source case (likely error)" = COLOR_ALERT)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = "Days between source case and contact symptom onset",
         subtitle = paste0("Confirmed-confirmed pairs with one recorded onset date each (n = ",
                           nrow(onset_pairs), "). Dashed line = median of non-negative values (",
                           med, " days)"),
         x = "Contact onset minus source case onset (days)", y = "Number of pairs",
         fill = "Contact onset",
         caption = caption_text) +
    ct_plot_theme()
  save_plot(p6, "06_onset_interval.svg")
} else {
  message("  Skipped 06_onset_interval.svg (no confirmed-confirmed pairs with onset dates)")
}

# ------------------------- 7. Pairs over time ---------------------------------
weekly <- pairs |>
  dplyr::filter(!is.na(first_followup_date)) |>
  dplyr::mutate(
    week = lubridate::floor_date(first_followup_date, "week", week_start = 1),
    infector_group = ifelse(infector_in_linelist, "In linelist", "Not in linelist")
  ) |>
  dplyr::count(week, infector_group)

# x axis starts here. coord_cartesian() just zooms in, so earlier pairs are
# hidden from view rather than dropped (no "removed rows" warnings).
PAIRS_OVER_TIME_START <- as.Date("2026-05-01")
p7_xlim <- c(PAIRS_OVER_TIME_START, max(weekly$week, PAIRS_OVER_TIME_START) + 7)

p7 <- ggplot(weekly, aes(x = week, y = n, fill = infector_group)) +
  geom_col(width = 6) +
  scale_fill_manual(values = c("In linelist" = COLOR_INK, "Not in linelist" = COLOR_MUTED)) +
  scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "New source case-contact pairs per week",
       subtitle = "By week of the pair's first follow-up visit (weeks start Monday)",
       x = NULL, y = "Number of pairs", fill = "Source case", caption = caption_text) +
  coord_cartesian(xlim = p7_xlim) +
  ct_plot_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p7, "07_pairs_over_time.svg")

# ------------- 8. Infector onset to first record as source case ---------------
# One row per infector: days from their symptom onset (raw linelist date, only
# if they have a single unambiguous one) to their first record as a source
# case (earliest registration_date among their contacts). Grouped by the week
# of that first record, from PAIRS_OVER_TIME_START. Delays outside
# [DELAY_MIN, DELAY_MAX] days are excluded as likely date errors (same range
# as the epi pipeline's delay config), and the number excluded is reported.
DELAY_MIN <- pipeline_configs$epi_linelist$nowcasting$delay$min_days
DELAY_MAX <- pipeline_configs$epi_linelist$nowcasting$delay$max_days

infector_delays <- pairs |>
  dplyr::distinct(infector_person_id, infector_onset_date, infector_first_source_record_date) |>
  dplyr::filter(!is.na(infector_onset_date), !grepl("/", infector_onset_date),
                !is.na(infector_first_source_record_date)) |>
  dplyr::mutate(
    onset = as.Date(infector_onset_date),
    first_record = as.Date(infector_first_source_record_date),
    delay_days = as.numeric(first_record - onset),
    week = lubridate::floor_date(first_record, "week", week_start = 1)
  ) |>
  dplyr::filter(first_record >= PAIRS_OVER_TIME_START)

n_out_of_range <- sum(infector_delays$delay_days < DELAY_MIN | infector_delays$delay_days > DELAY_MAX)
infector_delays <- dplyr::filter(infector_delays, delay_days >= DELAY_MIN, delay_days <= DELAY_MAX)

weekly_delay <- infector_delays |>
  dplyr::group_by(week) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean_delay = mean(delay_days),
    median_delay = stats::median(delay_days),
    q25 = stats::quantile(delay_days, 0.25, names = FALSE),
    q75 = stats::quantile(delay_days, 0.75, names = FALSE),
    .groups = "drop"
  )

# Long format for the two lines, so mean/median get a legend.
weekly_delay_lines <- weekly_delay |>
  tidyr::pivot_longer(c(median_delay, mean_delay), names_to = "stat", values_to = "delay") |>
  dplyr::mutate(stat = factor(ifelse(stat == "median_delay", "Median", "Mean"),
                              levels = c("Median", "Mean")))

if (nrow(weekly_delay)) {
  p8 <- ggplot(weekly_delay, aes(x = week)) +
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = "IQR (25th-75th percentile)"), alpha = 0.25) +
    geom_line(data = weekly_delay_lines, aes(y = delay, colour = stat, linetype = stat), linewidth = 0.9) +
    geom_point(data = weekly_delay_lines, aes(y = delay, colour = stat, shape = stat), size = 2) +
    # number of infectors behind each week, along the bottom
    geom_text(aes(y = 0, label = n), vjust = -0.5, size = 2.8, color = COLOR_MUTED) +
    scale_fill_manual(values = c("IQR (25th-75th percentile)" = COLOR_MAIN)) +
    scale_colour_manual(values = c(Median = COLOR_INK, Mean = COLOR_ALERT)) +
    scale_linetype_manual(values = c(Median = "solid", Mean = "dashed")) +
    scale_shape_manual(values = c(Median = 16, Mean = 17)) +
    scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks") +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(title = "Time from source case symptom onset to first record as a source case",
         subtitle = paste0(
           "By week of first record (earliest contact registration). ",
           "Numbers = source cases per week (", nrow(infector_delays), " total).\n",
           n_out_of_range, " source cases with delays outside ", DELAY_MIN, "-", DELAY_MAX,
           " days excluded as likely date errors."),
         x = NULL, y = "Delay (days)", colour = NULL, linetype = NULL, shape = NULL, fill = NULL,
         caption = caption_text) +
    ct_plot_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  save_plot(p8, "08_onset_to_source_record.svg")
} else {
  message("  Skipped 08_onset_to_source_record.svg (no source cases with onset and registration dates)")
}

message("")
message("Plots written to ", plots_dir)
