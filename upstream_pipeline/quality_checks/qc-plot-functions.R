# qc-plot-functions.R
#
# The quality-check charts, as functions that take data and RETURN ggplot
# objects -- no files read or written, nothing run on load. Shared by:
#   plot-column-timeline.R, plot-metrics.R   (nightly: save PNG / PDF)
#   shiny/server_quality.R                   (the Data Quality tab: draw live)
# so the app and the saved charts cannot drift apart.
#
# Needs ggplot2 loaded by the caller. Everything here is prefixed qcp_.

QCP_TITLES <- c(epi_linelist = "Epi linelist",
                contact_tracing = "Contact tracing, each export (its own window)",
                contact_tracing_combined = "Contact tracing, combined all-time table")
QCP_TOP <- 8   # categories kept for the many-valued measures
QCP_POOLED <- c("health_zone", "province")
# Measures drawn on a log10 scale in the line chart: one category dwarfs the
# rest (GeneXpert is ~98% blank). The panel's numbers are log10(share):
# -1 = 10%, -2 = 1%, -3 = 0.1%. A share of 0 has no log, so the line breaks.
QCP_LOG <- c("genexpert_result")
# Measures stacked over TESTED rows only (blank dropped) in the stacked charts.
# Empty since 2026-10-27: shares are shown out of ALL rows, blanks included, so
# every band reads as a share of the same total (GeneXpert's tested rows are
# then a thin layer under a large blank band -- the line chart, on a log
# scale, is the place to read them).
QCP_TESTED_ONLY <- character(0)

# Read a quality-check CSV as text (NULL if absent).
qcp_read <- function(dir, name) {
  p <- file.path(dir, name)
  if (!file.exists(p)) return(NULL)
  utils::read.csv(p, colClasses = "character", fileEncoding = "UTF-8",
                  na.strings = character(0), check.names = FALSE)
}

qcp_trunc <- function(x, n = 70)
  ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1), "…"), x)

qcp_theme <- function()
  ggplot2::theme_minimal(base_size = 9) +
  ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0),
                 panel.grid.minor = ggplot2::element_blank(),
                 plot.title.position = "plot")


# =============================================================================
# Column timeline
# =============================================================================

# The ctsafe allowlists, read out of the cleaning scripts without running
# them, so the bold rows always match what is actually shared.
qcp_vector_from <- function(script, name) {
  for (e in parse(script, keep.source = FALSE, encoding = "UTF-8")) {
    if (is.call(e) && length(e) == 3L &&
        (identical(e[[1]], as.name("<-")) || identical(e[[1]], as.name("="))) &&
        identical(e[[2]], as.name(name)))
      return(as.character(eval(e[[3]], baseenv())))
  }
  stop(basename(script), " no longer defines ", name, call. = FALSE)
}
qcp_safe_columns <- function(repo_root) list(
  epi_linelist = qcp_vector_from(
    file.path(repo_root, "pipelines", "epi_linelist_pipeline", "1-colname-standard.R"),
    "CT_COLUMNS"),
  contact_tracing = qcp_vector_from(
    file.path(repo_root, "pipelines", "contact_tracing_pipeline", "1-colname-standard.R"),
    "CT_SAFE_COLUMNS"))

# Gantt-style chart of which columns each export had. Returns
# list(plot, height, n_columns, n_exports), or NULL with no exports.
#   one row per column, as cleaning names it: green = renamed by us (our name
#   shown), orange = no rename (raw name shown), bold = in the ctsafe export
#   (for the org-unit hierarchy: its province / zone / area parts are);
#   a dot per export that had it, a line joining consecutive exports, a gap
#   where it was missing, a break + new grey label where DHIS2 renamed it.
qcp_column_timeline <- function(cols, files, safe, ds, title) {
  fh <- files[files$dataset == ds & files$read_ok == "TRUE", , drop = FALSE]
  if (!nrow(fh)) return(NULL)
  fh <- fh[order(fh$export_date, fh$file), , drop = FALSE]
  fh$idx  <- seq_len(nrow(fh))      # export order: "consecutive" means idx + 1
  fh$date <- as.Date(fh$export_date)

  d <- cols[cols$dataset == ds, , drop = FALSE]
  d <- merge(d, fh[, c("file", "idx", "date")], by = "file")
  d$mapped <- d$mapping %in% c("explicit", "builtin")
  d$row    <- ifelse(d$mapped, d$standard_name, d$raw_name)
  # One entry per row per export (two raw columns renamed to the same name in
  # one export: keep the first).
  d <- d[order(d$row, d$idx, as.integer(d$position)), , drop = FALSE]
  d <- d[!duplicated(d[, c("row", "idx")]), , drop = FALSE]

  # Segments: a new one starts at a new row, after a missing export, or when
  # the raw name changes.
  n <- nrow(d)
  d$seg <- cumsum(c(TRUE, d$row[-1] != d$row[-n] | diff(d$idx) != 1 |
                          d$raw_name[-1] != d$raw_name[-n]))
  segs <- do.call(rbind, lapply(split(d, d$seg), function(s)
    data.frame(row = s$row[1], raw_name = s$raw_name[1], mapped = s$mapped[1],
               start = min(s$date), end = max(s$date), stringsAsFactors = FALSE)))
  segs <- segs[order(segs$row, segs$start), , drop = FALSE]
  # Label a segment with its raw name when the row is green and the raw name
  # is new for that row (first segment, or DHIS2 renamed it).
  first_or_changed <- c(TRUE, segs$row[-1] != segs$row[-nrow(segs)] |
                              segs$raw_name[-1] != segs$raw_name[-nrow(segs)])
  labs_df <- segs[first_or_changed & segs$mapped, , drop = FALSE]
  labs_df$label <- qcp_trunc(labs_df$raw_name)

  # Rows: by first appearance, then column position; first at the top.
  d$pos <- as.integer(d$position)
  first <- stats::aggregate(cbind(idx, pos) ~ row, data = d, FUN = min)
  first <- first[order(first$idx, first$pos), , drop = FALSE]
  meta  <- unique(d[, c("row", "mapped", "standard_name")])
  meta  <- meta[!duplicated(meta$row), , drop = FALSE]
  meta  <- meta[match(first$row, meta$row), , drop = FALSE]
  meta$safe <- meta$mapped & (meta$standard_name %in% safe[[ds]] |
                              meta$standard_name == "org_unit_hierarchy")
  levels_bottom_up <- rev(meta$row)
  meta_bu <- meta[match(levels_bottom_up, meta$row), , drop = FALSE]

  d$row       <- factor(d$row, levels = levels_bottom_up)
  segs$row    <- factor(segs$row, levels = levels_bottom_up)
  labs_df$row <- factor(labs_df$row, levels = levels_bottom_up)
  d$status    <- ifelse(d$mapped, "mapped", "unmapped")
  segs$status <- ifelse(segs$mapped, "mapped", "unmapped")

  pal <- c(mapped = "#2E8B57", unmapped = "#D9730D")
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = segs, ggplot2::aes(x = start, xend = end, y = row, yend = row,
                                                    colour = status),
                          linewidth = 1.1, lineend = "round") +
    ggplot2::geom_point(data = d, ggplot2::aes(x = date, y = row, colour = status), size = 1.4) +
    ggplot2::geom_text(data = labs_df, ggplot2::aes(x = start, y = row, label = label),
                       hjust = 0, vjust = -0.7, size = 3.2, colour = "grey35") +
    ggplot2::scale_colour_manual(values = pal, name = NULL,
                                 labels = c(mapped = "renamed by us (our name shown)",
                                            unmapped = "no rename (raw DHIS2 name shown)")) +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%d %b",
                          expand = ggplot2::expansion(mult = c(0.01, 0.02))) +
    ggplot2::scale_y_discrete(labels = qcp_trunc(levels_bottom_up, 55),
                              expand = ggplot2::expansion(add = c(0.6, 1.2))) +
    ggplot2::labs(title = title,
         subtitle = sprintf("%d exports, %s to %s. Dot = column present in that export; gap = missing; break + new grey label = DHIS2 renamed it.",
                            nrow(fh), format(min(fh$date), "%d %b"), format(max(fh$date), "%d %b %Y")),
         caption = "Bold = in the ctsafe export shared downstream. Grey text = raw DHIS2 column name.",
         x = "Export date", y = NULL) +
    ggplot2::theme_minimal(base_size = 13) +
    # element_text() with one colour per label works but warns that it is
    # "not officially supported"; callers suppress that warning.
    ggplot2::theme(axis.text.y = ggplot2::element_text(
                     colour = pal[ifelse(meta_bu$mapped, "mapped", "unmapped")],
                     face = ifelse(meta_bu$safe, "bold", "plain")),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_line(colour = "grey93"),
                   legend.position = "top", plot.title.position = "plot")
  # Text sized for reading at full size (base 13, labels 3.2 -- raised from
  # 9 / 2.1 on 2026-10-27); each row gets the height that text needs.
  list(plot = p, height = max(6, 2.2 + 0.42 * nrow(meta)), n_columns = nrow(meta),
       n_exports = nrow(fh))
}


# =============================================================================
# Metrics history
# =============================================================================

# Types the text columns of metrics_history.csv.
qcp_metrics_types <- function(hist) {
  hist$date  <- as.Date(hist$export_date)
  hist$n     <- as.numeric(hist$n)
  hist$share <- as.numeric(hist$share)
  hist
}

# Everything the three metric chart styles share, for one dataset and subset:
# many-valued measures cut to their largest categories (the rest "(other)"),
# every category present in every export (0 where absent -- so a code turning
# into a label shows as one line falling as another rises), the row counts,
# and the flagged jumps not explained by a known event. NULL if no data.
qcp_metrics_prepare <- function(hist, find, ds, subset = "all", measures = NULL) {
  h <- hist[hist$dataset == ds & hist$subset == subset, , drop = FALSE]
  if (!nrow(h)) return(NULL)
  s <- h[!is.na(h$share), c("date", "source", "measure", "category", "n", "share")]
  # No measures named = the hand-picked ones only. The per-column measures
  # ("col.<column>", one per column of the processed file) are drawn only when
  # asked for by name: all of them at once is ~60 panels.
  if (!is.null(measures)) s <- s[s$measure %in% measures, , drop = FALSE]
  else s <- s[!startsWith(s$measure, "col."), , drop = FALSE]
  if (!nrow(s)) return(NULL)
  latest <- max(s$date)
  for (m in QCP_POOLED) {
    sm <- s[s$measure == m, , drop = FALSE]
    if (!nrow(sm)) next
    last <- sm[sm$date == latest, , drop = FALSE]
    keep <- utils::head(last$category[order(-last$share)], QCP_TOP)
    sm$category[!sm$category %in% keep] <- "(other)"
    sm <- stats::aggregate(cbind(n, share) ~ date + source + measure + category, sm, sum)
    s <- rbind(s[s$measure != m, , drop = FALSE], sm[, names(s)])
  }
  s <- do.call(rbind, lapply(split(s, s$measure), function(sm) {
    grid <- merge(unique(sm[, c("date", "source", "measure")]),
                  data.frame(category = unique(sm$category), stringsAsFactors = FALSE))
    out <- merge(grid, sm, all.x = TRUE)
    out$n[is.na(out$n)] <- 0; out$share[is.na(out$share)] <- 0
    out[, c("date", "source", "measure", "category", "n", "share")]
  }))
  rows <- hist[hist$dataset == ds & hist$subset == "all" & hist$measure == "n_rows", , drop = FALSE]
  f <- if (is.null(find)) NULL else
    find[find$dataset == ds & find$status == "new", , drop = FALSE]
  if (is.null(f)) f <- data.frame(file = character(0), column = character(0),
                                  stringsAsFactors = FALSE)
  f$measure  <- sub(": .*$", "", f$column)
  f$category <- sub("^[^:]*: ", "", f$column)
  # flags are only computed on subset "all"
  if (subset != "all") f <- f[0, , drop = FALSE]
  list(ds = ds, subset = subset, s = s, rows = rows, flags = f, latest = latest,
       n_exports = length(unique(s$source)), show_rows = is.null(measures))
}

qcp_label <- function(m) ifelse(m %in% QCP_LOG,
                                paste0(m, " (share, log10: -1 = 10%, -2 = 1%, -3 = 0.1%)"),
                                paste0(m, " (share)"))

# Lines: each category's share over time; red rings on flagged jumps.
# Returns the plot with attribute "n_panels".
qcp_metrics_lines <- function(pr) {
  s <- pr$s
  lg <- s$measure %in% QCP_LOG
  s$share[lg] <- ifelse(s$share[lg] > 0, log10(s$share[lg]), NA_real_)
  s$measure <- qcp_label(s$measure)
  d <- s
  lv <- sort(unique(s$measure))
  if (pr$show_rows && nrow(pr$rows)) {
    rows <- data.frame(date = pr$rows$date, source = pr$rows$source, measure = "rows (count)",
                       category = "rows", n = pr$rows$n, share = pr$rows$n,
                       stringsAsFactors = FALSE)
    d <- rbind(rows, s)
    lv <- c("rows (count)", lv)
  }
  d$measure <- factor(d$measure, levels = lv)

  f <- pr$flags; f$measure <- qcp_label(f$measure)
  fl <- merge(d, unique(f[, c("file", "measure", "category")]),
              by.x = c("source", "measure", "category"), by.y = c("file", "measure", "category"))
  # Direct labels at each line's last point instead of one huge legend.
  lab <- do.call(rbind, lapply(split(d, paste(d$measure, d$category)), function(g)
    { g <- g[!is.na(g$share), , drop = FALSE]      # log panels: skip 0 shares
      if (nrow(g)) g[which.max(g$date), , drop = FALSE] else NULL }))

  p <- ggplot2::ggplot(d, ggplot2::aes(date, share, group = category, colour = category)) +
    ggplot2::geom_line(linewidth = 0.5, na.rm = TRUE) +
    ggplot2::geom_point(size = 0.6, na.rm = TRUE) +
    ggplot2::geom_point(data = fl, shape = 21, size = 2.6, colour = "red", stroke = 0.8,
                        fill = NA, na.rm = TRUE) +
    ggplot2::geom_text(data = lab, ggplot2::aes(label = category), hjust = -0.05, size = 2.3,
                       show.legend = FALSE) +
    ggplot2::facet_wrap(~ measure, scales = "free_y", ncol = 2) +
    ggplot2::scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b",
                          expand = ggplot2::expansion(mult = c(0.02, 0.22))) +
    ggplot2::labs(title = QCP_TITLES[[pr$ds]],
         subtitle = sprintf("%d exports to %s. Share of %s rows in each category; red ring = share jumped (> 10 points and unlikely to be chance).",
                            pr$n_exports, format(pr$latest, "%d %b %Y"),
                            if (pr$subset == "all") "all" else "new"),
         x = NULL, y = NULL) +
    qcp_theme() + ggplot2::theme(legend.position = "none")
  attr(p, "n_panels") <- length(lv)
  p
}

# Stacked bands: shares (to 100%) or counts (to the total). Bands are ribbons
# with bounds computed here (not position = "stack"), so each band's label can
# sit in its middle at the right edge. Bottom to top: largest categories,
# "(other)", then "(blank)" on top; those two grey. GeneXpert over tested rows
# only. Red triangles under a panel: an export where a share in it jumped.
# Returns the plot with attribute "n_panels".
qcp_metrics_stacked <- function(pr, value = c("share", "n")) {
  value <- match.arg(value)
  s <- pr$s
  tested <- s$measure %in% QCP_TESTED_ONLY
  s <- s[!(tested & s$category == "(blank)"), , drop = FALSE]
  if (value == "share") {
    tot <- stats::ave(s$n, s$measure, s$source, FUN = sum)
    s$y <- ifelse(tot > 0, s$n / tot, 0)
  } else {
    s$y <- s$n
  }
  s$panel <- paste0(s$measure, ifelse(s$measure %in% QCP_TESTED_ONLY, " (tested rows only)", ""))

  ord <- stats::aggregate(y ~ panel + category, s, sum)
  ord$rank <- ord$y
  ord$rank[ord$category == "(other)"] <- -1
  ord$rank[ord$category == "(blank)"] <- -2
  ord <- ord[order(ord$panel, -ord$rank), , drop = FALSE]
  ord$pos <- stats::ave(seq_len(nrow(ord)), ord$panel, FUN = seq_along)
  s$pos <- ord$pos[match(paste(s$panel, s$category), paste(ord$panel, ord$category))]
  s <- s[order(s$panel, s$date, s$pos), , drop = FALSE]
  s$ymax <- stats::ave(s$y, s$panel, s$date, FUN = cumsum)
  s$ymin <- s$ymax - s$y

  last <- s[s$date == pr$latest, , drop = FALSE]
  top <- stats::ave(last$ymax, last$panel, FUN = max)
  lab <- last[last$y >= 0.03 * top & last$y > 0, , drop = FALSE]
  lab$ymid <- (lab$ymin + lab$ymax) / 2

  f <- pr$flags
  f$panel <- paste0(f$measure, ifelse(f$measure %in% QCP_TESTED_ONLY, " (tested rows only)", ""))
  fl <- unique(merge(unique(f[, c("file", "panel")]), unique(s[, c("source", "date", "panel")]),
                     by.x = c("file", "panel"), by.y = c("source", "panel")))

  cats <- unique(s$category)
  pal <- stats::setNames(grDevices::hcl.colors(max(3, length(cats)), "Dark 3")[seq_along(cats)], cats)
  pal[names(pal) == "(blank)"] <- "grey85"
  pal[names(pal) == "(other)"] <- "grey65"

  what <- if (value == "share") "share of rows in each category, stacked to 100%"
          else "rows in each category, stacked to the total (contacts for per-contact measures)"
  p <- ggplot2::ggplot(s, ggplot2::aes(date, group = category)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ymin, ymax = ymax, fill = category),
                         colour = "white", linewidth = 0.1, alpha = 0.9) +
    ggplot2::geom_text(data = lab, ggplot2::aes(x = date, y = ymid, label = category),
                       hjust = -0.05, size = 2.3) +
    ggplot2::geom_point(data = fl, ggplot2::aes(x = date, y = 0), inherit.aes = FALSE,
                        shape = 17, colour = "red", size = 1.6) +
    ggplot2::facet_wrap(~ panel, scales = "free_y", ncol = 2) +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b",
                          expand = ggplot2::expansion(mult = c(0.01, 0.22))) +
    ggplot2::scale_y_continuous(labels = if (value == "share") function(x) paste0(round(100 * x), "%")
                                         else function(x) format(x, big.mark = ",", scientific = FALSE)) +
    ggplot2::labs(title = QCP_TITLES[[pr$ds]],
         subtitle = sprintf("%d exports to %s (%s rows): %s; red triangle = a share in that panel jumped.",
                            pr$n_exports, format(pr$latest, "%d %b %Y"), pr$subset, what),
         x = NULL, y = NULL) +
    qcp_theme() + ggplot2::theme(legend.position = "none")
  attr(p, "n_panels") <- length(unique(s$panel))
  p
}

# Sankey: type of alert -> patient status at sampling, for one linelist export
# (default: the latest). NULL if there is nothing to draw.
qcp_sankey <- function(hist, source = NULL) {
  h <- hist[hist$dataset == "epi_linelist" & hist$subset == "all" &
            hist$measure == "alert_type_x_sample_status", , drop = FALSE]
  if (!nrow(h)) return(NULL)
  if (is.null(source)) source <- h$source[which.max(h$date)]
  h <- h[h$source == source & h$n > 0, , drop = FALSE]
  if (!nrow(h)) return(NULL)
  parts <- strsplit(h$category, " -> ", fixed = TRUE)
  fl <- data.frame(from = vapply(parts, `[`, "", 1), to = vapply(parts, `[`, "", 2),
                   n = h$n, stringsAsFactors = FALSE)
  total <- sum(fl$n); gap <- 0.02 * total

  stack_nodes <- function(sizes) {
    sizes <- sort(sizes, decreasing = TRUE)
    top <- cumsum(sizes + gap) - gap
    data.frame(node = names(sizes), y1 = top, y0 = top - sizes, stringsAsFactors = FALSE)
  }
  L <- stack_nodes(tapply(fl$n, fl$from, sum)); R <- stack_nodes(tapply(fl$n, fl$to, sum))
  fl <- fl[order(match(fl$from, L$node), match(fl$to, R$node)), , drop = FALSE]
  fl$l0 <- NA_real_; fl$r0 <- NA_real_
  for (nd in L$node) { i <- which(fl$from == nd)
    fl$l0[i] <- L$y0[L$node == nd] + c(0, cumsum(fl$n[i])[-length(i)]) }
  o <- order(match(fl$to, R$node), match(fl$from, L$node))
  for (nd in R$node) { i <- o[fl$to[o] == nd]
    fl$r0[i] <- R$y0[R$node == nd] + c(0, cumsum(fl$n[i])[-length(i)]) }

  t <- seq(0, 1, length.out = 60); sm <- 3 * t^2 - 2 * t^3
  ribbons <- do.call(rbind, lapply(seq_len(nrow(fl)), function(k) {
    lo <- fl$l0[k] + (fl$r0[k] - fl$l0[k]) * sm
    data.frame(id = k, from = fl$from[k], x = c(t, rev(t)), y = c(lo + fl$n[k], rev(lo)))
  }))
  nodes <- rbind(data.frame(L, x0 = -0.04, x1 = 0), data.frame(R, x0 = 1, x1 = 1.04))
  nodes$n <- nodes$y1 - nodes$y0
  nodes$label <- sprintf("%s\n%s (%.0f%%)", nodes$node, format(nodes$n, big.mark = ","),
                         100 * nodes$n / total)
  nodes$hj <- ifelse(nodes$x0 < 0.5, 1.05, -0.05)
  nodes$lx <- ifelse(nodes$x0 < 0.5, nodes$x0, nodes$x1)

  ggplot2::ggplot() +
    ggplot2::geom_polygon(data = ribbons, ggplot2::aes(x, y, group = id, fill = from), alpha = 0.45) +
    ggplot2::geom_rect(data = nodes, ggplot2::aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1),
                       fill = "grey30") +
    ggplot2::geom_text(data = nodes, ggplot2::aes(x = lx, y = (y0 + y1) / 2, label = label,
                                                  hjust = hj), size = 3, lineheight = 0.9) +
    ggplot2::annotate("text", x = c(-0.02, 1.02), y = -gap * 1.5,
                      label = c("Type of alert", "Status at sampling"), fontface = "bold", size = 3.5) +
    ggplot2::scale_x_continuous(limits = c(-0.45, 1.45)) +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(title = "Epi linelist: type of alert -> patient status at sampling",
         subtitle = sprintf("%s, %s rows. Ribbon width = number of rows with that combination.",
                            source, format(total, big.mark = ","))) +
    ggplot2::theme_void(base_size = 10) + ggplot2::theme(legend.position = "none")
}
