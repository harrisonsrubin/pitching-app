# Look and feel. A scouting card rather than a dashboard: a condensed sports face for
# names, page titles and grade numbers; a clean sans with even-width digits for tables
# and controls. Fonts load from Google Fonts in the browser (nothing to install).

INK <- "#16213a"; SLATE <- "#4a5568"; PAPER <- "#f6f8fb"; LINE <- "#d8dee8"; FIELD <- "#1f6f4a"
DISPLAY_FONT <- "Barlow Condensed"
BODY_FONT <- "IBM Plex Sans"

app_theme <- function() {
  bslib::bs_theme(
    version = 5, bg = PAPER, fg = INK, primary = FIELD, secondary = SLATE,
    base_font = bslib::font_google(BODY_FONT, wght = c(400, 500, 600), local = FALSE),
    heading_font = bslib::font_google(DISPLAY_FONT, wght = c(500, 600, 700), local = FALSE),
    font_scale = 0.95, `border-color` = LINE, `border-radius` = "6px"
  ) %>% bslib::bs_add_rules(APP_CSS)
}

APP_CSS <- sprintf("
body { font-variant-numeric: tabular-nums; }
.navbar { background: %1$s !important; border-bottom: 3px solid %2$s; }
.navbar .navbar-brand { font-family: '%3$s', sans-serif; font-weight: 700; font-size: 1.6rem;
  letter-spacing: .01em; color: #fff !important; }
.navbar .nav-link { font-family: '%3$s', sans-serif; font-size: 1.15rem; font-weight: 500;
  color: rgba(255,255,255,.72) !important; }
.navbar .nav-link.active, .navbar .nav-link:hover { color: #fff !important; }
.navbar .nav-link.active { box-shadow: inset 0 -3px 0 %2$s; }
.bslib-sidebar-layout > .sidebar { background: #fff; border-right: 1px solid %4$s; }
.sidebar .control-label, .sidebar label { font-weight: 600; font-size: .85rem; color: %5$s; }
.sidebar .form-text, .sidebar .help-block { font-size: .8rem; color: %5$s; }
.form-control, .selectize-input { border-color: %4$s; }

/* Scouting card: the pitcher's name and his three overall grades */
.pv-card { display: flex; flex-wrap: wrap; align-items: center; gap: 22px; padding: 18px 22px;
  background: #fff; border: 1px solid %4$s; border-left: 6px solid %2$s; border-radius: 6px; margin-bottom: 18px; }
.pv-card img { height: 96px; width: auto; border-radius: 4px; background: %6$s; }
.pv-card .pv-name { font-family: '%3$s', sans-serif; font-weight: 700; font-size: 2.6rem; line-height: 1; margin: 0; }
.pv-card .pv-sub { color: %5$s; font-size: .95rem; margin-top: 6px; }
.pv-card .pv-line { color: %1$s; font-size: .9rem; margin-top: 2px; }
.pv-grades { display: flex; gap: 10px; margin-left: auto; }
.pv-grade { min-width: 92px; text-align: center; padding: 8px 10px 6px; border-radius: 6px; border: 1px solid rgba(22,33,58,.08); }
.pv-grade .pv-num { font-family: '%3$s', sans-serif; font-weight: 700; font-size: 2.6rem; line-height: 1; color: %1$s; }
.pv-grade .pv-range { font-size: .75rem; color: %1$s; opacity: .75; }
.pv-proj { flex-basis: 100%%; border-top: 1px solid %4$s; padding-top: 12px; margin-top: 2px; }
.pv-proj-head { font-family: '%3$s', sans-serif; font-weight: 600; font-size: 1.15rem; color: %1$s; }
.pv-basis { margin-left: 10px; font-family: inherit; font-size: .75rem; font-weight: 600; padding: 2px 8px; border-radius: 10px; background: %6$s; color: %5$s; }
.pv-bet { position: relative; height: 62px; margin: 6px 4px 4px; max-width: 560px; }
.pv-bet-axis { position: absolute; left: 0; right: 0; bottom: 0; height: 18px; border-top: 1px solid %4$s; }
.pv-bet-tick { position: absolute; top: 2px; transform: translateX(-50%%); font-size: .7rem; color: %5$s; }
.pv-chip { position: absolute; width: 11px; height: 11px; margin-left: -5px; margin-bottom: 14px; border-radius: 50%%; border: 1px solid rgba(22,33,58,.35); }
.pv-proj-line { font-size: .92rem; color: %1$s; margin-top: 3px; }
.pv-weight-bar { height: 10px; max-width: 420px; background: %4$s; border-radius: 5px; overflow: hidden; margin-top: 8px; }
.pv-weight-stuff { height: 100%%; background: %2$s; }
.pv-section { font-family: '%3$s', sans-serif; font-weight: 600; font-size: 1.3rem; margin: 18px 0 8px; color: %1$s; }
.pv-luck { max-width: 360px; }
.pv-grade .pv-tool { font-size: .85rem; font-weight: 600; color: %1$s; }

h2.pv-title, .pv-title { font-family: '%3$s', sans-serif; font-weight: 600; font-size: 1.7rem; margin: 4px 0 10px; }
.pv-note { color: %5$s; font-size: .85rem; }

/* Tables */
table.dataTable thead th { font-weight: 600; font-size: .82rem; color: %5$s; border-bottom: 2px solid %1$s !important; white-space: nowrap; }
table.dataTable tbody td { font-size: .9rem; padding: 7px 10px !important; }
table.dataTable tbody td.pv-gradecell { font-weight: 600; white-space: nowrap; }
table.dataTable tbody td.pv-name-cell { white-space: nowrap; font-weight: 500; }
table.dataTable tbody tr:last-child td { border-top: 2px solid %4$s; }
.pv-overall-row td { font-weight: 600; background: #fff !important; }

/* Scorecard */
.pv-scorecard { max-width: 1100px; }
.pv-scorecard h1 { font-family: '%3$s', sans-serif; font-weight: 700; font-size: 2.4rem; }
.pv-scorecard h2 { font-family: '%3$s', sans-serif; font-weight: 600; font-size: 1.6rem; margin-top: 28px;
  padding-bottom: 4px; border-bottom: 2px solid %4$s; }
.pv-scorecard p, .pv-scorecard li { max-width: 78ch; line-height: 1.55; }
.pv-scorecard table { border-collapse: collapse; margin: 10px 0 18px; font-size: .88rem; background: #fff; }
.pv-scorecard th { text-align: left; font-weight: 600; color: %5$s; border-bottom: 2px solid %1$s; padding: 6px 12px; }
.pv-scorecard td { border-bottom: 1px solid %4$s; padding: 5px 12px; }
.pv-scorecard tr td:first-child { font-weight: 600; }
.alert { border-radius: 6px; }
", INK, FIELD, DISPLAY_FONT, LINE, SLATE, PAPER)

# Plot styling shared by plotly and ggplot (fonts fall back to the system sans when the
# web fonts are not installed for server-side rendering).
plotly_style <- function(p) {
  plotly::layout(p, font = list(family = paste0(BODY_FONT, ", Helvetica, Arial, sans-serif"), color = INK, size = 13),
                 paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
                 hoverlabel = list(font = list(family = BODY_FONT, size = 12, color = INK), bgcolor = "#ffffff", bordercolor = LINE),
                 legend = list(orientation = "h", y = -0.12, font = list(size = 13)),
                 margin = list(t = 20, r = 20, b = 20, l = 60))
}

theme_pv <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(text = ggplot2::element_text(color = INK),
                   strip.text = ggplot2::element_text(face = "bold", size = 13, color = INK),
                   axis.title = ggplot2::element_text(color = SLATE),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(color = "#edf0f5"),
                   plot.background = ggplot2::element_rect(fill = "#ffffff", color = NA))
}

# Text on the two darkest grade colors is white so it stays readable.
grade_text_color <- function() DT::styleInterval(c(39.999999, 69.999999), c("#ffffff", INK, "#ffffff"))
grade_fill <- function() DT::styleInterval(rev(GRADE_BINS) - 1e-9, rev(GRADE_COLORS))
