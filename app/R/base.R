# Shared constants and theme for the app

# Constants
`%>%` <- magrittr::`%>%`  # app is deployed alone, so define the pipe here

# Must match reference/pitch_type_map.csv (a test checks it)
PITCH_LABELS <- c("4S", "SNK", "CUT", "SL", "SW", "CB", "CS", "SV", "CH", "SP", "FK", "KN")

PITCH_COLORS <- c(
  "4S" = "#E41A1C", "SNK" = "#08306B", "CUT" = "#17BECF", "SL" = "#006D2C", "SW" = "#7B2D8B",
  "CB" = "#FFD700", "CS" = "#B8860B", "SV" = "#964B00", "CH" = "#FF8C00", "SP" = "#FF69B4",
  "FK" = "#FA8072", "KN" = "#A9A9A9")

# Grade color steps, best to worst
GRADE_BINS <- c(70, 60, 55, 45, 40, 30)
GRADE_COLORS <- c("#1a7f37", "#8fd19e", "#e6f4c3", "#fff7cc", "#fdd49e", "#f4a582", "#c0392b")

# Map grades to their fill colors
grade_color <- function(x) {
  i <- vapply(x, function(v) if (is.na(v)) NA_integer_ else which(c(v >= GRADE_BINS, TRUE))[1], 1L)
  GRADE_COLORS[i]
}

# Keep grades on the 20-80 scale
clamp_grade <- function(x) pmin(pmax(x, 20), 80)

# Stepped plotly colorscale matching the grade colors
grade_colorscale <- function() {
  breaks <- c(20, rev(GRADE_BINS), 80)
  cols <- rev(GRADE_COLORS)
  pos <- (breaks - 20) / 60
  unlist(lapply(seq_along(cols), function(i) list(list(pos[i], cols[i]), list(pos[i + 1], cols[i]))),
         recursive = FALSE)
}

PLATE_HALF_WIDTH <- 0.83
LEVELS <- c("MLB", "AAA")
LEVEL_LABELS <- c(MLB = "MLB", AAA = "Triple-A")

# Theme
INK <- "#16213a"; SLATE <- "#4a5568"; PAPER <- "#f6f8fb"; LINE <- "#d8dee8"; FIELD <- "#1f6f4a"
DISPLAY_FONT <- "Barlow Condensed"
BODY_FONT <- "IBM Plex Sans"

# Bootstrap theme with Google fonts and the app CSS
app_theme <- function() {
  bslib::bs_theme(
    version = 5, bg = PAPER, fg = INK, primary = FIELD, secondary = SLATE,
    base_font = bslib::font_google(BODY_FONT, wght = c(400, 500, 600), local = FALSE),
    heading_font = bslib::font_google(DISPLAY_FONT, wght = c(500, 600, 700), local = FALSE),
    font_scale = 0.95, `border-color` = LINE, `border-radius` = "6px"
  ) %>% bslib::bs_add_rules(APP_CSS)
}

# Page CSS, colors and fonts filled in by sprintf
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
.pv-pills { flex-basis: 100%%; display: flex; flex-wrap: wrap; align-items: center; gap: 6px; margin-top: -6px; }
.pv-pills-label { font-size: .8rem; font-weight: 600; color: %5$s; margin-right: 4px; }
.pv-pill { display: inline-flex; align-items: center; gap: 6px; padding: 3px 4px 3px 10px; border: 1px solid %4$s;
  border-radius: 14px; background: #fff; color: %1$s; font-size: .85rem; font-weight: 600; cursor: pointer; }
.pv-pill:hover { border-color: %2$s; }
.pv-pill.active { background: %1$s; border-color: %1$s; color: #fff; }
.pv-pill-lvl { font-size: .68rem; font-weight: 600; opacity: .75; }
.pv-pill-grade { min-width: 28px; padding: 1px 6px; border-radius: 10px; color: %1$s; font-family: '%3$s', sans-serif; font-weight: 700; font-size: .95rem; text-align: center; }
.pv-proj { flex-basis: 100%%; border-top: 1px solid %4$s; padding-top: 14px; margin-top: 4px; }
.pv-proj-title { font-family: '%3$s', sans-serif; font-weight: 600; font-size: 1.25rem; color: %1$s; margin-bottom: 10px; }
.pv-basis { margin-left: 10px; font-family: inherit; font-size: .75rem; font-weight: 600; padding: 2px 9px; border-radius: 10px; background: %6$s; color: %5$s; vertical-align: middle; }
.pv-proj-grid { display: grid; grid-template-columns: minmax(300px, 1fr) minmax(320px, 1.2fr); gap: 28px; align-items: end; }
.pv-bet-head { display: flex; align-items: baseline; gap: 8px; margin-bottom: 6px; }
.pv-bet-grade { font-family: '%3$s', sans-serif; font-weight: 700; font-size: 2.2rem; line-height: 1; color: %1$s; }
.pv-bars { display: flex; align-items: flex-end; gap: 4px; height: 96px; max-width: 480px; }
.pv-bar { display: flex; flex-direction: column; align-items: center; justify-content: flex-end; flex: 1 1 0; max-width: 38px; height: 100%%; }
.pv-bar-n { font-size: .72rem; font-weight: 700; color: %1$s; height: 14px; }
.pv-bar-fill { width: 78%%; border-radius: 3px 3px 0 0; background: %4$s; border: 1px solid rgba(22,33,58,.18); border-bottom: 0; }
.pv-bar-empty .pv-bar-fill { border-color: transparent; }
.pv-bar-x { font-size: .68rem; color: %5$s; margin-top: 3px; border-top: 1px solid %4$s; width: 100%%; text-align: center; padding-top: 2px; }
.pv-bet-note { font-size: .75rem; color: %5$s; margin-top: 6px; }
.pv-stats { display: grid; grid-template-columns: repeat(3, minmax(96px, 1fr)); gap: 10px; }
.pv-stat { background: %6$s; border: 1px solid %4$s; border-radius: 6px; padding: 10px 12px; }
.pv-stat-label { font-size: .78rem; font-weight: 600; color: %5$s; }
.pv-stat-value { font-family: '%3$s', sans-serif; font-weight: 700; font-size: 1.9rem; line-height: 1.05; color: %1$s; margin-top: 2px; }
.pv-stat-sub { font-size: .75rem; color: %5$s; }
.pv-luck-row { display: flex; flex-wrap: wrap; align-items: center; gap: 8px 22px; margin-top: 14px; padding: 9px 12px; border-radius: 6px; background: %6$s; border: 1px solid %4$s; }
.pv-luck-title { font-family: '%3$s', sans-serif; font-weight: 600; font-size: 1rem; color: %1$s; }
.pv-luck-piece { font-size: .85rem; color: %5$s; display: flex; gap: 6px; align-items: baseline; }
.pv-luck-piece b { font-weight: 700; font-size: .95rem; }
.pv-up { color: #b23b2e; }
.pv-down { color: %2$s; }
@media (max-width: 900px) { .pv-proj-grid { grid-template-columns: 1fr; } .pv-stats { grid-template-columns: repeat(2, 1fr); } }
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

# Shared plotly styling
plotly_style <- function(p) {
  plotly::layout(p, font = list(family = paste0(BODY_FONT, ", Helvetica, Arial, sans-serif"), color = INK, size = 13),
                 paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
                 hoverlabel = list(font = list(family = BODY_FONT, size = 12, color = INK), bgcolor = "#ffffff", bordercolor = LINE),
                 legend = list(orientation = "h", y = -0.12, font = list(size = 13)),
                 margin = list(t = 20, r = 20, b = 20, l = 60))
}

# Shared ggplot theme
theme_pv <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(text = ggplot2::element_text(color = INK),
                   strip.text = ggplot2::element_text(face = "bold", size = 13, color = INK),
                   axis.title = ggplot2::element_text(color = SLATE),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(color = "#edf0f5"),
                   plot.background = ggplot2::element_rect(fill = "#ffffff", color = NA))
}

# White text on the darkest grade colors
grade_text_color <- function() DT::styleInterval(c(39.999999, 69.999999), c("#ffffff", INK, "#ffffff"))
# DT cell fill by grade
grade_fill <- function() DT::styleInterval(rev(GRADE_BINS) - 1e-9, rev(GRADE_COLORS))
