`%>%` <- magrittr::`%>%`   # app/R is loaded on its own on the host, without R/load.R

# Shared app constants. PITCH_LABELS must match reference/pitch_type_map.csv (a test
# checks it); the app is deployed on its own, so it keeps its own copy.

PITCH_LABELS <- c("4S", "SNK", "CUT", "SL", "SW", "CB", "CS", "SV", "CH", "SP", "FK", "KN")

PITCH_COLORS <- c(
  "4S" = "#E41A1C", "SNK" = "#08306B", "CUT" = "#17BECF", "SL" = "#006D2C", "SW" = "#7B2D8B",
  "CB" = "#FFD700", "CS" = "#B8860B", "SV" = "#964B00", "CH" = "#FF8C00", "SP" = "#FF69B4",
  "FK" = "#FA8072", "KN" = "#A9A9A9")

GRADE_BINS <- c(70, 60, 55, 45, 40, 30)
GRADE_COLORS <- c("#1a7f37", "#8fd19e", "#e6f4c3", "#fff7cc", "#fdd49e", "#f4a582", "#c0392b")

grade_color <- function(x) {
  i <- vapply(x, function(v) if (is.na(v)) NA_integer_ else which(c(v >= GRADE_BINS, TRUE))[1], 1L)
  GRADE_COLORS[i]
}

# The scale runs 20-80; nothing outside it is ever shown.
clamp_grade <- function(x) pmin(pmax(x, 20), 80)

# Plotly colorscale for a 20-80 range that steps at GRADE_BINS with GRADE_COLORS.
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
