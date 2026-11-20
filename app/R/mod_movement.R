# Movement page: horizontal × induced vertical break (air-neutral, inches, catcher's
# view as Statcast reports it), league ellipses for the pitcher's own pitch types at his
# arm-angle decile, and per-pitch hover with expected run value and its league
# percentile for that pitch type. The editor reads selections from this plot.

make_ellipse <- function(cx, cy, sx, sy, rho, scale = 1.5, n = 72) {
  t <- seq(0, 2 * pi, length.out = n)
  rho <- max(min(rho, 0.99), -0.99)
  a <- sx * scale * cos(t)
  b <- sy * scale * (rho * cos(t) + sqrt(1 - rho^2) * sin(t))
  tibble::tibble(x = cx + a, y = cy + b)
}

xrv_pctl <- function(xrv, q) {
  if (is.null(q) || length(q) < 2) return(rep(NA_real_, length(xrv)))
  p <- stats::approx(q, seq(0, 100, 1), xout = xrv, rule = 2, ties = "ordered")$y
  round(100 - p)
}

pctl_text <- function(d, pctl, level, season) {
  lines <- lapply(c("stuff", "location", "pitching"), function(f) {
    out <- character(nrow(d))
    for (ty in unique(d$pitch_type)) {
      i <- d$pitch_type == ty
      q <- pctl$q[pctl$level == level & pctl$season == season & pctl$pitch_type == ty & pctl$facet == f]
      q <- if (length(q)) unlist(q[[1]]) else NULL
      pc <- xrv_pctl(d[[paste0(f, "_xrv")]][i], q)
      out[i] <- sprintf("%s xRV/100 %+.2f (pctl %s)", tools::toTitleCase(f), 100 * d[[paste0(f, "_xrv")]][i],
                        ifelse(is.na(pc), "–", pc))
    }
    out
  })
  paste(sprintf("%s · %.1f mph · IVB %.1f · HB %.1f · %s rpm", d$pitch_type, d$velo, d$ivb, d$hb,
                formatC(d$spin_rate, format = "d", big.mark = ",")),
        sprintf("%s · %d-%d · vs %sHB", format(d$game_date), d$balls, d$strikes, d$stand),
        lines[[1]], lines[[2]], lines[[3]], sep = "<br>")
}

pitcher_arm_decile <- function(ref, hand, arm) {
  r <- dplyr::distinct(ref[ref$p_throws == hand, ], arm_decile, arm_lo, arm_hi)
  if (nrow(r) == 0 || is.na(arm)) return(NA_integer_)
  r$arm_decile[which.min(pmax(r$arm_lo - arm, arm - r$arm_hi, 0))]
}

movement_plot <- function(d, movement_ref, pctl, level, season, corrected_uids = character()) {
  hand <- d$p_throws[1]
  arm <- mean(d$arm_angle, na.rm = TRUE)
  ref <- movement_ref[movement_ref$level == level & movement_ref$season == season, ]
  dec <- pitcher_arm_decile(ref, hand, arm)
  p <- plotly::plot_ly(source = "movement")
  for (ty in intersect(PITCH_LABELS, unique(d$pitch_type))) {
    e <- ref[ref$p_throws == hand & ref$pitch_type == ty & ref$arm_decile %in% dec, ]
    if (nrow(e) == 1) {
      el <- make_ellipse(e$mean_hb, e$mean_ivb, e$sd_hb, e$sd_ivb, e$rho)
      p <- plotly::add_trace(p, data = el, x = ~x, y = ~y, type = "scatter", mode = "lines",
                             name = paste(ty, "league"), hoverinfo = "skip", showlegend = FALSE,
                             line = list(color = PITCH_COLORS[[ty]], dash = "dot", width = 1))
    }
  }
  text <- pctl_text(d, pctl, level, season)
  for (ty in intersect(PITCH_LABELS, unique(d$pitch_type))) {
    i <- d$pitch_type == ty
    corrected <- d$pitch_uid[i] %in% corrected_uids
    p <- plotly::add_trace(p, x = d$hb[i], y = d$ivb[i], key = d$pitch_uid[i], text = text[i],
                           type = "scatter", mode = "markers", name = ty, hoverinfo = "text",
                           marker = list(color = PITCH_COLORS[[ty]], size = 9, opacity = 0.8,
                                         line = list(color = "black", width = ifelse(corrected, 2, 0))))
  }
  if (!is.na(arm)) {
    side <- sign(mean(d$rel_x, na.rm = TRUE)); side <- ifelse(side == 0, 1, side)
    len <- 22
    p <- plotly::add_trace(p, x = c(0, side * len * cos(arm * pi / 180)), y = c(0, len * sin(arm * pi / 180)),
                           type = "scatter", mode = "lines", name = sprintf("arm angle %.0f°", arm),
                           line = list(color = "#2e8b57", dash = "dash", width = 2), hoverinfo = "name")
  }
  plotly::layout(p, dragmode = "lasso",
                 xaxis = list(title = "Horizontal break (in, catcher's view)", range = c(-28, 28), zeroline = TRUE),
                 yaxis = list(title = "Induced vertical break (in)", range = c(-28, 28), zeroline = TRUE,
                              scaleanchor = "x"),
                 legend = list(orientation = "h")) %>%
    plotly::event_register("plotly_selected")
}

side_filter <- function(d, side) if (is.null(d) || !(side %in% c("L", "R"))) d else d[d$stand == side, ]

corrected_uids <- function(d, edits) {
  if (is.null(d)) return(character())
  c(d$pitch_uid[!is.na(d$pitch_type_savant) & d$pitch_type != d$pitch_type_savant],
    if (!is.null(edits)) edits$pitch_uid else character())
}

UNAVAILABLE <- "Pitch data unavailable right now. Grades, Leaderboard and Scorecard still work."

movementUI <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("message")), plotly::plotlyOutput(ns("plot"), height = "620px"))
}

movementServer <- function(id, outputs, sel, display) {
  moduleServer(id, function(input, output, session) {
    output$message <- renderUI(if (is.null(display$rows())) div(class = "alert alert-warning", UNAVAILABLE))
    output$plot <- plotly::renderPlotly({
      d <- side_filter(display$rows(), sel$state()$side); req(d, nrow(d) > 0)
      s <- sel$state()
      movement_plot(d, outputs$movement_ref, outputs$pctl, s$level, s$season, corrected_uids(d, display$edits()))
    })
  })
}
