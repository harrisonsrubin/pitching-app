# Rolling: each pitch type's grade over the season, from the last 100 pitches of that
# type up to each game (shown once a type has 50 pitches), on the same 20-80 scale as
# the season grade. Velocity is shown the same way, in mph.

ROLLING_FACETS <- c("Stuff" = "stuff", "Location" = "location", "Pitching" = "pitching", "Velocity" = "velo")

# A 100-pitch average also carries sampling noise (large for Location and Pitching), so
# each window is pulled toward the type average by k = unit^2 / (unit^2 + s^2 / n): unit^2
# is the spread of true pitcher-type means (the grade unit), s^2 the pitch-to-pitch
# variance of this pitcher's type over the season. That is the same logic that shrinks
# the season grades, so the line moves only as much as the evidence supports.
rolling_grades <- function(display, anchors, facet, window = 100, min_n = 50) {
  if (is.null(display) || nrow(display) == 0) return(tibble::tibble())
  if (facet != "velo" && is.null(anchors)) return(tibble::tibble())
  col <- if (facet == "velo") "velo" else paste0(facet, "_xrv")
  d <- display %>% dplyr::filter(!is.na(.data[[col]])) %>%
    dplyr::arrange(pitch_type, game_date, game_pk, pitch_uid)
  out <- lapply(split(d, d$pitch_type), function(t) {
    if (nrow(t) < min_n) return(NULL)
    t$i <- seq_len(nrow(t))
    ends <- t %>% dplyr::group_by(game_date, game_pk) %>% dplyr::summarise(i = max(i), .groups = "drop")
    ends <- ends[ends$i >= min_n, ]
    if (nrow(ends) == 0) return(NULL)
    vals <- vapply(ends$i, function(e) mean(t[[col]][max(1, e - window + 1):e]), 0)
    tibble::tibble(pitch_type = t$pitch_type[1], game_date = ends$game_date,
                   n_window = pmin(ends$i, window), raw = vals, s2 = stats::var(t[[col]]))
  })
  r <- dplyr::bind_rows(out)
  if (nrow(r) == 0) return(r)
  if (facet == "velo") return(dplyr::mutate(r, value = raw))
  a <- anchors[anchors$facet == facet & anchors$scope == "type", ]
  i <- match(r$pitch_type, a$pitch_type)
  r %>% dplyr::mutate(k = a$unit[i]^2 / (a$unit[i]^2 + s2 / n_window),
                      value = clamp_grade(50 - 10 * k * (raw - a$mean[i]) / a$unit[i])) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::select(-k)
}

rollingUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("head")),
    radioButtons(ns("facet"), NULL, ROLLING_FACETS, inline = TRUE),
    uiOutput(ns("message")),
    bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, plotly::plotlyOutput(ns("plot"), height = "480px"))),
    p(class = "pv-note", "Each point is the last 100 pitches of that type up to that game (shown from 50 pitches on), pulled toward average in proportion to how noisy that window is. Location and Pitching move less than Stuff because single pitches vary more. Dashed lines are the full-season grades."))
}

rollingServer <- function(id, outputs, sel, display) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      rolling_grades(display$rows(), outputs$anchors, input$facet %||% "stuff")
    })
    output$head <- renderUI(pitcher_heading(outputs$pitchers, sel$state()))
    output$message <- renderUI({
      if (is.null(display$rows())) div(class = "alert alert-warning", UNAVAILABLE)
      else if (nrow(data()) == 0) div(class = "alert alert-info", "No pitch type has 50 pitches yet this season.")
    })
    output$plot <- plotly::renderPlotly({
      r <- data(); req(nrow(r) > 0)
      facet <- input$facet %||% "stuff"; s <- sel$state()
      p <- plotly::plot_ly()
      for (ty in intersect(PITCH_LABELS, unique(r$pitch_type))) {
        rt <- r[r$pitch_type == ty, ]
        p <- plotly::add_trace(p, x = rt$game_date, y = rt$value, type = "scatter", mode = "lines+markers", name = ty,
                               line = list(color = PITCH_COLORS[[ty]], width = 2.5), marker = list(color = PITCH_COLORS[[ty]], size = 6),
                               text = sprintf("%s, %s<br>%s %.1f (last %d)", ty, format(rt$game_date, "%b %d"),
                                              names(ROLLING_FACETS)[ROLLING_FACETS == facet], rt$value, rt$n_window),
                               hoverinfo = "text")
        if (facet != "velo") {
          g <- outputs$grades[outputs$grades$pitcher == s$pitcher & outputs$grades$level == s$level &
                                outputs$grades$season == s$season & outputs$grades$half == "full" &
                                outputs$grades$facet == facet & outputs$grades$pitch_type == ty, ]
          if (nrow(g) == 1 && !is.na(g$grade_type))
            p <- plotly::add_trace(p, x = range(r$game_date), y = rep(g$grade_type, 2), type = "scatter", mode = "lines",
                                   line = list(color = PITCH_COLORS[[ty]], dash = "dash", width = 1), hoverinfo = "skip",
                                   showlegend = FALSE)
        }
      }
      ytitle <- if (facet == "velo") "Velocity (mph)" else paste(names(ROLLING_FACETS)[ROLLING_FACETS == facet], "grade (20-80)")
      plotly_style(p) %>% plotly::layout(xaxis = list(title = ""), yaxis = list(title = ytitle)) %>%
        plotly::config(displaylogo = FALSE)
    })
  })
}
