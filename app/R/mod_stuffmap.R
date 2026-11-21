# Stuff Map: every pitcher's average movement for one pitch type, colored by his Stuff
# grade for it. Horizontal break is shown as arm-side break (lefties mirrored) so
# righties and lefties compare directly. The sidebar's pitcher is highlighted.

stuff_map_data <- function(profiles, grades, pitchers, level, season, pitch_type, min_pitches) {
  p <- profiles[profiles$level == level & profiles$season == season & profiles$pitch_type == pitch_type &
                  profiles$n >= min_pitches, ]
  if (nrow(p) == 0) return(tibble::tibble())
  g <- grades[grades$level == level & grades$season == season & grades$half == "full" &
                grades$facet == "stuff" & grades$pitch_type == pitch_type, c("pitcher", "grade_type")]
  who <- pitchers[pitchers$level == level & pitchers$season == season, c("pitcher", "name", "team_abbrev")]
  p %>%
    dplyr::mutate(arm_side = hb * ifelse(p_throws == "L", 1, -1)) %>%   # Statcast catcher's view
    dplyr::left_join(dplyr::rename(g, grade = grade_type), by = "pitcher") %>%
    dplyr::left_join(dplyr::rename(who, team = team_abbrev), by = "pitcher") %>%
    dplyr::filter(!is.na(grade))
}

stuffmapUI <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      col_widths = c(8, 4),
      radioButtons(ns("pitch_type"), "Pitch type", PITCH_LABELS, selected = "4S", inline = TRUE),
      sliderInput(ns("min_pitches"), "Min pitches of this type", min = 0, max = 500, value = 0, step = 25, ticks = FALSE)),
    uiOutput(ns("message")),
    bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, plotly::plotlyOutput(ns("plot"), height = "620px"))),
    p(class = "pv-note", "Each dot is one pitcher's average for this pitch type. Arm-side break is positive for both hands. Color is his Stuff grade for the pitch; the pitcher selected in the sidebar is outlined."))
}

stuffmapServer <- function(id, outputs, sel) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      s <- sel$state()
      stuff_map_data(outputs$profiles, outputs$grades, outputs$pitchers, s$level, s$season,
                     input$pitch_type %||% "4S", input$min_pitches %||% 0)
    })
    output$message <- renderUI({
      if (nrow(data()) == 0) div(class = "alert alert-info", "No pitchers threw this pitch type enough for this season and filter.")
    })
    output$plot <- plotly::renderPlotly({
      d <- data(); req(nrow(d) > 0)
      me <- d[d$pitcher == sel$state()$pitcher, ]
      text <- sprintf("<b>%s</b> (%s)<br>Stuff %d<br>%.1f mph, IVB %.1f, arm-side %.1f<br>%s pitches",
                      d$name, d$team, round(d$grade), d$velo, d$ivb, d$arm_side, format(d$n, big.mark = ","))
      p <- plotly::plot_ly(d, x = ~arm_side, y = ~ivb, type = "scatter", mode = "markers", text = text, hoverinfo = "text",
                           marker = list(color = ~grade, cmin = 20, cmax = 80, colorscale = grade_colorscale(), size = 9,
                                         line = list(color = "rgba(22,33,58,.35)", width = 0.5),
                                         colorbar = list(title = "Stuff", thickness = 12)),
                           name = "Pitchers", showlegend = FALSE)
      if (nrow(me) == 1)
        p <- plotly::add_trace(p, x = me$arm_side, y = me$ivb, type = "scatter", mode = "markers+text",
                               text = me$name, textposition = "top center", hoverinfo = "skip", inherit = FALSE,
                               marker = list(size = 18, color = "rgba(0,0,0,0)", line = list(color = INK, width = 2.5)),
                               showlegend = FALSE)
      plotly_style(p) %>% plotly::layout(
        xaxis = list(title = "Arm-side horizontal break (in)", zeroline = TRUE, zerolinecolor = LINE),
        yaxis = list(title = "Induced vertical break (in)", zeroline = TRUE, zerolinecolor = LINE)) %>%
        plotly::config(displaylogo = FALSE)
    })
  })
}
