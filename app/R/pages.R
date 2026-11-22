# Grade pages: Grades, Movement, Location, Leaderboard, Stuff Map, Command, Rolling

# Pitch summary

# Per-type metrics from the pitcher's view; L/R usage always uses every pitch
pitch_summary <- function(display, side = "All") {
  if (is.null(display) || nrow(display) == 0) return(NULL)
  splits <- display %>% dplyr::group_by(pitch_type) %>%
    dplyr::summarise(usage_l = sum(stand == "L"), usage_r = sum(stand == "R"), .groups = "drop") %>%
    dplyr::mutate(usage_l = usage_l / max(1, sum(display$stand == "L")),
                  usage_r = usage_r / max(1, sum(display$stand == "R")))
  d <- if (side %in% c("L", "R")) display[display$stand == side, ] else display
  if (nrow(d) == 0) return(NULL)
  d %>% dplyr::group_by(pitch_type) %>%
    dplyr::summarise(n = dplyr::n(), velo = mean(velo, na.rm = TRUE), ivb = mean(ivb, na.rm = TRUE),
                     hb = -mean(hb, na.rm = TRUE), spin = mean(spin_rate, na.rm = TRUE), ext = mean(ext, na.rm = TRUE),
                     vaa = mean(vaa, na.rm = TRUE), arm_angle = mean(arm_angle, na.rm = TRUE),
                     rel_z = mean(rel_z, na.rm = TRUE), rel_x = -mean(rel_x, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(usage = n / sum(n)) %>%
    dplyr::left_join(splits, by = "pitch_type") %>%
    dplyr::arrange(dplyr::desc(usage))
}

# Grades page
HEADSHOT_URL <- "https://img.mlbstatic.com/mlb-photos/image/upload/w_213,d_people:generic:headshot:silo:current.png,q_100/v1/people/%d/headshot/silo/current"

# Number and percent formatters that blank out NA
fmt_num <- function(x, digits = 1) ifelse(is.na(x) | is.nan(x), "", formatC(x, digits = digits, format = "f"))
fmt_pct <- function(x) ifelse(is.na(x) | is.nan(x), "", sprintf("%.1f%%", 100 * x))

# "Grade (lo-hi)" cell text
grade_cell <- function(value, lo, hi) {
  ifelse(is.na(value), "", sprintf("%d (%d–%d)", round(clamp_grade(value)), round(clamp_grade(lo)), round(clamp_grade(hi))))
}

# One facet's grades on the chosen scale, range shifted to match
facet_grade <- function(g, facet, scale) {
  f <- g[g$facet == facet, ]
  use_type <- scale == "type" & !is.na(f$grade_type)
  value <- ifelse(use_type, f$grade_type, f$grade_global)
  shift <- ifelse(use_type, f$grade_type - f$grade_global, 0)
  tibble::tibble(pitch_type = f$pitch_type, n_graded = f$n, value = value,
                 cell = grade_cell(value, f$grade_lo90 + shift, f$grade_hi90 + shift))
}

# Per-pitch-type grades table with metrics
grades_table <- function(grades, components, pitcher, level, season, summary, scale, min_pitches = 0) {
  g <- grades[grades$pitcher == pitcher & grades$level == level & grades$season == season & grades$half == "full", ]
  if (nrow(g) == 0) return(NULL)
  out <- NULL
  for (f in c("stuff", "location", "pitching")) {
    fg <- facet_grade(g, f, scale)
    nm <- tools::toTitleCase(f)
    fg <- dplyr::rename(fg, !!nm := cell, !!paste0(f, "_value") := value)
    out <- if (is.null(out)) fg else dplyr::left_join(out, dplyr::select(fg, -n_graded), by = "pitch_type")
  }
  comp <- components[components$pitcher == pitcher & components$level == level & components$season == season, ] %>%
    dplyr::transmute(pitch_type, swing = fmt_pct(x_swing), whiff_swing = fmt_pct(x_whiff_per_swing),
                     chase = fmt_pct(x_chase), cstrike_take = fmt_pct(x_cstrike_per_take),
                     xwoba_con = fmt_num(x_woba_contact, 3), gb = fmt_pct(x_gb))
  out <- dplyr::left_join(out, comp, by = "pitch_type")
  metric_cols <- c("n", "usage", "usage_l", "usage_r", "velo", "ivb", "hb", "spin", "ext", "vaa", "arm_angle", "rel_z", "rel_x")
  if (is.null(summary)) {
    summary <- tibble::tibble(pitch_type = character())
    for (m in metric_cols) summary[[m]] <- numeric()
  }
  out <- dplyr::left_join(out, summary, by = "pitch_type") %>%
    dplyr::mutate(n = dplyr::coalesce(as.numeric(n), as.numeric(n_graded))) %>%
    dplyr::select(-n_graded)
  types <- out[out$pitch_type != "ALL" & out$n >= min_pitches, ]
  types <- types[order(-dplyr::coalesce(types$usage, types$n / sum(types$n))), ]
  dplyr::bind_rows(types, out[out$pitch_type == "ALL", ])
}

# Career seasons

# Every season and level the pitcher has a line, oldest first, with overall grades
career_lines <- function(pitchers, grades, pitcher) {
  p <- pitchers[pitchers$pitcher == pitcher, c("level", "season")]
  g <- grades[grades$pitcher == pitcher & grades$pitch_type == "ALL" & grades$half == "full", ] %>%
    dplyr::select(level, season, facet, grade_global) %>%
    tidyr::pivot_wider(names_from = facet, values_from = grade_global)
  for (f in c("stuff", "location", "pitching")) if (!f %in% names(g)) g[[f]] <- rep(NA_real_, nrow(g))
  out <- dplyr::left_join(dplyr::distinct(p), g, by = c("level", "season"))
  out[order(out$season, match(out$level, LEVELS)), c("level", "season", "stuff", "location", "pitching")]
}

# One button per season; clicking sends "level|season" to input_id
season_pills <- function(lines, level, season, input_id) {
  if (nrow(lines) < 2) return(NULL)
  pill <- function(i) {
    r <- lines[i, ]
    key <- paste(r$level, r$season, sep = "|")
    tip <- sprintf("Stuff %s, Location %s, Pitching %s",
                   fmt_num(r$stuff, 0), fmt_num(r$location, 0), fmt_num(r$pitching, 0))
    shiny::tags$button(type = "button", class = paste("pv-pill", if (r$level == level && r$season == season) "active"),
                title = tip, `data-key` = key,
                onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", input_id, key),
                r$season,
                if (r$level != "MLB") shiny::span(class = "pv-pill-lvl", LEVEL_LABELS[[r$level]]),
                if (!is.na(r$pitching)) shiny::span(class = "pv-pill-grade", style = sprintf("background:%s;", grade_color(r$pitching)),
                                             round(r$pitching)))
  }
  shiny::div(class = "pv-pills", shiny::span(class = "pv-pills-label", "Seasons"), lapply(seq_len(nrow(lines)), pill))
}

# Grades page layout
gradesUI <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("header")), uiOutput(ns("empty")),
          bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, DT::DTOutput(ns("table"), height = "auto"))),
          p(class = "pv-note", "Grades are 20-80 (50 is average) with 90% ranges. Horizontal break and release side are from the pitcher's view. Expected rates come from the component models."))
}

# Scouting card header and grades table
gradesServer <- function(id, outputs, sel, display) {
  moduleServer(id, function(input, output, session) {
    output$header <- renderUI({
      s <- sel$state()
      p <- outputs$pitchers[outputs$pitchers$pitcher == s$pitcher & outputs$pitchers$level == s$level &
                              outputs$pitchers$season == s$season, ]
      if (nrow(p) == 0) return(NULL)
      ip <- if (is.na(p$ip_outs)) "–" else sprintf("%d.%d", p$ip_outs %/% 3, p$ip_outs %% 3)
      hand <- c(R = "Right-handed", L = "Left-handed")[[p$throws]] %||% ""
      overall <- outputs$grades[outputs$grades$pitcher == s$pitcher & outputs$grades$level == s$level &
                                  outputs$grades$season == s$season & outputs$grades$half == "full" &
                                  outputs$grades$pitch_type == "ALL", ]
      tile <- function(facet, label) {
        g <- overall[overall$facet == facet, ]
        if (nrow(g) == 0 || is.na(g$grade_global)) return(NULL)
        div(class = "pv-grade", style = sprintf("background:%s;", grade_color(g$grade_global)),
            div(class = "pv-num", round(g$grade_global)),
            div(class = "pv-range", sprintf("%d–%d", round(g$grade_lo90), round(g$grade_hi90))),
            div(class = "pv-tool", label))
      }
      div(class = "pv-card",
          tags$img(src = sprintf(HEADSHOT_URL, as.integer(p$pitcher)), alt = ""),
          div(h2(class = "pv-name", ifelse(is.na(p$name), paste("Pitcher", p$pitcher), p$name)),
              div(class = "pv-sub", sprintf("%s, %s pitcher, %s %d", p$team_abbrev, tolower(hand),
                                            LEVEL_LABELS[[s$level]], s$season)),
              div(class = "pv-line", sprintf("%s games (%s starts), %s innings, %s pitches", fmt_num(p$g, 0),
                                             fmt_num(p$gs, 0), ip, format(p$pitches, big.mark = ",")))),
          div(class = "pv-grades", tile("stuff", "Stuff"), tile("location", "Location"), tile("pitching", "Pitching")),
          season_pills(career_lines(outputs$pitchers, outputs$grades, s$pitcher), s$level, s$season, session$ns("season_pick")),
          projection_card(outputs$projections, outputs$luck, s$pitcher, s$level, s$season))
    })
    # Season pill clicked: same pitcher, that season
    observeEvent(input$season_pick, {
      k <- strsplit(input$season_pick, "|", fixed = TRUE)[[1]]
      sel$select(k[1], as.integer(k[2]), sel$state()$pitcher)
    })
    tbl <- reactive({
      s <- sel$state()
      grades_table(outputs$grades, outputs$components, s$pitcher, s$level, s$season,
                   pitch_summary(display$rows(), s$side), s$scale, s$min_pitches)
    })
    output$empty <- renderUI({
      t <- tbl()
      if (is.null(t) || nrow(t) == 0) div(class = "alert alert-info", "No grades for this pitcher in this season yet.")
    })
    output$table <- DT::renderDT({
      t <- tbl(); req(t)
      shown <- t %>% dplyr::transmute(
        Type = ifelse(pitch_type == "ALL", "Overall", pitch_type), Pitches = fmt_num(n, 0),
        Usage = fmt_pct(usage), `vs L` = fmt_pct(usage_l), `vs R` = fmt_pct(usage_r),
        Stuff, Location, Pitching, Velo = fmt_num(velo), IVB = fmt_num(ivb), HB = fmt_num(hb),
        Spin = fmt_num(spin, 0), Ext = fmt_num(ext), VAA = fmt_num(vaa), `Arm°` = fmt_num(arm_angle, 0),
        `Rel Ht` = fmt_num(rel_z, 2), `Rel Side` = fmt_num(rel_x, 2),
        `Swing%` = swing, `Whiff/Sw` = whiff_swing, `Chase%` = chase, `CStr/Take` = cstrike_take,
        `xwOBAcon` = xwoba_con, `GB%` = gb,
        sv = stuff_value, lv = location_value, pv = pitching_value)
      hidden <- which(names(shown) %in% c("sv", "lv", "pv")) - 1
      dt <- DT::datatable(shown, rownames = FALSE, selection = "none", class = "compact hover", fillContainer = FALSE,
                          options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE,
                                         columnDefs = list(list(visible = FALSE, targets = hidden),
                                                           list(className = "pv-gradecell", targets = which(names(shown) %in% c("Stuff", "Location", "Pitching")) - 1))))
      for (pair in list(c("Stuff", "sv"), c("Location", "lv"), c("Pitching", "pv")))
        dt <- DT::formatStyle(dt, pair[1], valueColumns = pair[2],
                              backgroundColor = grade_fill(), color = grade_text_color())
      dt
    })
  })
}

# Movement page

# Points for a correlated-normal ellipse
make_ellipse <- function(cx, cy, sx, sy, rho, scale = 1.5, n = 72) {
  t <- seq(0, 2 * pi, length.out = n)
  rho <- max(min(rho, 0.99), -0.99)
  a <- sx * scale * cos(t)
  b <- sy * scale * (rho * cos(t) + sqrt(1 - rho^2) * sin(t))
  tibble::tibble(x = cx + a, y = cy + b)
}

# League percentile of xRV (higher is better for the pitcher)
xrv_pctl <- function(xrv, q) {
  if (is.null(q) || length(q) < 2) return(rep(NA_real_, length(xrv)))
  p <- stats::approx(q, seq(0, 100, 1), xout = xrv, rule = 2, ties = "ordered")$y
  round(100 - p)
}

# Per-pitch hover text
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

# League arm-angle decile closest to this pitcher's arm angle
pitcher_arm_decile <- function(ref, hand, arm) {
  r <- dplyr::distinct(ref[ref$p_throws == hand, ], arm_decile, arm_lo, arm_hi)
  if (nrow(r) == 0 || is.na(arm)) return(NA_integer_)
  r$arm_decile[which.min(pmax(r$arm_lo - arm, arm - r$arm_hi, 0))]
}

# Flip Statcast's catcher's view to the pitcher's view
to_pitcher_view <- function(d) { d$hb <- -d$hb; d$rel_x <- -d$rel_x; d }

# Movement scatter with league ellipses, shared with the editor
movement_plot <- function(d, movement_ref, pctl, level, season, corrected_uids = character(),
                          source = "movement", dragmode = "zoom") {
  d <- to_pitcher_view(d)
  hand <- d$p_throws[1]
  arm <- mean(d$arm_angle, na.rm = TRUE)
  ref <- movement_ref[movement_ref$level == level & movement_ref$season == season, ]
  ref$mean_hb <- -ref$mean_hb; ref$rho <- -ref$rho
  dec <- pitcher_arm_decile(ref, hand, arm)
  p <- plotly::plot_ly(source = source)
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
  plotly_style(p) %>% plotly::layout(dragmode = dragmode,
                 xaxis = list(title = "Horizontal break (in, pitcher's view)", range = c(-28, 28), zeroline = FALSE,
                              constrain = "domain", dtick = 10),
                 yaxis = list(title = "Induced vertical break (in)", range = c(-28, 28), zeroline = FALSE,
                              scaleanchor = "x", constrain = "domain", dtick = 10),
                 shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = -28, y1 = 28, line = list(color = LINE, width = 1)),
                               list(type = "line", x0 = -28, x1 = 28, y0 = 0, y1 = 0, line = list(color = LINE, width = 1)))) %>%
    plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("autoScale2d", "toggleSpikelines")) %>%
    plotly::event_register("plotly_selected")
}

# Keep one batter side, or all
side_filter <- function(d, side) if (is.null(d) || !(side %in% c("L", "R"))) d else d[d$stand == side, ]

# Pitches retagged in the file or this session
corrected_uids <- function(d, edits) {
  if (is.null(d)) return(character())
  c(d$pitch_uid[!is.na(d$pitch_type_savant) & d$pitch_type != d$pitch_type_savant],
    if (!is.null(edits)) edits$pitch_uid else character())
}

UNAVAILABLE <- "Pitch data unavailable right now. Grades, Leaderboard and Scorecard still work."

# Movement over the years: one frame per season, played with a button or slider
movement_animation <- function(d, pctl, level) {
  d <- to_pitcher_view(d)
  seasons <- sort(unique(d$season))
  d$text <- ""
  for (y in seasons) {
    i <- d$season == y
    d$text[i] <- paste0("<b>", y, "</b><br>", pctl_text(d[i, ], pctl, level, y))
  }
  types <- intersect(PITCH_LABELS, unique(d$pitch_type))
  avg <- d %>% dplyr::group_by(season, pitch_type) %>%
    dplyr::summarise(n = dplyr::n(), hb = mean(hb, na.rm = TRUE), ivb = mean(ivb, na.rm = TRUE),
                     velo = mean(velo, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(season) %>%
    dplyr::mutate(text = sprintf("<b>%s, %d average</b><br>%.1f mph · IVB %.1f · HB %.1f<br>%s pitches",
                                 pitch_type, season, velo, ivb, hb, format(n, big.mark = ",")))
  arm <- d %>% dplyr::group_by(season) %>%
    dplyr::summarise(arm = mean(arm_angle, na.rm = TRUE), side = sign(mean(rel_x, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::mutate(side = ifelse(side == 0 | is.na(side), 1, side))
  # One season's points, averages and arm line, empty where a pitch wasn't thrown
  pts <- function(y, ty) { i <- d$season == y & d$pitch_type == ty; list(x = I(d$hb[i]), y = I(d$ivb[i]), text = I(d$text[i]), ids = I(d$pitch_uid[i])) }
  mean_pt <- function(y, ty) { a <- avg[avg$season == y & avg$pitch_type == ty, ]; list(x = I(a$hb), y = I(a$ivb), text = I(a$text), ids = I(rep(ty, nrow(a)))) }
  arm_line <- function(y) {
    a <- arm[arm$season == y, ]
    if (nrow(a) == 0 || is.na(a$arm)) return(list(x = I(numeric()), y = I(numeric()), text = I(character())))
    list(x = I(c(0, a$side * 22 * cos(a$arm * pi / 180))), y = I(c(0, 22 * sin(a$arm * pi / 180))),
         text = I(rep(sprintf("arm angle %.0f°", a$arm), 2)))
  }
  first <- seasons[1]
  p <- plotly::plot_ly()
  # Faint path through each type's season averages, fixed in every frame
  for (ty in types) {
    a <- avg[avg$pitch_type == ty, ]
    p <- plotly::add_trace(p, x = a$hb, y = a$ivb, text = a$text, type = "scatter", mode = "lines+markers",
                           name = paste(ty, "path"), hoverinfo = "text", showlegend = FALSE,
                           line = list(color = PITCH_COLORS[[ty]], width = 1, dash = "dot"),
                           marker = list(color = PITCH_COLORS[[ty]], size = 5, opacity = 0.5))
  }
  # Animated traces start at the first season
  for (ty in types) {
    s <- pts(first, ty)
    p <- plotly::add_trace(p, x = s$x, y = s$y, text = s$text, ids = s$ids, type = "scatter", mode = "markers",
                           name = ty, hoverinfo = "text", marker = list(color = PITCH_COLORS[[ty]], size = 7, opacity = 0.45))
  }
  for (ty in types) {
    s <- mean_pt(first, ty)
    p <- plotly::add_trace(p, x = s$x, y = s$y, text = s$text, ids = s$ids, type = "scatter", mode = "markers",
                           name = paste(ty, "avg"), hoverinfo = "text", showlegend = FALSE,
                           marker = list(color = PITCH_COLORS[[ty]], size = 20, line = list(color = INK, width = 2)))
  }
  s <- arm_line(first)
  p <- plotly::add_trace(p, x = s$x, y = s$y, text = s$text, type = "scatter", mode = "lines", name = "arm angle",
                         hoverinfo = "text", line = list(color = "#2e8b57", dash = "dash", width = 2))
  p <- plotly_style(p) %>% plotly::layout(
    xaxis = list(title = "Horizontal break (in, pitcher's view)", range = c(-28, 28), zeroline = FALSE, constrain = "domain", dtick = 10),
    yaxis = list(title = "Induced vertical break (in)", range = c(-28, 28), zeroline = FALSE, scaleanchor = "x", constrain = "domain", dtick = 10),
    shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = -28, y1 = 28, line = list(color = LINE, width = 1)),
                  list(type = "line", x0 = -28, x1 = 28, y0 = 0, y1 = 0, line = list(color = LINE, width = 1)))) %>%
    plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("autoScale2d", "toggleSpikelines", "lasso2d", "select2d"))
  b <- plotly::plotly_build(p)
  # Frames update the animated traces by index (0-based), every type in every season
  n <- length(types)
  idx <- as.list(seq(n, 3 * n))
  b$x$frames <- lapply(seasons, function(y) {
    list(name = as.character(y), traces = idx,
         data = c(lapply(types, function(ty) pts(y, ty)), lapply(types, function(ty) mean_pt(y, ty)), list(arm_line(y))))
  })
  # Play and pause buttons plus a season slider
  step <- function(frames, ms) list(frames, list(mode = "immediate", fromcurrent = TRUE, frame = list(duration = ms, redraw = FALSE),
                                                 transition = list(duration = 700, easing = "cubic-in-out")))
  b$x$layout$updatemenus <- list(list(
    type = "buttons", showactive = FALSE, direction = "left", x = 0, y = -0.08, xanchor = "left", yanchor = "top",
    buttons = list(list(label = "Play", method = "animate", args = step(NULL, 1400)),
                   list(label = "Pause", method = "animate",
                        args = list(list(NULL), list(mode = "immediate", frame = list(duration = 0, redraw = FALSE), transition = list(duration = 0)))))))
  b$x$layout$sliders <- list(list(
    active = 0, x = 0.16, len = 0.84, y = -0.04, pad = list(t = 30),
    currentvalue = list(prefix = "Season ", font = list(size = 18, color = INK)),
    steps = lapply(seasons, function(y) list(label = as.character(y), method = "animate", args = step(list(as.character(y)), 700)))))
  b
}

# Movement page layout
movementUI <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("message")),
          radioButtons(ns("view"), NULL, c("This season" = "season", "All seasons (animated)" = "all"), inline = TRUE),
          bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, plotly::plotlyOutput(ns("plot"), height = "680px"))),
          p(class = "pv-note", "Pitcher's view. Dotted ovals show where league pitches of the same type land for pitchers with a similar arm angle. All seasons plays his movement year by year at the selected level: big dots are season averages, dotted lines trace their path. Drag to zoom; double-click to reset. To retag pitches, use the Pitch-Type Editor."))
}

# Movement plot for the selected pitcher, one season or all of them
movementServer <- function(id, outputs, sel, display, cfg, cache) {
  moduleServer(id, function(input, output, session) {
    output$message <- renderUI(if (is.null(display$rows())) div(class = "alert alert-warning", UNAVAILABLE))
    output$plot <- plotly::renderPlotly({
      s <- sel$state()
      if (identical(input$view, "all")) {
        p <- outputs$pitchers
        seasons <- sort(unique(p$season[p$pitcher == s$pitcher & p$level == s$level]))
        d <- shiny::withProgress(message = "Loading every season", career_display(cfg, s$level, seasons, s$pitcher, cache))
        d <- side_filter(d, s$side); req(d, nrow(d) > 0)
        return(movement_animation(d, outputs$pctl, s$level))
      }
      d <- side_filter(display$rows(), s$side); req(d, nrow(d) > 0)
      movement_plot(d, outputs$movement_ref, outputs$pctl, s$level, s$season, corrected_uids(d, display$edits()))
    })
  })
}

# Location page

# Pitch density by type and batter side from the pitcher's view
location_plot <- function(d) {
  d <- d %>% dplyr::mutate(x = -plate_x, stand = factor(ifelse(stand == "L", "vs LHB", "vs RHB"), c("vs LHB", "vs RHB")),
                           pitch_type = factor(pitch_type, intersect(PITCH_LABELS, unique(pitch_type))))
  counts <- dplyr::count(d, stand, pitch_type)
  dense <- dplyr::semi_join(d, counts[counts$n >= 5, ], by = c("stand", "pitch_type"))
  sparse <- dplyr::semi_join(d, counts[counts$n < 5, ], by = c("stand", "pitch_type"))
  top <- mean(d$sz_top, na.rm = TRUE); bot <- mean(d$sz_bot, na.rm = TRUE)
  ggplot2::ggplot(d, ggplot2::aes(x, plate_z)) +
    ggplot2::stat_density_2d(data = dense, ggplot2::aes(fill = ggplot2::after_stat(level)), geom = "polygon",
                             alpha = 0.6, contour_var = "ndensity", bins = 8) +
    ggplot2::geom_point(data = sparse, color = "#333333", size = 1.5) +
    ggplot2::annotate("rect", xmin = -PLATE_HALF_WIDTH, xmax = PLATE_HALF_WIDTH, ymin = bot, ymax = top,
                      fill = NA, color = "black") +
    ggplot2::scale_fill_gradient(low = "#fde0dd", high = "#c51b8a", guide = "none") +
    ggplot2::coord_fixed(xlim = c(-2, 2), ylim = c(0, 5)) +
    ggplot2::facet_grid(stand ~ pitch_type, drop = FALSE) +
    ggplot2::labs(x = "Horizontal location (ft, pitcher's view)", y = "Height (ft)") +
    theme_pv()
}

# Location page layout
locationUI <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("message")), bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, plotOutput(ns("plot"), height = "560px"))))
}

# Location plot for the selected pitcher
locationServer <- function(id, sel, display) {
  moduleServer(id, function(input, output, session) {
    output$message <- renderUI(if (is.null(display$rows())) div(class = "alert alert-warning", UNAVAILABLE))
    output$plot <- renderPlot({
      d <- display$rows(); req(d, nrow(d) > 0)
      location_plot(d)
    })
  })
}

# Leaderboard

# Overall grades plus Stuff and Location by pitch type for every pitcher
leaderboard_table <- function(grades, pitchers, level, season, team, min_pitches, scale) {
  lv <- if (identical(level, "Both")) c("MLB", "AAA") else level
  p <- pitchers[pitchers$level %in% lv & pitchers$season == season & pitchers$pitches >= min_pitches, ]
  if (!identical(team, "All")) p <- p[vapply(strsplit(p$teams, ",", fixed = TRUE), function(t) team %in% t, TRUE), ]
  g <- grades[grades$level %in% lv & grades$season == season & grades$half == "full", ]
  if (nrow(p) == 0 || nrow(g) == 0) return(tibble::tibble(pitcher = integer(), name = character(), team = character(),
                                                           level = character(), pitches = integer(), stuff = numeric(),
                                                           location = numeric(), pitching = numeric()))
  g$value <- if (scale == "type") dplyr::coalesce(g$grade_type, g$grade_global) else g$grade_global
  overall <- g[g$pitch_type == "ALL", ] %>%
    dplyr::select(level, pitcher, facet, grade_global) %>%
    tidyr::pivot_wider(names_from = facet, values_from = grade_global)
  types <- g[g$pitch_type != "ALL" & g$facet %in% c("stuff", "location"), ] %>%
    dplyr::mutate(col = paste0(pitch_type, ifelse(facet == "stuff", "_S", "_L"))) %>%
    dplyr::select(level, pitcher, col, value) %>%
    tidyr::pivot_wider(names_from = col, values_from = value)
  type_cols <- unlist(lapply(PITCH_LABELS, function(t) paste0(t, c("_S", "_L"))))
  type_cols <- intersect(type_cols, names(types))
  p %>% dplyr::select(level, pitcher, name, team = team_abbrev, pitches) %>%
    dplyr::inner_join(overall, by = c("level", "pitcher")) %>%
    dplyr::left_join(types, by = c("level", "pitcher")) %>%
    dplyr::select(pitcher, name, team, level, pitches, dplyr::any_of(c("stuff", "location", "pitching")), dplyr::all_of(type_cols)) %>%
    dplyr::arrange(dplyr::desc(pitching))
}

# Leaderboard filters and table
leaderboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      selectInput(ns("level"), "Level", c("MLB" = "MLB", "Triple-A" = "AAA", "Both" = "Both")),
      selectInput(ns("season"), "Season", choices = NULL),
      selectInput(ns("team"), "Team", choices = "All"),
      sliderInput(ns("min_pitches"), "Min total pitches", min = 0, max = 2000, value = 0, step = 50, ticks = FALSE)),
    p(class = "pv-note", "Overall grades and Stuff (S) and Location (L) by pitch type. Click a row to open that pitcher."),
    bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, DT::DTOutput(ns("table"), height = "auto"))))
}

# Row click opens that pitcher on the Grades page
leaderboardServer <- function(id, outputs, sel, parent = NULL) {
  moduleServer(id, function(input, output, session) {
    observe({
      lv <- if (identical(input$level, "Both")) c("MLB", "AAA") else input$level
      s <- sort(unique(outputs$pitchers$season[outputs$pitchers$level %in% lv]), decreasing = TRUE)
      updateSelectInput(session, "season", choices = s, selected = s[1])
    }) %>% bindEvent(input$level)
    observe({
      req(input$season)
      lv <- if (identical(input$level, "Both")) c("MLB", "AAA") else input$level
      p <- outputs$pitchers[outputs$pitchers$level %in% lv & outputs$pitchers$season == as.integer(input$season), ]
      updateSelectInput(session, "team", choices = c("All", sort(unique(unlist(strsplit(p$teams, ","))))))
    }) %>% bindEvent(input$level, input$season)
    # Track only the scale so a pitcher change doesn't reset the table's sort and page
    scale <- reactiveVal(NULL)
    observe({
      v <- sel$state()$scale %||% "global"  # waits until the sidebar is ready
      if (!identical(v, isolate(scale()))) scale(v)
    })
    board <- reactive({
      req(input$season, input$team, scale())
      leaderboard_table(outputs$grades, outputs$pitchers, input$level, as.integer(input$season), input$team,
                        input$min_pitches, scale())
    })
    output$table <- DT::renderDT({
      t <- board()
      shown <- t %>% dplyr::select(-pitcher) %>%
        dplyr::rename(Name = name, Team = team, Level = level, Pitches = pitches,
                      Stuff = stuff, Location = location, Pitching = pitching)
      grade_cols <- setdiff(names(shown), c("Name", "Team", "Level", "Pitches"))
      DT::datatable(shown, rownames = FALSE, selection = "single", class = "compact hover", fillContainer = FALSE,
                    options = list(pageLength = 25, scrollX = TRUE, order = list(list(6, "desc")),
                                   columnDefs = list(list(className = "pv-name-cell", targets = 0),
                                                     list(className = "pv-gradecell", targets = which(names(shown) %in% grade_cols) - 1)))) %>%
        DT::formatRound(grade_cols, 0) %>%
        DT::formatStyle(grade_cols, backgroundColor = grade_fill(), color = grade_text_color())
    })
    observeEvent(input$table_rows_selected, {
      t <- board(); r <- t[input$table_rows_selected, ]
      sel$select(r$level, as.integer(input$season), r$pitcher)
      if (!is.null(parent)) bslib::nav_select("main_nav", "Grades", session = parent)
    })
  })
}

# Stuff map

# Average movement per pitcher for one pitch type, with arm-side break and Stuff grade
stuff_map_data <- function(profiles, grades, pitchers, level, season, pitch_type, min_pitches) {
  p <- profiles[profiles$level == level & profiles$season == season & profiles$pitch_type == pitch_type &
                  profiles$n >= min_pitches, ]
  if (nrow(p) == 0) return(tibble::tibble())
  g <- grades[grades$level == level & grades$season == season & grades$half == "full" &
                grades$facet == "stuff" & grades$pitch_type == pitch_type, c("pitcher", "grade_type")]
  who <- pitchers[pitchers$level == level & pitchers$season == season, c("pitcher", "name", "team_abbrev")]
  p %>%
    dplyr::mutate(arm_side = hb * ifelse(p_throws == "L", 1, -1)) %>%  # Statcast catcher's view
    dplyr::left_join(dplyr::rename(g, grade = grade_type), by = "pitcher") %>%
    dplyr::left_join(dplyr::rename(who, team = team_abbrev), by = "pitcher") %>%
    dplyr::filter(!is.na(grade))
}

# Stuff Map page layout
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

# Stuff Map plot with the selected pitcher highlighted
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

# Command and tunneling

# Zone shares per pitcher next to the overall Location grade
command_table <- function(command, grades, pitchers, level, season, team, min_pitches) {
  p <- pitchers[pitchers$level == level & pitchers$season == season & pitchers$pitches >= min_pitches, ]
  if (!identical(team, "All")) p <- p[vapply(strsplit(p$teams, ",", fixed = TRUE), function(t) team %in% t, TRUE), ]
  c0 <- command[command$level == level & command$season == season & command$pitch_type == "ALL", ]
  loc <- grades[grades$level == level & grades$season == season & grades$half == "full" & grades$facet == "location" &
                  grades$pitch_type == "ALL", c("pitcher", "grade_global")]
  p %>% dplyr::select(pitcher, name, team = team_abbrev, pitches) %>%
    dplyr::inner_join(dplyr::select(c0, pitcher, zone, heart, shadow, chase, waste), by = "pitcher") %>%
    dplyr::mutate(dplyr::across(c(zone, heart, shadow, chase, waste), ~ 100 * .x)) %>%
    dplyr::left_join(dplyr::rename(loc, location = grade_global), by = "pitcher") %>%
    dplyr::arrange(dplyr::desc(shadow))
}

# Gap between each pair of pitch types at the tunnel point and plate, per batter side
tunnel_pairs <- function(tunnel, pitcher, level, season, side = "All") {
  empty <- tibble::tibble(vs = character(), pair = character(), tunnel_gap_in = numeric(),
                          plate_gap_in = numeric(), split_in = numeric())
  t <- tunnel[tunnel$pitcher == pitcher & tunnel$level == level & tunnel$season == season, ]
  if (side %in% c("L", "R")) t <- t[t$stand == side, ]
  out <- lapply(split(t, t$stand), function(ts) {
    ts <- ts[order(match(ts$pitch_type, PITCH_LABELS)), ]
    if (nrow(ts) < 2) return(NULL)
    idx <- utils::combn(nrow(ts), 2)
    a <- ts[idx[1, ], ]; b <- ts[idx[2, ], ]
    tibble::tibble(vs = paste0(ts$stand[1], "HB"), pair = paste(a$pitch_type, "/", b$pitch_type),
                   tunnel_gap_in = 12 * sqrt((a$x_tun - b$x_tun)^2 + (a$z_tun - b$z_tun)^2),
                   plate_gap_in = 12 * sqrt((a$x_plate - b$x_plate)^2 + (a$z_plate - b$z_plate)^2))
  })
  r <- dplyr::bind_rows(c(list(empty), unname(out)))
  dplyr::mutate(r, split_in = plate_gap_in - tunnel_gap_in)
}

# Command & Tunneling page layout
commandUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2(class = "pv-title", "Command"),
    bslib::layout_columns(col_widths = c(4, 8),
      sliderInput(ns("min_pitches"), "Min total pitches", min = 0, max = 2000, value = 0, step = 50, ticks = FALSE),
      uiOutput(ns("scope"))),
    p(class = "pv-note", "Share of pitches by zone, sized to each batter's own strike zone: heart (middle two-thirds), shadow (the edges, inside and out), chase, and waste. Location is the tested 20-80 grade."),
    bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, DT::DTOutput(ns("table"), height = "auto"))),
    h2(class = "pv-title", "Tunneling"),
    uiOutput(ns("tunnel_head")),
    uiOutput(ns("tunnel_message")),
    bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, DT::DTOutput(ns("tunnels"), height = "auto"))),
    p(class = "pv-note", sprintf("Average gap between two pitch types %.1f ft in front of the plate, about when a hitter must decide, and at the plate, compared within each batter side (the sidebar's batter side narrows it). A small early gap with a large late one means the pitches look alike longer.", TUNNEL_FT_APP)))
}

TUNNEL_FT_APP <- 23.8  # feet in front of the plate

# Command table and the selected pitcher's tunnel pairs
commandServer <- function(id, outputs, sel) {
  moduleServer(id, function(input, output, session) {
    # Page follows the sidebar's level, season and team
    output$scope <- renderUI({
      s <- sel$state()
      p(class = "pv-note", sprintf("%s %d, %s (set in the sidebar).", LEVEL_LABELS[[s$level]], s$season,
                                   if (identical(s$team %||% "All", "All")) "all teams" else s$team))
    })
    board <- reactive({
      s <- sel$state()
      command_table(outputs$command, outputs$grades, outputs$pitchers, s$level, s$season,
                    s$team %||% "All", input$min_pitches %||% 0)
    })
    output$table <- DT::renderDT({
      t <- board()
      shown <- t %>% dplyr::transmute(Name = name, Team = team, Pitches = pitches, `Zone%` = zone, `Heart%` = heart,
                                      `Shadow%` = shadow, `Chase%` = chase, `Waste%` = waste, Location = location)
      DT::datatable(shown, rownames = FALSE, selection = "none", class = "compact hover", fillContainer = FALSE,
                    options = list(pageLength = 25, scrollX = TRUE, order = list(list(5, "desc")),
                                   columnDefs = list(list(className = "pv-name-cell", targets = 0),
                                                     list(className = "pv-gradecell", targets = 8)))) %>%
        DT::formatRound(c("Zone%", "Heart%", "Shadow%", "Chase%", "Waste%"), 1) %>%
        DT::formatRound("Location", 0) %>%
        DT::formatStyle("Location", backgroundColor = grade_fill(), color = grade_text_color())
    })
    pairs <- reactive({
      s <- sel$state()
      tunnel_pairs(outputs$tunnel, s$pitcher, s$level, s$season, s$side %||% "All")
    })
    output$tunnel_head <- renderUI(pitcher_heading(outputs$pitchers, sel$state()))
    output$tunnel_message <- renderUI({
      if (nrow(pairs()) > 0) return(NULL)
      s <- sel$state()
      has_any <- any(outputs$tunnel$pitcher == s$pitcher & outputs$tunnel$level == s$level & outputs$tunnel$season == s$season)
      div(class = "alert alert-info", if (has_any)
        "This pitcher has only one pitch type with 20 or more pitches to these batters, so there is nothing to compare."
        else "No tunnel data for this pitcher: no pitch type reached 20 pitches with full tracking.")
    })
    output$tunnels <- DT::renderDT({
      t <- pairs(); req(nrow(t) > 0)
      DT::datatable(dplyr::transmute(t, Batters = vs, Pair = pair, `Gap at decision (in)` = tunnel_gap_in,
                                     `Gap at plate (in)` = plate_gap_in, `Late split (in)` = split_in),
                    rownames = FALSE, selection = "none", class = "compact hover", fillContainer = FALSE,
                    options = list(dom = "t", paging = FALSE, order = list(list(4, "desc")))) %>%
        DT::formatRound(3:5, 1)
    })
  })
}

# Rolling
ROLLING_FACETS <- c("Stuff" = "stuff", "Location" = "location", "Pitching" = "pitching", "Velocity" = "velo")

# Rolling 100-pitch grade per type, shrunk toward the type average like the season grades
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

# Rolling page layout
rollingUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("head")),
    radioButtons(ns("facet"), NULL, ROLLING_FACETS, inline = TRUE),
    uiOutput(ns("message")),
    bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, plotly::plotlyOutput(ns("plot"), height = "480px"))),
    p(class = "pv-note", "Each point is the last 100 pitches of that type up to that game (shown from 50 pitches on), pulled toward average in proportion to how noisy that window is. Location and Pitching move less than Stuff because single pitches vary more. Dashed lines are the full-season grades."))
}

# Rolling lines with the season grade as a dashed reference
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

# Scorecard page

# Published scorecard markdown as HTML
scorecard_html <- function(md_path) {
  if (!file.exists(md_path)) return("<p>The scorecard has not been published yet.</p>")
  md <- readLines(md_path, warn = FALSE)
  html <- commonmark::markdown_html(paste(md, collapse = "\n"), extensions = TRUE)
  gsub('src="scorecard_chart.png"', 'src="pv_scorecard/scorecard_chart.png" style="max-width:100%"', html, fixed = TRUE)
}

# Scorecard page layout
scorecardUI <- function(id) uiOutput(NS(id, "body"))

# Serve the scorecard chart and render the page
scorecardServer <- function(id, outputs) {
  moduleServer(id, function(input, output, session) {
    if (file.exists(outputs$scorecard$chart)) addResourcePath("pv_scorecard", dirname(outputs$scorecard$chart))
    output$body <- renderUI(div(class = "pv-scorecard", HTML(scorecard_html(outputs$scorecard$md))))
  })
}
