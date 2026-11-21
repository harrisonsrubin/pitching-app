# Grades page: the selected pitcher's grades by pitch type, with 80% ranges, pitch
# metrics and expected component rates.

HEADSHOT_URL <- "https://img.mlbstatic.com/mlb-photos/image/upload/w_213,d_people:generic:headshot:silo:current.png,q_100/v1/people/%d/headshot/silo/current"

fmt_num <- function(x, digits = 1) ifelse(is.na(x) | is.nan(x), "", formatC(x, digits = digits, format = "f"))
fmt_pct <- function(x) ifelse(is.na(x) | is.nan(x), "", sprintf("%.1f%%", 100 * x))

grade_cell <- function(value, lo, hi) ifelse(is.na(value), "", sprintf("%d (%d–%d)", round(value), round(lo), round(hi)))

facet_grade <- function(g, facet, scale) {
  f <- g[g$facet == facet, ]
  use_type <- scale == "type" & !is.na(f$grade_type)
  value <- ifelse(use_type, f$grade_type, f$grade_global)
  shift <- ifelse(use_type, f$grade_type - f$grade_global, 0)
  tibble::tibble(pitch_type = f$pitch_type, n_graded = f$n, value = value,
                 cell = grade_cell(value, f$grade_lo80 + shift, f$grade_hi80 + shift))
}

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

gradesUI <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("header")), uiOutput(ns("empty")),
          bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, DT::DTOutput(ns("table"), height = "auto"))),
          p(class = "pv-note", "Grades are 20-80 (50 is average) with 80% ranges. Horizontal break and release side are from the pitcher's view. Expected rates come from the component models."))
}

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
            div(class = "pv-range", sprintf("%d–%d", round(g$grade_lo80), round(g$grade_hi80))),
            div(class = "pv-tool", label))
      }
      div(class = "pv-card",
          tags$img(src = sprintf(HEADSHOT_URL, as.integer(p$pitcher)), alt = ""),
          div(h2(class = "pv-name", ifelse(is.na(p$name), paste("Pitcher", p$pitcher), p$name)),
              div(class = "pv-sub", sprintf("%s, %s pitcher, %s %d", p$team_abbrev, tolower(hand),
                                            LEVEL_LABELS[[s$level]], s$season)),
              div(class = "pv-line", sprintf("%s games (%s starts), %s innings, %s pitches", fmt_num(p$g, 0),
                                             fmt_num(p$gs, 0), ip, format(p$pitches, big.mark = ",")))),
          div(class = "pv-grades", tile("stuff", "Stuff"), tile("location", "Location"), tile("pitching", "Pitching")))
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
