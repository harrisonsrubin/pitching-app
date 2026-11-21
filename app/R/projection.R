# Projection pages: the Grades card block, Player Projections and League Projections.

# ==== projection ======================================================================

# Projection (P3a): the true-talent projection on the Grades card, and a Player Projections page
# with the 1-5 season outlook, component rates, how much the pitch model versus results
# moved the estimate, and the luck gap. ERA and FIP are park-neutral; the outlook table
# also shows ERA in his current home park.

STEP_TIP <- "Chance his true-talent ERA next season is at least 0.75 runs better (Step forward) or worse (Step back) than today, about a full grade."
LUCK_TIP <- "Actual ERA minus the ERA his true-talent rates predict. Positive means results ran worse than his talent (unlucky)."

pitcher_projection <- function(proj, pitcher, season) {
  if (is.null(proj) || !nrow(proj)) return(NULL)
  p <- proj[proj$pitcher == pitcher, ]
  if (!nrow(p) || !identical(as.integer(season), as.integer(p$base_season[1]))) return(NULL)
  p[order(p$horizon), ]
}

signed <- function(x, digits = 2) ifelse(is.na(x), "–", sprintf(paste0("%+.", digits, "f"), x))

CHIP_RUNGS <- seq(20, 80, 5)

# The 10 chips counted onto the 20-80 grade steps (each chip to its nearest step).
chip_counts <- function(ch) {
  ch <- ch[!is.na(ch)]
  r <- pmin(pmax(round(ch / 5) * 5, 20), 80)
  stats::setNames(vapply(CHIP_RUNGS, function(g) sum(r == g), 0), as.character(CHIP_RUNGS))
}

# Bars at each grade step, height by chips (count on top, step below), in grade colors.
bet_bars <- function(ch) {
  n <- chip_counts(ch)
  if (sum(n) == 0) return(NULL)
  top <- max(n)
  shiny::div(class = "pv-bars", lapply(seq_along(CHIP_RUNGS), function(i) {
    k <- n[[i]]
    shiny::div(class = if (k > 0) "pv-bar" else "pv-bar pv-bar-empty",
               title = sprintf("Grade %d: %d chip%s", CHIP_RUNGS[i], k, if (k == 1) "" else "s"),
               shiny::span(class = "pv-bar-n", if (k > 0) k else ""),
               shiny::span(class = "pv-bar-fill", style = sprintf("height:%dpx;%s", if (k > 0) round(8 + 52 * k / top) else 3,
                                                                  if (k > 0) sprintf(" background:%s;", grade_color(CHIP_RUNGS[i])) else "")),
               shiny::span(class = "pv-bar-x", CHIP_RUNGS[i]))
  }))
}

proj_stat <- function(label, value, sub = NULL, tip = NULL) {
  shiny::div(class = "pv-stat", title = tip,
             shiny::div(class = "pv-stat-label", label),
             shiny::div(class = "pv-stat-value", value),
             if (!is.null(sub)) shiny::div(class = "pv-stat-sub", sub))
}

projection_card <- function(proj, luck, pitcher, level, season) {
  p <- pitcher_projection(proj, pitcher, season)
  if (is.null(p)) return(NULL)
  h1 <- p[p$horizon == 1, ]
  if (!nrow(h1)) return(NULL)
  l <- if (!is.null(luck) && nrow(luck)) luck[luck$pitcher == pitcher & luck$season == season, ] else NULL
  if (!is.null(l) && nrow(l) > 1) l <- l[l$level == level, ][1, ]
  luck_row <- if (!is.null(l) && nrow(l) == 1 && !is.na(l$gap)) {
    piece <- function(lab, v) if (!is.na(v)) shiny::div(class = "pv-luck-piece", shiny::span(lab), shiny::tags$b(class = if (v > 0) "pv-up" else "pv-down", signed(v)))
    shiny::div(class = "pv-luck-row", title = LUCK_TIP,
               shiny::div(class = "pv-luck-title", sprintf("%d luck", season)),
               piece("Gap (ERA)", l$gap), piece("BABIP", l$babip), piece("Strand", l$strand), piece("HR/FB", l$hr_fb))
  }
  shiny::div(class = "pv-proj",
    shiny::div(class = "pv-proj-title", sprintf("%d projection", h1$target_season),
               if (identical(h1$basis, "MLB-equivalent")) shiny::span(class = "pv-basis", "MLB-equivalent")),
    shiny::div(class = "pv-proj-grid",
      shiny::div(class = "pv-bet",
        shiny::div(class = "pv-bet-head",
                   shiny::span(class = "pv-stat-label", "True talent"),
                   shiny::span(class = "pv-bet-grade", round(h1$tt_grade)),
                   shiny::span(class = "pv-stat-sub", sprintf("(%d–%d)", round(h1$tt_lo), round(h1$tt_hi)))),
        bet_bars(unlist(h1[paste0("chip_", 1:10)])),
        shiny::div(class = "pv-bet-note", "10 chips: where his true talent next season is likely to land")),
      shiny::div(class = "pv-stats",
        proj_stat("ERA", sprintf("%.2f", h1$era), sprintf("%.2f–%.2f", h1$era_lo, h1$era_hi)),
        proj_stat("FIP", sprintf("%.2f", h1$fip), sprintf("%.2f–%.2f", h1$fip_lo, h1$fip_hi)),
        proj_stat("Step forward", sprintf("%.0f%%", 100 * h1$step_forward), "0.75+ ERA better", STEP_TIP),
        proj_stat("Step back", sprintf("%.0f%%", 100 * h1$step_back), "0.75+ ERA worse", STEP_TIP))),
    luck_row)
}

outlook_table <- function(p) {
  rng <- function(x, lo, hi) sprintf("%.2f (%.2f–%.2f)", x, lo, hi)
  tibble::tibble(Season = as.character(p$target_season), ERA = rng(p$era, p$era_lo, p$era_hi),
                 `Season range` = sprintf("%.2f–%.2f", p$era_out_lo, p$era_out_hi),
                 FIP = rng(p$fip, p$fip_lo, p$fip_hi),
                 `True talent` = sprintf("%d (%d–%d)", round(p$tt_grade), round(p$tt_lo), round(p$tt_hi)),
                 `Home-park ERA` = sprintf("%.2f", p$era_home))
}

projectionUI <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("head")), uiOutput(ns("message")),
          h3(class = "pv-section", "Outlook"),
          bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, DT::DTOutput(ns("table"), height = "auto"))),
          p(class = "pv-note", "True-talent ranges are 90%. The season range adds the luck of one season's results. ERA and FIP are park-neutral; home-park ERA uses his current parks."),
          h3(class = "pv-section", "Next season, by component"),
          bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, DT::DTOutput(ns("components"), height = "auto"))),
          uiOutput(ns("weights")),
          h3(class = "pv-section", "Luck gap"),
          uiOutput(ns("luck")))
}

projectionServer <- function(id, outputs, sel) {
  moduleServer(id, function(input, output, session) {
    proj <- reactive({ s <- sel$state(); pitcher_projection(outputs$projections, s$pitcher, s$season) })
    output$head <- renderUI(pitcher_heading(outputs$pitchers, sel$state()))
    output$message <- renderUI({
      if (is.null(proj())) div(class = "alert alert-info",
                               "No projection for this pitcher and season. Projections are made from the latest season.")
    })
    output$table <- DT::renderDT({
      p <- proj(); req(p)
      DT::datatable(outlook_table(p), rownames = FALSE, selection = "none", class = "compact", fillContainer = FALSE,
                    options = list(dom = "t", paging = FALSE, ordering = FALSE))
    })
    output$components <- DT::renderDT({
      p <- proj(); req(p); h <- p[p$horizon == 1, ]
      pct <- function(x, lo, hi) sprintf("%.1f%% (%.1f–%.1f)", 100 * x, 100 * lo, 100 * hi)
      t <- tibble::tibble(Component = c("Strikeouts per batter", "Walks + HBP per batter", "Home runs per batter",
                                        "Hits per ball in play"),
                          Projection = c(pct(h$k, h$k_lo, h$k_hi), pct(h$bbhbp, h$bbhbp_lo, h$bbhbp_hi),
                                         pct(h$hr, h$hr_lo, h$hr_hi), pct(h$hbip, h$hbip_lo, h$hbip_hi)))
      DT::datatable(t, rownames = FALSE, selection = "none", class = "compact", fillContainer = FALSE,
                    options = list(dom = "t", paging = FALSE, ordering = FALSE))
    })
    output$weights <- renderUI({
      p <- proj(); req(p); w <- p$stuff_weight[p$horizon == 1]
      div(class = "pv-weights",
          div(class = "pv-weight-bar", div(class = "pv-weight-stuff", style = sprintf("width:%.0f%%;", 100 * w))),
          p(class = "pv-note", sprintf("Pitch-model signals %.0f%% · Results %.0f%% of how far the estimate moved from league average.",
                                       100 * w, 100 * (1 - w))))
    })
    output$luck <- renderUI({
      s <- sel$state()
      l <- if (!is.null(outputs$luck) && nrow(outputs$luck))
        outputs$luck[outputs$luck$pitcher == s$pitcher & outputs$luck$season == s$season & outputs$luck$level == s$level, ] else NULL
      if (is.null(l) || !nrow(l)) return(p(class = "pv-note", "No luck gap for this pitcher and season."))
      rows <- list(c("Actual ERA", sprintf("%.2f", l$actual_era)), c("Expected ERA", sprintf("%.2f", l$expected_era)),
                   c("Gap", signed(l$gap)), c("BABIP", signed(l$babip)), c("Strand rate", signed(l$strand)),
                   c("HR/FB", signed(l$hr_fb)), c("Other", signed(l$other)))
      tagList(tags$table(class = "table table-sm pv-luck", tags$tbody(lapply(rows, function(r) tags$tr(tags$td(r[1]), tags$td(r[2]))))),
              p(class = "pv-note", LUCK_TIP))
    })
  })
}

# ==== projections =====================================================================

# Projections: next-season (base season + 1) projections for every projected pitcher,
# sortable, with team, level-basis and innings filters. A row click opens that pitcher.

projections_table <- function(proj, pitchers, basis = "All", team = "All", min_ip = 0) {
  empty <- tibble::tibble(pitcher = integer(), name = character(), team = character(), basis = character(), ip = numeric(),
                          true_talent = numeric(), tt_lo = numeric(), tt_hi = numeric(), era = numeric(), era_lo = numeric(),
                          era_hi = numeric(), fip = numeric(), k = numeric(), bbhbp = numeric(), hr = numeric(),
                          hbip = numeric(), step_forward = numeric(), step_back = numeric())
  if (is.null(proj) || !nrow(proj)) return(empty)
  h <- proj[proj$horizon == 1, ]
  base <- h$base_season[1]
  who <- pitchers[pitchers$season == base, ] %>% dplyr::arrange(level != "MLB") %>%
    dplyr::group_by(pitcher) %>%
    dplyr::summarise(team = dplyr::first(team_abbrev), teams = paste(unique(unlist(strsplit(teams, ","))), collapse = ","),
                     ip = sum(ip_outs, na.rm = TRUE) / 3, .groups = "drop")
  t <- h %>% dplyr::left_join(who, by = "pitcher") %>%
    dplyr::mutate(ip = dplyr::coalesce(ip, 0)) %>%
    dplyr::filter(ip >= min_ip)
  if (!identical(basis, "All")) t <- t[t$basis == basis, ]
  if (!identical(team, "All")) t <- t[vapply(strsplit(dplyr::coalesce(t$teams, ""), ",", fixed = TRUE), function(x) team %in% x, TRUE), ]
  if (!nrow(t)) return(empty)
  t %>% dplyr::transmute(pitcher, name, team, basis, ip, true_talent = tt_grade, tt_lo, tt_hi, era, era_lo, era_hi, fip,
                         k, bbhbp, hr, hbip, step_forward, step_back) %>%
    dplyr::arrange(dplyr::desc(true_talent))
}

projectionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("title")),
    bslib::layout_columns(
      selectInput(ns("basis"), "Basis", c("All" = "All", "MLB" = "MLB", "MLB-equivalent (mostly Triple-A)" = "MLB-equivalent")),
      selectInput(ns("team"), "Team", choices = "All"),
      sliderInput(ns("min_ip"), "Min innings last season", min = 0, max = 200, value = 0, step = 10, ticks = FALSE)),
    bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, DT::DTOutput(ns("table"), height = "auto"))),
    p(class = "pv-note", "True talent is the 20-80 grade of projected true-talent RA9 (90% range). ERA and FIP are park-neutral true-talent projections with 90% ranges; rates are per batter faced, BABIP per ball in play. Step forward / back: chance next season's true-talent ERA is 0.75+ runs better / worse than today's. Click a row to open that pitcher."))
}

projectionsServer <- function(id, outputs, sel, parent = NULL) {
  moduleServer(id, function(input, output, session) {
    proj <- outputs$projections
    base <- if (!is.null(proj) && nrow(proj)) proj$base_season[1] else NA
    output$title <- renderUI({
      if (is.na(base)) return(div(class = "alert alert-info", "No projections published yet."))
      h2(class = "pv-title", sprintf("%d projections", base + 1L))
    })
    observe({
      p <- outputs$pitchers[outputs$pitchers$season %in% base, ]
      updateSelectInput(session, "team", choices = c("All", sort(unique(unlist(strsplit(p$teams, ","))))))
    })
    board <- reactive({
      projections_table(proj, outputs$pitchers, input$basis %||% "All", input$team %||% "All", input$min_ip %||% 0)
    })
    output$table <- DT::renderDT({
      t <- board()
      rng <- function(x, lo, hi) sprintf("%.2f–%.2f", lo, hi)
      pct <- function(x) 100 * x
      shown <- t %>% dplyr::transmute(Name = name, Team = team, Basis = basis, `IP (last)` = ip, `True Talent` = true_talent,
                                      `TT range` = sprintf("%d–%d", round(tt_lo), round(tt_hi)),
                                      ERA = era, `ERA range` = rng(era, era_lo, era_hi), FIP = fip,
                                      `K%` = pct(k), `BB+HBP%` = pct(bbhbp), `HR%` = pct(hr), BABIP = hbip,
                                      `Step fwd` = pct(step_forward), `Step back` = pct(step_back))
      DT::datatable(shown, rownames = FALSE, selection = "single", class = "compact hover", fillContainer = FALSE,
                    options = list(pageLength = 25, scrollX = TRUE, order = list(list(4, "desc")),
                                   columnDefs = list(list(className = "pv-name-cell", targets = 0),
                                                     list(className = "pv-gradecell", targets = 4)))) %>%
        DT::formatRound(c("IP (last)", "True Talent"), 0) %>%
        DT::formatRound(c("ERA", "FIP", "BABIP"), c(2, 2, 3)) %>%
        DT::formatRound(c("K%", "BB+HBP%", "HR%", "Step fwd", "Step back"), 1) %>%
        DT::formatStyle("True Talent", backgroundColor = grade_fill(), color = grade_text_color())
    })
    observeEvent(input$table_rows_selected, {
      r <- board()[input$table_rows_selected, ]
      lv <- if (identical(r$basis, "MLB-equivalent")) "AAA" else "MLB"
      sel$select(lv, base, r$pitcher)
      if (!is.null(parent)) bslib::nav_select("main_nav", "Player Projections", session = parent)
    })
  })
}
