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
      if (!is.null(parent)) bslib::nav_select("main_nav", "Projection", session = parent)
    })
  })
}
