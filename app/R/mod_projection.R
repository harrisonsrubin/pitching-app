# Projection (P3a): the true-talent projection on the Grades card, and a Projection page
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

bet_strip <- function(ch) {
  ch <- ch[!is.na(ch)]
  if (!length(ch)) return(NULL)
  bin <- round(ch / 2)                                   # stack chips that land on the same spot
  level <- stats::ave(seq_along(bin), bin, FUN = seq_along) - 1
  shiny::div(class = "pv-bet",
      shiny::div(class = "pv-bet-axis", lapply(seq(20, 80, 10), function(t)
        shiny::span(class = "pv-bet-tick", style = sprintf("left:%.1f%%;", (t - 20) / 60 * 100), t))),
      lapply(seq_along(ch), function(i)
        shiny::span(class = "pv-chip", style = sprintf("left:%.1f%%; bottom:%dpx; background:%s;", (ch[i] - 20) / 60 * 100,
                                                 6 + 11 * level[i], grade_color(ch[i])))))
}

projection_card <- function(proj, luck, pitcher, level, season) {
  p <- pitcher_projection(proj, pitcher, season)
  if (is.null(p)) return(NULL)
  h1 <- p[p$horizon == 1, ]
  if (!nrow(h1)) return(NULL)
  l <- if (!is.null(luck) && nrow(luck)) luck[luck$pitcher == pitcher & luck$season == season, ] else NULL
  if (!is.null(l) && nrow(l) > 1) l <- l[l$level == level, ][1, ]
  luck_line <- if (!is.null(l) && nrow(l) == 1 && !is.na(l$gap))
    shiny::div(class = "pv-proj-line", title = LUCK_TIP,
        sprintf("Luck gap %d: %s ERA (BABIP %s, strand %s%s)", season, signed(l$gap), signed(l$babip), signed(l$strand),
                if (is.na(l$hr_fb)) "" else paste0(", HR/FB ", signed(l$hr_fb))))
  shiny::div(class = "pv-proj",
      shiny::div(class = "pv-proj-head", sprintf("True talent bet (%d)", h1$target_season),
          if (identical(h1$basis, "MLB-equivalent")) shiny::span(class = "pv-basis", "MLB-equivalent")),
      bet_strip(unlist(h1[paste0("chip_", 1:10)])),
      shiny::div(class = "pv-proj-line",
          sprintf("Projected %d: ERA %.2f (%.2f–%.2f) · FIP %.2f (%.2f–%.2f)", h1$target_season, h1$era, h1$era_lo,
                  h1$era_hi, h1$fip, h1$fip_lo, h1$fip_hi)),
      luck_line,
      shiny::div(class = "pv-proj-line", title = STEP_TIP,
          sprintf("Step forward %.0f%% · Step back %.0f%%", 100 * h1$step_forward, 100 * h1$step_back)))
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
