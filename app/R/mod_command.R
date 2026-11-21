# Command & Tunneling: where pitches land relative to each batter's zone (descriptive
# shares, next to the tested Location grade), and for the selected pitcher how far apart
# each pair of his pitches is at the decision point versus at the plate. No new grade.

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

# Pairs are compared within one batter side (pitches thrown to the same hitters).
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

commandUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2(class = "pv-title", "Command"),
    bslib::layout_columns(
      selectInput(ns("level"), "Level", c("MLB" = "MLB", "Triple-A" = "AAA")),
      selectInput(ns("season"), "Season", choices = NULL),
      selectInput(ns("team"), "Team", choices = "All"),
      sliderInput(ns("min_pitches"), "Min total pitches", min = 0, max = 2000, value = 500, step = 50, ticks = FALSE)),
    p(class = "pv-note", "Share of pitches by zone, sized to each batter's own strike zone: heart (middle two-thirds), shadow (the edges, inside and out), chase, and waste. Location is the tested 20-80 grade."),
    bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, DT::DTOutput(ns("table"), height = "auto"))),
    h2(class = "pv-title", "Tunneling"),
    uiOutput(ns("tunnel_head")),
    uiOutput(ns("tunnel_message")),
    bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, DT::DTOutput(ns("tunnels"), height = "auto"))),
    p(class = "pv-note", sprintf("Average gap between two pitch types %.1f ft in front of the plate, about when a hitter must decide, and at the plate, compared within each batter side (the sidebar's batter side narrows it). A small early gap with a large late one means the pitches look alike longer.", TUNNEL_FT_APP)))
}

TUNNEL_FT_APP <- 23.8

commandServer <- function(id, outputs, sel) {
  moduleServer(id, function(input, output, session) {
    observe({
      s <- sort(unique(outputs$pitchers$season[outputs$pitchers$level == input$level]), decreasing = TRUE)
      updateSelectInput(session, "season", choices = s, selected = s[1])
    }) %>% bindEvent(input$level)
    observe({
      req(input$season)
      p <- outputs$pitchers[outputs$pitchers$level == input$level & outputs$pitchers$season == as.integer(input$season), ]
      updateSelectInput(session, "team", choices = c("All", sort(unique(unlist(strsplit(p$teams, ","))))))
    }) %>% bindEvent(input$level, input$season)
    board <- reactive({
      req(input$season, input$team)
      command_table(outputs$command, outputs$grades, outputs$pitchers, input$level, as.integer(input$season),
                    input$team, input$min_pitches)
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
    output$tunnel_head <- renderUI({
      s <- sel$state()
      p <- outputs$pitchers[outputs$pitchers$pitcher == s$pitcher & outputs$pitchers$level == s$level &
                              outputs$pitchers$season == s$season, ]
      if (nrow(p)) p(sprintf("%s, %s %d (the pitcher selected in the sidebar)", p$name, LEVEL_LABELS[[s$level]], s$season))
    })
    output$tunnel_message <- renderUI({
      if (nrow(pairs()) == 0) div(class = "alert alert-info", "This pitcher has only one pitch type with 20 or more pitches to these batters, so there is nothing to compare.")
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
