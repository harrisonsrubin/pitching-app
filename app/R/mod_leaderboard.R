# Leaderboard: every graded pitcher for a level (or both) and season, overall Stuff /
# Location / Pitching plus Stuff and Location by pitch type. Clicking a row opens that
# pitcher on the Grades page.

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

leaderboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      selectInput(ns("level"), "Level", c("MLB" = "MLB", "Triple-A" = "AAA", "Both" = "Both")),
      selectInput(ns("season"), "Season", choices = NULL),
      selectInput(ns("team"), "Team", choices = "All"),
      sliderInput(ns("min_pitches"), "Min total pitches", min = 0, max = 2000, value = 300, step = 50, ticks = FALSE)),
    p(class = "pv-note", "Overall grades and Stuff (S) and Location (L) by pitch type. Click a row to open that pitcher."),
    bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, DT::DTOutput(ns("table"), height = "auto"))))
}

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
    # Only the grade scale matters here; a pitcher change elsewhere must not rebuild the
    # table (that would reset its sort and page). A reactiveVal ignores same-value sets.
    scale <- reactiveVal("global")
    observe({
      v <- tryCatch(sel$state()$scale, error = function(e) NULL) %||% "global"
      if (!identical(v, isolate(scale()))) scale(v)
    })
    board <- reactive({
      req(input$season, input$team)
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
