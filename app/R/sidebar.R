# Sidebar selection shared by every page.

# ==== sidebar =========================================================================

# Sidebar: level, season, team, pitcher, batter side, min pitches, grade scale. Returns
# sel$state() (the current selection) and sel$select() (jump to a pitcher, used by the
# Leaderboard). A traded pitcher is listed under every team he pitched for.

seasons_for <- function(pitchers, level) sort(unique(pitchers$season[pitchers$level == level]), decreasing = TRUE)

team_choices <- function(pitchers, level, season) {
  p <- pitchers[pitchers$level == level & pitchers$season == season, ]
  sort(unique(unlist(strsplit(p$teams, ",", fixed = TRUE))))
}

pitchers_for <- function(pitchers, level, season, team) {
  p <- pitchers[pitchers$level == level & pitchers$season == season, ]
  if (!identical(team, "All")) p <- p[vapply(strsplit(p$teams, ",", fixed = TRUE), function(t) team %in% t, TRUE), ]
  p[order(last_name_key(p$name), p$name, -p$pitches), ]
}

# Sort key for "last name, A-Z": drops suffixes (Jr., III) and keeps particles, so
# "Elvis De La Cruz" files under D like a box score would list him.
NAME_SUFFIXES <- c("jr", "sr", "ii", "iii", "iv", "v")
NAME_PARTICLES <- c("de", "del", "della", "la", "las", "los", "da", "di", "van", "von", "der", "st", "le", "mac")
last_name_key <- function(name) {
  vapply(name, function(n) {
    if (is.na(n)) return(NA_character_)
    w <- strsplit(trimws(n), "\\s+")[[1]]
    bare <- tolower(gsub("[.,]", "", w))
    while (length(w) > 1 && bare[length(bare)] %in% NAME_SUFFIXES) { w <- w[-length(w)]; bare <- bare[-length(bare)] }
    i <- length(w)
    while (i > 2 && bare[i - 1] %in% NAME_PARTICLES) i <- i - 1
    paste(bare[i:length(bare)], collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

# "Name, Triple-A 2025": names the sidebar's pitcher on pages that show one pitcher.
pitcher_heading <- function(pitchers, s) {
  p <- pitchers[pitchers$pitcher == s$pitcher & pitchers$level == s$level & pitchers$season == s$season, ]
  if (nrow(p) == 0) return(NULL)
  h2(class = "pv-title", sprintf("%s, %s %d", ifelse(is.na(p$name[1]), paste("Pitcher", p$pitcher[1]), p$name[1]),
                                 LEVEL_LABELS[[s$level]], s$season))
}

pitcher_label <- function(p) sprintf("%s (%s)", ifelse(is.na(p$name), paste("ID", p$pitcher), p$name), p$team_abbrev)

set_pitcher_choices <- function(session, p, selected) {
  updateSelectizeInput(session, "pitcher", choices = stats::setNames(p$pitcher, pitcher_label(p)),
                       selected = selected, server = TRUE)
}

sidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("level"), "Level", choices = c("MLB" = "MLB", "Triple-A" = "AAA")),
    selectInput(ns("season"), "Season", choices = NULL),
    selectInput(ns("team"), "Team", choices = "All"),
    selectizeInput(ns("pitcher"), "Pitcher", choices = NULL),
    radioButtons(ns("side"), "Batter side", c("All", "L", "R"), inline = TRUE),
    sliderInput(ns("min_pitches"), "Min pitches (pitch type)", min = 0, max = 200, value = 0, step = 10),
    radioButtons(ns("scale"), "Grade scale", c("Per pitch type" = "type", "Global" = "global"), inline = TRUE),
    helpText("Grades: 20-80, 50 = average. Ranges are 90% intervals.")
  )
}

sidebarServer <- function(id, outputs) {
  moduleServer(id, function(input, output, session) {
    # A Leaderboard jump: the target lives in a plain variable (clearing it must not
    # re-trigger anything) and `jump` counts requests so the pitcher observer fires even
    # when level, season and team are unchanged.
    target <- NULL
    jump <- reactiveVal(0L)
    pending <- function() target
    observeEvent(input$level, {
      s <- seasons_for(outputs$pitchers, input$level)
      want <- pending()$season
      updateSelectInput(session, "season", choices = s, selected = if (!is.null(want) && want %in% s) want else s[1])
    })
    observeEvent(list(input$level, input$season), {
      req(input$season)
      updateSelectInput(session, "team", choices = c("All", team_choices(outputs$pitchers, input$level, as.integer(input$season))),
                        selected = "All")
    })
    # Also fires on jump(): a Leaderboard click on the current level and season
    # changes no other input, but must still move to the clicked pitcher.
    observeEvent(list(input$level, input$season, input$team, jump()), {
      req(input$season, input$team)
      p <- pitchers_for(outputs$pitchers, input$level, as.integer(input$season), input$team)
      want <- pending()$pitcher
      if (!is.null(want) && !(want %in% p$pitcher)) {
        if (identical(pending()$season, as.integer(input$season)) && !identical(input$team, "All")) return()
        if (!identical(pending()$season, as.integer(input$season))) return()   # season update still coming
      }
      current <- suppressWarnings(as.integer(input$pitcher))
      sel <- if (!is.null(want) && want %in% p$pitcher) want
             else if (length(current) == 1 && !is.na(current) && current %in% p$pitcher) current   # keep him
             else p$pitcher[which.max(p$pitches)]   # default: his team's busiest pitcher
      set_pitcher_choices(session, p, sel)
      if (!is.null(want) && want %in% p$pitcher) target <<- NULL
    }, ignoreNULL = FALSE)
    state <- reactive({
      req(input$pitcher, input$season)
      list(level = input$level, season = as.integer(input$season), team = input$team,
           pitcher = as.integer(input$pitcher), side = input$side, min_pitches = input$min_pitches, scale = input$scale)
    })
    select <- function(level, season, pitcher) {
      target <<- list(season = as.integer(season), pitcher = as.integer(pitcher))
      jump(jump() + 1L)
      updateSelectInput(session, "level", selected = level)
      updateSelectInput(session, "season", selected = season)
      updateSelectInput(session, "team", selected = "All")
    }
    list(state = state, select = select)
  })
}

# The selected pitcher's pitches, with this session's pitch-type edits applied.
displayServer <- function(cfg, sel, cache) {
  edits <- reactiveVal(tibble::tibble(pitch_uid = character(), new_pitch_type = character()))
  raw <- reactive({
    s <- sel$state()
    fetch_display(cfg, s$level, s$season, s$pitcher, cache)
  })
  rows <- reactive({
    d <- raw()
    if (is.null(d)) return(NULL)
    apply_selection_edits(d, edits())
  })
  list(rows = rows, edits = edits)
}
