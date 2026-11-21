# Pitching app (README 10). Public and read-only; the Pitch-Type Editor unlocks with the
# admin password. Pages are modules in app/R/ (Shiny loads that folder automatically).
library(shiny)
library(bslib)

cfg <- app_config()
outputs <- load_outputs(cfg)
cache <- bucket_cache(max = 12L)   # shared by every session

ui <- page_navbar(
  title = "Pitch grades",
  id = "main_nav",
  theme = app_theme(),
  window_title = "Pitch grades",
  fillable = FALSE,
  sidebar = sidebar(width = 300, sidebarUI("sel")),
  nav_panel("Grades", gradesUI("grades")),
  nav_panel("Player Projections", projectionUI("projection")),
  nav_panel("Movement", movementUI("movement")),
  nav_panel("Location", locationUI("location")),
  nav_panel("Leaderboard", leaderboardUI("leaderboard")),
  nav_panel("League Projections", projectionsUI("projections")),
  nav_panel("Stuff Map", stuffmapUI("stuffmap")),
  nav_panel("Command & Tunneling", commandUI("command")),
  nav_panel("Rolling", rollingUI("rolling")),
  nav_panel("Pitch-Type Editor", editorUI("editor")),
  nav_panel("Scorecard", scorecardUI("scorecard"))
)

server <- function(input, output, session) {
  sel <- sidebarServer("sel", outputs)
  display <- displayServer(cfg, sel, cache)
  gradesServer("grades", outputs, sel, display)
  projectionServer("projection", outputs, sel)
  movementServer("movement", outputs, sel, display)
  locationServer("location", sel, display)
  leaderboardServer("leaderboard", outputs, sel, session)
  projectionsServer("projections", outputs, sel, session)
  stuffmapServer("stuffmap", outputs, sel)
  commandServer("command", outputs, sel)
  rollingServer("rolling", outputs, sel, display)
  editorServer("editor", cfg, sel, display, outputs)
  scorecardServer("scorecard", outputs)
}

shinyApp(ui, server)
