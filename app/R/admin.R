# Admin login (P2 design 6). The app holds only a bcrypt hash of the password
# (PV_ADMIN_HASH); five wrong tries lock the session. Nothing secret is ever rendered.

MAX_ATTEMPTS <- 5L

check_password <- function(pw, hash) {
  if (!nzchar(hash %||% "") || !nzchar(pw %||% "")) return(FALSE)
  isTRUE(tryCatch(bcrypt::checkpw(pw, hash), error = function(e) FALSE))
}

adminUI <- function(id) {
  ns <- NS(id)
  tagList(passwordInput(ns("pw"), "Admin password"), actionButton(ns("login"), "Unlock"), uiOutput(ns("status")))
}

adminServer <- function(id, cfg) {
  moduleServer(id, function(input, output, session) {
    ok <- reactiveVal(FALSE)
    fails <- reactiveVal(0L)
    observeEvent(input$login, {
      if (fails() >= MAX_ATTEMPTS) return()
      if (check_password(input$pw, cfg$admin_hash)) ok(TRUE) else fails(fails() + 1L)
      updateTextInput(session, "pw", value = "")
    })
    output$status <- renderUI({
      if (fails() >= MAX_ATTEMPTS) div(class = "text-danger", "Too many attempts. Reload the page to try again.")
      else if (fails() > 0) div(class = "text-danger", "Wrong password.")
    })
    is_admin <- reactive(ok())
    is_admin
  })
}
