# Admin login (P2 design 6). The app holds only a bcrypt hash of the password
# (PV_ADMIN_HASH). Five wrong tries in a row lock the login for every session for 15
# minutes, so reloading the page does not reset the count. Nothing secret is rendered.

MAX_ATTEMPTS <- 5L
LOCK_MINUTES <- 15
ADMIN_LOCK <- new.env()
admin_lock_reset <- function() { ADMIN_LOCK$fails <- 0L; ADMIN_LOCK$until <- NULL; invisible() }
admin_lock_reset()
admin_now <- function() getOption("pv.now", Sys.time())
admin_locked <- function() !is.null(ADMIN_LOCK$until) && admin_now() < ADMIN_LOCK$until

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
    msg <- reactiveVal("")
    observeEvent(input$login, {
      updateTextInput(session, "pw", value = "")
      if (!is.null(ADMIN_LOCK$until) && !admin_locked()) admin_lock_reset()   # lock expired
      if (admin_locked()) { msg(sprintf("Too many attempts. Try again in %d minutes.", LOCK_MINUTES)); return() }
      if (check_password(input$pw, cfg$admin_hash)) { admin_lock_reset(); msg(""); ok(TRUE); return() }
      ADMIN_LOCK$fails <- ADMIN_LOCK$fails + 1L
      if (ADMIN_LOCK$fails >= MAX_ATTEMPTS) {
        ADMIN_LOCK$until <- admin_now() + LOCK_MINUTES * 60
        msg(sprintf("Too many attempts. Try again in %d minutes.", LOCK_MINUTES))
      } else msg("Wrong password.")
    })
    output$status <- renderUI(if (nzchar(msg())) div(class = "text-danger", msg()))
    is_admin <- reactive(ok())
    is_admin
  })
}
