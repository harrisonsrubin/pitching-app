# Pitch-Type Editor: admin login, GitHub corrections file and the editor page

# Admin login

# Five wrong tries lock the login for every session for 15 minutes
MAX_ATTEMPTS <- 5L
LOCK_MINUTES <- 15
ADMIN_LOCK <- new.env()
admin_lock_reset <- function() { ADMIN_LOCK$fails <- 0L; ADMIN_LOCK$until <- NULL; invisible() }
admin_lock_reset()
admin_now <- function() getOption("pv.now", Sys.time())
admin_locked <- function() !is.null(ADMIN_LOCK$until) && admin_now() < ADMIN_LOCK$until

# Compare a password to the stored bcrypt hash
check_password <- function(pw, hash) {
  if (!nzchar(hash %||% "") || !nzchar(pw %||% "")) return(FALSE)
  isTRUE(tryCatch(bcrypt::checkpw(pw, hash), error = function(e) FALSE))
}

# Password box and unlock button
adminUI <- function(id) {
  ns <- NS(id)
  tagList(passwordInput(ns("pw"), "Admin password"), actionButton(ns("login"), "Unlock"), uiOutput(ns("status")))
}

# Returns a reactive that is TRUE once unlocked
adminServer <- function(id, cfg) {
  moduleServer(id, function(input, output, session) {
    ok <- reactiveVal(FALSE)
    msg <- reactiveVal("")
    observeEvent(input$login, {
      updateTextInput(session, "pw", value = "")
      if (!is.null(ADMIN_LOCK$until) && !admin_locked()) admin_lock_reset()  # lock expired
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

# GitHub corrections

# Corrections CSVs are read and written through the GitHub contents API
CORRECTION_COLS <- c("pitch_uid", "play_id", "new_pitch_type", "original_pitch_type", "corrected_at", "corrected_by")
CORRECTIONS_PATH_RE <- "^corrections/pitch_corrections_\\d{4}\\.csv$"

# Repo path of a season's corrections file
corrections_path <- function(season) sprintf("corrections/pitch_corrections_%d.csv", as.integer(season))

# Empty corrections table with all columns
empty_corrections <- function() {
  stats::setNames(as.data.frame(replicate(length(CORRECTION_COLS), character(), simplify = FALSE)), CORRECTION_COLS) %>%
    tibble::as_tibble()
}

# Add new corrections, replacing any older row for the same pitch
upsert_corrections <- function(existing, new, now = Sys.time()) {
  stamp <- format(as.POSIXct(now, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  add <- tibble::tibble(pitch_uid = new$pitch_uid, play_id = NA_character_, new_pitch_type = new$new_pitch_type,
                        original_pitch_type = new$original_pitch_type, corrected_at = stamp, corrected_by = "admin")
  existing <- dplyr::mutate(existing, dplyr::across(dplyr::everything(), as.character))
  dplyr::bind_rows(existing[!existing$pitch_uid %in% add$pitch_uid, ], add) %>%
    dplyr::select(dplyr::all_of(CORRECTION_COLS)) %>%
    dplyr::arrange(corrected_at, pitch_uid)
}

# Error messages for unknown pitch types or pitches
validate_corrections <- function(new, display) {
  msgs <- character()
  bad <- setdiff(unique(new$new_pitch_type), c(PITCH_LABELS, "REMOVED"))
  if (length(bad)) msgs <- c(msgs, paste("Unknown pitch type:", paste(bad, collapse = ", ")))
  missing <- setdiff(new$pitch_uid, display$pitch_uid)
  if (length(missing)) msgs <- c(msgs, paste("Unknown pitch:", paste(utils::head(missing, 5), collapse = ", ")))
  msgs
}

# GitHub contents API URL for a repo path
contents_url <- function(path, cfg) sprintf("%s/repos/%s/contents/%s", GITHUB_API, cfg$app_repo, path)

# Read a corrections file and its sha; a missing file is empty with no sha
github_get_corrections <- function(path, cfg) {
  meta <- httr2::req_perform(gh_request(contents_url(path, cfg), cfg$corrections_token))
  if (httr2::resp_status(meta) == 404) return(list(rows = empty_corrections(), sha = NULL))
  if (httr2::resp_status(meta) != 200) stop("Could not read ", path, " (HTTP ", httr2::resp_status(meta), ")")
  sha <- httr2::resp_body_json(meta)$sha
  raw <- httr2::req_perform(gh_request(contents_url(path, cfg), cfg$corrections_token) %>%
                              httr2::req_headers(Accept = "application/vnd.github.raw+json"))
  if (httr2::resp_status(raw) != 200) stop("Could not read ", path, " (HTTP ", httr2::resp_status(raw), ")")
  rows <- readr::read_csv(I(httr2::resp_body_string(raw)), col_types = readr::cols(.default = "c"), show_col_types = FALSE)
  list(rows = dplyr::bind_rows(empty_corrections(), rows), sha = sha)
}

# Write a corrections file and return the HTTP status
github_put_file <- function(path, csv_text, sha, message, cfg) {
  if (!grepl(CORRECTIONS_PATH_RE, path)) stop("Only corrections files can be written, not ", path)
  body <- list(message = message, content = jsonlite::base64_enc(charToRaw(csv_text)))
  if (!is.null(sha)) body$sha <- sha
  resp <- httr2::req_perform(gh_request(contents_url(path, cfg), cfg$corrections_token) %>%
                               httr2::req_method("PUT") %>% httr2::req_body_json(body))
  httr2::resp_status(resp)
}

# Table to CSV text
corrections_csv <- function(rows) {
  paste0(paste(utils::capture.output(readr::write_csv(rows, stdout(), na = "")), collapse = "\n"), "\n")
}

# Upsert and write back, retrying once if the file changed mid-save
save_corrections <- function(new, season, cfg) {
  path <- corrections_path(season)
  msg <- sprintf("Pitch-type corrections %d (%d pitches)", as.integer(season), nrow(new))
  for (attempt in 1:2) {
    cur <- tryCatch(github_get_corrections(path, cfg), error = function(e) e)
    if (inherits(cur, "error")) return(list(ok = FALSE, message = conditionMessage(cur)))
    status <- github_put_file(path, corrections_csv(upsert_corrections(cur$rows, new)), cur$sha, msg, cfg)
    if (status %in% c(200L, 201L)) return(list(ok = TRUE, message = sprintf("Saved %d pitch(es).", nrow(new))))
    if (!status %in% c(409L, 422L)) return(list(ok = FALSE, message = sprintf("Save failed (HTTP %d).", status)))
  }
  list(ok = FALSE, message = "The corrections file changed while saving. Nothing was overwritten; reload and try again.")
}

# Retag or drop the given pitches
apply_selection <- function(display, uids, new_type) {
  if (identical(new_type, "REMOVED")) return(display[!display$pitch_uid %in% uids, ])
  display$pitch_type[display$pitch_uid %in% uids] <- new_type
  display
}

# Apply all session edits to the display data
apply_selection_edits <- function(display, edits) {
  if (is.null(edits) || nrow(edits) == 0) return(display)
  for (ty in unique(edits$new_pitch_type)) display <- apply_selection(display, edits$pitch_uid[edits$new_pitch_type == ty], ty)
  display
}

# Removed pitches from this pitcher's plate appearances, found in the corrections file
removed_for_pitcher <- function(corr, display) {
  if (is.null(corr) || nrow(corr) == 0 || is.null(display) || nrow(display) == 0) return(corr[0, ])
  pa <- function(uid) sub("-\\d+$", "", uid)
  corr[corr$new_pitch_type == "REMOVED" & pa(corr$pitch_uid) %in% pa(display$pitch_uid), ]
}

# Season's corrections from the local repo file or GitHub
read_season_corrections <- function(season, cfg) {
  if (isTRUE(cfg$local)) {
    f <- file.path(cfg$data_dir, "..", corrections_path(season))
    if (!file.exists(f)) return(empty_corrections())
    return(dplyr::bind_rows(empty_corrections(), readr::read_csv(f, col_types = readr::cols(.default = "c"), show_col_types = FALSE)))
  }
  tryCatch(github_get_corrections(corrections_path(season), cfg)$rows, error = function(e) empty_corrections())
}

# Editor page

# Own movement plot here since switching tabs clears a plotly selection
editorUI <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("panel")), DT::DTOutput(ns("corrections")))
}

# Select pitches on the plot, retag them and save to the corrections file
editorServer <- function(id, cfg, sel, display, outputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    is_admin <- adminServer("admin", cfg)
    status <- reactiveVal("")
    saved <- reactiveVal(character())  # keys just saved, so the stale selection is ignored
    selected <- reactive({
      ev <- tryCatch(plotly::event_data("plotly_selected", source = "editor"), error = function(e) NULL)
      k <- if (is.null(ev) || is.null(ev$key)) character() else as.character(stats::na.omit(unlist(ev$key)))
      if (length(k) && setequal(k, saved())) character() else k
    })
    # Built once per unlock so the type dropdown keeps its choice while selecting
    output$panel <- renderUI({
      if (!isTRUE(is_admin())) return(bslib::card(bslib::card_header("Pitch-Type Editor"), adminUI(ns("admin"))))
      tagList(
        uiOutput(ns("head")),
        bslib::layout_columns(col_widths = c(8, 4),
          bslib::card(fill = FALSE, bslib::card_body(fillable = FALSE, plotly::plotlyOutput(ns("plot"), height = "600px"))),
          bslib::card(fill = FALSE, bslib::card_header("Retag selected pitches"),
            p("Draw a lasso (or switch to box select) around pitches on the plot, choose the correct type, then Save."),
            div(class = "pv-count", textOutput(ns("count"), inline = TRUE)),
            selectInput(ns("new_type"), "New pitch type", c(PITCH_LABELS, "REMOVED"), selected = PITCH_LABELS[1]),
            actionButton(ns("save"), "Save", class = "btn-primary"),
            uiOutput(ns("status")),
            div(class = "alert alert-info mt-3", "Grades update after the rescore (about 30 minutes)."))),
        actionButton(ns("restore"), "Restore Savant type for selected rows below"))
    })
    output$head <- renderUI(pitcher_heading(outputs$pitchers, sel$state()))
    output$count <- renderText(sprintf("%d pitch%s selected", length(selected()), if (length(selected()) == 1) "" else "es"))
    output$status <- renderUI(if (nzchar(status())) div(class = "mt-2", status()))
    output$plot <- plotly::renderPlotly({
      req(isTRUE(is_admin()))
      d <- display$rows(); req(d, nrow(d) > 0)
      s <- sel$state()
      movement_plot(d, outputs$movement_ref, outputs$pctl, s$level, s$season, corrected_uids(d, display$edits()),
                    source = "editor", dragmode = "lasso")
    })
    season_corr <- reactiveVal(NULL)  # reread on unlock and after saves
    observeEvent(list(is_admin(), sel$state()$season), {
      if (isTRUE(is_admin())) season_corr(read_season_corrections(sel$state()$season, cfg))
    })
    # Removed pitches from the file plus this session
    removed <- reactive({
      r <- removed_for_pitcher(season_corr(), display$rows())
      e <- display$edits()
      if (!is.null(e) && any(e$new_pitch_type == "REMOVED")) {
        sess <- e[e$new_pitch_type == "REMOVED", ]
        r <- dplyr::bind_rows(r[!r$pitch_uid %in% sess$pitch_uid, ],
                              tibble::tibble(pitch_uid = sess$pitch_uid, new_pitch_type = "REMOVED",
                                             original_pitch_type = sess$original_pitch_type %||% NA_character_))
      }
      r
    })
    # Every retagged or removed pitch for the table
    corrected <- reactive({
      d <- display$rows(); req(d)
      ch <- d[!is.na(d$pitch_type_savant) & d$pitch_type != d$pitch_type_savant,
              c("pitch_uid", "game_date", "pitch_type_savant", "pitch_type")]
      rm_ <- removed()
      dplyr::bind_rows(dplyr::mutate(ch, game_date = as.character(game_date)),
                       tibble::tibble(pitch_uid = rm_$pitch_uid, game_date = NA_character_,
                                      pitch_type_savant = rm_$original_pitch_type, pitch_type = "REMOVED"))
    })
    # Validate, save, then apply locally
    commit <- function(new, d) {
      known <- dplyr::bind_rows(d[, "pitch_uid"], tibble::tibble(pitch_uid = removed()$pitch_uid))
      errs <- validate_corrections(new, known)
      if (length(errs)) { status(paste(errs, collapse = " ")); return() }
      r <- save_corrections(new, sel$state()$season, cfg)
      status(r$message)
      if (r$ok) {
        e <- display$edits()
        display$edits(dplyr::bind_rows(e[!e$pitch_uid %in% new$pitch_uid, ],
                                       dplyr::select(new, pitch_uid, new_pitch_type, original_pitch_type)))
        season_corr(read_season_corrections(sel$state()$season, cfg))
        saved(new$pitch_uid)
      }
    }
    observeEvent(input$save, {
      req(isTRUE(is_admin()))
      d <- display$rows(); uids <- selected()
      if (is.null(d) || length(uids) == 0) { status("Select pitches first."); return() }
      commit(tibble::tibble(pitch_uid = uids, new_pitch_type = input$new_type,
                            original_pitch_type = d$pitch_type_savant[match(uids, d$pitch_uid)]), d)
    })
    observeEvent(input$restore, {
      req(isTRUE(is_admin()), length(input$corrections_rows_selected) > 0)
      rows <- corrected()[input$corrections_rows_selected, ]
      commit(tibble::tibble(pitch_uid = rows$pitch_uid, new_pitch_type = rows$pitch_type_savant,
                            original_pitch_type = rows$pitch_type_savant), display$rows())
    })
    output$corrections <- DT::renderDT({
      req(isTRUE(is_admin()))
      DT::datatable(dplyr::rename(corrected(), Pitch = pitch_uid, Date = game_date, Savant = pitch_type_savant, Now = pitch_type),
                    rownames = FALSE, selection = "multiple", options = list(pageLength = 10),
                    caption = "This pitcher's corrected pitches. Select rows and press Restore to undo.")
    })
  })
}
