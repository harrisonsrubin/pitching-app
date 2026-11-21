# Pitch-Type Editor (admin only): lasso or box-select pitches on this page's movement
# plot, choose a label, save. Saving commits the season's corrections CSV to the repo;
# the change shows immediately in this session and in the grades after the rescore.
# The plot lives on this page (not the Movement tab) because switching tabs clears a
# plotly selection.

editorUI <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("panel")), DT::DTOutput(ns("corrections")))
}

editorServer <- function(id, cfg, sel, display, outputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    is_admin <- adminServer("admin", cfg)
    status <- reactiveVal("")
    saved <- reactiveVal(character())   # keys just saved: the redrawn plot has no selection
    selected <- reactive({
      ev <- tryCatch(plotly::event_data("plotly_selected", source = "editor"), error = function(e) NULL)
      k <- if (is.null(ev) || is.null(ev$key)) character() else as.character(stats::na.omit(unlist(ev$key)))
      if (length(k) && setequal(k, saved())) character() else k
    })
    # Built once per unlock: the selection count and status are separate outputs, so the
    # type list never re-renders (and never loses its choice) while pitches are selected.
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
    season_corr <- reactiveVal(NULL)   # the season's corrections file, read on unlock and after saves
    observeEvent(list(is_admin(), sel$state()$season), {
      if (isTRUE(is_admin())) season_corr(read_season_corrections(sel$state()$season, cfg))
    })
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
    corrected <- reactive({
      d <- display$rows(); req(d)
      ch <- d[!is.na(d$pitch_type_savant) & d$pitch_type != d$pitch_type_savant,
              c("pitch_uid", "game_date", "pitch_type_savant", "pitch_type")]
      rm_ <- removed()
      dplyr::bind_rows(dplyr::mutate(ch, game_date = as.character(game_date)),
                       tibble::tibble(pitch_uid = rm_$pitch_uid, game_date = NA_character_,
                                      pitch_type_savant = rm_$original_pitch_type, pitch_type = "REMOVED"))
    })
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
