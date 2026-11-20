# Scorecard page: the published P1 scorecard (markdown tables + chart).

scorecard_html <- function(md_path) {
  if (!file.exists(md_path)) return("<p>The scorecard has not been published yet.</p>")
  md <- readLines(md_path, warn = FALSE)
  html <- commonmark::markdown_html(paste(md, collapse = "\n"), extensions = TRUE)
  gsub('src="scorecard_chart.png"', 'src="pv_scorecard/scorecard_chart.png" style="max-width:100%"', html, fixed = TRUE)
}

scorecardUI <- function(id) uiOutput(NS(id, "body"))

scorecardServer <- function(id, outputs) {
  moduleServer(id, function(input, output, session) {
    if (file.exists(outputs$scorecard$chart)) addResourcePath("pv_scorecard", dirname(outputs$scorecard$chart))
    output$body <- renderUI(div(class = "pv-scorecard", HTML(scorecard_html(outputs$scorecard$md))))
  })
}
