# Location page: where each pitch type is thrown, by batter side, from the pitcher's
# view (catcher's-view plate_x flipped), with the average strike zone drawn.

location_plot <- function(d) {
  d <- d %>% dplyr::mutate(x = -plate_x, stand = factor(ifelse(stand == "L", "vs LHB", "vs RHB"), c("vs LHB", "vs RHB")),
                           pitch_type = factor(pitch_type, intersect(PITCH_LABELS, unique(pitch_type))))
  counts <- dplyr::count(d, stand, pitch_type)
  dense <- dplyr::semi_join(d, counts[counts$n >= 5, ], by = c("stand", "pitch_type"))
  sparse <- dplyr::semi_join(d, counts[counts$n < 5, ], by = c("stand", "pitch_type"))
  top <- mean(d$sz_top, na.rm = TRUE); bot <- mean(d$sz_bot, na.rm = TRUE)
  ggplot2::ggplot(d, ggplot2::aes(x, plate_z)) +
    ggplot2::stat_density_2d(data = dense, ggplot2::aes(fill = ggplot2::after_stat(level)), geom = "polygon",
                             alpha = 0.6, contour_var = "ndensity", bins = 8) +
    ggplot2::geom_point(data = sparse, color = "#333333", size = 1.5) +
    ggplot2::annotate("rect", xmin = -PLATE_HALF_WIDTH, xmax = PLATE_HALF_WIDTH, ymin = bot, ymax = top,
                      fill = NA, color = "black") +
    ggplot2::scale_fill_gradient(low = "#fde0dd", high = "#c51b8a", guide = "none") +
    ggplot2::coord_fixed(xlim = c(-2, 2), ylim = c(0, 5)) +
    ggplot2::facet_grid(stand ~ pitch_type, drop = FALSE) +
    ggplot2::labs(x = "Horizontal location (ft, pitcher's view)", y = "Height (ft)") +
    ggplot2::theme_minimal(base_size = 13)
}

locationUI <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("message")), plotOutput(ns("plot"), height = "700px"))
}

locationServer <- function(id, sel, display) {
  moduleServer(id, function(input, output, session) {
    output$message <- renderUI(if (is.null(display$rows())) div(class = "alert alert-warning", UNAVAILABLE))
    output$plot <- renderPlot({
      d <- display$rows(); req(d, nrow(d) > 0)
      location_plot(d)
    })
  })
}
