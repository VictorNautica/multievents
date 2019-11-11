#' Create visualisation of decathlon scores
#'
#' \code{decathlon_vis} creates a visualisation for an individual completed decathlon.
#'
#' @param ... Arguments to \code{decathlon_s2p}
#'
#' @export

decathlon_vis <- function(...) {

  extrafont::loadfonts(device = "win", quiet = TRUE)

  line_plot <- select(decathlon_s2p(...), Event, Points, Score)

  line_plot <-
    ggplot(line_plot, aes(Event, Points, group = 1, label = Score)) +
    geom_point() +
    geom_line() +
    geom_rect(
      data = line_plot[1,],
      aes(
        xmin = 5.5,
        xmax = -Inf,
        ymin = -Inf,
        ymax = Inf
      ),
      alpha = 0.2,
      fill = "#fcba03"
    ) +
    geom_rect(
      data = line_plot[1, ],
      aes(
        xmin = 5.5,
        xmax = Inf,
        ymin = -Inf,
        ymax = Inf
      ),
      alpha = 0.2,
      fill = "#29ff69"
    ) +
    scale_y_continuous(breaks = seq(0,2000,100)) +
    ggrepel::geom_label_repel(segment.colour = NA,
                              family = "Segoe UI Light") +
    theme(text = element_text(family = "Segoe UI Light", size = 18), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

  return(line_plot)

  # text_table <- grid.table(decathlon_s2p(...))

  # bar <- select(decathlon_s2p(...), Event, Points)
  #
  # bar <-
  #   ggplot(bar, aes(
  #     x = "",
  #     y = Points,
  #     fill = fct_rev(Event)
  #   )) +
  #   geom_bar(
  #     stat = "identity",
  #     position = "fill",
  #     alpha = 0.75,
  #     width = 0.1
  #   ) +
  #   scale_fill_manual(name = "Event",
  #                     values = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(10)) +
  #   labs(y = "Percentage of\ntotal score") +
  #   scale_y_continuous(
  #     breaks = seq(0, 100, 0.1),
  #     labels = function(x)
  #       paste0(x * 100, "%")
#     )
# ), bar)

}

## decathlon_vis(11.01, 8.02, 16, 2.15, 47.35, 13.6, 46.7, 4.90, 57.7, 300)
## always problem with the example? why?
