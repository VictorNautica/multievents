#' Create visualisation of decathlon scores
#'
#' \code{decathlon_vis} creates a visualisation for an individual completed decathlon.
#'
#' @param ... Arguments to \code{decathlon_s2p}
#'
#' @export

decathlon_vis <- function(...) {

  convenient_table <- dplyr::select(decathlon_s2p(...), Event, Points, Score, `Average Points`)
  convenient_table <- convenient_table %>% rename(`Individual Points` = "Points") %>% pivot_longer(cols = c(`Individual Points`, `Average Points`))

  myplot <-
    ggplot(convenient_table,
           aes(
             Event,
             value,
             group = name,
             label = Score,
             colour = name
           )) +
    annotation_raster(
      alpha("darkgrey", .25),
      xmin = 5.5,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf
    ) + ## from https://github.com/tidyverse/ggplot2/issues/3184
    geom_point(aes(colour = name)) +
    geom_line(aes(colour = name)) +
    scale_y_continuous(breaks = seq(0, 2000, 100),
                       limits = c(min(convenient_table$value)-100, max(convenient_table$value, na.rm = T)+100)) +
    scale_x_discrete(labels = function(x) str_replace_all(x, " ", "\n")) +
    ggrepel::geom_label_repel(
      data = function(x)
        x[seq(1, 20, by = 2),],
      colour = "black",
      vjust = -9999,
      segment.color = NA,
      family = "Segoe UI"
    ) +
    theme(
      text = element_text(family = "Segoe UI", size = 18),
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    labs(y = "Points")

  return(myplot)

}

## decathlon_vis(11.01, 8.02, 16, 2.15, 47.35, 13.6, 46.7, 4.90, 57.7, 300)
## always problem with the example? why?
