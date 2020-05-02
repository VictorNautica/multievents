  # line_plot <- dplyr::select(decathlon_s2p(10.7, 8.02, 16, 2.15, 47.35, 13.6, 46.7, 4.90, 57.7, 280.75), Event, Points, Score)
  #
  # line_plot <- ggplot(line_plot, aes(Event, Points, group = 1, label = Score)) +
  #   geom_point() +
  #   geom_line() +
  #   geom_rect(aes(
  #     xmin = 5.5,
  #     xmax = 1,
  #     ymin = -Inf,
  #     ymax = Inf
  #   ),
  #   alpha = 0.01,
  #   fill = "blue") +
  #   geom_rect(aes(
  #     xmin = 5.5,
  #     xmax = 10,
  #     ymin = -Inf,
  #     ymax = Inf
  #   ),
  #   alpha = 0.01,
  #   fill = "green") +
  #   ggrepel::geom_label_repel(segment.colour = NA) +
  #   theme(text = element_text(family = "Tahoma", size = 18))
  #
  # text_table <- grid.table(decathlon_s2p(10.7, 8.02, 16, 2.15, 47.35, 13.6, 46.7, 4.90, 57.7, 280.75))
  #
  # bar <- select(decathlon_s2p(10.7, 8.02, 16, 2.15, 47.35, 13.6, 46.7, 4.90, 57.7, 280.75), Event, Points)
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
  #   )
  #
  # grid.arrange(line_plot, bar)
