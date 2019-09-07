theme_neat <- function(base_size = 10,
                       base_family = "") {
  ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = rel(1.2),
        hjust = 0.5
      ),
      panel.background = element_rect(colour = NA),
      plot.background = element_rect(colour = NA),
      panel.border = element_rect(colour = NA),
      axis.title = element_text(face = "bold", size = rel(1)),
      axis.title.y = element_text(angle = 90, vjust = 1),
      axis.title.y.right = element_text(margin = margin(l = base_size)),
      axis.title.x = element_text(vjust = 0),
      axis.text = element_text(size = rel(0.8)),
      axis.line = element_line(colour = "grey70"),
      axis.ticks = element_line(colour = "grey70"),
      panel.spacing = unit(1, "line"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = rel(1)),
      legend.text = element_text(size = rel(0.9)),
      legend.key = element_rect(colour = NA),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = unit(5, "mm"),
      legend.margin = margin(
        t = 0,
        r = 0,
        b = 0,
        l = 0,
        unit = "cm"
      ),
      plot.margin = margin(
        t = 3,
        r = 4,
        b = 3,
        l = 4,
        unit = "mm"
      ),
      # strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
      strip.background = element_rect(colour = NA, fill = NA),
      strip.text = element_text(face = "bold", size = rel(1))
    )
}
