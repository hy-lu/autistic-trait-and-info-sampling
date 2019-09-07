id_ordered_by_aq <-
  right_join(per_trial, sub_info) %>%
  mutate(id = fct_reorder(factor(id, ordered = T), aq_total_4)) %>%
  pull(id) %>%
  levels()
f_s2_plot <- function(idx) {
  ggplot(data = filter(rt_sim, id == idx, model == "ts_decay")) +
    stat_density(aes(x = lg_rt, color = "1"),
                 geom = "line",
                 position = "identity") +
    stat_density(
      aes(x = lg_rt_sim, color = "2"),
      unnest(filter(rt_sim, id == idx, model == "os_costOnly")),
      geom = "line",
      position = "identity",
      inherit.aes = F
    ) +
    stat_density(
      aes(x = lg_rt_sim, color = "3"),
      unnest(filter(rt_sim, id == idx, model == "ts_decay")),
      geom = "line",
      position = "identity",
      inherit.aes = F
    ) +
    scale_x_continuous(limits = c(2.5, 8)) +
    scale_color_manual(
      values = c("1" = "#8A8A8D",
                 "2" = "#00BFFF",
                 "3" = "#FF6F61"),
      labels = c("Data", "One-stage prediction", "Two-stage prediction")
    ) +
    facet_grid(ratio ~ cost, labeller = as_labeller(
      c(
        `0.6` = "E: 60/40",
        `0.8` = "E: 80/20",
        `0` = "C: 0",
        `0.1` = "C: 0.1",
        `0.4` = "C: 0.4"
      )
    ), scales = "free") +
    labs(subtitle = paste0("AQ: ", sub_info$aq_total_4[sub_info$id == idx])) +
    theme_neat(base_size = 6, base_family = "Arial") +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      strip.background = element_blank(),
      plot.subtitle = element_text(size = 6),
      panel.spacing = unit(0, "mm"),
      plot.margin = margin(
        t = 2,
        r = 2,
        b = 2,
        l = 2
      ),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.position = "none",
      plot.background = element_rect(color = "grey90", size = 0.1)
    ) +
    NULL
}
f_s2_ls <- lapply(id_ordered_by_aq, f_s2_plot)
walk(1:5, function(.x) {
  save_plot(
    paste0("f_s2_", .x, ".pdf"),
    plot_grid(plotlist = f_s2_ls[1:24 + (.x - 1) * 24], ncol = 4),
    device = cairo_pdf,
    base_width = 210,
    base_height = 297,
    units = "mm"
  )
})
f_s2_legend <- plot_grid(get_legend(f_s2_ls[[1]] + theme(legend.position = "bottom")))
save_plot("f_s2_legend.pdf", f_s2_legend, device = cairo_pdf,
          base_width = 3, base_height = 0.5)
