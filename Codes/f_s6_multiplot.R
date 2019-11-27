id_ordered_by_aq <-
  right_join(per_trial, sub_info) %>%
  mutate(id = fct_reorder(factor(id, ordered = T), aq_total_4)) %>%
  pull(id) %>%
  levels()
f_s6_plot <- function(idx) {
  right_join(per_trial, sub_info) %>%
    filter(id == idx) %>%
    ggplot(aes(x = draw_times)) +
    stat_density(
      aes(color = "1"),
      bw = 1,
      geom = "line"
    ) +
    stat_density(
      aes(
        x = draw_sim_os,
        color = "2"
      ),
      data = sim_draw_os %>% filter(id == idx),
      inherit.aes = F,
      bw = 1,
      geom = "line"
    ) +
    stat_density(
      aes(
        x = draw_sim_ts,
        color = "3",
      ),
      data = sim_draw_ts %>% filter(id == idx),
      inherit.aes = F,
      bw = 1,
      geom = "line"
    ) +
    facet_grid(ratio ~ cost, labeller = as_labeller(
      c(`0.6` = "E: 60/40",
        `0.8` = "E: 80/20",
        `0` = "C: 0",
        `0.1` = "C: 0.1",
        `0.4` = "C: 0.4"
      )), scales = "free_y") +
    scale_color_manual(
      name = "",
      values = c("1" = "#8A8A8D", "2" =  "#00BFFF", "3" = "#FF6F61"),
      labels = c("Data", "One-stage prediction", "Two-stage prediction")
    ) +
    labs(subtitle = paste0("AQ: ", sub_info$aq_total_4[sub_info$id == idx])) +
    theme_neat(base_size = 6, base_family = "Arial") +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          strip.background = element_blank(),
          plot.subtitle = element_text(size = 6),
          panel.spacing = unit(0, "mm"),
          plot.margin = margin(t = 2, r = 2, b = 2, l = 2),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = "none",
          plot.background = element_rect(color = "grey90", size = 0.1)) +
    NULL
}
f_s6_ls <- lapply(id_ordered_by_aq, f_s6_plot)
f_s6_plotInGrid <- map(1:5, ~plot_grid(plotlist = f_s6_ls[1:24 + (.x - 1) * 24], ncol = 4))
walk(1:5, function(.x) {
  save_plot(
    paste0("f_s6_", .x, ".pdf"),
    plot_grid(plotlist = f_s6_ls[1:24 + (.x - 1) * 24], ncol = 4),
    device = cairo_pdf,
    base_width = 210,
    base_height = 297,
    units = "mm"
  )
})
f_s6_legend <- plot_grid(get_legend(f_s6_ls[[1]] + theme(legend.position = "bottom")))
save_plot("f_s6_legend.pdf", f_s6_legend, device = cairo_pdf,
          base_width = 3, base_height = 0.5)
