## Figure number may not be in accordance with the number in the manuscript
library(showtext)
library(colorspace)
library(cowplot)
library(here)
source(here("Codes", "theme_neat.R"))
font_size <- 10
# Please try to import Arial fonts, otherwise following codes might not be able to execute
font_add("Arial", regular = "/usr/share/fonts/TTF/arial.ttf",
         bold = "/usr/share/fonts/TTF/arialbd.ttf",
         italic = "/usr/share/fonts/TTF/ariali.ttf",
         bolditalic = "/usr/share/fonts/TTF/arialbi.ttf")
showtext_auto()

# Figure 1: Task design ----
## Figure 1b (new): Optimal Curve + Remaining rewrads + Probability of Correctness====
reward_df <- tibble(
  draw = rep(0:20, each = 6),
  ratio = rep(rep(c(0.6, 0.8), each = 3), times = 21),
  cost = rep(rep(c(0, 0.1, 0.4), times = 2), times = 21),
  reward = 10 - draw * cost
)
prob_hit <- function(n, r) {
  if ((n %% 2) == 1) {
    p <- pbinom((n - 1) / 2, n, r, lower.tail = F)
  } else{
    p <- 0.5 * dbinom(n / 2, n, r) + pbinom(n / 2, n, r, lower.tail = F)
  }
  return(p)
}
prob_df <- tibble(
  draw = rep(0:20, each = 6),
  ratio = rep(rep(c(0.6, 0.8), each = 3), times = 21),
  cost = rep(rep(c(0, 0.1, 0.4), times = 2), times = 21)
) %>%
  group_by(draw, ratio, cost) %>%
  mutate(p = prob_hit(draw, ratio) * 10) %>%
  ungroup
source(here("Codes", "expected_gain_fun.R"))
opt_df <- tibble(
  draw = rep(0:20, each = 6),
  ratio = rep(rep(c(0.6, 0.8), each = 3), times = 21),
  cost = rep(rep(c(0, 0.1, 0.4), times = 2), times = 21)
) %>% mutate(eg = pmap_dbl(list(
  n = draw,
  r = ratio,
  c = cost
), EG))
eg_reward_prob_df <-
  left_join(opt_df, reward_df) %>%
  left_join(prob_df) %>%
  gather("vars", "vals", eg:p)
eg_perPart_df <- efficiency %>%
  group_by(id, cost, ratio) %>%
  summarise(
    draw = mean(draw_times),
    eg = mean(gain),
    eff = mean(efficiency)
  ) %>%
  ungroup() %>%
  left_join(sub_info, ., by = "id")
f1_b <- ggplot(eg_reward_prob_df, aes(x = draw, y = vals)) +
  geom_line(aes(linetype = vars, group = vars),
            color = "#555459",
            size = 0.5) +
  geom_point(
    data = eg_perPart_df,
    aes(y = eg, color = aq_total_4),
    shape = 1,
    stroke = 0.3,
    size = 1.3
  ) +
  # stat_ellipse(data = eg_perPart_df, aes(color = aq_group)) +
  scale_color_continuous_sequential(name = "AQ score", palette = "Purples 3") +
  scale_linetype_manual(
    name = NULL,
    labels = c(
      "eg" = "Expected gain",
      "p" = "P[Correct]",
      "reward" = "Remaining points"
    ),
    values = c("solid", "dashed", "dotted")
  ) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 10,
                                         name = "P[Correct]")) +
  facet_grid(cost ~ ratio, labeller = as_labeller(
    c(
      `0.6` = "Evidence: 60/40",
      `0.8` = "Evidence: 80/20",
      `0` = "Cost: 0",
      `0.1` = "Cost: 0.1",
      `0.4` = "Cost: 0.4"
    )
  )) +
  labs(x = "Number of bead samples", y = "Bonus points") +
  theme_neat(base_size = font_size, base_family = "Arial")
## Build up Figure 1 ====
f1_b_main <- f1_b + theme(legend.position = "none")
f1_b_legend <-
  get_legend(f1_b + theme(legend.position = "right", legend.direction = "vertical"))
f1_a_raw <-
  magick::image_read("fig1_a.tif")
f1_a_info <-
  magick::image_info(f1_a_raw) %>% mutate(density_w = as.numeric(str_split(
    density, pattern = "x", simplify = T
  ))[1],
  density_h = as.numeric(str_split(
    density, pattern = "x", simplify = T
  ))[2])
f1_a <-
  ggdraw() + draw_image(f1_a_raw, scale = 0.9)
f1_b <-
  plot_grid(
    f1_b_main,
    f1_b_legend,
    nrow = 1,
    rel_widths = c(
      1.5,
      0.25 * 1.5
    )
  )
f1 <- plot_grid(
  f1_a,
  f1_b,
  labels = "auto",
  label_fontfamily = "Arial",
  label_size = 12,
  nrow = 2,
  ncol = 1,
  align = "h",
  axis = "l",
  rel_heights = c((f1_a_info$height / f1_a_info$density_h) / (6.5 / 3 * 2), 1.5)
)
save_plot("f1.pdf",
          f1,
          base_height = (6.5 / 3 * 2 * 1.5 + (f1_a_info$height / f1_a_info$density_h)) *
            (6.5 / (f1_a_info$width / f1_a_info$density_w)),
          base_width = 6.5)

# Figure 2: Behavior results ----

## Column 1: Efficiency ----

### Row 1:
f2_c1_r1 <-
  eff_data_scaled_filtered %>%
  ggplot(aes(x = cost, y = efficiency, color = ratio)) +
  stat_summary(
    fun.data = mean_se,
    na.rm = T,
    fatten = 1,
    size = 0.8,
    position = position_dodge(width = 0.1)
  ) +
  stat_summary(
    fun.y = mean,
    na.rm = T,
    geom = "line",
    aes(group = ratio),
    size = 0.8,
    position = position_dodge(width = 0.1)
  ) +
  ggtitle("Efficiency") +
  scale_x_discrete(name = "Cost") +
  scale_y_continuous(name = "",
                     labels = scales::percent) +
  scale_color_manual(
    name = "Evidence",
    values = c(`0.6` = "#e3b9bd", `0.8` = "#BB525B"),
    labels = c(`0.6` = "60/40", `0.8` = "80/20")
  ) +
  theme_neat(base_size = font_size, base_family = "Arial") +
  theme(
    legend.position = "none",
    plot.margin = margin(
      t = 2,
      r = 1,
      b = 0,
      l = 1,
      unit = "pt"
    ),
    plot.title = element_text(size = 12)
  )

### Row 2:
f2_c1_r2 <-
  eff_ns_coef %>%
  ggplot(aes(
    x = ratio,
    y = estimate,
    ymin = estimate - SE,
    ymax = estimate + SE,
    color = cost
  )) +
  geom_pointrange(position = position_dodge(width = 0.3),
                  fatten = 1,
                  size = 0.8) +
  geom_point(
    aes(
      x = ratio,
      y = sig_y,
      shape = sig,
      group = cost
    ),
    color = "darkorange2",
    size = 1,
    position = position_dodge(width = 0.3),
    na.rm = T,
    inherit.aes = F
  ) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  # ggtitle("Efficiency") +
  scale_x_discrete(name = "Evidence",
                   labels = c(`0.6` = "60/40", `0.8` = "80/20")) +
  scale_y_continuous(name = expression(bold(Beta[AQ])),
                     labels = my_percent) +
  scale_color_manual(name = "Cost",
                     values = c("#A4D289", "#6ABB45", "#0E8040")) +
  scale_shape_identity() +
  theme_neat(base_size = font_size, base_family = "Arial") +
  theme(
    legend.position = "none",
    plot.margin = margin(
      t = 1,
      r = 1,
      b = 2,
      l = 1,
      unit = "pt"
    ),
    axis.title.y = element_text(vjust = -2)
  )

## Column 2: Number of bead samples ----

### Row 1:
f2_c2_r1 <-
  num_bead_dat_scaled_filtered %>%
  ggplot(aes(x = cost, y = draw_times, color = ratio)) +
  stat_summary(
    data = efficiency,
    mapping = aes(y = opt_draw, group = ratio,
                  linetype = "dashed"),
    fun.y = mean,
    na.rm = T,
    geom = "line",
    size = 0.8
  ) +
  stat_summary(
    fun.data = mean_se,
    na.rm = T,
    fatten = 1,
    size = 0.8,
    position = position_dodge(width = 0.1)
  ) +
  stat_summary(
    fun.y = mean,
    na.rm = T,
    geom = "line",
    mapping = aes(group = ratio),
    size = 0.8,
    position = position_dodge(width = 0.1)
  ) +
  ggtitle("Number of Bead Samples") +
  scale_x_discrete(name = "Cost") +
  scale_y_continuous(name = "") +
  scale_color_manual(
    name = "Evidence",
    values = c(`0.6` = "#e3b9bd", `0.8` = "#BB525B"),
    labels = c(`0.6` = "60/40", `0.8` = "80/20")
  ) +
  scale_linetype_identity(name = "Optimal number ", guide = "legend") +
  theme_neat(base_size = font_size, base_family = "Arial") +
  theme(
    legend.position = "none",
    plot.margin = margin(
      t = 2,
      r = 1,
      b = 0,
      l = 1,
      unit = "pt"
    ),
    plot.title = element_text(size = 12)
  )

### Row 2:
f2_c2_r2 <- draw_ns_coef %>%
  ggplot(aes(
    x = ratio,
    y = estimate,
    ymin = estimate - SE,
    ymax = estimate + SE,
    color = cost
  )) +
  geom_pointrange(position = position_dodge(width = 0.3),
                  fatten = 1,
                  size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_point(
    aes(
      x = ratio,
      y = sig_y,
      shape = sig,
      group = cost
    ),
    color = "darkorange2",
    size = 1,
    position = position_dodge(width = 0.3),
    na.rm = T,
    inherit.aes = F
  ) +
  # ggtitle("Number of bead samples") +
  scale_x_discrete(name = "Evidence",
                   labels = c(`0.6` = "60/40", `0.8` = "80/20")) +
  scale_y_continuous(name = expression(bold(Beta[AQ]))) +
  scale_color_manual(name = "Cost",
                     values = c("#A4D289", "#6ABB45", "#0E8040")) +
  scale_shape_identity() +
  theme_neat(base_size = font_size, base_family = "Arial") +
  theme(
    legend.position = "none",
    plot.margin = margin(
      t = 1,
      r = 1,
      b = 2,
      l = 1,
      unit = "pt"
    ),
    axis.title.y = element_text(vjust = -2)
  )

## Column 3: Standard Deviation of Bead Samples ----

### Row 1:
f2_c3_r1 <-
  draw_var_dat_scaled_filtered %>%
  ggplot(aes(x = cost, y = sd , color = ratio)) +
  stat_summary(
    fun.data = mean_se,
    na.rm = T,
    fatten = 1,
    size = 0.8,
    position = position_dodge(width = 0.1)
  ) +
  stat_summary(
    fun.y = mean,
    na.rm = T,
    geom = "line",
    aes(group = ratio),
    size = 0.8,
    position = position_dodge(width = 0.1)
  ) +
  ggtitle("Sampling Variability") +
  scale_x_discrete(name = "Cost") +
  scale_y_continuous(name = "") +
  scale_color_manual(
    name = "Evidence",
    values = c(`0.6` = "#e3b9bd", `0.8` = "#BB525B"),
    labels = c(`0.6` = "60/40", `0.8` = "80/20")
  ) +
  theme_neat(base_size = font_size, base_family = "Arial") +
  theme(
    legend.position = "none",
    plot.margin = margin(
      t = 2,
      r = 1,
      b = 0,
      l = 1,
      unit = "pt"
    ),
    plot.title = element_text(size = 12)
  )

f2_c3_r2 <-
  sd_ns_coef %>%
  ggplot(aes(
    x = ratio,
    y = estimate,
    ymin = estimate - SE,
    ymax = estimate + SE,
    color = cost
  )) +
  geom_pointrange(position = position_dodge(width = 0.3),
                  fatten = 1,
                  size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_point(
    aes(
      x = ratio,
      y = sig_y,
      shape = sig,
      group = cost
    ),
    color = "darkorange2",
    size = 1,
    position = position_dodge(width = 0.3),
    na.rm = T,
    inherit.aes = F
  ) +
  # ggtitle("Sampling variability") +
  scale_x_discrete(name = "Evidence",
                   labels = c(`0.6` = "60/40", `0.8` = "80/20")) +
  scale_y_continuous(name = expression(bold(Beta[AQ]))) +
  scale_color_manual(name = "Cost",
                     values = c("#A4D289", "#6ABB45", "#0E8040")) +
  scale_shape_identity() +
  theme_neat(base_size = font_size, base_family = "Arial") +
  theme(
    legend.position = "none",
    plot.margin = margin(
      t = 1,
      r = 1,
      b = 2,
      l = 1,
      unit = "pt"
    ),
    axis.title.y = element_text(vjust = -2)
  )

## Assemble Figure 2 ----

f2_main <-
  plot_grid(
    f2_c1_r1,
    f2_c2_r1,
    f2_c3_r1,
    f2_c1_r2,
    f2_c2_r2,
    f2_c3_r2,
    nrow = 2,
    ncol = 3,
    align = "hv",
    axis = "tblr",
    labels = "auto",
    label_fontfamily = "Arial",
    label_size = 12,
    label_x = 0.05
  )
f2_lengend <-
  plot_grid(
  get_legend(
    f2_c1_r1 +
      guides(color = guide_legend(override.aes = list(size = 0.3))) +
      theme(legend.position = "bottom")
  ),
  get_legend(
    f2_c2_r1 +
      guides(color = "none", linetype = guide_legend(title = "Optimal number ", label = F, keywidth = 3)) +
      theme(legend.position = "bottom")
  ),
  get_legend(
    f2_c2_r2 +
      guides(color = guide_legend(override.aes = list(size = 0.3))) +
      theme(legend.position = "bottom")
  ),
  nrow = 1)
save_plot(
  "f2.pdf",
  plot_grid(
    f2_main,
    f2_lengend,
    nrow = 2,
    rel_heights = c(1, 0.1)
  ),
  base_width = 9,
  base_height = 6,
  device = cairo_pdf
)

# Figure 3 ----
library(mixtools)
set.seed(123)
rt_bimodal <- rt_w0_outlier %>%
  .$lg_rt %>%
  normalmixEM()
bw_rt <- rt_w0_outlier %>%
  group_by(cost, ratio) %>%
  summarise(bw = provenance::botev(lg_rt)) %>%
  ungroup()
plot_mix_comps <- function(x, mu, sigma, lam) {
  lam * dnorm(x, mu, sigma)
}
f3_conds <- rt_w0_outlier %>%
  ggplot(aes(x = lg_rt)) +
  geom_line(
    stat = "density",
    adjust = 4,
    bw = mean(bw_rt$bw),
    size = 0.5
  ) +
  scale_x_continuous(breaks = log(100 * c(1, 4, 16)),
                     labels = 100 * c(1, 4, 16)) +
  coord_cartesian(xlim = c(3.5, 8),
                  ylim = c(0, 1.4),
                  expand = FALSE) +
  scale_linetype_manual(name = "", values = "dotted") +
  labs(x = "DT (ms)",
       y = "Density",
       color = "AQ Group") +
  facet_grid(cost ~ ratio, labeller = as_labeller(
    c(
      `0.6` = "E: 60/40",
      `0.8` = "E: 80/20",
      `0` = "C: 0",
      `0.1` = "C: 0.1",
      `0.4` = "C: 0.4"
    )
  )) +
  theme_neat(base_size = font_size, base_family = "Arial") + theme(
    axis.title = element_blank(),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(
      t = 0,
      r = 0,
      b = 0,
      l = 0,
      unit = "mm"
    )
  )
f3_bimodal <- tibble(x = rt_bimodal$x) %>%
  ggplot() +
  geom_histogram(
    aes(x, ..density..),
    binwidth = 0.1,
    color = "#8A8A8D",
    fill = "white",
    size = 0.2
  ) +
  stat_function(
    geom = "line",
    fun = plot_mix_comps,
    args = list(
      mu = rt_bimodal$mu[1],
      sigma = rt_bimodal$sigma[1],
      lam = rt_bimodal$lambda[1]
    ),
    color = "#555459",
    size = 1.5
  ) +
  stat_function(
    geom = "line",
    fun = plot_mix_comps,
    args = list(
      mu = rt_bimodal$mu[2],
      sigma = rt_bimodal$sigma[2],
      lam = rt_bimodal$lambda[2]
    ),
    color = "#555459",
    size = 1.5
  ) +
  stat_function(
    geom = "line",
    fun = function(x, mu, sigma, lam) {
      lam[1] * dnorm(x, mu[1], sigma[1]) +
        lam[2] * dnorm(x, mu[2], sigma[2])
    },
    args = list(
      mu = rt_bimodal$mu,
      sigma = rt_bimodal$sigma,
      lam = rt_bimodal$lambda
    ),
    color = "#C8C7CA",
    size = 0.5
  ) +
  scale_x_continuous(breaks = log(100 * c(0.5, 1, 2, 4, 8, 16)),
                     labels = 100 * c(0.5, 1, 2, 4, 8, 16)) +
  coord_cartesian(xlim = c(3.5, 8), expand = FALSE) +
  labs(x = "DT (ms)", y = "Density") +
  theme_neat(base_size = font_size, base_family = "Arial")

f3_a <-
  ggdraw() +
  draw_plot(f3_bimodal, 0, 0, 1, 1) +
  draw_plot(f3_conds, 0.475, 0.475, 0.475, 0.475)

f3_b <-
  avg_rt_w0_outlier %>%
  ggplot(aes(x = cost, y = lg_rt, color = ratio)) +
  stat_summary(
    fun.data = mean_se,
    na.rm = T,
    fatten = 1,
    size = 0.8,
    position = position_dodge(width = 0.1)
  ) +
  stat_summary(
    fun.y = mean,
    na.rm = T,
    geom = "line",
    aes(group = ratio),
    size = 0.8,
    position = position_dodge(width = 0.1)
  ) +
  scale_x_discrete(name = "Cost") +
  scale_y_continuous(
    name = "DT (ms)",
    breaks = log(seq(150, 400, by = 50)),
    labels = seq(150, 400, by = 50)
  ) +
  scale_color_manual(
    name = "Evidence",
    values = c(`0.6` = "#e3b9bd", `0.8` = "#BB525B"),
    labels = c(`0.6` = "60/40", `0.8` = "80/20")
  ) +
  theme_neat(base_size = font_size, base_family = "Arial") +
  theme(legend.position = "none",
        plot.margin = margin(
          t = 2,
          r = 4,
          b = 0,
          l = 2,
          unit = "pt"
        ))

f3_c <-
  rt_ns_coef %>%
  ggplot(aes(
    x = ratio,
    y = estimate,
    ymin = estimate - SE,
    ymax = estimate + SE,
    color = cost
  )) +
  geom_pointrange(position = position_dodge(width = 0.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(
    aes(
      x = ratio,
      y = sig_y,
      shape = sig,
      group = cost
    ),
    color = "#9932CC",
    size = 1,
    position = position_dodge(width = 0.2),
    na.rm = T,
    inherit.aes = F
  ) +
  scale_x_discrete(name = "Evidence",
                   labels = c(`0.6` = "60/40", `0.8` = "80/20")) +
  scale_y_continuous(name = expression(bold(Beta[AQ]))) +
  scale_color_manual(name = "Cost",
                     values = c("#A4D289", "#6ABB45", "#0E8040")) +
  scale_shape_identity() +
  theme_neat(base_size = font_size, base_family = "Arial") +
  theme(legend.position = "none",
        plot.margin = margin(
          t = 1,
          r = 4,
          b = 2,
          l = 2,
          unit = "pt"
        ))

### Assemble Figure 3 ----
f3_bc_main <-
  plot_grid(
    f3_b,
    f3_c,
    ncol = 1,
    nrow = 2,
    align = "v",
    axis = "lr",
    rel_heights = c(1, 1.1),
    labels = c("b", "c"),
    label_fontfamily = "Arial",
    label_size = 12
  )
f3_bc <- plot_grid(
  f3_bc_main,
  plot_grid(
    get_legend(
      f3_b +
        guides(color = guide_legend(
          override.aes = list(size = 0.3), ncol = 1
        )) +
        theme(legend.position = "bottom")
    ),
    get_legend(
      f3_c +
        guides(color = guide_legend(override.aes = list(size = 0.3))) +
        theme(legend.position = "bottom")
    ),
    ncol = 2
  ),
  ncol = 1,
  nrow = 2,
  rel_heights = c(1, 0.1)
)
f3 <- plot_grid(f3_a, f3_bc, rel_widths = c(1.2, 1),
                labels = c("a", ""),
                label_fontfamily = "Arial", label_size = 12)
save_plot("f3.pdf",
          f3,
          base_width = 9,
          base_height = 6)

# Figure 4 ----
load(here("Data", "sim_draw_exp_os_costOnly_20190827.RData"))
load(here("Data", "sim_draw_exp_ts_decay_20190827.RData"))

sim_draw_os <-
  sim_draw_exp_os %>%
  map(bind_rows) %>%
  bind_rows() %>%
  left_join(mutate_at(per_trial, "trial", as.integer))
sim_draw_ts <-
  sim_draw_exp_ts %>%
  map(bind_rows) %>%
  bind_rows() %>%
  left_join(mutate_at(per_trial, "trial", as.integer))

f4_b <-
  choice_rt_groupbmc %>%
  filter(info_crit == "AICc") %>%
  left_join(chc_rt_delAICc) %>%
  mutate(model = map_chr(model, replace_model_name),
         model = fct_reorder(as.factor(model), del_AICc)) %>%
  ggplot(aes(model, del_AICc, color = alpha)) +
  geom_segment(aes(
    xend = model,
    y = 0,
    yend = del_AICc
  ), color = "#8A8A8D") +
  geom_point(size = 3) +
  annotate(
    x = 1,
    y = 15220,
    hjust = -0.1,
    label = "Pexc > 99.9%",
    geom = "text",
    color = "black",
    family = "Arial"
  ) +
  scale_color_continuous_sequential(palette = "Viridis",
                                   guide = guide_colorbar(title.position = "top",
                                                          title.theme = element_text(family = "Arial",
                                                                                     size = 9),
                                                          title.hjust = 1,
                                                          direction = "horizontal",
                                                          barwidth = unit(80, "pt"),
                                                          barheight = unit(8, "pt"))) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .05))) +
  labs(x = "", y = "Summed \u0394AICc", color = "Estimated model frequency") +
  coord_flip() +
  theme_neat(base_size = font_size, base_family = "Arial") +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_blank(),
        legend.position = c(0.8, 0.15))
f4_c <- ggplot(semi_join(per_trial, sub_info), aes(x = draw_times)) +
  geom_histogram(aes(y = ..density..,
    color = "1"
  ),
  fill = "white",
  binwidth = 2) +
  stat_density(
    aes(
      x = draw_sim_os,
      color = "2"
    ),
    size = 0.8,
    data = sim_draw_os,
    inherit.aes = F,
    bw = 1,
    geom = "line"
  ) +
  stat_density(
    aes(
      x = draw_sim_ts,
      color = "3",
    ),
    size = 0.8,
    data = sim_draw_ts,
    inherit.aes = F,
    bw = 1,
    geom = "line"
  ) +
  facet_grid(ratio ~ cost, labeller = as_labeller(
    c(
      `0.6` = "Evidence: 60/40",
      `0.8` = "Evidence: 80/20",
      `0` = "Cost: 0",
      `0.1` = "Cost: 0.1",
      `0.4` = "Cost: 0.4"
    )
  )) +
  scale_y_continuous(
    breaks = c(0, 0.1, 0.2)
  ) +
  scale_color_manual(
    name = "",
    values = c("1" = "#8A8A8D", "2" =  "#00BFFF", "3" = "#FF6F61"),
    labels = c("Data", "One-stage prediction", "Two-stage prediction")
  ) +
  labs(x = "# Bead samples",
       y = "Density") +
  theme_neat(base_size = font_size, base_family = "Arial") +
  theme(axis.line.y = element_blank(), legend.position = "none") +
  NULL

source(here("Codes", "rt_model_simulation.R"))
f4_d <-
  ggplot() +
  geom_histogram(
    aes(x = lg_rt, y = ..density.., color = "1"),
    data = filter(rt_sim, model == "ts_decay"),
    binwidth = 0.2,
    fill = "white"
  ) +
  stat_density(aes(x = lg_rt_sim, color = "2"),
               unnest(filter(rt_sim, model == "os_costOnly")),
               size = 0.8,
               geom = "line",
               position = "identity",
               inherit.aes = F
  ) +
  stat_density(aes(x = lg_rt_sim, color = "3"),
               unnest(filter(rt_sim, model == "ts_decay")),
               size = 0.8,
               geom = "line",
               position = "identity",
               inherit.aes = F
  ) +
  scale_x_continuous(breaks = log(100 * c(1, 4, 16)),
                     labels = 100 * c(1, 4, 16),
                     limits = c(3.5, 8)) +
  scale_color_manual(
    values = c("1" = "#8A8A8D",
               "2" = "#00BFFF",
               "3" = "#FF6F61"),
    labels = c("Data", "One-stage prediction", "Two-stage prediction")
  ) +
  facet_grid(ratio ~ cost, labeller = as_labeller(
    c(
      `0.6` = "Evidence: 60/40",
      `0.8` = "Evidence: 80/20",
      `0` = "Cost: 0",
      `0.1` = "Cost: 0.1",
      `0.4` = "Cost: 0.4"
    )
  )) +
  labs(x = "DT (ms)",
       y = "Density",
       color = "") +
  theme_neat(base_size = font_size, base_family = "Arial") +
  theme(axis.line.y = element_blank(), legend.position = "none") +
  NULL
### Assemble Figure 4 ----
f4_a_pdf <- magick::image_read_pdf("f4_a.pdf", density = 300)
f4_legend <- get_legend(f4_d + theme(legend.position = "bottom"))
f4_main_noA <- plot_grid(NULL, f4_b, f4_c, f4_d, labels = "auto",
                         label_fontfamily = "Arial",
                         rel_heights = c(1.1, 1), nrow = 2, label_size = 12)
f4_noA <- plot_grid(f4_main_noA, f4_legend, nrow = 2, rel_heights = c(1, 0.05))
f4 <-
  ggdraw(f4_noA) +
  draw_image(f4_a_pdf, 0, 0.50, 0.5, 0.5)
save_plot("f4.pdf", f4, base_height = 8, base_width = 12, device = cairo_pdf)


# Figure 5 ----
source(here("Codes", "cor_txt_fn.R"))
ptr_rt <- mod_par_ls$ts_decay %>% left_join(sub_info) %>%
  dplyr::select(id, ptr0:ptr4) %>%
  gather(key = "ptr", value = "prob", ptr0:ptr4) %>%
  mutate(cost = dplyr::recode(
    ptr,
    "ptr0" = "0",
    "ptr1" = "0.1",
    "ptr4" = "0.4"
  )) %>%
  dplyr::select(-ptr) %>%
  left_join(rt_w0_outlier %>% group_by(id, cost) %>% summarise_at("lg_rt", "mean"))

f5_1_main <- ptr_rt %>%
  ggplot(aes(
    x = prob,
    y = lg_rt,
    color = cost,
    fill = cost
  )) +
  geom_point(alpha = 0.4,
             size = 1.3,
             shape = 19) +
  geom_smooth(method = "lm",
              alpha = 0.2,
              size = 1.1) +
  ggpubr::stat_cor(
    method = "spearman",
    label.y = 0.87,
    size = 3,
    color = "#555459",
    show.legend = F
  ) +
  labs(x = "Second-thought probability",
       y = "% Slow decision",
       color = "Cost",
       fill = "Cost") +
  scale_color_manual(values = c("#A4D289", "#6ABB45", "#0E8040", "#003427"),
                     labels = c("0", "0.1", "0.4", "All")) +
  scale_fill_manual(values = c("#A4D289", "#6ABB45", "#0E8040", "#003427"),
                    labels = c("0", "0.1", "0.4", "All")) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_grid( ~ cost,
              scales = "free_x",
              labeller = as_labeller(
                c(
                  "0" = "Cost: 0",
                  "0.1" = "Cost: 0.1",
                  "0.4" = "Cost: 0.4",
                  "(all)" = "All"
                )
              ),
              margins = T) +
  theme_neat(font_size, base_family = "Arial") +
  theme(panel.spacing = unit(1, "lines"),
        legend.position = "none")
f5_legend <-
  get_legend(f5_1_main + theme(legend.position = "bottom"))

f5_2_cortxt_sep <- ptr_rt %>%
  split(.$cost) %>%
  map_df( ~ {
    cor_txt(.x, prob, lg_rt, method = "spearman") %>% mutate(x = min(.x$prob))
  },
  .id = "cost") %>%
  mutate(y = log(1200))
f5_2_cortxt_all <-
  cor_txt(ptr_rt, prob, lg_rt, method = "spearman") %>% mutate(x = min(ptr_rt$prob)) %>%
  mutate(y = log(1200))

f5_2_main_sep <-
  ptr_rt %>%
  ggplot(aes(
    x = prob,
    y = lg_rt,
    color = cost,
    fill = cost
  )) +
  geom_point(alpha = 0.4,
             size = 1.3,
             shape = 19) +
  geom_smooth(method = "lm",
              alpha = 0.2,
              size = 1.1) +
  geom_text(
    aes(x = x, y = y, label = text),
    data = f5_2_cortxt_sep,
    parse = T,
    size = 3*1.5,
    color = "#555459",
    hjust = 0
  ) +
  labs(x = "",
       y = "DT (ms)",
       color = "Cost",
       fill = "Cost") +
  scale_color_manual(values = c("#A4D289", "#6ABB45", "#0E8040")) +
  scale_fill_manual(values = c("#A4D289", "#6ABB45", "#0E8040")) +
  scale_y_continuous(breaks = log(c(100, 200, 400, 800, 1600)), labels = c(100, 200, 400, 800, 1600)) +
  facet_grid( ~ cost, scales = "free_x", labeller = as_labeller(c(
    `0` = "Cost: 0", `0.1` = "Cost: 0.1", `0.4` = "Cost: 0.4"
  ))) +
  theme_neat(font_size, base_family = "Arial") +
  theme(panel.spacing = unit(1, "lines"),
        legend.position = "none",
        axis.title.x = element_blank())
f5_2_main_all <-
  ptr_rt %>%
  ggplot(aes(x = prob,
             y = lg_rt)) +
  geom_point(
    aes(color = cost,
        fill = cost),
    alpha = 0.4,
    size = 1.3,
    shape = 19
  ) +
  geom_smooth(
    method = "lm",
    alpha = 0.2,
    size = 1.1,
    color = "#003427",
    fill = "#003427"
  ) +
  geom_text(
    aes(x = x, y = y, label = text),
    data = f5_2_cortxt_all,
    parse = T,
    size = 3*1.5,
    color = "#555459",
    hjust = 0
  ) +
  labs(x = "Second-thought probability",
       y = "DT (ms)",
       color = "Cost",
       fill = "Cost") +
  scale_color_manual(values = c("#A4D289", "#6ABB45", "#0E8040")) +
  scale_fill_manual(values = c("#A4D289", "#6ABB45", "#0E8040")) +
  scale_y_continuous(breaks = log(c(100, 200, 400, 800, 1600)), labels = c(100, 200, 400, 800, 1600)) +
  facet_grid( ~ 1, scales = "free_x", labeller = as_labeller(c(`1` = "All"))) +
  theme_neat(font_size, base_family = "Arial") +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 0,
      unit = "mm"
    )
  )
f5_2_main <-
  plot_grid(
    f5_2_main_sep,
    f5_2_main_all,
    align = "h",
    axis = "tb",
    rel_widths = c(3, 1)
  )
save_plot(
  "f5.pdf",
  ggdraw(plot_grid(
    f5_2_main,
    f5_legend,
    nrow = 2,
    rel_heights = c(1, 0.1)
  )) + draw_label("Second-thought probability", y = 0.1, size = 10, vjust = 0, fontface = "bold", fontfamily = "Arial"),
  base_width = 12,
  base_height = 4,
  device = cairo_pdf
)

# Figure 6 ----
source(here("Codes", "corr.test2.R"))
f6 <-
  famwise_aicc_diff %>%
  select(id, ts_vs_ts2) %>%
  left_join(eff_wide) %>% {
    corr.test2(
      select(., `0_0.6`:`0.4_0.8`),
      select(., contains("_vs_")),
      method = "spearman", adjust = "fdr"
    )
  } %>% {left_join(rownames_to_column(.$ci, "cost_ratio"), .$ci.adj)} %>%
  mutate(cost_ratio = str_extract(cost_ratio, ".*(?=-)"),
         cost_ratio = str_c("C:", cost_ratio),
         cost_ratio = str_replace(cost_ratio, "_", ", E:")) %>%
  mutate(
    sig = if_else(p < .05, "*p < .05", if_else(p < .1, "+p < .1", "p >= .1"))
  ) %>%
  ggplot(aes(
    cost_ratio,
    r,
    ymin = lower.adj,
    ymax = upper.adj,
    color = sig
  )) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(fatten = 1, size = 0.8) +
  coord_flip() +
  scale_color_manual(values = c("p >= .1" = "#0D0887FF",
                                "+p < .1" = "#B12A90FF",
                                "*p < .05" = "#FCA636FF"),
                     guide = guide_legend(override.aes = list(size = 0.4))) +
  labs(x = "", y = expression(paste("Corr", group("(",list(plain(AICc)[Cost %->% Evidence] - plain(AICc)[Evidence %->% Cost], plain(Efficiency)), ")"))), color = "") +
  theme_neat(font_size, base_family = "Arial") +
  theme(axis.title.y = element_blank(),
        legend.position = "right",
        legend.direction = "vertical",
        plot.margin = margin(3, 2, 3, 2, unit = "mm"))
# Figure 7 ----
source(here("Codes", "aiccDif_AQ_clust_perm.R"))
# Extra 1 ----
f_e1_cortxt <-
    famwise_aicc_diff %>%
    select(id, ts_vs_ts2) %>%
    left_join(sub_info) %>% cor_txt(ts_vs_ts2, aq_total_4, method = "spearman") %>%
    mutate(x = -3000, y = 50)
  f_e1 <-
    famwise_aicc_diff %>%
    select(id, ts_vs_ts2) %>%
    left_join(sub_info) %>%
    ggplot(aes(x = ts_vs_ts2, y = aq_total_4)) +
    geom_point() +
    geom_smooth(method = "lm") +
    geom_text(
      aes(x = x, y = y, label = text),
      data = f_e1_cortxt,
      parse = T,
      size = 3,
      color = "#555459",
      hjust = 0.5,
      family = "Arial"
    ) +
    labs(x = expression(AICc[Cost %->% Evidence] - AICc[Evidence %->% Cost]),
         y = "AQ",
         parse = T) +
    theme_neat(font_size, base_family = "Arial")
f_67e1 <-
  plot_grid(
    plot_grid(
      f_e1,
      f6,
      align = "v",
      axis = "tb",
      nrow = 1,
      labels = c("a", "b"),
      label_size = 12,
      label_fontfamily = "Arial"
    ),
    f7_2,
    rel_heights = c(1, 1.2),
    align = "h",
    axis = "lr",
    nrow = 2,
    labels = c("", "c"),
    label_size = 12,
    label_fontfamily = "Arial"
  )
save_plot("f_67e1.pdf", f_67e1, device = cairo_pdf, base_height = 8.75, base_width = 7.5)
