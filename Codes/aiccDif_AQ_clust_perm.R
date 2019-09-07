# Compare with 0 ----
aqAICcDif_rank <-
  choice_rt_gof %>%
  select(id, model, AICc) %>%
  spread(model, AICc) %>%
  mutate(ts_vs_ts2 = rowMeans(select(., starts_with("ts_"))) - rowMeans(select(., starts_with("ts2_")))) %>%
  left_join(sub_info, by = "id") %>%
  mutate(ts_vs_ts2_rank = min_rank(ts_vs_ts2))
max_cluster_z <- function(df) {
  sig <- df$p < .05
  sign <- df$r >= 0
  type <- 2 * sig + sign
  runLength <- rle(type)
  cluster <- rep(seq_along(runLength$values), runLength$lengths)
  cluster_z <- data.frame(r = df$r, sig, cluster) %>%
    filter(sig) %>%
    group_by(cluster) %>%
    summarise_at("r", list(cluster_statistic = ~ sum(psych::fisherz(.)))) %>%
    pull(cluster_statistic)
  z <- cluster_z[which.max(abs(cluster_z))]
  ifelse(length(z) == 0, 0, z)
}
generate_r <-
  function(x) {
    tmp_df <- mutate(aqAICcDif_rank, ts_vs_ts2 = sample(ts_vs_ts2))
    map_df(15:104, ~ {
      filter(tmp_df, ts_vs_ts2_rank <= .x) %>%
        select(., ts_vs_ts2, aq_total_4) %>%
        psych::corr.test(method = "spearman", adjust = "none") %>%
        .$ci
    }) %>% max_cluster_z()
  }
set.seed(123)
perm_z_dist <-
  parallel::mclapply(1:10000, generate_r,
                     mc.cores = parallel::detectCores() - 1)

aqAICcDif_cor <- map_df(15:104, ~{aqAICcDif_rank %>%
    filter(ts_vs_ts2_rank <= .x) %>%
    select(., ts_vs_ts2, aq_total_4) %>%
    psych::corr.test(method = "spearman", adjust = "none") %>%
    .$ci}, .id = "nop")
obs_cluster <- aqAICcDif_cor %>%
  mutate(
  sig = p < .05,
  sign = r >= 0,
  type = 2 * sig + sign,
  cluster = rep(seq_along(rle(type)$values), rle(type)$lengths)
) %>%
  group_by(cluster) %>%
  summarise(
    cluster_statistic = sum(psych::fisherz(r)),
    cluster_sig = mean(sig),
    cluster_size = n()
  ) %>%
  ungroup() %>%
  mutate(perm_p = map_dbl(cluster_statistic, ~mean(abs(.x) < abs(unlist(perm_z_dist)))))
obs_cluster

# Compare with average: 1st method ----

set.seed(123)
aqAICcDif_cor_perm <- parallel::mclapply(1:10000, function(x) {
  tmp_df <- aqAICcDif_rank %>%
    mutate(ts_vs_ts2_rank = sample(ts_vs_ts2_rank))
  map_df(15:104, ~{tmp_df %>%
      filter(ts_vs_ts2_rank <= .x) %>%
      select(., ts_vs_ts2, aq_total_4) %>%
      psych::corr.test(method = "spearman", adjust = "none") %>%
      .$ci}, .id = "nop")}, mc.cores = parallel::detectCores() - 1)

perm_quantile_range <- aqAICcDif_cor_perm %>%
  bind_rows() %>%
  as_tibble() %>%
  group_by(nop) %>%
  summarize(lower = quantile(r, .025), upper = quantile(r, .975)) %>%
  ungroup() %>%
  arrange(as.numeric(nop))

generate_r2 <-
  function(x) {
    tmp_df <- mutate(aqAICcDif_rank, ts_vs_ts2_rank = sample(ts_vs_ts2_rank))
    map_df(15:104, ~ {
      filter(tmp_df, ts_vs_ts2_rank <= .x) %>%
        select(., ts_vs_ts2, aq_total_4) %>%
        psych::corr.test(method = "spearman", adjust = "none") %>%
        .$ci
    }, .id = "nop") %>%
      select(nop, r) %>%
      left_join(perm_quantile_range) %>%
      mutate(p = ifelse(r < lower | r > upper, 0, 1)) %>%
      arrange(as.integer(nop)) %>%
      max_cluster_z()
  }
perm_z_dist2 <-
  parallel::mclapply(1:10000, generate_r2,
                     mc.cores = parallel::detectCores() - 1)

obs_cluster_2 <-
  perm_quantile_range %>%
  left_join(select(aqAICcDif_cor,nop, r)) %>%
  mutate(nop = as.integer(nop)) %>%
  arrange(nop) %>%
  mutate(sig = r < lower | r > upper, sign = r >= 0,type = 2 * sig + sign,
         cluster = rep(seq_along(rle(type)$values), rle(type)$lengths)) %>%
  group_by(cluster) %>%
  summarise(
    cluster_statistic = sum(psych::fisherz(r)),
    cluster_sig = mean(sig),
    cluster_size = n()
  ) %>%
  ungroup() %>%
  mutate(perm_p = map_dbl(cluster_statistic, ~mean(abs(.x) < abs(unlist(perm_z_dist2)))))

obs_sig_interval <-
  obs_cluster %>%
  mutate(sample_end = cumsum(cluster_size) + 14,
         sample_start = sample_end - cluster_size + 1) %>%
  filter(cluster_sig == 1, perm_p < .1)
obs2_sig_interval <-
  obs_cluster_2 %>%
  mutate(sample_end = cumsum(cluster_size) + 14,
         sample_start = sample_end - cluster_size + 1) %>%
  filter(cluster_sig == 1, perm_p < .1)

f7_2 <- ggplot(aqAICcDif_cor, aes(x = as.numeric(nop) + 14, r)) +
  geom_point(size = 0.5) +
  geom_vline(xintercept = 91.5, size = 0.1, color = "grey50") +
  geom_hline(yintercept = last(aqAICcDif_cor$r), size = 0.1, color = "grey50", linetype = "dotted") +
  geom_segment(data = obs_sig_interval,
               mapping = aes(x = sample_start, xend = sample_end, y = -0.52, yend = -0.52, color = "1", size = "sig")) +
  geom_segment(data = obs2_sig_interval,
               mapping = aes(x = sample_start, xend = sample_end, y = -0.5, yend = -0.5, color = "2", size = "marg_sig")) +
  annotate("segment", x = 90, xend = 80, y = -0.13, yend = -0.13, arrow = arrow(length = unit(2, "mm"))) +
  annotate("segment", x = 93, xend = 103, y = -0.13, yend = -0.13, arrow = arrow(length = unit(2, "mm"))) +
  # annotate("segment", x = 102, xend = 104.5, y = last(aqAICcDif_cor$r) - 0.06, yend = last(aqAICcDif_cor$r) - 0.005,
  #          arrow = arrow(length = unit(1.5, "mm"))) +
  annotate("text", label = "More 'Cost-First'", x = 90, y = -0.1, hjust = 1,
           family = "Arial") +
  annotate("text", label = "More 'Evidence-First'", x = 93, y = -0.1, hjust = 0,
           family = "Arial") +
  annotate("text", label = "Overall\ncorrelation", x = 110, y = last(aqAICcDif_cor$r), hjust = 0.5, vjust = 0.5,
           family = "Arial", color = "grey20", size = 2.91, lineheight = 1) +
  scale_size_manual(values = c("sig" = 1.2, "marg_sig" = 0.6),
                      labels = c("marg_sig" = "p < .10", "sig" = "p < .05"),
                        guide = guide_legend(title = NULL)) +
  scale_color_discrete_qualitative(labels = c("1" = "Compared with 0", "2" = "Compared with overall correlation"),
                                   guide = guide_legend(title = NULL,
                                                        keywidth = unit(1, "pt"),
                                                        override.aes = list(size = 4)), palette = "Dynamic") +
  coord_cartesian(xlim = c(1, 90) + 14, clip = "off") +
  labs(x = "Number of participants included",
       y = expression(paste("Corr", group("(",list(plain(AICc)[Cost %->% Evidence] - plain(AICc)[Evidence %->% Cost], plain(AQ)), ")")))) +
  theme_neat(font_size, base_family = "Arial") +
  theme(plot.margin = margin(3, 14, 3, 4, "mm")) +
  NULL
# ggsave("f7_2.pdf", f7_2, device = cairo_pdf, width = 7.5, height = 5)

# # Compare with average: 2nd Method, fisher-z test ----
# max_cluster_z2 <- function(df) {
#   fisher_z <- (psych::fisherz(df$r) - psych::fisherz(overall_r))/sqrt(1/(seq_len(nrow(df)) + 14 - 3) + 1/(overall_n - 3))
#   sig <- abs(fisher_z) > 1
#   sign <- df$r >= 0
#   type <- 2 * sig + sign
#   runLength <- rle(type)
#   cluster <- rep(seq_along(runLength$values), runLength$lengths)
#   cluster_z <- data.frame(fisher_z, sig, cluster) %>%
#     filter(sig) %>%
#     group_by(cluster) %>%
#     summarise(cluster_statistic = sum(fisher_z)) %>%
#     pull(cluster_statistic)
#   z <- cluster_z[which.max(abs(cluster_z))]
#   ifelse(length(z) == 0, 0, z)
# }
#
# overall_r <- last(aqAICcDif_cor$r)
# overall_n <- as.numeric(last(aqAICcDif_cor$nop)) + 14
#
# generate_r2.2 <-
#   function(x) {
#     tmp_df <- mutate(aqAICcDif_rank, ts_vs_ts2_rank = sample(ts_vs_ts2_rank))
#     map_df(15:104, ~ {
#       filter(tmp_df, ts_vs_ts2_rank <= .x) %>%
#         select(., ts_vs_ts2, aq_total_4) %>%
#         psych::corr.test(method = "spearman", adjust = "none") %>%
#         .$ci
#     }) %>%
#       max_cluster_z2()
#   }
# perm_z_dist2.2 <-
#   parallel::mclapply(1:1000, generate_r2.2,
#                      mc.cores = parallel::detectCores() - 1)
#
# aqAICcDif_cor %>%
#   mutate(fisher_z = (psych::fisherz(r) - psych::fisherz(overall_r))/sqrt(1/(as.numeric(nop) + 14 - 3) + 1/(overall_n - 3))) %>%
#   mutate(sig = abs(fisher_z) > 1, sign = r >= 0, type = 2 * sig + sign,
#          cluster = rep(seq_along(rle(type)$values), rle(type)$lengths)) %>%
#   group_by(cluster) %>%
#   summarise(
#     cluster_statistic = sum(fisher_z),
#     cluster_sig = mean(sig),
#     cluster_size = n()
#   ) %>%
#   ungroup() %>%
#   mutate(perm_p = map_dbl(cluster_statistic, ~mean(abs(.x) < abs(unlist(perm_z_dist2.2)))))
