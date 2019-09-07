in_trial_sim <-
  read_rds(here("Data", "in_trial_sim.rds")) %>%
  mutate(
    info = dplyr::recode(bead, "b" = 1, "p" = -1),
    draw = as.integer(draw)
  ) %>%
  select(-bead)
in_trial_sim <-
  in_trial_sim %>%
  distinct(id, trial, .keep_all = T) %>%
  mutate(draw = 0, info = 0) %>%
  bind_rows(in_trial_sim) %>%
  distinct()
mod_ts <- mod_par_ls$ts_decay
mod_os <- mod_par_ls$os_costOnly
per_trial_sim <-
  per_trial %>%
  select(-order, -(resp:gain))
raw_sim_ts_df <-
  left_join(in_trial_sim, per_trial) %>%
  right_join(mod_ts) %>%
  mutate(trial = as.integer(trial)) %>%
  arrange(id, trial, draw) %>%
  group_by(id, trial) %>%
  mutate(
    cr_resp = dplyr::recode(cr_resp, "b" = 1, "p" = -1),
    cost0 = if_else(cost == "0", 1, 0),
    cost0_1 = if_else(cost == "0.1", 1, 0),
    cost0_4 = if_else(cost == "0.4", 1, 0),
    ratio = log(as.numeric(ratio)/(1 - as.numeric(ratio))),
    cost = as.numeric(cost),
    online_cost = cost*draw,
    decay_info = compute_decay(info, alpha),
    decay_info_abs = abs(decay_info),
    decay_info_val = decay_info_abs * ratio,
    last_cr = 0,
    last_dt = 0,
    kConst = 1,
    ptr = cost0 * ptr0 + cost0_1 * ptr1 + cost0_4 * ptr4
  ) %>%
  ungroup() %>%
  select(id, trial, kConst, cost0, cost0_1, cost0_4, draw, online_cost, ratio, decay_info_abs, decay_info_val, last_dt, last_cr, decay_info, cr_resp)
raw_sim_os_df <-
  left_join(in_trial_sim, per_trial) %>%
  right_join(mod_os) %>%
  mutate(trial = as.integer(trial)) %>%
  arrange(id, trial, draw) %>%
  group_by(id, trial) %>%
  mutate(
    cr_resp = dplyr::recode(cr_resp, "b" = 1, "p" = -1),
    cost0 = if_else(cost == "0", 1, 0),
    cost0_1 = if_else(cost == "0.1", 1, 0),
    cost0_4 = if_else(cost == "0.4", 1, 0),
    ratio = log(as.numeric(ratio)/(1 - as.numeric(ratio))),
    cost = as.numeric(cost),
    online_cost = cost*draw,
    cum_info = cumsum(info),
    cum_info_abs = abs(cum_info),
    cum_info_val = cum_info * ratio,
    last_cr = 0,
    last_dt = 0,
    kConst = 1
  ) %>%
  ungroup() %>%
  select(id, trial, kConst, cost0, cost0_1, cost0_4, draw, online_cost, ratio, cum_info_abs, cum_info_val, last_dt, last_cr, info, cum_info, cr_resp)

library(doParallel)
library(doRNG)
logist <- function(x) {
  p <- 1 / (1 + exp(-x))
  return(p)
}
p_fn_os <- function(params, dat, col) {
  p <- logist(as.matrix(dat[, col]) %*% params)
  return(p[1:20,])
}
p_fn_ts <- function(params, dat, os_col, ts_col, trp_col) {
  p <-
    logist(as.matrix(dat[, os_col]) %*% params[seq_along(os_col)]) +
    (1 - logist(as.matrix(dat[, os_col]) %*% params[seq_along(os_col)])) *
    (as.matrix(dat[, trp_col]) %*% params[seq_along(trp_col) + length(c(os_col, ts_col))]) *
    logist(as.matrix(dat[, ts_col]) %*% params[seq_along(ts_col) + length(os_col)])
  return(p[1:20,])
}

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
rep_times <- 250
rng <- RNGseq(rep_times * 104, 123)

sim_draw_exp_ts <- foreach(k = 1:rep_times) %:% foreach(
  i = mod_ts$id,
  r = rng[1:104 + (k - 1) * 104]
) %dopar% {
  rngtools::setRNG(r)
  pars <- unlist(mod_ts[mod_ts$id == i, 2:15])
  data <- raw_sim_ts_df[raw_sim_ts_df$id == i,]
  draw <- vector(mode = "integer", 288)
  correctness <- vector(mode = "integer", 288)
  for (j in 1:288) {
    p_mat <- p_fn_ts(
      pars,
      data[data$trial == j,],
      os_col = c(3, 5:8),
      ts_col = c(3, 9:13),
      trp_col = 4:6
    )
    draw_j <- rbinom(length(p_mat), 1, p_mat)
    draw[j] <- ifelse(any(draw_j), min(which(draw_j == 1)) - 1, 20)
    stop_draw <- data[data$trial == j & data$draw == draw[j], ]
    correctness[j] <- if_else(
      stop_draw$decay_info == 0,
      rbinom(1,1,0.5),
      as.integer(sign(stop_draw$decay_info) == stop_draw$cr_resp)
    )
    if (j < 288) {
      data[data$trial == j + 1, "last_dt"] <- draw[j]
      data[data$trial == j + 1, "last_cr"] <- correctness[j]
    }
  }
  draw.sim <- tibble(id = i,
                     trial = 1:288,
                     draw_sim_ts = draw,
                     correct_sim_ts = correctness
  )
  return(draw.sim)
}

sim_draw_exp_os <- foreach(k = 1:rep_times) %:% foreach(
  i = mod_os$id,
  r = rng[1:104 + (k - 1) * 104]
) %dopar% {
  rngtools::setRNG(r)
  pars <- unlist(mod_os[mod_os$id == i, c(2:6)])
  data <- raw_sim_os_df[raw_sim_os_df$id == i,]
  draw <- vector(mode = "integer", 288)
  correctness <- vector(mode = "integer", 288)
  for (j in 1:288) {
    p_mat <- p_fn_os(
      pars,
      data[data$trial == j,],
      col = c(3, 5:8)
    )
    draw_j <- rbinom(length(p_mat), 1, p_mat)
    draw[j] <- ifelse(any(draw_j), min(which(draw_j == 1)) - 1, 20)
    stop_draw <- data[data$trial == j & data$draw == draw[j], ]
    correctness[j] <- if_else(
      stop_draw$cum_info == 0,
      rbinom(1,1,0.5),
      as.integer(sign(stop_draw$cum_info) == stop_draw$cr_resp)
    )
    if (j < 288) {
      data[data$trial == j + 1, "last_dt"] <- draw[j]
      data[data$trial == j + 1, "last_cr"] <- correctness[j]
    }
  }
  draw.sim <- tibble(id = i,
                     trial = 1:288,
                     draw_sim_os = draw,
                     correct_sim_os = correctness
  )
  return(draw.sim)
}

stopCluster(cl)
stopImplicitCluster()

# save(sim_draw_exp_ts, file = "sim_draw_exp_ts_decay_20190827.RData", compress = T)
# save(sim_draw_exp_os, file = "sim_draw_exp_os_costOnly_20190827.RData", compress = T)
