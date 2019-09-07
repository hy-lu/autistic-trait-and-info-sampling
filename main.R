## ----Basic data and packages preparation------------------
library(tidyverse)
library(here)
library(afex)
library(emmeans)
library(broom)
library(knitr)
library(kableExtra)
library(entropy)
source(here("Codes", "aq_scoring.R"))
source(here("Codes", "theme_neat.R"))
source(here("Codes", "to_std_raven.R"))
source(here("Codes", "corr.test2.R"))
source(here("Codes", "expected_gain_fun.R"))
source(here("Codes", "replace_model_name.R"))
source(here("Codes", "simulation_functions.R"))

set_sum_contrasts()
lmerCtrl <- lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
par_pattern <- "(^bk?[A-Z]+.*)|(ptr[0-9]+)|alpha|(^[bk][0-9]*$)"

# load rt, choice, subject data
sub_info_all <- read_rds(here("Data", "subject_data.rds"))
per_trial <- read_rds(here("Data", "choice_data.rds"))
in_trial <- read_rds(here("Data", "rt_data.rds"))

# Filter out subjects based on IQ, accuracy, and consistency
sub_info <- id_filtered <- sub_info_all %>%
  filter(raven >= quantile(raven, 1 / 4) - 1.5 * IQR(raven)) %>%
  # these participants were excluded because they
  # had poor performance according to accuracy and consistency
  # or showed strong bias
  # or misunderstood the task instruction
  filter(!(id %in% c(44, 42, 84, 59, 66, 6)))

# Filtered subjects tended to have low accuracy and/or low consistency
# Actual accuracy: percents of correct trials
per_trial %>%
    group_by(id) %>%
    summarise_at("correctness", "mean") %>%
    arrange(correctness)
# Consistency: choices were consistent with the predominant bead color
in_trial %>%
  left_join(per_trial, by = c("id", "trial")) %>%
  group_by(id, trial) %>%
  mutate(
    info = dplyr::recode(bead, "b" = 1, "p" = -1),
    cum_info = lag(cumsum(info), default = 0),
    resp_num = if_else(resp == "b", 1, 0)
  ) %>%
  ungroup() %>%
  filter(draw == 21) %>%
  dplyr::select(id, trial, resp_num, cum_info) %>%
  mutate(consistent = if_else(cum_info == 0,
                              NA,
                              near((sign(cum_info) + 1), resp_num*2))) %>%
  group_by(id) %>%
  summarise_at("consistent", "mean", na.rm = T) %>%
  arrange(consistent)

## ----finding out outlier sampling strategy,----------------
# Find out outlier data points of participants whose sampling strategy is
# non-compliant, based on Jones et al. (2019, JEP)

avg_num_bead_dat <- per_trial %>%
  group_by(id, ratio, cost) %>%
  summarise_at(vars(draw_times), list(mean)) %>%
  ungroup() %>%
  left_join(sub_info, ., by = "id")

noncompliance_id <-
  avg_num_bead_dat %>%
  group_by(cost, ratio) %>%
  mutate(
    up.mad = median(draw_times) + mad(draw_times, constant = 2.5),
    low.mad = median(draw_times) - mad(draw_times, constant = 2.5),
    low.box = quantile(draw_times, probs = 1 / 4) - 1.5 * IQR(draw_times),
    up.box = quantile(draw_times, probs = 3 / 4) + 1.5 * IQR(draw_times)
  ) %>%
  mutate(
    mad = draw_times < low.mad | draw_times > up.mad,
    box = draw_times < low.box | draw_times > up.box
  ) %>%
  filter(box) %>%
  ungroup() %>%
  select(id, cost, ratio)

## ----Efficiency data preparation-------------------------

efficiency <- tibble(
  ratio = rep(c("0.6", "0.8"), times = 3),
  cost = rep(c("0", "0.1", "0.4"), each = 2),
  opt_draw = c(19, 19, 13, 7, 1, 3)
) %>%
  mutate(opt_eg = pmap_dbl(list(
    n = opt_draw,
    r = as.numeric(ratio),
    c = as.numeric(cost)
  ), EG)) %>%
  left_join(per_trial, ., by = c("cost", "ratio")) %>%
  mutate(
    eg = pmap_dbl(list(
      n = draw_times,
      r = as.numeric(ratio),
      c = as.numeric(cost)
    ), EG),
    efficiency = eg / opt_eg
  )
avg_eff_dat <-
  efficiency %>%
  mutate(
    dev = draw_times - opt_draw,
    abs_dev = abs(dev)
  ) %>%
  group_by(id, cost, ratio) %>%
  summarise_at(c("efficiency", "dev", "abs_dev"), "mean") %>%
  ungroup() %>%
  left_join(sub_info, ., by = "id")
eff_data_scaled_filtered <-
  avg_eff_dat %>%
  anti_join(noncompliance_id) %>%
  mutate(aq_total = as.vector(scale(aq_total_4)))

## ----Formal test of efficiency: mixed model---------------
eff_aq_all_filtered <-
  mixed(efficiency ~ aq_total * cost * ratio + (cost + ratio | id),
    data = eff_data_scaled_filtered,
    control = lmerCtrl
  )

tidy(eff_aq_all_filtered$anova_table)


## ----AQ trend of efficiency in each condition----------------------------
emtrends(eff_aq_all_filtered, ~ cost + ratio, var = "aq_total") %>%
  summary(infer = c(T, T))


## ----Non-standardized coefficients of AQ for efficiency------------------
aq_sd <- sd(eff_data_scaled_filtered$aq_total_4)
my_percent <- scales::percent_format(accuracy = 0.01)
eff_ns_coef <-
  emmeans(eff_aq_all_filtered, pairwise ~ aq_total | cost + ratio, at = list(aq_total = c(1 / aq_sd, 0))) %>%
  summary(infer = c(T, T)) %>%
  .$contrast %>%
  as.data.frame() %>%
  mutate(
    sig = if_else(p.value < .05, 8, if_else(p.value < .10, 3, NA_real_)),
    sig_y = if_else(p.value < .10, estimate + SE + SE / 3, NA_real_)
  )

## ----Comparison to interpret interaction of efficiency-------------------
emtrends(eff_aq_all_filtered, ~ cost + ratio, var = "aq_total") %>%
  pairs(by = "ratio", adjust = "mvt") %>%
  tidy()

## ----Formal test of Abs. Dev.-----------------------------
abs_dev_aq_all_filtered <-
  mixed(abs_dev ~ aq_total * cost * ratio + (cost + ratio | id), data = eff_data_scaled_filtered, control = lmerCtrl)
tidy(abs_dev_aq_all_filtered$anova_table) %>%
  kable()


## ----AQ trend of Abs. Dev. in each condition-----------------------------
emtrends(abs_dev_aq_all_filtered, ~ cost + ratio, var = "aq_total") %>%
  summary(infer = c(T, T))

## ----Non-standardized coefficients of AQ for Abs. Dev.-------------------
abs_dev_ns_coef <-
  emmeans(abs_dev_aq_all_filtered, pairwise ~ aq_total | cost + ratio, at = list(aq_total = c(1 / aq_sd, 0))) %>%
  summary(infer = c(T, T)) %>%
  .$contrast %>%
  as.data.frame() %>%
  mutate(
    sig = if_else(p.value < .05, 8, if_else(p.value < .10, 3, NA_real_)),
    sig_y = if_else(p.value < .1, estimate + SE + SE / 3, NA_real_)
  )

## ----Comparison to interpret interaction of Abs. Dev.--------------------
emtrends(abs_dev_aq_all_filtered, ~cost, var = "aq_total") %>%
  pairs(adjust = "mvt") %>%
  tidy()

emtrends(abs_dev_aq_all_filtered, ~ratio, var = "aq_total") %>%
  pairs() %>%
  tidy()

## ----Formal test of Signed Dev.---------------------------
## It's equivalent to the test of the number of bead samples
dev_aq_all_filtered <-
  mixed(dev ~ aq_total * cost * ratio + (cost + ratio | id), data = eff_data_scaled_filtered, control = lmerCtrl)
tidy(dev_aq_all_filtered$anova_table)

## ----AQ trend of Signed Dev. in each condition---------------------------
emtrends(dev_aq_all_filtered, ~ cost + ratio, var = "aq_total") %>%
  summary(infer = c(T, T))

## ----Non-standardized coefficients of AQ for Signed Dev.-----------------
dev_ns_coef <-
  emmeans(dev_aq_all_filtered, pairwise ~ aq_total | cost + ratio, at = list(aq_total = c(1 / aq_sd, 0))) %>%
  summary(infer = c(T, T)) %>%
  .$contrast %>%
  as.data.frame() %>%
  mutate(
    sig = if_else(p.value < .05, 8, if_else(p.value < .10, 3, NA_real_)),
    sig_y = if_else(p.value < .1, estimate + SE + SE / 3, NA_real_)
  )
## ----Comparison to interpret interaction of Signed Dev.------------------
emtrends(dev_aq_all_filtered, ~cost, var = "aq_total") %>%
  pairs(adjust = "mvt") %>%
  tidy()


## ----Variation data preparation---------------------------
draw_var_dat <- per_trial %>%
  left_join(sub_info, ., by = c("id")) %>%
  group_by(id, cost, ratio) %>%
  summarise(sd = sd(draw_times)) %>%
  ungroup() %>%
  left_join(sub_info)
draw_var_dat_scaled_filtered <-
  draw_var_dat %>%
  anti_join(noncompliance_id) %>%
  mutate(aq_total = as.vector(scale(aq_total_4)))

## ----Formal test of SD------------------------------------
sd_aq_all_filtered <-
  mixed(sd ~ aq_total * cost * ratio + (cost + ratio | id), data = draw_var_dat_scaled_filtered, control = lmerCtrl)
tidy(sd_aq_all_filtered$anova_table)

## ----AQ trend of SD in each condition------------------------------------
emtrends(sd_aq_all_filtered, ~ cost + ratio, var = "aq_total") %>%
  summary(infer = c(T, T))

## ----Non-standardized coefficients of AQ for SD--------------------------
sd_ns_coef <-
  emmeans(sd_aq_all_filtered, pairwise ~ aq_total | cost + ratio, at = list(aq_total = c(1 / aq_sd, 0))) %>%
  summary(infer = c(T, T)) %>%
  .$contrast %>%
  as.data.frame() %>%
  mutate(
    sig = if_else(p.value < .05, 8, if_else(p.value < .10, 3, NA_real_)),
    sig_y = if_else(p.value < .1, estimate + SE + SE / 3, NA_real_)
  )

## ----Comparison to interpret interaction of SD---------------------------
emtrends(sd_aq_all_filtered, ~ cost + ratio, var = "aq_total") %>%
  pairs(by = "ratio", adjust = "mvt") %>%
  tidy()


## ----Draws data preparation-------------------------------
avg_num_bead_dat <-
  per_trial %>%
  group_by(id, ratio, cost) %>%
  summarise_at(vars(draw_times), list(mean)) %>%
  ungroup() %>%
  left_join(sub_info, ., by = "id")
num_bead_dat_scaled_filtered <-
  avg_num_bead_dat %>%
  anti_join(noncompliance_id) %>%
  mutate(aq_total = as.vector(scale(aq_total_4)))

## ----Formal test of Draws---------------------------------
draw_aq_all_filtered <-
  mixed(draw_times ~ aq_total * cost * ratio + (cost + ratio | id), data = num_bead_dat_scaled_filtered, control = lmerCtrl)
tidy(draw_aq_all_filtered$anova_table)


## ----AQ trend of Draws in each condition---------------------------------
emtrends(draw_aq_all_filtered, ~ cost + ratio, var = "aq_total") %>%
  summary(infer = c(T, T))


## ----Non-standardized coefficients of AQ for Draw number-----------------
draw_ns_coef <-
  emmeans(draw_aq_all_filtered, pairwise ~ aq_total | cost + ratio, at = list(aq_total = c(1 / aq_sd, 0))) %>%
  summary(infer = c(T, T)) %>%
  .$contrast %>%
  as.data.frame() %>%
  mutate(
    sig = if_else(p.value < .05, 8, if_else(p.value < .10, 3, NA_real_)),
    sig_y = if_else(p.value < .1, estimate + SE + SE / 3, NA_real_)
  )


## ----Comparison to interpret interaction of Draws------------------------
emtrends(draw_aq_all_filtered, ~cost, var = "aq_total") %>%
  pairs(adjust = "mvt") %>%
  tidy()

## ----Preparing RT data------------------------------------
boxplot.extreme <- function(x, ...) {
  iqr <- IQR(x, ...)
  upper <- quantile(x, probs = 3 / 4, ...) + 1.5 * iqr
  lower <- quantile(x, probs = 1 / 4, ...) - 1.5 * iqr
  names(lower) <- "Lower extreme"
  names(upper) <- "Upper extreme"
  return(c(lower, upper))
}

rt_w0_outlier <-
  in_trial %>%
  left_join(per_trial) %>%
  right_join(sub_info) %>%
  mutate_at(vars(trial, draw), as.numeric) %>%
  mutate(
    lg_rt = log(rt)
  ) %>%
  filter(draw %in% 1:20) %>%
  mutate(
    low_ext = boxplot.extreme(lg_rt)[1],
    up_ext = boxplot.extreme(lg_rt)[2]
  ) %>%
  filter(lg_rt <= up_ext & lg_rt >= low_ext) %>%
  dplyr::select(-low_ext, -up_ext)

## ----Formal test of RT: mixed model-----------------------
avg_rt_w0_outlier <-
  rt_w0_outlier %>%
  group_by(id, aq_total_4, cost, ratio) %>%
  summarise_at("lg_rt", mean) %>%
  ungroup() %>%
  mutate(aq_total = as.vector(scale(aq_total_4)))
avg_rt_aq_all <-
  mixed(lg_rt ~ aq_total * cost * ratio + (cost + ratio | id),
    data = avg_rt_w0_outlier,
    control = lmerCtrl
  )

tidy(avg_rt_aq_all$anova_table)

## ----AQ trend of RT in each condition------------------------------------
emtrends(avg_rt_aq_all, ~ cost + ratio, var = "aq_total") %>%
  summary(infer = c(T, T))


## ----Non-standardized coefficients of AQ for RT--------------------------
rt_aq_sd <- sd(avg_rt_w0_outlier$aq_total_4)
rt_ns_coef <-
  emmeans(avg_rt_aq_all, pairwise ~ aq_total | cost + ratio, at = list(aq_total = c(1 / aq_sd, 0))) %>%
  summary(infer = c(T, T)) %>%
  .$contrast %>%
  as.data.frame() %>%
  mutate(
    sig = if_else(p.value < .05, 8, if_else(p.value < .10, 3, NA_real_)),
    sig_y = if_else(p.value < .1, estimate + SE + SE / 3, NA_real_)
  )

## ----Comparison to interpret interaction of RT---------------------------
emtrends(avg_rt_aq_all, ~ cost, var = "aq_total") %>%
  pairs(adjust = "mvt") %>%
  tidy()

## Modeling analyses ----
## load modeling results ----
nobs <- read_csv(here("Data", "nobm_w_info.csv")) %>%
  filter(draw != 1) %>%
  count(id) %>%
  mutate(id = as.character(id))
# ncol(.) is the sum of the number of parameters and id, ll, model, n
# Choice model fitted in MATLAB, see .m codes in choice_model folder
mod_file_path <- here("Data", "choice_model")
mod_fit_ls <-
  mod_file_path %>%
  purrr::map(~ list.files(.x, pattern = "(os|ts).*\\.csv", full.names = T)) %>%
  unlist() %>%
  purrr::map(read_csv, col_types = cols(
    .default = "d",
    id = "c",
    model = "c"
  )) %>%
  purrr::map(~ {
    left_join(.x, nobs, by = "id") %>%
      mutate(
        k = ncol(.) - 4,
        AICc = -2 * ll + 2 * k + 2 * k * (k + 1) / (n - k - 1),
        BIC = -2 * ll + log(n) * k
      )
  }) %>%
  bind_rows() %>%
  dplyr::select(id, model, n, k, ll, AICc, BIC)

mod_fit <-
  mod_fit_ls %>%
  left_join(sub_info, .)

if (FALSE) {
  # export data to perform group-level BMS in MATLAB
  mod_fit %>%
    dplyr::select(id, model, AICc) %>%
    spread(model, AICc) %>%
    arrange(as.numeric(id)) %>%
    write_csv(here("Data", "model_aicc.csv"))
  mod_fit %>%
    dplyr::select(id, model, BIC) %>%
    spread(model, BIC) %>%
    arrange(as.numeric(id)) %>%
    write_csv(here("Data", "model_bic.csv"))
}

pexc_prob <-
  list("model_pxp_aicc.csv", "model_pxp_bic.csv") %>%
  map_dfr(~ read_csv(here("Data", .), col_types = cols(.default = "d")), .id = "info_crit") %>%
  mutate(info_crit = case_when(info_crit == "1" ~ "AICc", info_crit == "2" ~ "BIC")) %>%
  gather("model", "pxp", -info_crit)

mod_evi <-
  mod_fit %>%
  gather(info_crit, ic_value, AICc, BIC) %>%
  group_by(id, info_crit) %>%
  mutate(del_info_crit = ic_value - min(ic_value)) %>%
  ungroup() %>%
  right_join(pexc_prob) %>%
  mutate(model = map_chr(model, replace_model_name)) %>%
  group_by(info_crit, model) %>%
  summarise(del_info_crit = sum(del_info_crit), pxp = mean(pxp)) %>%
  ungroup() %>%
  arrange(info_crit, del_info_crit) %>%
  mutate(order = row_number())


## ----Model parameter data preparation---------------------
# Choice model fitted in MATLAB, see .m codes in rt_model folder
all_mod_par_ls <- mod_file_path %>%
  purrr::map(~ list.files(.x, pattern = "(os|ts).*\\.csv", full.names = T)) %>%
  unlist() %>%
  purrr::map(read_csv, col_types = cols(
    .default = "d",
    id = "c",
    model = "c"
  ))
names(all_mod_par_ls) <- map_chr(all_mod_par_ls, ~ unique(.$model))
mod_par_ls <-
  map(unique(names(all_mod_par_ls)), ~ bind_rows(all_mod_par_ls[names(all_mod_par_ls) %in% .x])) %>%
  purrr::map(semi_join, y = sub_info) %>%
  purrr::map(~ arrange(., id))
names(mod_par_ls) <- map_chr(mod_par_ls, ~ unique(.$model))

## ----Calculate and export intermediate probability for RT model estimation----
# Very time consuming !!!
if (FALSE) {
  mod_data <-
  read_csv(here("Data", "nobm_w_info.csv"), col_types = cols("id" = "c")) %>%
  filter(draw != 20) %>%
  mutate(
    ratio_46 = if_else(near(log(6 / 4), ratio), 1, 0),
    ratio_82 = if_else(near(log(8 / 2), ratio), 1, 0)
  ) %>%
  semi_join(sub_info)
  have_alpha <- function(x) {
  mod_data %>%
    right_join(select(x, id, alpha)) %>%
    arrange(id, trial, draw) %>%
    group_by(id, trial) %>%
    mutate(
      info = compute_decay(info, alpha),
      cum_info = abs(info),
      cum_info_val = cum_info * ratio
    ) %>%
    ungroup()
  }
  not_have_alpha <- function(x) {
    mod_data %>%
      arrange(id, trial, draw) %>%
      group_by(id, trial) %>%
      mutate(
        info = compute_decay(info, 1),
        cum_info = abs(info),
        cum_info_val = cum_info * ratio
      ) %>%
      ungroup()
  }
  build_args <- function(params_df) {
    model <- unique(params_df$model)
    if (str_detect(model, "^os$|(os_decay)")) {
      arg.list <- list(col = c(3, 4, 6, 12, 14, 15, 7:10))
      return(arg.list)
    } else if (str_detect(model, "costOnly")) {
      arg.list <- list(col = c(3, 14, 15, 7, 8))
      return(arg.list)
    } else if (str_detect(model, "evidenceOnly")) {
      arg.list <- list(col = c(3, 4, 6, 12, 9, 10))
      return(arg.list)
    } else if (str_detect(model, "ts_")) {
      arg.list <- list(os_col = c(3, 14, 15, 7, 8), ts_col = c(3, 4, 6, 12, 9, 10))
    } else if (str_detect(model, "ts2_")) {
      arg.list <- list(os_col = c(3, 4, 6, 12, 9, 10), ts_col = c(3, 14, 15, 7, 8))
    }
    if (str_detect(model, "ratio")) {
      arg.list <- c(arg.list, list(trp_col = 17:18))
    } else if (str_detect(model, "ratio|flex", negate = T)) {
      arg.list <- c(arg.list, list(trp_col = 13:15))
    }
    if (str_detect(model, "stop")) {
      arg.list <- c(arg.list, continue = F)
    } else {
      arg.list <- c(arg.list, continue = T)
    }
    return(arg.list)
  }
  compute_ts_ps <- function(params, dat, arg.list) {
    os_col <- arg.list$os_col
    ts_col <- arg.list$ts_col
    trp_col <- arg.list$trp_col
    continue <- arg.list$continue
    if (is.list(params)) {
      params <- unlist(params)
    }
    p_s1 <- logistic(as.matrix(dat[, os_col]) %*% params[seq_along(os_col)])
    if (is.null(trp_col)) {
      k <- params[length(c(os_col, ts_col)) + 1]
      b0 <- params[length(c(os_col, ts_col)) + 2]
      ps <- logistic(k * (as.matrix(dat[, os_col]) %*% params[seq_along(os_col)]) + b0)
    } else {
      ps <- as.matrix(dat[, trp_col]) %*% params[seq_along(trp_col) + length(c(os_col, ts_col))]
    }
    p_s2 <- logistic(as.matrix(dat[, ts_col]) %*% params[seq_along(ts_col) + length(os_col)])
    if (continue) {
      p <- p_s1 + (1 - p_s1) * ps * p_s2
      p_c1 <- (1 - p_s1) * (1 - ps)
      p_c2 <- (1 - p_s1) * ps * (1 - p_s2)
    } else {
      p <- p_s1 * (1 - ps) + p_s1 * ps * p_s2
      p_c1 <- 1 - p_s1
      p_c2 <- p_s1 * ps * (1 - p_s2)
    }
    p_c1_over_c <- p_c1 / (p_c2 + p_c1)
    p_c2_over_c <- p_c2 / (p_c2 + p_c1)
    p_tibble <- tibble::tibble(
      id = dat$id,
      trial = dat$trial,
      draw = dat$draw,
      p_s1 = as.vector(p_s1),
      p_s2 = as.vector(p_s2),
      ps = as.vector(ps),
      p_c1_over_c = as.vector(p_c1_over_c),
      p_c2_over_c = as.vector(p_c2_over_c),
      p_stop = as.vector(p)
    )
    return(p_tibble)
  }
  compute_os_ps <- function(params, dat, arg.list) {
    col <- arg.list$col
    if (is.list(params)) {
      params <- unlist(params)
    }
    p <- logistic(as.matrix(dat[, col]) %*% params[seq_along(col)])
    p_tibble <- tibble::tibble(
      id = dat$id,
      trial = dat$trial,
      draw = dat$draw,
      p_s1 = as.vector(p)
    )
    return(p_tibble)
  }
  prob_calc_wrapper <- function(params_df, data_df, ...) {
    if (all(str_detect(params_df$model, "os"))) {
      fn <- compute_os_ps
    } else if (all(str_detect(params_df$model, "ts"))) {
      fn <- compute_ts_ps
    }
    arg.list <- build_args(params_df)
    params_df <- select(params_df, -ll, -model)
    stopifnot(all(params_df$id == unique(data_df$id)))
    map2_df(
      .x = group_split(params_df, id, keep = F),
      .y = group_split(data_df, id),
      .f = fn,
      arg.list,
      ...
    )
  }
  data_mod_ls <- map_if(mod_par_ls, ~ is.element("alpha", names(.)), have_alpha, .else = not_have_alpha)
  probs_ls <- map2(mod_par_ls, data_mod_ls[names(mod_par_ls)], prob_calc_wrapper)

  if (FALSE) {
    # export data to fit RT model
    list(
      x = probs_ls %>% map(~ {
        mutate(., draw = draw + 1) %>%
          right_join(rt_w0_outlier) %>%
          select(id:bead, lg_rt)
      }),
      path = paste0(names(mod_par_ls), "_rt.csv")
    ) %>%
      pwalk(write_csv)
  }
}

## ----RT model data preparation--------
rt_mod_data <-
  list.files(
    here("Data", "rt_model"),
    pattern = ".*_rt.csv$",
    full.names = T
  ) %>%
  map(~ read_csv(.,
    col_types = cols(id = "c", trial = "c")
  ))
names(rt_mod_data) <-
  list.files(here("Data", "rt_model"), pattern = ".*_rt.csv$") %>%
  str_remove("_rt\\.csv")
# RT model fitted in MATLAB, see .m codes in rt_model folder
rt_params <-
  list.files(
    here(
      "rt_model",
      "twelve_ts_plus_four_os"
    ),
    pattern = "mr_.*\\.csv",
    full.names = T
  ) %>%
  map(~ {
    read_csv(.,
      col_types = cols(id = "c")
    )
  })
names(rt_params) <- map_chr(rt_params, ~ unique(.$model))
rt_mod_gof <- map2_df(rt_mod_data, rt_params[names(rt_mod_data)], left_join) %>%
  group_by(id, model) %>%
  summarise(ll_rt = unique(ll), n_rt = n()) %>%
  mutate(k_rt = if_else(str_detect(model, "os"), 4, 8)) %>%
  mutate(
    AICc = 2 * k_rt - 2 * ll_rt +
      2 * (k_rt + k_rt^2) / (n_rt - k_rt - 1),
    BIC = log(n_rt) * k_rt - 2 * ll_rt
  ) %>%
  ungroup()

## ----Export Choice + RT model evidence--------------------
choice_mod_fit <- mod_fit %>%
  left_join(nobs) %>%
  select(id, model, ll_choice = ll, n_choice = n, k_choice = k)
choice_rt_gof <- left_join(choice_mod_fit, rt_mod_gof) %>%
  mutate(ll = ll_choice + ll_rt, n = n_rt, k = k_choice + k_rt) %>%
  mutate(AICc = 2 * k - 2 * ll + 2 * (k + k^2) / (n - k - 1),
         BIC = log(n) * k - 2 * ll) %>%
  filter_all(all_vars(!is.na(.)))
if (FALSE) {
  # export choice + rt model evidence for group-level BMS
  choice_rt_gof %>%
    left_join(sub_info) %>%
    select(id, aq_total_4, model, AICc) %>%
    spread(model, AICc) %>%
    arrange(as.numeric(aq_total_4), as.numeric(id)) %>%
    write_csv(here("Data", "choice_rt_combined_model_aicc.csv"))
  choice_rt_gof %>%
    left_join(sub_info) %>%
    select(id, aq_total_4, model, BIC) %>%
    spread(model, BIC) %>%
    arrange(as.numeric(aq_total_4), as.numeric(id)) %>%
    write_csv(here("Data", "choice_rt_combined_model_bic.csv"))
}


## ----Number of best fitted model-----------------------------------------
chc_rt_delAICc <-
  choice_rt_gof %>%
    group_by(id) %>%
    mutate(AICc_rank = min_rank(AICc), del_AICc = AICc - min(AICc)) %>%
    ungroup() %>%
    group_by(model) %>%
    summarise_at("del_AICc", sum)
choice_rt_gof %>%
  group_by(id) %>%
  mutate(AICc_rank = min_rank(AICc), del_AICc = AICc - min(AICc)) %>%
  ungroup() %>%
  count(model, AICc_rank) %>%
  filter(AICc_rank == 1) %>%
  arrange(-n) %>%
  mutate(cum_n = cumsum(n), prop_n = cum_n/sum(n)) %>%
  left_join(chc_rt_delAICc)

## ----Import Choice + RT model comparison data--------------
choice_rt_groupbmc <-
  list(
    "choice_rt_combined_model_aicc_output.csv",
    "choice_rt_combined_model_bic_output.csv"
  ) %>%
  map_dfr( ~ read_csv(here("Data", .), col_types = cols(.default = "d", output = "c")), .id = "info_crit") %>%
  mutate(info_crit = case_when(info_crit == "1" ~ "AICc", info_crit == "2" ~ "BIC")) %>%
  gather("model", "value",-info_crit,-output) %>%
  spread(output, value) %>%
  group_by(info_crit) %>%
  mutate(alpha = alpha / sum(alpha)) %>%
  ungroup()

## ----Family-wise AQ corr. with model evidence difference----
parameter_repair <- c(
"os_v_-aq__4" = "One-stages - Two-stages",
"ts__2-aq__4"  = "Cost-first - Evid-first",
"stp__-aq__4" = "Stop - Continue",
"c_vs_e-aq__4" = "Sec-Thought: Cost - Evid",
"c_vs_f-aq__4" = "Sec-Thought: Cost - Flex",
"flx__-aq__4"  = "Sec-Thought: Flex - Evid"
)

famwiseAICcDif_aq_cor <- choice_rt_gof %>%
  select(id, model, AICc) %>%
  spread(model, AICc) %>%
  mutate(
    os_vs_ts = rowMeans(select(., matches("^os.*"))) - rowMeans(select(., matches("^ts.*"))),
    ts_vs_ts2 = rowMeans(select(., starts_with("ts_"))) - rowMeans(select(., starts_with("ts2_"))),
    stop_vs_cont = rowMeans(select(., matches("ts.*_stop$"))) - rowMeans(select(., matches("ts.*_decay$"))),
    c_vs_e = rowMeans(select(., matches("ts(2*)_decay.*"))) - rowMeans(select(., matches("ts(2*)_ratio_decay.*"))),
    c_vs_flex = rowMeans(select(., matches("ts(2*)_decay.*"))) - rowMeans(select(., matches("ts(2*)_flex_decay.*"))),
    flex_vs_e = rowMeans(select(., matches("ts(2*)_flex_decay.*"))) - rowMeans(select(., matches("ts(2*)_ratio_decay.*")))
  ) %>%
  left_join(sub_info) %>%
  {
    psych::corr.test(
      select(., contains("_vs_")),
      select(., aq_total_4),
      method = "spearman",
      adjust = "none"
    )
  } %>%
  .$ci %>%
  rownames_to_column("parameter") %>%
  filter(!is.na(r)) %>%
  mutate(
    sig = if_else(p < .05, "*p < .05", if_else(p < .1, "+p < .1", " p >= .1")),
    sig = fct_reorder(as.factor(sig), p),
    parameter = parameter_repair[parameter],
    parameter = fct_reorder(as.factor(parameter), r)
  )
famwiseAICcDif_aq_cor


## ----Best of Best participants (Participants who were best fitted by the best model-------------------------------------------
bob_id <-
  choice_rt_gof %>%
  group_by(id) %>%
  mutate(del_AICc = AICc - min(AICc),
         sub_rank_AICc = min_rank(del_AICc)) %>%
  group_by(model) %>%
  mutate(sum_del_AICc = sum(del_AICc)) %>%
  ungroup() %>%
  mutate(mod_rank_AICc = dense_rank(sum_del_AICc)) %>%
  filter(mod_rank_AICc == 1, sub_rank_AICc %in% 1) %>%
  pull(id)


## ----Parameter of best model --------------------------
mod_par_new <- mod_par_ls$ts_decay

## ----Parameter of best model in bob---------------------
mod_par_best <- filter(mod_par_new, id %in% bob_id)

## ----Model parameter and AQ correlations------------------
par_aq_new <- mod_par_new %>%
  left_join(sub_info) # %>% anti_join(noncompliance_id, by = "id")

## ----Parameter group correlation for all participants--------------------
par_aq_new %>%
  {
    corr.test2(
      select(., bkC:bPR),
      select(., aq_total_4),
      adjust = "fdr",
      method = "spearman"
    )
} %>%
  .$ci %>% rownames_to_column("parameter") %>% mutate(
  sig = if_else(p < .05, "*p < .05", if_else(p < .1, "+p < .1", " p >= .1")),
  sig = fct_reorder(as.factor(sig), p),
  parameter = str_extract(parameter, ".+(?=-)"),
  parameter = fct_reorder(as.factor(parameter), r)
) %>%
  kable(caption = "Cost- or Evidence-related: For all participants") %>%
  kable_styling(
    full_width = F,
    position = "left",
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  )
par_aq_new %>%
  {
    corr.test2(
      select(., ptr0:ptr4),
      select(., aq_total_4),
      adjust = "fdr",
      method = "spearman"
    )
} %>%
  .$ci %>% rownames_to_column("parameter") %>% mutate(
  sig = if_else(p < .05, "*p < .05", if_else(p < .1, "+p < .1", " p >= .1")),
  sig = fct_reorder(as.factor(sig), p),
  parameter = str_extract(parameter, ".+(?=-)"),
  parameter = fct_reorder(as.factor(parameter), r)
) %>%
  kable(caption = "Second-thought probabilities: For all participants") %>%
  kable_styling(
    full_width = F,
    position = "left",
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  )
par_aq_new %>%
  {
    corr.test2(
      select(., alpha),
      select(., aq_total_4),
      adjust = "fdr",
      method = "spearman"
    )
} %>%
  .$ci %>% rownames_to_column("parameter") %>% mutate(
  sig = if_else(p < .05, "*p < .05", if_else(p < .1, "+p < .1", " p >= .1")),
  sig = fct_reorder(as.factor(sig), p),
  parameter = str_extract(parameter, ".+(?=-)"),
  parameter = fct_reorder(as.factor(parameter), r)
) %>%
  kable(caption = "Alpha: For all participants") %>%
  kable_styling(
    full_width = F,
    position = "left",
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  )


## ----Model parameter and AQ correlations: bob-------------
par_aq_best <- mod_par_best %>%
  left_join(sub_info) # %>% anti_join(noncompliance_id, by = "id")

## ----Parameter group correlation for best participants-------------------
par_aq_best %>%
  {
    corr.test2(
      select(., bkC:bPR),
      select(., aq_total_4),
      adjust = "fdr",
      method = "spearman"
    )
} %>%
  .$ci %>% rownames_to_column("parameter") %>% mutate(
  sig = if_else(p < .05, "*p < .05", if_else(p < .1, "+p < .1", " p >= .1")),
  sig = fct_reorder(as.factor(sig), p),
  parameter = str_extract(parameter, ".+(?=-)"),
  parameter = fct_reorder(as.factor(parameter), r)
) %>%
  kable(caption = "Cost- or Evidence-related: For best-fitted participants") %>%
  kable_styling(
    full_width = F,
    position = "left",
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  )
par_aq_best %>%
  {
    corr.test2(
      select(., ptr0:ptr4),
      select(., aq_total_4),
      adjust = "fdr",
      method = "spearman"
    )
} %>%
  .$ci %>% rownames_to_column("parameter") %>% mutate(
  sig = if_else(p < .05, "*p < .05", if_else(p < .1, "+p < .1", " p >= .1")),
  sig = fct_reorder(as.factor(sig), p),
  parameter = str_extract(parameter, ".+(?=-)"),
  parameter = fct_reorder(as.factor(parameter), r)
) %>%
  kable(caption = "Second-thought probabilities: For best-fitted participants") %>%
  kable_styling(
    full_width = F,
    position = "left",
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  )
par_aq_best %>%
  {
    corr.test2(
      select(., alpha),
      select(., aq_total_4),
      adjust = "fdr",
      method = "spearman"
    )
} %>%
  .$ci %>% rownames_to_column("parameter") %>% mutate(
  sig = if_else(p < .05, "*p < .05", if_else(p < .1, "+p < .1", " p >= .1")),
  sig = fct_reorder(as.factor(sig), p),
  parameter = str_extract(parameter, ".+(?=-)"),
  parameter = fct_reorder(as.factor(parameter), r)
) %>%
  kable(caption = "Alpha: For best-fitted participants") %>%
  kable_styling(
    full_width = F,
    position = "left",
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  )

## ----Fam-wise AICc diff correlatd with performance---------------------------------------------
famwise_aicc_diff <-
  choice_rt_gof %>%
  select(id, model, AICc) %>%
  spread(model, AICc) %>%
  mutate(
    os_vs_ts = rowMeans(select(., matches("^os.*"))) - rowMeans(select(., matches("^ts.*"))),
    ts_vs_ts2 = rowMeans(select(., starts_with("ts_"))) - rowMeans(select(., starts_with("ts2_"))),
    stop_vs_cont = rowMeans(select(., matches("ts.*_stop$"))) - rowMeans(select(., matches("ts.*_decay$"))),
    c_vs_e = rowMeans(select(., matches("ts(2*)_decay.*"))) - rowMeans(select(., matches("ts(2*)_ratio_decay.*"))),
    c_vs_flex = rowMeans(select(., matches("ts(2*)_decay.*"))) - rowMeans(select(., matches("ts(2*)_flex_decay.*"))),
    flex_vs_e = rowMeans(select(., matches("ts(2*)_flex_decay.*"))) - rowMeans(select(., matches("ts(2*)_ratio_decay.*")))
  ) %>%
  select_if(function(x) all(!is.na(x)))
famwise_dim <- list("os_vs_ts", "ts_vs_ts2", "stop_vs_cont", c("c_vs_e", "c_vs_flex", "flex_vs_e"))
famwiseAICcDif_cor_fn <-
  function(fam_dim, var_df) {
    famwise_aicc_diff %>%
      select(id, ts_vs_ts2) %>%
      left_join(var_df) %>% {
        corr.test2(
          select(., `0_0.6`:`0.4_0.8`),
          select(., one_of(fam_dim)),
          method = "spearman",
          adjust = "fdr"
        )
      } %>% {
        left_join(rownames_to_column(.$ci, "cost_ratio"), .$ci.adj)
      } %>%
      mutate(
        cost_ratio = str_extract(cost_ratio, ".*(?=-)"),
        cost_ratio = str_c("C:", cost_ratio),
        cost_ratio = str_replace(cost_ratio, "_", ", E:")
      ) %>%
      mutate(sig = if_else(p < .05, "*p < .05", if_else(p < .1, "+p < .1", "p >= .1"))) %>%
      kable() %>%
      kable_styling(
        full_width = F,
        position = "left",
        bootstrap_options = c("striped", "hover", "condensed", "responsive")
      )
  }
eff_wide <-
  avg_eff_dat %>%
  anti_join(noncompliance_id) %>%
  unite("cost_ratio", cost, ratio) %>%
  select(id, cost_ratio, efficiency) %>%
  spread(cost_ratio, efficiency)
dev_wide <-
  avg_eff_dat %>%
  anti_join(noncompliance_id) %>%
  unite("cost_ratio", cost, ratio) %>%
  select(id, cost_ratio, dev) %>%
  spread(cost_ratio, dev)
sd_wide <-
  draw_var_dat_scaled_filtered %>%
  unite("cost_ratio", cost, ratio) %>%
  select(id, cost_ratio, sd) %>%
  spread(cost_ratio, sd)
lapply(famwise_dim, famwiseAICcDif_cor, var_df = eff_wide)
lapply(famwise_dim, famwiseAICcDif_cor, var_df = dev_wide)
lapply(famwise_dim, famwiseAICcDif_cor, var_df = sd_wide)

## ----Effect of cost on RT would be explained away by second-thought probabilities-------------
avg_rt_prob <- mod_par_new %>%
  left_join(sub_info) %>%
  select(id, aq_total_4, ptr0:ptr4) %>%
  gather("ptr", "prob", ptr0:ptr4) %>%
  mutate(cost = dplyr::recode(
    ptr,
    "ptr0" = "0",
    "ptr1" = "0.1",
    "ptr4" = "0.4"
  )) %>%
  left_join(rt_w0_outlier) %>%
  group_by(id, aq_total_4, prob, cost, ratio) %>%
  summarise_at("lg_rt", mean) %>%
  ungroup() %>%
  mutate(aq_total = as.vector(scale(aq_total_4)), prob = as.vector(scale(prob)))
mixed(lg_rt ~ prob * ratio * aq_total + aq_total * cost * ratio + (cost + ratio + prob | id), data = avg_rt_prob, control = lmerCtrl) %>%
  .$anova_table %>%
  tidy()


## ----Order effect regarding draw samples---------------------------------
cost_order <- per_trial %>%
    semi_join(sub_info) %>%
    distinct(id, cost) %>%
    group_by(id) %>%
    # mutate(cost_order = paste0(row_number(cost), collapse = ""))
    mutate(cost_order = as.factor(seq_along(cost)))
draw_order <-
  num_bead_dat_scaled_filtered %>%
  right_join(cost_order) %>%
  mixed(draw_times ~ cost * cost_order + (cost + cost_order | id), data = ., control = lmerCtrl)
tidy(draw_order$anova_table)


## ----Order effect regarding efficiency-----------------------------------
eff_order <-
  eff_data_scaled_filtered %>%
  right_join(cost_order) %>%
  mixed(efficiency ~ cost * cost_order + (cost + cost_order | id), data = ., control = lmerCtrl)
tidy(eff_order$anova_table)


## ----Order effect regarding variance-------------------------------------
sd_order <-
  draw_var_dat_scaled_filtered %>%
  right_join(cost_order) %>%
  mixed(sd ~ cost * cost_order + (cost + cost_order | id), data = ., control = lmerCtrl)
tidy(sd_order$anova_table)

## ----Order effect regarding DT-------------------------------------------
rt_order <-
  avg_rt_w0_outlier %>%
  right_join(cost_order) %>%
  mixed(lg_rt ~ cost * cost_order + (cost + cost_order | id), data = ., control = lmerCtrl)
tidy(rt_order$anova_table)
emmeans(rt_order, ~ cost_order) %>% contrast(method = "consec")


## ----Formal test of sequence effect of sample number---------------------
avg_rt_draw_data <- rt_w0_outlier %>%
  mutate(draw = factor(draw, levels = 1:20, ordered = T)) %>%
  group_by(id, draw) %>%
  summarise_at("lg_rt", list(
    m = ~ mean(.),
    sd = ~ sd(.),
    n = ~ n()
  )) %>%
  ungroup()
avg_rt_draw_aq_data <-
  left_join(avg_rt_draw_data, sub_info) %>%
  mutate(aq_total = as.vector(scale(aq_total_4))) %>%
  select(id, draw, m, n, aq_total)
avg_rt_draw_aq <- mixed(
  m ~ draw * aq_total + (1 | id),
  data = avg_rt_draw_aq_data,
  weights = n
)
tidy(avg_rt_draw_aq$anova_table)


## ----Sequence effect of sample number------------------------------------
emmeans(avg_rt_draw_aq, ~ draw) %>%
  contrast(method = "poly", adjust = "mvt") %>%
  tidy()


## ----Sequence effect of sample number:AQ---------------------------------
emmeans(avg_rt_draw_aq, ~ draw + aq_total, cov.reduce = range) %>%
  contrast(interaction = c("poly", "pairwise"), adjust = "mvt") %>%
  tidy()

## ----L.vs.Q: Efficiency--------------------------------------------------
eff_L <-
  lme4::lmer(efficiency ~ aq_total * cost * ratio + (cost + ratio | id),
    data = eff_data_scaled_filtered, REML = F, control = lmerCtrl
  )
eff_Q <-
  lme4::lmer(efficiency ~ poly(aq_total,2) * cost * ratio + (cost + ratio | id),
    data = eff_data_scaled_filtered, REML = F, control = lmerCtrl
  )
anova(eff_L, eff_Q) %>%
  tidy()


## ----L.vs.Q: Sampling Dev------------------------------------------------
dev_L <-
  lme4::lmer(dev ~ aq_total * cost * ratio + (cost + ratio | id),
             data = eff_data_scaled_filtered, REML = F, control = lmerCtrl)
dev_Q <-
  lme4::lmer(dev ~ poly(aq_total,2) * cost * ratio + (cost + ratio | id),
             data = eff_data_scaled_filtered, REML = F, control = lmerCtrl)
anova(dev_L, dev_Q) %>%
  tidy()


## ----L.vs.Q: Sampling Var------------------------------------------------
sd_L <-
  lme4::lmer(sd ~ aq_total * cost * ratio + (cost + ratio | id),
             data = draw_var_dat_scaled_filtered, REML = F, control = lmerCtrl)
sd_Q <-
  lme4::lmer(sd ~ poly(aq_total,2) * cost * ratio + (cost + ratio | id),
             data = draw_var_dat_scaled_filtered, REML = F, control = lmerCtrl)
anova(sd_L, sd_Q) %>%
  tidy()


## ----L.vs.Q: Avg DT------------------------------------------------------
avg_rt_L <-
  lme4::lmer(lg_rt ~ aq_total * cost * ratio + (cost + ratio | id),
    data = avg_rt_w0_outlier, REML = F, control = lmerCtrl
  )
avg_rt_Q <-
  lme4::lmer(lg_rt ~ poly(aq_total, 2) * cost * ratio + (cost + ratio | id),
    data = avg_rt_w0_outlier, REML = F, control = lmerCtrl
  )
anova(avg_rt_L, avg_rt_Q) %>%
  tidy()

## ----L.vs.Q: Avg DT ~ Prob-----------------------------------------------
avg_rt_prob_L <-
  lme4::lmer(lg_rt ~ prob * ratio * aq_total + aq_total * cost * ratio + (cost + ratio + prob | id),
             data = avg_rt_prob, REML = F, control = lmerCtrl)
avg_rt_prob_Q <-
  lme4::lmer(lg_rt ~ prob * ratio * poly(aq_total,2) + poly(aq_total,2) * cost * ratio + (cost + ratio + prob | id),
             data = avg_rt_prob, REML = F, control = lmerCtrl)
anova(avg_rt_prob_L, avg_rt_prob_Q) %>%
  tidy()

## ----New sub info by group-----------------------------------------------
sub_info_grp <-
  sub_info %>%
  mutate(aq_group_4 = cut(
      aq_total_4,
      breaks = c(0, quantile(aq_total_4, c(1 / 3, 2 / 3)), 150),
      labels = c("Low", "Mid", "High")
    ))


## ----Efficiency by AQ group----------------------------------------------
eff_aq_grp <-
  mixed(efficiency ~ aq_group_4 * cost * ratio + (cost + ratio | id),
    data = left_join(eff_data_scaled_filtered, sub_info_grp),
    control = lmerCtrl
  )
eff_aq_grp$anova_table %>%
  tidy()
emmeans(eff_aq_grp, ~ aq_group_4 | cost + ratio) %>%
  joint_tests(by = c("cost", "ratio"))
emmeans(eff_aq_grp, ~ aq_group_4 | cost + ratio) %>%
  contrast("pairwise", adjust = "mvt") %>%
  tidy()

## ----AICc difference by AQ group-----------------------------------------
(aiccDif_aqGrp <- famwise_aicc_diff %>%
  left_join(sub_info_grp) %>%
  aov_4(ts_vs_ts2 ~ aq_group_4 + (1|id), data = .))

emmeans(aiccDif_aqGrp, pairwise ~ aq_group_4, adjust = "mvt")


## ----AICc difference with other variables--------------------------------
famwise_aicc_diff %>%
  left_join(sub_info) %>%
  mutate(gender = as.integer(gender) - 1) %>%
  {corr.test2(select(., ts_vs_ts2), select(., raven, age, gender), adjust = "none")}


## ----Previous Paper correlation preparation---------------------------------------------------
es_df <- tribble(
  ~author, ~year, ~r, ~n,
  "Lawson et al.", 2018, -.44, 28,
  "Lawson et al.", 2018, -.33, 28,
  "Lawson et al.", 2018, -.30, 28,
  "Karvelis et al.", 2018, -.327, 83,
  "Karvelis et al.", 2018, -.175, 83,
  "Karvelis et al.", 2018, -.238, 83,
  "Chouinard et al.", 2016, -.26, 131,
  "Chouinard et al.", 2016, -.20, 131,
  "Chouinard et al.", 2016, -.18, 131,
  "van Boxtel & Lu (1)", 2013, .43, 25,
  "van Boxtel & Lu (1)", 2013, .31, 25,
  "van Boxtel & Lu (2)", 2013, .45, 30,
  "van Boxtel & Lu (2)", 2013, -.38, 30,
  "Shah et al.", 2016, -.43, 40,
  "Haffey et al.", 2013, -.367, 36,
  "Haffey et al.", 2013, .266, 36,
  "Haffey et al.", 2013, .158, 36,
  "Haffey et al.", 2013, .273, 36,
  "Robic et al.", 2015, -.53, 29,
  "Robic et al.", 2015, -.53, 29,
  "Robic et al.", 2015, -.47, 29,
  "Sevgi et al.", 2016, -.39, 36,
  "Sevgi et al.", 2016, -.42, 36,
  "Turi et al.", 2018, .70, 50
)
mini_meta_r <-
  es_df %>% mutate(
  fisher_z = psych::fisherz(r),
  abs_fisher_z = abs(fisher_z)
) %>%
  group_by(author) %>%
  summarize(avg_abs_rz = mean(abs_fisher_z),
            n = mean(n)) %>%
  ungroup() %>%
  summarize(weighted_rz = sum(avg_abs_rz * (n - 3))/sum(n - 3)) %>%
  mutate(weighted_r = psych::fisherz2r(weighted_rz))
map_dbl(seq(0.8, 0.95, by = 0.05), ~pwr::pwr.r.test(r = mini_meta_r$weighted_r, power = .x)$n)

