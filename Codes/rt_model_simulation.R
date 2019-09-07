# library(tidyverse)
# library(here)
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
# identical(names(rt_mod_data), names(rt_params[names(rt_mod_data)]))
# > TRUE


rt_data_pars <- map2_df(rt_mod_data, rt_params[names(rt_mod_data)], left_join) %>%
  filter(model %in% c("ts_decay", "os_costOnly")) %>%
  mutate(
    log_dt1 = a1 * p_s1 + b1 * p_s1 * (1 - p_s1) + gamma1,
    log_dt2 = log(exp(a1 * (1 - p_s1) + b1 * p_s1 * (1 - p_s1) + gamma1) +
                    exp(a2 * p_s2 + b2 * p_s2 * (1 - p_s2) + gamma2))
  ) %>%
  select(
    p_c1_over_c,
    p_c2_over_c,
    log_dt1,
    log_dt2,
    sigma1,
    sigma2,
    lg_rt,
    id,
    trial,
    draw,
    ll,
    model
  )

if (TRUE) {
  rmixnorm <- function(mu1, mu2, sd1, sd2, lam1, lam2, n = 100) {
    if (any(is.na(c(lam1, lam2)))) {
      warning("Revert back to Gaussian distribution because of missing lambda(s)!")
      stopifnot(any(is.na(c(mu1, mu2))))
      idx <- which(!is.na(c(mu1, mu2)))
      return(rnorm(n, mean = c(mu1, mu2)[idx], sd = c(sd1, sd2)[idx]))
    }
    mixtools::rnormmix(
      n,
      c(lam1, lam2),
      c(mu1, mu2),
      c(sd1, sd2)
    )
  }
  set.seed(123)
  rt_sim <-
    rt_data_pars %>%
    mutate(
      trial = as.character(trial),
      lg_rt_sim = pmap(
        list(log_dt1, log_dt2, sigma1, sigma2, p_c1_over_c, p_c2_over_c),
        rmixnorm,
        n = 10
      )
    ) %>%
    select(id, trial, draw, lg_rt, lg_rt_sim, model) %>%
    left_join(select(per_trial, id, trial, cost, ratio))
}