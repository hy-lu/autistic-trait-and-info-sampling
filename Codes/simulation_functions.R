logistic <- function(x) {
  1 / (1 + exp(-x))
}

compute_decay <- function(info, alpha) {
  alpha <- ifelse(
    length(unique(alpha)) == 1,
    unique(alpha),
    simpleError("'alpha' has multiple unique values.")
  )
  decay <- vector(mode = "numeric", length = length(info))
  for (i in seq_along(info)) {
    if (dplyr::near(info[i], 0)) {
      decay[i] <- 0
    } else {
      decay[i] <- decay[i - 1] * alpha + info[i]
    }
  }
  return(decay)
}
