MaxN2D <- function(r, c, G = 10) {
  n <- ((r - 1 / 2) * G - r * c) / ((r - 1 / 2) * c)
  return(n)
}
EG <- function(n,
               r = 0.6,
               c = 0.5,
               G = 10,
               type = NULL) {
  if (n == 0) {
    EG <- G * 0.5
  }
  if (n > 0) {
    if ((n %% 2) == 1) {
      p <- pbinom((n - 1) / 2, n, r, lower.tail = F)
    } else{
      if (identical(type,"omniscient") && n < MaxN2D(r, c)) {
        p <-  pbinom(n / 2, n, r, lower.tail = F) / (1 - dbinom(n / 2, n, r))
      } else{
        p <- 0.5 * dbinom(n / 2, n, r) + pbinom(n / 2, n, r, lower.tail = F)
      }
    }
    EG <- (G - n * c) * p
  }
  return(EG)
}
