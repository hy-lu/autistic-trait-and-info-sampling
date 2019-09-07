numformat <- function(x, digits = 2) {
  txt <- sub("^(-?)0.", "\\1.", formatC(x, digits = digits, format = "fg", flag = "#"))
  if (str_detect(txt, "-")) {
    txt <- str_trunc(txt, width = 5, side = "right", ellipsis = "")
  }else{
    txt <- str_trunc(txt, width = 4, side = "right", ellipsis = "")
  }
  return(txt)
}
cor_txt <-
  function(data,
           x,
           y,
           method,
           ...,
           sep = ", ",
           output = "character") {
    x <- pull(data, !!enquo(x))
    y <- pull(data, !!enquo(y))
    c.t.list <- stats::cor.test(x = x,
                                y = y,
                                method = method,
                                ...)
    c.t.df <- broom::tidy(c.t.list)
    cortxt <-
      switch(
        stringr::str_extract(c.t.df$method, "Spearman|Kendall|Pearson"),
        "Pearson" =
          latex2exp::TeX(paste(
            paste0(
              "$\\textit{r}($",
              c.t.df$parameter,
              "$) = ",
              numformat(c.t.df$estimate), "$"
            ),
            ifelse(
              c.t.df$p.value >= .001,
              paste0("$\\textit{p} = ", numformat(c.t.df$p.value), "$"),
              paste0("$\\textit{p} < .001 $")
            ),
            sep = sep
          ),
          output = output),
        "Spearman" =
          latex2exp::TeX(paste(
            paste0("$\\textit{r_S} = ", numformat(c.t.df$estimate), "$"),
            ifelse(
              c.t.df$p.value >= .001,
              paste0("$\\textit{p} = ", numformat(c.t.df$p.value), "$"),
              paste0("$\\textit{p} < .001 $")
            ),
            sep = sep
          ),
          output = output),
        "Kendall" =
          latex2exp::TeX(paste(
            paste0("$\\textit{\\tau} = ",
                   numformat(c.t.df$estimate), "$"),
            ifelse(
              c.t.df$p.value >= .001,
              paste0("$\\textit{p} = ", numformat(c.t.df$p.value), "$"),
              paste0("$\\textit{p} < .001 $")
            ),
            sep = sep
          ),
          output = output)
      )
    return(tibble::tibble(text = list(cortxt)))
  }
