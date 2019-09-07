replace_model_name <- function(model_name) {
  name_elem <-
    stringr::str_split(model_name, "[:punct:]", simplify = TRUE)

  if (any(name_elem %in% c("costOnly", "evidenceOnly"))) {
    first_line <-
      ifelse(is.element("costOnly", name_elem),
             "Cost only",
             "Evidence only")
  } else if (is.element("os", name_elem)) {
    first_line <- "Cost + Evidence"
  } else if (any(is.element(c("ts", "ts2"), name_elem))) {
    first_line <-
      ifelse(is.element("ts", name_elem),
             "Cost \u2192 Evidence",
             "Evidence \u2192 Cost")
  }
  second_line <- NULL
  if (is.element("ratio", name_elem)) {
    second_line <- c(second_line, "Cond.E")
  }
  if (is.element("flex", name_elem)) {
    second_line <- c(second_line, "Flex Ps")
  }
  if (!is.element("decay", name_elem) & stringr::str_detect(first_line, "Evidence")) {
    if (is.element("os", name_elem)) {
      return(stringr::str_c(first_line, " w/o decay"))
    }
    second_line <- c(second_line, "w/o Decay")
  }
  if (is.element("stop", name_elem)) {
    second_line <- c(second_line, "Stop2Second")
  }
  stringr::str_c(first_line,
                 if (!is.null(second_line))
                   "\n",
                 stringr::str_c(second_line, collapse = ", "))
}

