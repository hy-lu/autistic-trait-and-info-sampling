to_std_raven <- function(raw_score, age) {
  ifelse(
    age >= 17 && age < 18,
    std_score <- (raw_score - 63.5) / 4.7 * 15 + 100,
    ifelse(
      age >= 18 && age < 19,
      std_score <- (raw_score - 63.2) / 5.7 * 15 + 100,
      ifelse(
        age >= 19 && age < 25,
        std_score <- (raw_score - 63.4) / 5.6 * 15 + 100,
        ifelse(
          age >= 25 && age < 30,
          std_score <- (raw_score - 63.4) / 5.8 * 15 + 100,
          stop("Age provided is out of available options")
        )
      )
    )
  )
}
