aq_scoring <- function(ans_vec, sub_areas = FALSE, four_points = F) {
  if (!is.vector(ans_vec)) {
    try(ans_vec <- unname(unlist(ans_vec)))
  }
  if (!is.double(ans_vec)) {
    try(ans_vec <- as.double(ans_vec))
  }
  item_score <- double(length = 50)
  agree <- c(2,4,5,6,7,9,12,13,16,18,19,20,21,22,23,26,33,35,39,41,42,43,45,46)
  disagree <- setdiff(1:50, agree)
  if (four_points) {
    item_score[agree] <- ans_vec[agree] - 1
    item_score[disagree] <- 4 - ans_vec[disagree]
  } else {
    item_score[agree] <- ifelse(ans_vec[agree] >= 3, 1, 0)
    item_score[disagree] <- ifelse(ans_vec[disagree] >= 3, 0, 1)
  }
  total <- sum(item_score)
  res.df <- data.frame(aq_total = total)
  if (sub_areas) {
    soc_ski <- sum(item_score[c(1,11,13,15,22,36,44,45,47,48)])
    att_swi <- sum(item_score[c(2,4,10,16,25,32,34,37,43,46)])
    att_to_det <- sum(item_score[c(5,6,9,12,19,23,28,29,30,49)])
    com <- sum(item_score[c(7,17,18,26,27,31,33,35,38,39)])
    ima <- sum(item_score[c(3,8,14,20,21,24,40,41,42,50)])
    num_pat <- sum(item_score[c(6,9,19,23,41)])
    res.df <- data.frame(res.df, soc_ski, att_swi, att_to_det, com, ima, num_pat)
  }
  return(res.df)
}
aq_reverse <- function(ans_vec, four_points = F) {
  if (!is.vector(ans_vec)) {
    try(ans_vec <- unname(unlist(ans_vec)))
  }
  if (!is.double(ans_vec)) {
    try(ans_vec <- as.double(ans_vec))
  }
  item_score <- double(length = 50)
  agree <- c(2,4,5,6,7,9,12,13,16,18,19,20,21,22,23,26,33,35,39,41,42,43,45,46)
  disagree <- setdiff(1:50, agree)
  if (four_points) {
    item_score[agree] <- ans_vec[agree] - 1
    item_score[disagree] <- 4 - ans_vec[disagree]
  } else {
    item_score[agree] <- ifelse(ans_vec[agree] >= 3, 1, 0)
    item_score[disagree] <- ifelse(ans_vec[disagree] >= 3, 0, 1)
  }
  return(item_score)
}
