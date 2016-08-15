#' @title Punisher
#'
#' @description Strategy rules:
#'    1. Cooperate the first time you play a new opponent.
#'    2. Defect as soon as the opponent defects.
#'    3. Cooperate again after a time proportional to the number of times the
#'      opponent chose to defect.
#'
punisher <- function(opponent, memory) {
  idx <- which(memory$opponent == opponent)

  if (length(idx) == 0) {
    "C"
  } else {
    n_d <- sum(memory$opponent_play[idx] == "D")
    l_d <- rle(memory$opponent_play[idx])

    if (tail(l_d$values, 1) == "D") {
      "D"
    } else {
      if (tail(l_d$lengths, 1) >= n_d) {
        "C"
      } else {
        "D"
      }
    }
  }
}

