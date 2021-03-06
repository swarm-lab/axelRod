#' @title Lose Shifter
#'
#' @description Strategy rules:
#'    1. Cooperates the first time it plays a new opponent.
#'    2. Changes behavior every time it loses to its opponent in the previous
#'      round.
#'
loseShifter <- function(opponent, memory) {
  idx <- tail(which(memory$opponent == opponent), 1)

  if (length(idx) == 0) {
    "C"
  } else {
    if (memory$opponent_score[idx] > memory$score[idx]) {
      ifelse(memory$play[idx] == "C", "D", "C")
    } else {
      memory$play[idx]
    }
  }
}
