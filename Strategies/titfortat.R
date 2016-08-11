#' @title Tit-For-Tat
#'
#' @description Strategy rules:
#'    1. Cooperate the first time you play a new opponent.
#'    2. Mirror the last move of each opponent in subsequent encounters.
#'
titfortat <- function(opponent, memory) {
  idx <- tail(which(memory$opponent == opponent), 1)

  if (length(idx) == 0) {
    "C"
  } else {
    memory$opponent_play[idx]
  }
}
