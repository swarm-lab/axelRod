#' @title Retaliater
#'
#' @description Strategy rules:
#'    1. Cooperates the first time you play a new opponent.
#'    2. If the opponent has played "D" to my "C" more often than 10% of the
#'        time that I've done the same to him, play "D". Otherwise, play "C".
#'
retaliater <- function(opponent, memory) {
  idx <- which(memory$opponent == opponent)
  idx1 <- (memory$play[idx] == "D") & (memory$opponent_play[idx] == "C")
  idx2 <- (memory$play[idx] == "C") & (memory$opponent_play[idx] == "D")

  if (sum(idx1, na.rm = TRUE) < 1.1 * sum(idx2, na.rm = TRUE)) {
    "D"
  } else {
    "C"
  }
}

