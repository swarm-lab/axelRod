#' @title Alternator
#'
#' @description Strategy rules:
#'    1. Alternate between cooperation and defection at each round.
#'
alternator <- function(opponent, memory) {
  if ((nrow(memory) %% 2) == 0) {
    "C"
  } else {
    "D"
  }
}