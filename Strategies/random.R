#' @title Random
#'
#' @description Strategy rules:
#'    1. Cooperate or defect at random
#'
random <- function(opponent, memory) {
  sample(c("C", "D"), 1)
}
