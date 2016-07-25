#' @export
Player <- R6::R6Class(
  classname = "Player",

  private = list(),

  public = list(
    name = NA,
    strat = NA,
    memory = NA,

    initialize = function(name, strat, mem_size = 100) {
      if (missing(name))
        self$name <- randomNames::randomNames(1, which.names = "last")
      else
        self$name <- name

      if (missing(strat))
        self$strat <- function(...) { sample(c("C", "D"), 1) }
      else
        self$strat <- strat

      if (!is.function(self$strat))
        stop("strat must be a function.")

      self$memory <- data.frame(round = 1:mem_size, opponent = NA, opponent_play = NA, score = NA)
    },

    play = function(opponent, ...) {
      self$strat(..., opponent = opponent, memory = self$memory)
    },

    update = function(opponent, opponent_play, score) {
      idx <- which(is.na(self$memory$score))[1]
      self$memory$opponent[idx] <- opponent
    }
  )
)









