.onetime <- function(players, payoff, nreps) {
  plays <- t(combn(1:length(players), 2))

  res <- data.frame(rep = rep(1:nreps, each = 2 * nrow(plays)),
                    round = 1, player = NA, opponent = NA, play = NA,
                    outcome = NA, score = NA)

  for (i in 1:nreps) {
    for (j in 1:nrow(plays)) {
      players[[plays[j, 1]]]$reset()
      players[[plays[j, 2]]]$reset()

      p1 <- players[[plays[j, 1]]]$play(opponent = players[[plays[j, 2]]]$name)
      p2 <- players[[plays[j, 2]]]$play(opponent = players[[plays[j, 1]]]$name)

      if (p1 == p2) {
        if (p1 == "C") {
          sc1 <- sc2 <- payoff[1, 1]
        } else {
          sc1 <- sc2 <- payoff[2, 2]
        }
      } else {
        if (p1 == "C") {
          sc1 <- payoff[1, 2]
          sc2 <- payoff[2, 1]
        } else {
          sc1 <- payoff[2, 1]
          sc2 <- payoff[1, 2]
        }
      }

      idx <- which(is.na(res$player))[1]
      res[idx, ]$player <- players[[plays[j, 1]]]$name
      res[idx, ]$opponent <- players[[plays[j, 2]]]$name
      res[idx, ]$play <- p1
      res[idx, ]$outcome <- ifelse(p1 == p2, "draw", ifelse(sc1 > sc2, "win", "loss"))
      res[idx, ]$score <- sc1

      idx <- which(is.na(res$player))[1]
      res[idx, ]$player <- players[[plays[j, 2]]]$name
      res[idx, ]$opponent <- players[[plays[j, 1]]]$name
      res[idx, ]$play <- p2
      res[idx, ]$outcome <- ifelse(p1 == p2, "draw", ifelse(sc2 > sc1, "win", "loss"))
      res[idx, ]$score <- sc2
    }
  }

  res
}

.repeated <- function(players, payoff, nreps, nrounds) {
  plays <- t(combn(1:length(players), 2))

  res <- data.frame(rep = rep(1:nreps, each = 2 * nrow(plays)),
                    round = 1, player = NA, opponent = NA, play = NA,
                    outcome = NA, score = NA)

  pb <- progress::progress_bar$new(total = nrow(res) * nrounds / 2)

  for (i in 1:nreps) {
    for (j in 1:nrow(plays)) {
      players[[plays[j, 1]]]$reset()
      players[[plays[j, 2]]]$reset()

      for (k in 1:nrounds) {
        p1 <- players[[plays[j, 1]]]$play(opponent = players[[plays[j, 2]]]$name)
        p2 <- players[[plays[j, 2]]]$play(opponent = players[[plays[j, 1]]]$name)

        if (p1 == p2) {
          if (p1 == "C") {
            sc1 <- sc2 <- payoff[1, 1]
          } else {
            sc1 <- sc2 <- payoff[2, 2]
          }
        } else {
          if (p1 == "C") {
            sc1 <- payoff[1, 2]
            sc2 <- payoff[2, 1]
          } else {
            sc1 <- payoff[2, 1]
            sc2 <- payoff[1, 2]
          }
        }

        players[[plays[j, 1]]]$update(p1, players[[plays[j, 2]]]$name, p2, sc1)
        players[[plays[j, 2]]]$update(p2, players[[plays[j, 1]]]$name, p1, sc2)
        pb$tick()
      }

      idx <- which(is.na(res$player))[1]
      res[idx, ]$player <- players[[plays[j, 1]]]$name
      res[idx, ]$opponent <- players[[plays[j, 2]]]$name
      res[idx, ]$score <- sum(players[[plays[j, 1]]]$memory$score)

      idx <- which(is.na(res$player))[1]
      res[idx, ]$player <- players[[plays[j, 2]]]$name
      res[idx, ]$opponent <- players[[plays[j, 1]]]$name
      res[idx, ]$score <- sum(players[[plays[j, 2]]]$memory$score)
    }
  }

  res
}

.random <- function(players, payoff, nreps, nrounds) {

}

#' @title Tournament class
#'
#' @export
Tournament <- R6::R6Class(
  classname = "Tournament",

  private = list(),

  public = list(
    type = NA,
    players = NA,
    payoff = NA,
    nreps = NA,
    nrounds = NA,
    res = NA,

    initialize = function(type, players, nreps = 100, nrounds = 100,
                          payoff = matrix(c(3, 5, 0, 1), nrow = 2)) {
      if (missing(type))
        self$type <- "onetime"
      else
        self$type <- type

      if (!(self$type %in% c("onetime", "repeated", "random")))
        stop(paste0("type must be one of the following: ", paste(c("onetime", "repeated", "random"), collapse = ", "), "."))

      if (missing(players))
        stop("Players must be provided.")

      if (is.null(names(players)) | !all(names(players) != ""))
        stop("All players must be named.")

      self$nreps <- nreps
      self$nrounds <- nrounds
      self$payoff <- payoff

      self$players <- c()
      for (i in 1:length(players)) {
        self$players <- c(self$players, Player$new(names(players)[i], players[[i]], nrounds))
      }
    },

    play = function() {
      if (self$type == "onetime")
        self$res <- .onetime(players = self$players, payoff = self$payoff, nreps = self$nreps)
      else if (self$type == "repeated")
        self$res <- .repeated(players = self$players, payoff = self$payoff, nreps = self$nreps, nrounds = self$nrounds)
      else if (self$type == "random")
        self$res <- .random(players = self$players, payoff = self$payoff, nreps = self$nreps, nrounds = self$nrounds)
    }
  )
)


















