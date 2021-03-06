% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Player.R
\docType{class}
\name{Player}
\alias{Player}
\title{Player Class}
\format{\code{\link{R6Class}} object.}
\usage{
Player
}
\value{
\code{\link{R6Class}} object with methods for a player in an
 Axelrod-like tournament.
}
\section{Fields}{

\describe{
\item{\code{name}}{A character string giving the name of the player.}

\item{\code{strat}}{A function implementing the strategy of the player. This function
must take 2 arguments:
\describe{
  \item{opponent}{A character string giving the name of the opponent.}
  \item{memory}{A data frame with 5 columns named "round", "play", "opponent",
    "opponent_play", and "score" (see \code{memory} field below).}
}}

\item{\code{memory}}{A 5-column data frame, with the following columns:
\describe{
  \item{round}{Numerical value indicating the round of the tournament at
    which the data was collected.}
  \item{play}{A character string indicating the play ("C" for cooperate or
    "D" for defect) played by the player at each round.}
  \item{opponent}{A character string indicating the name of the opponent at
    each round.}
  \item{opponent_play}{A character string indicating the play ("C" for
    cooperate or "D" for defect) played by the opponent at each round.}
  \item{score}{A numerical value indicating the payoff received by the
    player at each round.}
}}
}}
\section{Methods}{

 \describe{
   \item{\code{new(type, players, nreps, nrounds, payoff)}}{Create an object
   of clase \code{Tournament}.
     \describe{
       \item{\code{name}}{A character string indicating the name of the
         player. If not provided, a random name is generated by the function.}
       \item{\code{strat}}{A function implementing the strategy of the player.
         This function must take 2 arguments:
         \describe{
           \item{opponent}{A character string giving the name of the opponent.}
           \item{memory}{A data frame with 5 columns named "round", "play",
           "opponent", "opponent_play", and "score" (see \code{memory} field
           above).}}}
       \item{\code{mem_size}}{A numerical value indicating the size of the
         player's memory (default: 100).}
     }
   }
   \item{\code{play(opponent)}}{Play a tournament round.
     \describe{
       \item{opponent}{A character string indicating the name of the opponent.}
     }
   }
   \item{\code{update(play, opponent, opponent_play, score)}}{Update the
     player's memory.
     \describe{
       \item{play}{The play of the player in the current round ("C" for
         cooperate; "D" for defect).}
       \item{opponent}{A character string indicating the name of the opponent.}
       \item{opponent_play}{The play of the opponent in the current round
         ("C" for cooperate; "D" for defect).}
       \item{score}{A numerical value indicating the payoff received at the
         current round.}
       \item{opponent_score}{A numerical value indicating the payoff received
         by the opponent at the current round.}
     }
   }
   \item{\code{reset()}}{Reset the player's memory.}
 }
}
\examples{
# TODO

}
\author{
Simon Garnier, \email{garnier@njit.edu}
}
\keyword{datasets}

