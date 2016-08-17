library(shiny)
library(ggvis)
library(dplyr)
library(axelRod)

# define custom functions
all_values <- function(x) {
  if (is.null(x))
    return(NULL)

  paste0("Score: ", x$stack_upr)
}


# load strategies in memory
strat_files <- list.files("../Strategies", full.names = TRUE)
strats <- do.call(rbind, lapply(strat_files, function(x) {
  source(x)
  tmp <- readLines(x, -1)
  data.frame(name = gsub("#' @title ", "", tmp[1]),
             fn = gsub(" .*$", "", tmp[!startsWith(tmp, "#'")][1]),
             stringsAsFactors = FALSE)
}))


# define global variables
counter <- 1
steps <- 1
dat <- data.frame(rep = 1, player = c("Player 1", "Player 2"), cum_score = 0)
