.getStrategy <- function(path) {
  tmp <- roxygen2:::parse_file(path, .GlobalEnv)
  list(name = tmp[[1]]$title,
       description = tmp[[1]]$description,
       fn = eval(parse(path)))
}

#' @export
#'
defaultStrategies <- function() {
  strat_path <- paste0(find.package("axelRod"), "/Strategies")
  strat_files <- list.files(strat_path, full.names = TRUE)
  lapply(strat_files, .getStrategy)
}
