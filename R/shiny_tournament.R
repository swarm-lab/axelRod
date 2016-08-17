#' @export
#'
shiny_tournament <- function() {
  app_path <- paste0(find.package("axelRod"), "/App/")
  shiny::runApp(app_path)
}