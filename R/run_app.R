#' Launch the statease Shiny App
#'
#' @description Launches an interactive Shiny application for
#'   running statistical analyses without writing any code.
#'
#' @param ... Additional arguments passed to shiny::runApp()
#'
#' @return Launches the Shiny app in your browser
#' @export
#'
#' @examples
#' if(interactive()){
#'   run_app()
#' }
run_app <- function(...) {
  app_dir <- system.file("shiny", package = "statease")
  if (app_dir == "") {
    stop("Shiny app not found. Try reinstalling statease.")
  }
  shiny::runApp(app_dir, ...)
}
