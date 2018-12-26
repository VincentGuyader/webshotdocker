#' run the Shiny Application
#'
#'
#' @export
#' @importFrom shiny shinyApp
#'
#' @examples
#'
#' if (interactive()) {
#'
#'   run_app()
#'
#' }
#'
run_app <- function() {
  # appDir <- system.file("mini", package = "reprexsnapshot")
  # if (appDir == "") {
  #   stop("Could not find . Try re-installing `demoshiny`.", call. = FALSE)
  # }
  # shiny::runApp(appDir, display.mode = "normal")
  shinyApp(ui = reprexsnapshot:::app_ui(), server = reprexsnapshot:::app_server)
  
}
#' Title
#'
#' @return
#' @export
#'
#' @examples
run_app2 <- function() {
  appDir <- system.file("mini", package = "reprexsnapshot")
  if (appDir == "") {
    stop("Could not find . Try re-installing `demoshiny`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
  
}
