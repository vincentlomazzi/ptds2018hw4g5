#' @export
runDemo <- function() {
  # REPLACE N BY YOUR GROUP NUMBER AND DELETE THIS COMMENT
  appDir <- system.file("shiny-examples", "pi", package = "ptds2018hw4g5")
  if (appDir == "") {
    stop(
      # REPLACE N BY YOUR GROUP NUMBER AND DELETE THIS COMMENT
      "Could not find example directory. Try re-installing ptds2018hw4g5.",
      call. = FALSE
    )
  }

  shiny::runApp(appDir, display.mode = "normal")

}
