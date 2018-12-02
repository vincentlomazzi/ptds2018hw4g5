#' @title Shiny App for estimating Pi
#' @description Shiny App used to estimate pi and plot the corresponding graph
#' with the different functions estimate_pi and estimate_pi2 and varying the
#' number of generated points and the seed.
#' @author Bron Luca
#' @author Germano David
#' @author Grandadam Patrik
#' @author Lomazzi Vincent
#' @author Raisin Edgar
#' @export
runDemo <- function() {
  appDir <- system.file("shiny-examples", "pi", package = "ptds2018hw4g5")
  if (appDir == "") {
    stop(
      "Could not find example directory. Try re-installing ptds2018hw4g5.",
      call. = FALSE
    )
  }

  shiny::runApp(appDir, display.mode = "normal")

}
