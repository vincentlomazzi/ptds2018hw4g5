#' This function makes an approximtion of the number pi
#'
#' @param B the length one numeric vector specifying the number of simulations.
#' @param seed allows the function to have always the same output
#'
#' @return two coordinates between -1 and 1 as well as a boolean output for each iterations
#'
#' @export
estimate_pi <- function(B = 5000, seed = 10){

  if (missing(B) && missing(seed)) {
    message(
      "No input values. The default values used will be B = 5000 seed = 10"
    )
  } else if ((B <= 0 || !(B %% 1 == 0)) && (seed <= 0 || !(seed %% 1 == 0))){
    stop("Please enter a positive and integer value B and for the seed")
  } else  if (B <= 0 || !(B %% 1 == 0)) {
    stop("Please enter a positive and integer value for B")
  } else if (seed <= 0 || !(seed %% 1 == 0)) {
    message("Please enter a positive and integer value for the seed")
  } else if (missing(B)){
    message("No B value defined. The default value used will be B = 5000")
  } else if (missing(seed)){
    message("No seed defined. The default seed will be seed = 10")
  }

  set.seed(seed)
  points <- data.frame(
    x = runif(n = B, min = -1, max = 1),
    y = runif(n = B, min = -1, max = 1),
    inside = rep(NA,B)
  )
  points$inside <- ifelse(points[,1] ^ 2 + points[,2] ^ 2 <= 1, TRUE, FALSE)

  estimated_pi <- 4 * sum(points$inside) / B

  rval <- list(
    estimated_pi = estimated_pi,
    points = points
  )

  class(rval) <- "pi"

  return(rval)
}

#' This function draws a plot representing the approximtion of the number pi above
#' @export
plot.pi <- function(x) {

  points <- x[["points"]]

  ggplot(as.data.frame(points), aes(x, y, color = inside))+
    geom_point() +
    geom_rect(aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1),
              colour = "blue", alpha = 0, linetype = 2) +
    annotate("path",
             x = cos(seq(0, 2 * pi, length.out = 100)),
             y = sin(seq(0, 2 * pi, length.out = 100))) +
    theme(legend.position = 'none',
          panel.background = element_rect(fill = 'white', color = 'black'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line('grey', linetype = 'dashed'),
          axis.line = element_line('black'), aspect.ratio = 1) +
    labs(x='x',y='y')
}
