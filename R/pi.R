#' @title Monte Carlo estimation of Pi
#' @description This function makes an approximtion of the number pi based on a
#' random drawing of points between -1 and 1 and establishes whether they fall or
#' not within a circle of radius 1.
#' @param B the length one numeric vector specifying the number of simulations.
#' @param seed allows the function to have always the same output
#' @return A matrix of two coordinates between -1 and 1 as well as a boolean
#' output for each iterations
#' @author Germano David
#' @author Lomazzi Vincent
#' @author Bron Luca
#' @author Raisin Edgar
#' @author Grandadam Patrik
#' @export
#' @examples
#' estimate_pi(B = 5000)
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

#' @title Plot objects of resulting from the estimate_pi function
#' @description This function draws a plot representing the approximtion of the
#' number pi obtained with the estimate_pi function
#' @param x An output from the estimate_pi function
#' @return A plot of the simulated points
#' @author Germano David
#' @author Lomazzi Vincent
#' @author Bron Luca
#' @author Raisin Edgar
#' @author Grandadam Patrik
#' @export
#' @examples
#' estimated_pi <- estimate_pi(5000)
#' plot(estimated_pi)

plot.pi <- function(x) {

  points <- x[["points"]]

  ggplot2::ggplot(as.data.frame(points), ggplot2::aes(x, y, color = inside))+
    ggplot2::geom_point() +
    ggplot2::geom_rect(ggplot2::aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1),
              colour = "blue", alpha = 0, linetype = 2) +
    ggplot2::annotate("path",
             x = cos(seq(0, 2 * pi, length.out = 100)),
             y = sin(seq(0, 2 * pi, length.out = 100))) +
    ggplot2::theme(legend.position = 'none',
          panel.background = ggplot2::element_rect(fill = 'white', color = 'black'),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_line('grey', linetype = 'dashed'),
          axis.line = ggplot2::element_line('black'), aspect.ratio = 1) +
    ggplot2::labs(x = 'x', y = 'y')
}

#' @title Monte Carlo estimation of Pi with c++
#' @description This function makes an approximtion of the number pi based on a
#' random drawing of points between -1 and 1 and establishes whether they fall or
#' not within a circle of radius 1 using c++.
#' @param B the length one numeric vector specifying the number of simulations.
#' @param seed allows the function to have always the same output
#' @return A matrix of two coordinates between -1 and 1 as well as a boolean
#' output for each iterations
#' @author Germano David
#' @author Lomazzi Vincent
#' @author Bron Luca
#' @author Raisin Edgar
#' @author Grandadam Patrik
#' @export
#' @examples
#' estimate_pi2(B = 5000)

estimate_pi2 <- function(B = 5000, seed = 10){
  set.seed(seed)

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

  points <- df_pi(B)  # generating random points from the df_pi function in c++
  inside <- is_inside(points)  # checking if the points are inside  the circle
  points <- data.frame(points, inside)
  colnames(points) <- c("x","y", "inside")
  estimated_pi2 <- 4 * sum(inside) / B

  rval <- list(
    estimated_pi2 = estimated_pi2,
    points = points
  )

  class(rval) <- "pi"

  return(rval)
}



