estimate_pi <- function(B = 5000, seed = 10) {
  # set a seed
  set.seed(seed)
  # simulate B points
  points <- data.frame(
    x = runif(n = B, min = -1, max = 1),
    y = runif(n = B, min = -1, max = 1),
    inside = rep(NA,B)
  )
  points$inside <- ifelse(points[,1] ^ 2 + points[,2] ^ 2 <= 1, TRUE, FALSE)

  # your loop goes here
  # i <- 1
  # inCircle <- 0
  # for(i in 1:B){
  #   point <- points[i,3]
  #   if(point){
  #     inCircle <- inCircle + 1
  #   }
  # }
  #
  # estimated_pi <- 4 * inCircle / B

  estimated_pi <- 4 * sum(points$inside) / B

  # create a new list
  rval <- list(
    estimated_pi = estimated_pi,
    points = points
  )
  # assign pi class to rval
  class(rval) <- "pi"
  # return rval
  return(rval)
}


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
