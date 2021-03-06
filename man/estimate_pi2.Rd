% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pi.R
\name{estimate_pi2}
\alias{estimate_pi2}
\title{Monte Carlo estimation of Pi with c++}
\usage{
estimate_pi2(B = 5000, seed = 10)
}
\arguments{
\item{B}{the length one numeric vector specifying the number of simulations.}

\item{seed}{allows the function to have always the same output}
}
\value{
A matrix of two coordinates between -1 and 1 as well as a boolean
output for each iterations
}
\description{
This function makes an approximtion of the number pi based on a
random drawing of points between -1 and 1 and establishes whether they are or
not in a circle of radius 1 using c++.
}
\examples{
estimate_pi2(B = 5000)
}
\author{
Germano David

Lomazzi Vincent

Bron Luca

Raisin Edgar

Grandadam Patrik
}
