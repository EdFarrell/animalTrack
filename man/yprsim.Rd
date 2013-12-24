\name{yprsim}
\alias{yprsim}
\docType{data}
\title{Yaw, pitch, and roll simulation data.}
\description{
Simulated calibration sequence with 2 yaw, 2 pitch, and 2 roll rotations. No magnetic inclination is assumed. The yaw rotation is clockwise, with the x-axis facing north at the beginning and end of the rotations. The pitch rotation is a downward pitch with the x-axis facing north at the beginning. The roll rotation is a downward rotation with the x-axis facing west at the beginning. 
}
\usage{data(yprsim)}
\format{
  A data frame with 378 observations on the following 11 variables.
  \describe{
    \item{\code{mx}}{a numeric vector of magnetometer X-axis values.}
    \item{\code{my}}{a numeric vector of magnetometer Y-axis values.}
    \item{\code{mz}}{a numeric vector of magnetometer Z-axis values.}
    \item{\code{ax}}{a numeric vector of accelerometer X-axis values.}
    \item{\code{ay}}{a numeric vector of accelerometer Y-axis values.}
    \item{\code{az}}{a numeric vector of accelerometer Z-axis values.}
    \item{\code{pitch}}{pitch values (in radians from \eqn{-\pi/2 \le} to \eqn{\le \pi/2})}
    \item{\code{roll}}{roll values (in radians from \eqn{-\pi \le} to \eqn{\le \pi})}
    \item{\code{heading}}{heading values (in radians from \eqn{0} to \eqn{2\pi})}
    \item{\code{xh}}{tilt corrected X-axis magnetometer values (in radians)}
    \item{\code{yh}}{tilt corrected Y-axis magnetometer values (in radians)}
  }
}
\examples{
data(yprsim)
}
\keyword{datasets}