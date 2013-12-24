\name{missionbay2}
\alias{missionbay2}
\docType{data}
\title{Mission Bay navigation trials.}
\description{Data from calibrated field trials in Mission Bay, CA.}
\usage{data(missionbay2)}
\format{
  A data frame with 3,020 observations on the following 27 variables.
  \describe{
    \item{\code{date}}{a Date}
    \item{\code{time}}{a POSIXlt}
    \item{\code{year}}{year}
    \item{\code{month}}{month}
    \item{\code{day}}{day}
    \item{\code{hour}}{hour}
    \item{\code{minute}}{minute}
    \item{\code{second}}{second}
    \item{\code{speed}}{calibrated speed (meters per second)}
    \item{\code{depth}}{depth (meters)}
    \item{\code{mx_raw}}{a numeric vector of magnetometer X-axis values (in bits)}
    \item{\code{my_raw}}{a numeric vector of magnetometer Y-axis values (in bits)}
    \item{\code{mz_raw}}{a numeric vector of magnetometer Z-axis values (in bits)}
    \item{\code{ax_raw}}{a numeric vector of accelerometer X-axis values (in bits)}
    \item{\code{ay_raw}}{a numeric vector of accelerometer Y-axis values (in bits)}
    \item{\code{az_raw}}{a numeric vector of accelerometer Z-axis values (in bits)}
    \item{\code{mx_scaled}}{a numeric vector of calibrated magnetometer X-axis values scaled from -1 to +1}
    \item{\code{my_scaled}}{a numeric vector of calibrated magnetometer Y-axis values scaled from -1 to +1}
    \item{\code{mz_scaled}}{a numeric vector of calibrated magnetometer Z-axis values scaled from -1 to +1}
    \item{\code{ax_scaled}}{a numeric vector of calibrated accelerometer X-axis values scaled from -1 to +1}
    \item{\code{ay_scaled}}{a numeric vector of calibrated accelerometer Y-axis values scaled from -1 to +1}
    \item{\code{az_scaled}}{a numeric vector of calibrated accelerometer Z-axis values scaled from -1 to +1}
    \item{\code{roll}}{roll values in radians (\eqn{-\pi} to \eqn{+\pi})}
    \item{\code{pitch}}{pitch values in radians (\eqn{-\pi/2} to \eqn{+\pi/2})}
    \item{\code{heading_mag}}{magnetic heading direction in radians (\eqn{0 - 2\pi})}
    \item{\code{heading_geo}}{geographic heading (corrected for declination) direction in radians (\eqn{0 - 2\pi})}
    \item{\code{trial}}{trial number, 1-4}
  }
}
\details{
Four trials were conducted in which an instrument that was recording
speed, magnetic fields, acceleration, depth, and time was towed behind a boat to a series of known locations. During each 
trial, the boat returned to the starting location after a short trip.
}
\source{
These data were recorded with great seamanship by Bill Hagey and Randall Davis
}
\examples{
data(missionbay2)
}
\keyword{datasets}