\name{missionbay_calib2}
\alias{missionbay_calib2}
\docType{data}
\title{Magnetometer/accelerometer calibration data.}
\description{
Empirical magnetometer/accelerometer calibration data from Mission Bay, CA. These data are a sequence of 2 clockwise yaw rotations, 2
pitch rotations (nose down), and 2 roll rotations (nose facing west, starboard down).
}
\usage{data(missionbay_calib2)}
\format{
  A data frame with 275 observations on the following 15 variables.
  \describe{
    \item{\code{date}}{a Date}
    \item{\code{time}}{a POSIXlt}
    \item{\code{year}}{a numeric vector}
    \item{\code{month}}{a numeric vector}
    \item{\code{day}}{a numeric vector}
    \item{\code{hour}}{a numeric vector}
    \item{\code{minute}}{a numeric vector}
    \item{\code{second}}{a numeric vector}
    \item{\code{mx_raw}}{a numeric vector. Magnetometer x-axis value in bits}
    \item{\code{my_raw}}{a numeric vector. Magnetometer y-axis value in bits}
    \item{\code{mz_raw}}{a numeric vector. Magnetometer z-axis value in bits}
    \item{\code{ax_raw}}{a numeric vector. Accelerometer x-axis value in bits}
    \item{\code{ay_raw}}{a numeric vector. Accelerometer y-axis value in bits}
    \item{\code{az_raw}}{a numeric vector. Accelerometer z-axis value in bits}
    \item{\code{rotation}}{a character vector. One of \code{c("yaw1: clockwise",}\cr
    \code{"yaw2: clockwise","pitch1: nose down", "pitch2: nose down",}\cr
    \code{"roll1: starboard down","roll2: starboard down")}}
  }
}
\details{
These data are raw bit values from a three-axis magnetometer and accelerometer. See the \code{$rotation} column for 
when each rotation starts and ends.
}
\source{
These data were recorded by Bill Hagey and Randall Davis.
}
\examples{
data(missionbay_calib2)

}
\keyword{datasets}
