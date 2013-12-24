\name{hugh}
\alias{hugh}
\docType{data}
\title{Seal "hugh" dive data.}
\description{
A sample of recorded field data from a seal. The seal makes 6 dives with 5 surface intervals. Data have been pre-filtered but are in somewhat of an intermediate form for the purposes of examples within the \code{animalTrack} package. 
}
\usage{data(hugh)}
\format{
  A data frame with 13,953 observations on the following 21 variables.
  \describe{
    \item{\code{Year}}{year}
    \item{\code{Month}}{month}
    \item{\code{Day}}{day}
    \item{\code{Hour}}{hour}
    \item{\code{Minute}}{minute}
    \item{\code{Second}}{second}
    \item{\code{MX}}{a numeric vector of magnetometer X-axis values (in bits)}
    \item{\code{MY}}{a numeric vector of magnetometer Y-axis values (in bits)}
    \item{\code{MZ}}{a numeric vector of magnetometer Z-axis values (in bits)}
    \item{\code{depth.cal}}{depth (meters)}
    \item{\code{MX2}}{a numeric vector of temperature calibrated magnetometer x-axis values (in bits)}
    \item{\code{MY2}}{a numeric vector of temperature calibrated magnetometer Y-axis values (in bits)}
    \item{\code{MZ2}}{a numeric vector of temperature calibrated magnetometer Z-axis values (in bits)}
    \item{\code{MXone}}{a numeric vector of temperature calibrated magnetometer X-axis values scaled from -1 to +1}
    \item{\code{MYone}}{a numeric vector of temperature calibrated magnetometer Y-axis values scaled from -1 to +1}
    \item{\code{MZone}}{a numeric vector of temperature calibrated magnetometer Z-axis values scaled from -1 to +1}
    \item{\code{speed.cal}}{calibrated speed (meters per second)}
    \item{\code{Ax.dec}}{a numeric vector of calibrated accelerometer x-axis values scaled from -1 to +1g. A low-pass filter was applied in
    order to obtain "static" acceleration.}
    \item{\code{Ay.dec}}{a numeric vector of calibrated accelerometer Y-axis values scaled from -1 to +1g. A low-pass filter was applied in
    order to obtain "static" acceleration.}
    \item{\code{Az.dec}}{a numeric vector of calibrated accelerometer Z-axis values scaled from -1 to +1g. A low-pass filter was applied in
    order to obtain "static" acceleration.}
    \item{\code{diven}}{dive number}
  }
}
\source{
Data were collected under NSF Grant #0739390 and NOAA Marine Mammal Permit #984-1814. 
}
\examples{
data(hugh)

}
\keyword{datasets}
