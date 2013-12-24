\name{animalTrack-package}
\alias{animalTrack-package}
\alias{animalTrack}
\docType{package}
\title{Animal track reconstruction for high frequency 2-dimensional (2D) or 3-dimensional (3D) movement data.}
\description{
2D and 3D animal tracking data can be used to reconstruct tracks through time/space with correction based on known positions. 3D visualization of animal position and attitude.
}
\details{
\tabular{ll}{
Package: \tab animalTrack\cr
Type: \tab Package\cr
Version: \tab 1.0.0\cr
Date: \tab 2013-09-19\cr
License: \tab GPL (>=2)\cr
LazyLoad: \tab yes\cr
}

This package is intended for processing and visualization of high frequency animal tracking data, either 2D or 3D. As miniaturization technology has improved over the past 30 years, high resolution animal-borne data loggers have become increasingly used by scientists to study animal behavior. This package is intended to bridge the gap between raw multi-sensor data and meaningful calibrated variables (e.g. position, speed, attitude, heading) that can be used to understand natural behavior.\cr
\cr
All calculations that are performed to estimate the pitch, roll, and yaw angles of the animal are in the north-east-down (NED) frame of reference. It is important that the user either record measurements in the NED frame, or shift the raw measurements of each axis to NED before any calculations using this package.
}
\author{
\bold{Author:} Ed Farrell <edward.farrell27@gmail.com> and Lee Fuiman <lee.fuiman@utexas.edu>

\bold{Affiliation:} The University of Texas at Austin, Marine Science Institute \url{http://www.utmsi.utexas.edu/}

\bold{Maintainer:} Ed Farrell <edward.farrell27@gmail.com>
}
\keyword{ package }