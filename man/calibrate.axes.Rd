\name{calibrate.axes}
\alias{calibrate.axes}
\title{
Calibrate two accelerometer/magnetometer axes. Axes are scaled and centered.
}
\description{
Hard iron distortion compass calibration. Hard iron error arises from magnetized iron or steel on the compass platform that creates a constant offset for the compass axes. This offset is additive and constant.
}
\usage{
calibrate.axes(x, y, scale = 1, xcal = NULL, ycal = NULL)
}

\arguments{
  \item{x}{a numeric vector of axis values. This should be calibration rotations (i.e. yaw, pitch, roll)}
  \item{y}{a numeric vector of axis values. This should be calibration rotations (i.e. yaw, pitch, roll)}
  \item{scale}{the maximum value \code{x} and \code{y} will have after being scaled (i.e. values will range from \code{-scale} to \code{
  +scale}. Defaults to 1.}
  \item{xcal}{a numeric vector of axis values (corresponding to \code{x}). This should be track data that should be scaled according to
  the scale factor, offset, and \code{scale} variable. Optional.}
  \item{ycal}{a numeric vector of axis values (corresponding to \code{y}). This should be track data that should be scaled according to
  the scale factor, offset, and \code{scale} variable. Optional.}
}
\details{
A common source of compass error is hard and/or soft iron disturbance. Hard iron error arises from magnetized iron or steel on the compass platform that creates a constant offset for the compass axes. To account for the additive shift, an offset value is determined for each axis and applied by adding or subtracting the offset value from \code{x} or \code{y}. Soft iron error results from the interaction of soft iron near the compass and the earth's magnetic field. Soft iron error is a much more complicated distortion to account for and is not handled in this function. 
}
\value{returns a list containing scale factors, offset values, and scaled axes

  \item{xsf }{scale factor for x.}
  \item{ysf }{scale factor for y.}
  \item{xoff }{offset value for x.}
  \item{yoff }{offset value for y.}
  \item{xh }{calibrated x value. Scale factor and offset applied.}
  \item{yh }{calibrated y value. Scale factor and offset applied.}
  \item{xhscaled }{calibrated x value. Scale factor and offset applied, and scaled using \code{scale}.}
  \item{yhscaled }{calibrated y value. Scale factor and offset applied, and scaled using \code{scale}.}
  \item{xcal }{calibrated independent \code{xcal} value. Scale factor and offset applied to \code{xcal}, and scaled using \code{scale}.
  Optional. Will be NULL if \code{xcal} is not provided.}
  \item{ycal }{calibrated independent \code{ycal} value. Scale factor and offset applied to \code{ycal}, and scaled using \code{scale}.
  Optional. Will be NULL if \code{ycal} is not provided.}

}
\note{If no distortion is present, \code{plot(x,y)} will be circular and centered on (0,0). If hard-iron distortion is present, the center of the circle will not be (0,0), it will be shifted along the x, y, or both axes. However, the plot should still be circular. If the plot is not circular (e.g. ellipsoid), then soft-iron effects may be at play. In which case, a more complex calibration is needed. See Caruso (2000) for a discussion of compass and accelerometer errors. The hard-iron calibration used in this function is commonly implemented and can be found in Grygorenko (2011), among others. 
}
\section{Warning }{If any dynamic acceleration is present during the calibration procedure or track, it will result in an erroneous calibration. This can be avoided by keeping the sensor still and by pausing during rotations. Once the calibration constants are known for the instrument, they can be applied to the raw signal from the deployment on the animal. The raw accelerometer signal will include both static and dynamic acceleration, so the accelerometer signal must be filtered using a low-pass filter to extract the "static" portion of the signal. 
}

\references{
Caruso, M. J. (2000), Applications of magnetic sensors for low cost compass
systems. Proc. Position Location and Navigation Symp., IEEE 2000,
Mar. 2000, pp. 177-184

Grygorenko, V. (2011), Sensing - magnetic compass with tilt compensation. Cypress Perform, AN2272, Document No. 001-32379 Rev. B.
}
\author{Ed Farrell <edward.farrell27@gmail.com>}
\seealso{
  \code{\link{calibrate.axis}}
}
\examples{

## Import the missionbay_calib2 dataset. There is a calibration sequence 
## at Mission Bay, CA. Use help(missionbay_calib2) for info.

data(missionbay_calib2)

# Uncalibrated raw values
mx <- missionbay_calib2$mx_raw
my <- missionbay_calib2$my_raw

plot(mx,col='blue',type='l',lwd=3)
lines(my,col='green',lwd=3)

## Scale and center the x and y magnetometer axes against one another. Also, make 
## sure that each axis is in the NED (north-east-down) orientation.
cal <- calibrate.axes(mx, my)
attributes(cal)

plot(cal$xhscaled,col='blue',type='l',lwd=3)
lines(cal$yhscaled,col='green',lwd=3)

}

