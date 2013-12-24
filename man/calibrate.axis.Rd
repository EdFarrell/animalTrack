\name{calibrate.axis}
\alias{calibrate.axis}
\title{Scale and center an accelerometer/magnetometer axis.}
\description{
Scale and center \code{x} using the minimum and maximum values of the sensor. \code{scale} value indicates the maximum value \code{x} will have after being scaled (i.e. values will range \code{-scale to}  \code{+scale}).
}
\usage{
calibrate.axis(x, max, min, scale = 1)
}
\arguments{
  \item{x}{a numeric vector of axis values.}
  \item{max}{maximum uncalibrated value of the axis. This should be measured empirically through a series of calibration
  rotations. For a magnetometer axis, \code{max} should be the value of the Earth's "total field" at the locality (i.e. 
  when the axis is aligned with the Earth's total field vector). The \code{min} value would conversely be the value when
  the axis is pointed in the opposite direction. The same logic applied if using accelerometer axis, but instead of the 
  Earth's magnetic field, the gravity vector of the Earth is used (i.e. vector pointing toward the center of the Earth).}
  \item{min}{see \code{max}. value when axis is pointed opposite of reference vector (e.g. Earth magnetic or gravity field).}
  \item{scale}{the maximum value \code{x} will have after being scaled (i.e. values will range from \code{-scale to}  \code{+scale}. Defaults to -1 to +1.}
}
\details{
This function is useful for calibrating the axes of magnetometers/accelerometers and correcting for hard-iron distortion. Centering and scaling each axis will standardize and essentially calibrate each axis so that the simulaneous measurements of each will be trigonometrically accurate. This function effectively deals with hard iron distortion problems as well as small misalignments between axes. However, there are other factors that can cause discontinuity (e.g. temperature, electronic noise, etc.) that are not accounted for in this function. For a more complete discussion of dealing with compass errors see Caruso (2000).
}
\value{object of  scaled/centered values of \code{x}.
}
\note{If no distortion is present, \code{plot(x,y)} will be circular and centered on (0,0). If hard-iron distortion is present, the center of the circle will not be (0,0), it will be shifted along the x, y, or both axes. However, the plot should still be circular. If the plot is not circular (e.g. ellipsoid), then soft-iron effects may be at play. In which case, a more complex calibration is needed. See Caruso (2000) for a discussion of compass and accelerometer errors. The hard-iron calibration used in this function is commonly implemented and can be found in Grygorenko (2011), among others. 
}
\section{Warning }{If any dynamic acceleration is present during the calibration procedure or track, it will result in an erroneous calibration. This can be avoided by keeping the sensor still and by pausing during rotations. Once the calibration constants are known for the instrument, they can be applied to the raw signal from the deployment on the animal. But remember, the raw accelerometer signal will contain both static and dynamic acceleration, so the accelerometer signal must be filtered using a low-pass filter to extract the static signal.
}
\references{
Caruso, M. J. (2000), Applications of magnetic sensors for low cost compass
systems. Proc. Position Location and Navigation Symp., IEEE 2000,
Mar. 2000, pp. 177-184
}
\author{Ed Farrell <edward.farrell27@gmail.com>}
\seealso{
\code{\link{calibrate.axes}}
}
\examples{

## Import the missionbay_calib2 dataset. There is a calibration sequence 
## at Mission Bay, CA. Use help(missionbay_calib2) for info.

data(missionbay_calib2)
mx <- missionbay_calib2$mx_raw
my <- missionbay_calib2$my_raw
mz <- missionbay_calib2$mz_raw

ax <- missionbay_calib2$ax_raw
ay <- missionbay_calib2$ay_raw
az <- missionbay_calib2$az_raw
## Scale and center the magnetometer axes. Also, make sure that each axis 
## is in the NED (north-east-down) orientation (this is the reason for the
## negative signs).

Mxscaled <- calibrate.axis(mx, max(mx), min(mx))
Myscaled <- -calibrate.axis(my, max(my), min(my))
Mzscaled <- -calibrate.axis(mz, max(mz), min(mz))

## Scale and center the accelerometer axes. Also, make sure that each axis 
## is in the NED (north-east-down) orientation.

Axscaled <- -calibrate.axis(ax, max(ax), min(ax))
Ayscaled <- calibrate.axis(ay, max(ay), min(ay))
Azscaled <- calibrate.axis(az, max(az), min(az))

## Compare
par(mfrow=c(2,3))
plot(mx,type="l",col="blue",xlab="time (s)",ylab="bits",main="Mx Raw",lwd=3)
plot(my,type="l",col="blue",xlab="time (s)",ylab="bits",main="My Raw",lwd=3)
plot(mz,type="l",col="blue",xlab="time (s)",ylab="bits",main="Mz Raw",lwd=3)
plot(Mxscaled,type="l",col="red",xlab="time (s)",ylab="bits",main="Mx Calibrated",lwd=3) 
plot(Myscaled,type="l",col="red",xlab="time (s)",ylab="bits",main="My Calibrated",lwd=3) 
plot(Mzscaled,type="l",col="red",xlab="time (s)",ylab="bits",main="Mz Calibrated",lwd=3) 

}

