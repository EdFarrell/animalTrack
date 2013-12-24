\name{roll}
\alias{roll}
\title{Calculate roll angle.}
\description{
calculate roll angle using measurements from a three-axis accelerometer. However, only two axes are required.
}
\usage{
roll(y, z)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{a numeric vector of y-axis values.}
  \item{z}{a numeric vector of z-axis values.}
}
\details{
Roll angle is the angle between the left-right axis and the horizon. The angle is calculated assuming the NED
(north, east, down) frame of reference, where x is north, y is east, and z is down. Typically, this is measured with an accelerometer and is scaled from -1 to +1. Using NED, this means that when the accelerometer is measuring 1g, this is equal to -9.8 \eqn{m/s^2}. This also means that if the axis is aligned with the earth gravity vector (i.e. down, towards the center of the earth), it should have a value of +1. Roll angle is calcualted by 

\eqn{roll = atan2(y,z)}

A downward movement of the y-axis gives a positive roll angle.
}
\value{an object of roll values (in radians) from \eqn{-\pi \le} \code{roll} \eqn{\le \pi}
}
\references{
Ozyagcilar, T. (2012), Implementing a tilt-compensated eCompass using accelerometer and magnetometer sensors. Freescale semiconductor, AN 4248, Rev. 3.

Smith, K. J. (1998), \emph{Essentials of Trigonometry}. Pacific Grove, CA: Brooks/Cole. 

}
\author{Ed Farrell <edward.farrell27@gmail.com>}
\examples{
## Import the yaw, pitch and roll simulated calibration dataset. For
## an explanation of the data use help(yprsim).
data(yprsim)

## Assign accelerometer variables
ay <- yprsim$ay
az <- yprsim$az

## Calculate roll
theta <- roll(ay, az)

plot(theta*(180/pi),type='l',lty=1,lwd=2,xlab="time (s)",ylab="pitch (degrees)",
     main="Roll Calculation (2 roll rotations)")
abline(v=c(126,252),lty=3,lwd=2)
legend(-10,110,legend=c("Roll","Change in \n Rotation"),
       col=c("black","black"),lty=c(1,3),bty="n")
text(50,-70,"Yaw");text(175,-70,"Pitch");text(320,-70,"Roll")

}

