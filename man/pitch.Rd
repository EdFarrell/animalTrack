\name{pitch}
\alias{pitch}
\title{Calculate pitch angle.}
\description{
calculate pitch angle using measurements from a 3-axis accelerometer.
}
\usage{
pitch(x, y, z)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector of x-axis values.}
  \item{y}{a numeric vector of y-axis values.}
  \item{z}{a numeric vector of z-axis values.}
}
\details{
\code{pitch()} will return a pitch angle (in radians) representing the posterior-anterior axis position of the animal. This angle is the             difference between the posterior-anterior axis (vector) of the animal and the horizon. This angle is calculated assuming the NED (north, east, down) frame of reference. Typically, this is measured with an accelerometer and is scaled from -1 to +1. Using NED means that
when the accelerometer is measuring 1g, this is equal to -9.8 \eqn{m/s^2}. This also means that if the axis is aligned with
the earth gravity vector (i.e. down, towards the center of the earth), it should have a value of +1. Pitch angle is calculated using

\deqn{pitch = -atan(x/\sqrt(y^2 + z^2))}.
  
A positive pitch value indicates that the nose of the animal (x-axis) is pitched upward (toward the sky). Conversely, a negative pitch angle indicates that the animal is pitched downward. (Note: axis values must be converted to NED frame of reference prior to using this function)
}
\value{an object of pitch values (in radians) from \eqn{-\pi/2 \le} \code{pitch} \eqn{\le \pi/2}
}
\references{
Tuck, K. (2007), Tilt sensing using linear accelerometers. Freescale Semiconductor, AN3461 Rev. 2.

Smith, K. J. (1998), \emph{Essentials of Trigonometry}. Pacific Grove, CA: Brooks/Cole.
}
\author{Ed Farrell <edward.farrell27@gmail.com>}
\seealso{
\code{\link{pitch2}} \code{\link{atan}}
}
\examples{

## Import the yaw, pitch and roll simulated calibration dataset. For
## an explanation of the data use help(yprsim).
data(yprsim)

## Calculate pitch (the nose/x-axis is rotated downward through 2 full rotations)
phi <- pitch(yprsim$ax,yprsim$ay,yprsim$az)

## Plot
plot(phi*(180/pi),type='l',lty=1,lwd=2,xlab="time (s)",ylab="pitch (degrees)",
     main="Pitch Calculation (2 pitch rotations)")
abline(v=c(126,252),lty=3,lwd=2)
legend(-10,70,legend=c("Pitch","Change in \n Rotation"),col=c("black","black"),
       lty=c(1,3),bty="n")
text(50,-70,"Yaw");text(175,-70,"Pitch");text(320,-70,"Roll")

}

