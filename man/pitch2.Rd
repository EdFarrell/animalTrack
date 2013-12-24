\name{pitch2}
\alias{pitch2}
\title{Calculate pitch angle.}
\description{
calculate pitch angle using measurements from a three-axis accelerometer and roll angle.
}
\usage{
pitch2(x, y, z, roll)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector of x-axis values.}
  \item{y}{a numeric vector of y-axis values.}
  \item{z}{a numeric vector of z-axis values.}
  \item{roll}{numeric vector of roll angles. (\eqn{-\pi \le} \code{roll} \eqn{\le \pi})  }
}

\details{
\code{pitch2()} will return a pitch angle (in radians) representing the posterior-anterior axis position of the animal. This angle is the             difference between the posterior-anterior axis (vector) of the animal and the horizon. This angle is calculated assuming the NED (north, east, down) frame of reference. Typically, this is measured with an accelerometer and is scaled from -1 to +1. Using the NED frame of reference means that when the accelerometer is measuring 1g, this is equal to -9.8 \eqn{m/s^2}. Also, if the axis is aligned with the earth gravity vector (i.e. down, towards the center of the earth), it should have a value of +1. Pitch angle is calculated by 

\deqn{pitch = atan(-x/(y*sin(roll)+(z*cos(roll)))  )  }
  
A positive pitch value indicates that the nose of the animal (x-axis) is pitched upward (toward the sky). Conversely, a negative pitch angle indicates that the animal is pitched downward. (Note: axis values must be converted to NED frame of reference prior to using this function)
}
\value{an object of pitch values (in radians) from \eqn{-\pi/2 \le} \code{pitch} \eqn{\le \pi/2}
}
\references{
Ozyagcilar, T. (2012), Implementing a tilt-compensated eCompass using accelerometer and magnetometer sensors. Freescale semiconductor, AN 4248, Rev. 3.

Smith, K. J. (1998), \emph{Essentials of Trigonometry}. Pacific Grove, CA: Brooks/Cole.
}
\author{Ed Farrell <edward.farrell27@gmail.com>}
\seealso{
\code{\link{pitch}} \code{\link{atan}}
}
\examples{
## Import the yaw, pitch and roll simulated calibration dataset. For
## an explanation of the data use help(yprsim).
data(yprsim)

## Accelerometer axes
ax <- yprsim$ax
ay <- yprsim$ay
az <- yprsim$az

## Calculate roll
theta <- roll(ay, az)

## Calculate pitch (the nose/x-axis is rotated downward through 2 full rotations)
phi <- pitch2(ax, ay, az, theta)

## Plot
plot(phi*(180/pi),type='l',lty=1,lwd=2,xlab="time (s)",ylab="pitch (degrees)",
     main="Pitch2 Calculation (2 pitch rotations)")
abline(v=c(126,252),lty=3,lwd=2)
legend(-10,70,legend=c("Pitch","Change in \n Rotation"),
       col=c("black","black"),lty=c(1,3),bty="n")
text(50,-70,"Yaw");text(175,-70,"Pitch");text(320,-70,"Roll")

}

