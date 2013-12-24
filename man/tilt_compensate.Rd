\name{tilt_compensate}
\alias{tilt_compensate}
\title{Tilt compensated compass.}
\description{
calculate tilt compensated compass heading using three-axis magnetometer and accelerometer. Magnetometer and accelerometer must be in NED frame of reference. 
}
\usage{
tilt_compensate(x, y, z, pitch, roll, declination = 0, angle = "degree")
}
\arguments{
  \item{x}{a numeric vector of magnetic x-axis values (e.g. anterior axis of animal)}
  \item{y}{a numeric vector of magnetic y-axis values (e.g. starboard axis of animal)}
  \item{z}{a numeric vector of magnetic z-axis values (e.g. downward/ventral axis of animal)}
  \item{pitch}{a numeric vector of pitch angle values. values must range from \eqn{-\pi/2 \le} \code{pitch} \eqn{\le \pi/2}. (NOTE:
  pitch must be calculated using a static acceleration signal.)}
  \item{roll}{a numeric vector of roll angle values. values must range from \eqn{-\pi \le} \code{roll} \eqn{\le \pi}. (NOTE:
  pitch must be calculated using a static acceleration signal.)}
  \item{declination}{magnetic declination angle at animal track locality.}
  \item{angle}{unit of angular measurement of \code{pitch, roll, declination}
  }
}
\details{
Accurate compass heading is crucial when reconstructing an animal's 3D (or 2D) track through space, whether in the water or through the air. One of the goal's of deploying high frequency bio-logging tags on large fauna is to relate physiological parameters to geolocation. Tags that are equipped with a three-axis accelerometer and three-axis magnetometer can accurately measure the the attitude and movement of an animal. Each tag usually has a unique configuration for how the magnetometer and accelerometer are placed in the instrument, so it may be necessary to rotate the sensor outputs so that they are in the standard north-east-down (NED) frame of reference (also referred to as the "body frame", common in aviation).  

The need for tilt compensation arises because free-ranging animals are not always oriented parallel to the horizon while moving or stationary. When a compass is tilted, the basic trigonometric functions for calculating heading using the x and y axes fail. In order to correct for this, the x and y values are re-projected back to the horizontal plane using Euler angles. The arctan(yh/xh) is then used to solve for the compass angle on the horizontal plane. For a proof of the tilt compensation equation see Ozyagcilar (2012) or Grygorenko (2011). Furthermore, a practical application of tilt compensated heading as well as a proof can be found in Johnson & Tyack (2003).  
}
\value{Object of class \code{tiltcompensate}, which is a list with the following components:

  \item{xh }{numeric vector of tilt compensated values of magnetic x-axis.}
  \item{yh }{numeric vector of tilt compensated values of magnetic y-axis.}
  \item{heading_mag }{numeric vector of compass heading values (in radians).}
  \item{heading_geo }{numeric vector of compass heading values corrected for declination angle (in radians). If no declination angle is
  given, this will be the same as \code{heading_mag}.}

}
\references{
Cai, G., Chen, B. M., and T. H. Lee (2011). Coordinate systems and transformations. In \emph{Unmanned Rotocraft Systems} (pp. 23-34). Springer, New York, NY.

Grygorenko, V. (2011), Sensing - magnetic compass with tilt compensation. Cypress Perform, AN2272, Document No. 001-32379 Rev. B.

Johnson, M. P. & P. L. Tyack (2003), A digital acoustic recording tag for measuring the response of wild animals to sound. IEEE Journal of Ocean Engineering, \bold{28}, 3:12.

Ozyagcilar, T. (2012), Implementing a tilt-compensated eCompass using accelerometer and magnetometer sensors. Freescale semiconductor, AN 4248, Rev. 3.
}
\author{Ed Farrell <edward.farrell27@gmail.com>}
\note{Geomagnetic declination can be found at \url{http://www.ngdc.noaa.gov/}. Since the earth's magnetic field is dynamic, the declination value should be at the date and location of the study area.

Pitch and roll must also be calculated in the NED frame of reference.

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{pitch}},  \code{\link{roll}},  \code{\link{calibrate.axis}},  \code{\link{calibrate.axes}}
}
\examples{

## Import the yprsim dataset. This is a series of 2 yaws, 2 pitches, 
## and 2 rolls. It is a simulated calibration Sequence. For details see 
## help(yprsim)

data(yprsim)

## assign variables

mx <- yprsim$mx
my <- yprsim$my
mz <- yprsim$mz

ax <- yprsim$ax
ay <- yprsim$ay
az <- yprsim$az

## The magnetometer and accelerometer data are already scaled and centered in the 
## NED frame. Also, there is no magnetic inclination simulated in this dataset. 
## So, this calibration simulation would be valid at the magnetic equator.

pitch <- pitch(ax, ay, az)
roll <- roll(ay, az)

## calculate tilt compensated heading. with no declination.

tilt <- tilt_compensate(mx, my, mz, pitch, roll, 
                        declination = 0, angle = "radian")

## Check the list of variables that is produced.
attributes(tilt)

## Plot
plot(tilt$heading_mag*(180/pi),col='red',type="l",lwd=2,ylim=c(-180,360),
     xlab="time (s)", ylab="degrees",main="Simulated Heading,
       Yaw, Pitch, and Roll")
lines(pitch*(180/pi),col='black',type='l',lty=1,lwd=2)
lines(roll*(180/pi),col='blue',lty=2,lwd=2)
abline(v=c(126,252),lty=3,lwd=2)
legend(0,-50,legend=c("Heading","Pitch","Roll"),col=c("red","black","blue"),
       lty=c(1,1,2),bty="n")

## Create a series of 3d plots, that can be saved to a 
## file directory. These can then be used for animation.
\dontrun{  
  ## make sure that the "rgl" library is installed
  
  filename <- "~Rdir/"
  # Make an animated 3d plot of all the rotations
  
  for (i in 1:(length(yprsim[,1]))){
    ii <- roll[i]; jj <- pitch[i]; kk <- tilt$heading[i]
    animalplot3d(ii, jj, kk,angle = "radian")
    par3d(windowRect = c(50,50,700,700)) # make the window a bit larger.
    #save each plot as a png
    rgl.snapshot(filename=paste(filename,as.character(i),".png",sep=""),fmt="png") 
  }
}


}
