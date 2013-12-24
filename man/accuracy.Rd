\name{accuracy}
\alias{accuracy}
\alias{accuracy.improve}
\title{Estimate accuracy of three axis accelerometer/magnetometer.}
\description{The accuracy of the accelerometer and magnetometer is crucial to estimating pitch, roll, and heading. The axes must be orthogonal.}
\usage{
accuracy(x, y, z)

accuracy.improve(x, y, z)
}

\arguments{
  \item{x}{a numeric vector of x-axis values.}
  \item{y}{a numeric vector of y-axis values.}
  \item{z}{a numeric vector of z-axis values.}
}
\details{In the case of a three-axis accelerometer, the combined static acceleration (i.e. acceleration due to gravity) should always be 1\bold{g}. Accelerometers measure the Earth's gravity as well as movement of the animal in gravitational units, denoted as \bold{g} units. The sensor should be calibrated so that 1\bold{g} = -9.8 \eqn{m/s^2}, which is the acceleration due to gravity (this varies locally). The combined acceleration will vary from 1\bold{g} during dynamic acceleration (e.g. an animal accelerates to chase prey), or if the axes are not perfectly orthogonal. In the case of dynamic acceleration, an effort must be made to filter out the high frequency signal so that only the static signal (i.e. body orientation) remains. The \code{accuracy} function evaluates the accuracy of the accelerometer by calculating the vector norm of the acceleration, \eqn{A}, using the equation

\eqn{A = \sqrt{Ax^2 + Ay^2 + Az^2}}

and subsequently calculating the variance and standard deviation.
\cr
\cr
The \code{accuracy.improve} function provides an estimate of the Z-axis value of the accelerometer using the X and Y-axis values. The Z-axis estimate gives a value for Z that makes the vector norm 1bold{g} (i.e. all three axes are perfectly orthogonal and X and Y are assumed to be correct). This estimate for Z can then be used to evaluate error that may correspond to pitch angle, or may be a constant error. This will vary between sensors.

}
\note{
  The \code{accuracy.improve} function should be used to evaluate the accuracy of the Z-axis, assuming that the X and Y axes estimates are 
  reliable. If X and Y are not reliable, then the estimate for Z will be erroneous. This function is especially useful if the 
  accelerometer or magnetometer has a two axis X and Y module, and a one-axis Z module that may not be properly aligned within the 
  instrument housing.

}
\value{For \code{accuracy()}, a list containing the raw accuracy values (which should equal 1, in the case of perfect accuracy), variance, and standard deviation

  \item{acc }{vector containing the estimated accuracy values}
  \item{var }{variance of \code{acc}}
  \item{std }{standard deviation of \code{acc}}
  
  For \code{accuracy.improve()}, a list containing multiple components
  
  \item{z.est }{vector of estimated Z-axis values.}
  \item{z.resid }{vector of residuals (\code{z - z.est}).}
  \item{acc.input }{vector of accuracy values of the input (i.e. vector norm of \code{x, y, z}). Same as \code{accuracy().}}
  \item{acc.input.var }{accuracy variance of the input values.}
  \item{acc.input.std }{accuracy standard deviation of the input values.}
  \item{acc.improve }{vector of accuracy values using \code{z.est}. Same as \code{accuracy(x, y, z.est)}}
  \item{acc.improve.var }{accuracy variance of \code{acc.improve}.}
  \item{acc.improve.std }{accuracy standard deviation of \code{acc.improve}.}
  
}
\references{
Grygorenko, V. (2011), Sensing - magnetic compass with tilt compensation. Cypress Perform, AN2272, Document No. 001-32379 Rev. B.

Tuck, K. (2007), Tilt sensing using linear accelerometers. Freescale Semiconductor, AN3461 Rev. 2.
}
\author{Ed Farrell <edward.farrell27@gmail.com>}
\examples{

## Load seal dive data. see help(hugh) for details on variables.
## There are six dives, with surface intervals. here, we will
## look at the first 3 dives.
data(hugh)
## accuracy of the accelerometer.
A.acc.1 <- accuracy(hugh$Ax.dec[hugh$diven == 1],hugh$Ay.dec[hugh$diven == 1],
                    hugh$Az.dec[hugh$diven == 1])
A.acc.2 <- accuracy(hugh$Ax.dec[hugh$diven == 2],hugh$Ay.dec[hugh$diven == 2],
                    hugh$Az.dec[hugh$diven == 2])
A.acc.3 <- accuracy(hugh$Ax.dec[hugh$diven == 3],hugh$Ay.dec[hugh$diven == 3],
                    hugh$Az.dec[hugh$diven == 3])
## Plot the accuracy during the track.
## dive 1 has a high error due to some outliers. dive 2 looks much better.
## dive 3 has some outliers similar to dive 1.
plot(A.acc.1$acc)
plot(A.acc.2$acc)
plot(A.acc.3$acc)
## error angle in degrees
asin(A.acc.1$std)*(180/pi)
asin(A.acc.2$std)*(180/pi)
asin(A.acc.3$std)*(180/pi)

## See if there is a systematic error in the Z-axis. Warnings will occur
## if there are errors in the X, Y, or Z-axis (i.e. too large/small
## of a value). 
acc.d1 <- accuracy.improve(hugh$Ax.dec[hugh$diven == 1],hugh$Ay.dec[hugh$diven == 1],
                           hugh$Az.dec[hugh$diven == 1])
acc.d2 <- accuracy.improve(hugh$Ax.dec[hugh$diven == 2],hugh$Ay.dec[hugh$diven == 2],
                           hugh$Az.dec[hugh$diven == 2])
acc.d3 <- accuracy.improve(hugh$Ax.dec[hugh$diven == 3],hugh$Ay.dec[hugh$diven == 3],
                           hugh$Az.dec[hugh$diven == 3])
## Plot the Z-axis values, the estimates, and the residuals
## We can see that there are large spikes. These spikes may
## coincide with movement. The Z values during these spikes
## could be filtered out, or a new filter to the Z accelerometer
## may be needed. This will vary according to the sensor,
## type of animal, etc.
plot(hugh$Az.dec[hugh$diven == 1],ylim=c(-.5,1.5))
points(acc.d1$z.est,col='red')
lines(acc.d1$z.resid,col='blue')
legend(400,-.05,legend=c("Z-axis","Z-axis estimates","Residuals"),
       lty=c(0,0,1),pch=c(1,1,NA),col=c("black","red","blue"),bty="n")
plot(hugh$Az.dec[hugh$diven == 2],ylim=c(-.5,1.5))
points(acc.d2$z.est,col='red')
lines(acc.d2$z.resid,col='blue')
legend(300,-.05,legend=c("Z-axis","Z-axis estimates","Residuals"),
       lty=c(0,0,1),pch=c(1,1,NA),col=c("black","red","blue"),bty="n")
plot(hugh$Az.dec[hugh$diven == 3],ylim=c(-.5,1.5))
points(acc.d3$z.est,col='red')
lines(acc.d3$z.resid,col='blue')
legend(400,-.05,legend=c("Z-axis","Z-axis estimates","Residuals"),
       lty=c(0,0,1),pch=c(1,1,NA),col=c("black","red","blue"),bty="n")


}

