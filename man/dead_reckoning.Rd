\name{dead_reckoning}
\alias{dead_reckoning}
\title{Create a course steered and course made good using the principles of navigation by dead reckoning.}
\description{
Calculate past, present, or future position using "dead reckoning" Bowditch (1995). Specifically, the function calculates the "course steered" and the "course made good" (if end position is known). 
}
\usage{
dead_reckoning(speed, heading, angle = "degree", ret = TRUE, depth = NULL,
                     pitch = NULL, endcoords=NULL, speedhorizontal = "corrected")
}
\arguments{
  \item{speed}{
  an object containing the values for measured speed (i.e. speed through water). (Note: this
  is assumed to be horizontal speed (i.e. no pitch), unless specified in the \code{speedhorizontal} argument.
  }
  \item{heading}{
  an object containing the values for measured heading. Can be one of c("degree",
  "radian").
  }
  \item{angle}{
  Unit of angular measure for \code{heading}. Default is "degree", but will accept "radian".
  }
  \item{ret}{
  Does the animal return to the same position as where it started? (i.e. (0,0))
  }
  \item{depth}{
  an object containing the values for measured depth (e.g pressure sensor, or altitude sensor).optional.
  }
  \item{pitch}{
  an object containing the values for calculated pitch. Both the \code{\link{pitch}} or 
  \code{\link{pitch2}} functions can be used. (Note: must be in radians). Optional.
  }
  \item{endcoords}{
  Coordinates for known location at the end of the track, \code{c(x,y)}. If \code{ret == F}, what 
  are the ending cartesian coordinates (i.e. (\emph{x,y}))? This is the location
  where \code{dead_reckoning} will correct the course to based on a constant set and drift.(NOTE: \code{ret}   
  must be \code{FALSE}).
  }
  \item{speedhorizontal}{Indicates how the input speed values should be handled. Default is \code{"corrected"}, 
  which indicates that the input speed values are horizontal speeds (i.e. speed across a plane tangent
  to the surface of the earth). If \code{"pitch"}, then speed is assumed to be a 3D vector and will be corrected
  to horizontal using \eqn{speedh = cos(\phi)*}\code{speed}. If \code{"depth"}, then speed is assumed to be a 3D 
  vector and will be corrected to horizontal using \eqn{speedh = \sqrt (speed^2 - \Delta depth^2)}. Optional.
  }
}
\details{
See Bowditch (1995) for a complete discussion of dead reckoning and navigation. This function extends traditional 
navigation calculations by providing the flexibility for 3-dimensional parameters common in animal tracking data.
}
\value{Object of class \code{navigate}, which is a list with the following components:

  \item{CSx }{a numeric vector with the x-coordinates for the calculated course steered. Position ascertained through "dead reckoning".
  }
  \item{CSy }{a numeric vector with the y-coordinates for the calculated course steered. Position ascertained through "dead reckoning".
  }
  \item{CMGx }{a numeric vector with the x-coordinates for the calculated course made good. Will be \code{NA} if
  end location is unknown (i.e. \code{ret == F} and \code{endcoords = NULL}. This is what Bowdith refers to as an "estimated position".)
  }
  \item{CMGy }{a numeric vector with the y-coordinates for the calculated course made goodWill be \code{NA} if
  end location is unknown (i.e. \code{ret == F} and \code{endcoords = NULL}. This is what Bowdith refers to as an "estimated position".)
  }
  \item{speedh }{speed horizontal. A numeric vector of corrected horizontal speed values. If \code{speedhorizontal = "corrected"}
  , then this will be the same as the input speed}
  \item{speedmg }{speed made good. A numeric vector of speed made good values. These are the speeds between observations after 
  correcting the animal track for a course made good}
  \item{drift }{the speed of the current that resulted in the course steered. This is the distance between the course 
  steered end location and the known (i.e. real) end location, divided by time. This represents the speed of the prevailing
  current, which is assumed to be constant
  }
  \item{errordistance }{This is the distance between the course steered end location and the known (i.e. real) end location}
  \item{set }{compass direction of the current. \eqn{0 <=} \code{set} \eqn{< 2\pi}
  }
}
\references{
Bowditch, N. (1995), \emph{The New American Practical Navigator}. Bethesda, MD: Defense Mapping Agency Hydrographic Topographic Center.
}
\author{Ed Farrell <edward.farrell27@gmail.com>}
\examples{

## Import the "missionbay2" dataset. See help(missionbay2)
## for full documentation.
data(missionbay2)

trial.1 <- missionbay2[missionbay2$trial == 1,]
trial.2 <- missionbay2[missionbay2$trial == 2,]
trial.3 <- missionbay2[missionbay2$trial == 3,]
trial.4 <- missionbay2[missionbay2$trial == 4,]

## Calculate the course made good for the four trials. Each returns
## to the starting position.
CS1 <- dead_reckoning(trial.1$speed, trial.1$heading_geo,angle="radian")
CS2 <- dead_reckoning(trial.2$speed, trial.2$heading_geo,angle="radian")
CS3 <- dead_reckoning(trial.3$speed, trial.3$heading_geo,angle="radian")
CS4 <- dead_reckoning(trial.4$speed, trial.4$heading_geo,angle="radian")

## Plot the course steered for each trial.
plot(CS1$CSx,CS1$CSy,type='l',col='blue',xlab="X-coordinate (unprojected)",
     ylab="Y-coordinate (unprojected)",ylim=c(-400,150))
lines(CS2$CSx,CS2$CSy,col='green')
lines(CS3$CSx,CS3$CSy,col='red')
lines(CS4$CSx,CS4$CSy,col='magenta')
legend(-300,100,legend=c("Run1","Run2","Run3","Run4"),col=c("blue","green",
       "red","magenta"),lty=c(1,1,1,1),bty="n")
title('Course Steered for Mission Bay Trials')
grid()

## Plot the course steered vs. course made good
plot(CS1$CSx,CS1$CSy,type='l',col='blue',xlab="X-coordinate (unprojected)",
     ylab="Y-coordinate (unprojected)",ylim=c(-400,150))
lines(CS1$CMGx,CS1$CMGy,col='black')
t.set <- paste("Track 1, Set Angle: ",as.character(round(CS1$set*(180/pi),2)))
t.drift <- paste("Track 1, Drift: ",as.character(round(CS1$drift,2))," m/s")
t.error <- paste("Track 1, Error Distance: ",as.character(round(CS1$errordistance,2))," m")
title(paste(t.set,"\n",t.drift,"\n",t.error))
legend(-300,100,legend=c("Course Steered","Course Made Good"),
       col=c("blue","black"),lty=c(1,1),bty="n")
grid()

}

