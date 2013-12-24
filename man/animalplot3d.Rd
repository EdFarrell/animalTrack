\name{animalplot3d}
\alias{animalplot3d}
\title{Make a 3D rotated plot of animal orientation based on roll, pitch, and yaw angles.}
\description{3D rendering of animal attitude using the "rgl" package. }
\usage{
animalplot3d(roll, pitch, yaw, angle = "degree", add = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{roll}{Roll angle of the animal
}
  \item{pitch}{Pitch angle of the animal
}
  \item{yaw}{Yaw angle of the animal
}
  \item{angle}{Unit of angular measure for input. Default is "degree", but the function will accept
  "radian". All three input angles must be either "degree" or "radian".
}
  \item{add}{Indicates if the plot will be added to an existing rgl plot.
}
  \item{\dots}{Additional items to be passed on to the rgl plotting function.
}
}
\details{This plotting function uses rgl plotting functions to produce a new 3D plot, or to add
to an existing 3D plot. Plotting parameters will be passed on the the plot3d() function for 
customization. The stationary "body frame" which uses the "North-East-Down" (NED) frame of reference is plotted using the color red. Each 
of the three axes is plotted with small spheres on the end. The X-axis points posterior to anterior (i.e. direction the animal is facing) 
and faces north when not rotated. The Y-axis is oriented to the east of the X-axis (points left to right). The Z-axis is the dorso-ventral axis and points downward when not rotated (toward the center of the Earth).
The rotated axes are also plotted in blue. The X-axis is given a large sphere so that it is easy to
represent the direction that the animal is facing.
}
\value{An object with two rotated matrices

  \item{rotmat}{3x3 rotation matrix
  }
  \item{hrotmat}{homogenous rotation matrix
  }
}

\author{Ed Farrell <edward.farrell27@gmail.com>}

\seealso{\code{\link[rgl]{plot3d}}}
\examples{

## Animal with a roll angle of 30 degrees, and a pitch angle of 10 degrees
roll <- 30
pitch <- 10
yaw <- 0

animalplot3d(roll,pitch,yaw,angle = "degree")

## Make the plot a bit larger
par3d(windowRect = c(50,50,700,700)) 

## Add another animal orientation with the same roll and pitch, but a 40 degree yaw.
animalplot3d(roll,pitch,40,angle = "degree", add=TRUE)


}

