\name{directorWorld}
\alias{directorWorld}
\alias{realWorld}
\alias{dockerWorld}
\alias{shootVideo}
\title{
  Define a Movie World
}
\description{
  Define the world within which a movie will be generated.

  \code{realWorld} represents the desktop on localhost.

  \code{dockerWorld} describes a virtual environment based on a Docker
  image.

  \code{directorWorld} should be used to define any new worlds.

  \code{shootVideo} is called by \code{\link{makeMovie}};  it is only
  public so that new worlds can write a method for it.
}
\usage{
realWorld
dockerWorld(image, volumes=NULL)
directorWorld(..., class)
shootVideo(world, ...)
}
\arguments{
  \item{image}{The name of a Docker image.}
  \item{volumes}{Volumes to mount within the Docker container.}
  \item{\dots}{Parameters that describe the world.}
  \item{class}{A unique class for the world, typically
    \code{DirectorSomethingWorld}.}
  \item{world}{A \code{"DirectorWorld"} object.}
}
\details{
  When a movie is made, the \code{shootVideo} function is called with
  the \code{world} as its first argument.  Each world class 
  implements a \code{shootVideo} method.  
}
\value{
  A \code{"DirectorWorld"} object.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{makeMovie}}.
}
\keyword{ utilities }
