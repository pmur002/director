\name{shootVideo}
\alias{shootVideo}
\title{
  Read a video script
}
\description{
  Read the script for a video from an XML file.
  This function is called by \code{\link{shootVideo}};  it is
  exported so that the steps in video construction can be run
  individually, e.g., for debugging purposes.
}
\usage{
shootVideo(filename,
           wd=paste0(gsub("[.]xml$", "", filename), "-video"),
           clean=FALSE) 
}
\arguments{
  \item{filename}{The name of an XML script file.}
  \item{wd}{A path to a working directory, where all files will be
    created.}
  \item{clean}{A logical indicating whether to erase the working
    directory before beginning recording.}
}
\details{
  The XML file is assumed to have the following structure:
  \itemize{
    \item{A root element called \code{script}, containing ...}
    \item{A single \code{stage} element containing one or more
      \code{location} elements. The \code{stage} can have \code{width}
      and \code{height} attributes.  Each \code{location} must have 
      \code{id} and \code{program} attributes and can have
      \code{x}, \code{y}, \code{width}, and \code{height}, plus
      \code{args} attributes.}
    \item{One or more \code{scene} elements (with optional
      \code{id} attribute), containing ...}
    \item{One or more \code{shot} elements (with optional
      \code{duration},
      \code{location} and \code{id} attributes), containing ...}
    \item{Zero or one \code{action} element and zero or one
      \code{dialogue} element.  The \code{action} element can have
      \code{keydelay} and \code{linedelay} attributes.}
  }
}
\value{
  A list containing the path to the complete final video, plus
  paths to individual scene video files.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{shootVideo}}
}
\keyword{ utilities }
