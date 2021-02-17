\name{makeMovie}
\alias{makeMovie}
\title{
  Make a Movie
}
\description{
  Make a movie from an XML script file.
}
\usage{
makeMovie(filename,
          wd=paste0(gsub("[.]xml$", "", filename), "-movie"),
          TTS=espeakTTS(),
          world=realWorld,
          validate=TRUE,
          clean=FALSE) 
}
\arguments{
  \item{filename}{The name of an XML script file.}
  \item{wd}{A path to a working directory, where all files will be
    created.}
  \item{TTS}{An object that can turn text into audio;  see
    \code{\link{TTS}}.}
  \item{world}{The world that the movie will be set in; see
    \code{\link{realWorld}}.}
  \item{validate}{Should the script be validated against a DTD.}
  \item{clean}{A logical indicating whether to erase the working
    directory before beginning recording.}
}
\details{
  The XML file is assumed to have the following structure:
  \itemize{
    \item{A root element called \code{script}, containing ...}
    \item{A single \code{setting} element (optional).}
    \item{A single \code{stage} element containing one or more
      \code{location} elements. The \code{stage} can have \code{width}
      and \code{height} attributes.  Each \code{location} must have an
      \code{id} attribute and can have
      \code{x}, \code{y}, \code{width}, and \code{height} attributes.
      The content of the \code{location} element should be a shell
      command that can be run by the setting.}
    \item{One or more \code{scene} elements (with optional
      \code{id} attribute), containing ...}
    \itemize{
      \item{One or more \code{shot} elements (with optional
        \code{duration},
        \code{location} and \code{id} attributes), containing ...}
      \itemize{
        \item{Zero or one \code{backstage}, \code{keyaction}, or
          \code{pointeraction} elements and zero or one
          \code{dialogue} elements.  The content of the \code{backstage}
          element should be a shell command that the setting can
          execute.
          The \code{keyaction} element can have
          \code{keydelay} and \code{linedelay} attributes.
          The content of the \code{keyaction} element will be fed as
          key events to the location that is identified by the
          \code{location} attribute of the \code{keyaction} element.}
      }
    }
  }

  The package contains a DTD for the script structure (see the
  \code{DTD} directory in the installed package) and, if
  \code{validate} is \code{TRUE}, the script is validated against
  that DTD.
}
\value{
  A list containing the path to the complete final video, plus
  paths to individual scene video files.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{TTS}}, \code{\link{realWorld}}.
}
\keyword{ utilities }
