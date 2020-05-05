\name{setting}
\alias{setting}
\alias{localLinux}
\alias{localWindows}
\title{
  Define a Video Setting
}
\description{
  Define the setting within which to shoot a video.
}
\usage{
setting(create, focus, close, key, pointer)
localLinux()
localWindows()
}
\arguments{
  \item{create}{A function that can create a window on the desktop,
    given information about a \code{location} element
    (x, y, width, height, and shell command).
    This function should return a numeric ID for the window.
  }
  \item{focus}{A function that can give focus to a window on the
    desktop, given a numeric ID.  If the ID is \code{NULL}, the
    function should show a blank desktop (minimize all windows).}
  \item{close}{A function that can close a window on the desktop,
    given a numeric ID.}
  \item{key}{A function that can send key events to the window with
    focus.  This function is given the information from a \code{keyaction}
    element (keys and key delay).}
  \item{pointer}{A function that can send pointer events to the window
    with focus.  This function is given the information from a
    \code{pointeraction} element.}
}
\details{
  The \code{setting} function is used to define a setting for
  shooting a movie.

  The \code{localLinux} function defines a Linux setting.
  It assumes the existence of a windowing system and the utilities that
  underly the \pkg{wmctrl} and \pkg{xdotool} packages.

  The \code{localWindows} function defines a Windows setting.
  It assumes that \command{AutoHotKey} is installed.
}
\value{
  An object of class \code{DirectorSetting}.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{shootVideo}}
}
\keyword{ utilities }
