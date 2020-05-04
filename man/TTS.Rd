\name{TTS}
\alias{TTS}
\alias{espeakTTS}
\alias{pollyTTS}
\title{
  Define a Text-To-Speech Engine
}
\description{
  Define a text-to-speech engine that can convert text into audio.
}
\usage{
TTS(read, speak, ...)
espeakTTS()
pollyTTS(voice="Matthew",
         engine=c("standard", "neural"),
         format=c("mp3", "ogg_vorbis", "pcm"),
         sampleRate="default",
         language=NULL,
         lexicons=NULL) 
}
\arguments{
  \item{read}{A function that reads text from a \code{<dialogue>}
    element.  This function should return a character value that is in a
    format that the corresponding \code{speak} function can consume.
    This function should be able to handle SSML content, even if it is
    only to ignore it.}
  \item{speak}{A function that accepts the name of an \code{infile} containing
    text (or SMML) and the name of an \code{outfile} that the function
    should write to.  This function is responsible for generating
    audio from the \code{infile} text and saving that audio in the
    \code{outfile}.}
  \item{\dots}{Other arguments passed to the TTS engine.}
  \item{voice}{The name of an Amazon Polly voice.}
  \item{engine}{The name of an Amazon Polly engine;
    either \code{"standard"} or \code{"neural"}.}
  \item{format}{The format for the audio recording.}
  \item{sampleRate}{The sample rate for the audio recording.  The
    default value depends on the choice of engine and format.}
  \item{language}{The language used for the audio recording
    (only some voices are multilingual).}
  \item{lexicons}{The lexicons (special pronunciations) used for
    the audio recording; a vector of filenames.}
}
\details{
  The \code{TTS} function is for defining new TTS engines for recording
  the audio in a video.  The \code{espeakTTS} and \code{pollyTTS}
  functions define TTS engines based on the \command{espeak}
  and Amazon Polly text-to-speech systems.

  The \code{pollyTTS} engine assumes that the Amazon CLI has been
  installed and configured.
}
\value{
  An object of class \code{DirectorTTS}.
}
\references{
  espeak \url{http://espeak.sourceforge.net/}.
  Amazon Polly \url{https://aws.amazon.com/polly/}.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{shootVideo}}
}
\keyword{ utilities }
